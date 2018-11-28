use llvm_sys::LLVMRealPredicate;
use llvm_sys::prelude::*;
use llvm_sys::core::*;
use kolgac::ast::Ast;
use kolgac::token::{Token, TknTy};
use kolgac::ty_rec::{TyRec, TyName};
use error::gen::{GenErrTy, GenErr};
use valtab::ValTab;
use classtab::ClassTab;
//use fpm::FPM;
use std::ptr;
use std::slice;
use std::ffi::CString;

const LLVM_FALSE: LLVMBool = 0;

/// CodeGenerator handles the code generation for LLVM IR. Converts an AST to LLVM IR. We assume
/// there are no parsing errors and that each node in the AST can be safely unwrapped. Each
/// variable can be assumed to exist.
pub struct CodeGenerator<'t, 'v> {
    /// Parsed AST
    ast: &'t Ast,

    /// Value table stores LLVMValueRef's for lookup.
    valtab: &'v mut ValTab,

    /// Class table stores LLVmStructTypes so we can look them up before allocating.
    classtab: ClassTab,

    /// LLVM Context.
    context: LLVMContextRef,

    /// LLVM Builder.
    builder: LLVMBuilderRef,

    /// LLVM Module. We use only a single module for single file programs.
    module: LLVMModuleRef,

    /// Owned CStrings that we use for naming things in our LLVM module.
    strings: Vec<CString>,

    /// Vector of potential errors to return.
    pub errors: Vec<GenErr>

    // /// LLVM Function pass manager, for some optimization passes after function codegen.
    //fpm: FPM
}

/// We implement Drop for the CodeGenerator to ensure that our LLVM structs are safely
/// disposed of when the CodeGenerator goes out of scope.
impl<'t, 'v> Drop for CodeGenerator<'t, 'v> {
    fn drop(&mut self) {
        unsafe {
            LLVMDisposeBuilder(self.builder);
            LLVMDisposeModule(self.module);
            LLVMContextDispose(self.context);
        }
    }
}

impl<'t, 'v> CodeGenerator<'t, 'v> {
    /// Creates a new CodeGenerator, given a properly parsed AST, symbol table, and value table.
    /// We assume that the symbol table already contains all the required variables in this module,
    /// and that the value table is newly defined and should be empty.
    /// This function also sets up all the required LLVM structures needed to generate the IR:
    /// the context, the builder, and the module.
    pub fn new(ast: &'t Ast, valtab: &'v mut ValTab) -> CodeGenerator<'t, 'v> {
        unsafe {
            let context = LLVMContextCreate();
            let module = LLVMModuleCreateWithNameInContext(c_str!("kolga"), context);
            CodeGenerator {
                ast: ast,
                valtab: valtab,
                classtab: ClassTab::new(),
                errors: Vec::new(),
                context: context,
                builder: LLVMCreateBuilderInContext(context),
                module: module,
                strings: Vec::new()
                //fpm: FPM::new(module)
            }
        }
    }

    /// Initial entry point for LLVM IR code generation. Loops through each statement in the
    /// program and generates LLVM IR for each of them. The code is written to the module,
    /// to be converted to assembly later.
    pub fn gen_ir(&mut self) {
        match self.ast {
            Ast::Prog{stmts} => {
                for stmt in stmts {
                    self.gen_stmt(stmt);
                }
            },
            _ => ()
        }
    }

    /// Dumps the current module's IR to stdout.
    pub fn dump_ir(&self) {
        unsafe { LLVMDumpModule(self.module); }
    }

    /// Saves the current module's IR to a file.
    pub fn print_ir(&self, filename: String) {
        unsafe {
            LLVMPrintModuleToFile(self.module,
                                  filename.as_bytes().as_ptr() as *const i8,
                                  ptr::null_mut());
        }
    }

    pub fn get_mod(&self) -> LLVMModuleRef {
        self.module
    }

    /// Generate LLVM IR for a kolga statement. This handles all statement types, and will also
    /// call through to self.gen_expr() when needed. This is a recursive function, and will walk
    /// the AST for any nested statements or block statements.
    ///
    /// Returns a vector of LLVMValueRef's, which may be needed to generate PHI blocks or to make
    /// checks after recursive calls return. If there is no generated values, returns empty vec.
    // TODO: This is a bit of a hack, should probably return a result
    // instead of an empty vec (statements dont evaluate to anything, so there's never an
    // LLVMValueRef returned). But in the case that we do have to generate an expression,
    // we need to know which values we generated.
    fn gen_stmt(&mut self, stmt: &Ast) -> Vec<LLVMValueRef> {
        match stmt {
            Ast::BlckStmt{stmts, sc: _} => {
                let mut generated = Vec::new();
                for stmt in stmts {
                    let mb_gen = self.gen_stmt(&stmt.clone());
                    generated.extend(mb_gen);
                }

                generated
            },
            Ast::ExprStmt(maybe_ast) => {
                let ast = maybe_ast.clone();
                let val = self.gen_expr(&ast);
                match val {
                    Some(exprval) => vec![exprval],
                    None => {
                        self.error(GenErrTy::InvalidAst);
                        Vec::new()
                    }
                }
            },
            Ast::IfStmt{cond_expr, if_stmts, elif_exprs, el_stmts} => {
                self.if_stmt(cond_expr, if_stmts, elif_exprs, el_stmts)
            },
            Ast::WhileStmt(mb_cond_expr, mb_stmts) => {
                self.while_stmt(mb_cond_expr, mb_stmts)
            },
            Ast::ForStmt{for_var_decl, for_cond_expr, for_step_expr, stmts} => {
                self.for_stmt(for_var_decl, for_cond_expr, for_step_expr, stmts)
            },
            Ast::FnDecl{ident_tkn, fn_params, ret_ty, fn_body, sc: _} => {
                unsafe {
                    self.valtab.init_sc();

                    let fn_name = self.c_str(&ident_tkn.get_name());
                    let fn_ty = self.llvm_ty_from_ty_rec(ret_ty);

                    // Convert our params to an array of LLVMTypeRef's. We then pass these
                    // types to the function to encode the types of our params. After we create
                    // our function, we can add it to the builder and position it at
                    // the end of the new basic block.
                    let mut param_tys = self.llvm_tys_from_ty_rec_arr(fn_params);
                    let llvm_fn_ty = LLVMFunctionType(fn_ty,
                                                      param_tys.as_mut_ptr(),
                                                      param_tys.len() as u32,
                                                      LLVM_FALSE);

                    let llvm_fn = LLVMAddFunction(self.module, fn_name, llvm_fn_ty);
                    let fn_val = LLVMAppendBasicBlockInContext(self.context, llvm_fn, fn_name);
                    LLVMPositionBuilderAtEnd(self.builder, fn_val);

                    // Get the params from the function we created. This is a little weird since
                    // we pass in an array of LLVMTypeRef's to the function, but we want
                    // LLVMValueRef's to store in the symbol table and to give them names. We need
                    // to get the params and loop through them again.
                    let mut llvm_params: *mut LLVMValueRef = Vec::with_capacity(param_tys.len()).as_mut_ptr();
                    LLVMGetParams(llvm_fn, llvm_params);
                    let param_value_vec = slice::from_raw_parts(llvm_params, param_tys.len()).to_vec();
                    for (idx, param) in param_value_vec.iter().enumerate() {
                        let name = self.c_str(&fn_params[idx].tkn.get_name());
                        LLVMSetValueName(*param, name);

                        let alloca_instr = self.build_entry_bb_alloca(llvm_fn,
                                                                      fn_params[idx].clone(),
                                                                      &fn_params[idx].tkn.get_name());
                        LLVMBuildStore(self.builder, *param, alloca_instr);
                        self.valtab.store(&fn_params[idx].tkn.get_name(), alloca_instr);
                    }

                    // Store the function symbol inside the value table before parsing the
                    // body, so we can accept recursive calls.
                    self.valtab.store(&ident_tkn.get_name(), llvm_fn);

                    // TODO: this is hard to read -_-
                    match *fn_body.clone() {
                        Ast::BlckStmt{stmts, sc: _} => {
                            for stmt in stmts {
                                match stmt.clone() {
                                    Ast::RetStmt(mb_expr) => {
                                        if mb_expr.is_none() {
                                            // Use a null ptr when we return void
                                            LLVMBuildRet(self.builder, ptr::null_mut());
                                        } else {
                                            let llvm_val = self.gen_expr(&mb_expr.clone().unwrap());
                                            LLVMBuildRet(self.builder, llvm_val.unwrap());
                                        }
                                    },
                                    _ => { self.gen_stmt(&stmt.clone()); }
                                }
                            }
                        },
                        _ => ()
                    }

                    // Run the function pass through our manager
                    //self.fpm.run(llvm_fn);

                    // Close the function level scope, which will pop off any params and
                    // variable declared here (we don't need these anymore, since we aren't
                    // going to be making another pass over them later). Add the llvm function
                    // to the value table so we can look it up later for a call.
                    self.valtab.close_sc();
                    self.valtab.store(&ident_tkn.get_name(), llvm_fn);
                }

                Vec::new()
            },
            Ast::VarAssignExpr{ty_rec, ident_tkn, is_imm:_, is_global, value} => {
                match is_global {
                    true => {
                        let c_name = self.c_str(&ident_tkn.get_name());
                        let var_ident = ident_tkn.get_name();

                        match *value.clone() {
                            Ast::ClassDecl{ident_tkn, methods:_, props:_, prop_pos:_, sc:_} => {
                                let llvm_ty = self.classtab.retrieve(&ident_tkn.get_name());
                                if llvm_ty.is_none() {
                                    self.error(GenErrTy::InvalidClass(ident_tkn.get_name()));
                                    return Vec::new();
                                }
                                unsafe {
                                    let global = LLVMAddGlobal(self.module, llvm_ty.unwrap(), c_name);
                                    self.valtab.store(&var_ident, global);
                                    vec![global]
                                }
                            },
                            _ => {
                                unsafe {
                                    let llvm_ty = self.llvm_ty_from_ty_rec(ty_rec);
                                    let global = LLVMAddGlobal(self.module, llvm_ty, c_name);

                                    let val = self.gen_expr(&value.clone()).unwrap();
                                    LLVMSetInitializer(global, val);
                                    self.valtab.store(&ident_tkn.get_name(), global);
                                    vec![global]
                                }
                            }
                        }
                    },
                    false => {
                        unsafe {
                            let insert_bb = LLVMGetInsertBlock(self.builder);
                            let mut llvm_func = LLVMGetBasicBlockParent(insert_bb);
                            let alloca_instr = self.build_entry_bb_alloca(llvm_func,
                                                                          ty_rec.clone(),
                                                                          &ident_tkn.get_name());

                            let raw_val = value.clone();
                            // We don't need to store anything for class types, since they
                            // are already built into structs in the class declaration. The class
                            // here should already be a struct type (if we tried to create a class
                            // before declaring it we would not pass parsing).
                            match *raw_val {
                                Ast::ClassDecl{ident_tkn:_, methods:_, props:_, prop_pos:_, sc:_} => {
                                    vec![alloca_instr]
                                },
                                _ => {
                                    let val = self.gen_expr(&raw_val).unwrap();
                                    LLVMBuildStore(self.builder, val, alloca_instr);
                                    self.valtab.store(&ident_tkn.get_name(), alloca_instr);
                                    vec![alloca_instr]
                                }
                            }
                        }
                    }
                }
            },
            Ast::VarDeclExpr{ty_rec, ident_tkn, is_imm:_, is_global} => {
                match is_global {
                    true => {
                        unsafe {
                            let c_name = self.c_str(&ident_tkn.get_name());
                            let llvm_ty = self.llvm_ty_from_ty_rec(ty_rec);
                            let global = LLVMAddGlobal(self.module, llvm_ty, c_name);
                            self.valtab.store(&ident_tkn.get_name(), global);
                            vec![global]
                        }
                    },
                    false => {
                        unsafe {
                            let insert_bb = LLVMGetInsertBlock(self.builder);
                            let mut llvm_func = LLVMGetBasicBlockParent(insert_bb);
                            let alloca_instr = self.build_entry_bb_alloca(llvm_func,
                                                                          ty_rec.clone(),
                                                                          &ident_tkn.get_name());
                            self.valtab.store(&ident_tkn.get_name(), alloca_instr);
                            vec![alloca_instr]
                        }
                    }
                }
            },
            Ast::ClassDecl{ident_tkn, methods, props, prop_pos:_, sc:_} => {
                unsafe {
                    let mut prop_tys = Vec::new();
                    for pr in props {
                        // Here we just want to lay out the props,
                        // we don't actually want to allocate them until we
                        // create an object of this class.
                        // So, we want the llvm type of the props, but we
                        // don't want to generate any code for them yet.
                        match &pr.clone() {
                            Ast::VarDeclExpr{ty_rec, ident_tkn:_, is_imm:_, is_global:_} => {
                                let llvm_ty = self.llvm_ty_from_ty_rec(ty_rec);
                                prop_tys.push(llvm_ty);
                            },
                            _ => ()
                        }
                    }

                    let class_name = ident_tkn.get_name();
                    let llvm_struct = LLVMStructCreateNamed(self.context, self.c_str(&class_name));
                    LLVMStructSetBody(llvm_struct, prop_tys.as_mut_ptr(), prop_tys.len() as u32, LLVM_FALSE);

                    // Store the struct type in a special class table, so we can look it up
                    // later when we want to allocate one. This is not the same as a the value table,
                    // as it doesn't represent an allocated value, just the type info for the class.
                    // Note: This must be stored before we process the class method declarations,
                    // because they need to look up the class name from the symbol table in order
                    // to insert the class as a 'self' param.
                    self.classtab.store(&class_name, llvm_struct);

                    // Methods are generated like any other method, but with a pointer to
                    // the enclosing class as the first parameter ('self'). This pointer can be
                    // used to access class variables and other class methods
                    // These don't "belong" to the class in the llvm ir, but just
                    // live anywhere in the output
                    let class_tkn = ident_tkn.clone();
                    for mtod in methods {
                        match mtod.clone() {
                            Ast::FnDecl{ident_tkn, fn_params, ret_ty, fn_body, sc} => {
                                // We need to add the class declaration type to the list of
                                // params so we obtain a pointer to it inside the method body.
                                let fake_class_param = TyRec {
                                    ty: Some(TyName::Class(class_name.clone())),
                                    tkn: class_tkn.clone()
                                };

                                let mut new_params = fn_params.clone();
                                new_params.insert(0, fake_class_param);

                                // We change the name of the function by prepending the
                                // class name so we avoid storing duplicates in the value table.
                                // TOOD: This is mostly a hack though, because then no one can
                                // create a function with the class name prepended to the class
                                // function name!
                                let curr_name = ident_tkn.get_name();
                                let new_name = format!("{}_{}", class_tkn.get_name(), curr_name);
                                let new_tkn = Token::new(TknTy::Ident(new_name),
                                                         ident_tkn.line,
                                                         ident_tkn.pos);

                                let new_method = Ast::FnDecl{
                                    ident_tkn: new_tkn,
                                    fn_params: new_params,
                                    ret_ty: ret_ty.clone(),
                                    fn_body: fn_body.clone(),
                                    sc: sc
                                };

                                self.gen_stmt(&new_method);
                            },
                            _ => ()
                        }
                    }
                }

                Vec::new()
            },
            _ => unimplemented!("Ast type {:?} is not implemented for codegen", stmt)
        }
    }

    /// Generate LLVM IR for expression type ASTs. This handles building comparisons and constant
    /// ints and strings, as well as function call expressions.
    /// This is a recursive function, and will walk the expression AST until we reach a point
    /// to terminate on.
    fn gen_expr(&mut self, expr: &Ast) -> Option<LLVMValueRef> {
        match expr {
            Ast::PrimaryExpr{ty_rec} => self.gen_primary(&ty_rec),
            Ast::BinaryExpr{ty_rec:_, op_tkn, lhs, rhs} |
            Ast::LogicalExpr{ty_rec:_, op_tkn, lhs, rhs} => {
                // Recursively generate the LLVMValueRef's for the LHS and RHS. This is just
                // a single call for each if they are primary expressions.
                let mb_lhs_llvm_val = self.gen_expr(&lhs.clone());
                let mb_rhs_llvm_val = self.gen_expr(&rhs.clone());

                if mb_lhs_llvm_val.is_none() || mb_rhs_llvm_val.is_none() {
                    return None;
                }

                let lhs_llvm_val = mb_lhs_llvm_val.unwrap();
                let rhs_llvm_val = mb_rhs_llvm_val.unwrap();

                // Convert the operator to an LLVM instruction once we have the
                // LHS and RHS values.
                self.llvm_val_from_op(&op_tkn.ty, lhs_llvm_val, rhs_llvm_val)
            },
            Ast::UnaryExpr{ty_rec:_, op_tkn, rhs} => {
                let mb_rhs_llvm_val = self.gen_expr(&rhs);
                if mb_rhs_llvm_val.is_none() {
                    return None;
                }

                let rhs_llvm_val = mb_rhs_llvm_val.unwrap();
                match op_tkn.ty {
                    TknTy::Minus => {
                        unsafe { Some(LLVMBuildFNeg(self.builder, rhs_llvm_val, self.c_str("tmpneg"))) }
                    },
                    TknTy::Bang => {
                        unsafe {
                            // There isn't any logical not instruction, so we use XOR to
                            // flip the value (which is of type i8 now) from 0/1 to represent
                            // the opposite boolean value.
                            let xor_rhs = LLVMConstInt(self.i8_ty(), 1, LLVM_FALSE);
                            Some(LLVMBuildXor(self.builder, rhs_llvm_val, xor_rhs, self.c_str("tmpnot")))
                        }
                    },
                    _ => None
                }
            },
            Ast::FnCall{fn_tkn, fn_params} => {
                // Check if the function was defined in the IR. We should always have
                // the function defined in the IR though, since we wouldn't pass the parsing
                // phase if we tried to call an undefined function name.
                let fn_name = fn_tkn.clone().get_name();
                let llvm_fn = self.valtab.retrieve(&fn_name);
                if llvm_fn.is_none() {
                    self.error(GenErrTy::InvalidFn(fn_name));
                    return None;
                }

                // Recursively generate LLVMValueRef's for the function params, which
                // might be non-primary expressions themselves. We store these in a vector,
                // so we can pass it to the LLVM IR function call instruction.
                let mut param_tys: Vec<LLVMValueRef> = Vec::new();
                for param in fn_params {
                    let llvm_val = self.gen_expr(param);
                    if llvm_val.is_none() {
                        self.error(GenErrTy::InvalidFnParam);
                        return None;
                    }

                    param_tys.push(llvm_val.unwrap());
                }

                unsafe {
                    Some(LLVMBuildCall(self.builder,
                                       llvm_fn.unwrap(),
                                       param_tys.as_mut_ptr(),
                                       param_tys.len() as u32,
                                       self.c_str("")))
                }

            },
            Ast::ClassFnCall{class_tkn, class_name, fn_tkn, fn_params, sc:_} => {
                // The class function is stored under a different name in the
                // value table, with the class name prepended.
                let fn_name = fn_tkn.clone().get_name();
                let class_fn_name = format!("{}_{}", class_name, fn_name);

                let llvm_fn = self.valtab.retrieve(&class_fn_name);
                if llvm_fn.is_none() {
                    self.error(GenErrTy::InvalidFn(fn_name));
                    return None;
                }

                // We need to insert a pointer to the class instance as the first param in order to
                // call the class function. We get that pointer from the value table (the pointer
                // is the actual instance of the class that has been created).
                let mut param_tys: Vec<LLVMValueRef> = Vec::new();
                let class_instance = self.valtab.retrieve(&class_tkn.get_name());
                param_tys.push(class_instance.unwrap());

                for param in fn_params {
                    let llvm_val = self.gen_expr(param);
                    if llvm_val.is_none() {
                        self.error(GenErrTy::InvalidFnParam);
                        return None;
                    }

                    param_tys.push(llvm_val.unwrap());
                }

                unsafe {
                    Some(LLVMBuildCall(self.builder,
                                       llvm_fn.unwrap(),
                                       param_tys.as_mut_ptr(),
                                       param_tys.len() as u32,
                                       self.c_str("")))
                }
            },
            Ast::VarAssignExpr{ty_rec:_, ident_tkn, is_imm:_, is_global:_, value} => {
                // This is a variable re-assign, not a new declaration and assign. Thus,
                // we look up the alloca instruction from the value table, and build
                // a new store instruction for it. We don't need to update the value table
                // (I don't THINK we need to), because we still want to manipulate the old
                // alloca instruction.
                // TODO: what if this isn't a re-assign?
                unsafe {
                    let curr_alloca_instr = self.valtab.retrieve(&ident_tkn.get_name()).unwrap();
                    let raw_val = value.clone();
                    let val = self.gen_expr(&raw_val).unwrap();

                    LLVMBuildStore(self.builder, val, curr_alloca_instr);
                    Some(val)
                }
            },
            // Class declarations ast types can be used as rvalues when creating a class.
            Ast::ClassDecl{ident_tkn, methods:_, props:_, prop_pos:_, sc:_} => {
                let name = ident_tkn.get_name();
                let llvm_struct_ty = self.classtab.retrieve(&name);
                match llvm_struct_ty {
                    Some(ty_ref) => {
                        let c_name = self.c_str(&name);
                        unsafe {
                            let llvm_val = LLVMBuildAlloca(self.builder, ty_ref, c_name);
                            return Some(llvm_val);
                        }
                    },
                    None => {
                        self.error(GenErrTy::InvalidClass(name));
                        None
                    }
                }
            },
            Ast::ClassPropAccess{ident_tkn, prop_name, idx, owner_class:_} => {
                let name = ident_tkn.get_name();
                let class = self.valtab.retrieve(&name);
                if class.is_none() {
                    self.error(GenErrTy::InvalidClass(name));
                    return None;
                }

                let classptr = class.unwrap();
                unsafe {
                    let gep_val = LLVMBuildStructGEP(self.builder,
                                                     classptr,
                                                     *idx as u32,
                                                     self.c_str(prop_name));
                    // GEP returns the address of the prop we want to access. We can load it
                    // into a variable here so that we return a non-pointer type.
                    // TODO: can this be set as a global variable?
                    let ld_val = LLVMBuildLoad(self.builder, gep_val, self.c_str(prop_name));
                    Some(ld_val)
                }
            },
            _ => unimplemented!("Ast type {:?} is not implemented for codegen", expr)
        }
    }

    /// Generate LLVM IR for a primary expression. This returns an Option because
    /// it's possible that we cant retrieve an identifier from the value table (if it's
    /// undefined).
    fn gen_primary(&mut self, ty_rec: &TyRec) -> Option<LLVMValueRef> {
        match ty_rec.tkn.ty {
            TknTy::Val(ref val) => unsafe { Some(LLVMConstReal(self.double_ty(), *val)) },
            TknTy::Str(ref lit) => unsafe { Some(LLVMBuildGlobalStringPtr(self.builder,
                                                                          self.c_str(lit),
                                                                          self.c_str("")))},
            TknTy::True => unsafe { Some(LLVMConstInt(self.i8_ty(), 1, LLVM_FALSE)) },
            TknTy::False => unsafe { Some(LLVMConstInt(self.i8_ty(), 0, LLVM_FALSE)) },
            TknTy::Ident(ref name) => {
                match self.valtab.retrieve(name) {
                    Some(val) => {
                        unsafe {
                            let c_name = self.c_str(&name);
                            Some(LLVMBuildLoad(self.builder, val, c_name))
                        }
                    },
                    None => None
                }
            },
            _ => unimplemented!("Tkn ty {:?} is unimplemented in codegen", ty_rec.tkn.ty)
        }
    }

    /// Generate LLVM IR for an if statement. This handles elif and else conditions as well.
    /// Returns a vector of LLVM values that are created during generation. If there are no
    /// values created, returns an empty vector.
    fn if_stmt(&mut self,
               if_cond: &Box<Ast>,
               then_stmts: &Box<Ast>,
               else_if_stmts: &Vec<Ast>,
               else_stmts: &Vec<Ast>) -> Vec<LLVMValueRef> {
        let mut return_stmt_vec = Vec::new();
        unsafe {
            let has_elif = else_if_stmts.len() > 0;
            let has_else = else_stmts.len() > 0 && else_stmts.len() < 2;

            // Set up our required blocks. We need an initial block to start building
            // from (insert_bb), and a block representing the then branch (then_bb), which
            // is always present. We always keep an else block for conditional branching,
            // and a merge block to branch to after we have evaluated all the code in the
            // if statement. Blocks are manually reordered here as well, to account
            // for any nested if statements. If there is no nesting, these re-orders
            // effectively do nothing.
            let insert_bb = LLVMGetInsertBlock(self.builder);
            let fn_val = LLVMGetBasicBlockParent(insert_bb);

            let then_bb = LLVMAppendBasicBlockInContext(self.context, fn_val, self.c_str("then"));
            LLVMMoveBasicBlockAfter(then_bb, insert_bb);

            let else_bb = LLVMAppendBasicBlockInContext(self.context, fn_val, self.c_str("el"));
            LLVMMoveBasicBlockAfter(else_bb, then_bb);

            let merge_bb = LLVMAppendBasicBlockInContext(self.context, fn_val, self.c_str("merge"));
            LLVMMoveBasicBlockAfter(merge_bb, else_bb);

            // Build any necessary blocks for elif conditions. This is a vector of conditional blocks
            // that we use to decide branching instructions.
            let mut elif_bb_vec = Vec::new();
            for i in 0..else_if_stmts.len() {
                let name = format!("{}{}{}", "elifcond", i, "\0");
                let mut tmp_bb = LLVMAppendBasicBlockInContext(self.context,
                                                               fn_val,
                                                               name.as_bytes().as_ptr() as *const i8);
                elif_bb_vec.push(tmp_bb);
            }

            // Move position to end of merge block to create our phi block at the end of the
            // conditional. We immediately move it back to the start of the conditional so
            // we're still in the correct position.
            LLVMPositionBuilderAtEnd(self.builder, merge_bb);
            let phi_bb = LLVMBuildPhi(self.builder, self.double_ty(), self.c_str("phi"));
            LLVMPositionBuilderAtEnd(self.builder, insert_bb);

            // Calculate the LLVMValueRef for the if conditional expression. We use this
            // to build a conditional branch from the then block to the else block, if needed.
            let cond_val = self.gen_expr(&if_cond.clone());
            if cond_val.is_none() {
                self.error(GenErrTy::InvalidAst);
                return Vec::new();
            }

            // Build the conditional branch from the then block to the next required block. If we
            // have any else ifs, we branch to the first else if conditional block, otherwise
            // we check if there is an else block. If so, we branch there. If not, we branch to the
            // merge block.
            LLVMPositionBuilderAtEnd(self.builder, insert_bb);
            let else_cond_br = match has_elif {
                true => elif_bb_vec[0],
                false => {
                    match has_else {
                        true => else_bb,
                        false => merge_bb
                    }
                }
            };
            LLVMBuildCondBr(self.builder, cond_val.unwrap(), then_bb, else_cond_br);

            // Build then block values and branch to merge block from inside the then block.
            LLVMPositionBuilderAtEnd(self.builder, then_bb);
            let mut then_expr_vals = self.gen_stmt(&then_stmts.clone());
            return_stmt_vec.extend(then_expr_vals.clone());
            LLVMBuildBr(self.builder, merge_bb);

            let then_end_bb = LLVMGetInsertBlock(self.builder);
            LLVMPositionBuilderAtEnd(self.builder, merge_bb);
            if then_expr_vals.len() > 0 {
                LLVMAddIncoming(phi_bb, then_expr_vals.as_mut_ptr(), vec![then_end_bb].as_mut_ptr(), 1);
            }

            // Generate blocks for any elif statements.
            // This block is used to correctly position the else block, if any. We want the
            // else block to sit after the elifs, and not after the then block.
            let mut final_elif_bb = then_bb;
            for (idx, stmt) in else_if_stmts.iter().enumerate() {
                match stmt.clone() {
                    Ast::ElifStmt(mb_cond, mb_stmts) => {
                        // Get the conditional block from the vector made above. Create a seperate
                        // block to the elif code to live in, that we can branch to from the
                        // elif conditioanl block.
                        let mut elif_cond_bb = elif_bb_vec[idx];
                        LLVMPositionBuilderAtEnd(self.builder, elif_cond_bb);
                        LLVMMoveBasicBlockAfter(elif_cond_bb, else_bb);
                        let name = format!("{}{}{}", "elifblck", idx, "\0");
                        let mut elif_code_bb = LLVMAppendBasicBlockInContext(
                            self.context,
                            fn_val,
                            name.as_ptr() as *const i8);

                        LLVMMoveBasicBlockAfter(elif_code_bb, elif_cond_bb);

                        let elif_cond_val = self.gen_expr(&mb_cond.clone());
                        if elif_cond_val.is_none() {
                            self.error(GenErrTy::InvalidAst);
                            continue;
                        }

                        // If we're in the last elif block, we want to branch to the else block.
                        // If there's no else block, we branch to the merge block. If we're not
                        // in the last elif block,  we branch to the next elif conditional block
                        // in the elif block vector.
                        LLVMPositionBuilderAtEnd(self.builder, elif_cond_bb);
                        let else_cond_br = match idx == else_if_stmts.len()-1 {
                            true => {
                                match has_else {
                                    true => else_bb,
                                    false => merge_bb
                                }
                            },
                            false => elif_bb_vec[idx+1]
                        };

                        LLVMBuildCondBr(self.builder,
                                        elif_cond_val.unwrap(),
                                        elif_code_bb,
                                        else_cond_br);
                        LLVMPositionBuilderAtEnd(self.builder, elif_code_bb);

                        // Evaluate the elif block statements and branch to the merge block
                        // from inside the elif block.
                        let mut elif_expr_vals = self.gen_stmt(&mb_stmts.clone());
                        return_stmt_vec.extend(elif_expr_vals.clone());
                        LLVMBuildBr(self.builder, merge_bb);
                        let mut elif_end_bb = LLVMGetInsertBlock(self.builder);
                        LLVMPositionBuilderAtEnd(self.builder, merge_bb);
                        LLVMAddIncoming(phi_bb,
                                        elif_expr_vals.as_mut_ptr(),
                                        vec![elif_end_bb].as_mut_ptr(),
                                        1);
                        LLVMPositionBuilderAtEnd(self.builder, elif_code_bb);
                        final_elif_bb = elif_code_bb;
                    },
                    _ => ()
                }
            }

            // Generate code the the else block, if we have one.
            if has_else {
                LLVMMoveBasicBlockAfter(else_bb, final_elif_bb);
                LLVMPositionBuilderAtEnd(self.builder, else_bb);
                let mut else_expr_vals = self.gen_stmt(&else_stmts[0]);
                return_stmt_vec.extend(else_expr_vals.clone());

                LLVMBuildBr(self.builder, merge_bb);
                let else_end_bb = LLVMGetInsertBlock(self.builder);
                LLVMPositionBuilderAtEnd(self.builder, merge_bb);
                LLVMAddIncoming(phi_bb, else_expr_vals.as_mut_ptr(), vec![else_end_bb].as_mut_ptr(), 1);
            } else {
                LLVMPositionBuilderAtEnd(self.builder, merge_bb);
            }

            return_stmt_vec
        }
    }

    /// Generates LLVM IR for a while loop statement, and returns a vector of values
    /// that are created during that code gen. If there are no values, the vector is
    /// empty.
    fn while_stmt(&mut self, cond_expr: &Box<Ast>, stmts: &Box<Ast>) -> Vec<LLVMValueRef> {
        let mut return_stmt_vec = Vec::new();
        unsafe {
            let insert_bb = LLVMGetInsertBlock(self.builder);
            let fn_val = LLVMGetBasicBlockParent(insert_bb);

            // Set up our blocks
            let entry_bb = LLVMAppendBasicBlockInContext(self.context, fn_val, self.c_str("entry"));
            let while_bb = LLVMAppendBasicBlockInContext(self.context, fn_val, self.c_str("while"));
            let merge_bb = LLVMAppendBasicBlockInContext(self.context, fn_val, self.c_str("merge"));

            LLVMPositionBuilderAtEnd(self.builder, merge_bb);
            let phi_bb = LLVMBuildPhi(self.builder, self.double_ty(), self.c_str("phi"));
            LLVMPositionBuilderAtEnd(self.builder, insert_bb);

            // Evaluate the conditional expression
            let cond_val = self.gen_expr(&cond_expr.clone());
            if cond_val.is_none() {
                self.error(GenErrTy::InvalidAst);
                return Vec::new();
            }

            // Buld the conditional branch
            LLVMPositionBuilderAtEnd(self.builder, entry_bb);
            LLVMBuildCondBr(self.builder, cond_val.unwrap(), while_bb, merge_bb);
            LLVMPositionBuilderAtEnd(self.builder, while_bb);

            let mut stmt_vals = self.gen_stmt(&stmts.clone());
            return_stmt_vec.extend(stmt_vals.clone());

            // Evaluate the conditional expression again. This will handle reading
            // the updated loop variable (if any) to properly branch out of the loop
            // if necessary. We build another conditional branch in the loop to handle
            // this.
            let updated_cond_val = self.gen_expr(&cond_expr.clone());
            LLVMBuildCondBr(self.builder, updated_cond_val.unwrap(), while_bb, merge_bb);
            let while_end_bb = LLVMGetInsertBlock(self.builder);
            LLVMPositionBuilderAtEnd(self.builder, merge_bb);
            LLVMAddIncoming(phi_bb, stmt_vals.as_mut_ptr(), vec![while_end_bb].as_mut_ptr(), 1);
        }

        return_stmt_vec
    }

    /// Generates LLVM IR for a for loop statement, and returns a vector of values
    /// that are created during that code gen. If there are no values, the vector is
    /// empty.
    fn for_stmt(&mut self,
                for_var_decl: &Box<Ast>,
                for_cond_expr: &Box<Ast>,
                for_step_expr: &Box<Ast>,
                stmts: &Box<Ast>) -> Vec<LLVMValueRef> {
        let mut return_stmt_vec = Vec::new();

        unsafe {
            let insert_bb = LLVMGetInsertBlock(self.builder);
            let fn_val = LLVMGetBasicBlockParent(insert_bb);

            let entry_bb = LLVMAppendBasicBlockInContext(self.context, fn_val, self.c_str("entry"));
            let for_bb = LLVMAppendBasicBlockInContext(self.context, fn_val, self.c_str("for"));
            let merge_bb = LLVMAppendBasicBlockInContext(self.context, fn_val, self.c_str("merge"));

            LLVMPositionBuilderAtEnd(self.builder, merge_bb);
            let phi_bb = LLVMBuildPhi(self.builder, self.double_ty(), self.c_str("phi"));
            LLVMPositionBuilderAtEnd(self.builder, entry_bb);

            // Codegen the var declaration and save the loop counter variable. We do this
            // first to store the loop var and to make sure it's allocated.
            self.gen_stmt(&for_var_decl.clone());
            LLVMBuildBr(self.builder, for_bb);
            LLVMPositionBuilderAtEnd(self.builder, for_bb);

            // Codegen the for loop body
            let mut stmt_vals = self.gen_stmt(&stmts.clone());
            return_stmt_vec.extend(stmt_vals.clone());

            // Codegen the loop step counter
            self.gen_stmt(&for_step_expr.clone());

            // Codegen the conditional for exit the loop
            let cond_val = self.gen_stmt(&for_cond_expr.clone())[0];
            LLVMBuildCondBr(self.builder, cond_val, for_bb, merge_bb);

            let for_end_bb = LLVMGetInsertBlock(self.builder);
            LLVMPositionBuilderAtEnd(self.builder, merge_bb);
            LLVMAddIncoming(phi_bb, stmt_vals.as_mut_ptr(), vec![for_end_bb].as_mut_ptr(), 1);
        }

        return_stmt_vec
    }

    /// Builds an alloca instruction at the beginning of a function so we can store
    /// parameters on the function stack. This uses a new builder so the current builder
    /// doesn't move positions. We would have to move it back to its original spot, which
    /// makes this function more complex than it needs to be.
    fn build_entry_bb_alloca(&mut self, func: LLVMValueRef, ty_rec: TyRec, name: &str) -> LLVMValueRef {
        unsafe {
            let builder = LLVMCreateBuilderInContext(self.context);
            let entry_bb = LLVMGetEntryBasicBlock(func);
            let entry_first_instr = LLVMGetFirstInstruction(entry_bb);
            LLVMPositionBuilder(builder, entry_bb, entry_first_instr);

            let llvm_ty = self.llvm_ty_from_ty_rec(&ty_rec);
            let c_name = self.c_str(name);
            LLVMBuildAlloca(builder, llvm_ty, c_name)
        }
    }

    /// Converts a TyRec type to an LLVMTypeRef
    fn llvm_ty_from_ty_rec(&self, ty_rec: &TyRec) -> LLVMTypeRef {
        match ty_rec.ty.clone().unwrap() {
            TyName::String => self.str_ty(),
            TyName::Num => self.double_ty(),
            TyName::Bool => self.i8_ty(),
            TyName::Void => self.void_ty(),
            TyName::Class(name) => {
                // Retrieve the class type from the class table, and return a pointer to it.
                // TODO: error checking here
                let class_ty = self.classtab.retrieve(&name).unwrap();
                unsafe { LLVMPointerType(class_ty, 0) }
            },
            TyName::Symbolic(_) => panic!("llvm gen: Uninferred type found!")
        }
    }

    /// Converts a vector of TyRecs into a vector of LLVMTypeRefs
    fn llvm_tys_from_ty_rec_arr(&self, ty_recs: &Vec<TyRec>) -> Vec<LLVMTypeRef> {
        let mut llvm_tys = Vec::new();
        for ty_rec in ty_recs {
            llvm_tys.push(self.llvm_ty_from_ty_rec(&ty_rec));
        }

        llvm_tys
    }

    /// Creates a new LLVMValueRef from a binary expression. The type of LLVM IR is determined by
    /// the operator type passed in. We assume that the LHS and RHS values given here are fully
    /// generated already. Comparison instructions are built from each function argument, if the
    /// operator given is of the logical type.
    /// We return None if the operator given is not supported.
    fn llvm_val_from_op(&mut self, op: &TknTy, lhs: LLVMValueRef, rhs: LLVMValueRef) -> Option<LLVMValueRef> {
        unsafe {
            match op {
                TknTy::Plus => Some(LLVMBuildFAdd(self.builder, lhs, rhs, self.c_str("addtmp"))),
                TknTy::Minus => Some(LLVMBuildFSub(self.builder, lhs, rhs, self.c_str("subtmp"))),
                TknTy::Star => Some(LLVMBuildFMul(self.builder, lhs, rhs, self.c_str("multmp"))),
                TknTy::Slash => Some(LLVMBuildFDiv(self.builder, lhs, rhs, self.c_str("divtmp"))),
                TknTy::AmpAmp | TknTy::And => Some(LLVMBuildAnd(self.builder, lhs, rhs, self.c_str("andtmp"))),
                TknTy::PipePipe | TknTy::Or => Some(LLVMBuildOr(self.builder, lhs, rhs, self.c_str("ortmp"))),
                TknTy::Lt => Some(LLVMBuildFCmp(self.builder,
                                                LLVMRealPredicate::LLVMRealULT,
                                                lhs,
                                                rhs,
                                                self.c_str("lttmp"))),
                TknTy::Gt => Some(LLVMBuildFCmp(self.builder,
                                                LLVMRealPredicate::LLVMRealUGT,
                                                lhs,
                                                rhs,
                                                self.c_str("gttmp"))),
                TknTy::LtEq => Some(LLVMBuildFCmp(self.builder,
                                                  LLVMRealPredicate::LLVMRealULE,
                                                  lhs,
                                                  rhs,
                                                  self.c_str("ltetmp"))),
                TknTy::GtEq => Some(LLVMBuildFCmp(self.builder,
                                                  LLVMRealPredicate::LLVMRealUGE,
                                                  lhs,
                                                  rhs,
                                                  self.c_str("gtetmp"))),
                TknTy::EqEq => Some(LLVMBuildFCmp(self.builder,
                                                  LLVMRealPredicate::LLVMRealUEQ,
                                                  lhs,
                                                  rhs,
                                                  self.c_str("eqtmp"))),
                TknTy::BangEq => Some(LLVMBuildFCmp(self.builder,
                                                    LLVMRealPredicate::LLVMRealUNE,
                                                    lhs,
                                                    rhs,
                                                    self.c_str("neqtmp"))),
                _ => None
            }
        }
    }

    fn void_ty(&self) -> LLVMTypeRef {
        unsafe { LLVMVoidTypeInContext(self.context) }
    }

    fn str_ty(&self) -> LLVMTypeRef {
        unsafe { LLVMPointerType(self.i8_ty(), 0) }
    }

    fn double_ty(&self) -> LLVMTypeRef {
        unsafe { LLVMDoubleTypeInContext(self.context) }
    }

    fn i8_ty(&self) -> LLVMTypeRef {
        unsafe { LLVMInt8TypeInContext(self.context) }
    }

    fn c_str(&mut self, s: &str) -> *mut i8 {
        let cstr = CString::new(s).unwrap();
        let cstr_ptr = cstr.as_ptr() as *mut _;
        self.strings.push(cstr);

        cstr_ptr
    }

    fn error(&mut self, ty: GenErrTy) {
        let err = GenErr::new(ty);
        self.errors.push(err);
    }
}
