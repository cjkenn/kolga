use std::collections::HashMap;

// Rudimentary command line flag "parsing"
// TODO: probably should get around to using structopt or clap?
static HELP_FLAG: &str = "--help";
static AST_FLAG: &str = "--dump-ast";
static IR_FLAG: &str = "--dump-ir";

#[derive(Default, Debug, Clone)]
pub struct KolgaOpts {
    pub filename: String,
    pub dump_ast: bool,
    pub dump_ir: bool,
}

impl KolgaOpts {
    pub fn new() -> KolgaOpts {
        KolgaOpts::default()
    }

    pub fn from_args(args: Vec<String>) -> Result<KolgaOpts, String> {
        if args.len() < 2 {
            return Err(usage());
        }

        if &args[1] == HELP_FLAG {
            return Err(usage());
        }

        let filename = args[1].clone();
        let mut arg_map: HashMap<String, bool> = [
            (String::from(AST_FLAG), false),
            (String::from(IR_FLAG), false),
        ]
            .iter()
            .cloned()
            .collect();

        for arg in args {
            if arg_map.contains_key(&arg) {
                arg_map.insert(arg, true);
            }
        }

        let opts = KolgaOpts {
            filename: filename,
            dump_ast: *arg_map.get(AST_FLAG).unwrap(),
            dump_ir: *arg_map.get(IR_FLAG).unwrap(),
        };

        Ok(opts)
    }
}

fn usage() -> String {
    let help = "
usage: kolga [filename] [options]

flags:
        --dump-ast  print the contents of the AST to stdout after parsing
        --dump-ir   print LLVM IR to stdout after codegen
        --help      display this message
";

    String::from(help)
}
