extern crate llvm_sys;
extern crate kolgac;
extern crate error;

macro_rules! c_str {
    ($s:expr) => (
        concat!($s, "\0").as_ptr() as *const i8
    );
}

pub mod codegen;
pub mod valtab;
pub mod classtab;
pub mod fpm;
pub mod objgen;
