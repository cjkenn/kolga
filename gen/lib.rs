extern crate error;
extern crate kolgac;
extern crate llvm_sys;

macro_rules! c_str {
    ($s:expr) => {
        concat!($s, "\0").as_ptr() as *const i8
    };
}

pub mod classtab;
pub mod fpm;
pub mod llvm;
pub mod obj;
pub mod valtab;
