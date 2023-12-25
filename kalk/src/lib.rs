#![allow(clippy::unused_unit)]
#![allow(clippy::float_cmp)]
#![allow(clippy::clone_on_copy)] // the float type needs explicit cloning if the rug feature is enabled
mod analysis;
pub mod ast;
pub mod calculation_result;
pub mod errors;
mod integration_testing;
mod interpreter;
mod inverter;
pub mod kalk_value;
mod lexer;
mod numerical;
pub mod parser;
mod prelude;
mod radix;
mod symbol_table;
mod test_helpers;
pub mod text_utils;
