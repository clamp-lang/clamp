extern crate nom;
mod module;
mod parser;

use crate::parser::parse;

fn main() {
    let code = "(fn hello '() (println \"Hello World\")) (export hello)";
    let ast = parse(code);
    println!("{:?}", ast);
}
