extern crate nom;
mod parser;

use crate::parser::parse;

fn main() {
    let code = "(fn hello '() (println \"Hello World\"))";
    println!("{:?}", parse(code));
}
