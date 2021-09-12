extern crate nom;
mod module;
mod parser;

use crate::parser::parse;

fn main() {
    let code = "(def hello (fn (name) (print \"Hello\" name)))";
    let ast = parse(code);
    println!("{:?}", ast);
}
