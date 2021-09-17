extern crate nom;
mod parser;

use crate::parser::parse;

fn main() {
    let code = "
(: add (-> i32 i32 i32))
(def add (fn (a b) (+ a b)))
";
    let ast = parse(code);
    println!("{:?}", ast);
}
