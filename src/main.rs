#[macro_use]
extern crate lazy_static;
extern crate fnv;
extern crate itertools;
extern crate regex;

extern crate rustyline;
use rustyline::error::ReadlineError;
use rustyline::Editor;

#[macro_use]
mod reader;

fn main() {
    // `()` can be used when no completer is required
    let mut rl = Editor::<()>::new();
    if rl.load_history(".clamp-history").is_err() {
        eprintln!("No previous history.");
    }

    loop {
        let readline = rl.readline("user> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(&line);
                rl.save_history(".clamp-history").unwrap();
                if line.len() > 0 {
                    reader::read_str(line)
                }
            }
            Err(ReadlineError::Interrupted) => continue,
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                println!("Error: {:?}", err);
                break;
            }
        }
    }
}
