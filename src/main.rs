mod ast;
mod infer;
mod ty;

use lalrpop_util::lalrpop_mod;
use std::iter::repeat;
lalrpop_mod!(pub parser);

fn main() {
    let filename = std::env::args().nth(1).expect("no filename given");
    let input = std::fs::read_to_string(filename).expect("cannot read file");
    let res = parser::ProgramParser::new().parse(&input);
    match res {
        Ok(program) => {
            let mut inferer = infer::Infer::new();
            let res = inferer.type_infer(&program);
            match res {
                Ok(mut typed_program) => {
                    typed_program.reverse();
                    for func in typed_program {
                        println!("{}", func);
                    }
                }
                Err(e) => {
                    println!("error: {:#?}", e);
                    std::process::exit(1);
                }
            }
        }
        Err(e) => {
            // pretty preint error
            // find line and column
            println!(
                "error: {}",
                e.map_location(|l| {
                    // find where the error is
                    let mut linenum = 1;
                    let mut colnum = 1;
                    for c in input.chars().take(l) {
                        if c == '\n' {
                            linenum += 1;
                            colnum = 1;
                        } else {
                            colnum += 1;
                        }
                    }
                    let line = input.lines().nth(linenum - 1).unwrap();
                    let space = repeat(" ").take(colnum - 1).collect::<String>();
                    let pointer = format!("{}\n{}^", line, space);
                    format!("{}:{}\n{}", linenum, colnum, pointer)
                })
            );
            std::process::exit(1);
        }
    }
}
