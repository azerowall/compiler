mod ast;
mod parser;
mod codegen;


use std::fs::File;
use std::io::prelude::*;

use parser::code_parser;
use codegen::Codegen;




fn main() {
    let mut code = String::new();
    {
        let mut file = File::open("program").unwrap();
        file.read_to_string(&mut code).unwrap();
    }

    let ast = code_parser::program(&code);

    match ast {
        Ok(ast) => {
            //println!("{:#?}", ast);
    
            let mut cg = Codegen::new();
            cg.gen(&ast);
            cg.dump_to_file("out.s");
            cg.dump();
        },
        Err(err) => {
            println!("{:#?}", err);
        }
    }
}