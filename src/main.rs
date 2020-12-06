mod ast;
mod parser;
mod codegen;


use std::fs::File;
use std::io::prelude::*;

use parser::code_parser;




fn main() {
    let mut code = String::new();
    {
        let mut file = File::open("program").unwrap();
        file.read_to_string(&mut code).unwrap();
    }
    
    println!("{}", code);

    let ast = code_parser::program(&code);
    
    if let Ok(ast) = ast {
        println!("{:#?}", ast);
        /*
        unsafe {
            codegen(&ast);
        }
        */
    } else {
        println!("{:#?}", ast);
    }
}