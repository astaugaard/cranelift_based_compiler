use std::{env, fs::{self, File}};

use crate::{anf::ast_to_anf, type_checking::type_check};

mod parser;
mod ast;
mod type_checking;
mod anf;
mod anf_to_cranelift;

fn compile(file: String) -> Result<(),String> {
    let (_,parse) = parser::parse(&file).map_err(|err| err.to_string())?;
    println!("{:?}", &parse);
    let types = type_check(&parse).map_err(|err| format!("{:?}", err))?;

    let anf = ast_to_anf(parse,&types);

    println!("anf: {:#?}",anf);

    let comp = anf_to_cranelift::compile(anf.0, anf.1)?;

    let mut file = File::create("output.o").unwrap();
    comp.object.write_stream(&mut file).unwrap();

    Ok(())
}


fn main() {
    let args:Vec<String> = env::args().collect();
    let file = &args[1];
    let file = fs::read_to_string(file).unwrap();

    match compile(file) {
        Ok(_) => { },
        Err(err) => panic!("error: {}", err),
    };
}
