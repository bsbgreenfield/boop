use vm::Vm;

mod compiler;
mod object;
mod parser;
mod r#type;
mod value;
mod vm;
use std::fs::File;
use std::io::Read;

fn string_from_file(path: &str) -> std::io::Result<String> {
    let mut file = File::open(path)?;
    let mut contents = String::new();
    file.read_to_string(&mut contents)?;
    Ok(contents)
}

fn main() {
    let file_path = "./src/code.txt";
    match string_from_file(file_path) {
        Ok(contents) => {
            let mut vm = Vm::new(&contents);
            let _ = vm.run();
        }
        Err(e) => eprintln!("{e}"),
    }
}
