mod compiler;
mod object;
mod parser;
mod value;
fn main() {
    let code: String = String::from("true and (false or true) or false");
    let mut compiler = compiler::Compiler::new(&code);
    compiler.expression();
}
