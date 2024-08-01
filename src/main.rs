mod compiler;
mod object;
mod parser;
mod value;
fn main() {
    let code: String = String::from("(1) + 1234");
    let mut compiler = compiler::Compiler::new(&code);
    compiler.expression();
}
