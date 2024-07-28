use parser::Token;
mod compiler;
mod object;
mod parser;
mod value;
fn main() {
    let code: String = String::from("1 + 1 * 2");
    let mut compiler = compiler::Compiler::new(&code);
    compiler.expression();
}
