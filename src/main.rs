mod compiler;
mod object;
mod parser;
mod value;
fn main() {
    let code: String = String::from("0 * (1 * 2 * (3 + 4))");
    let mut compiler = compiler::Compiler::new(&code);
    compiler.expression();
}
