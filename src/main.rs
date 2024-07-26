use parser::Token;
mod compiler;
mod object;
mod parser;
mod value;
fn main() {
    let code: String = String::from("1 + 1 = 2");
    let mut parser = parser::Parser::new(&code);

    loop {
        match parser.parse_next() {
            Some(token) => {
                println!("{:?}", token);
                match token {
                    Token::TkEof => break,
                    _ => continue,
                }
            }
            None => break,
        }
    }

    let compiler = compiler::Compiler::new(&code);
}
