use vm::Vm;

mod compiler;
mod object;
mod parser;
mod value;
mod vm;
fn main() {
    let code: String = String::from("\"hello\" + \"world\"");
    let mut vm = Vm::new(&code);
    let _ = vm.run();
}
