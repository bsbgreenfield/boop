use vm::Vm;

mod compiler;
mod object;
mod parser;
mod value;
mod vm;
fn main() {
    let code: String = String::from("1 + 23 * (4+2)");
    let mut vm = Vm::new(&code);
    let _ = vm.run();
}
