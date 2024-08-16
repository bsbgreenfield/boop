use vm::Vm;

mod compiler;
mod object;
mod parser;
mod r#type;
mod value;
mod vm;
fn main() {
    let code: String = String::from("int i = 1");
    let mut vm = Vm::new(&code);
    let _ = vm.run();
}
