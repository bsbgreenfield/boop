use std::cell::RefCell;

thread_local! {
    static BUFFER: RefCell<Option<String>> = const {RefCell::new(None)}
}
pub fn capture<F: FnOnce()>(body: F) -> String {
    BUFFER.with(|b| *b.borrow_mut() = Some(String::new()));
    body();
    drain()
}

pub fn drain() -> String {
    BUFFER.with(|b| b.borrow_mut().take().unwrap_or_default())
}

pub fn is_capturing() -> bool {
    BUFFER.with(|b| b.borrow().is_some())
}

pub fn emit(line: &str) {
    BUFFER.with(|b| match b.borrow_mut().as_mut() {
        Some(out) => {
            out.push_str(line);
            out.push('\n');
        }
        None => println!("{line}"),
    })
}
