#[derive(Eq, Hash, PartialEq)]
pub struct DataType {
    name: String,
    copy: bool,
}

impl DataType {
    pub fn new_sized(name: &str) -> Self {
        DataType {
            name: String::from(name),
            copy: true,
        }
    }
    pub fn new_unsized(name: &str) -> Self {
        DataType {
            name: String::from(name),
            copy: false,
        }
    }
}
