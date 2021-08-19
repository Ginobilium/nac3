use rustpython_parser::ast;
use std::vec::Vec;

#[derive(Clone, Copy, PartialEq)]
pub struct FileID(u32);

#[derive(Clone, Copy, PartialEq)]
pub enum Location {
    CodeRange(FileID, ast::Location),
    Builtin,
}

pub struct FileRegistry {
    files: Vec<String>,
}

impl FileRegistry {
    pub fn new() -> FileRegistry {
        FileRegistry { files: Vec::new() }
    }

    pub fn add_file(&mut self, path: &str) -> FileID {
        let index = self.files.len() as u32;
        self.files.push(path.to_owned());
        FileID(index)
    }

    pub fn query_file(&self, id: FileID) -> &str {
        &self.files[id.0 as usize]
    }
}
