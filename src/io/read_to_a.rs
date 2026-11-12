// Why does std not have a `read_to_vec` function and why is its
// `read_to_string` method unlike the function?

use std::{fs::File, io::Read};

use bstr::BString;

pub trait ReadToA {
    fn read_to_a_vec(&mut self) -> std::io::Result<Vec<u8>>;
    fn read_to_a_bstring(&mut self) -> std::io::Result<BString>;
    fn read_to_a_string(&mut self) -> std::io::Result<String>;
}

impl ReadToA for File {
    fn read_to_a_vec(&mut self) -> std::io::Result<Vec<u8>> {
        let mut vec = Vec::new();
        self.read_to_end(&mut vec)?;
        Ok(vec)
    }

    fn read_to_a_bstring(&mut self) -> std::io::Result<BString> {
        Ok(BString::from(self.read_to_a_vec()?))
    }

    fn read_to_a_string(&mut self) -> std::io::Result<String> {
        std::io::read_to_string(self)
    }
}
