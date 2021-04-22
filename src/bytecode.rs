#[repr(u8)]
pub enum OpCode {
    Return,
}

pub struct Chunk {
    code: Vec<u8>,
}

impl Chunk {
    pub fn new() -> Self {
        Self { code: Vec::new() }
    }

    pub fn code(&self) -> &[u8] {
        &self.code[..]
    }

    pub fn write(&mut self, byte: u8) {
        self.code.push(byte);
    }
}
