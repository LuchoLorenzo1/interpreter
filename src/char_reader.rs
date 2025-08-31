use std::io::Read;

pub struct CharReader<R: Read> {
    reader: R,
    buf: Vec<u8>,
}

impl<R: Read> CharReader<R> {
    pub fn new(reader: R) -> Self {
        Self {
            reader,
            buf: Vec::with_capacity(4),
        }
    }
}

impl<R: Read> Iterator for CharReader<R> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        let mut byte = [0u8; 1];

        loop {
            if !self.buf.is_empty() {
                if let Ok(s) = std::str::from_utf8(&self.buf) {
                    let ch = s.chars().next().unwrap();
                    self.buf.clear();
                    return Some(ch);
                }
            }

            match self.reader.read(&mut byte) {
                Ok(0) => {
                    if self.buf.is_empty() {
                        return None;
                    } else {
                        panic!("Invalid UTF-8 sequence at end of file: {:?}", self.buf);
                    }
                }
                Ok(_) => {
                    self.buf.push(byte[0]);
                    if self.buf.len() > 4 {
                        panic!("Invalid UTF-8 sequence: {:?}", self.buf);
                    }
                }
                Err(e) => panic!("I/O error while reading: {}", e),
            }
        }
    }
}
