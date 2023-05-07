#[derive(Debug, PartialEq)]
pub struct Position {
    pub(crate) line: usize,
    pub(crate) col: usize,
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.col)
    }
}

pub(crate) struct Reader<'a> {
    input: &'a str,
    read_pos: std::iter::Peekable<std::str::Bytes<'a>>,
    idx: usize,
    line: usize,
    col: usize,
}

impl<'a> Reader<'a> {
    pub fn new(input: &'a str) -> Reader<'a> {
        Reader {
            input,
            read_pos: input.bytes().peekable(),
            idx: 0,
            line: 0,
            col: 0,
        }
    }

    pub fn next_byte(&mut self) -> Option<u8> {
        match self.read_pos.next() {
            r @ Some(b'\n') => {
                self.line += 1;
                self.col = 0;
                self.idx += 1;
                r
            }
            r @ Some(_) => {
                self.col += 1;
                self.idx += 1;
                r
            }
            None => None,
        }
    }

    pub fn skip_byte(&mut self) {
        _ = self.next_byte();
    }

    pub fn peek_next_byte(&mut self) -> Option<u8> {
        self.read_pos.peek().map(|b| *b)
    }

    pub fn position(&self) -> Position {
        Position {
            line: self.line,
            col: self.col,
        }
    }

    pub fn skip_whitespace(&mut self) {
        while self
            .peek_next_byte()
            .map(|b| b.is_ascii_whitespace())
            .unwrap_or(false)
        {
            self.skip_byte();
        }
    }

    pub fn is_eof(&mut self) -> bool {
        self.peek_next_byte().is_some()
    }

    pub fn offset(&self) -> usize {
        self.idx
    }

    pub fn get_str(&self, start: usize, end: usize) -> &'a str {
        &self.input[start..end]
    }
}
