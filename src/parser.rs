use crate::reader::Reader;
use crate::Result;
use crate::{error::NamelistError, namelist::Namelist};

use crate::constant::{Constant, LiteralConstant};
use crate::constant_list::ConstantList;
use crate::value::{Map, Value};

pub struct Parser<'a> {
    reader: Reader<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(input: &'a str) -> Parser<'a> {
        Parser {
            reader: Reader::new(input),
        }
    }

    fn skip_value_seperator(&mut self) {
        if let Some(b',') = self.reader.peek_next_byte() {
            self.reader.skip_byte();
        }
    }

    fn peek_identifer(&mut self) -> bool {
        self.reader
            .peek_next_byte()
            .map(|b| b.is_ascii_alphabetic() || b == b'_')
            .unwrap_or(false)
    }

    fn parse_identifier(&mut self) -> Result<&'a str> {
        let ident_begin = self.reader.offset();
        let Some(first_byte) = self.reader.peek_next_byte() else {
            return Err(self.unexpected_eof());
        };

        if !first_byte.is_ascii_alphabetic() {
            return Err(NamelistError::unexpected_byte(
                first_byte,
                "identifier",
                self.reader.position(),
            ));
        }

        self.reader.skip_byte();

        while self
            .reader
            .peek_next_byte()
            .map(|b| b.is_ascii_alphanumeric() || b == b'_')
            .unwrap_or(false)
        {
            self.reader.skip_byte();
        }
        let ident_end = self.reader.offset();
        Ok(self.reader.get_str(ident_begin, ident_end))
    }

    fn parse_bool(&mut self) -> Result<LiteralConstant> {
        let literal_begin = self.reader.offset();
        self.reader.skip_byte();
        while self
            .reader
            .peek_next_byte()
            .map(|b| b.is_ascii_alphabetic() || b == b'.')
            .unwrap_or(false)
        {
            self.reader.skip_byte();
        }
        let literal_end = self.reader.offset();

        let literal = self.reader.get_str(literal_begin, literal_end);

        if literal == ".TRUE." || literal == ".true." {
            Ok(LiteralConstant::Bool(true))
        } else if literal == ".FALSE." || literal == ".false." {
            Ok(LiteralConstant::Bool(false))
        } else {
            Err(NamelistError::unexpected_token(
                literal,
                ".TRUE. or .FALSE.",
                self.reader.position(),
            ))
        }
    }

    fn parse_numeric(&mut self) -> Result<LiteralConstant> {
        let literal_begin = self.reader.offset();
        let mut is_float = false;

        while let Some(c) = self.reader.peek_next_byte() {
            if c == b'.' {
                if is_float {
                    break;
                } else {
                    is_float = true;
                }
            } else if !c.is_ascii_digit() && c != b'-' {
                break;
            }
            self.reader.skip_byte();
        }

        let slice = self.reader.get_str(literal_begin, self.reader.offset());
        if is_float {
            Ok(LiteralConstant::Float(slice.parse::<f64>().map_err(
                |e| NamelistError::parse_float(e, self.reader.position()),
            )?))
        } else {
            Ok(LiteralConstant::Int(slice.parse::<i64>().map_err(|e| {
                NamelistError::parse_int(e, self.reader.position())
            })?))
        }
    }

    fn parse_string(&mut self) -> Result<LiteralConstant> {
        let quote = self.reader.next_byte().unwrap();
        let mut bytes = Vec::new();

        while let Some(next_byte) = self.reader.next_byte() {
            if next_byte == quote {
                if self.reader.peek_next_byte() != Some(quote) {
                    return Ok(LiteralConstant::String(String::from_utf8(bytes).map_err(
                        |_e| NamelistError::SyntaxError(self.reader.position()),
                    )?));
                } else {
                    self.reader.skip_byte();
                }
            }
            bytes.push(next_byte);
        }
        Err(self.unexpected_eof())
    }

    fn peek_constant(&mut self) -> bool {
        match self.reader.peek_next_byte() {
            Some(b'"') => true,
            Some(b'\'') => true,
            Some(b'.') => true,
            Some(b'-') => true,
            Some(b) if b.is_ascii_digit() => true,
            _ => false,
        }
    }

    fn parse_literal_constant(&mut self) -> Result<LiteralConstant> {
        match self.reader.peek_next_byte() {
            Some(b'.') => self.parse_bool(),
            Some(b'-') => self.parse_numeric(),
            Some(b) if b.is_ascii_digit() => self.parse_numeric(),
            Some(b'"') | Some(b'\'') => self.parse_string(),
            Some(b',') => Ok(LiteralConstant::Null),
            Some(_) => return Err(self.parse_unexpected_token("a literal constant or ','")),
            None => return Err(self.unexpected_eof()),
        }
    }

    fn parse_constant(&mut self) -> Result<Constant> {
        self.reader.skip_whitespace();

        let first_literal = self.parse_literal_constant()?;

        self.reader.skip_whitespace();

        if let Some(b'*') = self.reader.peek_next_byte() {
            match first_literal {
                LiteralConstant::Int(i) if !i.is_negative() => {
                    self.reader.skip_byte();
                    self.reader.skip_whitespace();
                    let real_first_literal = self.parse_literal_constant()?;
                    Ok(Constant::Repeated(i as usize, real_first_literal))
                }
                _ => Err(NamelistError::SyntaxError(self.reader.position())),
            }
        } else {
            Ok(Constant::Literal(first_literal))
        }
    }

    fn parse_constant_list(&mut self) -> Result<ConstantList> {
        self.reader.skip_whitespace();
        let first_constant = self.parse_constant()?;
        let mut constant_list = ConstantList::Single(first_constant);
        self.reader.skip_whitespace();
        self.skip_value_seperator();
        self.reader.skip_whitespace();

        while self.peek_constant() {
            let next_value = self.parse_constant()?;
            if constant_list.is_type_compatible(&next_value) {
                constant_list.push(next_value);
            } else {
                panic!()
            }
            self.reader.skip_whitespace();
            self.skip_value_seperator();
            self.reader.skip_whitespace();
        }
        Ok(constant_list)
    }

    fn parse_name_value_pairs(&mut self, keyvals: &mut Map<String, Value>) -> Result<()> {
        let key = self.parse_identifier()?;
        self.reader.skip_whitespace();

        if self.reader.peek_next_byte() == Some(b'%') {
            self.reader.skip_byte();

            let next_keyvals = match keyvals
                .entry(key.to_owned())
                .or_insert_with(|| Value::Derived(Map::new()))
            {
                Value::Derived(m) => m,
                _ => {
                    return Err(NamelistError::AlreadyAssigned {
                        key: key.to_owned(),
                        position: self.reader.position(),
                    })
                }
            };
            self.reader.skip_whitespace();
            self.parse_name_value_pairs(next_keyvals)
        } else {
            // expecting '='
            match self.reader.next_byte() {
                Some(b'=') => (),
                Some(_) => return Err(self.parse_unexpected_token("/")),
                None => return Err(self.unexpected_eof()),
            }

            self.reader.skip_whitespace();

            let constant_list = self.parse_constant_list()?;

            if keyvals.contains_key(key) {
                return Err(NamelistError::AlreadyAssigned {
                    key: key.to_owned(),
                    position: self.reader.position(),
                });
            }

            keyvals.insert(key.to_owned(), Value::ConstantList(constant_list));

            Ok(())
        }
    }
    fn parse_begin(&mut self) -> Result<&'a str> {
        match self.reader.next_byte() {
            Some(b'&') => (),
            Some(_) => return Err(self.parse_unexpected_token("&")),
            None => return Err(self.unexpected_eof()),
        }

        self.parse_identifier()
    }

    fn parse_unexpected_token(&mut self, expected: &str) -> NamelistError {
        let pos = self.reader.position();
        let start_offset = self.reader.offset();
        while self
            .reader
            .peek_next_byte()
            .map(|b| !b.is_ascii_whitespace())
            .unwrap_or(false)
        {
            self.reader.skip_byte();
        }
        let end_offset = self.reader.offset();

        let found_token = self.reader.get_str(start_offset, end_offset);

        NamelistError::unexpected_token(found_token, expected, pos)
    }

    fn unexpected_eof(&mut self) -> NamelistError {
        NamelistError::UnexpectedEOF(self.reader.position())
    }

    pub fn parse(&mut self) -> Result<Namelist> {
        self.reader.skip_whitespace();
        let identifier = self.parse_begin()?;
        self.reader.skip_whitespace();

        let mut keyvals = Map::new();
        while self.peek_identifer() {
            self.parse_name_value_pairs(&mut keyvals)?;
            self.reader.skip_whitespace();
        }

        match self.reader.next_byte() {
            Some(b'/') => (),
            Some(_) => return Err(self.parse_unexpected_token("/")),
            None => return Err(self.unexpected_eof()),
        }

        Ok(Namelist::new(identifier, keyvals))
    }
}

impl<'a> Iterator for Parser<'a> {
    type Item = Result<Namelist>;

    fn next(&mut self) -> Option<Self::Item> {
        self.reader.skip_whitespace();
        if self.reader.is_eof() {
            Some(self.parse())
        } else {
            None
        }
    }
}
