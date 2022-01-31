// examples/hex.rs
use nyst::parser::{AndParser, LiteralParser, RangeParser};
use nyst::prelude::*;

fn main() {
    let data = b"#bf2b49".to_vec();
    let color = HexCodeParser {}.parse(data.as_slice()).unwrap().0;
    println!("input: {:?}\noutput: {:?}", data, color);
    assert_eq!(color.red, 191);
    assert_eq!(color.green, 43);
    assert_eq!(color.blue, 73);
}

#[derive(Debug)]
struct Color {
    pub red: u8,
    pub green: u8,
    pub blue: u8,
}

#[derive(Clone)]
struct HexCodeParser {}
impl<'parser> Parser<'parser> for HexCodeParser {
    type Input = u8;
    type Output = Color;

    fn parse(&self, data: &'parser [Self::Input]) -> ParseResult<Self::Output> {
        let start_hash = LiteralParser::new(b'#');
        let hex_to_byte = HexToByteParser {};
        let three_hex_pairs = RangeParser::repetitions(&hex_to_byte, 3);
        let result = AndParser::new(&start_hash, &three_hex_pairs).parse(data)?;

        let (result, units_read) = result;
        Ok((
            Color {
                red: result.1[0],
                green: result.1[1],
                blue: result.1[2],
            },
            units_read,
        ))
    }
}

#[derive(Clone)]
struct HexToByteParser {}
impl<'parser> Parser<'parser> for HexToByteParser {
    type Input = u8;
    type Output = u8;

    fn parse(&self, data: &'parser [Self::Input]) -> ParseResult<Self::Output> {
        if data.len() < 2 {
            return Err(ParseError::NotEnoughData);
        }
        let data_str = std::str::from_utf8(&data[0..2]);
        if data_str.is_err() {
            return Err(ParseError::InvalidData);
        }
        let value = u8::from_str_radix(data_str.unwrap(), 16);
        if value.is_err() {
            return Err(ParseError::InvalidData);
        }
        Ok((value.unwrap(), 2))
    }
}
