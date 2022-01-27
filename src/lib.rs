/// Pre-built parsers for convenience.
pub mod parser;

/// Generic parser trait.
/// Only `Output` and `parse_unprotected` need to be defined to apply the trait.
pub trait Parser {
  type Output;
  /// Parse `data` given `offset`.
  /// May leave `offset` in an incorrect state on failure.
  fn parse_unprotected(&self, data: &[u8], offset: &mut usize) -> ParseResult<Self::Output>;
  /// A wrapper around `parse_unprotected` that resets `offset` to what it was before parsing on failure.
  fn parse(&self, data: &[u8], offset: &mut usize) -> ParseResult<Self::Output> {
    let offset_before = *offset;
    let parse_result = self.parse_unprotected(data, offset);
    if parse_result.is_err() {
      *offset = offset_before;
    }
    parse_result
  }
}

/// A wrapper around `Result` with `ParseError` as the error.
pub type ParseResult<T> = Result<T, ParseError>;

/// A container for all the different possible errors when parsing.
#[derive(Debug)]
pub enum ParseError {
    InvalidData,
    NotEnoughData,
    Other(Box<dyn std::error::Error>),
}