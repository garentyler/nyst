/// Pre-built parsers for convenience.
pub mod parser;

/// Generic parser trait.
/// Only `Output` and `parse` need to be defined to apply the trait.
pub trait Parser {
    type Input;
    type Output;
    /// Parse `data`, and return a `ParseResult`.
    fn parse(&self, data: &[Self::Input]) -> ParseResult<Self::Output>;
}

/// A wrapper around `Result` with `ParseError` as the error.
pub type ParseResult<T> = Result<(T, usize), ParseError>;

/// A container for all the different possible errors when parsing.
#[derive(Debug, PartialEq)]
pub enum ParseError {
    InvalidData,
    NotEnoughData,
    Other(&'static str),
}

/// Includes all of the necessary traits for working with nyst.
pub mod prelude {
    pub use super::{ParseError, ParseResult, Parser};
}
