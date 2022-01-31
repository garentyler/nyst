/// Pre-built parsers for convenience.
pub mod parser;

/// Generic parser trait.
/// Only `Output` and `parse` need to be defined to apply the trait.
pub trait Parser<'parser> {
    type Input;
    type Output;
    /// Parse `data`, and return a `ParseResult`.
    fn parse(&self, data: &'parser [Self::Input]) -> ParseResult<Self::Output>;
}

/// A wrapper around `Result` with `ParseError` as the error.
pub type ParseResult<T> = Result<(T, usize), ParseError>;

/// A container for all the different possible errors when parsing.
#[derive(Debug)]
pub enum ParseError {
    InvalidData,
    NotEnoughData,
    Other(&'static str),
    GenericError(Box<dyn std::error::Error>),
}

/// Includes all of the necessary traits for working with nyst.
pub mod prelude {
    pub use super::{ParseError, ParseResult, Parser};
}
