/// Pre-built parsers for convenience.
pub mod parser;

use std::sync::{Arc, RwLock};

/// Generic parser trait.
/// Only `Output` and `parse_unprotected` need to be defined to apply the trait.
pub trait Parser {
  type Output;
  /// Parse `data` given `offset`.
  /// May leave `offset` in an incorrect state on failure.
  fn parse_unprotected(self: &mut Self, data: &[u8], offset: &mut usize) -> ParseResult<Self::Output>;
  /// A wrapper around `parse_unprotected` that resets `offset` to what it was before parsing on failure.
  fn parse(self: &mut Self, data: &[u8], offset: &mut usize) -> ParseResult<Self::Output> {
    let offset_before = *offset;
    let parse_result = self.parse_unprotected(data, offset);
    if parse_result.is_err() {
      *offset = offset_before;
    }
    parse_result
  }
  /// Helper method to contain `self` in a `Box<Self>`.
  fn into_boxed(self: Self) -> Box<Self> where Self: Sized {
    Box::new(self)
  }
  /// Helper method to contain `Self` in a `Box`, made generic to `Box<dyn Parser>`.
  fn into_boxed_generic(self: Self) -> Box<dyn Parser<Output = Self::Output>> where Self: Sized + 'static {
    Box::new(self) as Box<dyn Parser<Output = Self::Output>>
  }
  /// Helper method to contain `Self` in an `Arc<RwLock<Self>>`.
  fn into_arc_rwlock(self: Self) -> Arc<RwLock<Self>> where Self: Sized {
    Arc::new(RwLock::new(self))
  }
  /// Helper method to contain `Self` in an `Arc<RwLock<Self>>`, made generic to `Arc<RwLock<dyn Parser>>`.
  fn into_arc_rwlock_generic(self: Self) -> Arc<RwLock<dyn Parser<Output = Self::Output>>> where Self: Sized + 'static {
    Arc::new(RwLock::new(self)) as Arc<RwLock<dyn Parser<Output = Self::Output>>>
  }
}

/// A wrapper around `Result` with `ParseError` as the error.
pub type ParseResult<T> = Result<T, ParseError>;

/// A container for all the different possible errors when parsing.
#[derive(Debug)]
pub enum ParseError {
    InvalidData,
    NotEnoughData,
    LockedSubparser,
    Io(std::io::Error),
}