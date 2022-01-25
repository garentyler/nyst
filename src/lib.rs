pub mod parser;

pub use std::sync::{Arc, RwLock};

pub trait Parser {
  type Output;
  fn parse_unprotected(self: &mut Self, data: &[u8], offset: &mut usize) -> ParseResult<Self::Output>;
  fn parse(self: &mut Self, data: &[u8], offset: &mut usize) -> ParseResult<Self::Output> {
    let offset_before = *offset;
    let parse_result = self.parse_unprotected(data, offset);
    if parse_result.is_err() {
      *offset = offset_before;
    }
    parse_result
  }
  fn into_boxed(self: Self) -> Box<Self> where Self: Sized {
    Box::new(self)
  }
  fn into_boxed_generic(self: Self) -> Box<dyn Parser<Output = Self::Output>> where Self: Sized + 'static {
    Box::new(self) as Box<dyn Parser<Output = Self::Output>>
  }
  fn into_arc_rwlock(self: Self) -> Arc<RwLock<Self>> where Self: Sized {
    Arc::new(RwLock::new(self))
  }
  fn into_arc_rwlock_generic(self: Self) -> Arc<RwLock<dyn Parser<Output = Self::Output>>> where Self: Sized + 'static {
    Arc::new(RwLock::new(self)) as Arc<RwLock<dyn Parser<Output = Self::Output>>>
  }
}

pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
pub enum ParseError {
    InvalidData,
    NotEnoughData,
    LockedSubparser,
    Io(std::io::Error),
}