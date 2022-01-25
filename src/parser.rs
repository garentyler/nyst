use crate::*;
use std::sync::{Arc, RwLock};

pub struct AndParser<A, B> {
  pub left_parser: Arc<RwLock<dyn Parser<Output = A>>>,
  pub right_parser: Arc<RwLock<dyn Parser<Output = B>>>,
}
impl<A, B> AndParser<A, B> {
  pub fn new(left_parser: &Arc<RwLock<dyn Parser<Output = A>>>, right_parser: &Arc<RwLock<dyn Parser<Output = B>>>) -> Self {
    AndParser {
      left_parser: Arc::clone(left_parser),
      right_parser: Arc::clone(right_parser),
    }
  }
}
impl<A, B> Clone for AndParser<A, B> {
  fn clone(&self) -> Self {
    AndParser {
      left_parser: Arc::clone(&self.left_parser),
      right_parser: Arc::clone(&self.right_parser),
    }
  }
}
impl<A, B> Parser for AndParser<A, B> {
  type Output = (A, B);

  fn parse_unprotected(self: &mut Self, data: &[u8], offset: &mut usize) -> ParseResult<Self::Output> {
    let left_result = match self.left_parser.try_write() {
      Ok(p) => p,
      Err(_) => return Err(ParseError::LockedSubparser),
    }.parse(data, offset)?;
    let right_result = match self.right_parser.try_write() {
      Ok(p) => p,
      Err(_) => return Err(ParseError::LockedSubparser),
    }.parse(data, offset)?;
    // let left_result = self.left_parser.parse(data, offset)?;
    // let right_result = self.right_parser.parse(data, offset)?;
    Ok((left_result, right_result))
  }
}

pub enum OrParserSuccess<A, B> {
  Left(A),
  Right(B),
}
pub struct OrParser<A, B> {
  pub left_parser: Arc<RwLock<dyn Parser<Output = A>>>,
  pub right_parser: Arc<RwLock<dyn Parser<Output = B>>>,
}
impl<A, B> OrParser<A, B> {
  pub fn new(left_parser: &Arc<RwLock<dyn Parser<Output = A>>>, right_parser: &Arc<RwLock<dyn Parser<Output = B>>>) -> Self {
    OrParser {
      left_parser: Arc::clone(left_parser),
      right_parser: Arc::clone(right_parser),
    }
  }
}
impl<A, B> Clone for OrParser<A, B> {
  fn clone(&self) -> Self {
    OrParser {
      left_parser: Arc::clone(&self.left_parser),
      right_parser: Arc::clone(&self.right_parser),
    }
  }
}
impl<A, B> Parser for OrParser<A, B> {
  type Output = OrParserSuccess<A, B>;

  fn parse_unprotected(self: &mut Self, data: &[u8], offset: &mut usize) -> ParseResult<OrParserSuccess<A, B>> {
    let left_result = match self.left_parser.try_write() {
      Ok(p) => p,
      Err(_) => return Err(ParseError::LockedSubparser),
    }.parse(data, offset);
    match left_result {
      Ok(result) => return ParseResult::Ok(OrParserSuccess::Left(result)),
      Err(_) => {},
    }
    let right_result = match self.right_parser.try_write() {
      Ok(p) => p,
      Err(_) => return Err(ParseError::LockedSubparser),
    }.parse(data, offset);
    match right_result {
      Ok(result) => Ok(OrParserSuccess::Right(result)),
      Err(e) => Err(e),
    }
  }
}

pub struct RangeParser<A> {
  pub parser: Arc<RwLock<dyn Parser<Output = A>>>,
  pub range: std::ops::Range<usize>,
}
impl<A> RangeParser<A> {
  pub fn range(parser: &Arc<RwLock<dyn Parser<Output = A>>>, range: std::ops::Range<usize>) -> Self {
    RangeParser {
      parser: Arc::clone(parser),
      range,
    }
  }
  pub fn repetitions(parser: &Arc<RwLock<dyn Parser<Output = A>>>, repetitions: usize) -> Self {
    RangeParser {
      parser: Arc::clone(parser),
      range: std::ops::Range {
        start: repetitions,
        end: repetitions,
      },
    }
  }
}
impl<A> Clone for RangeParser<A> {
  fn clone(&self) -> Self {
    RangeParser {
      parser: Arc::clone(&self.parser),
      range: self.range.clone(),
    }
  }
}
impl<A> Parser for RangeParser<A> {
  type Output = Vec<A>;

  fn parse_unprotected(self: &mut Self, data: &[u8], offset: &mut usize) -> ParseResult<Self::Output> {
    let mut out = vec![];
    let mut parser = match self.parser.try_write() {
      Ok(p) => p,
      Err(_) => return Err(ParseError::LockedSubparser),
    };
    // Parse the minimum amount required.
    for _ in 0..self.range.start {
      out.push(parser.parse(data, offset)?);
    }
    // Parse the optional ones.
    let mut index = out.len() - 1;
    loop {
      index += 1;
      if index >= self.range.end {
        break;
      }
      if let Ok(result) = parser.parse(data, offset) {
        out.push(result);
      }
    }
    Ok(out)
  }
}

pub struct PeekParser<A> {
  pub parser: Arc<RwLock<dyn Parser<Output = A>>>,
}
impl<A> PeekParser<A> {
  pub fn new(parser: &Arc<RwLock<dyn Parser<Output = A>>>) -> Self {
    PeekParser {
      parser: Arc::clone(parser),
    }
  }
}
impl<A> Clone for PeekParser<A> {
  fn clone(&self) -> Self {
    PeekParser {
      parser: Arc::clone(&self.parser),
    }
  }
}
impl<A> Parser for PeekParser<A> {
  type Output = Option<(A, usize)>;

  fn parse_unprotected(self: &mut Self, data: &[u8], offset: &mut usize) -> ParseResult<Self::Output> {
    let offset_before = *offset;

    let mut parser = match self.parser.try_write() {
      Ok(p) => p,
      Err(_) => return Err(ParseError::LockedSubparser),
    };

    match parser.parse(data, offset) {
      Ok(result) => {
        let offset_delta = *offset - offset_before;
        *offset = offset_before;
        Ok(Some((result, offset_delta)))
      },
      Err(_) => Ok(None), 
    }
  }
}