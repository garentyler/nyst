use crate::*;

use std::cmp::PartialEq;

/// Parses and then ignores the result, only advancing `offset`.
#[derive(Copy, Clone)]
pub struct IgnoreParser<'a, A> {
  pub parser: &'a dyn Parser<Output = A>,
}
impl<'a, A> IgnoreParser<'a, A> {
  pub fn new(parser: &'a dyn Parser<Output = A>) -> Self {
    IgnoreParser {
      parser,
    }
  }
}
impl<'a, A> Parser for IgnoreParser<'a, A> {
  type Output = ();

  fn parse_unprotected(&self, data: &[u8], offset: &mut usize) -> ParseResult<Self::Output> {
    match self.parser.parse(data, offset) {
      Ok(_result) => Ok(()),
      Err(err) => Err(err),
    }
  }
}

/// Parses the left parser and then the right parser, and then joins the result in a tuple.
#[derive(Copy, Clone)]
pub struct AndParser<'a, 'b, A, B> {
  pub left_parser: &'a dyn Parser<Output = A>,
  pub right_parser: &'b dyn Parser<Output = B>,
}
impl<'a, 'b, A, B> AndParser<'a, 'b, A, B> {
  pub fn new(left_parser: &'a dyn Parser<Output = A>, right_parser: &'b dyn Parser<Output = B>) -> Self {
    AndParser {
      left_parser,
      right_parser,
    }
  }
}
impl<'a, 'b, A, B> Parser for AndParser<'a, 'b, A, B> {
  type Output = (A, B);

  fn parse_unprotected(&self, data: &[u8], offset: &mut usize) -> ParseResult<Self::Output> {
    let left_result = self.left_parser.parse(data, offset)?;
    let right_result = self.right_parser.parse(data, offset)?;
    Ok((left_result, right_result))
  }
}

/// A container for either the left result or the right result.
#[derive(Copy, Clone, PartialEq)]
pub enum OrParserSuccess<A, B> {
  Left(A),
  Right(B),
}
/// Parses the left parser and then the right parser, returning the first successful result.
#[derive(Copy, Clone)]
pub struct OrParser<'a, 'b, A, B> {
  pub left_parser: &'a dyn Parser<Output = A>,
  pub right_parser: &'b dyn Parser<Output = B>,
}
impl<'a, 'b, A, B> OrParser<'a, 'b, A, B> {
  pub fn new(left_parser: &'a dyn Parser<Output = A>, right_parser: &'b dyn Parser<Output = B>) -> Self {
    OrParser {
      left_parser,
      right_parser,
    }
  }
}
impl<'a, 'b, A, B> Parser for OrParser<'a, 'b, A, B> {
  type Output = OrParserSuccess<A, B>;

  fn parse_unprotected(&self, data: &[u8], offset: &mut usize) -> ParseResult<OrParserSuccess<A, B>> {
    if let Ok(result) = self.left_parser.parse(data, offset) {
      ParseResult::Ok(OrParserSuccess::Left(result))
    } else {
      match self.right_parser.parse(data, offset) {
        Ok(result) => Ok(OrParserSuccess::Right(result)),
        Err(e) => Err(e),
      }
    }
  }
}

/// Repeatedly applies a parser over a given range.
#[derive(Clone)]
pub struct RangeParser<'a, A> {
  pub parser: &'a dyn Parser<Output = A>,
  pub range: std::ops::Range<usize>,
}
impl<'a, A> RangeParser<'a, A> {
  pub fn range(parser: &'a dyn Parser<Output = A>, range: std::ops::Range<usize>) -> Self {
    RangeParser {
      parser,
      range,
    }
  }
  pub fn repetitions(parser: &'a dyn Parser<Output = A>, repetitions: usize) -> Self {
    RangeParser {
      parser,
      range: repetitions..repetitions,
    }
  }
}
impl<'a, A> Parser for RangeParser<'a, A> {
  type Output = Vec<A>;

  fn parse_unprotected(&self, data: &[u8], offset: &mut usize) -> ParseResult<Self::Output> {
    let mut out = vec![];
    // Parse the minimum amount required.
    for _ in 0..self.range.start {
      out.push(self.parser.parse(data, offset)?);
    }
    // Parse the optional ones.
    let mut index = out.len() - 1;
    loop {
      index += 1;
      if index >= self.range.end {
        break;
      }
      if let Ok(result) = self.parser.parse(data, offset) {
        out.push(result);
      }
    }
    Ok(out)
  }
}

/// Parses without advancing `offset`, returning `None` on error.
#[derive(Copy, Clone)]
pub struct PeekParser<'a, A> {
  pub parser: &'a dyn Parser<Output = A>,
}
impl<'a, A> PeekParser<'a, A> {
  pub fn new(parser: &'a dyn Parser<Output = A>) -> Self {
    PeekParser {
      parser,
    }
  }
}
impl<'a, A> Parser for PeekParser<'a, A> {
  type Output = Option<(A, usize)>;

  fn parse_unprotected(&self, data: &[u8], offset: &mut usize) -> ParseResult<Self::Output> {
    let offset_before = *offset;

    match self.parser.parse(data, offset) {
      Ok(result) => {
        let offset_delta = *offset - offset_before;
        *offset = offset_before;
        Ok(Some((result, offset_delta)))
      },
      Err(_) => Ok(None), 
    }
  }
}