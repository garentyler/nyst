use crate::*;
use std::fmt::Debug;

/// Parses and then ignores the result, still returning the units read.
/// 
/// # Example
/// ```rust
/// use nyst::{prelude::*, parser::{LiteralParser, IgnoreParser}};
/// 
/// let a_parser = LiteralParser::new('a'); // This can be any struct that implements `nyst::Parser`.
/// 
/// let ignore_parser = IgnoreParser::new(&a_parser);
/// 
/// let result = ignore_parser.parse(&['a']);
/// assert_eq!(
///   result,
///   ParseResult::Ok((
///     (), // The result from the internal parser was ignored.
///     1, // The number of units read from the input.
///   ))
/// );
/// ```
#[derive(Copy, Clone)]
pub struct IgnoreParser<'parser, Input: Debug, Output> {
    pub parser: &'parser dyn Parser<'parser, Input = Input, Output = Output>,
}
impl<'parser, Input: Debug, Output> IgnoreParser<'parser, Input, Output> {
    pub fn new(parser: &'parser dyn Parser<'parser, Input = Input, Output = Output>) -> Self {
        IgnoreParser { parser }
    }
}
impl<'parser, Input: Debug, Output> Parser<'parser> for IgnoreParser<'parser, Input, Output> {
    type Input = Input;
    type Output = ();

    fn parse(&self, data: &'parser [Self::Input]) -> ParseResult<Self::Output> {
        match self.parser.parse(data) {
            Ok((_result, offset)) => Ok(((), offset)),
            Err(error) => Err(error),
        }
    }
}

/// Parses the left parser and then the right parser, and then joins the result in a tuple.
/// 
/// `AndParser` will parse the left parser first. If the left parser fails, that error is passed on.
/// After running the left parser, it will run the right parser with the offset provided by the left,
/// and the result is also unwrapped. When both succeed, `AndParser` returns a tuple of `(left_result, right_result)`.
/// 
/// # Example
/// ```rust
/// use nyst::{prelude::*, parser::{LiteralParser, AndParser}};
/// 
/// // Both of these can be any struct that implements `nyst::Parser`.
/// let a_parser = LiteralParser::new('a');
/// let b_parser = LiteralParser::new('b');
/// 
/// let and_parser = AndParser::new(
///   &a_parser,
///   &b_parser,
/// );
/// 
/// // This should succeed.
/// let result = and_parser.parse(&['a', 'b']);
/// assert_eq!(
///   result,
///   ParseResult::Ok((
///     // Both parsers succeeded, returning `(left_result, right_result)`
///     ('a', 'b'),
///     2, // The number of units read from the input.
///   )),
/// );
/// 
/// // This should fail.
/// let result = and_parser.parse(&['c', 'b']);
/// assert_eq!(
///   result,
///   // This failed because the left subparser expected 'a' but got 'c'.
///   // The right subparser was never applied.
///   ParseResult::Err(
///     ParseError::InvalidData
///   ),
/// );
/// ```
#[derive(Copy, Clone)]
pub struct AndParser<'parser, Input: Debug, LeftOutput, RightOutput> {
    pub left_parser: &'parser dyn Parser<'parser, Input = Input, Output = LeftOutput>,
    pub right_parser: &'parser dyn Parser<'parser, Input = Input, Output = RightOutput>,
}
impl<'parser, Input: Debug, LeftOutput, RightOutput>
    AndParser<'parser, Input, LeftOutput, RightOutput>
{
    pub fn new(
        left_parser: &'parser dyn Parser<'parser, Input = Input, Output = LeftOutput>,
        right_parser: &'parser dyn Parser<'parser, Input = Input, Output = RightOutput>,
    ) -> Self {
        AndParser {
            left_parser,
            right_parser,
        }
    }
}
impl<'parser, Input: Debug, LeftOutput, RightOutput> Parser<'parser>
    for AndParser<'parser, Input, LeftOutput, RightOutput>
{
    type Input = Input;
    type Output = (LeftOutput, RightOutput);

    fn parse(&self, data: &'parser [Self::Input]) -> ParseResult<Self::Output> {
        let (left_result, left_offset) = self.left_parser.parse(data)?;
        let (right_result, right_offset) = self.right_parser.parse(&data[left_offset..])?;
        Ok(((left_result, right_result), left_offset + right_offset))
    }
}

/// A simple container for either the left result or the right result.
#[derive(Copy, Clone, PartialEq)]
pub enum OrParserSuccess<LeftOutput, RightOutput> {
    Left(LeftOutput),
    Right(RightOutput),
}
/// Parses the left parser and then the right parser, returning the first successful result.
/// 
/// # Example
/// ```rust
/// use nyst::{prelude::*, parser::{LiteralParser, OrParser}};
/// 
/// // Both of these can be any struct that implements `nyst::Parser`.
/// let a_parser = LiteralParser::new('a');
/// let b_parser = LiteralParser::new('b');
/// 
/// let or_parser = OrParser::new(
///   &a_parser,
///   &b_parser,
/// );
/// 
/// let result = or_parser.parse(&['a']);
/// assert_eq!(
///   result,
///   ParseResult::Ok((
///     // The left parser succeeded, meaning the
///     // right parser did not need to be used.
///     OrParserSuccess::Left('a'),
///     1, // The number of units read from the input.
///   ))
/// );
/// 
/// let result = or_parser.parse(&['b']);
/// assert_eq!(
///   result,
///   ParseResult::Ok((
///     // The left parser failed,
///     // so the right parser was applied,
///     // which succeeded.
///     OrParserSuccess::Right('b'),
///     1, // The number of units read from the input.
///   ))
/// );
/// 
/// let result = or_parser.parse(&['c']);
/// assert_eq!(
///   result,
///   // Both the left and right parser failed, resulting in an error.
///   ParseResult::Err(
///     ParseError::InvalidData
///   ),
/// );
/// ```
#[derive(Copy, Clone)]
pub struct OrParser<'parser, Input: Debug, LeftOutput, RightOutput> {
    pub left_parser: &'parser dyn Parser<'parser, Input = Input, Output = LeftOutput>,
    pub right_parser: &'parser dyn Parser<'parser, Input = Input, Output = RightOutput>,
}
impl<'parser, Input: Debug, LeftOutput, RightOutput>
    OrParser<'parser, Input, LeftOutput, RightOutput>
{
    pub fn new(
        left_parser: &'parser dyn Parser<'parser, Input = Input, Output = LeftOutput>,
        right_parser: &'parser dyn Parser<'parser, Input = Input, Output = RightOutput>,
    ) -> Self {
        OrParser {
            left_parser,
            right_parser,
        }
    }
}
impl<'parser, Input: Debug, LeftOutput, RightOutput> Parser<'parser>
    for OrParser<'parser, Input, LeftOutput, RightOutput>
{
    type Input = Input;
    type Output = OrParserSuccess<LeftOutput, RightOutput>;

    fn parse(
        &self,
        data: &'parser [Self::Input],
    ) -> ParseResult<OrParserSuccess<LeftOutput, RightOutput>> {
        if let Ok((result, offset)) = self.left_parser.parse(data) {
            ParseResult::Ok((OrParserSuccess::Left(result), offset))
        } else {
            match self.right_parser.parse(data) {
                Ok((result, offset)) => Ok((OrParserSuccess::Right(result), offset)),
                Err(error) => Err(error),
            }
        }
    }
}

/// Repeatedly applies a parser over a given range.
/// 
/// Given a range of `start..end`, `RangeParser` applies the given parser as many times as possible up to `start` times,
/// failing if `start` cannot be met. After `start`, it will keep applying the parser until it reaches `end` iterations,
/// after which it returns.
/// 
/// # Examples
/// 
/// Using a range:
/// ```rust
/// use nyst::{prelude::*, parser::{LiteralParser, RangeParser}};
/// 
/// let a_parser = LiteralParser::new('a'); // Can be any struct implementing `nyst::Parser`
/// let range = 3..5; // The start is 3, and the end is 5.
/// let data = vec!['a', 'a', 'a', 'a']; // 4 iterations
/// 
/// let result = RangeParser::range(
///   &a_parser,
///   range,
/// ).parse(data.as_slice());
/// 
/// assert_eq!(
///     result,
///     ParseResult::Ok((
///         vec!['a', 'a', 'a', 'a'], // The parser output.
///         4, // The number of units read from the input.
///     ))
/// );
/// ```
/// 
/// Using a number of repetitions:
/// ```rust
/// use nyst::{prelude::*, parser::{LiteralParser, RangeParser}};
/// 
/// let a_parser = LiteralParser::new('a'); // Can be any struct implementing `nyst::Parser`
/// let repetitions = 4; // Internally this gets represented as a range of `4..4`.
/// let data = vec!['a', 'a', 'a', 'a']; // 4 iterations
/// 
/// let result = RangeParser::repetitions(
///   &a_parser,
///   repetitions,
/// ).parse(data.as_slice());
/// 
/// assert_eq!(
///     result,
///     ParseResult::Ok((
///         vec!['a', 'a', 'a', 'a'], // The parser output.
///         4, // The number of units read from the input.
///     ))
/// );
/// ```
#[derive(Clone)]
pub struct RangeParser<'parser, Input: Debug, Output> {
    pub parser: &'parser dyn Parser<'parser, Input = Input, Output = Output>,
    pub range: std::ops::Range<usize>,
}
impl<'parser, Input: Debug, Output> RangeParser<'parser, Input, Output> {
    pub fn range(
        parser: &'parser dyn Parser<'parser, Input = Input, Output = Output>,
        range: std::ops::Range<usize>,
    ) -> Self {
        RangeParser { parser, range }
    }
    pub fn repetitions(
        parser: &'parser dyn Parser<'parser, Input = Input, Output = Output>,
        repetitions: usize,
    ) -> Self {
        RangeParser {
            parser,
            range: repetitions..repetitions,
        }
    }
}
impl<'parser, Input: Debug, Output> Parser<'parser> for RangeParser<'parser, Input, Output> {
    type Input = Input;
    type Output = Vec<Output>;

    fn parse(&self, data: &'parser [Self::Input]) -> ParseResult<Self::Output> {
        let mut out = vec![];
        let mut offset = 0;
        // Parse the minimum amount required.
        for _ in 0..self.range.start {
            let (item, offset_delta) = self.parser.parse(&data[offset..])?;
            out.push(item);
            offset += offset_delta;
        }
        // Parse the optional ones.
        let mut index = out.len() - 1;
        loop {
            index += 1;
            if index >= self.range.end {
                break;
            }
            if let Ok((item, offset_delta)) = self.parser.parse(&data[offset..]) {
                out.push(item);
                offset += offset_delta;
            }
        }
        Ok((out, offset))
    }
}

/// Always succeeds, turning the `Result` into an `Option`, returning 0 for the units read.
/// 
/// `PeekParser` will try to apply the given parser without advancing the number of units read,
/// allowing a simple way to peek ahead in the data. If the subparser fails, it simply
/// returns `ParseResult::Ok((None, 0))`. If the subparser succeeds, it returns
/// `ParseResult::Ok((Some(<result>), <units read>))`.
/// 
/// # Example
/// ```rust
/// use nyst::{prelude::*, parser::{LiteralParser, PeekParser}};
/// 
/// let a_parser = LiteralParser::new('a'); // Can be any struct implementing `nyst::Parser`
/// 
/// let peek_parser = PeekParser::new(
///   &a_parser,
/// );
/// 
/// let result = peek_parser.parse(&['a']);
/// assert_eq!(
///   result,
///   ParseResult::Ok((
///     Some('a'), // The subparser succeeded, and here is the result.
///     0, // This should always be 0.
///   ))
/// );
/// 
/// // There is no 'a' here to parse, this should fail.
/// let result = peek_parser.parse(&[]);
/// assert_eq!(
///   result,
///   ParseResult::Ok((
///     None, // The subparser failed, so `None` is returned.
///     0, // This should always be 0.
///   ))
/// );
/// ```
#[derive(Copy, Clone)]
pub struct PeekParser<'parser, Input: Debug, Output> {
    pub parser: &'parser dyn Parser<'parser, Input = Input, Output = Output>,
}
impl<'parser, Input: Debug, Output> PeekParser<'parser, Input, Output> {
    pub fn new(parser: &'parser dyn Parser<'parser, Input = Input, Output = Output>) -> Self {
        PeekParser { parser }
    }
}
impl<'parser, Input: Debug, Output> Parser<'parser> for PeekParser<'parser, Input, Output> {
    type Input = Input;
    type Output = Option<Output>;

    fn parse(&self, data: &'parser [Self::Input]) -> ParseResult<Self::Output> {
        match self.parser.parse(data) {
            Ok((result, _offset)) => Ok((Some(result), 0)),
            Err(_error) => Ok((None, 0)),
        }
    }
}

/// Parses the given literal.
/// 
/// If `data[0]` does not equal the given literal, `LiteralParser`
/// returns `ParseResult::Err(ParseError::InvalidData)`. If it does
/// equal the literal, it just returns the value.
/// 
/// # Example
/// ```rust
/// use nyst::{prelude::*, parser::LiteralParser};
/// 
/// let literal_parser = LiteralParser::new('a'); // Can be any struct implementing `nyst::Parser`
/// 
/// // This should succeed, `data[0] /* with the value of 'a' */ == 'a'`
/// let result = literal_parser.parse(&['a']);
/// assert_eq!(
///   result,
///   ParseResult::Ok((
///     'a', // Success! The literal was found and returned.
///     1, // The number of units read from the input.
///   )),
/// );
/// 
/// // This should fail, `data[0] /* with the value of 'b' */ != 'a'`
/// let result = literal_parser.parse(&['b']);
/// assert_eq!(
///   result,
///   ParseResult::Err(
///     ParseError::InvalidData
///   ),
/// );
/// ```
#[derive(Copy, Clone)]
pub struct LiteralParser<Input: PartialEq + Clone + Debug> {
    literal: Input,
}
impl<Input: PartialEq + Clone + Debug> LiteralParser<Input> {
    pub fn new(literal: Input) -> Self {
        LiteralParser { literal }
    }
}
impl<'parser, Input: 'parser + PartialEq + Clone + Debug> Parser<'parser> for LiteralParser<Input> {
    type Input = Input;
    type Output = Input;

    fn parse(&self, data: &'parser [Self::Input]) -> ParseResult<Self::Output> {
        if data[0] == self.literal {
            Ok((self.literal.clone(), 1))
        } else {
            Err(ParseError::InvalidData)
        }
    }
}

/// A parser that always fails.
/// 
/// # Example
/// ```rust
/// use nyst::{prelude::*, parser::{LiteralParser, FailParser}};
/// 
/// let a_parser = LiteralParser::new('a'); // This can be any struct that implements `nyst::Parser`.
/// 
/// let fail_parser = FailParser::new(&a_parser);
/// 
/// let result = fail_parser.parse(&['a']);
/// assert_eq!(
///   result,
///   // This should always be `ParseResult::Err`, no matter the input data.
///   ParseResult::Err(ParseError::Other("FailParser")),
/// );
/// ```
#[derive(Copy, Clone)]
pub struct FailParser<Input> {
    phantom: std::marker::PhantomData<Input>,
}
impl<Input> FailParser<Input> {
    pub fn new() -> FailParser<Input> {
        FailParser {
            phantom: std::marker::PhantomData,
        }
    }
}
impl<Input> Default for FailParser<Input> {
    fn default() -> Self {
        Self::new()
    }
}
impl<Input> Parser<'_> for FailParser<Input> {
    type Input = Input;
    type Output = ();

    fn parse(&self, _data: &[Self::Input]) -> ParseResult<Self::Output> {
        Err(ParseError::Other("FailParser"))
    }
}

/// A parser that succeeds if the subparser fails.
/// 
/// # Example
/// ```rust
/// use nyst::{prelude::*, parser::{LiteralParser, NotParser}};
/// 
/// let a_parser = LiteralParser::new('a'); // This can be any struct that implements `nyst::Parser`.
/// 
/// let not_parser = NotParser::new(&a_parser);
/// 
/// 
/// // This should fail because the subparser succeeds.
/// let result = not_parser.parse(&['a']);
/// assert_eq!(
///   result,
///   ParseResult::Err(ParseError::Other("Subparser succeeded in NotParser")),
/// );
/// 
/// // This should succeed because the subparser fails.
/// let result = not_parser.parse(&['b']);
/// assert_eq!(
///   result,
///   ParseResult::Ok((
///     (), // There is no output to return here, so we just return the unit type `()`.
///     0, // The number of units read from the input.
///   )),
/// );
/// ```
#[derive(Clone)]
pub struct NotParser<'parser, Input: Debug, Output> {
    pub parser: &'parser dyn Parser<'parser, Input = Input, Output = Output>,
}
impl<'parser, Input: Debug, Output> NotParser<'parser, Input, Output> {
    pub fn new(parser: &'parser dyn Parser<'parser, Input = Input, Output = Output>) -> Self {
        NotParser { parser }
    }
}
impl<'parser, Input: Debug, Output> Parser<'parser> for NotParser<'parser, Input, Output> {
    type Input = Input;
    type Output = ();

    fn parse(&self, data: &'parser [Self::Input]) -> ParseResult<Self::Output> {
        match self.parser.parse(data) {
            Ok((_result, _offset)) => Err(ParseError::Other("Subparser succeeded in NotParser")),
            Err(_error) => Ok(((), 0)),
        }
    }
}

/// Parses the given parser, then provides the output as the input to the given closure.
/// 
/// `MapParser` will apply the given parser and then unwrap the result,
/// passing that result into the given closure.
/// 
/// # Example
/// ```rust
/// use nyst::{prelude::*, parser::{LiteralParser, MapParser}};
/// 
/// let a_parser = LiteralParser::new('a'); // This can be any struct that implements `nyst::Parser`.
/// let next_char_closure = |result: (char, usize)| -> ParseResult<char> {
///   Ok((((result.0 as u8) + 1) as char, result.1))
/// };
/// 
/// let map_parser = MapParser::new(&a_parser, &next_char_closure);
/// 
/// let result = map_parser.parse(&['a']);
/// assert_eq!(
///   result,
///   ParseResult::Ok((
///     'b'
///     1,
///   )),
/// );
/// ```
#[derive(Copy, Clone)]
pub struct MapParser<'parser, 'closure, Input, Intermediary, Output> {
    parser: &'parser dyn Parser<'parser, Input = Input, Output = Intermediary>,
    closure: &'closure dyn Fn(Intermediary) -> ParseResult<Output>,
}
impl<'parser, 'closure, Input: Debug, Intermediary, Output>
    MapParser<'parser, 'closure, Input, Intermediary, Output>
{
    pub fn new(
        parser: &'parser dyn Parser<'parser, Input = Input, Output = Intermediary>,
        closure: &'closure dyn Fn(Intermediary) -> ParseResult<Output>,
    ) -> Self {
        MapParser { parser, closure }
    }
}
impl<'parser, 'closure, Input, Intermediary, Output> Parser<'parser>
    for MapParser<'parser, 'closure, Input, Intermediary, Output>
{
    type Input = Input;
    type Output = Output;

    fn parse(&self, data: &'parser [Self::Input]) -> ParseResult<Self::Output> {
        let (result, offset) = self.parser.parse(data)?;
        let result = (self.closure)(result)?;
        Ok((result.0, offset))
    }
}
