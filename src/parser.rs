use crate::*;
use std::fmt::Debug;

/// Parses and then ignores the result, only advancing `offset`.
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
            Ok((_result, _offset)) => Ok(((), 0)),
            Err(error) => Err(error),
        }
    }
}

/// Parses the left parser and then the right parser, and then joins the result in a tuple.
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

/// A container for either the left result or the right result.
#[derive(Copy, Clone, PartialEq)]
pub enum OrParserSuccess<LeftOutput, RightOutput> {
    Left(LeftOutput),
    Right(RightOutput),
}
/// Parses the left parser and then the right parser, returning the first successful result.
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

/// Parses without advancing `offset`, returning `None` on error.
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
