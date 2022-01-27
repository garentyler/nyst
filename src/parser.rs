use crate::*;

/// Parses and then ignores the result, only advancing `offset`.
#[derive(Copy, Clone)]
pub struct IgnoreParser<'parser, Input, Output> {
    pub parser: &'parser dyn Parser<Input = Input, Output = Output>,
}
impl<'parser, Input, Output> IgnoreParser<'parser, Input, Output> {
    pub fn new(parser: &'parser dyn Parser<Input = Input, Output = Output>) -> Self {
        IgnoreParser { parser }
    }
}
impl<'parser, Input, Output> Parser for IgnoreParser<'parser, Input, Output> {
    type Input = Input;
    type Output = ();

    fn parse_unprotected(
        &self,
        data: &[Self::Input],
        offset: &mut usize,
    ) -> ParseResult<Self::Output> {
        match self.parser.parse(data, offset) {
            Ok(_result) => Ok(()),
            Err(err) => Err(err),
        }
    }
}

/// Parses the left parser and then the right parser, and then joins the result in a tuple.
#[derive(Copy, Clone)]
pub struct AndParser<'left, 'right, Input, LeftOutput, RightOutput> {
    pub left_parser: &'left dyn Parser<Input = Input, Output = LeftOutput>,
    pub right_parser: &'right dyn Parser<Input = Input, Output = RightOutput>,
}
impl<'left, 'right, Input, LeftOutput, RightOutput>
    AndParser<'left, 'right, Input, LeftOutput, RightOutput>
{
    pub fn new(
        left_parser: &'left dyn Parser<Input = Input, Output = LeftOutput>,
        right_parser: &'right dyn Parser<Input = Input, Output = RightOutput>,
    ) -> Self {
        AndParser {
            left_parser,
            right_parser,
        }
    }
}
impl<'left, 'right, Input, LeftOutput, RightOutput> Parser
    for AndParser<'left, 'right, Input, LeftOutput, RightOutput>
{
    type Input = Input;
    type Output = (LeftOutput, RightOutput);

    fn parse_unprotected(
        &self,
        data: &[Self::Input],
        offset: &mut usize,
    ) -> ParseResult<Self::Output> {
        let left_result = self.left_parser.parse(data, offset)?;
        let right_result = self.right_parser.parse(data, offset)?;
        Ok((left_result, right_result))
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
pub struct OrParser<'left, 'right, Input, LeftOutput, RightOutput> {
    pub left_parser: &'left dyn Parser<Input = Input, Output = LeftOutput>,
    pub right_parser: &'right dyn Parser<Input = Input, Output = RightOutput>,
}
impl<'left, 'right, Input, LeftOutput, RightOutput>
    OrParser<'left, 'right, Input, LeftOutput, RightOutput>
{
    pub fn new(
        left_parser: &'left dyn Parser<Input = Input, Output = LeftOutput>,
        right_parser: &'right dyn Parser<Input = Input, Output = RightOutput>,
    ) -> Self {
        OrParser {
            left_parser,
            right_parser,
        }
    }
}
impl<'left, 'right, Input, LeftOutput, RightOutput> Parser
    for OrParser<'left, 'right, Input, LeftOutput, RightOutput>
{
    type Input = Input;
    type Output = OrParserSuccess<LeftOutput, RightOutput>;

    fn parse_unprotected(
        &self,
        data: &[Self::Input],
        offset: &mut usize,
    ) -> ParseResult<OrParserSuccess<LeftOutput, RightOutput>> {
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
pub struct RangeParser<'parser, Input, Output> {
    pub parser: &'parser dyn Parser<Input = Input, Output = Output>,
    pub range: std::ops::Range<usize>,
}
impl<'parser, Input, Output> RangeParser<'parser, Input, Output> {
    pub fn range(
        parser: &'parser dyn Parser<Input = Input, Output = Output>,
        range: std::ops::Range<usize>,
    ) -> Self {
        RangeParser { parser, range }
    }
    pub fn repetitions(
        parser: &'parser dyn Parser<Input = Input, Output = Output>,
        repetitions: usize,
    ) -> Self {
        RangeParser {
            parser,
            range: repetitions..repetitions,
        }
    }
}
impl<'parser, Input, Output> Parser for RangeParser<'parser, Input, Output> {
    type Input = Input;
    type Output = Vec<Output>;

    fn parse_unprotected(
        &self,
        data: &[Self::Input],
        offset: &mut usize,
    ) -> ParseResult<Self::Output> {
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
pub struct PeekParser<'parser, Input, Output> {
    pub parser: &'parser dyn Parser<Input = Input, Output = Output>,
}
impl<'parser, Input, Output> PeekParser<'parser, Input, Output> {
    pub fn new(parser: &'parser dyn Parser<Input = Input, Output = Output>) -> Self {
        PeekParser { parser }
    }
}
impl<'parser, Input, Output> Parser for PeekParser<'parser, Input, Output> {
    type Input = Input;
    type Output = Option<(Output, usize)>;

    fn parse_unprotected(
        &self,
        data: &[Self::Input],
        offset: &mut usize,
    ) -> ParseResult<Self::Output> {
        let offset_before = *offset;

        match self.parser.parse(data, offset) {
            Ok(result) => {
                let offset_delta = *offset - offset_before;
                *offset = offset_before;
                Ok(Some((result, offset_delta)))
            }
            Err(_) => Ok(None),
        }
    }
}

/// Parses the given literal.
#[derive(Copy, Clone)]
pub struct LiteralParser<Input: PartialEq + Clone> {
    literal: Input,
}
impl<Input: PartialEq + Clone> LiteralParser<Input> {
    pub fn new(literal: Input) -> Self {
        LiteralParser { literal }
    }
}
impl<Input: PartialEq + Clone> Parser for LiteralParser<Input> {
    type Input = Input;
    type Output = Input;

    fn parse_unprotected(
        &self,
        data: &[Self::Input],
        offset: &mut usize,
    ) -> ParseResult<Self::Output> {
        if data.len() <= *offset {
            return Err(ParseError::NotEnoughData);
        }
        let value = data[*offset].clone();
        *offset += 1;
        if value == self.literal {
            Ok(value)
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
impl<Input> Parser for FailParser<Input> {
    type Input = Input;
    type Output = ();

    fn parse_unprotected(
        &self,
        _data: &[Self::Input],
        _offset: &mut usize,
    ) -> ParseResult<Self::Output> {
        Err(ParseError::Other("FailParser"))
    }
}

/// A parser that succeeds if the subparser fails.
#[derive(Copy, Clone)]
pub struct NotParser<'parser, Input, Output> {
    pub parser: &'parser dyn Parser<Input = Input, Output = Output>,
}
impl<'parser, Input, Output> NotParser<'parser, Input, Output> {
    pub fn new(parser: &'parser dyn Parser<Input = Input, Output = Output>) -> Self {
        NotParser { parser }
    }
}
impl<'parser, Input, Output> Parser for NotParser<'parser, Input, Output> {
    type Input = Input;
    type Output = ();

    fn parse_unprotected(
        &self,
        data: &[Self::Input],
        offset: &mut usize,
    ) -> ParseResult<Self::Output> {
        match self.parser.parse(data, offset) {
            Ok(_) => Err(ParseError::Other("Subparser succeeded in NotParser")),
            Err(_) => Ok(()),
        }
    }
}

/// Parses the left parser, then provides the output as the input to the right parser.
#[derive(Copy, Clone)]
pub struct PipeParser<'left, 'right, Input, Intermediary, Output> {
    left_parser: &'left dyn Parser<Input = Input, Output = Intermediary>,
    right_parser: &'right dyn Parser<Input = Intermediary, Output = Output>,
}
impl<'left, 'right, Input, Intermediary, Output>
    PipeParser<'left, 'right, Input, Intermediary, Output>
{
    pub fn new(
        left_parser: &'left dyn Parser<Input = Input, Output = Intermediary>,
        right_parser: &'right dyn Parser<Input = Intermediary, Output = Output>,
    ) -> Self {
        PipeParser {
            left_parser,
            right_parser,
        }
    }
}
impl<'left, 'right, Input, Intermediary, Output> Parser
    for PipeParser<'left, 'right, Input, Intermediary, Output>
{
    type Input = Input;
    type Output = Output;

    fn parse_unprotected(
        &self,
        data: &[Self::Input],
        output: &mut usize,
    ) -> ParseResult<Self::Output> {
        self.right_parser
            .parse(&[self.left_parser.parse(data, output)?], &mut 0)
    }
}

/// Parses the given parser, then provides the output as the input to the given closure.
#[derive(Copy, Clone)]
pub struct MapParser<'parser, 'closure, Input, Intermediary, Output> {
    parser: &'parser dyn Parser<Input = Input, Output = Intermediary>,
    closure: &'closure dyn Fn(Intermediary) -> ParseResult<Output>,
}
impl<'parser, 'closure, Input, Intermediary, Output> Parser
    for MapParser<'parser, 'closure, Input, Intermediary, Output>
{
    type Input = Input;
    type Output = Output;

    fn parse_unprotected(
        &self,
        data: &[Self::Input],
        output: &mut usize,
    ) -> ParseResult<Self::Output> {
        (self.closure)(self.parser.parse(data, output)?)
    }
}
