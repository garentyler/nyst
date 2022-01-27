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
