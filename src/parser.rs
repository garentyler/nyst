use crate::*;
use std::fmt::Debug;

/// Parses and then ignores the result, still returning the units read.
#[derive(Clone)]
pub struct IgnoreParser<'p, I, O> {
    parser: &'p dyn Parser<Input = I, Output = O>,
}
impl<'p, I, O> IgnoreParser<'p, I, O> {
    pub fn new(parser: &'p dyn Parser<Input = I, Output = O>) -> Self {
        IgnoreParser { parser }
    }
}
impl<'p, I, O> Parser for IgnoreParser<'p, I, O> {
    type Input = I;
    type Output = ();
    fn parse(&self, data: &[Self::Input]) -> ParseResult<Self::Output> {
        let units_read = self.parser.parse(data)?.1;
        Ok(((), units_read))
    }
}

/// Parses the left parser and then the right parser, and then joins the result in a tuple.
///
/// `AndParser` will parse the left parser first. If the left parser fails, that error is passed on.
/// After running the left parser, it will run the right parser with the offset provided by the left,
/// and the result is also unwrapped. When both succeed, `AndParser` returns a tuple of `(left_result, right_result)`.
#[derive(Clone)]
pub struct AndParser<'l, 'r, I, L, R> {
    left_parser: &'l dyn Parser<Input = I, Output = L>,
    right_parser: &'r dyn Parser<Input = I, Output = R>,
}
impl<'l, 'r, I, L, R> AndParser<'l, 'r, I, L, R> {
    pub fn new(
        left_parser: &'l dyn Parser<Input = I, Output = L>,
        right_parser: &'r dyn Parser<Input = I, Output = R>,
    ) -> Self {
        AndParser {
            left_parser,
            right_parser,
        }
    }
}
impl<'l, 'r, I, L, R> Parser for AndParser<'l, 'r, I, L, R> {
    type Input = I;
    type Output = (L, R);
    fn parse(&self, data: &[Self::Input]) -> ParseResult<Self::Output> {
        let (left_output, left_offset): (L, usize) = self.left_parser.parse(data)?;
        let (right_output, right_offset): (R, usize) =
            self.right_parser.parse(&data[left_offset..])?;
        Ok(((left_output, right_output), left_offset + right_offset))
    }
}

/// A simple container for either the left result or the right result.
#[derive(PartialEq, Debug)]
pub enum OrParserSuccess<L, R> {
    Left(L),
    Right(R),
}
/// Parses the left parser and then the right parser, returning the first successful result.
#[derive(Clone)]
pub struct OrParser<'l, 'r, I, L, R> {
    left_parser: &'l dyn Parser<Input = I, Output = L>,
    right_parser: &'r dyn Parser<Input = I, Output = R>,
}
impl<'l, 'r, I, L, R> OrParser<'l, 'r, I, L, R> {
    pub fn new(
        left_parser: &'l dyn Parser<Input = I, Output = L>,
        right_parser: &'r dyn Parser<Input = I, Output = R>,
    ) -> Self {
        OrParser {
            left_parser,
            right_parser,
        }
    }
}
impl<'l, 'r, I, L, R> Parser for OrParser<'l, 'r, I, L, R> {
    type Input = I;
    type Output = OrParserSuccess<L, R>;
    fn parse(&self, data: &[Self::Input]) -> ParseResult<Self::Output> {
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
#[derive(Clone)]
pub struct RangeParser<'p, I, O> {
    parser: &'p dyn Parser<Input = I, Output = O>,
    range: std::ops::Range<usize>,
}
impl<'p, I, O> RangeParser<'p, I, O> {
    pub fn repetitions(parser: &'p dyn Parser<Input = I, Output = O>, repetitions: usize) -> Self {
        RangeParser {
            parser,
            range: repetitions..repetitions,
        }
    }
    pub fn range(
        parser: &'p dyn Parser<Input = I, Output = O>,
        range: std::ops::Range<usize>,
    ) -> Self {
        RangeParser { parser, range }
    }
}
impl<'p, I, O> Parser for RangeParser<'p, I, O> {
    type Input = I;
    type Output = Vec<O>;
    fn parse(&self, data: &[Self::Input]) -> ParseResult<Self::Output> {
        let mut out = vec![];
        let mut offset = 0;
        // Parse the minimum amount required.
        for _ in 0..self.range.start {
            let (item, offset_delta) = self.parser.parse(&data[offset..])?;
            out.push(item);
            offset += offset_delta;
        }
        // Parse the optional ones.
        for _ in 0..(self.range.end - self.range.start) {
            if let Ok((item, offset_delta)) = self.parser.parse(&data[offset..]) {
                out.push(item);
                offset += offset_delta;
            } else {
                break;
            }
        }
        Ok((out, offset))
    }
}

/// Parses the given literal.
///
/// If `data[0]` does not equal the given literal, `LiteralParser`
/// returns `ParseResult::Err(ParseError::InvalidData)`. If it does
/// equal the literal, it just returns the value.
#[derive(Clone)]
pub struct LiteralParser<I: PartialEq + Clone> {
    literal: I,
}
impl<I: PartialEq + Clone> LiteralParser<I> {
    pub fn new(literal: I) -> Self {
        LiteralParser { literal }
    }
}
impl<I: PartialEq + Clone> Parser for LiteralParser<I> {
    type Input = I;
    type Output = I;
    fn parse(&self, data: &[Self::Input]) -> ParseResult<Self::Output> {
        if data.is_empty() {
            Err(ParseError::NotEnoughData)
        } else if data[0] == self.literal {
            Ok((self.literal.clone(), 1))
        } else {
            Err(ParseError::InvalidData)
        }
    }
}

/// A parser that always fails.
#[derive(Copy, Clone)]
pub struct FailParser<I> {
    phantom: std::marker::PhantomData<I>,
}
impl<I> FailParser<I> {
    pub fn new() -> FailParser<I> {
        FailParser {
            phantom: std::marker::PhantomData,
        }
    }
}
impl<I> Default for FailParser<I> {
    fn default() -> Self {
        Self::new()
    }
}
impl<I> Parser for FailParser<I> {
    type Input = I;
    type Output = ();
    fn parse(&self, _data: &[Self::Input]) -> ParseResult<Self::Output> {
        Err(ParseError::Other("FailParser"))
    }
}

/// A parser that uses a custom function.
pub struct CustomParser<'f, I, O> {
    closure: &'f dyn Fn(&[I]) -> ParseResult<O>,
}
impl<'f, I, O> CustomParser<'f, I, O> {
    pub fn new(closure: &'f dyn Fn(&[I]) -> ParseResult<O>) -> Self {
        CustomParser { closure }
    }
}
impl<'f, I, O> Parser for CustomParser<'f, I, O> {
    type Input = I;
    type Output = O;
    fn parse(&self, data: &[Self::Input]) -> ParseResult<Self::Output> {
        (self.closure)(data)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ignore_parser_works() {
        let literal_parser = LiteralParser::new('a');
        let ignore_parser = IgnoreParser::new(&literal_parser);

        let no_data = [];
        let correct_data = ['a', 'b'];
        let incorrect_data = ['b', 'a'];

        assert_eq!(
            ignore_parser.parse(&no_data),
            Err(ParseError::NotEnoughData)
        );
        assert_eq!(ignore_parser.parse(&correct_data), Ok(((), 1)));
        assert_eq!(
            ignore_parser.parse(&incorrect_data),
            Err(ParseError::InvalidData)
        );
    }

    #[test]
    fn and_parser_works() {
        let a_parser = LiteralParser::new('a');
        let b_parser = LiteralParser::new('b');
        let and_parser = AndParser::new(&a_parser, &b_parser);

        let no_data = [];
        let not_enough_data = ['a'];
        let incorrect_data = ['b', 'a'];
        let correct_data = ['a', 'b'];
        let excess_data = ['a', 'b', 'c'];

        assert_eq!(and_parser.parse(&no_data), Err(ParseError::NotEnoughData),);
        assert_eq!(
            and_parser.parse(&not_enough_data),
            Err(ParseError::NotEnoughData),
        );
        assert_eq!(
            and_parser.parse(&incorrect_data),
            Err(ParseError::InvalidData),
        );
        assert_eq!(and_parser.parse(&correct_data), Ok((('a', 'b'), 2)),);
        assert_eq!(and_parser.parse(&excess_data), Ok((('a', 'b'), 2)),);
    }

    #[test]
    fn or_parser_works() {
        let a_parser = LiteralParser::new('a');
        let b_parser = LiteralParser::new('b');
        let or_parser = OrParser::new(&a_parser, &b_parser);

        let no_data = [];
        let a_success = ['a'];
        let b_success = ['b'];
        let excess_data = ['a', 'b', 'c'];

        assert_eq!(or_parser.parse(&no_data), Err(ParseError::NotEnoughData),);
        assert_eq!(
            or_parser.parse(&a_success),
            Ok((OrParserSuccess::Left('a'), 1))
        );
        assert_eq!(
            or_parser.parse(&b_success),
            Ok((OrParserSuccess::Right('b'), 1))
        );
        assert_eq!(
            or_parser.parse(&excess_data),
            Ok((OrParserSuccess::Left('a'), 1))
        );
    }

    #[test]
    fn range_parser_works() {
        let literal_parser = LiteralParser::new('a');
        let range_parser = RangeParser::range(&literal_parser, 2..3);

        let no_data = [];
        let incorrect_data = ['b'];
        let not_enough_data = ['a'];
        let two_data = ['a', 'a'];
        let three_data = ['a', 'a', 'a'];
        let excess_data = ['a', 'a', 'a', 'a'];

        assert_eq!(range_parser.parse(&no_data), Err(ParseError::NotEnoughData));
        assert_eq!(
            range_parser.parse(&incorrect_data),
            Err(ParseError::InvalidData)
        );
        assert_eq!(
            range_parser.parse(&not_enough_data),
            Err(ParseError::NotEnoughData)
        );
        assert_eq!(range_parser.parse(&two_data), Ok((vec!['a', 'a'], 2)));
        assert_eq!(
            range_parser.parse(&three_data),
            Ok((vec!['a', 'a', 'a'], 3))
        );
        assert_eq!(
            range_parser.parse(&excess_data),
            Ok((vec!['a', 'a', 'a'], 3))
        );
    }

    #[test]
    fn literal_parser_works() {
        let literal_parser = LiteralParser::new('a');

        let no_data = [];
        let incorrect_data = ['b', 'a'];
        let correct_data = ['a', 'b'];

        assert_eq!(
            literal_parser.parse(&no_data),
            Err(ParseError::NotEnoughData)
        );
        assert_eq!(
            literal_parser.parse(&incorrect_data),
            Err(ParseError::InvalidData)
        );
        assert_eq!(literal_parser.parse(&correct_data), Ok(('a', 1)));
    }

    #[test]
    fn fail_parser_works() {
        let fail_parser = FailParser::new();

        let no_data = [];
        let data = ['a'];

        assert_eq!(
            fail_parser.parse(&no_data),
            Err(ParseError::Other("FailParser"))
        );
        assert_eq!(
            fail_parser.parse(&data),
            Err(ParseError::Other("FailParser"))
        );
    }

    #[test]
    fn custom_parser_works() {
        fn byte_parser(data: &[u8]) -> ParseResult<i8> {
            if !data.is_empty() {
                let data = [data[0]];
                Ok((i8::from_be_bytes(data), 1))
            } else {
                Err(ParseError::NotEnoughData)
            }
        }
        let custom_parser = CustomParser::new(&byte_parser);

        let no_data = [];
        let data = [16];

        assert_eq!(custom_parser.parse(&no_data), Err(ParseError::NotEnoughData));
        assert_eq!(custom_parser.parse(&data), Ok((16i8, 1)));
    }
}
