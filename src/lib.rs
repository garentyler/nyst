use regex::Regex;
use ron::to_string;
use std::ops::Range;
use std::rc::Rc;

pub enum ParserKind {
    /// Parse a regular expression using the regex library.
    Regex(Regex),
    /// Parse the left subparser AND the right subparser.
    ///
    /// If both succeed, return both results.
    /// If either fail, fail parsing.
    And,
    /// Parse the left subparser XOR the right subparser.
    ///
    /// If the left parser succeeds, return the result.
    /// If the left parser fails, parse the right parser.
    /// If the right parser succeeds, return the result.
    /// If both the left and right parsers fail, fail parsing.
    Or,
    /// Parse the subparser and ignore the results by returning an empty string.
    ///
    /// If the subparser fails, fail parsing.
    Ignore,
    /// Parse the subparser repeatedly.
    ///
    /// If an iteration fails before the minimum requirement is met, fail parsing.
    /// If an iteration fails between the minimum and maximum requirements, return all results.
    /// If the maximum number of iterations is reached, stop parsing and return all results.
    RepeatRange(Range<usize>),
    /// Parse the subparser, and perform a computation on it before returning the result.
    Map(Rc<Box<dyn Fn(String) -> Result<String, ron::Error>>>),
    /// Custom user-defined parsing function.
    Custom(Rc<Box<dyn Fn(String) -> Result<(String, String), String>>>),
}
impl std::fmt::Debug for ParserKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // Use the implementation of std::fmt::Display.
        write!(f, "{}", self)
    }
}
impl std::fmt::Display for ParserKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use ParserKind::*;
        match self {
            Regex(r) => write!(f, "Regex /{}/", r.as_str()),
            And => write!(f, "And"),
            Ignore => write!(f, "Ignore"),
            Or => write!(f, "Or"),
            RepeatRange(range) => write!(f, "RepeatRange {:?}", range),
            Map(_) => write!(f, "Map"),
            Custom(_) => write!(f, "Custom"),
        }
    }
}
impl Clone for ParserKind {
    fn clone(&self) -> Self {
        use ParserKind::*;
        match self {
            Regex(r) => Regex(r.clone()),
            And => And,
            Ignore => Ignore,
            Or => Or,
            RepeatRange(range) => RepeatRange(range.clone()),
            Map(cfn) => Map(Rc::clone(cfn)),
            Custom(cfn) => Custom(Rc::clone(cfn)),
        }
    }
}
impl PartialEq for ParserKind {
    fn eq(&self, other: &ParserKind) -> bool {
        format!("{}", self) == format!("{}", other)
    }
}

#[derive(Debug, Clone)]
pub struct Parser {
    kind: ParserKind,
    subparsers: Vec<Parser>,
}
impl std::fmt::Display for Parser {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.pretty_print(f, 0)
    }
}
impl Parser {
    /// Parse a string and return a Result.
    ///
    /// # Arguments
    ///
    /// * `src` - The string to be parsed
    ///
    /// # Returns
    ///
    /// `parse()` returns a `Result<(String, String), String>`
    /// If the `Result` is `Ok`, the first field of the enum is the matched text and the second field is the remaining text.
    /// If the `Result` is `Err`, the String is the original input.
    pub fn parse<T: Into<String>>(&self, src: T) -> Result<(String, String), String> {
        use ParserKind::*;
        let s: String = src.into();
        match &self.kind {
            /// Parse a regular expression using the regex library.
            Regex(re) => {
                if let Some(mat) = re.find(&s) {
                    if mat.start() == 0 {
                        Ok((
                            s[mat.start()..mat.end()].to_owned(),
                            s[mat.end()..].to_owned(),
                        ))
                    } else {
                        Err(s)
                    }
                } else {
                    Err(s)
                }
            }
            /// Parse the left subparser AND the right subparser.
            ///
            /// If both succeed, return both results.
            /// If either fail, fail parsing.
            And => {
                if self.subparsers[0].kind == Ignore && self.subparsers[1].kind == Ignore {
                    Ok(("".into(), s))
                } else if self.subparsers[0].kind == Ignore {
                    let (_, rest) = self.subparsers[0].parse(s)?;
                    self.subparsers[1].parse(rest)
                } else if self.subparsers[1].kind == Ignore {
                    let (matched, lrest) = self.subparsers[0].parse(s.clone())?;
                    if let Ok((_, rest)) = self.subparsers[1].parse(lrest) {
                        Ok((matched, rest))
                    } else {
                        Err(s)
                    }
                } else {
                    let (lmatched, lrest) = self.subparsers[0].parse(s)?;
                    let (rmatched, rrest) = self.subparsers[1].parse(lrest)?;
                    Ok((
                        to_string(&vec![lmatched.clone(), rmatched.clone()]).unwrap(),
                        rrest,
                    ))
                }
            }
            /// Parse the left subparser XOR the right subparser.
            ///
            /// If the left parser succeeds, return the result.
            /// If the left parser fails, parse the right parser.
            /// If the right parser succeeds, return the result.
            /// If both the left and right parsers fail, fail parsing.
            Or => {
                if let Ok(lresult) = self.subparsers[0].parse(s.clone()) {
                    Ok(lresult)
                } else {
                    self.subparsers[1].parse(s.clone())
                }
            }
            /// Parse the subparser and ignore the results by returning an empty string.
            ///
            /// If the subparser fails, fail parsing.
            Ignore => Ok(("".into(), self.subparsers[0].parse(s)?.1)),
            /// Parse the subparser repeatedly.
            ///
            /// If an iteration fails before the minimum requirement is met, fail parsing.
            /// If an iteration fails between the minimum and maximum requirements, return all results.
            /// If the maximum number of iterations is reached, stop parsing and return all results.
            RepeatRange(range) => {
                let mut matched = vec![];
                let mut rest = s.clone();

                // Parse up to range.start
                for _ in 0..range.start {
                    let (m, r) = self.subparsers[0].parse(rest)?;
                    matched.push(m);
                    rest = r;
                }

                // Parse optionally up to range.end
                for _ in 0..(range.end - range.start) {
                    let parse_result = self.subparsers[0].parse(rest);
                    if let Err(r) = parse_result {
                        rest = r;
                        break;
                    } else {
                        let (m, r) = parse_result.unwrap();
                        matched.push(m);
                        rest = r;
                    }
                }

                Ok((to_string(&matched).unwrap(), rest))
            }
            /// Parse the subparser, and perform a computation on it before returning the result.
            Map(cfn) => {
                let (matched, rest) = self.subparsers[0].parse(s)?;
                if let Ok(m) = cfn(matched) {
                    Ok((m, rest))
                } else {
                    Err(rest)
                }
            }
            /// Custom user-defined parsing function.
            Custom(cfn) => cfn(s),
        }
    }

    /// Create a new `ParserKind::Regex` parser.
    ///
    /// # Arguments
    ///
    /// * `s` - The regular expression in string form.
    ///
    /// # Examples
    ///
    /// ```
    /// let src = "abcABC123_".to_owned();
    ///
    /// let regex_parser = Parser::regex(r"[a-zA-Z]");
    /// assert_eq!(
    ///   regex_parser.parse(src),
    ///   Ok(("abcABC", "123_"))
    /// );
    /// ```
    pub fn regex<T: Into<String>>(s: T) -> Parser {
        Parser {
            kind: ParserKind::Regex(Regex::new(&s.into()).expect("could not compile regex")),
            subparsers: vec![],
        }
    }

    /// Create a new `ParserKind::And` parser.
    ///
    /// # Arguments
    ///
    /// * `r` - The right side parser.
    ///
    /// # Examples
    ///
    /// ```
    /// let src = "abc123".to_owned();
    /// let letters_parser = Parser::regex(r"[a-zA-Z]+");
    /// let numbers_parser = Parser::regex(r"\d+");
    ///
    /// let letters_then_numbers = letters_parser.and(numbers_parser);
    /// assert_eq!(
    ///   letters_then_numbers.parse(src),
    ///   Ok(("["abc","123"]"), "")
    /// );
    /// ```
    pub fn and(self, r: Parser) -> Parser {
        Parser {
            kind: ParserKind::And,
            subparsers: vec![self, r],
        }
    }

    /// Create a new `ParserKind::Or` parser.
    ///
    /// # Arguments
    ///
    /// * `r` - The right side parser.
    ///
    /// # Examples
    ///
    /// ```
    /// let letters = "abc".to_owned();
    /// let numbers = "123".to_owned();
    /// let letters_parser = Parser::regex(r"[a-zA-Z]+");
    /// let numbers_parser = Parser::regex(r"\d+");
    ///
    /// let letters_or_numbers = letters_parser.or(numbers_parser);
    /// assert_eq!(
    ///   letters_or_numbers.parse(letters),
    ///   Ok(("abc"), "")
    /// );
    /// assert_eq!(
    ///   letters_or_numbers.parse(numbers),
    ///   Ok(("123"), "")
    /// );
    /// ```
    pub fn or(self, r: Parser) -> Parser {
        Parser {
            kind: ParserKind::Or,
            subparsers: vec![self, r],
        }
    }

    /// Create a new `ParserKind::Ignore` parser.
    ///
    /// # Examples
    ///
    /// ```
    /// let src = "abc123".to_owned();
    /// let letters = Parser::regex(r"[a-zA-Z]+");
    ///
    /// let parse_letters_and_ignore = letters.ignore();
    /// assert_eq!(
    ///   parse_letters_and_ignore.parse(src),
    ///   Ok(("", "123"))
    /// );
    /// ```
    pub fn ignore(self) -> Parser {
        Parser {
            kind: ParserKind::Ignore,
            subparsers: vec![self],
        }
    }

    /// Create a new `ParserKind::RepeatRange` parser.
    ///
    /// # Arguments
    ///
    /// * `num_repeats` - The range of possible iterations.
    ///
    /// # Examples
    ///
    /// ```
    /// let too_few = "aa".to_owned(); // 2 of 'a'
    /// let just_right = "aaaaa".to_owned(); // 5 of 'a'
    /// let too_many = "aaaaaaaa".to_owned(); // 8 of 'a'
    /// let a_parser = Parser::regex(r"a");
    ///
    /// let repeating_a = a_parser.repeat_range(3..6);
    /// assert_eq!(
    ///   repeating_a.parse(too_few),
    ///   Err("aa")
    /// );
    /// assert_eq!(
    ///   repeating_a.parse(just_right),
    ///   Ok(("["a", "a", "a", "a", "a"]", ""))
    /// );
    /// assert_eq!(
    ///   repeating_a.parse(too_many),
    ///   Ok(("["a", "a", "a", "a", "a", "a"]", "aa"))
    /// );
    /// ```
    pub fn repeat_range(self, num_repeats: Range<usize>) -> Parser {
        Parser {
            kind: ParserKind::RepeatRange(num_repeats),
            subparsers: vec![self],
        }
    }

    /// Create a new `ParserKind::RepeatRange` parser with a range of `0..1`.
    ///
    /// # Examples
    ///
    /// ```
    /// let no_c = "ab".to_owned();
    /// let with_c = "abc".to_owned();
    /// let ab_parser = Parser::regex(r"a");
    ///
    /// let optional_c_parser = Parser::regex(r"c").optional();
    ///
    /// let combined_parser = ab_parser.and(optional_c_parser);
    /// assert_eq!(
    ///   combined_parser.parse(no_c),
    ///   Ok(("ab", ""))
    /// );
    /// assert_eq!(
    ///   combined_parser.parse(with_c),
    ///   Ok(("abc", ""))
    /// );
    /// ```
    pub fn optional(self) -> Parser {
        Parser {
            kind: ParserKind::RepeatRange(0..1),
            subparsers: vec![self],
        }
    }

    /// Create a new `ParserKind::Map` parser.
    ///
    /// # Arguments
    ///
    /// * `cfn` - The custom function to apply to the results.
    ///
    /// # Examples
    ///
    /// ```
    /// use ron::{to_string, from_str};
    ///
    /// let src = "abc".to_owned();
    /// let multiple_letter_parser = Parser::regex(r"[a-zA-Z]").repeat_range(0..usize::MAX);
    ///
    /// assert_eq!(multiple_letter_parser.parse(src), Ok(("["a", "b", "c"]", "")));
    ///
    /// let mapped = multiple_letter_parser.map(|result| {
    ///   let letters = from_str::<Vec<String>>(&result)?;
    ///   let numbered_letters = letters.iter().enumerate().collect<Vec<(usize, String)>>();
    ///   Ok(to_string(&numbered_letters))
    /// });
    /// assert_eq!(
    ///   mapped.parse(src),
    ///   Ok(("[(0, "a"), (1, "b"), (2, "c")]", ""))
    /// );
    /// ```
    pub fn map<F: 'static>(self, cfn: F) -> Parser
    where
        F: Fn(String) -> Result<String, ron::Error>,
    {
        Parser {
            kind: ParserKind::Map(Rc::new(Box::new(cfn))),
            subparsers: vec![self],
        }
    }

    /// Create a new `ParserKind::Custom` parser.
    ///
    /// # Arguments
    ///
    /// * `cfn` - The custom function to parse with.
    pub fn custom<F: 'static>(cfn: F) -> Parser
    where
        F: Fn(String) -> Result<(String, String), String>,
    {
        Parser {
            kind: ParserKind::Custom(Rc::new(Box::new(cfn))),
            subparsers: vec![],
        }
    }

    #[doc(hidden)]
    pub fn pretty_print(&self, f: &mut std::fmt::Formatter<'_>, indent: usize) -> std::fmt::Result {
        for _ in 0..indent {
            write!(f, " ")?;
        }
        write!(f, "{}", self.kind)?;
        if self.subparsers.len() > 0 {
            write!(f, " [\n")?;
            for subparser in &self.subparsers {
                subparser.pretty_print(f, indent + 2)?;
                write!(f, ",\n")?;
            }
            for _ in 0..indent {
                write!(f, " ")?;
            }
            write!(f, "]")
        } else {
            write!(f, "")
        }
    }
}
