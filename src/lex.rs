use super::{
    BinOp, ColNr, Error, ErrorDetail, LineNr, Offset, Result, Source, SourceLoc, Token, TokenLoc,
};
use std::iter::Peekable;
use std::str::CharIndices;

/// A lexer for tokenizing source code into a stream of tokens.
/// The primary way to use it is as an iterator.
#[derive(Clone)]
pub(super) struct Lexer<'src> {
    /// The source code to be tokenized.
    pub(super) source: &'src Source,
    /// Iterator over the source's chars.
    chars: Peekable<CharIndices<'src>>,
    /// Current location in the source.
    loc: (Offset, LineNr, ColNr),
    /// Current character being processed.
    curr_char: Option<(usize, char)>,
}

impl<'src> Lexer<'src> {
    /// Creates a new lexer for the given source code.
    pub(super) fn new(source: &'src Source) -> Self {
        let mut lexer = Self {
            source,
            chars: source.contents.char_indices().peekable(),
            curr_char: None,
            loc: (0, 0, 0),
        };
        lexer.next_char();
        lexer
    }

    /// Returns the next token from the source code.
    fn next_token(&mut self) -> Option<Result<'src, TokenLoc<'src>>> {
        self.matching_substr(|chr| chr.is_whitespace());
        self.curr_char.map(|(start, chr)| {
            let loc = self.curr_loc();
            if chr.is_alphabetic() || chr == '_' {
                let ident = self.matching_substr(|chr| chr.is_alphanumeric() || chr == '_');
                Ok(TokenLoc {
                    token: match ident {
                        "fn" => Token::Fn,
                        "if" => Token::If,
                        "else" => Token::Else,
                        "while" => Token::While,
                        "for" => Token::For,
                        "in" => Token::In,
                        "return" => Token::Return,
                        "true" => Token::True,
                        "false" => Token::False,
                        _ => Token::Ident(ident),
                    },
                    loc,
                })
            } else if chr.is_ascii_digit() {
                let whole = self.matching_substr(|chr| chr.is_ascii_digit());
                let token = match (self.curr_char, self.chars.peek()) {
                    (Some((_, '.')), Some((_, '.'))) => {
                        Token::Int(whole.parse().expect("Should be a string of digits"))
                    }
                    (Some((_, '.')), _) => {
                        self.next_char();
                        self.matching_substr(|chr| chr.is_ascii_digit());
                        let end = match self.curr_char {
                            Some((end, _)) => end,
                            None => self.source.contents.len(),
                        };
                        let full_num = &self.source.contents[start..end];
                        Token::Float(full_num.parse().unwrap())
                    }
                    (_, _) => Token::Int(whole.parse().expect("Should be a string of digits")),
                };
                Ok(TokenLoc { token, loc })
            } else if chr == '"' {
                self.next_char();
                let mut escaped = false;
                let token = Token::String(self.matching_substr(|chr| {
                    let ok = escaped || chr != '"';
                    escaped = !escaped && chr == '\\';
                    ok
                }));
                self.next_char();
                Ok(TokenLoc { token, loc })
            } else {
                let (offset, line, col) = self.loc;
                let mut matches = vec![];
                while let Some((idx, _)) = self
                    .curr_char
                    .filter(|(_, chr)| chr.is_ascii_punctuation() && *chr != '"')
                {
                    self.next_char();
                    matches.push((self.clone(), &self.source.contents[start..=idx]));
                }
                for (lexer, token) in matches.into_iter().rev() {
                    if let Some(token) = match token {
                        "//" => {
                            *self = lexer;
                            let token = Token::Comment(self.matching_substr(|chr| chr != '\n'));
                            return Ok(TokenLoc { token, loc });
                        }
                        "!" => Some(Token::Not),
                        "+" => Some(Token::BinOp(BinOp::Add)),
                        "-" => Some(Token::BinOp(BinOp::Sub)),
                        "*" => Some(Token::BinOp(BinOp::Mul)),
                        "/" => Some(Token::BinOp(BinOp::Div)),
                        "%" => Some(Token::BinOp(BinOp::Mod)),
                        "<" => Some(Token::BinOp(BinOp::Lt)),
                        "<=" => Some(Token::BinOp(BinOp::Leq)),
                        ">" => Some(Token::BinOp(BinOp::Gt)),
                        ">=" => Some(Token::BinOp(BinOp::Geq)),
                        "==" => Some(Token::BinOp(BinOp::Eq)),
                        "!=" => Some(Token::BinOp(BinOp::Neq)),
                        "&&" => Some(Token::BinOp(BinOp::And)),
                        "||" => Some(Token::BinOp(BinOp::Or)),
                        "," => Some(Token::Comma),
                        "." => Some(Token::Dot),
                        ".." => Some(Token::Range),
                        "(" => Some(Token::LParen),
                        ")" => Some(Token::RParen),
                        "[" => Some(Token::LBrack),
                        "]" => Some(Token::RBrack),
                        "{" => Some(Token::LBrace),
                        "}" => Some(Token::RBrace),
                        "=" => Some(Token::Assign),
                        _ => None,
                    } {
                        *self = lexer;
                        return Ok(TokenLoc {
                            token,
                            loc: SourceLoc {
                                source: self.source,
                                offset,
                                line,
                                col,
                            },
                        });
                    }
                }
                Err(Error {
                    loc,
                    detail: ErrorDetail::InvalidToken,
                })
            }
        })
    }

    /// Returns a substring of the source code that matches the given predicate.
    fn matching_substr(&mut self, mut pred: impl FnMut(char) -> bool) -> &'src str {
        if let Some((start, chr)) = self.curr_char {
            if !pred(chr) {
                return "";
            }
            let mut end = start;
            while let Some((idx, _)) = self.next_char().filter(|(_, chr)| pred(*chr)) {
                end = idx;
            }
            &self.source.contents[start..=end]
        } else {
            ""
        }
    }

    /// Advances to the next character in the source code.
    fn next_char(&mut self) -> Option<(usize, char)> {
        let last_char = self.curr_char;
        self.curr_char = self.chars.next();
        if let Some((idx, _)) = self.curr_char {
            let (_, line, col) = self.loc;
            let (line, col) = if let Some((_, '\n')) = last_char {
                (line + 1, 0)
            } else if last_char.is_some() {
                (line, col + 1)
            } else {
                (line, col)
            };
            self.loc = (idx, line, col);
        }
        self.curr_char
    }

    /// Returns the current location in the source code.
    fn curr_loc(&mut self) -> SourceLoc<'src> {
        let (offset, line, col) = self.loc;
        SourceLoc {
            source: self.source,
            offset,
            line,
            col,
        }
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = Result<'src, TokenLoc<'src>>;

    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}

#[cfg(test)]
mod tests {
    use super::{BinOp::*, Token::*, *};

    macro_rules! lexer_test {
        ($name:ident, $source:literal, Err($detail:tt), $line:literal : $col:literal : $offset:literal) => {
            #[test]
            fn $name() {
                let source = &Source {
                    name: format!("<{}>", stringify!($name)),
                    contents: $source.to_owned(),
                };
                let lexer = Lexer::new(source);
                let mut got_err = false;
                for tok in lexer {
                    if let Err(err) = tok {
                        assert_eq!(
                            err,
                            Error {
                                loc: SourceLoc {
                                    source,
                                    offset: $offset,
                                    line: $line,
                                    col: $col,
                                },
                                detail: ErrorDetail::$detail,
                            }
                        );
                        got_err = true;
                    }
                }
                assert!(got_err, "Expected error, but lexing succeeded");
            }
        };
        ($name:ident, $source:literal, $tokens:tt) => {
            #[test]
            fn $name() {
                let source = &Source {
                    name: format!("<{}>", stringify!($name)),
                    contents: $source.to_owned(),
                };
                let mut lexer = Lexer::new(source);
                let mut exp_iter = $tokens
                    .iter()
                    .map(|&(token, offset, line, col)| -> Result<_> {
                        Ok(TokenLoc {
                            token,
                            loc: SourceLoc {
                                source,
                                offset,
                                line,
                                col,
                            },
                        })
                    });
                let mut act_opt = lexer.next();
                let mut exp_opt = exp_iter.next();
                while let (Some(actual), Some(expected)) = (&act_opt, &exp_opt) {
                    match (actual, expected) {
                        (Ok(actual), Ok(expected)) => assert_eq!(actual, expected),
                        (Ok(actual), Err(expected)) => {
                            panic!("Expected token {expected:?}, got error {actual:?}")
                        }
                        (Err(actual), Ok(expected)) => {
                            panic!("Expected error {expected:?}, got token {actual:?}")
                        }
                        (Err(actual), Err(expected)) => assert_eq!(actual, expected),
                    }
                    exp_opt = exp_iter.next();
                    act_opt = lexer.next();
                }
                assert_eq!(act_opt, None);
                assert_eq!(exp_opt, None);
            }
        };
    }

    lexer_test!(empty, "", []);
    lexer_test!(whitespace, " \t\n", []);
    lexer_test!(
        keywords,
        "fn if else while for in return",
        [
            (Fn, 0, 0, 0),
            (If, 3, 0, 3),
            (Else, 6, 0, 6),
            (While, 11, 0, 11),
            (For, 17, 0, 17),
            (In, 21, 0, 21),
            (Return, 24, 0, 24),
        ]
    );
    lexer_test!(
        ident,
        "abc _ __xxx",
        [
            (Ident("abc"), 0, 0, 0),
            (Ident("_"), 4, 0, 4),
            (Ident("__xxx"), 6, 0, 6)
        ]
    );
    lexer_test!(int, "123", [(Int(123), 0, 0, 0)]);
    lexer_test!(
        float,
        "3.14 3.",
        [(Float(3.14), 0, 0, 0), (Float(3.), 5, 0, 5)]
    );
    lexer_test!(string, "\"abc\"", [(String("abc"), 0, 0, 0)]);
    lexer_test!(
        punctuation,
        "(){},.",
        [
            (LParen, 0, 0, 0),
            (RParen, 1, 0, 1),
            (LBrace, 2, 0, 2),
            (RBrace, 3, 0, 3),
            (Comma, 4, 0, 4),
            (Dot, 5, 0, 5),
        ]
    );
    lexer_test!(
        binop,
        "+-*/%",
        [
            (BinOp(Add), 0, 0, 0),
            (BinOp(Sub), 1, 0, 1),
            (BinOp(Mul), 2, 0, 2),
            (BinOp(Div), 3, 0, 3),
            (BinOp(Mod), 4, 0, 4),
        ]
    );
    lexer_test!(
        function,
        "fn fib(n) { return n }",
        [
            (Fn, 0, 0, 0),
            (Ident("fib"), 3, 0, 3),
            (LParen, 6, 0, 6),
            (Ident("n"), 7, 0, 7),
            (RParen, 8, 0, 8),
            (LBrace, 10, 0, 10),
            (Return, 12, 0, 12),
            (Ident("n"), 19, 0, 19),
            (RBrace, 21, 0, 21),
        ]
    );
    lexer_test!(
        comment,
        "foo // bar",
        [(Ident("foo"), 0, 0, 0), (Comment(" bar"), 4, 0, 4)]
    );
    lexer_test!(error_1, "foo @ bar", Err(InvalidToken), 0:4:4);
    lexer_test!(error_2, "1\n;\n5", Err(InvalidToken), 1:0:2);
}
