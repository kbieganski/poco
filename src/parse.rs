use crate::text::Text;

use super::{
    lex::Lexer, text::Bc, text::BcIdx, text::Func, BinOp, Error, ErrorDetail, FuncRef, Result,
    SourceLoc, Token, TokenLoc, UnOp, Val,
};
use std::{collections::HashSet, iter::Peekable, mem::take, rc::Rc};

/// Internal state of the parser during parsing.
#[derive(Debug, Clone)]
enum ParseState<'src> {
    Module,
    Scope,
    Func {
        func: FuncRef,
        name: Option<&'src str>,
        loc: SourceLoc,
    },
    Body,
    If {
        loc: SourceLoc,
    },
    Then {
        idx: BcIdx,
    },
    Else {
        idx: BcIdx,
    },
    While {
        idx: BcIdx,
        loc: SourceLoc,
    },
    ForInit {
        counter_name: &'src str,
        loc: SourceLoc,
    },
    ForCond {
        idx: BcIdx,
        counter_name: &'src str,
        loc: SourceLoc,
    },
    ForInc {
        counter_name: &'src str,
        loc: SourceLoc,
    },
    Loop {
        branch_idx: BcIdx,
        start_idx: BcIdx,
        loc: SourceLoc,
        pop_scope: bool,
    },
    Assign {
        lhs: &'src str,
        loc: SourceLoc,
    },
    IndexAssign,
    IndexSet {
        loc: SourceLoc,
    },
    Expr,
    Logic,
    Relation,
    Factor,
    Term,
    Primary,
    IndexGet {
        loc: SourceLoc,
    },
    Call {
        name: &'src str,
        args: u32,
        loc: SourceLoc,
    },
    Table {
        count: i64,
        field: Option<SourceLoc>,
    },
    Paren,
    UnOp {
        op: UnOp,
        loc: SourceLoc,
    },
    BinOp {
        op: BinOp,
        loc: SourceLoc,
    },
    Return {
        loc: SourceLoc,
    },
}

/// Converts a stream of tokens into `Text` (a set of executable `Func`s).
pub(super) struct Parser<'src> {
    /// Lexer to read tokens from.
    lexer: Peekable<Lexer<'src>>,
    /// Location of the last token, used for unexpected EOF.
    last_loc: SourceLoc,
    /// Currently processed token.
    curr_tok: Option<TokenLoc<'src>>,
    /// Parser state stack.
    state_stack: Vec<ParseState<'src>>,
    /// Function stack, used to keep track of the current function being parsed.
    func_stack: Vec<FuncRef>,
    /// Pool of string constants.
    str_pool: HashSet<Rc<str>>,
    /// Bytecode produced by the parser.
    text: Text,
}

macro_rules! expect_token {
    ($self:ident, $tok:pat) => {
        match $self.expect_curr_tok()? {
            TokenLoc { token: $tok, loc } => {
                $self.advance()?;
                loc
            }
            TokenLoc { loc, .. } => {
                return Err(Error {
                    loc,
                    detail: ErrorDetail::SyntaxError,
                })
            }
        }
    };
}

impl<'src> Parser<'src> {
    /// Creates a new parser for the given lexer.
    pub(super) fn new(lexer: Lexer<'src>) -> Self {
        let last_loc = SourceLoc {
            offset: 0,
            line: 0,
            col: 0,
        };
        let mut text = Text::default();
        text.add_func(
            format!("<{}>", &lexer.source.name).into(),
            Default::default(),
        );
        Self {
            lexer: lexer.peekable(),
            last_loc,
            curr_tok: None,
            state_stack: Default::default(),
            func_stack: vec![FuncRef(0)],
            text,
            str_pool: Default::default(),
        }
    }

    /// Parses the tokens into `Text` (a set of `Func`s).
    pub(super) fn parse(&mut self) -> Result<Text> {
        self.advance()?;
        self.push_state(ParseState::Module);
        while let Some(state) = self.state_stack.pop() {
            match state {
                ParseState::Module => {
                    if let Some(curr_tok) = self.curr_tok {
                        self.push_state(ParseState::Module);
                        if let Token::Fn = curr_tok.token {
                            self.discard_after_call();
                            let loc = expect_token!(self, Token::Fn);
                            let name = match self.expect_curr_tok()? {
                                TokenLoc {
                                    token: Token::Ident(name),
                                    ..
                                } => {
                                    self.advance()?;
                                    name
                                }
                                TokenLoc { loc, .. } => {
                                    return Err(Error {
                                        loc,
                                        detail: ErrorDetail::SyntaxError,
                                    });
                                }
                            };
                            let pooled_name = self.pooled_str(name);
                            let args = self.parse_func_args()?;
                            let func = self.text.add_func(pooled_name, args);
                            self.push_state(ParseState::Func {
                                func,
                                name: Some(name),
                                loc,
                            });
                            self.push_func(func);
                            expect_token!(self, Token::LBrace);
                            let curr_tok = self.expect_curr_tok()?;
                            if let Token::RBrace = curr_tok.token {
                                self.advance()?;
                            } else {
                                self.push_state(ParseState::Body);
                                self.parse_stmt()?;
                            }
                        } else {
                            self.parse_stmt()?;
                        }
                    }
                }
                ParseState::Body => {
                    let curr_tok = self.expect_curr_tok()?;
                    if let Token::RBrace = curr_tok.token {
                        self.advance()?;
                    } else {
                        self.push_state(ParseState::Body);
                        self.parse_stmt()?;
                    }
                }
                ParseState::Scope => {
                    self.parse_end_scope()?;
                }
                ParseState::Func { func, name, loc } => {
                    self.func_stack.pop();
                    self.curr_func_mut()
                        .append_instr(Bc::Imm(Val::Func(func)), loc);
                    if let Some(name) = name {
                        let name = self.pooled_str(name);
                        self.curr_func_mut().append_instr(Bc::Store(name), loc);
                    }
                }
                ParseState::If { loc } => {
                    let idx = self.curr_func().next_instr_idx();
                    self.curr_func_mut().append_instr(Bc::Branch(0), loc);
                    self.push_state(ParseState::Then { idx });
                    self.parse_scope()?;
                }
                ParseState::Then { idx } => {
                    let mut target_idx = self.curr_func().next_instr_idx();
                    if let Some(TokenLoc {
                        token: Token::Else,
                        loc,
                    }) = self.curr_tok
                    {
                        let idx = self.curr_func().next_instr_idx();
                        self.curr_func_mut().append_instr(Bc::Jump(0), loc);
                        self.advance()?;
                        self.push_state(ParseState::Else { idx });
                        target_idx = self.curr_func().next_instr_idx();

                        if let Some(TokenLoc {
                            token: Token::If,
                            loc,
                        }) = self.curr_tok
                        {
                            self.advance()?;
                            self.push_state(ParseState::If { loc });
                            self.push_state(ParseState::Expr);
                        } else {
                            self.parse_scope()?;
                        }
                    }
                    *self.curr_func_mut().instr_at_mut(idx) = Bc::Branch(target_idx - idx - 1);
                }
                ParseState::Else { idx } => {
                    *self.curr_func_mut().instr_at_mut(idx) =
                        Bc::Jump(self.curr_func().next_instr_idx() - idx - 1);
                }
                ParseState::While {
                    idx: start_idx,
                    loc,
                } => {
                    let idx = self.curr_func().next_instr_idx();
                    self.curr_func_mut().append_instr(Bc::Branch(0), loc);
                    self.push_state(ParseState::Loop {
                        start_idx,
                        branch_idx: idx,
                        loc,
                        pop_scope: false,
                    });
                    self.parse_scope()?;
                }
                ParseState::ForInit { counter_name, loc } => {
                    expect_token!(self, Token::Range);
                    let store_name = self.pooled_str(counter_name);
                    self.curr_func_mut()
                        .append_instr(Bc::Store(store_name), loc);
                    let start_idx = self.curr_func().next_instr_idx();
                    self.push_state(ParseState::ForCond {
                        idx: start_idx,
                        counter_name,
                        loc,
                    });
                    self.push_state(ParseState::Expr);
                }
                ParseState::ForCond {
                    idx: start_idx,
                    counter_name,
                    loc,
                } => {
                    let ref_name = self.pooled_str(counter_name);
                    self.curr_func_mut().append_instr(Bc::Ref(ref_name), loc);
                    self.curr_func_mut().append_instr(Bc::BinOp(BinOp::Gt), loc);
                    let idx = self.curr_func().next_instr_idx();
                    self.curr_func_mut().append_instr(Bc::Branch(0), loc);
                    self.push_state(ParseState::Loop {
                        start_idx,
                        branch_idx: idx,
                        loc,
                        pop_scope: true,
                    });
                    self.push_state(ParseState::ForInc { counter_name, loc });
                    self.parse_scope()?;
                }
                ParseState::ForInc { counter_name, loc } => {
                    let counter_name = self.pooled_str(counter_name);
                    self.curr_func_mut()
                        .append_instr(Bc::Ref(counter_name.clone()), loc);
                    self.curr_func_mut().append_instr(Bc::Imm(Val::Int(1)), loc);
                    self.curr_func_mut()
                        .append_instr(Bc::BinOp(BinOp::Add), loc);
                    self.curr_func_mut()
                        .append_instr(Bc::Store(counter_name), loc);
                }
                ParseState::Loop {
                    branch_idx,
                    start_idx,
                    loc,
                    pop_scope,
                } => {
                    let jump_idx = self.curr_func().next_instr_idx();
                    let start_off = start_idx - jump_idx - 1;
                    self.curr_func_mut().append_instr(Bc::Jump(start_off), loc);
                    let end_idx = self.curr_func().next_instr_idx();
                    *self.curr_func_mut().instr_at_mut(branch_idx) =
                        Bc::Branch(end_idx - branch_idx - 1);
                    if pop_scope {
                        self.discard_after_call();
                        self.curr_func_mut().append_instr(Bc::Pop, loc);
                    }
                }
                ParseState::Assign { lhs, loc } => {
                    let lhs = self.pooled_str(lhs);
                    self.curr_func_mut().append_instr(Bc::Store(lhs), loc);
                }
                ParseState::IndexAssign => {
                    expect_token!(self, Token::RBrack);
                    expect_token!(self, Token::Assign);
                }
                ParseState::IndexSet { loc } => {
                    self.curr_func_mut().append_instr(Bc::Set, loc);
                }
                ParseState::Expr => {
                    self.push_state(ParseState::Logic);
                    self.push_state(ParseState::Relation);
                    self.push_state(ParseState::Term);
                    self.push_state(ParseState::Factor);
                    self.push_state(ParseState::Primary);
                }
                ParseState::Logic => {
                    if let Some(TokenLoc {
                        token: Token::BinOp(op @ BinOp::And) | Token::BinOp(op @ BinOp::Or),
                        loc,
                    }) = self.curr_tok
                    {
                        self.advance()?;
                        self.push_state(ParseState::Logic);
                        self.push_state(ParseState::BinOp { op, loc });
                        self.push_state(ParseState::Relation);
                        self.push_state(ParseState::Term);
                        self.push_state(ParseState::Factor);
                        self.push_state(ParseState::Primary);
                    }
                }
                ParseState::Relation => {
                    if let Some(TokenLoc {
                        token:
                            Token::BinOp(
                                op @ BinOp::Lt
                                | op @ BinOp::Leq
                                | op @ BinOp::Gt
                                | op @ BinOp::Geq
                                | op @ BinOp::Eq
                                | op @ BinOp::Neq,
                            ),
                        loc,
                    }) = self.curr_tok
                    {
                        self.advance()?;
                        self.push_state(ParseState::Relation);
                        self.push_state(ParseState::BinOp { op, loc });
                        self.push_state(ParseState::Term);
                        self.push_state(ParseState::Factor);
                        self.push_state(ParseState::Primary);
                    }
                }
                ParseState::Term => {
                    if let Some(TokenLoc {
                        token: Token::BinOp(op @ BinOp::Add | op @ BinOp::Sub),
                        loc,
                    }) = self.curr_tok
                    {
                        self.advance()?;
                        self.push_state(ParseState::Term);
                        self.push_state(ParseState::BinOp { op, loc });
                        self.push_state(ParseState::Factor);
                        self.push_state(ParseState::Primary);
                    }
                }
                ParseState::Factor => {
                    if let Some(TokenLoc {
                        token: Token::BinOp(op @ BinOp::Mul | op @ BinOp::Div | op @ BinOp::Mod),
                        loc,
                    }) = self.curr_tok
                    {
                        self.advance()?;
                        self.push_state(ParseState::Factor);
                        self.push_state(ParseState::BinOp { op, loc });
                        self.push_state(ParseState::Primary);
                    }
                }
                ParseState::Primary => match self.expect_curr_tok()? {
                    TokenLoc {
                        token: Token::Int(value),
                        loc,
                    } => {
                        self.curr_func_mut()
                            .append_instr(Bc::Imm(Val::Int(value)), loc);
                        self.advance()?;
                    }
                    TokenLoc {
                        token: Token::Float(value),
                        loc,
                    } => {
                        self.curr_func_mut()
                            .append_instr(Bc::Imm(Val::Float(value)), loc);
                        self.advance()?;
                    }
                    TokenLoc {
                        token: Token::True,
                        loc,
                    } => {
                        self.curr_func_mut()
                            .append_instr(Bc::Imm(Val::Bool(true)), loc);
                        self.advance()?;
                    }
                    TokenLoc {
                        token: Token::False,
                        loc,
                    } => {
                        self.curr_func_mut()
                            .append_instr(Bc::Imm(Val::Bool(false)), loc);
                        self.advance()?;
                    }
                    TokenLoc {
                        token: Token::Not,
                        loc,
                    } => {
                        self.push_state(ParseState::UnOp { op: UnOp::Not, loc });
                        self.push_state(ParseState::Primary);
                        self.advance()?;
                    }
                    TokenLoc {
                        token: Token::BinOp(BinOp::Sub),
                        loc,
                    } => {
                        self.push_state(ParseState::UnOp { op: UnOp::Neg, loc });
                        self.push_state(ParseState::Primary);
                        self.advance()?;
                    }
                    TokenLoc {
                        token: Token::String(string),
                        loc,
                    } => {
                        let mut value = String::new();
                        value.reserve(string.len());
                        let mut escaped = false;
                        for c in string.chars() {
                            escaped = match c {
                                '\\' => {
                                    if escaped {
                                        value.push('\\');
                                    }
                                    !escaped
                                }
                                'n' => {
                                    if escaped {
                                        value.push('\n');
                                    } else {
                                        value.push('n');
                                    }
                                    false
                                }
                                c => {
                                    value.push(c);
                                    false
                                }
                            }
                        }
                        let val = Val::String(self.pooled_str(&value));
                        self.curr_func_mut().append_instr(Bc::Imm(val), loc);
                        self.advance()?;
                    }
                    TokenLoc {
                        token: Token::Ident(name),
                        loc,
                    } => {
                        self.advance()?;
                        match self.curr_tok {
                            Some(TokenLoc {
                                token: Token::Dot, ..
                            }) => {
                                self.advance()?;
                                let TokenLoc {
                                    token: Token::Ident(field),
                                    loc,
                                } = self.expect_curr_tok()?
                                else {
                                    return Err(Error {
                                        loc: self.expect_curr_tok()?.loc,
                                        detail: ErrorDetail::SyntaxError,
                                    });
                                };
                                self.advance()?;
                                let name = self.pooled_str(name);
                                self.curr_func_mut().append_instr(Bc::Ref(name), loc);
                                let field = Val::String(self.pooled_str(field));
                                self.curr_func_mut().append_instr(Bc::Imm(field), loc);
                                self.curr_func_mut().append_instr(Bc::Get, loc);
                            }
                            Some(TokenLoc {
                                token: Token::LBrack,
                                ..
                            }) => {
                                self.advance()?;
                                let name = self.pooled_str(name);
                                self.curr_func_mut().append_instr(Bc::Ref(name), loc);
                                self.push_state(ParseState::IndexGet { loc });
                                self.push_state(ParseState::Expr);
                            }
                            Some(TokenLoc {
                                token: Token::LParen,
                                loc,
                            }) => {
                                self.advance()?;
                                self.push_state(ParseState::Call { name, args: 0, loc });
                            }
                            _ => {
                                let name = self.pooled_str(name);
                                self.curr_func_mut().append_instr(Bc::Ref(name), loc);
                            }
                        }
                    }
                    TokenLoc {
                        token: Token::LParen,
                        ..
                    } => {
                        self.advance()?;
                        self.push_state(ParseState::Paren);
                        self.push_state(ParseState::Expr);
                    }
                    TokenLoc {
                        token: Token::LBrack,
                        loc,
                    } => {
                        self.advance()?;
                        self.curr_func_mut().append_instr(Bc::Table, loc);
                        self.push_state(ParseState::Table {
                            count: 0,
                            field: None,
                        });
                    }
                    TokenLoc {
                        token: Token::Fn,
                        loc,
                    } => {
                        self.advance()?;
                        let args = self.parse_func_args()?;
                        let func = self.text.add_func(format!("<func @ {loc}>").into(), args);
                        self.push_state(ParseState::Func {
                            func,
                            name: None,
                            loc,
                        });
                        self.push_func(func);
                        self.parse_scope()?;
                    }
                    _ => {
                        return Err(Error {
                            loc: self.expect_curr_tok()?.loc,
                            detail: ErrorDetail::SyntaxError,
                        })
                    }
                },
                ParseState::IndexGet { loc } => {
                    expect_token!(self, Token::RBrack);
                    self.curr_func_mut().append_instr(Bc::Get, loc);
                }
                ParseState::Call { name, args, loc } => {
                    if let Token::RParen = self.expect_curr_tok()?.token {
                        let name = self.pooled_str(name);
                        self.curr_func_mut()
                            .append_instr(Bc::Call { name, args }, loc);
                        self.advance()?;
                    } else {
                        if args > 0 {
                            expect_token!(self, Token::Comma);
                        }
                        let args = args + 1;
                        self.push_state(ParseState::Call { name, args, loc });
                        self.push_state(ParseState::Expr);
                    }
                }
                ParseState::Table { count, field } => {
                    if let Some(loc) = field {
                        self.curr_func_mut().append_instr(Bc::Set, loc);
                    }
                    if let Some(TokenLoc {
                        token: Token::RBrack,
                        ..
                    }) = self.curr_tok
                    {
                        self.advance()?;
                        continue;
                    }
                    if field.is_some() {
                        expect_token!(self, Token::Comma);
                    }
                    if let Some(TokenLoc {
                        token: Token::Ident(field),
                        loc,
                    }) = self.curr_tok
                    {
                        if let Some(TokenLoc {
                            token: Token::Assign,
                            ..
                        }) = self.peek()?
                        {
                            self.advance()?;
                            expect_token!(self, Token::Assign);
                            let val = Val::String(self.pooled_str(field));
                            self.curr_func_mut().append_instr(Bc::Imm(val), loc);
                            self.push_state(ParseState::Table {
                                count: count + 1,
                                field: Some(loc),
                            });
                            self.push_state(ParseState::Expr);
                            continue;
                        }
                    }
                    let Some(TokenLoc { loc, .. }) = self.curr_tok else {
                        return Err(Error {
                            loc: self.expect_curr_tok()?.loc,
                            detail: ErrorDetail::SyntaxError,
                        });
                    };
                    self.curr_func_mut()
                        .append_instr(Bc::Imm(Val::Int(count)), loc);
                    self.push_state(ParseState::Table {
                        count: count + 1,
                        field: Some(loc),
                    });
                    self.push_state(ParseState::Expr);
                }
                ParseState::Paren => {
                    expect_token!(self, Token::RParen);
                }
                ParseState::UnOp { op, loc } => {
                    self.curr_func_mut().append_instr(Bc::UnOp(op), loc);
                }
                ParseState::BinOp { op, loc } => {
                    self.curr_func_mut().append_instr(Bc::BinOp(op), loc);
                }
                ParseState::Return { loc } => {
                    self.curr_func_mut().append_instr(Bc::Ret, loc);
                }
            }
        }
        Ok(take(&mut self.text))
    }

    /// Starts parsing a single statement.
    /// It usually does not fully parse the statement, but only sets up parser states for `parse()`.
    fn parse_stmt(&mut self) -> Result<()> {
        self.discard_after_call();
        let curr_tok = self.expect_curr_tok()?;
        let loc = curr_tok.loc;
        match curr_tok.token {
            Token::Ident(name) => {
                let lhs = name;
                match self.peek()? {
                    Some(TokenLoc {
                        token: Token::Assign,
                        ..
                    }) => {
                        self.advance()?; // Identifier
                        self.advance()?; // Assign
                        self.push_state(ParseState::Assign { lhs, loc });
                        self.push_state(ParseState::Expr);
                    }
                    Some(TokenLoc {
                        token: Token::Dot, ..
                    }) => {
                        let lhs = self.pooled_str(lhs);
                        self.curr_func_mut().append_instr(Bc::Ref(lhs), loc);
                        self.advance()?; // Identifier (var)
                        self.advance()?; // Dot
                        let TokenLoc {
                            token: Token::Ident(ident),
                            loc,
                        } = self.expect_curr_tok()?
                        else {
                            return Err(Error {
                                loc: self.expect_curr_tok()?.loc,
                                detail: ErrorDetail::SyntaxError,
                            });
                        };
                        self.advance()?; // Identifier (field)
                        expect_token!(self, Token::Assign);
                        let val = Val::String(self.pooled_str(ident));
                        self.curr_func_mut().append_instr(Bc::Imm(val), loc);
                        self.push_state(ParseState::IndexSet { loc });
                        self.push_state(ParseState::Expr);
                    }
                    Some(TokenLoc {
                        token: Token::LBrack,
                        ..
                    }) => {
                        let lhs = self.pooled_str(lhs);
                        self.curr_func_mut().append_instr(Bc::Ref(lhs), loc);
                        self.advance()?; // Identifier (var)
                        self.advance()?; // LBrack
                        self.push_state(ParseState::IndexSet { loc });
                        self.push_state(ParseState::Expr);
                        self.push_state(ParseState::IndexAssign);
                        self.push_state(ParseState::Expr);
                    }
                    _ => {
                        self.push_state(ParseState::Primary);
                    }
                }
            }
            Token::LBrace => {
                self.parse_scope()?;
            }
            Token::If => {
                self.advance()?;
                self.push_state(ParseState::If { loc });
                self.push_state(ParseState::Expr);
            }
            Token::While => {
                self.advance()?;
                let start_idx = self.curr_func().next_instr_idx();
                self.push_state(ParseState::While {
                    idx: start_idx,
                    loc,
                });
                self.push_state(ParseState::Expr);
            }
            Token::For => {
                self.advance()?;
                let TokenLoc {
                    token: Token::Ident(counter_name),
                    loc,
                } = self.expect_curr_tok()?
                else {
                    return Err(Error {
                        loc: self.expect_curr_tok()?.loc,
                        detail: ErrorDetail::SyntaxError,
                    });
                };
                self.advance()?;
                expect_token!(self, Token::In);
                self.push_state(ParseState::ForInit { counter_name, loc });
                self.push_state(ParseState::Expr);
                self.curr_func_mut().append_instr(Bc::Push, loc);
            }
            Token::Return => {
                self.push_state(ParseState::Return { loc: curr_tok.loc });
                self.push_state(ParseState::Expr);
                self.advance()?;
            }
            _ => {
                return Err(Error {
                    loc: curr_tok.loc,
                    detail: ErrorDetail::SyntaxError,
                })
            }
        }
        Ok(())
    }

    /// Sets up parsing a new scope (between `{` and `}`).
    /// It usually does not fully parse the scope, but only sets up parser states for `parse()`.
    fn parse_scope(&mut self) -> Result<()> {
        expect_token!(self, Token::LBrace);
        let loc = self.expect_curr_tok()?.loc;
        self.curr_func_mut().append_instr(Bc::Push, loc);
        self.parse_end_scope()
    }

    /// Parses the end of a scope, or sets up states for parsing another statement and the end of scope after that.
    fn parse_end_scope(&mut self) -> Result<()> {
        let curr_tok = self.expect_curr_tok()?;
        if let Token::RBrace = curr_tok.token {
            self.advance()?;
            self.discard_after_call();
            self.curr_func_mut().append_instr(Bc::Pop, curr_tok.loc);
        } else {
            self.push_state(ParseState::Scope);
            self.parse_stmt()?;
        }
        Ok(())
    }

    /// Pushes an instruction that discards the result of a function call if it is not used.
    /// Used at the start of each statement that occurs after a function call.
    fn discard_after_call(&mut self) {
        if let Some(Bc::Call { .. }) = self.curr_func().instr_last() {
            let loc = self
                .curr_func()
                .instr_loc_last()
                .expect("Last loc should exist if last instr does");
            self.curr_func_mut().append_instr(Bc::Discard, loc);
        }
    }

    /// Parses function arguments.
    fn parse_func_args(&mut self) -> Result<Vec<Rc<str>>> {
        expect_token!(self, Token::LParen);
        let curr_tok = self.expect_curr_tok()?;
        let mut args = Vec::new();
        if let Token::Ident(ident) = curr_tok.token {
            args.push(ident.into());
            self.advance()?;
        }
        let mut curr_tok = self.expect_curr_tok()?;
        while let Token::Comma = curr_tok.token {
            self.advance()?;
            curr_tok = self.expect_curr_tok()?;
            if let Token::Ident(ident) = curr_tok.token {
                args.push(ident.into());
                self.advance()?;
                curr_tok = self.expect_curr_tok()?;
            } else {
                return Err(Error {
                    loc: curr_tok.loc,
                    detail: ErrorDetail::SyntaxError,
                });
            }
        }
        expect_token!(self, Token::RParen);
        Ok(args)
    }

    /// If the given string is already in the string pool, returns a `Rc` to it.
    /// Otherwise, it adds the string to the pool and returns a `Rc` to it.
    fn pooled_str(&mut self, string: &str) -> Rc<str> {
        if let Some(rc_str) = self.str_pool.get(string) {
            rc_str.clone()
        } else {
            let rc_str: Rc<str> = string.into();
            self.str_pool.insert(rc_str.clone());
            rc_str
        }
    }

    /// Pushes the given function reference onto the function stack.
    fn push_func(&mut self, func: FuncRef) {
        self.func_stack.push(func);
    }

    /// Returns a mutable reference to the currently parsed function.
    fn curr_func_mut(&mut self) -> &mut Func {
        &mut self.text[*self
            .func_stack
            .last()
            .expect("Should have at least one function")]
    }

    /// Returns a reference to the currently parsed function.
    fn curr_func(&mut self) -> &Func {
        &self.text[*self
            .func_stack
            .last()
            .expect("Should have at least one function")]
    }

    /// Pushes a new state onto the parser's state stack.
    fn push_state(&mut self, state: ParseState<'src>) {
        self.state_stack.push(state);
    }

    /// Advances to the next token.
    fn advance(&mut self) -> Result<()> {
        if let Some(TokenLoc { loc, .. }) = self.curr_tok {
            self.last_loc = loc
        }
        self.curr_tok = None;
        match self.lexer.next() {
            Some(Ok(TokenLoc {
                token: Token::Comment(_),
                ..
            })) => self.advance(),
            Some(Ok(tok)) => {
                self.curr_tok = Some(tok);
                Ok(())
            }
            Some(Err(err)) => Err(err),
            None => Ok(()),
        }
    }

    /// Peeks at the next token without advancing.
    fn peek(&mut self) -> Result<Option<&TokenLoc>> {
        match self.lexer.peek() {
            Some(Ok(tok)) => Ok(Some(tok)),
            Some(Err(err)) => Err(err.clone()),
            None => Ok(None),
        }
    }

    /// Expects the current token to be present and returns it.
    fn expect_curr_tok<'a>(&'a mut self) -> Result<TokenLoc<'src>> {
        match self.curr_tok {
            Some(tok) => Ok(tok),
            None => Err(Error {
                loc: self.last_loc,
                detail: ErrorDetail::UnexpectedEof,
            }),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Bc::*, BinOp::*, UnOp::*, *};

    macro_rules! parser_test {
        ($name:ident, $source:literal) => {
            #[test]
            fn $name() {
                let source = crate::Source {
                    name: format!("<{}>", stringify!($name)),
                    contents: "fn f() {}".to_owned(),
                };
                let lexer = Lexer::new(&source);
                let mut parser = Parser::new(lexer);
                parser.parse().expect("Parse should succeed");
            }
        };
        ($name:ident, $source:literal, Err($detail:tt), $line:literal : $col:literal : $offset:literal) => {
            #[test]
            fn $name() {
                let source = &crate::Source {
                    name: format!("<{}>", stringify!($name)),
                    contents: $source.to_owned(),
                };
                let lexer = Lexer::new(&source);
                let mut parser = Parser::new(lexer);
                let Err(err) = parser.parse() else {
                    panic!("Expected error, but parsing succeeded");
                };
                assert_eq!(
                    err,
                    Error {
                        loc: SourceLoc {
                            offset: $offset,
                            line: $line,
                            col: $col,
                        },
                        detail: ErrorDetail::$detail,
                    }
                );
            }
        };
        ($name:ident, $source:literal, $expected:expr) => {
            #[test]
            fn $name() {
                let source = crate::Source {
                    name: format!("<{}>", stringify!($name)),
                    contents: $source.to_owned(),
                };
                let lexer = Lexer::new(&source);
                let mut parser = Parser::new(lexer);
                let text = parser.parse().expect("Parse should succeed");
                let expected: &[(&str, &[&str], &[Bc])] = &$expected;
                for (func, args, instrs) in expected.iter() {
                    let refe = text
                        .func_ref(func)
                        .expect(&format!("Function {func} should exist"));
                    let func = &text[refe];
                    assert_eq!(
                        func.iter_args().map(|s| s.as_ref()).collect::<Vec<&str>>(),
                        *args
                    );
                    for (i, instr) in instrs.iter().enumerate() {
                        assert_eq!(*func.instr_at(BcIdx(i as u32)), *instr);
                    }
                    assert_eq!(func.instr_count(), instrs.len());
                }
            }
        };
    }
    parser_test!(empty, "");
    parser_test!(empty_func, "fn f() {}", [("f", &[], &[])]);
    parser_test!(
        call,
        "fn f() {} fn g() { f() }",
        [
            ("f", &[], &[]),
            (
                "g",
                &[],
                &[Call {
                    name: "f".into(),
                    args: 0
                },]
            )
        ]
    );
    parser_test!(
        call_args,
        "fn f(a, b) {} fn g() { f(1, 2) }",
        [
            ("f", &["a", "b"], &[]),
            (
                "g",
                &[],
                &[
                    Imm(Val::Int(1)),
                    Imm(Val::Int(2)),
                    Call {
                        name: "f".into(),
                        args: 2
                    },
                ]
            )
        ]
    );
    parser_test!(
        return_stmt,
        "fn f() { return 1 }",
        [("f", &[], &[Imm(Val::Int(1)), Ret,])]
    );
    parser_test!(
        assign,
        "fn f() { a = 1 }",
        [("f", &[], &[Imm(Val::Int(1)), Store("a".into()),])]
    );
    parser_test!(
        assign_field,
        "fn f(a) { a.b = 1 }",
        [(
            "f",
            &["a"],
            &[
                Ref("a".into()),
                Imm(Val::from_str("b")),
                Imm(Val::Int(1)),
                Set,
            ]
        )]
    );
    parser_test!(
        assign_index,
        "fn f(a) { a[1] = 2 }",
        [(
            "f",
            &["a"],
            &[Ref("a".into()), Imm(Val::Int(1)), Imm(Val::Int(2)), Set,]
        )]
    );
    parser_test!(
        precedence,
        "fn f() { return 2 / 1 < 2 || 1 + 2 * 3 }",
        [(
            "f",
            &[],
            &[
                Imm(Val::Int(2)),
                Imm(Val::Int(1)),
                BinOp(Div),
                Imm(Val::Int(2)),
                BinOp(Lt),
                Imm(Val::Int(1)),
                Imm(Val::Int(2)),
                Imm(Val::Int(3)),
                BinOp(Mul),
                BinOp(Add),
                BinOp(Or),
                Ret,
            ]
        )]
    );
    parser_test!(
        negate,
        "fn f() { return -1 }",
        [("f", &[], &[Imm(Val::Int(1)), UnOp(Neg), Ret,])]
    );
    parser_test!(
        if_then,
        "fn f() { if false {} }",
        [("f", &[], &[Imm(Val::Bool(false)), Branch(2), Push, Pop,])]
    );
    parser_test!(
        if_else,
        "fn f() { if true {} else {} }",
        [(
            "f",
            &[],
            &[
                Imm(Val::Bool(true)),
                Branch(3),
                Push,
                Pop,
                Jump(2),
                Push,
                Pop,
            ]
        )]
    );
    parser_test!(
        while_loop,
        "fn f() { while true {} }",
        [(
            "f",
            &[],
            &[Imm(Val::Bool(true)), Branch(3), Push, Pop, Jump(-5),]
        )]
    );
    parser_test!(
        for_loop,
        "fn f() { for i in 1..2 {} }",
        [(
            "f",
            &[],
            &[
                Push,
                Imm(Val::Int(1)),
                Store("i".into()),
                Imm(Val::Int(2)),
                Ref("i".into()),
                BinOp(Gt),
                Branch(7),
                Push,
                Pop,
                Ref("i".into()),
                Imm(Val::Int(1)),
                BinOp(Add),
                Store("i".into()),
                Jump(-11),
                Pop,
            ]
        )]
    );
    parser_test!(
        table_map,
        "fn f() { x = [a=1, b=2] }",
        [(
            "f",
            &[],
            &[
                Table,
                Imm(Val::from_str("a")),
                Imm(Val::Int(1)),
                Set,
                Imm(Val::from_str("b")),
                Imm(Val::Int(2)),
                Set,
                Store("x".into()),
            ]
        )]
    );
    parser_test!(
        table_array,
        "fn f() { x = [1, 2] }",
        [(
            "f",
            &[],
            &[
                Table,
                Imm(Val::Int(0)),
                Imm(Val::Int(1)),
                Set,
                Imm(Val::Int(1)),
                Imm(Val::Int(2)),
                Set,
                Store("x".into()),
            ]
        )]
    );
    parser_test!(error_1, "fn f() {", Err(UnexpectedEof), 0:7:7);
    parser_test!(error_2, "fn f() {\n    if\n}", Err(SyntaxError), 2:0:16);
}
