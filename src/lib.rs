mod eval;
mod lex;
mod parse;
mod text;

pub use eval::{eval_src as eval, EvalError, EvalResult, Heap, Table};
use std::{collections::HashSet, fmt, hash::Hash, ops, rc::Rc};

/// Evaluates the source code in debug mode, printing each instruction and stack state.
pub fn eval_dbg(source: &Source) -> Result<(Val, Heap)> {
    let lexer = lex::Lexer::new(source);
    let mut parser = parse::Parser::new(lexer);
    let text = parser.parse()?;
    eval::eval_dbg(text)
}

/// Emits the bytecode for the given source code.
pub fn emit_bc(source: &Source) -> Result<()> {
    let lexer = lex::Lexer::new(source);
    let mut parser = parse::Parser::new(lexer);
    let text = parser.parse()?;
    println!("{text}");
    Ok(())
}

/// Error with source location and detail information.
#[derive(Debug, Clone, PartialEq)]
pub struct Error<'src> {
    /// Location of the error in the source code.
    pub loc: SourceLoc<'src>,
    /// Details about the error.
    pub detail: ErrorDetail<'src>,
}

impl fmt::Display for Error<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{err} at {loc}", err = self.detail, loc = self.loc)
    }
}

/// The details (e.g. message) of an error.
#[derive(Debug, Clone, PartialEq)]
pub enum ErrorDetail<'src> {
    /// An invalid token was encountered during lexing.
    InvalidToken,
    /// End of file was encountered too early during parsing.
    UnexpectedEof,
    /// Invalid syntax was encountered during parsing.
    SyntaxError,
    /// An operation was attempted on incompatible types.
    TypeError,
    /// An undefined variable was referenced during evaluation.
    Undefined(String),
    /// An internal error occurred; indicates a bug.
    InternalError(&'src str),
}

impl fmt::Display for ErrorDetail<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ErrorDetail::InvalidToken => write!(f, "Invalid token"),
            ErrorDetail::UnexpectedEof => write!(f, "Unexpected EOF"),
            ErrorDetail::SyntaxError => write!(f, "Syntax error"),
            ErrorDetail::TypeError => write!(f, "Type error"),
            ErrorDetail::Undefined(name) => write!(f, "'{name}' is undefined"),
            ErrorDetail::InternalError(msg) => write!(f, "Internal error: {msg}"),
        }
    }
}

/// Result with an error type of `Error`.
pub type Result<'src, T> = std::result::Result<T, Error<'src>>;

/// Unary operation.
#[derive(Debug, Clone, Copy, PartialEq)]
enum UnOp {
    Not,
    Neg,
}

impl fmt::Display for UnOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use UnOp::*;
        write!(
            f,
            "{}",
            match self {
                Not => "not",
                Neg => "neg",
            }
        )
    }
}

/// Binary operation.
#[derive(Debug, Clone, Copy, PartialEq)]
enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Lt,
    Leq,
    Gt,
    Geq,
    Eq,
    Neq,
    And,
    Or,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use BinOp::*;
        write!(
            f,
            "{}",
            match self {
                Add => "add",
                Sub => "sub",
                Mul => "mul",
                Div => "div",
                Mod => "mod",
                Lt => "lt",
                Leq => "leq",
                Gt => "gt",
                Geq => "geq",
                Eq => "eq",
                Neq => "neq",
                And => "and",
                Or => "or",
            }
        )
    }
}

/// Token extracted from source code.
#[derive(Debug, Clone, Copy, PartialEq)]
enum Token<'src> {
    Comment(&'src str),
    Ident(&'src str),
    Int(i64),
    Float(f64),
    String(&'src str),
    Fn,
    If,
    Else,
    While,
    For,
    In,
    Return,
    True,
    False,
    Not,
    BinOp(BinOp),
    Comma,
    Dot,
    Range,
    LParen,
    RParen,
    LBrack,
    RBrack,
    LBrace,
    RBrace,
    Assign,
}

/// Source code with its name and contents.
#[derive(Debug, Clone)]
pub struct Source {
    /// Name of the source (e.g. filename).
    pub name: String,
    /// The actual source code.
    pub contents: String,
}

/// Byte offset in source code.
pub type Offset = usize;

/// Line number in source code.
pub type LineNr = u32;

/// Column number in source code.
pub type ColNr = u32;

/// Location (offset, line, column) in source code.
#[derive(Clone, Copy)]
pub struct SourceLoc<'src> {
    /// Source code.
    pub source: &'src Source,
    /// Byte offset in the source.
    pub offset: Offset,
    /// Line number in the source.
    pub line: LineNr,
    /// Column number in the source.
    pub col: ColNr,
}

impl PartialEq for SourceLoc<'_> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.source, other.source)
            && self.offset == other.offset
            && self.line == other.line
            && self.col == other.col
    }
}

impl fmt::Display for SourceLoc<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{source}:{line}:{col}",
            source = self.source.name,
            line = self.line + 1,
            col = self.col + 1,
        )
    }
}

impl fmt::Debug for SourceLoc<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "SourceLoc {{ source: Source {{ name: {name:?}, .. }}, offset: {offset:?}, line: {line:?}, col: {col:?} }}",
            name = self.source.name,
            offset = self.offset,
            line = self.line,
            col = self.col
        )
    }
}

/// Token with its location in source code.
#[derive(Debug, Clone, Copy, PartialEq)]
struct TokenLoc<'src> {
    /// Token extracted from source code.
    token: Token<'src>,
    /// Location of the token in the source code.
    loc: SourceLoc<'src>,
}

/// A native function callable through FFI.
pub trait ForeignFn: Fn(&[Val], &mut Heap) -> EvalResult<Val> {}
impl<F> ForeignFn for F where F: Fn(&[Val], &mut Heap) -> EvalResult<Val> {}

/// Wrapper around native functions callable through FFI.
#[derive(Clone)]
pub struct ForeignFunc(Rc<dyn ForeignFn>);

impl<F: ForeignFn + 'static> From<F> for ForeignFunc {
    fn from(func: F) -> Self {
        ForeignFunc(Rc::from(func))
    }
}

impl ops::Deref for ForeignFunc {
    type Target = Rc<dyn ForeignFn>;

    fn deref(&self) -> &Self::Target {
        let ForeignFunc(refe) = self;
        refe
    }
}

impl fmt::Debug for ForeignFunc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ForeignFunc(refe) = self;
        write!(f, "ForeignFunc({:?})", refe as *const _)
    }
}

impl fmt::Display for ForeignFunc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let ForeignFunc(refe) = self;
        write!(f, "{:?}", refe as *const _)
    }
}

impl PartialEq for ForeignFunc {
    fn eq(&self, other: &Self) -> bool {
        let ForeignFunc(refe) = self;
        let ForeignFunc(other_refe) = other;
        std::ptr::eq(refe.as_ref(), other_refe.as_ref())
    }
}

/// Reference to a function.
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FuncRef(u32);

impl fmt::Display for FuncRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<func {:#x}>", self.0)
    }
}

/// Values the language operates on.
#[derive(Debug, Clone, PartialEq)]
pub enum Val {
    /// Represents an unset value.
    None,
    /// Boolean true or false.
    Bool(bool),
    /// 64-bit signed integer.
    Int(i64),
    /// 64-bit floating point number.
    Float(f64),
    /// Unicode string.
    String(Rc<str>),
    /// Map from bool, int, string, and table keys to any values.
    Table(TableRef),
    /// Function reference with no scopes.
    Func(FuncRef),
    /// Function reference with captured scopes.
    Closure {
        /// Function reference.
        func: FuncRef,
        /// Scopes captured by the function.
        scopes: Rc<[TableRef]>,
    },
    /// FFI function reference.
    ForeignFunc(ForeignFunc),
}

impl Val {
    /// Constructs a `Val::String` from a string.
    fn from_str<S: AsRef<str>>(string: S) -> Self {
        Val::String(Rc::from(string.as_ref()))
    }

    /// Constructs a `Val::ForeignFunc` from a native function.
    fn from_fn<F: ForeignFn + 'static>(func: F) -> Self {
        Val::ForeignFunc(ForeignFunc::from(func))
    }
}

impl fmt::Display for Val {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Val::None => write!(f, "<none>"),
            Val::Bool(boolean) => write!(f, "{boolean}"),
            Val::Int(int) => write!(f, "{int}"),
            Val::Float(float) => write!(f, "{float}"),
            Val::String(string) => write!(f, "{string}"),
            Val::Table(refe) => write!(f, "{refe}"),
            Val::Func(FuncRef(refe)) => write!(f, "<func {refe:#x}>"),
            Val::Closure {
                func: FuncRef(refe),
                scopes,
            } => {
                write!(f, "<func {refe:#x} [")?;
                for scope in scopes.iter() {
                    write!(f, " {scope}")?;
                }
                write!(f, "]>")
            }
            Val::ForeignFunc(func) => write!(f, "<foreign func {func}>"),
        }
    }
}

/// Reference to a table in `Heap`.
#[derive(Debug, Clone, Copy, PartialEq, PartialOrd, Ord, Hash)]
pub struct TableRef(u32);

impl TableRef {
    pub fn format(self, heap: &Heap) -> TableFmt {
        TableFmt {
            refe: self,
            heap,
            quote_str: false,
            force_dec_sep: false,
        }
    }
}

impl Eq for TableRef {}

impl fmt::Display for TableRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "<table {:#x}>", self.0)
    }
}

/// Helper for pretty printing tables.
pub struct TableFmt<'a> {
    /// Reference to the table to be printed.
    refe: TableRef,
    /// `Heap` that contains the table to be printed.
    heap: &'a Heap,
    /// If true, string values are always printed with quotes.
    quote_str: bool,
    /// If true, float values are always printed with a decimal separator.
    force_dec_sep: bool,
}

impl TableFmt<'_> {
    /// Makes all string values quoted.
    pub fn quote_strings(self) -> Self {
        Self {
            quote_str: true,
            ..self
        }
    }

    /// Forces decimal separator for all float values.
    pub fn force_decimal_sep(self) -> Self {
        Self {
            force_dec_sep: true,
            ..self
        }
    }
}

impl fmt::Display for TableFmt<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fn fmt_rec(
            this: &TableFmt,
            refe: TableRef,
            f: &mut fmt::Formatter<'_>,
            visited: &mut HashSet<TableRef>,
            key: Option<&Val>,
        ) -> fmt::Result {
            let table = this.heap.get(refe);
            if let Some(key) = key {
                write!(f, "{key} = ")?;
            }
            if visited.get(&refe).is_some() {
                return write!(f, "{refe}");
            }
            visited.insert(refe);
            write!(f, "[")?;
            let mut first = true;
            for (key, val) in table.iter() {
                if !first {
                    write!(f, ", ")?;
                }
                first = false;
                if let &Val::Table(refe) = val {
                    if matches!(table, Table::Array(_)) {
                        fmt_rec(this, refe, f, visited, None)?;
                    } else {
                        fmt_rec(this, refe, f, visited, Some(&key))?;
                    }
                } else {
                    if !matches!(table, Table::Array(_)) {
                        write!(f, "{key} = ")?;
                    }
                    match val {
                        Val::Float(float) if this.force_dec_sep => write!(f, "{float:?}"),
                        Val::String(string) if this.quote_str => write!(f, "\"{string}\""),
                        val => write!(f, "{val}"),
                    }?;
                }
            }
            write!(f, "]")
        }
        fmt_rec(self, self.refe, f, &mut Default::default(), None)
    }
}
