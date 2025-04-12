use crate::text::Text;

use super::{
    lex::Lexer,
    parse::Parser,
    text::{Bc, BcIdx},
    BinOp, Error, ErrorDetail, FuncRef, Result, Source, SourceLoc, TableRef, UnOp, Val,
};
use std::{
    collections::{hash_map, HashMap},
    fmt,
    iter::Enumerate,
    mem::{replace, take},
    rc::Rc,
    slice,
};

/// Executes the given bytecode in the form of a `Text`.
fn eval(text: Text) -> Result<(Val, Heap)> {
    let mut proc = Process::new(text);
    while !proc.is_done() {
        proc.step()?;
    }
    Ok(proc.return_value())
}

/// Executes the given bytecode (given as a `Text`) in debug mode, printing each instruction and stack state.
pub(super) fn eval_dbg(text: Text) -> Result<(Val, Heap)> {
    let mut proc = Process::new(text);
    while !proc.is_done() {
        let instr = proc.stack.get_instr(&proc.text);
        let loc = proc.stack.get_instr_loc(&proc.text);
        eprint!("-> {instr} @ {loc} | stack: [");
        let frame_eval_stack = &proc.stack.vals[proc.stack.curr_frame().vals_idx..];
        for val in frame_eval_stack.iter() {
            eprint!(" {val}");
        }
        eprintln!(" ]");
        proc.step()?;
    }
    Ok(proc.return_value())
}

/// Parses and executes the source code.
pub fn eval_src(source: &Source) -> Result<(Val, Heap)> {
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);
    let text = parser.parse()?;
    eval(text)
}

/// Error that can occur during evaluation.
#[derive(Debug, Clone)]
pub enum EvalError {
    /// An operation was attempted on incompatible types.
    TypeError,
    /// An undefined variable was referenced during evaluation.
    Undefined(String),
    /// An internal error occurred; indicates a bug.
    InternalError(&'static str),
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            EvalError::TypeError => write!(f, "Type error"),
            EvalError::Undefined(name) => write!(f, "'{name}' is undefined"),
            EvalError::InternalError(msg) => write!(f, "{msg}"),
        }
    }
}

/// Result of evaluation.
pub type EvalResult<T> = std::result::Result<T, EvalError>;

impl EvalError {
    /// Converts an `EvalError` into an `Error` with a source location.
    fn with_loc(self, loc: SourceLoc) -> Error {
        match self {
            EvalError::TypeError => Error {
                loc,
                detail: ErrorDetail::TypeError,
            },
            EvalError::Undefined(name) => Error {
                loc,
                detail: ErrorDetail::Undefined(name),
            },
            EvalError::InternalError(msg) => Error {
                loc,
                detail: ErrorDetail::InternalError(msg),
            },
        }
    }
}

impl Val {
    /// Adds two values together.
    #[inline(always)]
    fn add(&self, other: &Self) -> EvalResult<Self> {
        use Val::*;
        Ok(match (self, other) {
            (&Int(left), &Int(other)) => Int(left + other),
            (&Float(left), &Float(other)) => Float(left + other),
            (&Int(left), &Float(other)) => Float(left as f64 + other),
            (&Float(left), &Int(other)) => Float(left + other as f64),
            (String(left), String(other)) => Val::from_str(format!("{left}{other}")),
            _ => return Err(EvalError::TypeError),
        })
    }

    /// Subtracts one value from another.
    #[inline(always)]
    fn sub(&self, other: &Self) -> EvalResult<Self> {
        use Val::*;
        Ok(match (self, other) {
            (&Int(left), &Int(other)) => Int(left - other),
            (&Float(left), &Float(other)) => Float(left - other),
            (&Int(left), &Float(other)) => Float(left as f64 - other),
            (&Float(left), &Int(other)) => Float(left - other as f64),
            _ => return Err(EvalError::TypeError),
        })
    }

    /// Multiplies two values together.
    #[inline(always)]
    fn mul(&self, other: &Self) -> EvalResult<Self> {
        use Val::*;
        Ok(match (self, other) {
            (&Int(left), &Int(other)) => Int(left * other),
            (&Float(left), &Float(other)) => Float(left * other),
            (&Int(left), &Float(other)) => Float(left as f64 * other),
            (&Float(left), &Int(other)) => Float(left * other as f64),
            _ => return Err(EvalError::TypeError),
        })
    }

    /// Divides one value by another.
    #[inline(always)]
    fn div(&self, other: &Self) -> EvalResult<Self> {
        use Val::*;
        Ok(match (self, other) {
            (&Int(left), &Int(other)) => Int(left / other),
            (&Float(left), &Float(other)) => Float(left / other),
            (&Int(left), &Float(other)) => Float(left as f64 / other),
            (&Float(left), &Int(other)) => Float(left / other as f64),
            _ => return Err(EvalError::TypeError),
        })
    }

    /// Computes the modulus of one value by another.
    #[inline(always)]
    fn modu(&self, other: &Self) -> EvalResult<Self> {
        use Val::*;
        Ok(match (self, other) {
            (&Int(left), &Int(other)) => Int(left % other),
            _ => return Err(EvalError::TypeError),
        })
    }

    /// Negates the value.
    #[inline(always)]
    fn neg(&self) -> EvalResult<Val> {
        Ok(match self {
            Val::Int(int) => Val::Int(-int),
            Val::Float(float) => Val::Float(-float),
            _ => return Err(EvalError::TypeError),
        })
    }

    /// Compares if one value is less than another.
    #[inline(always)]
    fn lt(&self, other: &Self) -> EvalResult<Self> {
        use Val::*;
        Ok(match (self, other) {
            (&Int(left), &Int(other)) => Bool(left < other),
            (&Float(left), &Float(other)) => Bool(left < other),
            (&Int(left), &Float(other)) => Bool((left as f64) < other),
            (&Float(left), &Int(other)) => Bool(left < other as f64),
            _ => return Err(EvalError::TypeError),
        })
    }

    /// Compares if one value is less than or equal to another.
    #[inline(always)]
    fn leq(&self, other: &Self) -> EvalResult<Self> {
        use Val::*;
        Ok(match (self, other) {
            (&Int(left), &Int(other)) => Bool(left <= other),
            (&Float(left), &Float(other)) => Bool(left <= other),
            (&Int(left), &Float(other)) => Bool(left as f64 <= other),
            (&Float(left), &Int(other)) => Bool(left <= other as f64),
            _ => return Err(EvalError::TypeError),
        })
    }

    /// Compares if two values are equal.
    #[inline(always)]
    fn eq(&self, other: &Self) -> EvalResult<Self> {
        use Val::*;
        Ok(match (self, other) {
            (None, None) => Bool(true),
            (None, _) => Bool(false),
            (_, None) => Bool(false),
            (&Bool(this), &Bool(other)) => Bool(this == other),
            (&Int(this), &Int(other)) => Bool(this == other),
            (&Float(this), &Int(other)) => Bool(this == other as f64),
            (&Float(this), &Float(other)) => Bool(this == other),
            (&Int(this), &Float(other)) => Bool(this as f64 == other),
            (String(this), String(other)) => Bool(this == other),
            (&Table(this), &Table(other)) => Bool(this == other),
            _ => Bool(false),
        })
    }

    /// Performs a logical AND operation between two values.
    #[inline(always)]
    fn and(&self, other: &Self) -> Self {
        use Val::*;
        match &(self, other) {
            (Bool(false), _) => Bool(false),
            (_, Bool(false)) => Bool(false),
            (_, None) => Bool(false),
            (None, _) => Bool(false),
            _ => Bool(true),
        }
    }

    /// Performs a logical OR operation between two values.
    #[inline(always)]
    fn or(&self, other: &Self) -> Self {
        use Val::*;
        match &(self, other) {
            (Bool(false), Bool(false))
            | (Bool(false), None)
            | (None, Bool(false))
            | (None, None) => Bool(false),
            _ => Bool(true),
        }
    }

    /// Performs a logical NOT on the value
    #[inline(always)]
    fn not(&self) -> Self {
        use Val::*;
        match self {
            None => Bool(true),
            Bool(b) => Bool(!b),
            _ => Bool(false),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Hash, PartialOrd, Ord)]
enum Key {
    Bool(bool),
    Int(i64),
    String(Rc<str>),
    Table(TableRef),
}

impl Eq for Key {}

impl Key {
    fn from_str<S: AsRef<str>>(string: S) -> Self {
        Key::String(Rc::from(string.as_ref()))
    }
}

impl TryFrom<&Val> for Key {
    type Error = EvalError;

    fn try_from(val: &Val) -> EvalResult<Self> {
        match &val {
            Val::Bool(b) => Ok(Key::Bool(*b)),
            Val::Int(i) => Ok(Key::Int(*i)),
            Val::String(s) => Ok(Key::String(s.clone())),
            Val::Table(refe) => Ok(Key::Table(*refe)),
            _ => Err(EvalError::TypeError),
        }
    }
}

impl TryFrom<Val> for Key {
    type Error = EvalError;

    fn try_from(val: Val) -> EvalResult<Self> {
        Key::try_from(&val)
    }
}

impl From<&Key> for Val {
    fn from(key: &Key) -> Self {
        match key {
            Key::Bool(b) => Val::Bool(*b),
            Key::Int(i) => Val::Int(*i),
            Key::String(s) => Val::String(s.clone()),
            Key::Table(refe) => Val::Table(*refe),
        }
    }
}

impl From<Key> for Val {
    fn from(key: Key) -> Self {
        Val::from(&key)
    }
}

/// Represents a table in the language.
///
/// If only indexed by a contiguous sequence of integers starting at 0, it is stored as an array.
/// Otherwise, it is stored as a hashmap and can also be indexed by bools, strings, and other tables.
#[derive(Debug, Clone)]
pub enum Table {
    /// Array variant for tables with contiguous integer keys starting at 0.
    Array(Vec<Val>),
    /// Flat map (sorted array) variant for small tables with keys other than contiguous integers starting at 0.
    #[allow(private_interfaces)]
    FlatMap(Vec<(Key, Val)>),
    /// Hashmap variant for big tables with keys other than contiguous integers starting at 0.
    #[allow(private_interfaces)]
    HashMap(HashMap<Key, Val>),
}

impl Default for Table {
    fn default() -> Self {
        Self::Array(Default::default())
    }
}

impl Table {
    /// Maximum number of elements in a flat map representation.
    const FLAT_MAP_LIMIT: usize = 32;

    /// Sets a value in the table at the specified key.
    /// If an entry under the given key exists, it is replaced.
    fn set(&mut self, key: Key, value: Val) {
        match self {
            Table::Array(array) => {
                if let Key::Int(key) = key {
                    let idx = key as usize;
                    if key >= 0 && idx < array.len() {
                        array[idx] = value;
                        return;
                    } else if (idx) == array.len() {
                        array.push(value);
                        return;
                    }
                }

                let elems = take(array)
                    .into_iter()
                    .enumerate()
                    .map(|(i, v)| (Key::Int(i as i64), v));
                if elems.len() >= Self::FLAT_MAP_LIMIT {
                    *self = Table::HashMap(elems.collect());
                } else {
                    *self = Table::FlatMap(elems.collect());
                }
                self.set(key, value);
            }
            Table::FlatMap(map) => {
                if let Err(idx) = map.binary_search_by_key(&&key, |(k, _)| k) {
                    map.insert(idx, (key, value))
                }
                if map.len() > Self::FLAT_MAP_LIMIT {
                    let elems = take(map).into_iter().collect();
                    *self = Table::HashMap(elems);
                }
            }
            Table::HashMap(map) => {
                map.insert(key, value);
            }
        }
    }

    /// Gets a value from the table at the specified key.
    /// If the entry does not exist, returns `None`.
    fn get(&self, key: &Key) -> Option<&Val> {
        match self {
            Table::Array(array) => {
                if let &Key::Int(key) = key {
                    let idx = key as usize;
                    if key >= 0 && idx < array.len() {
                        return Some(&array[idx]);
                    }
                }
                None
            }
            Table::FlatMap(map) => match map.binary_search_by_key(&key, |(k, _)| k) {
                Ok(idx) => {
                    let (_, val) = &map[idx];
                    Some(val)
                }
                Err(_) => None,
            },
            Table::HashMap(map) => map.get(key),
        }
    }

    /// Gets a mutable reference to a value in the table at the specified key.
    /// If the entry does not exist, returns `None`.
    fn get_mut(&mut self, key: &Key) -> Option<&mut Val> {
        match self {
            Table::Array(array) => {
                if let &Key::Int(key) = key {
                    let idx = key as usize;
                    if key >= 0 && idx < array.len() {
                        return Some(&mut array[idx]);
                    }
                }
                None
            }
            Table::FlatMap(map) => match map.binary_search_by_key(&key, |(k, _)| k) {
                Ok(idx) => Some(&mut map[idx].1),
                Err(_) => None,
            },
            Table::HashMap(map) => map.get_mut(key),
        }
    }

    /// Returns the number of elements in the table.
    pub fn len(&self) -> usize {
        match self {
            Table::Array(array) => array.len(),
            Table::FlatMap(map) => map.len(),
            Table::HashMap(map) => map.len(),
        }
    }

    /// Returns true if the table is empty.
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns an iterator over the keys and elements in the table.
    pub fn iter(&self) -> TableIter {
        match self {
            Table::Array(array) => TableIter::Array(array.iter().enumerate()),
            Table::FlatMap(map) => TableIter::FlatMap(map.iter()),
            Table::HashMap(map) => TableIter::HashMap(map.iter()),
        }
    }
}

/// An iterator over the keys and elements in a table.
pub enum TableIter<'a> {
    /// Iterates over the array variant of `Table`.
    Array(Enumerate<slice::Iter<'a, Val>>),
    /// Iterates over the flat map variant of `Table`.
    #[allow(private_interfaces)]
    FlatMap(slice::Iter<'a, (Key, Val)>),
    /// Iterates over the hashmap variant of `Table`.
    #[allow(private_interfaces)]
    HashMap(hash_map::Iter<'a, Key, Val>),
}

impl<'a> Iterator for TableIter<'a> {
    type Item = (Val, &'a Val);

    fn next(&mut self) -> Option<(Val, &'a Val)> {
        match self {
            TableIter::Array(iter) => iter.next().map(|(i, v)| (Val::Int(i as i64), v)),
            TableIter::FlatMap(iter) => iter.next().map(|(k, v)| (k.into(), v)),
            TableIter::HashMap(iter) => iter.next().map(|(k, v)| (k.into(), v)),
        }
    }
}

/// Stores all tables.
#[derive(Debug, Clone, Default)]
pub struct Heap {
    /// All tables in the heap.
    tables: Vec<Table>,
    /// Available (freed) indices in the `tables` vector.
    free: Vec<TableRef>,
}

impl Heap {
    /// Creates a new table in the heap and returns a `TableRef` pointing to it.
    fn make(&mut self) -> TableRef {
        if let Some(TableRef(refe)) = self.free.pop() {
            return TableRef(refe);
        }
        let idx = self.tables.len() as u32;
        self.tables.push(Default::default());
        TableRef(idx)
    }

    /// Drops a table from the heap.
    fn drop(&mut self, TableRef(refe): TableRef) {
        let idx = refe as usize;
        if idx == self.tables.len() - 1 {
            self.tables.pop();
        } else {
            self.tables[idx] = Default::default();
            self.free.push(TableRef(refe));
        }
    }

    /// Gets a reference to a table in the heap.
    pub fn get(&self, TableRef(refe): TableRef) -> &Table {
        let idx = refe as usize;
        &self.tables[idx]
    }

    /// Gets a mutable reference to a table in the heap.
    fn get_mut(&mut self, TableRef(refe): TableRef) -> &mut Table {
        let idx = refe as usize;
        &mut self.tables[idx]
    }
}

/// A single call frame in the stack.
#[derive(Debug, Clone)]
struct Frame {
    /// Function this frame belongs to.
    func: FuncRef,
    /// Scopes to look up variables in if not found in the current frame's scopes.
    fallback_scopes: Rc<[TableRef]>,
    /// Current instruction pointer.
    ip: BcIdx,
    /// The index of the frame in the global value stack.
    vals_idx: usize,
    /// The index of the current scope in the scope stack.
    scope_idx: usize,
}

/// A scope in the stack.
struct Scope {
    /// Table that represents this scope.
    table: TableRef,
    /// If true, this scope is captured and should not be dropped.
    escapes: bool,
}

/// Creates a new scope in the heap.
impl Scope {
    fn new(heap: &mut Heap) -> Self {
        Self {
            table: heap.make(),
            escapes: false,
        }
    }
}

/// A stack of call frames and scopes.
struct Stack {
    /// Call frames in the stack.
    frames: Vec<Frame>,
    /// Scopes that belong to the frames in the call stack.
    scopes: Vec<Scope>,
    /// Value stack for expression evaluation.
    vals: Vec<Val>,
}

impl Stack {
    /// Creates a new stack with the given function and heap.
    fn new(func: FuncRef, heap: &mut Heap) -> Self {
        Self {
            frames: vec![Frame {
                func,
                fallback_scopes: Default::default(),
                vals_idx: Default::default(),
                ip: BcIdx(0),
                scope_idx: 0,
            }],
            scopes: vec![Scope::new(heap)],
            vals: Default::default(),
        }
    }

    /// Pushes a new scope onto the stack.
    fn push_scope(&mut self, heap: &mut Heap) -> TableRef {
        let new_scope = Scope::new(heap);
        let table = new_scope.table;
        self.scopes.push(new_scope);
        table
    }

    /// Pushes a new call frame onto the stack.
    fn push_frame(
        &mut self,
        func: FuncRef,
        fallback_scopes: Rc<[TableRef]>,
        heap: &mut Heap,
        args: u32,
    ) -> EvalResult<TableRef> {
        self.frames.push(Frame {
            func,
            fallback_scopes,
            vals_idx: self.vals.len() - args as usize,
            ip: BcIdx(0),
            scope_idx: self.scopes.len(),
        });
        Ok(self.push_scope(heap))
    }

    /// Pushes a value onto the value stack.
    fn push_val(&mut self, val: Val) {
        self.vals.push(val);
    }

    /// Pops a value from the value stack.
    fn pop_val(&mut self) -> EvalResult<Val> {
        self.vals
            .pop()
            .ok_or(EvalError::InternalError("Unexpected empty value stack"))
    }

    /// Returns a reference to the top value on the value stack.
    fn top_val(&mut self) -> Option<&Val> {
        self.vals.last()
    }

    /// Gets a value from the current frame's scope or fallback scopes.
    fn get<'a>(&'a self, heap: &'a Heap, name: &str) -> EvalResult<&'a Val> {
        let frame = self.curr_frame();
        for scope in self.scopes[frame.scope_idx..]
            .iter()
            .rev()
            .map(|scope| scope.table)
            .chain(frame.fallback_scopes.iter().copied())
        {
            if let Some(val) = heap.get(scope).get(&Key::String(name.into())) {
                return Ok(val);
            }
        }
        Err(EvalError::Undefined(name.to_owned()))
    }

    /// Sets a value in the current frame's scope or fallback scopes.
    fn set(&mut self, heap: &mut Heap, field: Val, new_val: Val) -> EvalResult<()> {
        let frame = self.curr_frame();
        let key = field.try_into()?;
        // Find tightest scope with given var
        for scope in self.scopes[frame.scope_idx..]
            .iter()
            .rev()
            .map(|scope| scope.table)
            .chain(frame.fallback_scopes.iter().copied())
        {
            if let Some(val) = heap.get_mut(scope).get_mut(&key) {
                *val = new_val;
                return Ok(());
            }
        }
        // Var not declared, declare in current scope
        heap.get_mut(self.curr_scope()?).set(key, new_val);
        Ok(())
    }

    /// Pops the current scope from the stack.
    fn pop_scope(&mut self, heap: &mut Heap) -> EvalResult<()> {
        let Some(scope) = self.scopes.pop() else {
            return Err(EvalError::InternalError("No scope"));
        };
        if !scope.escapes {
            heap.drop(scope.table);
        }
        Ok(())
    }

    /// Pops the current call frame from the stack.
    fn pop_frame(&mut self, heap: &mut Heap) -> EvalResult<()> {
        if self.frames.len() == 1 {
            return Err(EvalError::InternalError("Cannot pop bottom frame"));
        };
        let frame = self.frames.pop().expect("Should have at least one frame");
        while self.scopes.len() > frame.scope_idx {
            self.pop_scope(heap)?;
        }
        Ok(())
    }

    /// Returns a reference to the current scope.
    fn curr_scope(&self) -> EvalResult<TableRef> {
        let Some(scope) = self.scopes.last() else {
            return Err(EvalError::InternalError("No scope"));
        };
        Ok(scope.table)
    }

    /// Returns a reference to the current call frame.
    fn curr_frame(&self) -> &Frame {
        self.frames.last().expect("At least one call frame")
    }

    /// Returns a mutable reference to the current call frame.
    fn curr_frame_mut(&mut self) -> &mut Frame {
        self.frames.last_mut().expect("At least one call frame")
    }

    /// Moves the instruction pointer by the specified offset.
    fn move_ip_by(&mut self, arg: i32) {
        let frame = self.curr_frame_mut();
        frame.ip += arg;
    }

    /// Moves the instruction pointer to the end of the current function.
    fn move_ip_end(&mut self, text: &Text) {
        let frame = self.curr_frame_mut();
        frame.ip = text[frame.func].next_instr_idx();
    }

    /// Returns true if the instruction pointer is at the end of the current function.
    fn ip_at_end(&self, text: &Text) -> bool {
        let frame = self.curr_frame();
        let func = &text[frame.func];
        frame.ip.0 as usize >= func.instr_count()
    }

    /// Gets the instruction at the instruction pointer.
    fn get_instr<'a, 'src>(&self, text: &'a Text<'src>) -> &'a Bc<'src> {
        let frame = self.curr_frame();
        let func = &text[frame.func];
        func.instr_at(frame.ip)
    }

    /// Gets the current instruction's source location.
    fn get_instr_loc(&self, text: &Text) -> SourceLoc {
        let frame = self.curr_frame();
        let func = &text[frame.func];
        func.instr_loc_at(frame.ip)
    }

    /// Captures the current scopes and returns their references.
    fn capture_scopes(&mut self) -> EvalResult<Rc<[TableRef]>> {
        let frame = self.frames.last_mut().expect("At least one call frame");
        Ok(self
            .scopes
            .iter_mut()
            .enumerate()
            .rev()
            .filter(|&(i, _)| i >= frame.scope_idx)
            .map(|(_, s)| {
                s.escapes = true;
                s.table
            })
            .collect())
    }

    /// Returns true if the current frame's value stack is empty.
    fn no_frame_vals(&self) -> bool {
        self.curr_frame().vals_idx >= self.vals.len()
    }

    /// Clears the current frame's values from the value stack.
    fn clear_vals(&mut self) {
        self.vals.drain(self.curr_frame().vals_idx..);
    }

    /// Pops multiple values from the value stack.
    fn pop_vals(&mut self, args: u32) -> Vec<Val> {
        self.vals.split_off(self.vals.len() - args as usize)
    }

    /// Returns true if the stack is at the bottom frame.
    fn at_bottom_frame(&self) -> bool {
        self.frames.len() == 1
    }
}

/// Bytecode executor. Ties together a `Text`, `Heap`, and `Stack`. Provides functions for bytecode evaluation.
struct Process<'src> {
    /// Bytecode to be executed.
    text: Text<'src>,
    /// Container for tables.
    heap: Heap,
    /// Call stack. Handles call frames, scopes, and value stacks.
    stack: Stack,
    /// Return value of the entire process.
    result: Val,
}

impl<'src> Process<'src> {
    /// Creates a new process for evaluating the given `Text`.
    fn new(text: Text<'src>) -> Self {
        let mut heap = Heap::default();
        let entry = FuncRef(0);
        let stack = Stack::new(entry, &mut heap);
        let top_scope = heap.get_mut(stack.curr_scope().expect("Should have scope at the start"));
        top_scope.set(
            Key::from_str("bool"),
            Val::from_fn(|args, _| {
                if let Some(Val::None) | Some(Val::Bool(false)) = args.first() {
                    Ok(Val::Bool(false))
                } else {
                    Ok(Val::Bool(true))
                }
            }),
        );
        top_scope.set(
            Key::from_str("int"),
            Val::from_fn(|args, _| {
                if let Some(&Val::Float(float)) = args.first() {
                    Ok(Val::Int(float as i64))
                } else {
                    Err(EvalError::TypeError)
                }
            }),
        );
        top_scope.set(
            Key::from_str("float"),
            Val::from_fn(|args, _| {
                if let Some(&Val::Int(int)) = args.first() {
                    Ok(Val::Float(int as f64))
                } else {
                    Err(EvalError::TypeError)
                }
            }),
        );
        top_scope.set(
            Key::from_str("str"),
            Val::from_fn(|args, _| {
                if let Some(val) = args.first() {
                    Ok(Val::String(val.to_string().into()))
                } else {
                    Err(EvalError::TypeError)
                }
            }),
        );
        top_scope.set(
            Key::from_str("len"),
            Val::from_fn(|args, heap| {
                if let Some(&Val::Table(refe)) = args.first() {
                    Ok(Val::Int(heap.get(refe).len() as i64))
                } else if let Some(Val::String(str)) = args.first() {
                    Ok(Val::Int(str.len() as i64))
                } else {
                    Err(EvalError::TypeError)
                }
            }),
        );
        top_scope.set(
            Key::from_str("keys"),
            Val::from_fn(|args, heap| {
                if let Some(&Val::Table(refe)) = args.first() {
                    let keys_ref = heap.make();
                    let keys = heap.get(refe).iter().map(|(k, _)| k).collect();
                    *heap.get_mut(keys_ref) = Table::Array(keys);
                    Ok(Val::Table(keys_ref))
                } else {
                    Err(EvalError::TypeError)
                }
            }),
        );
        top_scope.set(
            Key::from_str("print"),
            Val::from_fn(|args, heap| {
                let mut first = true;
                for val in args.iter() {
                    if !first {
                        print!(" ");
                    }
                    first = false;
                    match val {
                        Val::Table(refe) => {
                            let table_fmt = refe.format(heap);
                            print!("{table_fmt}");
                        }
                        val => print!("{val}"),
                    }
                }
                println!();
                Ok(Val::None)
            }),
        );
        Process {
            text,
            heap,
            stack,
            result: Val::None,
        }
    }

    /// Executes a single step of the process.
    fn step(&mut self) -> Result<()> {
        if !self.is_done() {
            if let Err(err) = self.eval_step() {
                return Err(err.with_loc(self.stack.get_instr_loc(&self.text)));
            }
        }
        Ok(())
    }

    /// Evaluates a single instruction.
    fn eval_step(&mut self) -> EvalResult<()> {
        let instr = self.stack.get_instr(&self.text);
        match instr {
            Bc::Imm(val) => match val {
                &Val::Func(func) => {
                    let scopes = self.stack.capture_scopes()?;
                    self.stack.push_val(Val::Closure { func, scopes });
                }
                val => self.stack.push_val(val.clone()),
            },
            Bc::Ref(name) => {
                let val = self.stack.get(&self.heap, name)?;
                self.stack.push_val(val.clone());
            }
            Bc::UnOp(op) => {
                let val = self.stack.pop_val()?;
                match op {
                    UnOp::Not => self.stack.push_val(val.not()),
                    UnOp::Neg => self.stack.push_val(val.neg()?),
                }
            }
            Bc::BinOp(op) => {
                let right = self.stack.pop_val()?;
                let left = self.stack.pop_val()?;
                self.stack.push_val(match op {
                    BinOp::Add => left.add(&right)?,
                    BinOp::Sub => left.sub(&right)?,
                    BinOp::Mul => left.mul(&right)?,
                    BinOp::Div => left.div(&right)?,
                    BinOp::Mod => left.modu(&right)?,
                    BinOp::Lt => left.lt(&right)?,
                    BinOp::Leq => left.leq(&right)?,
                    BinOp::Gt => right.lt(&left)?,
                    BinOp::Geq => right.leq(&left)?,
                    BinOp::Eq => left.eq(&right)?,
                    BinOp::Neq => left.eq(&right)?.not(),
                    BinOp::And => left.and(&right),
                    BinOp::Or => left.or(&right),
                });
            }
            Bc::Call { name, args } => {
                let func = self.stack.get(&self.heap, name)?.clone();
                if let Val::Closure { func, scopes } = func {
                    let scope = self.stack.push_frame(func, scopes, &mut self.heap, *args)?;
                    let func = &self.text[func];
                    let mut vals = self.stack.pop_vals(*args).into_iter();
                    for arg in func.iter_args() {
                        let scope = self.heap.get_mut(scope);
                        let val = if let Some(val) = vals.next() {
                            val
                        } else {
                            Val::None
                        };
                        scope.set(Key::from_str(arg), val);
                    }
                    return Ok(());
                } else if let Val::ForeignFunc(func) = func {
                    let vals = self.stack.pop_vals(*args);
                    self.stack.push_val(func(&vals, &mut self.heap)?);
                } else {
                    return Err(EvalError::TypeError);
                }
            }
            Bc::Table => {
                self.stack.push_val(Val::Table(self.heap.make()));
            }
            Bc::Store(lhs) => {
                let rhs = self.stack.pop_val()?;
                self.stack.set(&mut self.heap, Val::from_str(*lhs), rhs)?;
            }
            Bc::Set => {
                let val = self.stack.pop_val()?;
                let key = self.stack.pop_val()?.try_into()?;
                let Some(Val::Table(refe)) = self.stack.top_val() else {
                    return Err(EvalError::TypeError);
                };
                self.heap.get_mut(*refe).set(key, val);
            }
            Bc::Get => {
                let key = self.stack.pop_val()?.try_into()?;
                let Val::Table(refe) = self.stack.pop_val()? else {
                    return Err(EvalError::TypeError);
                };
                self.stack
                    .push_val(self.heap.get(refe).get(&key).cloned().unwrap_or(Val::None));
            }
            Bc::Push => {
                self.stack.push_scope(&mut self.heap);
            }
            Bc::Pop => {
                self.stack.pop_scope(&mut self.heap)?;
            }
            Bc::Branch(offset) => {
                if let Val::Bool(false) = self.stack.pop_val()? {
                    self.stack.move_ip_by(*offset);
                }
            }
            Bc::Jump(offset) => {
                self.stack.move_ip_by(*offset);
            }
            Bc::Discard => self.stack.clear_vals(),
            Bc::Ret => {
                self.stack.move_ip_end(&self.text);
                if self.stack.at_bottom_frame() {
                    self.result = self.stack.pop_val()?;
                    self.stack.clear_vals();
                }
            }
        }

        if !self.stack.at_bottom_frame() && self.stack.ip_at_end(&self.text) {
            self.stack.pop_frame(&mut self.heap)?;
            if self.stack.no_frame_vals() {
                self.stack.push_val(Val::None);
            }
        }
        self.stack.move_ip_by(1);
        Ok(())
    }

    /// Returns true if the process is done.
    fn is_done(&self) -> bool {
        self.stack.at_bottom_frame() && self.stack.ip_at_end(&self.text)
    }

    /// Retrieves the module's return value if it explicitly returned one
    fn return_value(&mut self) -> (Val, Heap) {
        (replace(&mut self.result, Val::None), take(&mut self.heap))
    }
}

#[cfg(test)]
mod tests {
    use super::{Val::*, *};

    macro_rules! eval_test {
        ($name:ident, $source:literal, Err($detail:expr), $line:literal : $col:literal : $offset:literal) => {
            #[test]
            fn $name() {
                let source = &crate::Source {
                    name: format!("<{}>", stringify!($name)),
                    contents: $source.to_owned(),
                };
                let Err(err) = eval_src(&source) else {
                    panic!("Expected error, but eval succeeded");
                };
                assert_eq!(
                    err,
                    Error {
                        loc: SourceLoc { offset: $offset, line: $line, col: $col, },
                        detail: $detail,
                    }
                );
            }
        };
        ($name:ident, $source:literal, Table[$($key:ident = $val:expr),*]) => {
            #[test]
            fn $name() {
                let source = crate::Source {
                    name: format!("<{}>", stringify!($name)),
                    contents: $source.to_owned(),
                };
                let result = eval_src(&source).expect("Eval should succeed");
                let (Val::Table(refe), heap) = result else {
                    panic!("Expected return value to be a table");
                };
                let table = heap.get(refe);
                let expected = [$((stringify!($key), $val)),*];
                for (key, val) in expected.iter() {
                    assert_eq!(table.get(&Key::String(Rc::from(*key))), Some(val))
                }
                assert_eq!(table.len(), expected.len());
            }
        };
        ($name:ident, $source:literal, Table[$($exp:expr),*]) => {
            #[test]
            fn $name() {
                let source = crate::Source {
                    name: format!("<{}>", stringify!($name)),
                    contents: $source.to_owned(),
                };
                let result = eval_src(&source).expect("Eval should succeed");
                let (Val::Table(refe), heap) = result else {
                    panic!("Expected return value to be a table");
                };
                let table = heap.get(refe);
                let expected = [$($exp),*];
                for (i, val) in expected.iter().enumerate() {
                    assert_eq!(table.get(&Key::Int(i as i64)), Some(val))
                }
                assert_eq!(table.len(), expected.len());
            }
        };
        ($name:ident, $source:literal, $exp:expr) => {
            #[test]
            fn $name() {
                let source = crate::Source {
                    name: format!("<{}>", stringify!($name)),
                    contents: $source.to_owned(),
                };
                let (result, _) = eval_src(&source).expect("Eval should succeed");
                assert_eq!(result, $exp);
            }
        };
    }

    eval_test!(return_value, "return 1", Int(1));
    eval_test!(sum, "return 1 + 2", Int(3));
    eval_test!(sum_float, "return 1.0 + 2.0", Float(3.0));
    eval_test!(sum_float_int, "return 1 + 2.0", Float(3.0));
    eval_test!(sum_int_float, "return 1.0 + 2", Float(3.0));
    eval_test!(
        concat_string,
        r#"return "foo" + "bar""#,
        String("foobar".into())
    );
    eval_test!(sub_int, "return 5 - 2", Int(3));
    eval_test!(sub_float, "return 5.0 - 2.0", Float(3.0));
    eval_test!(sub_float_int, "return 5 - 2.0", Float(3.0));
    eval_test!(sub_int_float, "return 5.0 - 2", Float(3.0));
    eval_test!(mul_int, "return 5 * 2", Int(10));
    eval_test!(mul_float, "return 5.0 * 2.0", Float(10.0));
    eval_test!(mul_float_int, "return 5 * 2.0", Float(10.0));
    eval_test!(mul_int_float, "return 5.0 * 2", Float(10.0));
    eval_test!(div_int, "return 5 / 2", Int(2));
    eval_test!(div_float, "return 5.0 / 2.0", Float(2.5));
    eval_test!(div_float_int, "return 5 / 2.0", Float(2.5));
    eval_test!(div_int_float, "return 5.0 / 2", Float(2.5));
    eval_test!(mod_int, "return 5 % 2", Int(1));
    eval_test!(and1, "return true && false", Bool(false));
    eval_test!(and2, "return true && true", Bool(true));
    eval_test!(and3, "return true && []", Bool(true));
    eval_test!(and4, "return true && 0", Bool(true));
    eval_test!(or1, "return true || false", Bool(true));
    eval_test!(or2, "return false || false", Bool(false));
    eval_test!(or3, "return false || []", Bool(true));
    eval_test!(or4, "return false || 0", Bool(true));
    eval_test!(not1, "return !true", Bool(false));
    eval_test!(not2, "return !false", Bool(true));
    eval_test!(not3, "return ![]", Bool(false));
    eval_test!(not4, "return !0", Bool(false));
    eval_test!(lt_int_1, "return 1 < 2", Bool(true));
    eval_test!(lt_int_2, "return 1 < -1", Bool(false));
    eval_test!(lt_int_float, "return 1 < 1.0", Bool(false));
    eval_test!(leq_int_1, "return 1 <= 1", Bool(true));
    eval_test!(leq_int_2, "return 1 <= -1", Bool(false));
    eval_test!(leq_int_float, "return 1 <= 1.0", Bool(true));
    eval_test!(gt_int_1, "return 0 > 0", Bool(false));
    eval_test!(gt_int_2, "return 0 > -99999", Bool(true));
    eval_test!(gt_int_float, "return 0 > 0.0", Bool(false));
    eval_test!(geq_int_1, "return 0 >= 0", Bool(true));
    eval_test!(geq_int_2, "return -1 >= 0", Bool(false));
    eval_test!(geq_int_float, "return 0 >= 0.0", Bool(true));
    eval_test!(eq_int_1, "return 0 == 1", Bool(false));
    eval_test!(eq_int_2, "return 1 == 1", Bool(true));
    eval_test!(eq_int_float, "return 1 == 1.0", Bool(true));
    eval_test!(eq_float, "return 1.0 == 1.0", Bool(true));
    eval_test!(eq_string_1, r#"return "foo" == "bar""#, Bool(false));
    eval_test!(eq_string_2, r#"return "qux" == "qux""#, Bool(true));
    eval_test!(eq_table_1, "return [] == []", Bool(false));
    eval_test!(eq_table_2, "t = []\nreturn t == t", Bool(true));
    eval_test!(precedence_1, "return 2 + 2 * 2", Int(6));
    eval_test!(precedence_2, "return 3 * 3 + 3 < 3 * 5", Bool(true));
    eval_test!(precedence_3, "return 2 > 1 && 1 < 2", Bool(true));
    eval_test!(table_array, "return [1, 2, 3]", Table[Int(1), Int(2), Int(3)]);
    eval_test!(table_map, "return [x = 4, y = 2]", Table[x = Int(4), y = Int(2)]);
    eval_test!(assign, "x = 1\nreturn x", Int(1));
    eval_test!(table_set, "t = [1]\nt[0] = 2\nreturn t", Table[Int(2)]);
    eval_test!(
        table_get_1,
        "t = [x = 4, y = 7, z = 11]\nreturn t.y",
        Int(7)
    );
    eval_test!(table_get_2, "t = [x = 4, y = 7, z = 11]\nreturn t.w", None);
    eval_test!(if_1, "if true { return true }", Bool(true));
    eval_test!(if_2, "if false { return false }", None);
    eval_test!(
        if_else_1,
        "if true { return true } else { return false }",
        Bool(true)
    );
    eval_test!(
        if_else_2,
        "if false { return true } else { return false }",
        Bool(false)
    );
    eval_test!(
        loop_while,
        "x = 2\nwhile x < 1024 {\nx = x * x}\nreturn x",
        Int(65536)
    );
    eval_test!(
        loop_for,
        "x = 2\nfor i in 0..4 {\nx = x * x}\nreturn x",
        Int(65536)
    );
    eval_test!(call, "fn f() { return 42 }\nreturn f()", Int(42));
    eval_test!(len_empty, "return len([])", Int(0));
    eval_test!(len_array, "return len([1, 2, 3])", Int(3));
    eval_test!(len_map, "return len([x = 2, y = 4])", Int(2));
    eval_test!(len_string, r#"return len("foo")"#, Int(3));
    eval_test!(keys, "return keys([1, 2, 3])", Table[Int(0), Int(1), Int(2)]);
    eval_test!(convert_bool_1, "return bool(0)", Bool(true));
    eval_test!(convert_bool_2, "return bool([])", Bool(true));
    eval_test!(convert_int_1, "return int(1.0)", Int(1));
    eval_test!(convert_float, "return float(1)", Float(1.0));
    eval_test!(convert_string, "return str(42)", String("42".into()));
    eval_test!(error_1, "print(x)", Err(ErrorDetail::Undefined("x".into())), 0:6:6);
    eval_test!(error_2, "\nreturn\n1 + true", Err(ErrorDetail::TypeError), 2:2:10);
    eval_test!(error_3, "return int([])", Err(ErrorDetail::TypeError), 0:10:10);
}
