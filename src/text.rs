use crate::FuncRef;

use super::{BinOp, SourceLoc, UnOp, Val};
use std::{collections::HashMap, fmt, ops};

/// Bytecode instruction.
#[derive(Debug, Clone, PartialEq)]
pub(super) enum Bc<'src> {
    Imm(Val),
    Ref(&'src str),
    UnOp(UnOp),
    BinOp(BinOp),
    Call { name: &'src str, args: u32 },
    Table,
    Store(&'src str),
    Set,
    Get,
    Push,
    Pop,
    Discard,
    Jump(i32),
    Branch(i32),
    Ret,
}

impl fmt::Display for Bc<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Bc::Imm(Val::Float(float)) => write!(f, "val {float:?}"),
            Bc::Imm(val) => write!(f, "val {val}"),
            Bc::Ref(refe) => write!(f, "ref {refe}"),
            Bc::BinOp(op) => write!(f, "{op}"),
            Bc::UnOp(op) => write!(f, "{op}"),
            Bc::Call { name: refe, args } => write!(f, "call {refe} {args}"),
            Bc::Table => write!(f, "table"),
            Bc::Store(refe) => write!(f, "store {refe}"),
            Bc::Set => write!(f, "set"),
            Bc::Get => write!(f, "get"),
            Bc::Push => write!(f, "push"),
            Bc::Pop => write!(f, "pop"),
            Bc::Discard => write!(f, "discard"),
            Bc::Jump(offset) => write!(f, "jump {offset}"),
            Bc::Branch(offset) => write!(f, "branch {offset}"),
            Bc::Ret => write!(f, "ret"),
        }
    }
}

/// Interpretable function with a name, arguments, a block of bytecode, and corresponding source locations.
#[derive(Debug, Clone)]
pub(super) struct Func<'src> {
    /// Name of the function.
    name: String,
    /// Ordered argument names.
    args: Vec<&'src str>,
    /// Bytecode instructions of the function.
    instrs: Vec<Bc<'src>>,
    /// Source locations of the instructions.
    locs: Vec<SourceLoc<'src>>,
}

/// Index into the bytecode within a function.
#[derive(Debug, Clone, Copy)]
pub(super) struct BcIdx(pub(super) u32);

impl<'src> Func<'src> {
    /// Returns the index of the next instruction that would be appended.
    pub(super) fn next_instr_idx(&self) -> BcIdx {
        BcIdx(self.instrs.len() as u32)
    }

    /// Appends an instruction to the function.
    pub(super) fn append_instr(&mut self, instr: Bc<'src>, loc: SourceLoc<'src>) {
        self.instrs.push(instr);
        self.locs.push(loc);
    }

    /// Returns a reference to the instruction at the specified index.
    pub(super) fn instr_at(&self, BcIdx(idx): BcIdx) -> &Bc<'src> {
        &self.instrs[idx as usize]
    }

    /// Returns a reference to the last instruction, if any.
    pub(super) fn instr_last(&self) -> Option<&Bc<'src>> {
        self.instrs.last()
    }

    /// Returns a mutable reference to the instruction at the specified index.
    pub(super) fn instr_at_mut(&mut self, BcIdx(idx): BcIdx) -> &mut Bc<'src> {
        &mut self.instrs[idx as usize]
    }

    /// Returns the source location of the instruction at the specified index.
    pub(super) fn instr_loc_at(&self, BcIdx(idx): BcIdx) -> SourceLoc<'src> {
        self.locs[idx as usize]
    }

    /// Returns the source location of the last instruction, if any.
    pub(super) fn instr_loc_last(&self) -> Option<SourceLoc<'src>> {
        self.locs.last().copied()
    }

    /// Returns an iterator over the function's arguments.
    pub(super) fn iter_args(&self) -> impl DoubleEndedIterator<Item = &str> {
        self.args.iter().copied()
    }

    /// Returns an iterator over the function's instructions.
    pub(super) fn iter_instrs(&self) -> impl Iterator<Item = &Bc<'src>> {
        self.instrs.iter()
    }

    /// Returns the number of instructions in the function.
    pub(super) fn instr_count(&self) -> usize {
        self.instrs.len()
    }
}

impl ops::Sub for BcIdx {
    type Output = i32;

    fn sub(self, BcIdx(other): Self) -> Self::Output {
        let BcIdx(this) = self;
        this as i32 - other as i32
    }
}

impl ops::AddAssign<i32> for BcIdx {
    fn add_assign(&mut self, offset: i32) {
        let BcIdx(this) = self;
        *this = (*this as i32 + offset) as u32
    }
}

/// A collection of functions and their bytecode.
#[derive(Debug, Clone, Default)]
pub(super) struct Text<'src> {
    /// All functions in the `Text`.
    funcs: Vec<Func<'src>>,
    /// Map from function names to their references.
    func_refs: HashMap<String, FuncRef>,
}

impl<'src> Text<'src> {
    /// Adds the given function to the `Text`.
    pub(super) fn add_func(&mut self, name: String, args: Vec<&'src str>) -> FuncRef {
        let idx = self.funcs.len();
        let refe = FuncRef(idx as u32);
        self.func_refs.insert(name.clone(), refe);
        self.funcs.push(Func {
            name,
            args,
            instrs: Default::default(),
            locs: Default::default(),
        });
        refe
    }

    /// Gets a reference to the function with the given name.
    #[cfg(test)]
    pub(super) fn func_ref<S: AsRef<str>>(&self, func_name: S) -> Option<FuncRef> {
        self.func_refs.get(func_name.as_ref()).copied()
    }

    /// Returns an iterator over the functions.
    fn iter(&self) -> impl Iterator<Item = &Func> {
        self.funcs.iter()
    }
}

impl<'src> ops::Index<FuncRef> for Text<'src> {
    type Output = Func<'src>;

    fn index(&self, FuncRef(refe): FuncRef) -> &Self::Output {
        &self.funcs[refe as usize]
    }
}

impl ops::IndexMut<FuncRef> for Text<'_> {
    fn index_mut(&mut self, FuncRef(refe): FuncRef) -> &mut Self::Output {
        &mut self.funcs[refe as usize]
    }
}

impl fmt::Display for Text<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for func in self.iter() {
            write!(f, "fn {name} (", name = func.name)?;
            let mut first = true;
            for arg in func.iter_args() {
                if !first {
                    write!(f, ", ")?;
                }
                first = false;
                write!(f, "{arg}")?;
            }
            writeln!(f, "):")?;
            let mut indent = 4;
            for instr in func.iter_instrs() {
                match &instr {
                    Bc::Imm(Val::String(string)) => writeln!(f, "{:indent$}| {string:?}", "")?,
                    instr @ Bc::Imm(_) | instr @ Bc::Ref(_) => {
                        writeln!(f, "{:indent$}| {instr}", "")?
                    }
                    instr @ Bc::Push => {
                        writeln!(f, "{:indent$}{instr}", "")?;
                        indent += 4;
                    }
                    instr @ Bc::Pop => {
                        indent -= 4;
                        writeln!(f, "{:indent$}{instr}", "")?;
                    }
                    instr => writeln!(f, "{:indent$}{instr}", "")?,
                }
            }
            writeln!(f)?;
        }
        Ok(())
    }
}
