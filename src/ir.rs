use std::collections::HashMap;
use std::fmt::Display;

use super::parser::Ast;

pub enum IrInstruction {
    /// Calls the first argument by passing along the rest of the arguments into it.
    Call,

    /// Loads its argument into the provided local.
    Load,

    /// Sets a local value's register to the provided value.
    Set,

    /// Creates a literal
    Literal,

    /// Returns from a function with the given value. This instruction can only end a basic block.
    Ret,

    /// Captures the second and onwards arguments by reference for the first argument.
    Capture,

    /// Increments the reference counter.
    RcInc,

    /// Decrements the reference counter.
    RcDec,

    /// Jumps to a different basic block. This instruction can only end a basic block.
    Jump,

    /// branch cmp, block0, block1
    /// Branches to block0 if cmp is true and to block 1 otherwise. This instruction can only end
    /// a basic block.
    Branch,

    /// phi block0, local0, block1, local1, ...
    /// Chooses between two locals based on which basic block was the last one. If block0 was the
    /// last block, then local0 is returned; otherwise, if block1 is the last block, local1 is
    /// returned. There can be as many block-local pairs as necessary.
    Phi,
}

impl Display for IrInstruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IrInstruction::Call => write!(f, "call"),
            IrInstruction::Load => write!(f, "load"),
            IrInstruction::Set => write!(f, "set"),
            IrInstruction::Literal => write!(f, "literal"),
            IrInstruction::Ret => write!(f, "ret"),
            IrInstruction::Capture => write!(f, "capture"),
            IrInstruction::RcInc => write!(f, "rcinc"),
            IrInstruction::RcDec => write!(f, "rcdec"),
            IrInstruction::Jump => write!(f, "jump"),
            IrInstruction::Branch => write!(f, "branch"),
            IrInstruction::Phi => write!(f, "phi"),
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum IrArgument {
    Float(f64),
    Int(i32),
    Local(usize),
    Argument(usize),
    Atom(String),
    Function(String),
    BasicBlock(usize),
    Closed(usize),
}

impl Display for IrArgument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            IrArgument::Float(float) => write!(f, "{}", float),
            IrArgument::Int(int) => write!(f, "{}", int),
            IrArgument::Local(local) => write!(f, "%{}", local),
            IrArgument::Argument(arg) => write!(f, "${}", arg),
            IrArgument::Atom(atom) => write!(f, "#{}", atom),
            IrArgument::Function(func) => write!(f, "@{}", func),
            IrArgument::BasicBlock(block) => write!(f, "&{}", block),
            IrArgument::Closed(cap) => write!(f, "[{}]", cap),
        }
    }
}

pub struct IrSsa {
    pub local: Option<usize>,
    pub local_lifetime: usize,
    pub local_register: usize,
    pub global: bool,
    pub instr: IrInstruction,
    pub args: Vec<IrArgument>,
}

impl Display for IrSsa {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(l) = self.local {
            write!(
                f,
                "%{}[{}{}] = ",
                l,
                self.local_lifetime,
                if self.global { "+" } else { "" }
            )?;
        }

        write!(f, "{}", self.instr)?;
        for a in self.args.iter() {
            write!(f, " {}", a)?;
        }
        Ok(())
    }
}

pub struct IrBasicBlock {
    pub id: usize,
    pub ssas: Vec<IrSsa>,
    pub terminator: IrSsa,
}

impl Display for IrBasicBlock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "  {}:", self.id)?;
        for ssa in self.ssas.iter() {
            writeln!(f, "    {}", ssa)?;
        }
        write!(f, "    {}", self.terminator)
    }
}

pub struct IrFunction {
    pub name: String,
    pub external: bool,
    pub argc: usize,
    pub captured: Vec<String>,
    pub blocks: Vec<IrBasicBlock>,
    pub last_local: usize,
}

impl Display for IrFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.external {
            writeln!(f, "extern {}\n\n", self.name)
        } else {
            writeln!(f, "{}({}; {:?}):", self.name, self.argc, self.captured)?;
            for block in self.blocks.iter() {
                writeln!(f, "{}\n", block)?;
            }
            writeln!(f)
        }
    }
}

pub struct IrModule {
    pub funcs: Vec<IrFunction>,
}

impl Display for IrModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for func in self.funcs.iter() {
            write!(f, "{}", func)?;
        }
        Ok(())
    }
}

#[derive(Debug)]
pub enum IrError {
    UnknownSymbol,
    FuncNameNotFound,
    FuncArgsNotFound,
    FuncBodyNotFound,
    CallerNotFound,
    NonSymbolInAssignment,
    AssignmentWithoutValue,
    NonSExprInCond,
    NoConditionInCond,
    NoBodyForCond,
}

type IrScope = Vec<HashMap<String, IrArgument>>;

mod scope {
    use super::*;

    pub fn get(scope: &[HashMap<String, IrArgument>], var: &str) -> Option<(IrArgument, bool)> {
        for (i, scope) in scope.iter().rev().enumerate() {
            if let Some(v) = scope.get(var) {
                return Some((v.clone(), i != 0));
            }
        }

        None
    }
}

fn ast_to_ir_helper<'a>(
    ast: Ast<'a>,
    scope: &mut IrScope,
    module: &mut IrModule,
    func: &mut IrFunction,
    block: &mut IrBasicBlock,
) -> Result<Option<usize>, IrError> {
    match ast {
        Ast::Float(float) => {
            let local = Some(func.last_local);
            func.last_local += 1;

            block.ssas.push(IrSsa {
                local,
                local_lifetime: 0,
                local_register: 0,
                global: false,
                instr: IrInstruction::Literal,
                args: vec![IrArgument::Float(float)],
            });

            Ok(local)
        }

        Ast::Int(int) => {
            let local = Some(func.last_local);
            func.last_local += 1;

            block.ssas.push(IrSsa {
                local,
                local_lifetime: 0,
                local_register: 0,
                global: false,
                instr: IrInstruction::Literal,
                args: vec![IrArgument::Int(int)],
            });

            Ok(local)
        }

        Ast::Symbol(sym) => {
            let (mut value, closed) = match scope::get(scope, sym) {
                Some(v) => v,
                None => return Err(IrError::UnknownSymbol),
            };
            if closed && !matches!(value, IrArgument::Atom(_) | IrArgument::Function(_)) {
                value = IrArgument::Closed(func.captured.len());
                scope
                    .last_mut()
                    .unwrap()
                    .insert(String::from(sym), value.clone());
                func.captured.push(String::from(sym));
            }

            let local = Some(func.last_local);
            func.last_local += 1;

            block.ssas.push(IrSsa {
                local,
                local_lifetime: 0,
                local_register: 0,
                global: false,
                instr: IrInstruction::Load,
                args: vec![value],
            });

            Ok(local)
        }

        Ast::SExpr(f, args) => {
            if args.is_empty() {
                return ast_to_ir_helper(*f, scope, module, func, block);
            }

            match *f {
                Ast::Symbol(v) if scope::get(scope, v).is_some() => {
                    let (mut value, closed) = scope::get(scope, v).unwrap();
                    if closed && !matches!(value, IrArgument::Atom(_) | IrArgument::Function(_)) {
                        value = IrArgument::Closed(func.captured.len());
                        scope
                            .last_mut()
                            .unwrap()
                            .insert(String::from(v), value.clone());
                        func.captured.push(String::from(v));
                    }

                    let mut locals = vec![value];
                    for arg in args {
                        if let Some(v) = ast_to_ir_helper(arg, scope, module, func, block)? {
                            locals.push(IrArgument::Local(v));
                        }
                    }

                    let local = Some(func.last_local);
                    func.last_local += 1;

                    block.ssas.push(IrSsa {
                        local,
                        local_lifetime: 0,
                        local_register: 0,
                        global: false,
                        instr: IrInstruction::Call,
                        args: locals,
                    });

                    Ok(local)
                }

                Ast::Symbol("atom") => {
                    let scope = scope.last_mut().unwrap();
                    for arg in args {
                        match arg {
                            Ast::Symbol("true") => {
                                scope.insert(String::from("true"), IrArgument::Int(1));
                            }
                            Ast::Symbol("false") => {
                                scope.insert(String::from("false"), IrArgument::Int(0));
                            }
                            Ast::Symbol(atom) => {
                                scope.insert(String::from(atom), IrArgument::Atom(String::from(atom)));
                            }
                            _ => (),
                        }
                    }

                    Ok(None)
                }

                Ast::Symbol("seq") => {
                    let mut last = None;
                    for arg in args.into_iter() {
                        if let Some(v) = ast_to_ir_helper(arg, scope, module, func, block)? {
                            last = Some(v);
                        }
                    }
                    Ok(last)
                }

                Ast::Symbol("extern") => {
                    for name in args {
                        match name {
                            Ast::Symbol(v) => {
                                let func = IrFunction {
                                    name: String::from(v),
                                    external: true,
                                    argc: 0,
                                    captured: vec![],
                                    blocks: vec![],
                                    last_local: 0,
                                };
                                module.funcs.push(func);
                                scope.last_mut().unwrap().insert(String::from(v), IrArgument::Function(String::from(v)));
                            }

                            _ => return Err(IrError::FuncNameNotFound)
                        }
                    }

                    Ok(None)
                }

                Ast::Symbol("func") => {
                    let mut args = args.into_iter();
                    let name = match args.next() {
                        Some(Ast::Symbol(v)) => v,
                        _ => return Err(IrError::FuncNameNotFound),
                    };

                    scope
                        .last_mut()
                        .unwrap()
                        .insert(String::from(name), IrArgument::Function(String::from(name)));

                    scope.push(HashMap::new());
                    let formals: Vec<_> = match args.next() {
                        Some(Ast::SExpr(head, tail)) => vec![*head]
                            .into_iter()
                            .chain(tail.into_iter())
                            .filter_map(|v| match v {
                                Ast::Symbol(v) => Some(v),
                                _ => None,
                            })
                            .collect(),

                        _ => return Err(IrError::FuncArgsNotFound),
                    };

                    let last_scope = scope.last_mut().unwrap();
                    for (i, formal) in formals.iter().enumerate() {
                        last_scope.insert(String::from(*formal), IrArgument::Argument(i));
                    }

                    let mut f = IrFunction {
                        name: String::from(name),
                        external: false,
                        argc: formals.len(),
                        captured: vec![],
                        blocks: vec![],
                        last_local: 0,
                    };
                    let mut b = IrBasicBlock {
                        id: 0,
                        ssas: vec![],
                        terminator: IrSsa {
                            local: None,
                            local_lifetime: 0,
                            local_register: 0,
                            global: false,
                            instr: IrInstruction::Ret,
                            args: vec![],
                        },
                    };

                    let ret = ast_to_ir_helper(
                        match args.next() {
                            Some(v) => v,
                            None => return Err(IrError::FuncBodyNotFound),
                        },
                        scope,
                        module,
                        &mut f,
                        &mut b,
                    )?;
                    if let Some(ret) = ret {
                        b.terminator.args.push(IrArgument::Local(ret));
                    }

                    scope.pop();

                    let local = Some(func.last_local);
                    func.last_local += 1;

                    if !f.captured.is_empty() {
                        let mut args = vec![IrArgument::Function(String::from(name))];
                        for captured in f.captured.iter() {
                            let (mut value, closed) = scope::get(scope, captured).unwrap();
                            if closed
                                && !matches!(value, IrArgument::Atom(_) | IrArgument::Function(_))
                            {
                                value = IrArgument::Closed(func.captured.len());
                                scope
                                    .last_mut()
                                    .unwrap()
                                    .insert(captured.clone(), value.clone());
                                func.captured.push(captured.clone());
                            }

                            args.push(value);
                        }

                        block.ssas.push(IrSsa {
                            local,
                            local_lifetime: 0,
                            local_register: 0,
                            global: false,
                            instr: IrInstruction::Capture,
                            args,
                        });
                    } else {
                        block.ssas.push(IrSsa {
                            local,
                            local_lifetime: 0,
                            local_register: 0,
                            global: false,
                            instr: IrInstruction::Load,
                            args: vec![IrArgument::Function(String::from(name))],
                        });
                    }

                    f.blocks.push(b);
                    module.funcs.push(f);
                    Ok(local)
                }

                Ast::Symbol("set") => {
                    let mut args = args.into_iter();
                    let name = match args.next() {
                        Some(Ast::Symbol(v)) => v,
                        _ => return Err(IrError::NonSymbolInAssignment),
                    };

                    let value = match args.next() {
                        Some(v) => ast_to_ir_helper(v, scope, module, func, block)?,
                        None => return Err(IrError::AssignmentWithoutValue),
                    };
                    if value.is_none() {
                        return Err(IrError::AssignmentWithoutValue);
                    }
                    let mut value = value.unwrap();

                    let scope = scope.last_mut().unwrap();
                    if scope.get(name).is_none() {
                        scope.insert(String::from(name), IrArgument::Local(value));
                    } else {
                        let local = func.last_local;
                        func.last_local += 1;

                        block.ssas.push(IrSsa {
                            local: Some(local),
                            local_lifetime: 0,
                            local_register: 0,
                            global: false,
                            instr: IrInstruction::Set,
                            args: vec![scope.get(name).unwrap().clone(), IrArgument::Local(value)],
                        });
                        value = local;
                    }

                    Ok(Some(value))
                }

                Ast::Symbol("cond") => {
                    let mut basic_block_pairs = vec![];
                    for arg in args {
                        match arg {
                            Ast::SExpr(cond, value) => {
                                if let Ast::Symbol("else") = *cond {
                                    let value = ast_to_ir_helper(
                                        match value.into_iter().next() {
                                            Some(v) => v,
                                            None => return Err(IrError::NoBodyForCond),
                                        },
                                        scope,
                                        module,
                                        func,
                                        block,
                                    )?;

                                    basic_block_pairs.push((func.blocks.len(), value));

                                    let mut temp = IrBasicBlock {
                                        id: func.blocks.len() + 1,
                                        ssas: vec![],
                                        terminator: IrSsa {
                                            local: None,
                                            local_lifetime: 0,
                                            local_register: 0,
                                            global: false,
                                            instr: IrInstruction::Ret,
                                            args: vec![],
                                        },
                                    };
                                    block.terminator.instr = IrInstruction::Jump;
                                    std::mem::swap(&mut temp, block);
                                    func.blocks.push(temp);
                                    break;
                                } else {
                                    let cond = match ast_to_ir_helper(
                                        *cond, scope, module, func, block,
                                    )? {
                                        Some(v) => v,
                                        None => return Err(IrError::NoConditionInCond),
                                    };

                                    let block_id = block.id;
                                    block.terminator = IrSsa {
                                        local: None,
                                        local_lifetime: 0,
                                        local_register: 0,
                                        global: false,
                                        instr: IrInstruction::Branch,
                                        args: vec![
                                            IrArgument::Local(cond),
                                            IrArgument::BasicBlock(func.blocks.len() + 1),
                                        ],
                                    };
                                    let mut temp = IrBasicBlock {
                                        id: func.blocks.len() + 1,
                                        ssas: vec![],
                                        terminator: IrSsa {
                                            local: None,
                                            local_lifetime: 0,
                                            local_register: 0,
                                            global: false,
                                            instr: IrInstruction::Ret,
                                            args: vec![],
                                        },
                                    };
                                    std::mem::swap(&mut temp, block);

                                    func.blocks.push(temp);
                                    let value = ast_to_ir_helper(
                                        match value.into_iter().next() {
                                            Some(v) => v,
                                            None => return Err(IrError::NoBodyForCond),
                                        },
                                        scope,
                                        module,
                                        func,
                                        block,
                                    )?;
                                    let len = func.blocks.len();
                                    func.blocks[block_id]
                                        .terminator
                                        .args
                                        .push(IrArgument::BasicBlock(len + 1));
                                    basic_block_pairs.push((len, value));

                                    block.terminator.instr = IrInstruction::Jump;
                                    let mut temp = IrBasicBlock {
                                        id: func.blocks.len() + 1,
                                        ssas: vec![],
                                        terminator: IrSsa {
                                            local: None,
                                            local_lifetime: 0,
                                            local_register: 0,
                                            global: false,
                                            instr: IrInstruction::Ret,
                                            args: vec![],
                                        },
                                    };
                                    std::mem::swap(&mut temp, block);
                                    func.blocks.push(temp);
                                }
                            }

                            _ => return Err(IrError::NonSExprInCond),
                        }
                    }

                    let local = Some(func.last_local);
                    func.last_local += 1;

                    let mut phi = IrSsa {
                        local,
                        local_lifetime: 0,
                        local_register: 0,
                        global: false,
                        instr: IrInstruction::Phi,
                        args: vec![],
                    };
                    let block_id = func.blocks.len();
                    for (block, value) in basic_block_pairs {
                        func.blocks[block]
                            .terminator
                            .args
                            .push(IrArgument::BasicBlock(block_id));

                        phi.args.push(IrArgument::BasicBlock(block));
                        if let Some(value) = value {
                            phi.args.push(IrArgument::Local(value));
                        }
                    }
                    block.ssas.push(phi);

                    Ok(local)
                }

                Ast::Symbol(_) => Err(IrError::UnknownSymbol),

                _ => {
                    let f = match ast_to_ir_helper(*f, scope, module, func, block)? {
                        Some(f) => IrArgument::Local(f),
                        None => return Err(IrError::CallerNotFound),
                    };

                    let mut locals = vec![f];
                    for arg in args {
                        if let Some(v) = ast_to_ir_helper(arg, scope, module, func, block)? {
                            locals.push(IrArgument::Local(v));
                        }
                    }

                    let local = Some(func.last_local);
                    func.last_local += 1;

                    block.ssas.push(IrSsa {
                        local,
                        local_lifetime: 0,
                        local_register: 0,
                        global: false,
                        instr: IrInstruction::Call,
                        args: locals,
                    });

                    Ok(local)
                }
            }
        }

        Ast::List(elements) => {
            let mut args = vec![IrArgument::Function(String::from("list")), IrArgument::Int(0)];

            let mut contains = false;
            for func in module.funcs.iter() {
                if func.name == "list" {
                    contains = true;
                    break;
                }
            }
            if !contains {
                module.funcs.push(IrFunction {
                    name: String::from("list"),
                    external: true,
                    argc: 0,
                    captured: vec![],
                    blocks: vec![],
                    last_local: 0,
                });
            }

            for e in elements {
                if let Some(v) = ast_to_ir_helper(e, scope, module, func, block)? {
                    args.push(IrArgument::Local(v));
                }
            }

            let local = Some(func.last_local);
            func.last_local += 1;

            args[1] = IrArgument::Int(args.len() as i32 - 2);

            block.ssas.push(IrSsa {
                local,
                local_lifetime: 0,
                local_register: 0,
                global: false,
                instr: IrInstruction::Call,
                args,
            });

            Ok(local)
        }
    }
}

fn is_not_not_tail_recursive(func: &IrFunction, block: &IrBasicBlock) -> bool {
    let name = IrArgument::Function(func.name.clone());
    let mut block_iter = func.blocks.iter();
    while let Some(block) = block_iter.next() {
        let mut ssa_iter = block.ssas.iter();
        while let Some(ssa) = ssa_iter.next() {
            if matches!(ssa.instr, IrInstruction::Call) && *ssa.args.first().unwrap() == name {
                for arg in ssa.args.iter().skip(1) {
                    if *arg == name {
                        return false;
                    }
                }

                if ssa.local.is_none() {
                    return false;
                }

                let mut local = IrArgument::Local(ssa.local.unwrap());
                let mut i = ssa.local_lifetime - 1;
                for ssa in ssa_iter.as_slice() {
                    for arg in ssa.args.iter() {
                        if i == 0 {
                            if ssa.local.is_none() || !matches!(ssa.instr, IrInstruction::Phi) {
                                return false;
                            }
                            local = IrArgument::Local(ssa.local.unwrap());
                            i = ssa.local_lifetime;
                        }
                        if *arg == local {
                            return false;
                        }
                        i -= 1;
                    }
                }

                if block.terminator.args[0] == local {
                    return i == 0 && matches!(block.terminator.instr, IrInstruction::Ret);
                }

                for block in block_iter.as_slice() {
                    for ssa in block.ssas.iter() {
                        for arg in ssa.args.iter() {
                            if i == 0 {
                                if ssa.local.is_none() || !matches!(ssa.instr, IrInstruction::Phi) {
                                    return false;
                                }
                                local = IrArgument::Local(ssa.local.unwrap());
                                i = ssa.local_lifetime;
                            }
                            if *arg == local {
                                return false;
                            }
                            i -= 1;
                        }
                    }

                    if block.terminator.args[0] == local {
                        return i == 0 && matches!(block.terminator.instr, IrInstruction::Ret);
                    }
                }
            } else {
                for arg in ssa.args.iter() {
                    if *arg == name {
                        return false;
                    }
                }
            }
        }
    }

    match block.terminator.instr {
        IrInstruction::Ret => true,
        IrInstruction::Jump => is_not_not_tail_recursive(
            func,
            &func.blocks[if let IrArgument::BasicBlock(b) = block.terminator.args[0] {
                b
            } else {
                unreachable!()
            }],
        ),
        IrInstruction::Branch => {
            is_not_not_tail_recursive(
                func,
                &func.blocks[if let IrArgument::BasicBlock(b) = block.terminator.args[1] {
                    b
                } else {
                    unreachable!()
                }],
            ) && is_not_not_tail_recursive(
                func,
                &func.blocks[if let IrArgument::BasicBlock(b) = block.terminator.args[2] {
                    b
                } else {
                    unreachable!()
                }],
            )
        }
        _ => unreachable!("invalid instruction for terminator"),
    }
}

fn self_references(func: &IrFunction) -> bool {
    let name = IrArgument::Function(func.name.clone());
    for block in func.blocks.iter() {
        for ssa in block.ssas.iter() {
            if matches!(ssa.instr, IrInstruction::Call) && *ssa.args.first().unwrap() == name {
                return true;
            }
        }
    }

    false
}

fn is_tail_recursive(func: &IrFunction) -> bool {
    self_references(func) && is_not_not_tail_recursive(func, &func.blocks[0])
}

fn remove_unused(func: &mut IrFunction) {
    let mut changed = true;
    while changed {
        calculate_lifetimes(func);
        changed = false;
        for block in func.blocks.iter_mut() {
            let mut to_remove = vec![];
            for (i, ssa) in block.ssas.iter_mut().enumerate() {
                if ssa.local.is_some() && ssa.local_lifetime == 0 {
                    to_remove.push(i);
                    changed = true;
                }
            }

            for (i, removed) in to_remove.into_iter().enumerate() {
                block.ssas.remove(removed - i);
            }
        }
    }
}

fn calculate_lifetimes(func: &mut IrFunction) {
    let mut func_iter = func.blocks.iter_mut();
    while let Some(block) = func_iter.next() {
        let mut iter = block.ssas.iter_mut();
        let mut i = 0;
        while let Some(ssa) = iter.next() {
            if ssa.local.is_none() {
                continue;
            }
            let local = ssa.local.unwrap();
            ssa.local_lifetime = 0;

            let mut j = i + 1;
            for next in iter.as_slice() {
                for arg in next.args.iter() {
                    if let IrArgument::Local(l) = arg {
                        if *l == local {
                            ssa.local_lifetime = j - i;
                            break;
                        }
                    }
                }

                j += 1;
            }
            if block.terminator.args.contains(&IrArgument::Local(local)) {
                ssa.local_lifetime = j - i;
            }
            j += 1;

            for next_block in func_iter.as_slice() {
                for next_block_ssa in next_block.ssas.iter() {
                    if next_block_ssa.args.contains(&IrArgument::Local(local)) {
                        ssa.local_lifetime = j - i;
                        if !matches!(next_block_ssa.instr, IrInstruction::Phi) {
                            ssa.global = true;
                        } else {
                            break;
                        }
                    }

                    j += 1;
                }

                if next_block
                    .terminator
                    .args
                    .contains(&IrArgument::Local(local))
                {
                    ssa.local_lifetime = j - i;
                }
                j += 1;
            }

            i += 1;
        }
    }
}

fn inliner(func: &mut IrFunction) {
    let mut changed = true;
    while changed {
        changed = false;
        let mut locals_to_inlined = HashMap::new();

        for block in func.blocks.iter_mut() {
            for ssa in block.ssas.iter_mut() {
                if matches!(ssa.instr, IrInstruction::Load | IrInstruction::Literal)
                    && ssa.local.is_some()
                {
                    locals_to_inlined.insert(ssa.local.unwrap(), ssa.args[0].clone());
                } else if matches!(ssa.instr, IrInstruction::Phi) {
                    continue;
                }

                for arg in ssa.args.iter_mut() {
                    if let IrArgument::Local(local) = arg {
                        if let Some(inline) = locals_to_inlined.get(local) {
                            changed = true;
                            *arg = inline.clone();
                        }
                    }
                }
            }

            for arg in block.terminator.args.iter_mut() {
                if let IrArgument::Local(local) = arg {
                    if let Some(inline) = locals_to_inlined.get(local) {
                        changed = true;
                        *arg = inline.clone();
                    }
                }
            }
        }
    }
}

fn unneeded_set_remover(func: &mut IrFunction) {
    for block in func.blocks.iter_mut() {
        let mut to_remove = vec![];
        for (i, ssa) in block.ssas.iter().enumerate() {
            if matches!(ssa.instr, IrInstruction::Set) && ssa.args[0] == ssa.args[1] {
                to_remove.push(i);
            }
        }

        for (i, remove) in to_remove.into_iter().enumerate() {
            block.ssas.remove(remove - i);
        }
    }
}

pub fn ast_to_ir(ast: Ast) -> Result<IrModule, IrError> {
    let mut module = IrModule { funcs: vec![] };
    let mut func = IrFunction {
        name: String::from("main"),
        external: false,
        argc: 0,
        captured: vec![],
        blocks: vec![],
        last_local: 0,
    };
    let mut block = IrBasicBlock {
        id: 0,
        ssas: vec![],
        terminator: IrSsa {
            local: None,
            local_lifetime: 0,
            local_register: 0,
            global: false,
            instr: IrInstruction::Ret,
            args: vec![],
        },
    };

    let mut scope = vec![HashMap::new()];

    let ret = ast_to_ir_helper(ast, &mut scope, &mut module, &mut func, &mut block)?;
    if let Some(ret) = ret {
        block.terminator.args.push(IrArgument::Local(ret));
    }

    func.blocks.push(block);
    module.funcs.push(func);

    for func in module.funcs.iter_mut() {
        remove_unused(func);

        if is_tail_recursive(func) {
            let name = IrArgument::Function(func.name.clone());
            for block in func.blocks.iter_mut() {
                let mut replace_index = None;
                let mut replacers = vec![];
                for (i, ssa) in block.ssas.iter().enumerate() {
                    if matches!(ssa.instr, IrInstruction::Call) && ssa.args[0] == name {
                        replace_index = Some(i);
                        for (i, arg) in ssa.args.iter().skip(1).enumerate() {
                            replacers.push(IrSsa {
                                local: None,
                                local_lifetime: 0,
                                local_register: 0,
                                global: false,
                                instr: IrInstruction::Set,
                                args: vec![IrArgument::Argument(i), arg.clone()],
                            });
                        }
                        break;
                    }
                }

                if let Some(replace_index) = replace_index {
                    block.ssas.remove(replace_index);
                    for (i, ssa) in replacers.into_iter().enumerate() {
                        block.ssas.insert(replace_index + i, ssa);
                    }
                    block.terminator = IrSsa {
                        local: None,
                        local_lifetime: 0,
                        local_register: 0,
                        global: false,
                        instr: IrInstruction::Jump,
                        args: vec![IrArgument::BasicBlock(0)],
                    };
                }
            }
        }
        inliner(func);
        unneeded_set_remover(func);
        remove_unused(func);
    }

    Ok(module)
}
