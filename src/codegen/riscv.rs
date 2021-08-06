use std::collections::{HashMap, HashSet};

use super::super::ir::{IrArgument, IrBasicBlock, IrFunction, IrInstruction, IrModule};
use super::{linear_scan, GeneratedCode, NaNBoxedTag};

const ARG_REGISTER_COUNT: usize = 8;
const NONARG_REGISTER_COUNT: usize = 15;

#[derive(Debug)]
pub enum RiscVRelocations {
    Lower12Bits,
    Upper20Bits
}

impl RiscVRelocations {
    pub fn as_int(&self) -> u32 {
        match self {
            RiscVRelocations::Lower12Bits => 24,
            RiscVRelocations::Upper20Bits => 23,
        }
    }

    pub fn get_size(&self) -> u8 {
        match self {
            RiscVRelocations::Lower12Bits => 12,
            RiscVRelocations::Upper20Bits => 20,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[allow(dead_code)]
enum Register {
    // Zero
    Zero,

    // Return address
    Ra,

    // Stack pointer
    Sp,

    // Idk what these do so we never use them
    Gp,
    Tp,

    // Temporaries (locals)
    T0,
    T1,
    T2,

    // Frame pointer
    Fp,

    // Temporary (locals)
    S1,

    // Argument registers
    A0, // Return register and argument count
    A1,
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,

    // Temporaries (locals)
    S2,
    S3,
    S4,
    S5,
    S6,
    S7,
    S8,
    S9,
    S10,
    S11,

    // Temporary (locals)
    T3,

    // Temporary registers for calculations
    T4,
    T5,
    T6,

    // Spilled locals
    Spilled(usize),

    // Spilled arguments
    Arg(usize),

    // Spilled arguments when pushing to stack
    AntiArg(usize, usize),
}

impl Register {
    fn is_callee_saved(&self) -> bool {
        use Register::*;
        matches!(
            self,
            Sp | Fp | S1 | S2 | S3 | S4 | S5 | S6 | S7 | S8 | S9 | S10 | S11
        )
    }

    fn is_register(&self) -> bool {
        use Register::*;
        !matches!(self, Spilled(_) | Arg(_) | AntiArg(_, _))
    }

    fn convert_arg_register_id(id: usize) -> Register {
        use Register::*;
        match id {
            0 => A0,
            1 => A1,
            2 => A2,
            3 => A3,
            4 => A4,
            5 => A5,
            6 => A6,
            7 => A7,
            _ => Arg(id - ARG_REGISTER_COUNT),
        }
    }

    fn convert_nonarg_register_id(id: usize) -> Register {
        use Register::*;
        match id {
            0 => S1,
            1 => S2,
            2 => S3,
            3 => S4,
            4 => S5,
            5 => S6,
            6 => S7,
            7 => S8,
            8 => S9,
            9 => S10,
            10 => S11,
            11 => T0,
            12 => T1,
            13 => T2,
            14 => T3,
            _ => Spilled(id - NONARG_REGISTER_COUNT),
        }
    }

    fn get_register(&self) -> u32 {
        use Register::*;
        match self {
            Zero => 0,
            Ra => 1,
            Sp => 2,
            Gp => 3,
            Tp => 4,
            T0 => 5,
            T1 => 6,
            T2 => 7,
            Fp => 8,
            S1 => 9,
            A0 => 10,
            A1 => 11,
            A2 => 12,
            A3 => 13,
            A4 => 14,
            A5 => 15,
            A6 => 16,
            A7 => 17,
            S2 => 18,
            S3 => 19,
            S4 => 20,
            S5 => 21,
            S6 => 22,
            S7 => 23,
            S8 => 24,
            S9 => 25,
            S10 => 26,
            S11 => 27,
            T3 => 28,
            T4 => 29,
            T5 => 30,
            T6 => 31,

            Spilled(_) => panic!("Spilled values are not registers!"),
            Arg(_) | AntiArg(_, _) => panic!("Arguments on the stack are not registers!"),
        }
    }
}

fn push_instr<T>(code: &mut GeneratedCode<T>, instr: u32) {
    code.data.push((instr & 0xff) as u8);
    code.data.push(((instr >> 8) & 0xff) as u8);
    code.data.push(((instr >> 16) & 0xff) as u8);
    code.data.push(((instr >> 24) & 0xff) as u8);
}

fn load_int<T>(
    code: &mut GeneratedCode<T>,
    reg: Register,
    int: i32,
) {
    if -(2i32.pow(11)) <= int && int < (2i32.pow(11)) {
        let instr = 0x13 | (reg.get_register() << 7) | ((int as u32 & 0xfff) << 20);
        push_instr(code, instr);
    } else {
        todo!();
    }
}

fn load_float<T>(
    code: &mut GeneratedCode<T>,
    reg: Register,
    float: f64,
) {
    let mut as_bits = float.to_bits();

    if reg.is_register() {
        let reg = reg.get_register();

        // lui reg, most significant 20 bits of float
        let instr = 0x37 | (reg << 7) | ((as_bits & 0xfffff00000000000) >> 32) as u32;
        push_instr(code, instr);
        as_bits &= !0xfffff00000000000;

        // Do the remaining bits
        as_bits &= 0x00000fffffffffff;
        if as_bits != 0 {
            let bits = [
                (as_bits >> 32) & 0x3ff,
                (as_bits >> 22) & 0x7ff,
                (as_bits >> 11) & 0x7ff,
                as_bits & 0x7ff,
            ];

            let mut bitshift_acc = -1;
            for bits in bits {
                if bits == 0 {
                    bitshift_acc += 11;
                    continue;
                }

                if bitshift_acc > 0 {
                    // slli reg, reg, bitshift_acc
                    let instr = 0x1013 | (reg << 7) | (reg << 15) | ((bitshift_acc as u32) << 20);
                    push_instr(code, instr);
                    bitshift_acc = 0;
                } else {
                    bitshift_acc += 11;
                }

                // ori reg, reg, bits
                let instr = 0x6013 | (reg << 7) | (reg << 15) | ((bits as u32) << 20);
                push_instr(code, instr);
            }
        } else {
            // slli reg, reg, 32
            let instr = 0x1013 | (reg << 7) | (reg << 15) | (32 << 20);
            push_instr(code, instr);
        }
    } else {
        load_float(code, Register::T6, float);
        generate_mov(code, reg, Register::T6);
    }
}

fn load_atom(code: &mut GeneratedCode<RiscVRelocations>, last_label: &mut usize, reg: Register, atom: &str) {
    if reg.is_register() {
        if !code.atoms.contains(atom) {
            code.atoms.insert(String::from(atom));
        }

        // auipc reg, higher 20 bits of the offset
        let instr = 0x17 | (reg.get_register() << 7);
        let label = format!("{}", last_label);
        *last_label += 1;
        code.addrs.insert(label.clone(), code.data.len()..code.data.len());
        code.refs.insert(code.data.len(), (String::from(atom), RiscVRelocations::Upper20Bits));
        push_instr(code, instr);

        // addi reg, reg, lower 12 bits of the offset
        let instr = 0x13 | (reg.get_register() << 15) | (reg.get_register() << 7);
        code.refs.insert(code.data.len(), (label, RiscVRelocations::Lower12Bits));
        push_instr(code, instr);

        // load_float t5, nan(atom)
        load_float(code, Register::T5, NaNBoxedTag::Atom.get_tagged_nan());

        // or reg, reg, t5
        let instr = 0x6033 | (reg.get_register() << 7) | (reg.get_register() << 15) | (Register::T5.get_register() << 20);
        push_instr(code, instr);
    } else {
        load_atom(code, last_label, Register::T6, atom);
        generate_mov(code, reg, Register::T6);
    }
}

fn generate_mov<T>(
    code: &mut GeneratedCode<T>,
    dest: Register,
    source: Register,
) {
    if dest == source {
        return;
    }

    match (dest.is_register(), source.is_register()) {
        (true, true) => {
            let instr = 0x0013 | (dest.get_register() << 7) | (source.get_register() << 15);
            push_instr(code, instr);
        }

        (true, false) => {
            match source {
                // TODO: temporarily disable inlining to test this
                Register::Spilled(local) => {
                    let offset = ((local as i32 + 1) * 8) as u32;
                    let instr = 0x3003 | (dest.get_register() << 7) | (Register::Fp.get_register() << 15) | ((offset & 0xfff) << 20);
                    push_instr(code, instr);
                }

                Register::Arg(arg) => {
                    let offset = (-(arg as i32 + 1) * 8) as u32;
                    let instr = 0x3003 | (dest.get_register() << 7) | (Register::Fp.get_register() << 15) | ((offset & 0xfff) << 20);
                    push_instr(code, instr);
                }

                _ => unreachable!()
            }
        }

        (false, true) => {
            match dest {
                Register::Spilled(local) => {
                    let offset = ((local as i32 + 1) * 8) as u32;
                    let instr = 0x3023 | (source.get_register() << 20) | (Register::Fp.get_register() << 15) | ((offset & 0xfe0) << 20) | ((offset & 0x1f) << 7);
                    push_instr(code, instr);
                }

                Register::Arg(arg) => {
                    let offset = (-(arg as i32 + 1) * 8) as u32;
                    let instr = 0x3023 | (source.get_register() << 20) | (Register::Fp.get_register() << 15) | ((offset & 0xfe0) << 20) | ((offset & 0x1f) << 7);
                    push_instr(code, instr);
                }

                Register::AntiArg(arg, count) => {
                    let offset = ((count - arg) * 8) as u32;
                    let instr = 0x3023 | (source.get_register() << 20) | (Register::Sp.get_register() << 15) | ((offset & 0xfe0) << 20) | ((offset & 0x1f) << 7);
                    push_instr(code, instr);
                }

                _ => unreachable!()
            }
        }

        (false, false) => {
            generate_mov(code, Register::T6, source);
            generate_mov(code, dest, Register::T6);
        }
    }
}

fn j_type(imm: i32) -> u32 {
    // imm[20|10:1|11|19:12]
    // I hate this so much >:(
    let imm = imm as u32;
    (imm & (1 << 20)) << 11 | ((imm & 0x7fe) << 20) | ((imm & 0x800) << 9) | (imm & 0xff000)
}

pub fn generate_code(root: &mut IrModule) -> GeneratedCode<RiscVRelocations> {
    let mut code = GeneratedCode {
        addrs: HashMap::new(),
        externs: root.funcs.iter().filter_map(|v| if v.external {
            Some(v.name.clone())
        } else {
            None
        }).collect(),
        atoms: HashSet::new(),
        refs: HashMap::new(),
        data: vec![],
    };

    for func in root.funcs.iter_mut() {
        linear_scan(func, NONARG_REGISTER_COUNT);
    }

    let mut last_label = 1;
    for func in root.funcs.iter() {
        if func.external {
            continue;
        }

        code.addrs.insert(func.name.clone(), code.data.len()..0);

        // addi sp, sp, 8
        push_instr(&mut code, 0xff810113);

        // sd fp, 0(sp)
        push_instr(&mut code, 0x00813023);

        // mv fp, sp
        push_instr(&mut code, 0x00010413);

        let mut used_registers = HashSet::new();
        for block in func.blocks.iter() {
            for ssa in block.ssas.iter() {
                if ssa.local.is_some()
                    && Register::convert_nonarg_register_id(ssa.local_register).is_callee_saved()
                {
                    used_registers.insert(ssa.local_register);
                }
            }
        }

        // Push used registers
        let used_registers: Vec<_> = used_registers.into_iter().collect();
        if !used_registers.is_empty() {
            push_instr(
                &mut code,
                0x00010113 | ((-(used_registers.len() as i32) * 8) << 20) as u32,
            );
            for (i, register) in used_registers.iter().enumerate() {
                let register = Register::convert_nonarg_register_id(*register);
                let offset = i as u32 * 8;
                push_instr(
                    &mut code,
                    0x03023
                        | (Register::Sp.get_register() << 15)
                        | (register.get_register() << 20)
                        | ((offset & 0x1f) << 7)
                        | ((offset & !0x1f) << 25),
                );
            }
        }

        let mut local_to_register = HashMap::new();
        let mut register_lifetimes = vec![0; NONARG_REGISTER_COUNT];
        let mut block_to_addr = Vec::new();
        let mut block_refs = HashMap::new();

        for block in func.blocks.iter() {
            block_to_addr.push(code.data.len());

            for ssa in block.ssas.iter() {
                for lifetime in register_lifetimes.iter_mut() {
                    if *lifetime != 0 {
                        *lifetime -= 1;
                    }
                }

                if let Some(local) = ssa.local {
                    let register = Register::convert_nonarg_register_id(ssa.local_register);

                    if register_lifetimes.len() < ssa.local_register {
                        register_lifetimes[ssa.local_register] = ssa.local_lifetime;
                    } else {
                        register_lifetimes.push(ssa.local_lifetime);
                    }

                    local_to_register.insert(local, register);
                }

                match ssa.instr {
                    IrInstruction::Call => {
                        // TODO: save caller saved registers

                        for (i, arg) in ssa.args.iter().skip(1).enumerate() {
                            let dest = Register::convert_arg_register_id(i);
                            let dest = if dest.is_register() {
                                dest
                            } else {
                                Register::AntiArg(i - ARG_REGISTER_COUNT, ssa.args.len() - ARG_REGISTER_COUNT - 1)
                            };

                            match arg {
                                IrArgument::Float(float) => {
                                    load_float(&mut code, dest, *float);
                                }

                                IrArgument::Int(int) => {
                                    load_float(&mut code, dest, NaNBoxedTag::Integer(*int).get_tagged_nan());
                                }

                                IrArgument::Local(_) => todo!(),
                                IrArgument::Argument(_) => todo!(),
                                IrArgument::Atom(_) => todo!(),
                                IrArgument::Function(_) => todo!(),
                                IrArgument::BasicBlock(_) => todo!(),
                                IrArgument::Closed(_) => todo!(),
                            }
                        }

                        match ssa.args.first().unwrap() {
                            IrArgument::Float(_) => unreachable!(),
                            IrArgument::Int(_) => unreachable!(),
                            IrArgument::Atom(_) => unreachable!(),
                            IrArgument::BasicBlock(_) => unreachable!(),

                            IrArgument::Local(_) => todo!(),
                            IrArgument::Argument(_) => todo!(),

                            IrArgument::Function(func) => {
                                // auipc t6, higher 20 bits of the offset
                                let instr = 0x17 | (Register::T6.get_register() << 7);
                                let label = format!("{}", last_label);
                                last_label += 1;
                                code.addrs.insert(label.clone(), code.data.len()..code.data.len());
                                code.refs.insert(code.data.len(), (func.clone(), RiscVRelocations::Upper20Bits));
                                push_instr(&mut code, instr);

                                // jalr lower 12 bits of the offset(t6)
                                let instr = 0x67 | (Register::T6.get_register() << 15) | (Register::Ra.get_register() << 7);
                                code.refs.insert(code.data.len(), (label, RiscVRelocations::Lower12Bits));
                                push_instr(&mut code, instr);
                            }

                            IrArgument::Closed(_) => todo!(),
                        }

                        if let Some(local) = ssa.local {
                            let reg = *local_to_register.get(&local).unwrap();
                            generate_mov(&mut code, reg, Register::A0);
                        }

                        // TODO: pop caller saved registers
                    }

                    IrInstruction::Load => todo!(),
                    IrInstruction::Set => todo!(),

                    IrInstruction::Literal => {
                        if let Some(local) = ssa.local {
                            let reg = *local_to_register.get(&local).unwrap();
                            load_float(
                                &mut code,
                                reg,
                                if let IrArgument::Float(lit) = ssa.args[0] {
                                    lit
                                } else if let IrArgument::Int(lit) = ssa.args[0] {
                                    NaNBoxedTag::Integer(lit).get_tagged_nan()
                                } else {
                                    unreachable!();
                                },
                            );
                        }
                    }

                    IrInstruction::Capture => todo!(),
                    IrInstruction::RcInc => todo!(),
                    IrInstruction::RcDec => todo!(),

                    IrInstruction::Phi => {
                        if let Some(local) = ssa.local {
                            let local = *local_to_register.get(&local).unwrap();
                            generate_mov(&mut code, local, Register::T6);
                        }
                    }

                    IrInstruction::Ret | IrInstruction::Jump | IrInstruction::Branch => {
                        unreachable!()
                    }
                }
            }

            match block.terminator.instr {
                IrInstruction::Ret => {
                    match &block.terminator.args[0] {
                        IrArgument::Float(float) => {
                            load_float(&mut code, Register::A0, *float);
                        }

                        IrArgument::Int(int) => {
                            load_float(&mut code, Register::A0, NaNBoxedTag::Integer(*int).get_tagged_nan());
                        }

                        IrArgument::Local(local) => {
                            let reg = *local_to_register.get(local).unwrap();
                            generate_mov(&mut code, Register::A0, reg);
                        }

                        IrArgument::Argument(arg) => {
                            let reg = Register::convert_arg_register_id(*arg);
                            generate_mov(&mut code, Register::A0, reg);
                        }

                        IrArgument::Atom(atom) => {
                            load_atom(&mut code, &mut last_label, Register::A0, atom);
                        }

                        IrArgument::Function(_) => todo!(),

                        IrArgument::Closed(_) => todo!(),

                        IrArgument::BasicBlock(_) => unreachable!(),
                    }

                    if !used_registers.is_empty() {
                        for (i, register) in used_registers.iter().enumerate() {
                            let register = Register::convert_nonarg_register_id(*register);
                            let offset = i as u32 * 8;
                            push_instr(
                                &mut code,
                                0x03003
                                    | (Register::Sp.get_register() << 15)
                                    | (register.get_register() << 7)
                                    | (offset << 20),
                            );
                        }

                        push_instr(
                            &mut code,
                            0x00010113 | ((used_registers.len() * 8) << 20) as u32,
                        );
                    }

                    // mv sp, fp
                    push_instr(&mut code, 0x00040113);

                    // ld fp, 0(sp)
                    push_instr(&mut code, 0x00013403);

                    // addi sp, sp, 8
                    push_instr(&mut code, 0x00810113);

                    // ret
                    push_instr(&mut code, 0x00008067);
                }

                IrInstruction::Jump => {
                    let next_block = if let IrArgument::BasicBlock(block) = block.terminator.args[0] {
                        block
                    } else {
                        unreachable!();
                    };

                    phi(&mut code, func, block, &local_to_register);
                    generate_jal(&mut code, next_block, &block_to_addr, &mut block_refs);
                }

                IrInstruction::Branch => {
                    let condition = &block.terminator.args[0];
                    let on_true = if let IrArgument::BasicBlock(block) = block.terminator.args[1] {
                        block
                    } else {
                        unreachable!();
                    };
                    let on_false = if let IrArgument::BasicBlock(block) = block.terminator.args[2] {
                        block
                    } else {
                        unreachable!();
                    };

                    phi(&mut code, func, block, &local_to_register);

                    let reg = match condition {
                        IrArgument::Float(float) => {
                            load_float(&mut code, Register::T5, *float);
                            Register::T5
                        }

                        IrArgument::Int(int) => {
                            load_int(&mut code, Register::T5, *int);
                            Register::T5
                        }

                        IrArgument::Atom(_) => todo!(),
                        IrArgument::Function(_) => todo!(),

                        IrArgument::Local(local) => {
                            let local = *local_to_register.get(local).unwrap();
                            if local.is_register() {
                                local
                            } else {
                                generate_mov(&mut code, Register::T5, local);
                                Register::T5
                            }
                        }

                        IrArgument::Argument(_) => todo!(),
                        IrArgument::Closed(_) => todo!(),

                        IrArgument::BasicBlock(_) => unreachable!(),
                    };

                    let instr = 0x0463 | (reg.get_register() << 15);
                    push_instr(&mut code, instr);
                    generate_jal(&mut code, on_true, &block_to_addr, &mut block_refs);
                    generate_jal(&mut code, on_false, &block_to_addr, &mut block_refs);
                }

                _ => unreachable!(),
            }
        }

        for (addr, block) in block_refs {
            let diff = block_to_addr[block] as i32 - addr as i32;
            if diff >= 2i32.pow(20) || diff < -(2i32.pow(20)) {
                panic!("unsupported difference");
            }

            let instr = 0x6f | j_type(diff);
            code.data[addr    ] = (instr         & 0xff) as u8;
            code.data[addr + 1] = ((instr >>  8) & 0xff) as u8;
            code.data[addr + 2] = ((instr >> 16) & 0xff) as u8;
            code.data[addr + 3] = ((instr >> 24) & 0xff) as u8;
        }

        code.addrs.get_mut(&func.name).unwrap().end = code.data.len();
    }

    code
}

fn generate_jal<T>(code: &mut GeneratedCode<T>, next_block: usize, block_to_addr: &[usize], block_refs: &mut HashMap<usize, usize>) {
    if let Some(next) = block_to_addr.get(next_block) {
        let diff = (*next as isize - code.data.len() as isize) as i32;

        if diff >= 2i32.pow(20) || diff < -(2i32.pow(20)) {
            panic!("unsupported difference");
        }

        // j diff
        let instr = 0x6f | j_type(diff);
        push_instr(code, instr);
    } else {
        block_refs.insert(code.data.len(), next_block);
        let instr = 0x6f;
        push_instr(code, instr);
    }
}

fn phi<T>(code: &mut GeneratedCode<T>, func: &IrFunction, block: &IrBasicBlock, local_to_register: &HashMap<usize, Register>) {
    let mut phi = None;
    'a: for block2 in func.blocks.iter().skip(block.id + 1) {
        for ssa in block2.ssas.iter() {
            if let IrInstruction::Phi = ssa.instr {
                let mut args = ssa.args.iter();
                while let Some(arg) = args.next() {
                    if let IrArgument::BasicBlock(id) = arg {
                        if *id == block.id {
                            phi = Some(args.next().unwrap());
                            break 'a;
                        }
                    }
                }
            }
        }
    }

    if let Some(phi) = phi {
        if let IrArgument::Local(local) = phi {
            let local = *local_to_register.get(local).unwrap();
            generate_mov(code, Register::T6, local);
        } else {
            unreachable!();
        }
    }
}

pub fn generate_start_fn(code: &mut GeneratedCode<RiscVRelocations>) {
    code.addrs.insert(String::from("_start"), code.data.len()..0);

    // auipc t6, higher 20 bits of the offset
    let instr = 0x17 | (Register::T6.get_register() << 7);
    code.refs.insert(code.data.len(), (String::from("main"), RiscVRelocations::Upper20Bits));
    push_instr(code, instr);

    // jalr lower 12 bits of the offset(t6)
    let instr = 0x67 | (Register::T6.get_register() << 15) | (Register::Ra.get_register() << 7);
    code.refs.insert(code.data.len(), (String::from("_start"), RiscVRelocations::Lower12Bits));
    push_instr(code, instr);

    code.addrs.get_mut("_start").unwrap().end = code.data.len();
}
