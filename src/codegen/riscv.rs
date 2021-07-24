use std::collections::{HashMap, HashSet};

use super::super::ir::{IrArgument, IrInstruction, IrModule};
use super::{linear_scan, GeneratedCode};

const ARG_REGISTER_COUNT: usize = 8;
const NONARG_REGISTER_COUNT: usize = 17;

#[derive(Debug, Clone, Copy)]
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

    // Temporary register for calculations
    S11,

    // Temporaries (locals)
    T3,
    T4,
    T5,
    T6,

    // Spilled locals
    Spilled(usize),

    // Spilled arguments
    Arg(usize),
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
        !matches!(self, Spilled(_) | Arg(_))
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
            0 => T0,
            1 => T1,
            2 => T2,
            3 => T3,
            4 => T4,
            5 => T5,
            6 => T6,
            7 => S1,
            8 => S2,
            9 => S3,
            10 => S4,
            11 => S5,
            12 => S6,
            13 => S7,
            14 => S8,
            15 => S9,
            16 => S10,
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
            Arg(_) => panic!("Arguments on the stack are not registers!"),
        }
    }
}

fn push_instr<T: std::hash::Hash + Eq + PartialEq>(code: &mut GeneratedCode<T>, instr: u32) {
    code.data.push((instr & 0xff) as u8);
    code.data.push(((instr >> 8) & 0xff) as u8);
    code.data.push(((instr >> 16) & 0xff) as u8);
    code.data.push(((instr >> 24) & 0xff) as u8);
}

fn load_float<T: std::hash::Hash + Eq + PartialEq>(
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
            let bits = [(as_bits >> 33) & 0x7ff, (as_bits >> 22) & 0x7ff, (as_bits >> 11) & 0x7ff, as_bits & 0x7ff];

            let mut bitshift_acc = 0;
            for bits in bits {
                bitshift_acc += 11;
                if bits == 0 {
                    continue;
                }

                // slli reg, reg, bitshift_acc
                let instr = 0x1013 | (reg << 7) | (reg << 15) | (bitshift_acc << 20);
                push_instr(code, instr);
                bitshift_acc = 0;

                // ori reg, reg, bits
                let instr = 0x6013 | (reg << 7) | (reg << 15) | ((bits as u32) << 20);
                push_instr(code, instr);
            }
        } else {
            // slli reg, reg, 31
            let instr = 0x1013 | (reg << 7) | (reg << 15) | (31 << 20);
            push_instr(code, instr);

            // slli reg, reg, 1
            let instr = 0x1013 | (reg << 7) | (reg << 15) | (1 << 20);
            push_instr(code, instr);
        }
    } else {
        load_float(code, Register::S11, float);
        generate_mov(code, reg, Register::S11);
    }
}

fn generate_mov<T: Eq + PartialEq + std::hash::Hash>(code: &mut GeneratedCode<T>, dest: Register, source: Register) {
    match (dest.is_register(), source.is_register()) {
        (true, true) => {
            let instr = 0x0013 | (dest.get_register() << 7) | (source.get_register() << 15);
            push_instr(code, instr);
        }

        (true, false) => todo!(),
        (false, true) => todo!(),

        (false, false) => {
            generate_mov(code, Register::S11, source);
            generate_mov(code, dest, Register::S11);
        }
    }
}

pub fn generate_code(root: &mut IrModule) -> GeneratedCode<String> {
    let mut code = GeneratedCode {
        addrs: HashMap::new(),
        refs: HashMap::new(),
        data: vec![],
    };

    for func in root.funcs.iter_mut() {
        linear_scan(func, NONARG_REGISTER_COUNT);
    }

    for func in root.funcs.iter() {
        code.addrs.insert(func.name.clone(), code.data.len()..0);

        let mut func_code = GeneratedCode {
            addrs: HashMap::new(),
            refs: HashMap::new(),
            data: vec![],
        };

        // addi sp, sp, 8
        push_instr(&mut func_code, 0x00810113);

        // sd fp, 0(sp)
        push_instr(&mut func_code, 0x00813023);

        // mv fp, sp
        push_instr(&mut func_code, 0x00010413);

        let mut used_registers = HashSet::new();
        for block in func.blocks.iter() {
            for ssa in block.ssas.iter() {
                if ssa.local.is_some()
                    && Register::convert_nonarg_register_id(ssa.local_register).is_callee_saved()
                    && !used_registers.contains(&ssa.local_register)
                {
                    used_registers.insert(ssa.local_register);
                }
            }
        }

        // Push used registers
        let used_registers: Vec<_> = used_registers.into_iter().collect();
        push_instr(&mut func_code, 0x00010113 | ((-(used_registers.len() as i32) * 8) << 20) as u32);
        for (i, register) in used_registers.iter().enumerate() {
            let register = Register::convert_nonarg_register_id(*register);
            let offset = i as u32 * 8;
            push_instr(&mut func_code, 0x03023 | (Register::Sp.get_register() << 15) | (register.get_register() << 20) | ((offset & 0x1f) << 7) | ((offset & !0x1f) << 25));
        }

        let mut local_to_register = HashMap::new();
        let mut register_lifetimes = vec![0; NONARG_REGISTER_COUNT];

        for block in func.blocks.iter() {
            func_code.addrs.insert(block.id, func_code.data.len()..0);

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
                    IrInstruction::Call => todo!(),
                    IrInstruction::List => todo!(),

                    IrInstruction::Load => todo!(),

                    IrInstruction::Set => todo!(),

                    IrInstruction::Literal => {
                        if let Some(local) = ssa.local {
                            let reg = *local_to_register.get(&local).unwrap();
                            load_float(&mut func_code, reg, if let IrArgument::Literal(lit) = ssa.args[0] { lit } else { unreachable!(); });
                        }
                    }

                    IrInstruction::Capture => todo!(),
                    IrInstruction::RcInc => todo!(),
                    IrInstruction::RcDec => todo!(),
                    IrInstruction::Phi => todo!(),

                    IrInstruction::Ret | IrInstruction::Jump | IrInstruction::Branch => {
                        unreachable!()
                    }
                }
            }

            match block.terminator.instr {
                IrInstruction::Ret => {
                    match &block.terminator.args[0] {
                        IrArgument::Literal(float) => {
                            load_float(&mut func_code, Register::A0, *float);
                        }

                        IrArgument::Local(_) => todo!(),
                        IrArgument::Argument(_) => todo!(),
                        IrArgument::Atom(_) => todo!(),

                        IrArgument::Function(_) => todo!(),

                        IrArgument::Closed(_) => todo!(),

                        IrArgument::BasicBlock(_) => unreachable!(),
                    }

                    // mv sp, fp
                    push_instr(&mut func_code, 0x00040113);

                    // ld fp, 0(sp)
                    push_instr(&mut func_code, 0x00013403);

                    // addi sp, sp, -8
                    push_instr(&mut func_code, 0xff810113);

                    // ret
                    push_instr(&mut func_code, 0x00008067);
                }

                IrInstruction::Jump => todo!(),
                IrInstruction::Branch => todo!(),
                _ => unreachable!(),
            }
        }

        code.data.append(&mut func_code.data);
        code.addrs.get_mut(&func.name).unwrap().end = code.data.len();
    }

    code
}
