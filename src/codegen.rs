pub mod riscv;

use std::collections::{HashMap, HashSet};
use std::ops::Range;

use super::ir::IrFunction;

#[derive(Debug)]
pub struct GeneratedCode<T> {
    addrs: HashMap<String, Range<usize>>,
    externs: HashSet<String>,
    refs: HashMap<usize, (String, T)>,
    data: Vec<u8>,
}

impl<T> GeneratedCode<T> {
    pub fn data(&self) -> &Vec<u8> {
        &self.data
    }

    pub fn get_externs(&self) -> &HashSet<String> {
        &self.externs
    }

    pub fn get_addrs(&self) -> &HashMap<String, Range<usize>> {
        &self.addrs
    }

    pub fn get_relocation_table(&self) -> &HashMap<usize, (String, T)> {
        &self.refs
    }
}

/// Performs register allocation by linear scan on an IrFunction.
pub(crate) fn linear_scan(func: &mut IrFunction, register_count: usize) {
    let mut saved = vec![(0, false); register_count];
    for block in func.blocks.iter_mut() {
        let mut register_lifetimes = saved;
        'a: for ssa in block.ssas.iter_mut() {
            for (lifetime, _) in register_lifetimes.iter_mut() {
                if *lifetime > 0 {
                    *lifetime -= 1;
                }
            }

            if ssa.local.is_some() {
                for (reg, (lifetime, global)) in register_lifetimes.iter_mut().enumerate() {
                    if *lifetime == 0 {
                        *lifetime = ssa.local_lifetime;
                        *global = ssa.global;
                        ssa.local_register = reg;
                        continue 'a;
                    }
                }
                ssa.local_register = register_lifetimes.len();
                register_lifetimes.push((ssa.local_lifetime, ssa.global));
            }
        }

        for (lifetime, _) in register_lifetimes.iter_mut() {
            if *lifetime > 0 {
                *lifetime -= 1;
            }
        }

        saved = register_lifetimes
            .into_iter()
            .filter(|v| v.1 && v.0 != 0)
            .collect();
    }
}
