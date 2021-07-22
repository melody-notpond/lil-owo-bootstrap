use std::fs::File;

use faerie::{ArtifactBuilder, Decl, Link};
use target_lexicon::{
    Architecture, BinaryFormat, Environment, OperatingSystem, Riscv64Architecture, Triple, Vendor,
};

use lil_owo_bootstrap::codegen::riscv;
use lil_owo_bootstrap::ir;
use lil_owo_bootstrap::parser;

fn main() {
    let parse = "
    3.141592653
    ";
    println!("{}", parse);
    let ast = parser::parse("stdin", parse).unwrap();
    let mut root = ir::ast_to_ir(ast).unwrap();
    println!("{}", root);
    let code = riscv::generate_code(&mut root);
    let mut artefact = ArtifactBuilder::new(Triple {
        architecture: Architecture::Riscv64(Riscv64Architecture::Riscv64gc),
        vendor: Vendor::Unknown,
        operating_system: OperatingSystem::Unknown,
        environment: Environment::Unknown,
        binary_format: BinaryFormat::Elf,
    })
    .name(String::from("uwu"))
    .finish();

    let mut funcs: Vec<_> = code.get_addrs().iter().collect();
    funcs.sort_by(|a, b| a.1.start.cmp(&b.1.start));
    match artefact.declarations({
        funcs.iter().map(|v| {
            (
                v.0,
                if v.0 == "_start" || v.0 == ".main" {
                    Decl::function().global().into()
                } else if v.1.start == 0 && v.1.end == 0 {
                    Decl::function_import().into()
                } else {
                    Decl::function().into()
                },
            )
        })
    }) {
        Ok(_) => (),
        Err(e) => {
            eprintln!("Error declaring functions: {}", e);
            return;
        }
    }

    for (func, range) in funcs {
        if range.start == 0 && range.end == 0 {
            continue;
        }

        match artefact.define(func, code.data()[range.start..range.end].to_owned()) {
            Ok(_) => (),
            Err(e) => {
                eprintln!("Error defining function: {}", e);
                return;
            }
        }
    }

    for (addr, to) in code.get_relocation_table() {
        for (from, range) in code.get_addrs() {
            if range.start <= *addr && *addr < range.end {
                match artefact.link(Link {
                    from,
                    to,
                    at: (addr - range.start) as u64,
                }) {
                    Ok(_) => (),
                    Err(e) => {
                        eprintln!("Error linking: {}", e);
                        return;
                    }
                }
                break;
            }
        }
    }

    match artefact.write(match File::create("uwu.o") {
        Ok(v) => v,
        Err(e) => {
            eprintln!("Error getting file uwu.o: {}", e);
            std::process::exit(1);
        }
    }) {
        Ok(_) => (),
        Err(e) => {
            eprintln!("Error writing artefact to file: {}", e);
        }
    }
}
