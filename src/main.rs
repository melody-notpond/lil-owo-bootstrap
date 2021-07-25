use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

use object::{Architecture, BinaryFormat, Endianness, RelocationEncoding, RelocationKind, SymbolFlags, SymbolKind, SymbolScope};
use object::write::{Object, Relocation, StandardSection, Symbol, SymbolSection};

use lil_owo_bootstrap::ir;
use lil_owo_bootstrap::parser;
use lil_owo_bootstrap::codegen::riscv;

fn main() {
    let parse = "
    (func loop (n) (loop n)) 0
    ";
    println!("{}", parse);
    let ast = parser::parse("stdin", parse).unwrap();
    let mut root = ir::ast_to_ir(ast).unwrap();
    println!("{}", root);
    let code = riscv::generate_code(&mut root);

    let mut obj = Object::new(BinaryFormat::Elf, Architecture::Riscv64, Endianness::Little);
    let mut funcs: HashMap<&str, _> = HashMap::new();
    let mut symbols = HashMap::new();

    for (name, range) in code.get_addrs() {
        if range.start == range.end {
            continue;
        }

        let (section_id, _) = obj.add_subsection(StandardSection::Text, name.as_bytes(), &code.data()[range.start..range.end], 16);
        funcs.insert(name, section_id);
        obj.add_symbol(Symbol {
            name: name.as_bytes().to_owned(),
            value: 0,
            size: (range.end - range.start) as u64,
            kind: SymbolKind::Text,
            scope: SymbolScope::Linkage,
            weak: false,
            section: SymbolSection::Section(section_id),
            flags: SymbolFlags::None,
        });
    }

    for (name, range) in code.get_addrs() {
        if range.start != range.end {
            continue;
        }

        let mut offset = 0;
        let mut parent = "";
        for (name, range2) in code.get_addrs() {
            if range2.start <= range.start && range.end < range2.end {
                offset = (range.start - range2.start) as u64;
                parent = name;
                break;
            }
        }

        let symbol_id = obj.add_symbol(Symbol {
            name: name.as_bytes().to_owned(),
            value: offset,
            size: 1,
            kind: SymbolKind::Text,
            scope: SymbolScope::Linkage,
            weak: false,
            section: SymbolSection::Section(*funcs.get(parent).unwrap()),
            flags: SymbolFlags::None,
        });

        symbols.insert(name, symbol_id);
    }

    for (addr, (to, reloc_type)) in code.get_relocation_table() {
        for (from, range) in code.get_addrs() {
            if range.start <= *addr && *addr < range.end {
                let from = *funcs.get(from.as_str()).unwrap();
                let symbol = match funcs.get(to.as_str()) {
                    Some(v) => obj.section_symbol(*v),
                    None => *symbols.get(to).unwrap()
                };

                match obj.add_relocation(from, Relocation {
                    offset: (*addr - range.start) as u64,
                    size: reloc_type.get_size(),
                    kind: RelocationKind::Elf(reloc_type.as_int()),
                    encoding: RelocationEncoding::Generic,
                    symbol,
                    addend: 0,
                }) {
                    Ok(_) => (),
                    Err(e) => {
                        eprintln!("Error creating relocation: {}", e);
                        std::process::exit(1);
                    }
                }
                break;
            }
        }
    }

    let obj = match obj.write() {
        Ok(v) => v,
        Err(e) => {
            eprintln!("Error writing object file to vector: {}", e);
            std::process::exit(1);
        }
    };

    let mut file = match File::create("uwu.o") {
        Ok(v) => v,
        Err(e) => {
            eprintln!("Error getting file uwu.o: {}", e);
            std::process::exit(1);
        }
    };

    match file.set_len(0) {
        Ok(_) => (),
        Err(e) => {
            eprintln!("Error clearing file: {}", e);
            std::process::exit(1);
        }
    }

    match file.write(&obj) {
        Ok(_) => (),
        Err(e) => {
            eprintln!("Error writing to file: {}", e);
            std::process::exit(1);
        }
    }

    /*
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

    for (addr, (to, reloc_type)) in code.get_relocation_table() {
        for (from, range) in code.get_addrs() {
            if range.start <= *addr && *addr < range.end {
                match artefact.link_with(Link {
                    from,
                    to,
                    at: (addr - range.start) as u64,
                }, Reloc::Raw { reloc: reloc_type.as_int(), addend: 0 }) {
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
*/
}
