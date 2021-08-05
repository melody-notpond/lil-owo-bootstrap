use std::collections::HashMap;
use std::fs::File;
use std::io::Write;

use object::{Architecture, BinaryFormat, Endianness, FileFlags, RelocationEncoding, RelocationKind, SectionKind, SymbolFlags, SymbolKind, SymbolScope};
use object::write::{Object, Relocation, StandardSection, Symbol, SymbolSection};

use lil_owo_bootstrap::ir;
use lil_owo_bootstrap::parser;
use lil_owo_bootstrap::codegen::riscv;

fn main() {
    let mut parse = "
        (extern test)
        (func loop (n)
            (seq
                (test n)
                (loop n)))
        (loop 0)
    ";

    let mut v = String::from("seq\n");
    if !parse.trim().starts_with("seq") {
        v.push_str(parse);
        parse = v.as_str();
    }

    println!("{}", parse);
    let ast = parser::parse("stdin", parse).unwrap();
    let mut root = ir::ast_to_ir(ast).unwrap();
    println!("{}", root);
    let mut code = riscv::generate_code(&mut root);
    riscv::generate_start_fn(&mut code);

    let mut obj = Object::new(BinaryFormat::Elf, Architecture::Riscv64, Endianness::Little);
    obj.flags = FileFlags::Elf {
        e_flags: 0x4
    };
    let mut funcs: HashMap<&str, _> = HashMap::new();
    let mut symbols = HashMap::new();
    let atom_section = obj.add_section(vec![], "lil_owo_atoms".as_bytes().to_owned(), SectionKind::ReadOnlyData);

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

    let mut offset = 0;
    for atom in code.get_atoms() {
        let symbol_id = obj.add_symbol(Symbol {
            name: atom.as_bytes().to_owned(),
            value: offset,
            size: atom.len() as u64,
            kind: SymbolKind::Data,
            scope: SymbolScope::Linkage,
            weak: false,
            section: SymbolSection::Section(atom_section),
            flags: SymbolFlags::None,
        });
        let section = obj.section_mut(atom_section);
        section.append_data(atom.as_bytes(), 16);
        section.append_data(&[0], 1);
        offset += atom.len() as u64 + 1;

        symbols.insert(atom, symbol_id);
    }

    for external_func in code.get_externs() {
        let symbol_id = obj.add_symbol(Symbol {
            name: external_func.as_bytes().to_owned(),
            value: 0,
            size: 0,
            kind: SymbolKind::Text,
            scope: SymbolScope::Linkage,
            weak: false,
            section: SymbolSection::Undefined,
            flags: SymbolFlags::None,
        });

        symbols.insert(external_func, symbol_id);
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
}
