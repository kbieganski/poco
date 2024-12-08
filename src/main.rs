use std::{fs::File, io::Read};

use poco::{Error, Heap, Result, Source, Val};

fn main() {
    let (files, debug, emit_bc) = {
        let mut debug = false;
        let mut emit_bc = false;
        let mut files = Vec::new();
        for arg in std::env::args().skip(1) {
            match arg.as_str() {
                "--help" | "-h" => {
                    println!("Usage: poco [--help] [--version] [--debug] [--emit-bc]");
                }
                "--version" | "-v" => {
                    println!("poco version {}", env!("CARGO_PKG_VERSION"));
                }
                "--debug" => {
                    debug = true;
                }
                "--emit-bc" => {
                    emit_bc = true;
                }
                filename => files.push(filename.to_owned()),
            }
        }
        (files, debug, emit_bc)
    };
    for filename in files.into_iter() {
        match File::open(&filename) {
            Ok(mut file) => {
                let mut contents = String::new();
                if let Err(err) = file.read_to_string(&mut contents) {
                    eprintln!("Error reading file {filename}: {err}");
                    std::process::exit(1);
                }
                let source = Source {
                    name: filename.to_owned(),
                    contents,
                };
                match exec(&source, debug, emit_bc) {
                    Ok((Val::Table(refe), heap)) => {
                        let table_fmt = refe.format(&heap).quote_strings().force_decimal_sep();
                        println!("{refe} = {table_fmt}");
                    }
                    Ok((Val::String(string), _)) => {
                        println!("\"{string}\"");
                    }
                    Ok((Val::Float(float), _)) => {
                        println!("{float:?}");
                    }
                    Ok((val, _)) => {
                        println!("{val}");
                    }
                    Err(Error { loc, detail }) => {
                        let source = loc.source;
                        let max_line = source.contents.lines().count();
                        let max_lineno_width = max_line.ilog10() as usize + 1;
                        source
                            .contents
                            .lines()
                            .enumerate()
                            .filter(|(line, _)| {
                                const CONTEXT: i64 = 2;
                                (*line as i64) >= loc.line as i64 - CONTEXT
                                    && (*line as i64) <= loc.line as i64 + CONTEXT
                            })
                            .for_each(|(lineno, text)| {
                                println!(
                                    "{lineno: >max_lineno_width$} | {text}",
                                    lineno = lineno + 1
                                );
                                if lineno as u32 == loc.line {
                                    println!(
                                        "{linecol} | {caret_offset}^ Error: {detail}",
                                        linecol = " ".repeat(max_lineno_width),
                                        caret_offset = " ".repeat(loc.col as usize)
                                    );
                                }
                            });
                        std::process::exit(1);
                    }
                }
            }
            Err(err) => {
                eprintln!("Error: {err}");
                std::process::exit(1);
            }
        }
    }
}

/// Exec script based on given flags
fn exec(source: &Source, debug: bool, emit_bc: bool) -> Result<(Val, Heap)> {
    if emit_bc {
        poco::emit_bc(source)?;
    }
    if debug {
        poco::eval_dbg(source)
    } else {
        poco::eval(source)
    }
}
