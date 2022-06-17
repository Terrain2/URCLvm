mod instructions;
use instructions::*;

use std::{collections::HashMap, fs, iter, path::PathBuf};

use byteorder::{BigEndian, WriteBytesExt};
use clap::Parser;
use tree_sitter::{Node, Tree};

#[derive(Parser, Debug)]
struct Args {
    #[clap(short, long = "input-file")]
    input: PathBuf,

    #[clap(short, long = "output-file")]
    output: PathBuf,

    #[clap(long)]
    sourcemap: Option<PathBuf>,
}

pub trait NodeExt {
    fn text<'a>(&self, source: &'a str) -> &'a str;
    fn field(&self, name: &str) -> Self;
}

impl NodeExt for Node<'_> {
    fn text<'a>(&self, source: &'a str) -> &'a str {
        &source[self.byte_range()]
    }

    fn field(&self, name: &str) -> Self {
        self.child_by_field_name(name).unwrap_or_else(|| {
            unreachable!(
                "Badly formatted syntax tree (`{}` node is missing field `{name}` at [{}:{}])",
                self.kind().to_string(),
                self.start_position().row + 1,
                self.start_position().column + 1,
            )
        })
    }
}

struct SourceError {
    range: Option<tree_sitter::Range>,
    message: String,
}

fn main() {
    let args = Args::parse();
    let source = &fs::read_to_string(&args.input).expect("Failed to read input file");

    let mut parser = tree_sitter::Parser::new();
    parser
        .set_language(tree_sitter_urcl::language())
        .expect("Error initializing parser");
    let tree = parser
        .parse(source, None)
        .expect("Error in Parser::parse (unreachable?)");

    let (volatile_reg, minheap, minstack, instructions) = compile(source, &tree).unwrap_or_else(|errors| {
            let max_line_no_width = source.lines().count().to_string().len();
            eprintln!();
            for SourceError { range, message } in errors {
                if let Some(tree_sitter::Range {
                    start_point,
                    end_point,
                    ..
                }) = range
                {
                    if start_point.row == end_point.row {
                        let row = start_point.row + 1;
                        let line = source
                            .lines()
                            .nth(start_point.row)
                            .expect("Error printing errors");
                        let start = start_point.column;
                        let end = end_point.column;
                        let err_pointer: String = iter::repeat(' ')
                            .take(start)
                            .chain(iter::repeat('^'))
                            .take(end)
                            .collect();
                        const SPC: char = ' ';
                        eprintln!("{row:>max_line_no_width$} | {line}");
                        eprintln!("{SPC:>max_line_no_width$}   {err_pointer}");
                    } else {
                        let lines = source
                            .lines()
                            .enumerate()
                            .skip(start_point.row)
                            .take(end_point.row - start_point.row);
                        for (row, line) in lines {
                            let row = row + 1;
                            eprintln!("{row:>max_line_no_width$} | {line}");
                        }
                    }
                }
                eprintln!("{message}");
                eprintln!();
            }
            std::process::exit(1);
    });

    

    let mut output = fs::File::create(args.output).expect("Failed to open output file");
    output
        .write_u16::<BigEndian>(minheap)
        .expect("Failed to write to output file");
    output
        .write_u16::<BigEndian>(minstack)
        .expect("Failed to write to output file");
    let mut sourcemap = args
        .sourcemap
        .map(fs::File::create)
        .map(|f| f.expect("Failed to open sourcemap file"));
    let mut size = 0;
    for (node, inst3) in instructions {
        for inst2 in inst3
            .lower(volatile_reg, size)
            .expect("Error processing instructions after all errors should have been caught.")
        {
            for word in inst2.encode() {
                output
                    .write_u16::<BigEndian>(word)
                    .expect("Failed to write to output file");
                if let Some(ref mut sourcemap) = sourcemap {
                    sourcemap.write_u16::<BigEndian>(node.start_position().row as u16).expect("Failed to write to sourcemap file");
                }
                size += 1;
            }
        }
    }
}

fn compile<'a>(source: &'a str, tree: &'a Tree) -> Result<(Option<Register>, u16, u16, Vec<(Node<'a>, Instruction3op<u16>)>), Vec<SourceError>> {
    let mut errors = Vec::new();
    macro_rules! err {
        (@nopush None, $($arg:tt)*) => {
            err!(@ None, $($arg)*)
        };
        (@nopush $node:expr, $($arg:tt)*) => {
            err!(@ Some($node.range()), $($arg)*)
        };
        (@ $range:expr, $($t:tt)*) => {
            SourceError {
                range: $range,
                message: format!($($t)*),
            }
        };
        (None$(; $value:expr)?, $($t:tt)*) => {{
            errors.push(err!(@nopush None, $($t)*));
            $($value)?
        }};
        ($node:expr$(; $value:expr)?, $($t:tt)*) => {{
            errors.push(err!(@nopush $node, $($t)*));
            $($value)?
        }};
    }

    let dummy = &mut tree.walk();
    let root = tree.root_node();
    let mut minheap = None;
    let mut minstack = None;
    let mut minreg = None;
    for header in root.children_by_field_name("header", dummy) {
        match header.kind() {
            "BITS" => {
                let value_field = header.field("value");
                let value = value_field.text(source).parse::<usize>();
                match value {
                    Ok(value) => {
                        let matches = match header
                            .child_by_field_name("comparison")
                            .map(|node| node.text(source))
                            .unwrap_or("==")
                        {
                            "==" => 16 == value,
                            "<=" => 16 <= value,
                            ">=" => 16 >= value,
                            comp => {
                                unreachable!("Invalid comparison operator in BITS header: {comp}")
                            }
                        };

                        if !matches {
                            err!(header, "URCLvm is only 16-bit for now");
                        }
                    }
                    Err(err) => {
                        err!(value_field, "Invalid BITS literal: {err}");
                    }
                };
            }
            "MINHEAP" => {
                let value_field = header.field("value");
                let value = value_field
                    .text(source)
                    .parse::<u16>()
                    .expect("Invalid MINHEAP value");
                minheap = match minheap {
                    Some(old) => {
                        err!(value_field; Some(old), "Duplicate MINHEAP header: previously set to {old}")
                    }
                    None => Some(value),
                }
            }
            "MINSTACK" => {
                let value_field = header.field("value");
                let value = value_field
                    .text(source)
                    .parse::<u16>()
                    .expect("Invalid MINSTACK value");
                minstack = match minstack {
                    Some(old) => {
                        err!(value_field; Some(old), "Duplicate MINSTACK header: previously set to {old}")
                    }
                    None => Some(value),
                }
            }
            "MINREG" => {
                let value_field = header.field("value");
                let value = value_field
                    .text(source)
                    .parse::<u16>()
                    .expect("Invalid MINREG value");
                if value >= 15 {
                    err!(
                        value_field,
                        "MINREG value is too high; URCLvm only has 14 regs and SP"
                    );
                }
                minreg = match minreg {
                    Some(old) => {
                        err!(value_field; Some(old), "Duplicate MINREG header: previously set to {old}")
                    }
                    None => Some(value),
                }
            }
            kind => err!(header, "Unknown header {kind}"),
        }
    }
    let minheap = minheap.unwrap_or_else(|| err!(None; 0, "Missing MINHEAP header"));
    let minstack = minstack.unwrap_or_else(|| err!(None; 0, "Missing MINSTACK header"));
    let minreg = minreg.unwrap_or(15);
    let cursor = &mut tree.walk();
    if !cursor.goto_first_child() {
        unreachable!("No children of root node");
    }

    let mut lines = Vec::new();
    let mut labels = HashMap::<&str, usize>::new();

    fn imm_literal<'a>(
        source: &'a str,
        len: usize,
    ) -> impl Fn(Node<'a>) -> Result<AnyImmediate<'a>, SourceError> {
        move |node| {
            Ok(match node.kind() {
                "number" => AnyImmediate::Number({
                    let text = node.text(source);
                    let (text, radix) = if text.starts_with("0x") {
                        (&text[2..], 16)
                    } else if text.starts_with("0b") {
                        (&text[2..], 2)
                    } else if text.starts_with("0o") {
                        (&text[2..], 8)
                    } else {
                        (text, 10)
                    };
                    u16::from_str_radix(text, radix)
                        .or_else(|_| i16::from_str_radix(text, radix).map(|x| x as u16))
                        .map_err(|err| err!(@nopush node, "Invalid number literal: {err}"))?
                }),
                "char" => AnyImmediate::Char(node.text(source).chars().nth(1).unwrap()),
                "char_escape" => {
                    AnyImmediate::Char(match node.text(source).chars().nth(2).unwrap() {
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        '\\' => '\\',
                        '\'' => '\'',
                        '0' => '\0',
                        _ => unreachable!("Invalid escape sequence"),
                    })
                }
                "label" => AnyImmediate::Label(node.text(source)),
                "relative" => AnyImmediate::Line(
                    (len as isize
                        + node.text(source)[1..]
                            .parse::<isize>()
                            .map_err(|err| err!(@nopush node, "Invalid relative address: {err}"))?)
                        as usize,
                ),
                "memory" => AnyImmediate::Memory(
                    node.text(source)[1..]
                        .parse()
                        .map_err(|err| err!(@nopush node, "Invalid memory address: {err}"))?,
                ),
                "macro" => AnyImmediate::Macro(node.text(source)),
                "placeholder" => {
                    Err(err!(@nopush node, "Please insert a value instead of a placeholder"))?
                }
                "identifier" => Err(err!(@nopush node, "Identifiers are not supported"))?,
                "register" => unreachable!("Registers should not appear here"),
                "port" => unreachable!("Ports should not appear here"),
                kind => unreachable!("Unknown operand kind `{kind}`"),
            })
        }
    }

    let mut volatile_reg = None;

    loop {
        let imm_literal = imm_literal(source, lines.len());
        match cursor.node().kind() {
            "DW" => {
                for label in cursor.node().children_by_field_name("label", dummy) {
                    labels.insert(label.text(source), lines.len());
                }
                let value = cursor.node().field("value");
                let values = match value.kind() {
                    "array" => value
                        .children_by_field_name("item", dummy)
                        .map(|value| imm_literal(value).map(|imm| (value, imm)))
                        .collect(),
                    "string" => vec![Err(err!(@nopush value, "DW strings are not supported"))],
                    _ => vec![imm_literal(value).map(|imm| (value, imm))],
                };
                lines.push((
                    cursor.node(),
                    Instruction3op::DW(
                        values
                            .into_iter()
                            .filter_map(|imm| {
                                imm.map_or_else(
                                    |err| {
                                        errors.push(err);
                                        None
                                    },
                                    Some,
                                )
                            })
                            .collect(),
                    ),
                ));
            }
            "instruction" => {
                for label in cursor.node().children_by_field_name("label", dummy) {
                    labels.insert(label.text(source), lines.len());
                }
                #[derive(Clone, Copy)]
                enum AnyOperand<'a> {
                    Register(Option<Register>),
                    Immediate(AnyImmediate<'a>),
                    Port(Port),
                    Error,
                }
                let operands = cursor
                    .node()
                    .children_by_field_name("operand", dummy)
                    .map(|operand| {
                        (
                            operand,
                            match operand.kind() {
                                "register" => {
                                    AnyOperand::Register(match &operand.text(source)[1..] {
                                        "0" => None,
                                        r => Some(r.try_into().unwrap()),
                                    })
                                }
                                "stack_pointer" => AnyOperand::Register(Some(Register::SP)),
                                "program_counter" => {
                                    err!(operand; AnyOperand::Error, "PC unsupported for now")
                                }
                                "port" => {
                                    AnyOperand::Port(operand.text(source).try_into().unwrap())
                                }
                                // could use filter_map here and have this arm return None
                                // but this preserves operand count, which prevents false-positive incorrect number of args errors
                                _ => imm_literal(operand).map_or_else(
                                    |err| {
                                        errors.push(err);
                                        AnyOperand::Error
                                    },
                                    AnyOperand::Immediate,
                                ),
                            },
                        )
                    })
                    .collect::<Vec<_>>();
                let opcode = cursor.node().field("name").text(source);

                // when AnyOperand::Error, it has already been handled. don't add anything to errors
                macro_rules! match_opcodes {
                    (@operands $operands:ident $($name:ident : $type:ident $($tail:tt)*)?) => {
                        $(
                            match_opcodes!(@operands $operands $($tail)*);
                            let $name = match_opcodes!(@operand $name:$type);
                        )?
                    };
                    (@operand $name:ident : reg) => {
                        match $name {
                            (_, AnyOperand::Register(reg)) => reg,
                            (node, AnyOperand::Immediate(_)) => err!(node; None, "Invalid operand type, expected register, found immediate"),
                            (node, AnyOperand::Port(_)) => err!(node; None, "Invalid operand type, expected register, found port"),
                            (_, AnyOperand::Error) => None,
                        }
                    };
                    (@operand $name:ident : imm) => {
                        match $name {
                            (node, AnyOperand::Register(_)) => err!(node; (node, AnyImmediate::Number(0)), "Invalid operand type, expected immediate, found register"),
                            (node, AnyOperand::Immediate(imm)) => (node, imm),
                            (node, AnyOperand::Port(_)) => err!(node; (node, AnyImmediate::Number(0)), "Invalid operand type, expected immediate, found port"),
                            (node, AnyOperand::Error) => (node, AnyImmediate::Number(0)),
                        }
                    };
                    (@operand $name:ident : port) => {
                        // fuck it choose some port as the default for errors lol
                        match $name {
                            (node, AnyOperand::Register(_)) => err!(node; Port::TEXT, "Invalid operand type, expected port, found register"),
                            (node, AnyOperand::Immediate(_)) => err!(node; Port::TEXT, "Invalid operand type, expected port, found immediate"),
                            (_, AnyOperand::Port(port)) => port,
                            (_, AnyOperand::Error) => Port::TEXT,
                        }
                    };
                    (@operand $name:ident : val) => {
                        match $name {
                            (_, AnyOperand::Register(reg)) => Value::Register(reg),
                            (node, AnyOperand::Immediate(imm)) => Value::Immediate((node, imm)),
                            (node, AnyOperand::Port(_)) => err!(node; Value::Register(None), "Invalid operand type, expected register or immediate, found port"),
                            (_, AnyOperand::Error) => Value::Register(None),
                        }
                    };
                    ($($opcode:ident$(($($name:ident : $type:ident),*))?,)*) => {
                        match (opcode, operands.as_slice()) {
                            ("@volatile", &[(_, AnyOperand::Register(Some(reg)))]) => {
                                volatile_reg = Some(reg);
                                None
                            }
                            $(
                                (stringify!($opcode), &[$($($name),*)?]) => {
                                    $(
                                    match_opcodes!(@operands operands $($name:$type)*);
                                    )?
                                    Some(Instruction3op::$opcode$(($($name),*))?)
                                }
                                (stringify!($opcode), &[..]) => {
                                    err!(cursor.node(); None, "Invalid number of operands for opcode `{opcode}`")
                                }
                            )*
                            (opcode, _) => {
                                err!(cursor.node(); None, "Unknown opcode `{opcode}`")
                            }
                        }
                    };
                }
                instructions3!(match_opcodes).map(|inst| lines.push((cursor.node(), inst)));
            }
            _ => (),
        }
        if !cursor.goto_next_sibling() {
            break;
        }
    }

    let volatile_reg = volatile_reg;

    let (line_offsets, total_size) = {
        let mut line_offsets = Vec::with_capacity(lines.len());
        let mut total_size = 0;

        for (node, inst) in lines.iter().cloned() {
            match inst.map(|_| 0).lower(volatile_reg, total_size) {
                Ok(insts) => {
                    line_offsets.push(total_size);
                    total_size += insts
                        .into_iter()
                        .map(|inst| inst.encode().len())
                        .sum::<usize>() as u16;
                }
                Err(err) => {
                    errors.push(SourceError {
                        range: Some(node.range()),
                        message: err.to_string(),
                    });
                }
            }
        }

        (line_offsets, total_size)
    };

    let instructions = lines
        .into_iter()
        .map(|(node, inst)| {
            (node, inst.map(|(node, imm)| match imm {
                AnyImmediate::Macro("@BITS") => u16::BITS as u16,
                AnyImmediate::Macro("@MINHEAP") => minheap,
                AnyImmediate::Macro("@MINSTACK") => minstack,
                AnyImmediate::Macro("@MINREG") => minreg,
                AnyImmediate::Macro("@HEAP") => minheap + minstack,
                AnyImmediate::Macro("@MAX") => u16::MAX,
                AnyImmediate::Macro("@SMAX") => i16::MAX as u16,
                AnyImmediate::Macro("@MSB") => i16::MIN as u16,
                AnyImmediate::Macro("@SMSB") => (i16::MIN as u16) >> 1,
                AnyImmediate::Macro("@UHALF") => (u8::MAX as u16) << u8::BITS,
                AnyImmediate::Macro("@LHALF") => u8::MAX as u16,
                AnyImmediate::Macro(other) => err!(node; 0, "Unknown macro {other}"),
                AnyImmediate::Number(num) => num,
                AnyImmediate::Char(ch) => (ch as u32).try_into().unwrap_or_else(|err| err!(node; 0, "Invalid character literal '{ch}'; does not fit in u16: {err}")),
                AnyImmediate::Line(idx) => {
                    if idx < line_offsets.len() {
                        line_offsets[idx] as u16
                    } else {
                        err!(node; 0, "Relative line reference resolved to out of bounds line `{idx}`")
                    }
                }
                AnyImmediate::Label(label) => {
                    labels
                        .get(label)
                        .map(|&idx| line_offsets[idx] as u16)
                        .unwrap_or_else(|| err!(node; 0, "Undefined label `{label}`"))
                }
                AnyImmediate::Memory(heap) => total_size as u16 + heap,
            }))
        })
        .collect::<Vec<_>>();
    if errors.is_empty() {
        Ok((volatile_reg, minheap, minstack, instructions))
    } else {
        Err(errors)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
#[repr(u16)]
pub enum Register {
    SP,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl TryFrom<&str> for Register {
    type Error = Option<String>;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let idx = value
            .parse::<u16>()
            .expect("Failed to parse register index");
        match idx {
            0 => Err(None),
            1 => Ok(Register::R1),
            2 => Ok(Register::R2),
            3 => Ok(Register::R3),
            4 => Ok(Register::R4),
            5 => Ok(Register::R5),
            6 => Ok(Register::R6),
            7 => Ok(Register::R7),
            8 => Ok(Register::R8),
            9 => Ok(Register::R9),
            10 => Ok(Register::R10),
            11 => Ok(Register::R11),
            12 => Ok(Register::R12),
            13 => Ok(Register::R13),
            14 => Ok(Register::R14),
            15 => Ok(Register::R15),
            16.. => Err(Some(format!(
                "Register ${idx} does not exist in URCLvm (only $0..$14 and SP)"
            ))),
        }
    }
}

impl Into<u16> for Register {
    fn into(self) -> u16 {
        self as _
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum AnyImmediate<'a> {
    Number(u16),
    Char(char),
    Macro(&'a str),
    Label(&'a str),
    Line(usize),
    Memory(u16),
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Value<Reg, Imm> {
    Register(Reg),
    Immediate(Imm),
}

impl<Reg, Imm> Value<Reg, Imm> {
    fn map<I>(self, f: &mut impl FnMut(Imm) -> I) -> Value<Reg, I> {
        match self {
            Value::Register(r) => Value::Register(r),
            Value::Immediate(i) => Value::Immediate(f(i)),
        }
    }
}

#[derive(Clone, Copy)]
#[repr(u16)]
pub enum Port {
    // General
    CPUBUS = 0,
    TEXT = 1,
    NUMB = 2,
    SUPPORTED = 5,
    SPECIAL = 6,
    PROFILE = 7,
    // Graphics
    X = 8,
    Y = 9,
    COLOR = 10,
    BUFFER = 11,
    GSPECIAL = 15,
    // Text
    ASCII8 = 16,
    CHAR5 = 17,
    CHAR6 = 18,
    ASCII7 = 19,
    UTF8 = 20,
    TSPECIAL = 23,
    // Numbers
    INT = 24,
    UINT = 25,
    BIN = 26,
    HEX = 27,
    FLOAT = 28,
    FIXED = 29,
    NSPECIAL = 31,
    // Storage
    ADDR = 32,
    BUS = 33,
    PAGE = 34,
    SSPECIAL = 39,
    // Miscellaneous
    RNG = 40,
    NOTE = 41,
    INSTR = 42,
    NLEG = 43,
    WAIT = 44,
    NADDR = 45,
    DATA = 46,
    MSPECIAL = 47,
    // User defined
    UD1 = 48,
    UD2 = 49,
    UD3 = 50,
    UD4 = 51,
    UD5 = 52,
    UD6 = 53,
    UD7 = 54,
    UD8 = 55,
    UD9 = 56,
    UD10 = 57,
    UD11 = 58,
    UD12 = 59,
    UD13 = 60,
    UD14 = 61,
    UD15 = 62,
    UD16 = 63,
}

impl Port {
    fn upper(self) -> u16 {
        self as u16 >> 4
    }

    fn lower(self) -> u16 {
        self as u16 & 0b1111
    }
}

impl TryFrom<&str> for Port {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "%CPUBUS" => Ok(Port::CPUBUS),
            "%TEXT" => Ok(Port::TEXT),
            "%NUMB" => Ok(Port::NUMB),
            "%SUPPORTED" => Ok(Port::SUPPORTED),
            "%SPECIAL" => Ok(Port::SPECIAL),
            "%PROFILE" => Ok(Port::PROFILE),
            "%X" => Ok(Port::X),
            "%Y" => Ok(Port::Y),
            "%COLOR" | "%COLOUR" => Ok(Port::COLOR),
            "%BUFFER" => Ok(Port::BUFFER),
            "%GSPECIAL" => Ok(Port::GSPECIAL),
            "%ASCII8" => Ok(Port::ASCII8),
            "%CHAR5" => Ok(Port::CHAR5),
            "%CHAR6" => Ok(Port::CHAR6),
            "%ASCII7" => Ok(Port::ASCII7),
            "%UTF8" => Ok(Port::UTF8),
            "%TSPECIAL" => Ok(Port::TSPECIAL),
            "%INT" => Ok(Port::INT),
            "%UINT" => Ok(Port::UINT),
            "%BIN" => Ok(Port::BIN),
            "%HEX" => Ok(Port::HEX),
            "%FLOAT" => Ok(Port::FLOAT),
            "%FIXED" => Ok(Port::FIXED),
            "%NSPECIAL" => Ok(Port::NSPECIAL),
            "%ADDR" => Ok(Port::ADDR),
            "%BUS" => Ok(Port::BUS),
            "%PAGE" => Ok(Port::PAGE),
            "%SSPECIAL" => Ok(Port::SSPECIAL),
            "%RNG" => Ok(Port::RNG),
            "%NOTE" => Ok(Port::NOTE),
            "%INSTR" => Ok(Port::INSTR),
            "%NLEG" => Ok(Port::NLEG),
            "%WAIT" => Ok(Port::WAIT),
            "%NADDR" => Ok(Port::NADDR),
            "%DATA" => Ok(Port::DATA),
            "%MSPECIAL" => Ok(Port::MSPECIAL),
            "%UD1" => Ok(Port::UD1),
            "%UD2" => Ok(Port::UD2),
            "%UD3" => Ok(Port::UD3),
            "%UD4" => Ok(Port::UD4),
            "%UD5" => Ok(Port::UD5),
            "%UD6" => Ok(Port::UD6),
            "%UD7" => Ok(Port::UD7),
            "%UD8" => Ok(Port::UD8),
            "%UD9" => Ok(Port::UD9),
            "%UD10" => Ok(Port::UD10),
            "%UD11" => Ok(Port::UD11),
            "%UD12" => Ok(Port::UD12),
            "%UD13" => Ok(Port::UD13),
            "%UD14" => Ok(Port::UD14),
            "%UD15" => Ok(Port::UD15),
            "%UD16" => Ok(Port::UD16),
            _ => Err(format!("Unknown port {value}")),
        }
    }
}
