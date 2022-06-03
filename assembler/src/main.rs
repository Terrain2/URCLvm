use std::{borrow::Cow, collections::HashMap, fmt, fs};

use byteorder::{BigEndian, WriteBytesExt};
use clap::Parser;
use tree_sitter::{Node, Point};

macro_rules! instructions {
    ($callback:ident$(!($($head:tt)*))?) => {
        $callback! { $($($head)*;)?
            NOP,
            HLT,

            IN(dest:reg, src:port),
            OUT(dest:port, src:val),

            CAL(dest:val),
            RET,
            PSH(src:val),
            POP(dest:reg),

            LOD(dest:reg, src:val),
            LLOD(dest:reg, src1:val, src2:val),
            STR(dest:val, src:val),
            LSTR(dest:val, src1:val, src2:val),
            CPY(dest:val, src:val),

            IMM(dest:reg, src:imm),
            MOV(dest:reg, src:reg),

            ADD(dest:reg, src1:val, src2:val),
            SUB(dest:reg, src1:val, src2:val),

            INC(dest:reg, src:val),
            DEC(dest:reg, src:val),
            NEG(dest:reg, src:val),
            NOT(dest:reg, src:val),

            MLT(dest:reg, src1:val, src2:val),
            DIV(dest:reg, src1:val, src2:val),
            SDIV(dest:reg, src1:val, src2:val),
            MOD(dest:reg, src1:val, src2:val),
            SMOD(dest:reg, src1:val, src2:val),

            AND(dest:reg, src1:val, src2:val),
            NAND(dest:reg, src1:val, src2:val),

            OR(dest:reg, src1:val, src2:val),
            NOR(dest:reg, src1:val, src2:val),

            XOR(dest:reg, src1:val, src2:val),
            XNOR(dest:reg, src1:val, src2:val),

            LSH(dest:reg, src:val),
            RSH(dest:reg, src:val),
            SRS(dest:reg, src:val),

            BSL(dest:reg, src1:val, src2:val),
            BSR(dest:reg, src1:val, src2:val),
            BSS(dest:reg, src1:val, src2:val),

            JMP(dest:val),

            BRG(dest:val, src1:val, src2:val),
            BGE(dest:val, src1:val, src2:val),
            BRL(dest:val, src1:val, src2:val),
            BLE(dest:val, src1:val, src2:val),

            SBRG(dest:val, src1:val, src2:val),
            SBGE(dest:val, src1:val, src2:val),
            SBRL(dest:val, src1:val, src2:val),
            SBLE(dest:val, src1:val, src2:val),

            BRE(dest:val, src1:val, src2:val),
            BNE(dest:val, src1:val, src2:val),
            BRC(dest:val, src1:val, src2:val),
            BNC(dest:val, src1:val, src2:val),

            BRZ(dest:val, src:val),
            BNZ(dest:val, src:val),

            BRP(dest:val, src:val),
            BRN(dest:val, src:val),
            BOD(dest:val, src:val),
            BEV(dest:val, src:val),

            SETG(dest:reg, src1:val, src2:val),
            SETGE(dest:reg, src1:val, src2:val),
            SETL(dest:reg, src1:val, src2:val),
            SETLE(dest:reg, src1:val, src2:val),

            SSETG(dest:reg, src1:val, src2:val),
            SSETGE(dest:reg, src1:val, src2:val),
            SSETL(dest:reg, src1:val, src2:val),
            SSETLE(dest:reg, src1:val, src2:val),

            SETE(dest:reg, src1:val, src2:val),
            SETNE(dest:reg, src1:val, src2:val),
            SETC(dest:reg, src1:val, src2:val),
            SETNC(dest:reg, src1:val, src2:val),
        }
    };
}

#[derive(Parser, Debug)]
struct Args {
    #[clap(short, long = "input-file")]
    input: String,

    #[clap(short, long = "output-file")]
    output: String,
}

#[derive(Clone, Copy, Default)]
pub struct Position {
    pub row: usize,
    pub column: usize,
}

impl From<Point> for Position {
    fn from(p: Point) -> Self {
        Self {
            row: p.row + 1,
            column: p.column + 1,
        }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.row == 0 && self.column == 0 {
            write!(f, "[compiler synthesized]")
        } else {
            write!(f, "[{}:{}]", self.row, self.column)
        }
    }
}

pub trait NodeExt {
    fn pos(&self) -> Position;
    fn end_pos(&self) -> Position;
    fn text<'a>(&self, source: &'a str) -> &'a str;
    fn field(&self, name: &str) -> Self;
}

impl NodeExt for Node<'_> {
    fn pos(&self) -> Position {
        self.start_position().into()
    }

    fn end_pos(&self) -> Position {
        self.end_position().into()
    }

    fn text<'a>(&self, source: &'a str) -> &'a str {
        &source[self.byte_range()]
    }

    fn field(&self, name: &str) -> Self {
        self.child_by_field_name(name).unwrap_or_else(|| {
            unreachable!(
                "Badly formatted syntax tree (`{}` node is missing field `{name}` at {})",
                self.kind().to_string(),
                self.pos()
            )
        })
    }
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
    let dummy = &mut tree.walk();
    let root = tree.root_node();
    let mut minheap = None;
    let mut minstack = None;
    let mut minreg = None;
    for header in root.children_by_field_name("header", dummy) {
        match header.kind() {
            "BITS" => {
                let value = header
                    .field("value")
                    .text(source)
                    .parse::<usize>()
                    .expect("Invalid BITS value");
                match header
                    .child_by_field_name("comparison")
                    .map(|node| node.text(source))
                    .unwrap_or("==")
                {
                    "==" => assert!(16 == value, "URCLvm is only 16-bit for now"),
                    "<=" => assert!(16 <= value, "URCLvm is only 16-bit for now"),
                    ">=" => assert!(16 >= value, "URCLvm is only 16-bit for now"),
                    _ => unreachable!("Invalid comparison operator in BITS header"),
                }
            }
            "MINHEAP" => {
                let value = header
                    .field("value")
                    .text(source)
                    .parse::<u16>()
                    .expect("Invalid MINHEAP value");
                minheap = match minheap {
                    Some(old) => panic!("Duplicate MINHEAP header: {old} and {value}"),
                    None => Some(value),
                }
            }
            "MINSTACK" => {
                let value = header
                    .field("value")
                    .text(source)
                    .parse::<u16>()
                    .expect("Invalid MINSTACK value");
                minstack = match minstack {
                    Some(old) => panic!("Duplicate MINSTACK header: {old} and {value}"),
                    None => Some(value),
                }
            }
            "MINREG" => {
                let value = header
                    .field("value")
                    .text(source)
                    .parse::<u16>()
                    .expect("Invalid MINREG value");
                if value >= 15 {
                    panic!("MINREG value is too high; URCLvm only has 14 regs and SP");
                }
                minreg = match minreg {
                    Some(old) => panic!("Duplicate MINREG header: {old} and {value}"),
                    None => Some(value),
                }
            }
            kind => unreachable!("Unknown header {kind}"),
        }
    }
    let minheap = minheap.expect("Missing MINHEAP header");
    let minstack = minstack.expect("Missing MINSTACK header");
    let minreg = minreg.unwrap_or(15);
    let cursor = &mut tree.walk();
    if !cursor.goto_first_child() {
        unreachable!("No children of root node");
    }

    let mut lines = Vec::<Instruction<AnyImmediate>>::new();
    let mut labels = HashMap::<&str, usize>::new();
    // number
    // char
    // char_escape
    // label
    // relative
    // memory
    // macro
    // port
    // placeholder
    // identifier
    fn imm_literal<'a>(source: &'a str, len: usize) -> impl Fn(Node<'a>) -> AnyImmediate<'a> {
        move |node| match node.kind() {
            "number" => AnyImmediate::Number({
                let text = node.text(source);
                if text.starts_with("0x") {
                    u16::from_str_radix(&text[2..], 16)
                } else if text.starts_with("0b") {
                    u16::from_str_radix(&text[2..], 2)
                } else if text.starts_with("0o") {
                    u16::from_str_radix(&text[2..], 8)
                } else {
                    u16::from_str_radix(text, 10)
                }.expect("Invalid number literal")
            }),
            "char" => AnyImmediate::Char(node.text(source).chars().nth(1).unwrap()),
            "char_escape" => AnyImmediate::Char(match node.text(source).chars().nth(2).unwrap() {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\\' => '\\',
                '\'' => '\'',
                '0' => '\0',
                _ => unreachable!("Invalid escape sequence"),
            }),
            "label" => AnyImmediate::Label(node.text(source)),
            "relative" => AnyImmediate::Line(
                (len as isize
                    + node.text(source)[1..]
                        .parse::<isize>()
                        .expect("Invalid relative address")) as usize,
            ),
            "memory" => AnyImmediate::Memory(
                node.text(source)[1..]
                    .parse()
                    .expect("Invalid memory address"),
            ),
            "macro" => AnyImmediate::Macro(node.text(source)),
            "placeholder" => panic!("Placeholder values are not supported"),
            "identifier" => panic!("Identifiers are not supported"),
            "register" => unreachable!("Registers should not appear here"),
            "port" => unreachable!("Ports should not appear here"),
            kind => unreachable!("Unknown operand kind `{kind}`"),
        }
    }

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
                        .map(imm_literal)
                        .collect(),
                    "string" => panic!("DW strings are not supported"),
                    _ => vec![imm_literal(value)],
                };
                lines.push(Instruction::DW(values));
            }
            "instruction" => {
                for label in cursor.node().children_by_field_name("label", dummy) {
                    labels.insert(label.text(source), lines.len());
                }
                #[derive(Clone, Copy)]
                enum AnyOperand<'a> {
                    Register(Register),
                    Immediate(AnyImmediate<'a>),
                    Port(Port),
                }
                let operands = cursor
                    .node()
                    .children_by_field_name("operand", dummy)
                    .map(|operand| match operand.kind() {
                        "register" => {
                            AnyOperand::Register(operand.text(source)[1..].try_into().unwrap())
                        }
                        "stack_pointer" => AnyOperand::Register(Register::SP),
                        "program_counter" => panic!("PC unsupported for now"),
                        "port" => AnyOperand::Port(operand.text(source).try_into().unwrap()),
                        _ => AnyOperand::Immediate(imm_literal(operand)),
                    })
                    .collect::<Vec<_>>();
                let opcode = cursor.node().field("name").text(source);
                macro_rules! match_opcodes {
                    (@operands $operands:ident $($name:ident : $type:ident $($tail:tt)*)?) => {
                        $(
                            match_opcodes!(@operands $operands $($tail)*);
                            let $name = match_opcodes!(@operand $name:$type);
                        )?
                    };
                    (@operand $name:ident : reg) => {
                        match $name {
                            AnyOperand::Register(reg) => reg,
                            AnyOperand::Immediate(_) => panic!("Invalid operand type, expected register, found imm, at {}", cursor.node().pos()),
                            AnyOperand::Port(_) => panic!("Invalid operand type, expected register, found port, at {}", cursor.node().pos()),
                        }
                    };
                    (@operand $name:ident : imm) => {
                        match $name {
                            AnyOperand::Register(_) => panic!("Invalid operand type, expected imm, found register, at {}", cursor.node().pos()),
                            AnyOperand::Immediate(imm) => imm,
                            AnyOperand::Port(_) => panic!("Invalid operand type, expected reg or imm, found port, at {}", cursor.node().pos()),
                        }
                    };
                    (@operand $name:ident : port) => {
                        match $name {
                            AnyOperand::Register(_) => panic!("Invalid operand type, expected port, found reg, at {}", cursor.node().pos()),
                            AnyOperand::Immediate(_) => panic!("Invalid operand type, expected port, found imm, at {}", cursor.node().pos()),
                            AnyOperand::Port(port) => port,
                        }
                    };
                    (@operand $name:ident : val) => {
                        match $name {
                            AnyOperand::Register(reg) => Value::Register(reg),
                            AnyOperand::Immediate(imm) => Value::Immediate(imm),
                            AnyOperand::Port(_) => panic!("Invalid operand type, expected reg or imm, found port, at {}", cursor.node().pos()),
                        }
                    };
                    ($($opcode:ident$(($($name:ident : $type:ident),*))?,)*) => {
                        match (opcode, operands.as_slice()) {
                            $(
                                (stringify!($opcode), &[$($($name),*)?]) => {
                                    $(
                                    match_opcodes!(@operands operands $($name:$type)*);
                                    )?
                                    Instruction::$opcode$(($($name),*))?
                                }
                                (stringify!($opcode), &[..]) => {
                                    panic!(concat!("Invalid number of operands for opcode `", stringify!($opcode), "` at {}"), cursor.node().pos());
                                }
                            )*
                            (opcode, _) => {
                                panic!("Unknown opcode `{opcode}` at {}", cursor.node().pos());
                            }
                        }
                    };
                }
                lines.push(instructions!(match_opcodes));
            }
            _ => (),
        }
        if !cursor.goto_next_sibling() {
            break;
        }
    }

    let (line_offsets, total_size) = {
        let mut line_offsets = Vec::with_capacity(lines.len());
        let mut offset = 0;

        for i in 0..lines.len() {
            offset += lines[i].len();
            line_offsets.push(offset);
        }

        (line_offsets, offset)
    };

    let instructions = lines
        .into_iter()
        .map(|inst| {
            inst.map(|imm| match imm {
                AnyImmediate::Macro("@BITS") => u16::BITS as u16,
                AnyImmediate::Macro("@MINHEAP") => minheap,
                AnyImmediate::Macro("@MINSTACK") => minstack,
                AnyImmediate::Macro("@MINREG") => minreg,
                AnyImmediate::Macro("@MAX") => u16::MAX,
                AnyImmediate::Macro("@SMAX") => i16::MAX as u16,
                AnyImmediate::Macro("@MSB") => i16::MIN as u16,
                AnyImmediate::Macro("@SMSB") => (i16::MIN as u16) >> 1,
                AnyImmediate::Macro("@UHALF") => (u8::MAX as u16) << u8::BITS,
                AnyImmediate::Macro("@LHALF") => u8::MAX as u16,
                AnyImmediate::Macro(other) => panic!("Unknown macro {other}"),
                AnyImmediate::Number(num) => num,
                AnyImmediate::Char(ch) => (ch as u32).try_into().unwrap_or_else(|err| {
                    panic!("Invalid character literal '{ch}'; does not fit in u16: {err}")
                }),
                AnyImmediate::Line(idx) => {
                    if idx >= line_offsets.len() {
                        panic!("Relative line reference resolved to out of bounds line `{idx}`");
                    }
                    line_offsets[idx] as u16
                }
                AnyImmediate::Label(label) => {
                    *labels.get(label).unwrap_or_else(|| panic!("Unknown label `{label}`")) as u16
                }
                AnyImmediate::Memory(heap) => total_size as u16 + heap,
            })
        })
        .collect::<Vec<_>>();

    let mut output = fs::File::create(args.output).expect("Failed to open output file");
    output
        .write_u16::<BigEndian>(minheap)
        .expect("Failed to write to output file");
    output
        .write_u16::<BigEndian>(minstack)
        .expect("Failed to write to output file");
    for inst in instructions {
        for &word in inst.encode().as_ref() {
            output
                .write_u16::<BigEndian>(word)
                .expect("Failed to write to output file");
        }
    }
}

#[derive(Clone, Copy)]
#[repr(u16)]
enum Register {
    R0,
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
    SP,
}

impl TryFrom<&str> for Register {
    type Error = String;

    fn try_from(value: &str) -> Result<Self, Self::Error> {
        let idx = value
            .parse::<u16>()
            .expect("Failed to parse register index");
        match idx {
            0 => Ok(Register::R0),
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
            15.. => Err(format!(
                "Register ${idx} does not exist in URCLvm (only $0..$14 and SP)"
            )),
        }
    }
}

impl Into<u16> for Register {
    fn into(self) -> u16 {
        self as _
    }
}

#[derive(Clone, Copy)]
enum AnyImmediate<'a> {
    Number(u16),
    Char(char),
    Macro(&'a str),
    Label(&'a str),
    Line(usize),
    Memory(u16),
}

#[derive(Clone, Copy)]
enum Value<Imm> {
    Register(Register),
    Immediate(Imm),
}

impl<Imm> Value<Imm> {
    fn map<T, F>(self, f: F) -> Value<T>
    where
        F: FnOnce(Imm) -> T,
    {
        match self {
            Value::Register(r) => Value::Register(r),
            Value::Immediate(i) => Value::Immediate(f(i)),
        }
    }

    fn map_ref<T, F>(&self, f: F) -> Value<T>
    where
        F: FnOnce(&Imm) -> T,
    {
        match self {
            Value::Register(r) => Value::Register(*r),
            Value::Immediate(i) => Value::Immediate(f(i)),
        }
    }
}

#[derive(Clone, Copy)]
#[repr(u16)]
enum Port {
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

macro_rules! inst_decl {
    (@type $imm:ident val) => {
        Value<$imm>
    };
    (@type $imm:ident imm) => {
        $imm
    };
    (@type $imm:ident reg) => {
        Register
    };
    (@type $imm:ident port) => {
        Port
    };
    ($name:ident<$imm:ident>; $($opcode:ident $(($($_:ident : $type:ident),*))?,)*) => {
        enum $name<$imm> {
            DW(Vec<$imm>),
            $($opcode$(($(inst_decl!(@type $imm $type)),*))?,)*
        }
    }
}
instructions!(inst_decl!(Instruction<Imm>));

impl<Imm> Instruction<Imm> {
    pub fn map<T, F>(self, f: F) -> Instruction<T>
    where
        F: Fn(Imm) -> T,
    {
        macro_rules! map {
            (@map $name:ident : reg) => {
                $name
            };
            (@map $name:ident : imm) => {
                f($name)
            };
            (@map $name:ident : val) => {
                $name.map(&f)
            };
            (@map $name:ident : port) => {
                $name
            };
            ($($opcode:ident $(($($name:ident : $type:ident),*))?,)*) => {
                match self {
                    Instruction::DW(vals) => Instruction::DW(vals.into_iter().map(f).collect()),
                    $(
                        Instruction::$opcode$(($($name),*))? => Instruction::$opcode$(($(map!(@map $name : $type)),*))?,
                    )*
                }
            }
        }
        instructions!(map)
    }

    pub fn map_ref<T, F>(&self, f: F) -> Instruction<T>
    where
        F: Fn(&Imm) -> T,
    {
        macro_rules! map {
            (@map $name:ident : reg) => {
                *$name
            };
            (@map $name:ident : imm) => {
                f($name)
            };
            (@map $name:ident : val) => {
                $name.map_ref(&f)
            };
            (@map $name:ident : port) => {
                *$name
            };
            ($($opcode:ident $(($($name:ident : $type:ident),*))?,)*) => {
                match self {
                    Instruction::DW(vals) => Instruction::DW(vals.iter().map(f).collect()),
                    $(
                        Instruction::$opcode$(($($name),*))? => Instruction::$opcode$(($(map!(@map $name : $type)),*))?,
                    )*
                }
            }
        }
        instructions!(map)
    }

    pub fn len(&self) -> usize {
        self.map_ref(|_| 0).encode().len()
    }
}

impl Instruction<u16> {
    pub fn encode<'a>(&'a self) -> Cow<'a, [u16]> {
        fn pair(a: Register, b: Register) -> u16 {
            (a as u16) << 8 | (b as u16)
        }

        fn pack2<A: Into<u16>, B: Into<u16>>(opcode: u16, a: A, b: B) -> u16 {
            opcode << 8 | a.into() << 4 | b.into()
        }

        fn pack1<T: Into<u16>>(opcode: u16, lower: T) -> u16 {
            opcode << 4 | lower.into()
        }

        macro_rules! reg {
            ($name:ident) => {
                Value::Register($name)
            };
            ($name:ident:!) => {
                $name@(
                    | Register::R0
                    | Register::R1
                    | Register::R2
                    | Register::R3
                    | Register::R4
                    | Register::R5
                    | Register::R6
                    | Register::R7
                    | Register::R8
                    | Register::R9
                    | Register::R10
                    | Register::R11
                    | Register::R12
                    | Register::R13
                    | Register::R14
                    | Register::SP
                )
            };
        }

        macro_rules! imm {
            ($name:ident) => {
                Value::Immediate($name)
            };
            ($name:ident:!) => {
                $name@0u16..=u16::MAX
            };
        }

        macro_rules! port {
            ($name:ident) => {
                $name@(
                    | Port::CPUBUS
                    | Port::TEXT
                    | Port::NUMB
                    | Port::SUPPORTED
                    | Port::SPECIAL
                    | Port::PROFILE
                    | Port::X
                    | Port::Y
                    | Port::COLOR
                    | Port::BUFFER
                    | Port::GSPECIAL
                    | Port::ASCII8
                    | Port::CHAR5
                    | Port::CHAR6
                    | Port::ASCII7
                    | Port::UTF8
                    | Port::TSPECIAL
                    | Port::INT
                    | Port::UINT
                    | Port::BIN
                    | Port::HEX
                    | Port::FLOAT
                    | Port::FIXED
                    | Port::NSPECIAL
                    | Port::ADDR
                    | Port::BUS
                    | Port::PAGE
                    | Port::SSPECIAL
                    | Port::RNG
                    | Port::NOTE
                    | Port::INSTR
                    | Port::NLEG
                    | Port::WAIT
                    | Port::NADDR
                    | Port::DATA
                    | Port::MSPECIAL
                    | Port::UD1
                    | Port::UD2
                    | Port::UD3
                    | Port::UD4
                    | Port::UD5
                    | Port::UD6
                    | Port::UD7
                    | Port::UD8
                    | Port::UD9
                    | Port::UD10
                    | Port::UD11
                    | Port::UD12
                    | Port::UD13
                    | Port::UD14
                    | Port::UD15
                    | Port::UD16
                )
            };
        }

        macro_rules! encode {
            ($($opcode:ident$(($($name:ident : $type:ident$(:$m:tt)?),*))? => [$($e:expr),*],)*) => {
                match self {
                    Self::DW(vals) => vals.into(),
                    Self::NOP => [0b0000_0000].as_ref().into(),
                    Self::HLT => [0b0000_0001].as_ref().into(),
                    Self::RET => [0b0000_0010].as_ref().into(),
                    $(Self::$opcode$(($($type!($name$(:$m)?)),*))? => vec![$($e),*].into(),)*
                }
            };
        }

        // :! suffix after type indicates that the other type is not allowed for that instruction
        // so: CPY(dest:reg) because there is also CPY(dest:imm)
        // but ADD(dest:reg:!) because there is no ADD(dest:imm)
        encode! {
            PSH(src:imm) => [0b_0000_0000_0000_0011, *src],
            STR(dest:imm, src:imm) => [0b_0000_0000_0000_0100, *dest, *src],
            CPY(dest:imm, src:imm) => [0b_0000_0000_0000_0101, *dest, *src],

            CAL(dest:imm) => [0b_0000_0000_0000_0110, *dest],
            JMP(dest:imm) => [0b_0000_0000_0000_0111, *dest],

            BRG(dest:imm, src1:imm, src2:imm) => [0b_0000_0000_0000_1000, *dest, *src1, *src2],
            BGE(dest:imm, src1:imm, src2:imm) => [0b_0000_0000_0000_1001, *dest, *src1, *src2],
            BRL(dest:imm, src1:imm, src2:imm) => [0b_0000_0000_0000_1010, *dest, *src1, *src2],
            BLE(dest:imm, src1:imm, src2:imm) => [0b_0000_0000_0000_1011, *dest, *src1, *src2],

            SBRG(dest:imm, src1:imm, src2:imm) => [0b_0000_0000_0000_1100, *dest, *src1, *src2],
            SBGE(dest:imm, src1:imm, src2:imm) => [0b_0000_0000_0000_1101, *dest, *src1, *src2],
            SBRL(dest:imm, src1:imm, src2:imm) => [0b_0000_0000_0000_1110, *dest, *src1, *src2],
            SBLE(dest:imm, src1:imm, src2:imm) => [0b_0000_0000_0000_1111, *dest, *src1, *src2],

            BRE(dest:imm, src1:imm, src2:imm) => [0b_0000_0000_0001_0000, *dest, *src1, *src2],
            BNE(dest:imm, src1:imm, src2:imm) => [0b_0000_0000_0001_0001, *dest, *src1, *src2],
            BRC(dest:imm, src1:imm, src2:imm) => [0b_0000_0000_0001_0010, *dest, *src1, *src2],
            BNC(dest:imm, src1:imm, src2:imm) => [0b_0000_0000_0001_0011, *dest, *src1, *src2],

            BRZ(dest:imm, src:imm) => [0b_0000_0000_0001_0100, *dest, *src],
            BNZ(dest:imm, src:imm) => [0b_0000_0000_0001_0101, *dest, *src],

            BRP(dest:imm, src:imm) => [0b_0000_0000_0001_0110, *dest, *src],
            BRN(dest:imm, src:imm) => [0b_0000_0000_0001_0111, *dest, *src],
            BOD(dest:imm, src:imm) => [0b_0000_0000_0001_1000, *dest, *src],
            BEV(dest:imm, src:imm) => [0b_0000_0000_0001_1001, *dest, *src],

            LSTR(dest:imm, src1:imm, src2:imm) => [0b_0000_0001_1010, *dest, *src1, *src2],

            BRG(dest:reg, src1:reg, src2:reg) => [pack1(0b_0000_0001_0000, *dest), pair(*src1, *src2)],
            BRG(dest:reg, src1:imm, src2:imm) => [pack1(0b_0000_0001_0001, *dest), *src1, *src2],
            BRG(dest:imm, src1:reg, src2:imm) => [pack1(0b_0000_0001_0010, *src1), *dest, *src2],
            BRG(dest:imm, src1:imm, src2:reg) => [pack1(0b_0000_0001_0011, *src2), *dest, *src1],

            BGE(dest:reg, src1:reg, src2:reg) => [pack1(0b_0000_0001_0100, *dest), pair(*src1, *src2)],
            BGE(dest:reg, src1:imm, src2:imm) => [pack1(0b_0000_0001_0101, *dest), *src1, *src2],
            BGE(dest:imm, src1:reg, src2:imm) => [pack1(0b_0000_0001_0110, *src1), *dest, *src2],
            BGE(dest:imm, src1:imm, src2:reg) => [pack1(0b_0000_0001_0111, *src2), *dest, *src1],

            BRL(dest:reg, src1:reg, src2:reg) => [pack1(0b_0000_0001_1000, *dest), pair(*src1, *src2)],
            BRL(dest:reg, src1:imm, src2:imm) => [pack1(0b_0000_0001_1001, *dest), *src1, *src2],
            BRL(dest:imm, src1:reg, src2:imm) => [pack1(0b_0000_0001_1010, *src1), *dest, *src2],
            BRL(dest:imm, src1:imm, src2:reg) => [pack1(0b_0000_0001_1011, *src2), *dest, *src1],

            BLE(dest:reg, src1:reg, src2:reg) => [pack1(0b_0000_0001_1100, *dest), pair(*src1, *src2)],
            BLE(dest:reg, src1:imm, src2:imm) => [pack1(0b_0000_0001_1101, *dest), *src1, *src2],
            BLE(dest:imm, src1:reg, src2:imm) => [pack1(0b_0000_0001_1110, *src1), *dest, *src2],
            BLE(dest:imm, src1:imm, src2:reg) => [pack1(0b_0000_0001_1111, *src2), *dest, *src1],

            SBRG(dest:reg, src1:reg, src2:reg) => [pack1(0b_0000_0010_0000, *dest), pair(*src1, *src2)],
            SBRG(dest:reg, src1:imm, src2:imm) => [pack1(0b_0000_0010_0001, *dest), *src1, *src2],
            SBRG(dest:imm, src1:reg, src2:imm) => [pack1(0b_0000_0010_0010, *src1), *dest, *src2],
            SBRG(dest:imm, src1:imm, src2:reg) => [pack1(0b_0000_0010_0011, *src2), *dest, *src1],

            SBGE(dest:reg, src1:reg, src2:reg) => [pack1(0b_0000_0010_0100, *dest), pair(*src1, *src2)],
            SBGE(dest:reg, src1:imm, src2:imm) => [pack1(0b_0000_0010_0101, *dest), *src1, *src2],
            SBGE(dest:imm, src1:reg, src2:imm) => [pack1(0b_0000_0010_0110, *src1), *dest, *src2],
            SBGE(dest:imm, src1:imm, src2:reg) => [pack1(0b_0000_0010_0111, *src2), *dest, *src1],

            SBRL(dest:reg, src1:reg, src2:reg) => [pack1(0b_0000_0010_1000, *dest), pair(*src1, *src2)],
            SBRL(dest:reg, src1:imm, src2:imm) => [pack1(0b_0000_0010_1001, *dest), *src1, *src2],
            SBRL(dest:imm, src1:reg, src2:imm) => [pack1(0b_0000_0010_1010, *src1), *dest, *src2],
            SBRL(dest:imm, src1:imm, src2:reg) => [pack1(0b_0000_0010_1011, *src2), *dest, *src1],

            SBLE(dest:reg, src1:reg, src2:reg) => [pack1(0b_0000_0010_1100, *dest), pair(*src1, *src2)],
            SBLE(dest:reg, src1:imm, src2:imm) => [pack1(0b_0000_0010_1101, *dest), *src1, *src2],
            SBLE(dest:imm, src1:reg, src2:imm) => [pack1(0b_0000_0010_1110, *src1), *dest, *src2],
            SBLE(dest:imm, src1:imm, src2:reg) => [pack1(0b_0000_0010_1111, *src2), *dest, *src1],

            BRE(dest:reg, src1:reg, src2:reg) => [pack1(0b_0000_0011_0000, *dest), pair(*src1, *src2)],
            BRE(dest:reg, src1:imm, src2:imm) => [pack1(0b_0000_0011_0001, *dest), *src1, *src2],
            BRE(dest:imm, src1:reg, src2:imm) => [pack1(0b_0000_0011_0010, *src1), *dest, *src2],
            BRE(dest:imm, src1:imm, src2:reg) => [pack1(0b_0000_0011_0011, *src2), *dest, *src1],

            BNE(dest:reg, src1:reg, src2:reg) => [pack1(0b_0011_0100, *dest), pair(*src1, *src2)],
            BNE(dest:reg, src1:imm, src2:imm) => [pack1(0b_0011_0101, *dest), *src1, *src2],
            BNE(dest:imm, src1:reg, src2:imm) => [pack1(0b_0011_0110, *src1), *dest, *src2],
            BNE(dest:imm, src1:imm, src2:reg) => [pack1(0b_0011_0111, *src2), *dest, *src1],

            BRC(dest:reg, src1:reg, src2:reg) => [pack1(0b_0011_1000, *dest), pair(*src1, *src2)],
            BRC(dest:reg, src1:imm, src2:imm) => [pack1(0b_0011_1001, *dest), *src1, *src2],
            BRC(dest:imm, src1:reg, src2:imm) => [pack1(0b_0011_1010, *src1), *dest, *src2],
            BRC(dest:imm, src1:imm, src2:reg) => [pack1(0b_0011_1011, *src2), *dest, *src1],

            BNC(dest:reg, src1:reg, src2:reg) => [pack1(0b_0011_1100, *dest), pair(*src1, *src2)],
            BNC(dest:reg, src1:imm, src2:imm) => [pack1(0b_0011_1101, *dest), *src1, *src2],
            BNC(dest:imm, src1:reg, src2:imm) => [pack1(0b_0011_1110, *src1), *dest, *src2],
            BNC(dest:imm, src1:imm, src2:reg) => [pack1(0b_0011_1111, *src2), *dest, *src1],

            BRP(dest:reg, src:imm) => [pack1(0b_0011_0000, *dest), *src],
            BRP(dest:imm, src:reg) => [pack1(0b_0011_0001, *src), *dest],
            BRN(dest:reg, src:imm) => [pack1(0b_0011_0010, *dest), *src],
            BRN(dest:imm, src:reg) => [pack1(0b_0011_0011, *src), *dest],

            BRZ(dest:reg, src:imm) => [pack1(0b_0011_0100, *dest), *src],
            BRZ(dest:imm, src:reg) => [pack1(0b_0011_0101, *src), *dest],
            BNZ(dest:reg, src:imm) => [pack1(0b_0011_0110, *dest), *src],
            BNZ(dest:imm, src:reg) => [pack1(0b_0011_0111, *src), *dest],

            ADD(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_0100_1000, *dest), pair(*src1, *src2)],
            ADD(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_0100_1001, *dest), *src1, *src2],
            SUB(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_0100_1010, *dest), pair(*src1, *src2)],
            SUB(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_0100_1011, *dest), *src1, *src2],

            INC(dest:reg:!, src:imm) => [pack1(0b_0000_0100_1100, *dest), *src],
            DEC(dest:reg:!, src:imm) => [pack1(0b_0000_0100_1101, *dest), *src],
            NEG(dest:reg:!, src:imm) => [pack1(0b_0000_0100_1110, *dest), *src],
            NOT(dest:reg:!, src:imm) => [pack1(0b_0000_0100_1111, *dest), *src],

            CPY(dest:reg, src:imm) => [pack1(0b_0000_0101_0000, *dest), *src],
            CPY(dest:imm, src:reg) => [pack1(0b_0000_0101_0001, *src), *dest],

            MLT(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_0101_0010, *dest), pair(*src1, *src2)],
            MLT(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_0101_0011, *dest), *src1, *src2],

            DIV(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_0101_0100, *dest), pair(*src1, *src2)],
            DIV(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_0101_0101, *dest), *src1, *src2],
            SDIV(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_0101_0110, *dest), pair(*src1, *src2)],
            SDIV(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_0101_0111, *dest), *src1, *src2],

            MOD(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_0101_1000, *dest), pair(*src1, *src2)],
            MOD(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_0101_1001, *dest), *src1, *src2],
            SMOD(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_0101_1010, *dest), pair(*src1, *src2)],
            SMOD(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_0101_1011, *dest), *src1, *src2],

            AND(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_0101_1100, *dest), pair(*src1, *src2)],
            AND(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_0101_1101, *dest), *src1, *src2],
            NAND(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_0101_1110, *dest), pair(*src1, *src2)],
            NAND(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_0101_1111, *dest), *src1, *src2],

            OR(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_0110_0000, *dest), pair(*src1, *src2)],
            OR(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_0110_0001, *dest), *src1, *src2],
            NOR(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_0110_0010, *dest), pair(*src1, *src2)],
            NOR(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_0110_0011, *dest), *src1, *src2],

            XOR(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_0110_0100, *dest), pair(*src1, *src2)],
            XOR(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_0110_0101, *dest), *src1, *src2],
            XNOR(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_0110_0110, *dest), pair(*src1, *src2)],
            XNOR(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_0110_0111, *dest), *src1, *src2],

            LLOD(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_0110_1000, *dest), pair(*src1, *src2)],
            LLOD(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_0110_1001, *dest), *src1, *src2],

            IMM(dest:reg:!, src:imm:!) => [pack1(0b_0000_0110_1010, *dest), *src],

            RSH(dest:reg:!, src:imm) => [pack1(0b_0000_0110_1011, *dest), *src],
            LSH(dest:reg:!, src:imm) => [pack1(0b_0000_0110_1100, *dest), *src],
            SRS(dest:reg:!, src:imm) => [pack1(0b_0000_0110_1101, *dest), *src],

            BSR(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_0110_1110, *dest), pair(*src1, *src2)],
            BSR(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_0110_1111, *dest), *src1, *src2],
            BSL(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_0111_0000, *dest), pair(*src1, *src2)],
            BSL(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_0111_0001, *dest), *src1, *src2],
            BSS(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_0111_0010, *dest), pair(*src1, *src2)],
            BSS(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_0111_0011, *dest), *src1, *src2],

            SETG(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_0111_0100, *dest), pair(*src1, *src2)],
            SETG(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_0111_0101, *dest), *src1, *src2],
            SETGE(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_0111_0110, *dest), pair(*src1, *src2)],
            SETGE(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_0111_0111, *dest), *src1, *src2],

            SETL(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_0111_1000, *dest), pair(*src1, *src2)],
            SETL(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_0111_1001, *dest), *src1, *src2],
            SETLE(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_0111_1010, *dest), pair(*src1, *src2)],
            SETLE(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_0111_1011, *dest), *src1, *src2],

            SETE(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_0111_1100, *dest), pair(*src1, *src2)],
            SETE(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_0111_1101, *dest), *src1, *src2],
            SETNE(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_0111_1110, *dest), pair(*src1, *src2)],
            SETNE(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_0111_1111, *dest), *src1, *src2],

            SETC(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_1000_0000, *dest), pair(*src1, *src2)],
            SETC(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_1000_0001, *dest), *src1, *src2],
            SETNC(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_1000_0010, *dest), pair(*src1, *src2)],
            SETNC(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_1000_0011, *dest), *src1, *src2],

            SSETG(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_1000_0100, *dest), pair(*src1, *src2)],
            SSETG(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_1000_0101, *dest), *src1, *src2],
            SSETGE(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_1000_0110, *dest), pair(*src1, *src2)],
            SSETGE(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_1000_0111, *dest), *src1, *src2],

            SSETL(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_1000_1000, *dest), pair(*src1, *src2)],
            SSETL(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_1000_1001, *dest), *src1, *src2],
            SSETLE(dest:reg:!, src1:reg, src2:reg) => [pack1(0b_0000_1000_1010, *dest), pair(*src1, *src2)],
            SSETLE(dest:reg:!, src1:imm, src2:imm) => [pack1(0b_0000_1000_1011, *dest), *src1, *src2],

            LSTR(dest:reg, src1:reg, src2:reg) => [pack1(0b_0000_1000_1100, *dest), pair(*src1, *src2)],
            LSTR(dest:reg, src1:imm, src2:imm) => [pack1(0b_0000_1000_1101, *dest), *src1, *src2],
            LSTR(dest:imm, src1:reg, src2:imm) => [pack1(0b_0000_1000_1110, *src1), *dest, *src2],
            LSTR(dest:imm, src1:imm, src2:reg) => [pack1(0b_0000_1000_1111, *src2), *dest, *src1],

            PSH(src:reg) => [pack1(0b_0000_1001_0000, *src)],
            POP(dest:reg:!) => [pack1(0b_0000_1001_0001, *dest)],

            CAL(dest:reg) => [pack1(0b_0000_1001_0010, *dest)],
            JMP(dest:reg) => [pack1(0b_0000_1001_0011, *dest)],

            LOD(dest:reg:!, src:imm) => [pack1(0b_0000_1001_0100, *dest), *src],
            STR(dest:reg, src:imm) => [pack1(0b_0000_1001_0101, *dest), *src],
            STR(dest:imm, src:reg) => [pack1(0b_0000_1001_0110, *src), *dest],

            BOD(dest:reg, src:imm) => [pack1(0b_0000_1001_1000, *dest), *src],
            BOD(dest:imm, src:reg) => [pack1(0b_0000_1001_1001, *src), *dest],
            BEV(dest:reg, src:imm) => [pack1(0b_0000_1001_1010, *dest), *src],
            BEV(dest:imm, src:reg) => [pack1(0b_0000_1001_1011, *src), *dest],

            OUT(dest:port, src:imm) => [pack1(0b_0111_1111_11 << 2 | dest.upper(), dest.lower()), *src],
            IN(dest:reg:!, src:port) => [pack2(0b_1000_00 << 2 | src.upper(), *dest, src.lower())],
            OUT(dest:port, src:reg) => [pack2(0b_1000_01 << 2 | dest.upper(), dest.lower(), *src)],

            LOD(dest:reg:!, src:reg) => [pack2(0b_0000_1000_1000, *dest, *src)],
            STR(dest:reg, src:reg) => [pack2(0b_0000_1000_1001, *dest, *src)],

            LLOD(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1000_1010, *dest, *src1), *src2],
            LLOD(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1000_1011, *dest, *src2), *src1],

            RSH(dest:reg:!, src:reg) => [pack2(0b_1000_1100, *dest, *src)],
            LSH(dest:reg:!, src:reg) => [pack2(0b_1000_1101, *dest, *src)],
            SRS(dest:reg:!, src:reg) => [pack2(0b_1000_1110, *dest, *src)],

            LSTR(dest:reg, src1:reg, src2:imm) => [pack2(0b_1000_1111, *dest, *src1), *src2],
            LSTR(dest:reg, src1:imm, src2:reg) => [pack2(0b_1001_0000, *dest, *src2), *src1],
            LSTR(dest:imm, src1:reg, src2:reg) => [pack2(0b_1001_0001, *src1, *src2), *dest],

            ADD(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1001_0010, *dest, *src1), *src2],
            ADD(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1001_0011, *dest, *src2), *src1],
            SUB(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1001_0100, *dest, *src1), *src2],
            SUB(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1001_0101, *dest, *src2), *src1],

            INC(dest:reg:!, src:reg) => [pack2(0b_1001_0110, *dest, *src)],
            DEC(dest:reg:!, src:reg) => [pack2(0b_1001_0111, *dest, *src)],
            NEG(dest:reg:!, src:reg) => [pack2(0b_1001_1000, *dest, *src)],
            NOT(dest:reg:!, src:reg) => [pack2(0b_1001_1001, *dest, *src)],

            MLT(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1001_1010, *dest, *src1), *src2],
            MLT(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1001_1011, *dest, *src2), *src1],

            DIV(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1001_1100, *dest, *src1), *src2],
            DIV(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1001_1101, *dest, *src2), *src1],
            SDIV(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1001_1110, *dest, *src1), *src2],
            SDIV(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1001_1111, *dest, *src2), *src1],

            MOD(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1010_0000, *dest, *src1), *src2],
            MOD(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1010_0001, *dest, *src2), *src1],
            SMOD(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1010_0010, *dest, *src1), *src2],
            SMOD(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1010_0011, *dest, *src2), *src1],

            AND(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1010_0100, *dest, *src1), *src2],
            AND(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1010_0101, *dest, *src2), *src1],
            NAND(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1010_0110, *dest, *src1), *src2],
            NAND(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1010_0111, *dest, *src2), *src1],

            OR(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1010_1000, *dest, *src1), *src2],
            OR(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1010_1001, *dest, *src2), *src1],
            NOR(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1010_1010, *dest, *src1), *src2],
            NOR(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1010_1011, *dest, *src2), *src1],

            XOR(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1010_1100, *dest, *src1), *src2],
            XOR(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1010_1101, *dest, *src2), *src1],
            XNOR(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1010_1110, *dest, *src1), *src2],
            XNOR(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1010_1111, *dest, *src2), *src1],

            BSL(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1011_0000, *dest, *src1), *src2],
            BSL(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1011_0001, *dest, *src2), *src1],
            BSR(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1011_0010, *dest, *src1), *src2],
            BSR(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1011_0011, *dest, *src2), *src1],
            BSS(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1011_0100, *dest, *src1), *src2],
            BSS(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1011_0101, *dest, *src2), *src1],

            BRC(dest:reg, src1:reg, src2:imm) => [pack2(0b_1011_0110, *dest, *src1), *src2],
            BRC(dest:reg, src1:imm, src2:reg) => [pack2(0b_1011_0111, *dest, *src2), *src1],
            BRC(dest:imm, src1:reg, src2:reg) => [pack2(0b_1011_1000, *src1, *src2), *dest],

            BNC(dest:reg, src1:reg, src2:imm) => [pack2(0b_1011_1001, *dest, *src1), *src2],
            BNC(dest:reg, src1:imm, src2:reg) => [pack2(0b_1011_1010, *dest, *src2), *src1],
            BNC(dest:imm, src1:reg, src2:reg) => [pack2(0b_1011_1011, *src1, *src2), *dest],

            BRE(dest:reg, src1:reg, src2:imm) => [pack2(0b_1011_1100, *dest, *src1), *src2],
            BRE(dest:reg, src1:imm, src2:reg) => [pack2(0b_1011_1101, *dest, *src2), *src1],
            BRE(dest:imm, src1:reg, src2:reg) => [pack2(0b_1011_1110, *src1, *src2), *dest],

            BNE(dest:reg, src1:reg, src2:imm) => [pack2(0b_1011_1111, *dest, *src1), *src2],
            BNE(dest:reg, src1:imm, src2:reg) => [pack2(0b_1100_0000, *dest, *src2), *src1],
            BNE(dest:imm, src1:reg, src2:reg) => [pack2(0b_1100_0001, *src1, *src2), *dest],

            BOD(dest:reg, src:reg) => [pack2(0b_1100_0010, *dest, *src)],
            BEV(dest:reg, src:reg) => [pack2(0b_1100_0011, *dest, *src)],

            BRZ(dest:reg, src:reg) => [pack2(0b_1100_0100, *dest, *src)],
            BNZ(dest:reg, src:reg) => [pack2(0b_1100_0101, *dest, *src)],

            BRP(dest:reg, src:reg) => [pack2(0b_1100_0110, *dest, *src)],
            BRN(dest:reg, src:reg) => [pack2(0b_1100_0111, *dest, *src)],

            SBRG(dest:reg, src1:reg, src2:imm) => [pack2(0b_1100_1000, *dest, *src1), *src2],
            SBRG(dest:reg, src1:imm, src2:reg) => [pack2(0b_1100_1001, *dest, *src2), *src1],
            SBRG(dest:imm, src1:reg, src2:reg) => [pack2(0b_1100_1010, *src1, *src2), *dest],

            SBGE(dest:reg, src1:reg, src2:imm) => [pack2(0b_1100_1011, *dest, *src1), *src2],
            SBGE(dest:reg, src1:imm, src2:reg) => [pack2(0b_1100_1100, *dest, *src2), *src1],
            SBGE(dest:imm, src1:reg, src2:reg) => [pack2(0b_1100_1101, *src1, *src2), *dest],

            SBRL(dest:reg, src1:reg, src2:imm) => [pack2(0b_1100_1110, *dest, *src1), *src2],
            SBRL(dest:reg, src1:imm, src2:reg) => [pack2(0b_1100_1111, *dest, *src2), *src1],
            SBRL(dest:imm, src1:reg, src2:reg) => [pack2(0b_1101_0000, *src1, *src2), *dest],

            SBLE(dest:reg, src1:reg, src2:imm) => [pack2(0b_1101_0001, *dest, *src1), *src2],
            SBLE(dest:reg, src1:imm, src2:reg) => [pack2(0b_1101_0010, *dest, *src2), *src1],
            SBLE(dest:imm, src1:reg, src2:reg) => [pack2(0b_1101_0011, *src1, *src2), *dest],

            SETE(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1101_0100, *dest, *src1), *src2],
            SETE(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1101_0101, *dest, *src2), *src1],
            SETNE(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1101_0110, *dest, *src1), *src2],
            SETNE(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1101_0111, *dest, *src2), *src1],

            SETC(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1101_1000, *dest, *src1), *src2],
            SETC(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1101_1001, *dest, *src2), *src1],
            SETNC(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1101_1010, *dest, *src1), *src2],
            SETNC(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1101_1011, *dest, *src2), *src1],

            SETG(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1101_1100, *dest, *src1), *src2],
            SETG(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1101_1101, *dest, *src2), *src1],
            SETGE(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1101_1110, *dest, *src1), *src2],
            SETGE(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1101_1111, *dest, *src2), *src1],

            SETL(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1110_0000, *dest, *src1), *src2],
            SETL(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1110_0001, *dest, *src2), *src1],
            SETLE(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1110_0010, *dest, *src1), *src2],
            SETLE(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1110_0011, *dest, *src2), *src1],

            SSETG(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1110_0100, *dest, *src1), *src2],
            SSETG(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1110_0101, *dest, *src2), *src1],
            SSETGE(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1110_0110, *dest, *src1), *src2],
            SSETGE(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1110_0111, *dest, *src2), *src1],

            SSETL(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1110_1000, *dest, *src1), *src2],
            SSETL(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1110_1001, *dest, *src2), *src1],
            SSETLE(dest:reg:!, src1:reg, src2:imm) => [pack2(0b_1110_1010, *dest, *src1), *src2],
            SSETLE(dest:reg:!, src1:imm, src2:reg) => [pack2(0b_1110_1011, *dest, *src2), *src1],

            MOV(dest:reg:!, src:reg:!) => [pack2(0b_1110_1100, *dest, *src)],
            CPY(dest:reg, src:reg) => [pack2(0b_1110_1101, *dest, *src)],

            BRG(dest:reg, src1:reg, src2:imm) => [pack2(0b_1111_0000, *dest, *src1), *src2],
            BRG(dest:reg, src1:imm, src2:reg) => [pack2(0b_1111_0001, *dest, *src2), *src1],
            BRG(dest:imm, src1:reg, src2:reg) => [pack2(0b_1111_0010, *src1, *src2), *dest],

            BGE(dest:reg, src1:reg, src2:imm) => [pack2(0b_1111_0011, *dest, *src1), *src2],
            BGE(dest:reg, src1:imm, src2:reg) => [pack2(0b_1111_0100, *dest, *src2), *src1],
            BGE(dest:imm, src1:reg, src2:reg) => [pack2(0b_1111_0101, *src1, *src2), *dest],

            BRL(dest:reg, src1:reg, src2:imm) => [pack2(0b_1111_0110, *dest, *src1), *src2],
            BRL(dest:reg, src1:imm, src2:reg) => [pack2(0b_1111_0111, *dest, *src2), *src1],
            BRL(dest:imm, src1:reg, src2:reg) => [pack2(0b_1111_1000, *src1, *src2), *dest],

            BLE(dest:reg, src1:reg, src2:imm) => [pack2(0b_1111_1001, *dest, *src1), *src2],
            BLE(dest:reg, src1:imm, src2:reg) => [pack2(0b_1111_1010, *dest, *src2), *src1],
            BLE(dest:imm, src1:reg, src2:reg) => [pack2(0b_1111_1011, *src1, *src2), *dest],
        }
    }
}
