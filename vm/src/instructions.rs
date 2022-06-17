use std::{fmt, borrow::Cow};

use derive_more::Display;
use derive_try_from_primitive::TryFromPrimitive;

use super::*;

macro_rules! conv {
    ($($name:ident $(: $type:ident)?),*) => {
        ($({
            $(let $name: $type = $name.into();)?
            $name
        }),*)
    };
}

pub enum ControlFlow {
    Next,
    Halted,
    Jump(Word),
}

#[derive(Debug)]
pub enum Instruction {
    NOP,
    HLT,
    RET,

    PSH(Value),
    JMP(Value),
    CAL(Value, Word),
    CPY(Word, Value),
    STR(Word, Value),
    POP(Option<Register>),

    IN(Register, Port),
    OUT(Port, Value),

    BinaryBranch(BinaryBranchCondition, Word, Register, Value),
    UnaryBranch(UnaryBranchCondition, Value, Register),
    Ordinary(OrdinaryInstruction, Register, Value),
    SET(BinaryBranchCondition, Register, Value),
}

impl fmt::Display for Instruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Instruction::NOP => write!(f, "NOP"),
            Instruction::HLT => write!(f, "HLT"),
            Instruction::RET => write!(f, "RET"),

            Instruction::PSH(src) => write!(f, "PSH {src}"),
            Instruction::JMP(dest) => write!(f, "JMP {dest}"),
            Instruction::CAL(dest, ret) => write!(f, "CAL {dest} (ret {ret})"),
            Instruction::CPY(dest, src) => write!(f, "CPY {dest} {src}"),
            Instruction::STR(dest, src) => write!(f, "STR {dest} {src}"),
            Instruction::POP(Some(dest)) => write!(f, "POP {dest}"),
            Instruction::POP(None) => write!(f, "POP $0"),

            Instruction::IN(dest, port) => write!(f, "IN {dest} %{port}"),
            Instruction::OUT(port, src) => write!(f, "OUT %{port} {src}"),

            Instruction::BinaryBranch(cond, dest, src1, src2) => {
                write!(f, "Branch [{dest}] if {src1} {cond} {src2}")
            }
            Instruction::UnaryBranch(UnaryBranchCondition::Zero, dest, src) => {
                write!(f, "Branch [{dest}] if {src} == 0")
            }
            Instruction::UnaryBranch(UnaryBranchCondition::NotZero, dest, src) => {
                write!(f, "Branch [{dest}] if {src} != 0")
            }
            Instruction::UnaryBranch(UnaryBranchCondition::Positive, dest, src) => {
                write!(f, "Branch [{dest}] if {src} is positive")
            }
            Instruction::UnaryBranch(UnaryBranchCondition::Negative, dest, src) => {
                write!(f, "Branch [{dest}] if {src} is negative")
            }
            Instruction::UnaryBranch(UnaryBranchCondition::Even, dest, src) => {
                write!(f, "Branch [{dest}] if {src} is even")
            }
            Instruction::UnaryBranch(UnaryBranchCondition::Odd, dest, src) => {
                write!(f, "Branch [{dest}] if {src} is odd")
            }

            Instruction::SET(cond, dest, src) => write!(f, "SET {dest} if {dest} {cond} {src}"),

            Instruction::Ordinary(inst, dest, src) => write!(f, "{inst} {dest} {src}"),
        }
    }
}

#[derive(Debug, Clone, Copy, TryFromPrimitive)]
#[repr(u8)]
pub enum UnaryBranchCondition {
    Zero = 0b_0000,
    Odd,
    Positive,
    NotZero = 0b_1000,
    Even,
    Negative,
}

#[derive(Debug, Clone, Copy, TryFromPrimitive)]
#[repr(u8)]
pub enum BinaryBranchCondition {
    Greater = 0b_0000,
    Less,
    SignedGreater,
    SignedLess,
    Equal,
    Carry,

    LessEqual = 0b_1000,
    GreaterEqual,
    SignedLessEqual,
    SignedGreaterEqual,
    NotEqual,
    NoCarry,
}

impl fmt::Display for BinaryBranchCondition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryBranchCondition::Greater => write!(f, "un>"),
            BinaryBranchCondition::Less => write!(f, "un<"),
            BinaryBranchCondition::SignedGreater => write!(f, "s>"),
            BinaryBranchCondition::SignedLess => write!(f, "s<"),
            BinaryBranchCondition::Equal => write!(f, "=="),
            BinaryBranchCondition::Carry => write!(f, "carry"),

            BinaryBranchCondition::LessEqual => write!(f, "un<="),
            BinaryBranchCondition::GreaterEqual => write!(f, "un>="),
            BinaryBranchCondition::SignedLessEqual => write!(f, "s<="),
            BinaryBranchCondition::SignedGreaterEqual => write!(f, "s>="),
            BinaryBranchCondition::NotEqual => write!(f, "!="),
            BinaryBranchCondition::NoCarry => write!(f, "!carry"),
        }
    }
}

impl BinaryBranchCondition {
    fn eval(self, a: Word, b: Word) -> bool {
        match self {
            BinaryBranchCondition::Greater => conv!(a: u16) > conv!(b: u16),
            BinaryBranchCondition::Less => conv!(a: u16) < conv!(b: u16),
            BinaryBranchCondition::SignedGreater => conv!(a: i16) > conv!(b: i16),
            BinaryBranchCondition::SignedLess => conv!(a: i16) < conv!(b: i16),
            BinaryBranchCondition::Equal => conv!(a: u16) == conv!(b: u16),
            BinaryBranchCondition::Carry => conv!(a: u16).checked_add(conv!(b: u16)).is_none(),

            BinaryBranchCondition::LessEqual => conv!(a: u16) <= conv!(b: u16),
            BinaryBranchCondition::GreaterEqual => conv!(a: u16) >= conv!(b: u16),
            BinaryBranchCondition::SignedLessEqual => conv!(a: i16) <= conv!(b: i16),
            BinaryBranchCondition::SignedGreaterEqual => conv!(a: i16) >= conv!(b: i16),
            BinaryBranchCondition::NotEqual => conv!(a: u16) != conv!(b: u16),
            BinaryBranchCondition::NoCarry => conv!(a: u16).checked_add(conv!(b: u16)).is_some(),
        }
    }
}

#[derive(Debug, Clone, Copy, Display, TryFromPrimitive)]
#[repr(u8)]
pub enum OrdinaryInstruction {
    MOV = 0b_0100_0000,
    AND,
    OR,
    XOR,
    NOT,
    NAND,
    NOR,
    XNOR,
    LSH,
    RSH,
    SRS,
    BSL,
    BSR,
    BSS,

    ADD,
    SUB,
    INC,
    DEC,
    NEG,

    MLT,
    DIV,
    SDIV,
    MOD,
    SMOD,

    UMLT,
    SUMLT,

    CPY,
    STR,
    LOD,
}

impl Instruction {
    pub fn decode(memory: &VmMem, Wrapping(pc): Wrapping<u16>) -> Option<(u16, Instruction)> {
        macro_rules! mem {
            ($index:expr) => {
                memory.get($index.into())?
            };
        }
        let opcode: u16 = mem![pc].into();

        let op1 = u4::from(((opcode >> 0xC) & 0xF) as u8);
        let op2 = u4::from(((opcode >> 0x8) & 0xF) as u8);
        let op3 = u4::from(((opcode >> 0x4) & 0xF) as u8);
        let op4 = u4::from(((opcode >> 0x0) & 0xF) as u8);

        if op1 == u4::b_0000 && op2 == u4::b_0000 {
            match op3 {
                u4::b_0000 => match op4 {
                    u4::b_0000 => Some((1, Instruction::NOP)),
                    u4::b_0001 => Some((2, Instruction::PSH(Value::Immediate(mem![pc + 1])))),
                    u4::b_0010 => Some((2, Instruction::JMP(Value::Immediate(mem![pc + 1])))),
                    u4::b_0011 => Some((
                        2,
                        Instruction::CAL(Value::Immediate(mem![pc + 1]), (pc + 2).into()),
                    )),
                    u4::b_0100 => Some((
                        3,
                        Instruction::CPY(mem![pc + 1], Value::Immediate(mem![pc + 2])),
                    )),
                    u4::b_0101 => Some((
                        3,
                        Instruction::STR(mem![pc + 1], Value::Immediate(mem![pc + 2])),
                    )),
                    u4::b_0110 => Some((1, Instruction::POP(None))),
                    u4::b_0111 => Some((1, Instruction::HLT)),
                    u4::b_1000 => Some((1, Instruction::RET)),
                    _ => None,
                },
                u4::b_0001 => Some((1, Instruction::PSH(Value::Register(Register::from(op4))))),
                u4::b_0010 => Some((1, Instruction::JMP(Value::Register(Register::from(op4))))),
                u4::b_0011 => Some((
                    1,
                    Instruction::CAL(Value::Register(Register::from(op4)), (pc + 1).into()),
                )),
                u4::b_0100 => Some((
                    2,
                    Instruction::CPY(mem![pc + 1], Value::Register(Register::from(op4))),
                )),
                u4::b_0101 => Some((
                    2,
                    Instruction::STR(mem![pc + 1], Value::Register(Register::from(op4))),
                )),
                u4::b_0110 => Some((1, Instruction::POP(Some(Register::from(op4))))),
                _ => None,
            }
        } else {
            let (op1, op2, operand1, operand2, len) = if op1 == u4::b_0000 {
                (
                    op2,
                    op3,
                    Register::from(op4),
                    Value::Immediate(mem![pc + 1]),
                    2,
                )
            } else {
                (
                    op1,
                    op2,
                    Register::from(op3),
                    Value::Register(Register::from(op4)),
                    1,
                )
            };
            match op1 {
                u4::b_0001 => ((op2 as u8) >> 3 == 0)
                    .then(|| {
                        let top2 = (op2 as u8 & 0b11) << 4;
                        match (((op2 as u8) >> 2) & 1 != 0, operand2) {
                            (false, Value::Register(reg)) => Some((1, Instruction::IN(operand1, Port(top2 | (reg as u8))))),
                            (false, Value::Immediate(_)) => None,
                            (true, source) => Some((len, Instruction::OUT(Port(top2 | (operand1 as u8)), source))),
                        }
                    })
                    .map_or(None, std::convert::identity),
                u4::b_0010 => BinaryBranchCondition::try_from(op2 as u8)
                    .ok()
                    .and_then(|cond| {
                        Some((
                            len + 1,
                            Instruction::BinaryBranch(cond, mem![pc + len], operand1, operand2),
                        ))
                    }),
                u4::b_0011 => UnaryBranchCondition::try_from(op2 as u8)
                    .ok()
                    .map(|cond| (len, Instruction::UnaryBranch(cond, operand2, operand1))),
                u4::b_0110 => BinaryBranchCondition::try_from(op2 as u8)
                    .ok()
                    .map(|cond| (len, Instruction::SET(cond, operand1, operand2))),
                _ => OrdinaryInstruction::try_from(((op1 as u8) << 4) | (op2 as u8))
                    .ok()
                    .map(|inst| (len, Instruction::Ordinary(inst, operand1, operand2))),
            }
        }
    }

    pub fn execute(
        self,
        VmState {
            registers,
            memory,
            ports,
        }: &mut VmState,
        bottom_of_stack: u16,
    ) -> Result<ControlFlow, Cow<'static, str>> {
        macro_rules! eval {
            ($($name:ident $(: $type:ident)?),*) => {
                ($({
                    let value = match $name {
                        Value::Register(reg) => registers[reg],
                        Value::Immediate(num) => num,
                    };
                    $(let value: $type = value.into();)?
                    value
                }),*)
            };
        }
        macro_rules! mem {
            ($index:expr) => {{
                let addr: Word = $index.into();
                let len = memory.len();
                memory.get_mut(addr).ok_or_else(|| format!("Memory access violation (read {addr}, mem ends at 0x{len:04x})"))?
            }};
        }

        // push, pop are already bounds checking. no need to use mem![]
        macro_rules! push {
            ($val:expr) => {{
                let val = $val;
                let sp: u16 = registers[Register::SP].into();
                if sp < bottom_of_stack {
                    return Err("Stack Overflow".into());
                }
                registers[Register::SP] -= 1;
                memory[registers[Register::SP]] = val;
            }};
        }

        macro_rules! pop {
            () => {{
                let sp = registers[Register::SP];
                if conv!(sp: u16) >= memory.len() {
                    return Err("Stack Underflow".into());
                }
                let val = memory[registers[Register::SP]];
                registers[Register::SP] += 1;
                val
            }};
        }

        Ok(match self {
            Instruction::NOP => ControlFlow::Next,
            Instruction::HLT => ControlFlow::Halted,
            Instruction::RET => ControlFlow::Jump(pop!()),
            Instruction::IN(dest, port) => {
                registers[dest] = ports[port].read();
                ControlFlow::Next
            }
            Instruction::OUT(port, src) => {
                ports[port].write(eval!(src));
                ControlFlow::Next
            }
            Instruction::PSH(src) => {
                push!(eval!(src));
                ControlFlow::Next
            }
            Instruction::JMP(dest) => ControlFlow::Jump(eval!(dest)),
            Instruction::CAL(dest, ret) => {
                push!(ret);
                ControlFlow::Jump(eval!(dest))
            }
            Instruction::CPY(dest, src) => {
                *mem![dest] = *mem![eval!(src)];
                ControlFlow::Next
            }
            Instruction::STR(dest, src) => {
                *mem![dest] = eval!(src);
                ControlFlow::Next
            }
            Instruction::POP(dest) => {
                let popped = pop!();
                if let Some(dest) = dest {
                    registers[dest] = popped;
                }
                ControlFlow::Next
            }

            Instruction::BinaryBranch(cond, dest, src1, src2) => {
                if cond.eval(registers[src1], eval!(src2)) {
                    ControlFlow::Jump(dest)
                } else {
                    ControlFlow::Next
                }
            }

            Instruction::UnaryBranch(cond, dest, src) => {
                let src = registers[src];
                let src = conv!(src: u16);
                let branch = match cond {
                    UnaryBranchCondition::Even => src & 1 == 0,
                    UnaryBranchCondition::Odd => src & 1 != 0,
                    UnaryBranchCondition::Positive => src & 0x8000 == 0,
                    UnaryBranchCondition::Negative => src & 0x8000 != 0,
                    UnaryBranchCondition::Zero => src == 0,
                    UnaryBranchCondition::NotZero => src != 0,
                };
                if branch {
                    ControlFlow::Jump(eval!(dest))
                } else {
                    ControlFlow::Next
                }
            }

            Instruction::SET(cond, op1, op2) => {
                registers[op1] = cond.eval(registers[op1], eval!(op2)).into();
                ControlFlow::Next
            }

            Instruction::Ordinary(inst, dest, src) => {
                let a = registers[dest];
                let b = eval!(src);
                registers[dest] = match inst {
                    OrdinaryInstruction::MOV => b,
                    OrdinaryInstruction::AND => a & b,
                    OrdinaryInstruction::OR => a | b,
                    OrdinaryInstruction::XOR => a ^ b,
                    OrdinaryInstruction::NOT => !b,
                    OrdinaryInstruction::NAND => !(a & b),
                    OrdinaryInstruction::NOR => !(a | b),
                    OrdinaryInstruction::XNOR => !(a ^ b),

                    OrdinaryInstruction::LSH => b << 1,
                    OrdinaryInstruction::RSH => (conv!(b: u16) >> 1).into(),
                    OrdinaryInstruction::SRS => (conv!(b: i16) >> 1).into(),
                    // shifting by >= u16::BITS will produce "overflow" and panic for some reason
                    // so therefore i have to cmp::min(15, x) to do essentialy a saturating shift
                    // unchecked_shr/shl and overflowing_shr/shl are not suitable here
                    OrdinaryInstruction::BSL => a << Word::from(cmp::min(15, conv!(b: u16))),
                    OrdinaryInstruction::BSR => {
                        (conv!(a: u16) >> cmp::min(15, conv!(b: u16))).into()
                    }
                    OrdinaryInstruction::BSS => {
                        (conv!(a: i16) >> cmp::min(15, conv!(b: u16))).into()
                    }

                    OrdinaryInstruction::ADD => a + b,
                    OrdinaryInstruction::SUB => a - b,
                    OrdinaryInstruction::INC => b + Word::from(1u16),
                    OrdinaryInstruction::DEC => b - Word::from(1u16),
                    OrdinaryInstruction::NEG => -b,

                    OrdinaryInstruction::MLT => a * b,
                    OrdinaryInstruction::DIV => (conv!(a: u16) / conv!(b: u16)).into(),
                    OrdinaryInstruction::SDIV => (conv!(a: i16) / conv!(b: i16)).into(),
                    OrdinaryInstruction::MOD => (conv!(a: u16) % conv!(b: u16)).into(),
                    OrdinaryInstruction::SMOD => (conv!(a: i16) % conv!(b: i16)).into(),

                    OrdinaryInstruction::UMLT => {
                        ((((conv!(a: u16) as u32) * (conv!(b: u16) as u32)) >> 16) as u16).into()
                    }
                    OrdinaryInstruction::SUMLT => {
                        ((((conv!(a: i16) as i32) * (conv!(b: i16) as i32)) >> 16) as i16).into()
                    }

                    // these don't actually update the reg, so make sure they return `a`
                    OrdinaryInstruction::CPY => {
                        memory[a] = memory[b];
                        a
                    }
                    OrdinaryInstruction::STR => {
                        memory[a] = b;
                        a
                    }
                    // LOD updates the reg tho
                    OrdinaryInstruction::LOD => *mem![b],
                };
                ControlFlow::Next
            } // Instruction::BSL(dest, src1, src2) => {
              //     registers[dest] = eval!(src1) << Word::from(cmp::min(15, eval!(src2: u16)));
              //     ControlFlow::Next
              // }
              // Instruction::BSR(dest, src1, src2) => {
              //     registers[dest] = Word::from(eval!(src1: u16) >> cmp::min(15, eval!(src2: u16)));
              //     ControlFlow::Next
              // }
              // Instruction::BSS(dest, src1, src2) => {
              //     registers[dest] = Word::from(eval!(src1: i16) >> cmp::min(15, eval!(src2: u16)));
              //     ControlFlow::Next
              // }
        })
    }
}
