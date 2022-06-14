use std::{mem, ops};

use super::*;

#[macro_export]
macro_rules! instructions3 {
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
            UMLT(dest:reg, src1:val, src2:val),
            SUMLT(dest:reg, src1:val, src2:val),
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

macro_rules! inst_decl {
    (@type $imm:ident val) => {
        Value<Option<Register>, $imm>
    };
    (@type $imm:ident imm) => {
        $imm
    };
    (@type $imm:ident reg) => {
        // None represents $0
        Option<Register>
    };
    (@type $imm:ident port) => {
        Port
    };
    ($name:ident<$imm:ident>; $($opcode:ident $(($($operand:ident : $type:ident),*))?,)*) => {
        pub enum $name<$imm> {
            DW(Vec<$imm>),
            $($opcode$(($(inst_decl!(@type $imm $type)),*))?,)*
        }

        impl<$imm: Clone> Clone for $name<$imm> {
            fn clone(&self) -> Self {
                match self {
                    $name::DW(v) => $name::DW(v.clone()),
                    $($name::$opcode$(($($operand),*))? => $name::$opcode$(($($operand.clone()),*))?,)*
                }
            }
        }
    }
}

instructions3!(inst_decl!(Instruction3op<Imm>));

#[derive(Clone)]
pub enum Instruction2op<Imm> {
    DW(Imm),
    NOP,
    HLT,
    RET,

    PSH(Value<Register, Imm>),
    JMP(Value<Register, Imm>),
    CAL(Value<Register, Imm>),
    CPY(Imm, Value<Register, Imm>),
    STR(Imm, Value<Register, Imm>),
    POP(Option<Register>),

    IN(Register, Port),
    OUT(Port, Value<Register, Imm>),

    BinaryBranch(BinaryBranchCondition, Imm, Register, Value<Register, Imm>),
    UnaryBranch(UnaryBranchCondition, Value<Register, Imm>, Register),
    Ordinary(OrdinaryInstruction, Register, Value<Register, Imm>),
}

#[derive(Clone, Copy)]
pub enum UnaryBranchCondition {
    Zero = 0b_0000,
    Odd,
    Positive,
    NotZero = 0b_1000,
    Even,
    Negative,
}

#[derive(Clone, Copy)]
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

impl ops::Not for BinaryBranchCondition {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Greater => Self::LessEqual,
            Self::Less => Self::GreaterEqual,
            Self::SignedGreater => Self::SignedLessEqual,
            Self::SignedLess => Self::SignedGreaterEqual,
            Self::Equal => Self::NotEqual,
            Self::Carry => Self::NoCarry,

            Self::LessEqual => Self::Greater,
            Self::GreaterEqual => Self::Less,
            Self::SignedLessEqual => Self::SignedGreater,
            Self::SignedGreaterEqual => Self::SignedLess,
            Self::NotEqual => Self::Equal,
            Self::NoCarry => Self::Carry,
        }
    }
}

impl BinaryBranchCondition {
    fn flip(self) -> Self {
        match self {
            BinaryBranchCondition::Equal => BinaryBranchCondition::Equal,
            BinaryBranchCondition::NotEqual => BinaryBranchCondition::NotEqual,
            BinaryBranchCondition::Carry => BinaryBranchCondition::Carry,
            BinaryBranchCondition::NoCarry => BinaryBranchCondition::NoCarry,

            BinaryBranchCondition::Greater => BinaryBranchCondition::Less,
            BinaryBranchCondition::GreaterEqual => BinaryBranchCondition::LessEqual,
            BinaryBranchCondition::Less => BinaryBranchCondition::Greater,
            BinaryBranchCondition::LessEqual => BinaryBranchCondition::GreaterEqual,

            BinaryBranchCondition::SignedGreater => BinaryBranchCondition::SignedLess,
            BinaryBranchCondition::SignedGreaterEqual => BinaryBranchCondition::SignedLessEqual,
            BinaryBranchCondition::SignedLess => BinaryBranchCondition::SignedGreater,
            BinaryBranchCondition::SignedLessEqual => BinaryBranchCondition::SignedGreaterEqual,
        }
    }

    fn set_inst(self) -> OrdinaryInstruction {
        match self {
            BinaryBranchCondition::Equal => OrdinaryInstruction::SETE,
            BinaryBranchCondition::NotEqual => OrdinaryInstruction::SETNE,
            BinaryBranchCondition::Carry => OrdinaryInstruction::SETC,
            BinaryBranchCondition::NoCarry => OrdinaryInstruction::SETNC,

            BinaryBranchCondition::Greater => OrdinaryInstruction::SETG,
            BinaryBranchCondition::GreaterEqual => OrdinaryInstruction::SETGE,
            BinaryBranchCondition::Less => OrdinaryInstruction::SETL,
            BinaryBranchCondition::LessEqual => OrdinaryInstruction::SETLE,

            BinaryBranchCondition::SignedGreater => OrdinaryInstruction::SSETG,
            BinaryBranchCondition::SignedGreaterEqual => OrdinaryInstruction::SSETGE,
            BinaryBranchCondition::SignedLess => OrdinaryInstruction::SSETL,
            BinaryBranchCondition::SignedLessEqual => OrdinaryInstruction::SSETLE,
        }
    }
}

impl ops::Not for UnaryBranchCondition {
    type Output = Self;

    fn not(self) -> Self::Output {
        match self {
            Self::Zero => Self::NotZero,
            Self::Odd => Self::Even,
            Self::Positive => Self::Negative,

            Self::NotZero => Self::Zero,
            Self::Even => Self::Odd,
            Self::Negative => Self::Positive,
        }
    }
}

#[derive(Clone, Copy)]
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

    SETG = 0b_0110_0000,
    SETL,
    SSETG,
    SSETL,
    SETE,
    SETC,

    SETLE = 0b_0111_0000,
    SETGE,
    SSETLE,
    SSETGE,
    SETNE,
    SETNC,
}

impl Instruction2op<u16> {
    pub fn encode(self) -> Vec<u16> {
        macro_rules! encode {
            ($($name:ident$(($($p:pat),+))? => [$($e:expr),*],)*) => {
                match self {
                    $(Instruction2op::$name$(($($p),+))? => vec![$($e),*],)*
                }
            };
        }

        fn pack(op: u16, reg: impl Into<u16>) -> u16 {
            (op << 4) | reg.into()
        }

        encode! {
            DW(imm) => [imm],

            NOP =>                              [0b_0000_0000],

            PSH(Value::Immediate(src)) =>       [0b_0000_0001, src],
            JMP(Value::Immediate(dest)) =>      [0b_0000_0010, dest],
            CAL(Value::Immediate(dest)) =>      [0b_0000_0011, dest],
            CPY(dest, Value::Immediate(src)) => [0b_0000_0100, dest, src],
            STR(dest, Value::Immediate(src)) => [0b_0000_0101, dest, src],
            POP(None) =>                        [0b_0000_0110],

            HLT =>                              [0b_0000_0111],
            RET =>                              [0b_0000_1000],

            PSH(Value::Register(src)) =>        [pack(0b_0001, src)],
            JMP(Value::Register(dest)) =>       [pack(0b_0010, dest)],
            CAL(Value::Register(dest)) =>       [pack(0b_0011, dest)],
            CPY(dest, Value::Register(src)) =>  [pack(0b_0100, src), dest],
            STR(dest, Value::Register(src)) =>  [pack(0b_0101, src), dest],
            POP(Some(dest)) =>                  [pack(0b_0110, dest)],

            IN(dest, port) =>                   [pack(pack(0b_0001_0000 | port.upper(), dest), port.lower())],
            OUT(port, Value::Register(src)) =>  [pack(pack(0b_0001_0100 | port.upper(), port.lower()), src)],
            OUT(port, Value::Immediate(src)) => [pack(0b_0001_0100 | port.upper(), port.lower()), src],

            BinaryBranch(cond, dest, a, Value::Immediate(b)) => [pack(pack(0b_0010, cond as u8), a), b, dest],
            BinaryBranch(cond, dest, a, Value::Register(b)) => [pack(pack(pack(0b_0010, cond as u8), a), b), dest],

            UnaryBranch(cond, Value::Immediate(dest), src) => [pack(pack(0b_0011, cond as u8), src), dest],
            UnaryBranch(cond, Value::Register(dest), src) => [pack(pack(pack(0b_0011, cond as u8), src), dest)],

            Ordinary(inst, a, Value::Immediate(b)) => [pack(inst as u8 as _, a), b],
            Ordinary(inst, a, Value::Register(b)) => [pack(pack(inst as u8 as _, a), b)],
        }
    }
}

impl<Imm> Instruction3op<Imm> {
    pub fn map<T, F>(self, mut f: F) -> Instruction3op<T>
    where
        F: FnMut(Imm) -> T,
    {
        macro_rules! map {
            (@map $name:ident : reg) => {
                $name
            };
            (@map $name:ident : imm) => {
                f($name)
            };
            (@map $name:ident : val) => {
                $name.map(&mut f)
            };
            (@map $name:ident : port) => {
                $name
            };
            ($($opcode:ident $(($($name:ident : $type:ident),*))?,)*) => {
                match self {
                    Instruction3op::DW(vals) => Instruction3op::DW(vals.into_iter().map(f).collect()),
                    $(
                        Instruction3op::$opcode$(($($name),*))? => Instruction3op::$opcode$(($(map!(@map $name : $type)),*))?,
                    )*
                }
            }
        }
        instructions3!(map)
    }
}

impl Instruction3op<u16> {
    pub fn lower(
        self,
        volatile_reg: Option<Register>,
        addr: u16,
    ) -> Result<Vec<Instruction2op<u16>>, &'static str> {
        macro_rules! inst {
            ($name:ident$(, $e:expr)*) => {
                Instruction2op::Ordinary(OrdinaryInstruction::$name$(, $e)*)
            }
        }
        fn map_val(value: Value<Option<Register>, u16>) -> Value<Register, u16> {
            match value {
                Value::Register(Some(reg)) => Value::Register(reg),
                Value::Register(None) => Value::Immediate(0),
                Value::Immediate(i) => Value::Immediate(i),
            }
        }

        fn ordinary3(
            inst: OrdinaryInstruction,
            dest: Option<Register>,
            src1: Value<Option<Register>, u16>,
            src2: Value<Option<Register>, u16>,
        ) -> Vec<Instruction2op<u16>> {
            if let Some(dest) = dest {
                let (src1, src2) = (map_val(src1), map_val(src2));
                if Value::Register(dest) == src1 {
                    vec![Instruction2op::Ordinary(inst, dest, src2)]
                } else {
                    vec![
                        inst!(MOV, dest, src1),
                        Instruction2op::Ordinary(inst, dest, src2),
                    ]
                }
            } else {
                vec![Instruction2op::NOP]
            }
        }

        fn ordinary2(
            inst: OrdinaryInstruction,
            dest: Option<Register>,
            src: Value<Option<Register>, u16>,
        ) -> Vec<Instruction2op<u16>> {
            if let Some(dest) = dest {
                vec![Instruction2op::Ordinary(inst, dest, map_val(src))]
            } else {
                vec![Instruction2op::NOP]
            }
        }

        // instructions like ADD, where the sources can be flipped
        fn symmetric(
            inst: OrdinaryInstruction,
            dest: Option<Register>,
            src1: Value<Option<Register>, u16>,
            src2: Value<Option<Register>, u16>,
        ) -> Vec<Instruction2op<u16>> {
            if let Some(dest) = dest {
                let (src1, src2) = (map_val(src1), map_val(src2));
                if Value::Register(dest) == src1 {
                    vec![Instruction2op::Ordinary(inst, dest, src2)]
                } else if Value::Register(dest) == src2 {
                    vec![Instruction2op::Ordinary(inst, dest, src1)]
                } else {
                    vec![
                        inst!(MOV, dest, src1),
                        Instruction2op::Ordinary(inst, dest, src2),
                    ]
                }
            } else {
                vec![Instruction2op::NOP]
            }
        }

        fn signed(u: u16) -> i16 {
            unsafe { mem::transmute(u) }
        }

        fn set_like(
            cond: BinaryBranchCondition,
            dest: Option<Register>,
            src1: Value<Option<Register>, u16>,
            src2: Value<Option<Register>, u16>,
        ) -> Vec<Instruction2op<u16>> {
            if let Some(dest) = dest {
                match (map_val(src1), map_val(src2)) {
                    (Value::Immediate(src1), Value::Immediate(src2)) => {
                        let eval = match cond {
                            BinaryBranchCondition::Greater => src1 > src2,
                            BinaryBranchCondition::GreaterEqual => src1 >= src2,
                            BinaryBranchCondition::Less => src1 < src2,
                            BinaryBranchCondition::LessEqual => src1 <= src2,
                            BinaryBranchCondition::SignedGreater => signed(src1) > signed(src2),
                            BinaryBranchCondition::SignedGreaterEqual => {
                                signed(src1) >= signed(src2)
                            }
                            BinaryBranchCondition::SignedLess => signed(src1) < signed(src2),
                            BinaryBranchCondition::SignedLessEqual => signed(src1) <= signed(src2),
                            BinaryBranchCondition::Equal => src1 == src2,
                            BinaryBranchCondition::NotEqual => src1 != src2,
                            BinaryBranchCondition::Carry => src1.checked_add(src2).is_none(),
                            BinaryBranchCondition::NoCarry => src1.checked_add(src2).is_some(),
                        };
                        if eval {
                            vec![Instruction2op::Ordinary(
                                OrdinaryInstruction::MOV,
                                dest,
                                Value::Immediate(u16::MAX),
                            )]
                        } else {
                            vec![Instruction2op::Ordinary(
                                OrdinaryInstruction::MOV,
                                dest,
                                Value::Immediate(0),
                            )]
                        }
                    }
                    (Value::Register(src1), Value::Immediate(src2)) => ordinary3(
                        cond.set_inst(),
                        Some(dest),
                        Value::Register(Some(src1)),
                        Value::Immediate(src2),
                    ),
                    (Value::Immediate(src1), Value::Register(src2)) => ordinary3(
                        cond.flip().set_inst(),
                        Some(dest),
                        Value::Register(Some(src2)),
                        Value::Immediate(src1),
                    ),
                    (Value::Register(src1), Value::Register(src2)) => symmetric(
                        cond.set_inst(),
                        Some(dest),
                        Value::Register(Some(src1)),
                        Value::Register(Some(src2)),
                    ),
                }
            } else {
                vec![Instruction2op::NOP]
            }
        }

        // on LSTR:
        // Absolutely CANNOT do optimizations based on imm(0) to STR
        // because "dry run" serialization before actually resolving immediates
        // will just map everything to 0, and if that changes output
        // then every address (labels, relatives) would be wrong.

        // HOWEVER, for branches this is fine.
        // For branches, the optimization is just if there are immediates.
        // We can definitely resolve it to JMP if this is the case
        // We do however need to ensure that the false branch has the **same size**
        // and that means inserting 2 NOPs (since that takes 2 words, equal to JMP + imm)
        // If the dest is a register, that means inserting only 1 NOP, because register is encoded in the same word.
        // It has to be the same size, because the values in dry run will be garbage and might resolve to the wrong path.

        fn unary_branch(
            cond: UnaryBranchCondition,
            dest: Value<Option<Register>, u16>,
            src: Value<Option<Register>, u16>,
        ) -> Vec<Instruction2op<u16>> {
            match map_val(src) {
                Value::Immediate(src) => {
                    let branch = match cond {
                        UnaryBranchCondition::Zero => src == 0,
                        UnaryBranchCondition::NotZero => src != 0,
                        UnaryBranchCondition::Positive => src & 0x8000 == 0,
                        UnaryBranchCondition::Negative => src & 0x8000 != 0,
                        UnaryBranchCondition::Even => src & 1 == 0,
                        UnaryBranchCondition::Odd => src & 1 != 0,
                    };
                    let dest = map_val(dest);
                    if branch {
                        vec![Instruction2op::JMP(dest)]
                    } else if let Value::Register(_) = dest {
                        vec![Instruction2op::NOP]
                    } else {
                        vec![Instruction2op::NOP, Instruction2op::NOP]
                    }
                }
                Value::Register(src) => {
                    vec![Instruction2op::UnaryBranch(cond, map_val(dest), src)]
                }
            }
        }

        fn binary_branch(
            addr: u16,
            cond: BinaryBranchCondition,
            dest: Value<Option<Register>, u16>,
            src1: Value<Option<Register>, u16>,
            src2: Value<Option<Register>, u16>,
        ) -> Vec<Instruction2op<u16>> {
            fn half(
                addr: u16,
                cond: BinaryBranchCondition,
                dest: Value<Register, u16>,
                src1: Register,
                src2: u16,
            ) -> Vec<Instruction2op<u16>> {
                match dest {
                    Value::Immediate(dest) => vec![Instruction2op::BinaryBranch(
                        cond,
                        dest,
                        src1,
                        Value::Immediate(src2),
                    )],
                    Value::Register(dest) => vec![
                        // addr + 4 because branch is 1 word + dest + src2 = 3
                        // and another one for the JMP after the branch
                        Instruction2op::BinaryBranch(!cond, addr + 4, src1, Value::Immediate(src2)),
                        Instruction2op::JMP(Value::Register(dest)),
                    ],
                }
            }
            match (map_val(src1), map_val(src2)) {
                (Value::Immediate(src1), Value::Immediate(src2)) => {
                    let branch = match cond {
                        BinaryBranchCondition::Greater => src1 > src2,
                        BinaryBranchCondition::GreaterEqual => src1 >= src2,
                        BinaryBranchCondition::Less => src1 < src2,
                        BinaryBranchCondition::LessEqual => src1 <= src2,
                        BinaryBranchCondition::SignedGreater => signed(src1) > signed(src2),
                        BinaryBranchCondition::SignedGreaterEqual => signed(src1) >= signed(src2),
                        BinaryBranchCondition::SignedLess => signed(src1) < signed(src2),
                        BinaryBranchCondition::SignedLessEqual => signed(src1) <= signed(src2),
                        BinaryBranchCondition::Equal => src1 == src2,
                        BinaryBranchCondition::NotEqual => src1 != src2,
                        BinaryBranchCondition::Carry => src1.checked_add(src2).is_none(),
                        BinaryBranchCondition::NoCarry => src1.checked_add(src2).is_some(),
                    };
                    let dest = map_val(dest);
                    if branch {
                        vec![Instruction2op::JMP(dest)]
                    } else if let Value::Register(_) = dest {
                        vec![Instruction2op::NOP]
                    } else {
                        vec![Instruction2op::NOP, Instruction2op::NOP]
                    }
                }
                (Value::Register(src1), Value::Immediate(src2)) => {
                    half(addr, cond, map_val(dest), src1, src2)
                }
                (Value::Immediate(src1), Value::Register(src2)) => {
                    half(addr, cond.flip(), map_val(dest), src2, src1)
                }
                (Value::Register(src1), Value::Register(src2)) => match map_val(dest) {
                    Value::Immediate(dest) => vec![Instruction2op::BinaryBranch(
                        cond,
                        dest,
                        src1,
                        Value::Register(src2),
                    )],
                    Value::Register(dest) => vec![
                        // addr + 3 because branch is 2 words, and the jmp is another word
                        Instruction2op::BinaryBranch(!cond, addr + 3, src1, Value::Register(src2)),
                        Instruction2op::JMP(Value::Register(dest)),
                    ],
                },
            }
        }

        Ok(match self {
            Instruction3op::DW(imms) => imms.into_iter().map(Instruction2op::DW).collect(),
            Instruction3op::NOP => vec![Instruction2op::NOP],
            Instruction3op::HLT => vec![Instruction2op::HLT],
            Instruction3op::RET => vec![Instruction2op::RET],
            Instruction3op::PSH(dest) => vec![Instruction2op::PSH(map_val(dest))],
            Instruction3op::JMP(dest) => vec![Instruction2op::JMP(map_val(dest))],
            Instruction3op::CAL(dest) => vec![Instruction2op::CAL(map_val(dest))],
            Instruction3op::CPY(Value::Immediate(dest), src) => {
                vec![Instruction2op::CPY(dest, map_val(src))]
            }
            Instruction3op::STR(Value::Immediate(dest), src) => {
                vec![Instruction2op::STR(dest, map_val(src))]
            }
            Instruction3op::POP(dest) => vec![Instruction2op::POP(dest)],
            Instruction3op::IN(dest, port) => dest
                .map(|dest| vec![Instruction2op::IN(dest, port)])
                // Reading to R0 can mutate the port state
                .unwrap_or_else(|| {
                    vec![
                        Instruction2op::PSH(Value::Register(Register::R1)),
                        Instruction2op::IN(Register::R1, port),
                        Instruction2op::POP(Some(Register::R1)),
                    ]
                }),
            Instruction3op::OUT(port, src) => vec![Instruction2op::OUT(port, map_val(src))],

            Instruction3op::LLOD(dest, src1, src2) => dest
                .map(|dest| match (map_val(src1), map_val(src2)) {
                    (Value::Immediate(src1), Value::Immediate(src2)) => {
                        Instruction3op::LOD(Some(dest), Value::Immediate(src1 + src2))
                            .lower(volatile_reg, addr)
                    }
                    (src1, src2) => Ok(vec![
                        inst!(MOV, dest, src1),
                        inst!(ADD, dest, src2),
                        inst!(LOD, dest, Value::Register(dest)),
                    ]),
                })
                .unwrap_or_else(|| Ok(vec![Instruction2op::NOP]))?,
            Instruction3op::LSTR(dest1, dest2, src) => {
                match (volatile_reg, map_val(dest1), map_val(dest2), map_val(src)) {
                    (_, Value::Immediate(dest1), Value::Immediate(dest2), src) => Instruction3op::STR(
                        Value::Immediate(dest1 + dest2),
                        match src {
                            Value::Register(r) => Value::Register(Some(r)),
                            Value::Immediate(i) => Value::Immediate(i),
                        }
                    )
                    .lower(volatile_reg, addr)?,
                    (_, Value::Register(dest1), dest2, src) | (_, dest2, Value::Register(dest1), src)
                        if src != Value::Register(dest1) && dest2 != Value::Register(dest1) =>
                    {
                        vec![
                            inst!(ADD, dest1, dest2),
                            inst!(STR, dest1, src),
                            inst!(SUB, dest1, dest2),
                        ]
                    }
                    (Some(volatile_reg), dest1, dest2, src)
                    if dest1 != Value::Register(volatile_reg) && dest2 != Value::Register(volatile_reg)
                    => {
                        vec![
                            inst!(MOV, volatile_reg, dest1),
                            inst!(ADD, volatile_reg, dest2),
                            inst!(STR, volatile_reg, src),
                        ]
                    }
                    (Some(volatile_reg), dest1, _dest2, src)
                    if dest1 != Value::Register(volatile_reg) /* && dest2 == Value::Register(volatile_reg) */
                    => {
                        vec![
                            inst!(ADD, volatile_reg, dest1),
                            inst!(STR, volatile_reg, src),
                        ]
                    }
                    (Some(volatile_reg), _dest1, dest2, src)
                    if /* dest1 == Value::Register(volatile_reg) && */ dest2 != Value::Register(volatile_reg)
                    => {
                        vec![
                            inst!(ADD, volatile_reg, dest2),
                            inst!(STR, volatile_reg, src),
                        ]
                    }
                    (Some(volatile_reg), _dest1, _dest2, src)
                    /* if dest1 == Value::Register(volatile_reg) && dest2 == Value::Register(volatile_reg) */
                    => {
                        vec![
                            inst!(ADD, volatile_reg, Value::Register(volatile_reg)),
                            inst!(STR, volatile_reg, src),
                        ]
                    }
                    (None, _, _, _) => Err("Unsupported LSTR combination: please rewrite to have an explicit ADD with a temp reg or put a @volatile directive with a temp reg")?
                }
            }
            // if it's zero reg, it's not equivalent to NOP, so not ordinary.
            Instruction3op::STR(Value::Register(dest), src) => {
                if let Some(dest) = dest {
                    vec![inst!(STR, dest, map_val(src))]
                } else {
                    vec![Instruction2op::STR(0, map_val(src))]
                }
            }
            // same here
            Instruction3op::CPY(Value::Register(dest), src) => {
                if let Some(dest) = dest {
                    vec![inst!(CPY, dest, map_val(src))]
                } else {
                    vec![Instruction2op::CPY(0, map_val(src))]
                }
            }
            // not for LOD tho
            Instruction3op::LOD(dest, src) => ordinary2(OrdinaryInstruction::LOD, dest, src),

            Instruction3op::IMM(dest, src) => {
                ordinary2(OrdinaryInstruction::MOV, dest, Value::Immediate(src))
            }
            Instruction3op::MOV(dest, src) => {
                ordinary2(OrdinaryInstruction::MOV, dest, Value::Register(src))
            }

            Instruction3op::ADD(dest, src1, src2) => {
                symmetric(OrdinaryInstruction::ADD, dest, src1, src2)
            }

            Instruction3op::SUB(dest, src1, src2) => {
                ordinary3(OrdinaryInstruction::SUB, dest, src1, src2)
            }

            Instruction3op::INC(dest, src) => ordinary2(OrdinaryInstruction::INC, dest, src),
            Instruction3op::DEC(dest, src) => ordinary2(OrdinaryInstruction::DEC, dest, src),
            Instruction3op::NEG(dest, src) => ordinary2(OrdinaryInstruction::NEG, dest, src),
            Instruction3op::NOT(dest, src) => ordinary2(OrdinaryInstruction::NOT, dest, src),

            Instruction3op::AND(dest, src1, src2) => {
                symmetric(OrdinaryInstruction::AND, dest, src1, src2)
            }
            Instruction3op::NAND(dest, src1, src2) => {
                symmetric(OrdinaryInstruction::NAND, dest, src1, src2)
            }

            Instruction3op::OR(dest, src1, src2) => {
                symmetric(OrdinaryInstruction::OR, dest, src1, src2)
            }
            Instruction3op::NOR(dest, src1, src2) => {
                symmetric(OrdinaryInstruction::NOR, dest, src1, src2)
            }

            Instruction3op::XOR(dest, src1, src2) => {
                symmetric(OrdinaryInstruction::XOR, dest, src1, src2)
            }
            Instruction3op::XNOR(dest, src1, src2) => {
                symmetric(OrdinaryInstruction::XNOR, dest, src1, src2)
            }

            Instruction3op::LSH(dest, src) => ordinary2(OrdinaryInstruction::LSH, dest, src),
            Instruction3op::RSH(dest, src) => ordinary2(OrdinaryInstruction::RSH, dest, src),
            Instruction3op::SRS(dest, src) => ordinary2(OrdinaryInstruction::SRS, dest, src),

            Instruction3op::BSR(dest, src1, src2) => {
                ordinary3(OrdinaryInstruction::BSR, dest, src1, src2)
            }
            Instruction3op::BSL(dest, src1, src2) => {
                ordinary3(OrdinaryInstruction::BSL, dest, src1, src2)
            }
            Instruction3op::BSS(dest, src1, src2) => {
                ordinary3(OrdinaryInstruction::BSS, dest, src1, src2)
            }

            Instruction3op::MLT(dest, src1, src2) => {
                symmetric(OrdinaryInstruction::MLT, dest, src1, src2)
            }
            Instruction3op::UMLT(dest, src1, src2) => {
                symmetric(OrdinaryInstruction::UMLT, dest, src1, src2)
            }
            Instruction3op::SUMLT(dest, src1, src2) => {
                symmetric(OrdinaryInstruction::SUMLT, dest, src1, src2)
            }

            Instruction3op::DIV(dest, src1, src2) => {
                ordinary3(OrdinaryInstruction::DIV, dest, src1, src2)
            }
            Instruction3op::SDIV(dest, src1, src2) => {
                ordinary3(OrdinaryInstruction::SDIV, dest, src1, src2)
            }
            Instruction3op::MOD(dest, src1, src2) => {
                ordinary3(OrdinaryInstruction::MOD, dest, src1, src2)
            }
            Instruction3op::SMOD(dest, src1, src2) => {
                ordinary3(OrdinaryInstruction::SMOD, dest, src1, src2)
            }

            Instruction3op::SETE(dest, src1, src2) => {
                set_like(BinaryBranchCondition::Equal, dest, src1, src2)
            }
            Instruction3op::SETNE(dest, src1, src2) => {
                set_like(BinaryBranchCondition::NotEqual, dest, src1, src2)
            }
            Instruction3op::SETC(dest, src1, src2) => {
                set_like(BinaryBranchCondition::Carry, dest, src1, src2)
            }
            Instruction3op::SETNC(dest, src1, src2) => {
                set_like(BinaryBranchCondition::NoCarry, dest, src1, src2)
            }

            Instruction3op::SETG(dest, src1, src2) => {
                set_like(BinaryBranchCondition::Greater, dest, src1, src2)
            }
            Instruction3op::SETGE(dest, src1, src2) => {
                set_like(BinaryBranchCondition::GreaterEqual, dest, src1, src2)
            }
            Instruction3op::SETL(dest, src1, src2) => {
                set_like(BinaryBranchCondition::Less, dest, src1, src2)
            }
            Instruction3op::SETLE(dest, src1, src2) => {
                set_like(BinaryBranchCondition::LessEqual, dest, src1, src2)
            }

            Instruction3op::SSETG(dest, src1, src2) => {
                set_like(BinaryBranchCondition::SignedGreater, dest, src1, src2)
            }
            Instruction3op::SSETGE(dest, src1, src2) => {
                set_like(BinaryBranchCondition::SignedGreaterEqual, dest, src1, src2)
            }
            Instruction3op::SSETL(dest, src1, src2) => {
                set_like(BinaryBranchCondition::SignedLess, dest, src1, src2)
            }
            Instruction3op::SSETLE(dest, src1, src2) => {
                set_like(BinaryBranchCondition::SignedLessEqual, dest, src1, src2)
            }

            Instruction3op::BRZ(dest, src) => unary_branch(UnaryBranchCondition::Zero, dest, src),
            Instruction3op::BNZ(dest, src) => {
                unary_branch(UnaryBranchCondition::NotZero, dest, src)
            }
            Instruction3op::BRP(dest, src) => {
                unary_branch(UnaryBranchCondition::Positive, dest, src)
            }
            Instruction3op::BRN(dest, src) => {
                unary_branch(UnaryBranchCondition::Negative, dest, src)
            }
            Instruction3op::BOD(dest, src) => unary_branch(UnaryBranchCondition::Odd, dest, src),
            Instruction3op::BEV(dest, src) => unary_branch(UnaryBranchCondition::Even, dest, src),

            Instruction3op::BRE(dest, src1, src2) => {
                binary_branch(addr, BinaryBranchCondition::Equal, dest, src1, src2)
            }
            Instruction3op::BNE(dest, src1, src2) => {
                binary_branch(addr, BinaryBranchCondition::NotEqual, dest, src1, src2)
            }
            Instruction3op::BRC(dest, src1, src2) => {
                binary_branch(addr, BinaryBranchCondition::Carry, dest, src1, src2)
            }
            Instruction3op::BNC(dest, src1, src2) => {
                binary_branch(addr, BinaryBranchCondition::NoCarry, dest, src1, src2)
            }

            Instruction3op::BRG(dest, src1, src2) => {
                binary_branch(addr, BinaryBranchCondition::Greater, dest, src1, src2)
            }
            Instruction3op::BGE(dest, src1, src2) => {
                binary_branch(addr, BinaryBranchCondition::GreaterEqual, dest, src1, src2)
            }
            Instruction3op::BRL(dest, src1, src2) => {
                binary_branch(addr, BinaryBranchCondition::Less, dest, src1, src2)
            }
            Instruction3op::BLE(dest, src1, src2) => {
                binary_branch(addr, BinaryBranchCondition::LessEqual, dest, src1, src2)
            }

            Instruction3op::SBRG(dest, src1, src2) => {
                binary_branch(addr, BinaryBranchCondition::SignedGreater, dest, src1, src2)
            }
            Instruction3op::SBGE(dest, src1, src2) => binary_branch(
                addr,
                BinaryBranchCondition::SignedGreaterEqual,
                dest,
                src1,
                src2,
            ),
            Instruction3op::SBRL(dest, src1, src2) => {
                binary_branch(addr, BinaryBranchCondition::SignedLess, dest, src1, src2)
            }
            Instruction3op::SBLE(dest, src1, src2) => binary_branch(
                addr,
                BinaryBranchCondition::SignedLessEqual,
                dest,
                src1,
                src2,
            ),
        })
    }
}
