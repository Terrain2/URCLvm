use super::*;

#[macro_export]
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
        pub enum $name<$imm> {
            DW(Vec<$imm>),
            $($opcode$(($(inst_decl!(@type $imm $type)),*))?,)*
        }
    }
}

instructions!(inst_decl!(Instruction<Imm>));

impl<Imm> Instruction<Imm> {
    pub fn map<T, F>(self, mut f: F) -> Instruction<T>
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
    pub fn encode(&self) -> Vec<u16> {
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
                    Self::DW(vals) => vals.clone(),
                    $(Self::$opcode$(($($type!($name$(:$m)?)),*))? => vec![$($e),*],)*
                }
            };
        }

        // :! suffix after type indicates that the other type is not allowed for that instruction
        // so: CPY(dest:reg) because there is also CPY(dest:imm)
        // but ADD(dest:reg:!) because there is no ADD(dest:imm)
        // this is to tell the difference between Value and the actual operand types
        encode! {
            NOP => [0b0000_0000],
            HLT => [0b0000_0001],
            RET => [0b0000_0010],

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