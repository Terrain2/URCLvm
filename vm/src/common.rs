use super::*;
use derive_more::{Add, BitAnd, BitOr, BitXor, Neg, Not, Shl, Sub};
use half::f16;
use std::ops::{AddAssign, Index, IndexMut, Mul, Shl, SubAssign};

// Sign-agnostic integer type which implements only the operators that do not care about sign.
#[derive(Debug, Clone, Copy, Eq, Add, Sub, Not, Neg, BitAnd, BitOr, BitXor, Shl)]
pub struct Word(Wrapping<u16>);

impl From<bool> for Word {
    fn from(b: bool) -> Self {
        Word(Wrapping(if b { u16::MAX } else { u16::MIN }))
    }
}

impl AddAssign<u16> for Word {
    fn add_assign(&mut self, rhs: u16) {
        self.0 += Wrapping(rhs);
    }
}

impl SubAssign<u16> for Word {
    fn sub_assign(&mut self, rhs: u16) {
        self.0 -= Wrapping(rhs);
    }
}

// derive_more does not implement this correctly???
impl Mul<Word> for Word {
    type Output = Word;

    fn mul(self, rhs: Word) -> Word {
        Word(self.0 * rhs.0)
    }
}

// it also does not implement this???
impl Shl<Word> for Word {
    type Output = Word;

    fn shl(self, rhs: Word) -> Word {
        Word::from(self.0 .0 << rhs.0 .0)
    }
}

impl<T: Into<Word> + Copy> PartialEq<T> for Word {
    fn eq(&self, rhs: &T) -> bool {
        self.0 == (*rhs).into().0
    }
}

impl Default for Word {
    fn default() -> Self {
        Word(Wrapping(0))
    }
}

impl From<u16> for Word {
    fn from(u: u16) -> Self {
        Word(Wrapping(u))
    }
}

impl From<i16> for Word {
    fn from(i: i16) -> Self {
        Word::from(i as u16)
    }
}

impl From<f16> for Word {
    fn from(f: f16) -> Self {
        Word::from(f.to_bits())
    }
}

impl<T> From<Wrapping<T>> for Word
where
    Word: From<T>,
{
    fn from(t: Wrapping<T>) -> Self {
        t.0.into()
    }
}

impl Into<u16> for Word {
    fn into(self) -> u16 {
        self.0 .0
    }
}

impl Into<i16> for Word {
    fn into(self) -> i16 {
        self.0 .0 as i16
    }
}

impl Into<f16> for Word {
    fn into(self) -> f16 {
        f16::from_bits(self.into())
    }
}

impl Into<usize> for Word {
    fn into(self) -> usize {
        self.0 .0 as usize
    }
}

impl<T> Into<Wrapping<T>> for Word
where
    Word: Into<T>,
{
    fn into(self) -> Wrapping<T> {
        Wrapping(self.into())
    }
}

pub struct VmMem {
    mem: Vec<Word>,
}
pub struct VmState {
    pub registers: Registers,
    pub memory: VmMem,
    pub ports: Ports,
}

impl<T> Index<T> for VmMem
where
    T: Into<usize>,
{
    type Output = Word;

    fn index(&self, index: T) -> &Self::Output {
        let index: usize = index.into();
        if index > self.mem.len() {
            panic!("Invalid RAM Location");
        }
        &self.mem[index]
    }
}

impl<T> IndexMut<T> for VmMem
where
    T: Into<usize>,
{
    fn index_mut(&mut self, index: T) -> &mut Self::Output {
        let index: usize = index.into();
        if index > self.mem.len() {
            panic!("Invalid RAM Location");
        }
        &mut self.mem[index]
    }
}

impl VmMem {
    pub fn len(&self) -> u16 {
        self.mem.len() as u16
    }

    pub fn new(size: u16) -> Self {
        VmMem {
            mem: vec![Word::default(); size as usize],
        }
    }
}

pub struct Registers {
    registers: [Word; 16],
}

impl Default for Registers {
    fn default() -> Self {
        Registers {
            registers: [Word::default(); 16],
        }
    }
}

impl std::ops::Index<Register> for Registers {
    type Output = Word;

    fn index(&self, reg: Register) -> &Word {
        &self.registers[reg as usize]
    }
}

impl std::ops::IndexMut<Register> for Registers {
    fn index_mut(&mut self, reg: Register) -> &mut Word {
        &mut self.registers[reg as usize]
    }
}

#[derive(Clone, Copy)]
#[repr(u8)]
#[allow(non_camel_case_types)]
pub enum u4 {
    b_0000,
    b_0001,
    b_0010,
    b_0011,
    b_0100,
    b_0101,
    b_0110,
    b_0111,
    b_1000,
    b_1001,
    b_1010,
    b_1011,
    b_1100,
    b_1101,
    b_1110,
    b_1111,
}

impl From<u8> for u4 {
    fn from(value: u8) -> Self {
        match value {
            0b_0000 => u4::b_0000,
            0b_0001 => u4::b_0001,
            0b_0010 => u4::b_0010,
            0b_0011 => u4::b_0011,
            0b_0100 => u4::b_0100,
            0b_0101 => u4::b_0101,
            0b_0110 => u4::b_0110,
            0b_0111 => u4::b_0111,
            0b_1000 => u4::b_1000,
            0b_1001 => u4::b_1001,
            0b_1010 => u4::b_1010,
            0b_1011 => u4::b_1011,
            0b_1100 => u4::b_1100,
            0b_1101 => u4::b_1101,
            0b_1110 => u4::b_1110,
            0b_1111 => u4::b_1111,
            0b10000.. => unreachable!("Internal Overflow"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(usize)]
pub enum Register {
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

impl From<u4> for Register {
    fn from(val: u4) -> Self {
        match val {
            u4::b_0000 => Register::R0,
            u4::b_0001 => Register::R1,
            u4::b_0010 => Register::R2,
            u4::b_0011 => Register::R3,
            u4::b_0100 => Register::R4,
            u4::b_0101 => Register::R5,
            u4::b_0110 => Register::R6,
            u4::b_0111 => Register::R7,
            u4::b_1000 => Register::R8,
            u4::b_1001 => Register::R9,
            u4::b_1010 => Register::R10,
            u4::b_1011 => Register::R11,
            u4::b_1100 => Register::R12,
            u4::b_1101 => Register::R13,
            u4::b_1110 => Register::R14,
            u4::b_1111 => Register::SP,
        }
    }
}

#[derive(Debug)]
pub enum Value {
    Register(Register),
    Immediate(Word),
}

impl From<Register> for Value {
    fn from(r: Register) -> Self {
        Value::Register(r)
    }
}

impl From<Word> for Value {
    fn from(i: Word) -> Self {
        Value::Immediate(i)
    }
}
