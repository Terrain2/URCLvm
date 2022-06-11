use super::*;

pub enum ControlFlow {
    Next,
    Halted,
    Jump(Word),
}

#[derive(Debug)]
pub enum Instruction {
    NOP,
    HLT,
    IN(Register, Port),
    OUT(Port, Value),
    CAL { dest: Value, ret: Word },
    RET,
    PSH(Value),
    POP(Register),
    LOD(Register, Value),
    LLOD(Register, Value, Value),
    STR(Value, Value),
    LSTR(Value, Value, Value),
    CPY(Value, Value),
    IMM(Register, Word),
    MOV(Register, Register),
    ADD(Register, Value, Value),
    SUB(Register, Value, Value),
    INC(Register, Value),
    DEC(Register, Value),
    NEG(Register, Value),
    NOT(Register, Value),
    MLT(Register, Value, Value),
    DIV(Register, Value, Value),
    SDIV(Register, Value, Value),
    MOD(Register, Value, Value),
    SMOD(Register, Value, Value),
    AND(Register, Value, Value),
    NAND(Register, Value, Value),
    OR(Register, Value, Value),
    NOR(Register, Value, Value),
    XOR(Register, Value, Value),
    XNOR(Register, Value, Value),
    LSH(Register, Value),
    RSH(Register, Value),
    SRS(Register, Value),
    BSL(Register, Value, Value),
    BSR(Register, Value, Value),
    BSS(Register, Value, Value),
    JMP(Value),
    BRG(Value, Value, Value),
    BGE(Value, Value, Value),
    BRL(Value, Value, Value),
    BLE(Value, Value, Value),
    SBRG(Value, Value, Value),
    SBGE(Value, Value, Value),
    SBRL(Value, Value, Value),
    SBLE(Value, Value, Value),
    BRE(Value, Value, Value),
    BNE(Value, Value, Value),
    BRC(Value, Value, Value),
    BNC(Value, Value, Value),
    BRZ(Value, Value),
    BNZ(Value, Value),
    BRP(Value, Value),
    BRN(Value, Value),
    BOD(Value, Value),
    BEV(Value, Value),
    SETG(Register, Value, Value),
    SETGE(Register, Value, Value),
    SETL(Register, Value, Value),
    SETLE(Register, Value, Value),
    SSETG(Register, Value, Value),
    SSETGE(Register, Value, Value),
    SSETL(Register, Value, Value),
    SSETLE(Register, Value, Value),
    SETE(Register, Value, Value),
    SETNE(Register, Value, Value),
    SETC(Register, Value, Value),
    SETNC(Register, Value, Value),
}

impl Instruction {
    pub fn decode(memory: &VmMem, Wrapping(pc): Wrapping<u16>) -> Option<(u16, Instruction)> {
        let opcode: u16 = memory[pc].into();

        let upper = (opcode >> 8) as u8;
        let lower = (opcode & 0xFF) as u8;
        let lower_top = u4::from(lower >> 4);
        let lower_bot = u4::from(lower & 0xF);
        let imm = |i| memory[pc + i];
        let vimm = |i| imm(i).into();
        let reg = |i| Register::from(i);
        let vreg = |i| reg(i).into();

        let xreg: u16 = memory[pc + 1].into();
        let xreg1 = Register::from(u4::from((xreg >> 8) as u8 & 0xF)).into();
        let xreg2 = Register::from(u4::from(xreg as u8 & 0xF)).into();

        match upper {
            0b_0000_0000 => match lower {
                0b_0000_0000 => Some((1, Instruction::NOP)),
                0b_0000_0001 => Some((1, Instruction::HLT)),
                0b_0000_0010 => Some((1, Instruction::RET)),
                0b_0000_0011 => Some((2, Instruction::PSH(vimm(1)))),

                0b_0000_0100 => Some((3, Instruction::STR(vimm(1), vimm(2)))),
                0b_0000_0101 => Some((3, Instruction::CPY(vimm(1), vimm(2)))),

                0b_0000_0110 => Some((
                    2,
                    Instruction::CAL {
                        dest: vimm(1),
                        ret: (pc + 2).into(),
                    },
                )),
                0b_0000_0111 => Some((2, Instruction::JMP(vimm(1)))),

                0b_0000_1000 => Some((4, Instruction::BRG(vimm(1), vimm(2), vimm(3)))),
                0b_0000_1001 => Some((4, Instruction::BGE(vimm(1), vimm(2), vimm(3)))),
                0b_0000_1010 => Some((4, Instruction::BRL(vimm(1), vimm(2), vimm(3)))),
                0b_0000_1011 => Some((4, Instruction::BLE(vimm(1), vimm(2), vimm(3)))),

                0b_0000_1100 => Some((4, Instruction::SBRG(vimm(1), vimm(2), vimm(3)))),
                0b_0000_1101 => Some((4, Instruction::SBGE(vimm(1), vimm(2), vimm(3)))),
                0b_0000_1110 => Some((4, Instruction::SBRL(vimm(1), vimm(2), vimm(3)))),
                0b_0000_1111 => Some((4, Instruction::SBLE(vimm(1), vimm(2), vimm(3)))),

                0b_0001_0000 => Some((4, Instruction::BRE(vimm(1), vimm(2), vimm(3)))),
                0b_0001_0001 => Some((4, Instruction::BNE(vimm(1), vimm(2), vimm(3)))),
                0b_0001_0010 => Some((4, Instruction::BRC(vimm(1), vimm(2), vimm(3)))),
                0b_0001_0011 => Some((4, Instruction::BNC(vimm(1), vimm(2), vimm(3)))),

                0b_0001_0100 => Some((3, Instruction::BRZ(vimm(1), vimm(2)))),
                0b_0001_0101 => Some((3, Instruction::BNZ(vimm(1), vimm(2)))),

                0b_0001_0110 => Some((3, Instruction::BRP(vimm(1), vimm(2)))),
                0b_0001_0111 => Some((3, Instruction::BRN(vimm(1), vimm(2)))),
                0b_0001_1000 => Some((3, Instruction::BOD(vimm(1), vimm(2)))),
                0b_0001_1001 => Some((3, Instruction::BEV(vimm(1), vimm(2)))),

                0b_0001_1010 => Some((4, Instruction::LSTR(vimm(1), vimm(2), vimm(3)))),
                0b_0001_1011..=u8::MAX => None,
            },
            0b_0000_0001..=0b_0000_1111 => match ((upper & 0xF) << 4) | (lower_top as u8) {
                0b_0000_0000..=0b_0000_1111 => unreachable!("Already handled by imm-only cases"),
                0b_0001_0000 => Some((2, Instruction::BRG(vreg(lower_bot), xreg1, xreg2))),
                0b_0001_0001 => Some((3, Instruction::BRG(vreg(lower_bot), vimm(1), vimm(2)))),
                0b_0001_0010 => Some((3, Instruction::BRG(vimm(1), vreg(lower_bot), vimm(2)))),
                0b_0001_0011 => Some((3, Instruction::BRG(vimm(1), vimm(2), vreg(lower_bot)))),

                0b_0001_0100 => Some((2, Instruction::BGE(vreg(lower_bot), xreg1, xreg2))),
                0b_0001_0101 => Some((3, Instruction::BGE(vreg(lower_bot), vimm(1), vimm(2)))),
                0b_0001_0110 => Some((3, Instruction::BGE(vimm(1), vreg(lower_bot), vimm(2)))),
                0b_0001_0111 => Some((3, Instruction::BGE(vimm(1), vimm(2), vreg(lower_bot)))),

                0b_0001_1000 => Some((2, Instruction::BRL(vreg(lower_bot), xreg1, xreg2))),
                0b_0001_1001 => Some((3, Instruction::BRL(vreg(lower_bot), vimm(1), vimm(2)))),
                0b_0001_1010 => Some((3, Instruction::BRL(vimm(1), vreg(lower_bot), vimm(2)))),
                0b_0001_1011 => Some((3, Instruction::BRL(vimm(1), vimm(2), vreg(lower_bot)))),

                0b_0001_1100 => Some((2, Instruction::BLE(vreg(lower_bot), xreg1, xreg2))),
                0b_0001_1101 => Some((3, Instruction::BLE(vreg(lower_bot), vimm(1), vimm(2)))),
                0b_0001_1110 => Some((3, Instruction::BLE(vimm(1), vreg(lower_bot), vimm(2)))),
                0b_0001_1111 => Some((3, Instruction::BLE(vimm(1), vimm(2), vreg(lower_bot)))),

                0b_0010_0000 => Some((2, Instruction::SBRG(vreg(lower_bot), xreg1, xreg2))),
                0b_0010_0001 => Some((3, Instruction::SBRG(vreg(lower_bot), vimm(1), vimm(2)))),
                0b_0010_0010 => Some((3, Instruction::SBRG(vimm(1), vreg(lower_bot), vimm(2)))),
                0b_0010_0011 => Some((3, Instruction::SBRG(vimm(1), vimm(2), vreg(lower_bot)))),

                0b_0010_0100 => Some((2, Instruction::SBGE(vreg(lower_bot), xreg1, xreg2))),
                0b_0010_0101 => Some((3, Instruction::SBGE(vreg(lower_bot), vimm(1), vimm(2)))),
                0b_0010_0110 => Some((3, Instruction::SBGE(vimm(1), vreg(lower_bot), vimm(2)))),
                0b_0010_0111 => Some((3, Instruction::SBGE(vimm(1), vimm(2), vreg(lower_bot)))),

                0b_0010_1000 => Some((2, Instruction::SBRL(vreg(lower_bot), xreg1, xreg2))),
                0b_0010_1001 => Some((3, Instruction::SBRL(vreg(lower_bot), vimm(1), vimm(2)))),
                0b_0010_1010 => Some((3, Instruction::SBRL(vimm(1), vreg(lower_bot), vimm(2)))),
                0b_0010_1011 => Some((3, Instruction::SBRL(vimm(1), vimm(2), vreg(lower_bot)))),

                0b_0010_1100 => Some((2, Instruction::SBLE(vreg(lower_bot), xreg1, xreg2))),
                0b_0010_1101 => Some((3, Instruction::SBLE(vreg(lower_bot), vimm(1), vimm(2)))),
                0b_0010_1110 => Some((3, Instruction::SBLE(vimm(1), vreg(lower_bot), vimm(2)))),
                0b_0010_1111 => Some((3, Instruction::SBLE(vimm(1), vimm(2), vreg(lower_bot)))),

                0b_0011_0000 => Some((2, Instruction::BRE(vreg(lower_bot), xreg1, xreg2))),
                0b_0011_0001 => Some((3, Instruction::BRE(vreg(lower_bot), vimm(1), vimm(2)))),
                0b_0011_0010 => Some((3, Instruction::BRE(vimm(1), vreg(lower_bot), vimm(2)))),
                0b_0011_0011 => Some((3, Instruction::BRE(vimm(1), vimm(2), vreg(lower_bot)))),

                0b_0011_0100 => Some((2, Instruction::BNE(vreg(lower_bot), xreg1, xreg2))),
                0b_0011_0101 => Some((3, Instruction::BNE(vreg(lower_bot), vimm(1), vimm(2)))),
                0b_0011_0110 => Some((3, Instruction::BNE(vimm(1), vreg(lower_bot), vimm(2)))),
                0b_0011_0111 => Some((3, Instruction::BNE(vimm(1), vimm(2), vreg(lower_bot)))),

                0b_0011_1000 => Some((2, Instruction::BRC(vreg(lower_bot), xreg1, xreg2))),
                0b_0011_1001 => Some((3, Instruction::BRC(vreg(lower_bot), vimm(1), vimm(2)))),
                0b_0011_1010 => Some((3, Instruction::BRC(vimm(1), vreg(lower_bot), vimm(2)))),
                0b_0011_1011 => Some((3, Instruction::BRC(vimm(1), vimm(2), vreg(lower_bot)))),

                0b_0011_1100 => Some((2, Instruction::BNC(vreg(lower_bot), xreg1, xreg2))),
                0b_0011_1101 => Some((3, Instruction::BNC(vreg(lower_bot), vimm(1), vimm(2)))),
                0b_0011_1110 => Some((3, Instruction::BNC(vimm(1), vreg(lower_bot), vimm(2)))),
                0b_0011_1111 => Some((3, Instruction::BNC(vimm(1), vimm(2), vreg(lower_bot)))),

                0b_0100_0000 => Some((2, Instruction::BRP(vreg(lower_bot), vimm(1)))),
                0b_0100_0001 => Some((2, Instruction::BRP(vimm(1), vreg(lower_bot)))),
                0b_0100_0010 => Some((2, Instruction::BRN(vreg(lower_bot), vimm(1)))),
                0b_0100_0011 => Some((2, Instruction::BRN(vimm(1), vreg(lower_bot)))),

                0b_0100_0100 => Some((2, Instruction::BRZ(vreg(lower_bot), vimm(1)))),
                0b_0100_0101 => Some((2, Instruction::BRZ(vimm(1), vreg(lower_bot)))),
                0b_0100_0110 => Some((2, Instruction::BNZ(vreg(lower_bot), vimm(1)))),
                0b_0100_0111 => Some((2, Instruction::BNZ(vimm(1), vreg(lower_bot)))),

                0b_0100_1000 => Some((2, Instruction::ADD(reg(lower_bot), xreg1, xreg2))),
                0b_0100_1001 => Some((3, Instruction::ADD(reg(lower_bot), vimm(1), vimm(2)))),
                0b_0100_1010 => Some((2, Instruction::SUB(reg(lower_bot), xreg1, xreg2))),
                0b_0100_1011 => Some((3, Instruction::SUB(reg(lower_bot), vimm(1), vimm(2)))),

                0b_0100_1100 => Some((2, Instruction::INC(reg(lower_bot), vimm(1)))),
                0b_0100_1101 => Some((2, Instruction::DEC(reg(lower_bot), vimm(1)))),
                0b_0100_1110 => Some((2, Instruction::NEG(reg(lower_bot), vimm(1)))),
                0b_0100_1111 => Some((2, Instruction::NOT(reg(lower_bot), vimm(1)))),

                0b_0101_0000 => Some((2, Instruction::CPY(vreg(lower_bot), vimm(1)))),
                0b_0101_0001 => Some((2, Instruction::CPY(vimm(1), vreg(lower_bot)))),

                0b_0101_0010 => Some((2, Instruction::MLT(reg(lower_bot), xreg1, xreg2))),
                0b_0101_0011 => Some((3, Instruction::MLT(reg(lower_bot), vimm(1), vimm(2)))),

                0b_0101_0100 => Some((2, Instruction::DIV(reg(lower_bot), xreg1, xreg2))),
                0b_0101_0101 => Some((3, Instruction::DIV(reg(lower_bot), vimm(1), vimm(2)))),
                0b_0101_0110 => Some((2, Instruction::SDIV(reg(lower_bot), xreg1, xreg2))),
                0b_0101_0111 => Some((3, Instruction::SDIV(reg(lower_bot), vimm(1), vimm(2)))),

                0b_0101_1000 => Some((2, Instruction::MOD(reg(lower_bot), xreg1, xreg2))),
                0b_0101_1001 => Some((3, Instruction::MOD(reg(lower_bot), vimm(1), vimm(2)))),
                0b_0101_1010 => Some((2, Instruction::SMOD(reg(lower_bot), xreg1, xreg2))),
                0b_0101_1011 => Some((3, Instruction::SMOD(reg(lower_bot), vimm(1), vimm(2)))),

                0b_0101_1100 => Some((2, Instruction::AND(reg(lower_bot), xreg1, xreg2))),
                0b_0101_1101 => Some((3, Instruction::AND(reg(lower_bot), vimm(1), vimm(2)))),
                0b_0101_1110 => Some((2, Instruction::NAND(reg(lower_bot), xreg1, xreg2))),
                0b_0101_1111 => Some((3, Instruction::NAND(reg(lower_bot), vimm(1), vimm(2)))),

                0b_0110_0000 => Some((2, Instruction::OR(reg(lower_bot), xreg1, xreg2))),
                0b_0110_0001 => Some((3, Instruction::OR(reg(lower_bot), vimm(1), vimm(2)))),
                0b_0110_0010 => Some((2, Instruction::NOR(reg(lower_bot), xreg1, xreg2))),
                0b_0110_0011 => Some((3, Instruction::NOR(reg(lower_bot), vimm(1), vimm(2)))),

                0b_0110_0100 => Some((2, Instruction::XOR(reg(lower_bot), xreg1, xreg2))),
                0b_0110_0101 => Some((3, Instruction::XOR(reg(lower_bot), vimm(1), vimm(2)))),
                0b_0110_0110 => Some((2, Instruction::XNOR(reg(lower_bot), xreg1, xreg2))),
                0b_0110_0111 => Some((3, Instruction::XNOR(reg(lower_bot), vimm(1), vimm(2)))),

                0b_0110_1000 => Some((2, Instruction::LLOD(reg(lower_bot), xreg1, xreg2))),
                0b_0110_1001 => Some((3, Instruction::LLOD(reg(lower_bot), vimm(1), vimm(2)))),

                0b_0110_1010 => Some((2, Instruction::IMM(reg(lower_bot), imm(1)))),

                0b_0110_1011 => Some((2, Instruction::RSH(reg(lower_bot), vimm(1)))),
                0b_0110_1100 => Some((2, Instruction::LSH(reg(lower_bot), vimm(1)))),
                0b_0110_1101 => Some((2, Instruction::SRS(reg(lower_bot), vimm(1)))),

                0b_0110_1110 => Some((2, Instruction::BSR(reg(lower_bot), xreg1, xreg2))),
                0b_0110_1111 => Some((3, Instruction::BSR(reg(lower_bot), vimm(1), vimm(2)))),
                0b_0111_0000 => Some((2, Instruction::BSL(reg(lower_bot), xreg1, xreg2))),
                0b_0111_0001 => Some((3, Instruction::BSL(reg(lower_bot), vimm(1), vimm(2)))),
                0b_0111_0010 => Some((2, Instruction::BSS(reg(lower_bot), xreg1, xreg2))),
                0b_0111_0011 => Some((3, Instruction::BSS(reg(lower_bot), vimm(1), vimm(2)))),

                0b_0111_0100 => Some((2, Instruction::SETG(reg(lower_bot), xreg1, xreg2))),
                0b_0111_0101 => Some((3, Instruction::SETG(reg(lower_bot), vimm(1), vimm(2)))),
                0b_0111_0110 => Some((2, Instruction::SETGE(reg(lower_bot), xreg1, xreg2))),
                0b_0111_0111 => Some((3, Instruction::SETGE(reg(lower_bot), vimm(1), vimm(2)))),

                0b_0111_1000 => Some((2, Instruction::SETL(reg(lower_bot), xreg1, xreg2))),
                0b_0111_1001 => Some((3, Instruction::SETL(reg(lower_bot), vimm(1), vimm(2)))),
                0b_0111_1010 => Some((2, Instruction::SETLE(reg(lower_bot), xreg1, xreg2))),
                0b_0111_1011 => Some((3, Instruction::SETLE(reg(lower_bot), vimm(1), vimm(2)))),

                0b_0111_1100 => Some((2, Instruction::SETE(reg(lower_bot), xreg1, xreg2))),
                0b_0111_1101 => Some((3, Instruction::SETE(reg(lower_bot), vimm(1), vimm(2)))),
                0b_0111_1110 => Some((2, Instruction::SETNE(reg(lower_bot), xreg1, xreg2))),
                0b_0111_1111 => Some((3, Instruction::SETNE(reg(lower_bot), vimm(1), vimm(2)))),

                0b_1000_0000 => Some((2, Instruction::SETC(reg(lower_bot), xreg1, xreg2))),
                0b_1000_0001 => Some((3, Instruction::SETC(reg(lower_bot), vimm(1), vimm(2)))),
                0b_1000_0010 => Some((2, Instruction::SETNC(reg(lower_bot), xreg1, xreg2))),
                0b_1000_0011 => Some((3, Instruction::SETNC(reg(lower_bot), vimm(1), vimm(2)))),

                0b_1000_0100 => Some((2, Instruction::SSETG(reg(lower_bot), xreg1, xreg2))),
                0b_1000_0101 => Some((3, Instruction::SSETG(reg(lower_bot), vimm(1), vimm(2)))),
                0b_1000_0110 => Some((2, Instruction::SSETGE(reg(lower_bot), xreg1, xreg2))),
                0b_1000_0111 => Some((3, Instruction::SSETGE(reg(lower_bot), vimm(1), vimm(2)))),

                0b_1000_1000 => Some((2, Instruction::SSETL(reg(lower_bot), xreg1, xreg2))),
                0b_1000_1001 => Some((3, Instruction::SSETL(reg(lower_bot), vimm(1), vimm(2)))),
                0b_1000_1010 => Some((2, Instruction::SSETLE(reg(lower_bot), xreg1, xreg2))),
                0b_1000_1011 => Some((3, Instruction::SSETLE(reg(lower_bot), vimm(1), vimm(2)))),

                0b_1000_1100 => Some((2, Instruction::LSTR(vreg(lower_bot), xreg1, xreg2))),
                0b_1000_1101 => Some((3, Instruction::LSTR(vreg(lower_bot), vimm(1), vimm(2)))),
                0b_1000_1110 => Some((3, Instruction::LSTR(vimm(1), vreg(lower_bot), vimm(2)))),
                0b_1000_1111 => Some((3, Instruction::LSTR(vimm(1), vimm(2), vreg(lower_bot)))),

                0b_1001_0000 => Some((1, Instruction::PSH(vreg(lower_bot)))),
                0b_1001_0001 => Some((1, Instruction::POP(reg(lower_bot)))),

                0b_1001_0010 => Some((
                    1,
                    Instruction::CAL {
                        dest: vreg(lower_bot),
                        ret: (pc + 1).into(),
                    },
                )),
                0b_1001_0011 => Some((1, Instruction::JMP(vreg(lower_bot)))),

                0b_1001_0100 => Some((2, Instruction::LOD(reg(lower_bot), vimm(1)))),
                0b_1001_0101 => Some((2, Instruction::STR(vreg(lower_bot), vimm(1)))),
                0b_1001_0110 => Some((2, Instruction::STR(vimm(1), vreg(lower_bot)))),
                0b_1001_0111 => None,

                0b_1001_1000 => Some((2, Instruction::BOD(vreg(lower_bot), vimm(1)))),
                0b_1001_1001 => Some((2, Instruction::BOD(vimm(1), vreg(lower_bot)))),
                0b_1001_1010 => Some((2, Instruction::BEV(vreg(lower_bot), vimm(1)))),
                0b_1001_1011 => Some((2, Instruction::BEV(vimm(1), vreg(lower_bot)))),
                0b_1001_1100..=u8::MAX => None,
            },
            0b_0001_0000..=0b0111_1110 => None,
            0b_0111_1111 => match lower_top {
                u4::b_0000
                | u4::b_0001
                | u4::b_0010
                | u4::b_0011
                | u4::b_0100
                | u4::b_0101
                | u4::b_0110
                | u4::b_0111
                | u4::b_1000
                | u4::b_1001
                | u4::b_1010
                | u4::b_1011 => None,
                u4::b_1100 => Some((
                    2,
                    Instruction::OUT(
                        ((0b00 << 4) | lower_bot as u8)
                            .try_into()
                            .expect("Reserved Port"),
                        vimm(1),
                    ),
                )),
                u4::b_1101 => Some((
                    2,
                    Instruction::OUT(
                        ((0b01 << 4) | lower_bot as u8)
                            .try_into()
                            .expect("Reserved Port"),
                        vimm(1),
                    ),
                )),
                u4::b_1110 => Some((
                    2,
                    Instruction::OUT(
                        ((0b10 << 4) | lower_bot as u8)
                            .try_into()
                            .expect("Reserved Port"),
                        vimm(1),
                    ),
                )),
                u4::b_1111 => Some((
                    2,
                    Instruction::OUT(
                        ((0b11 << 4) | lower_bot as u8)
                            .try_into()
                            .expect("Reserved Port"),
                        vimm(1),
                    ),
                )),
            },
            0b_1000_0000 => Some((
                1,
                Instruction::IN(
                    reg(lower_top),
                    ((0b00 << 4) | lower_bot as u8)
                        .try_into()
                        .expect("Reserved Port"),
                ),
            )),
            0b_1000_0001 => Some((
                1,
                Instruction::IN(
                    reg(lower_top),
                    ((0b01 << 4) | lower_bot as u8)
                        .try_into()
                        .expect("Reserved Port"),
                ),
            )),
            0b_1000_0010 => Some((
                1,
                Instruction::IN(
                    reg(lower_top),
                    ((0b10 << 4) | lower_bot as u8)
                        .try_into()
                        .expect("Reserved Port"),
                ),
            )),
            0b_1000_0011 => Some((
                1,
                Instruction::IN(
                    reg(lower_top),
                    ((0b11 << 4) | lower_bot as u8)
                        .try_into()
                        .expect("Reserved Port"),
                ),
            )),
            0b_1000_0100 => Some((
                1,
                Instruction::OUT(
                    ((0b00 << 4) | lower_top as u8)
                        .try_into()
                        .expect("Reserved Port"),
                    vreg(lower_bot),
                ),
            )),
            0b_1000_0101 => Some((
                1,
                Instruction::OUT(
                    ((0b01 << 4) | lower_top as u8)
                        .try_into()
                        .expect("Reserved Port"),
                    vreg(lower_bot),
                ),
            )),
            0b_1000_0110 => Some((
                1,
                Instruction::OUT(
                    ((0b10 << 4) | lower_top as u8)
                        .try_into()
                        .expect("Reserved Port"),
                    vreg(lower_bot),
                ),
            )),
            0b_1000_0111 => Some((
                1,
                Instruction::OUT(
                    ((0b11 << 4) | lower_top as u8)
                        .try_into()
                        .expect("Reserved Port"),
                    vreg(lower_bot),
                ),
            )),

            0b_1000_1000 => Some((1, Instruction::LOD(reg(lower_top), vreg(lower_bot)))),
            0b_1000_1001 => Some((1, Instruction::STR(vreg(lower_top), vreg(lower_bot)))),

            0b_1000_1010 => Some((
                2,
                Instruction::LLOD(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1000_1011 => Some((
                2,
                Instruction::LLOD(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),

            0b_1000_1100 => Some((1, Instruction::RSH(reg(lower_top), vreg(lower_bot)))),
            0b_1000_1101 => Some((1, Instruction::LSH(reg(lower_top), vreg(lower_bot)))),
            0b_1000_1110 => Some((1, Instruction::SRS(reg(lower_top), vreg(lower_bot)))),

            0b_1000_1111 => Some((
                2,
                Instruction::LSTR(vreg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1001_0000 => Some((
                2,
                Instruction::LSTR(vreg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1001_0001 => Some((
                2,
                Instruction::LSTR(vimm(1), vreg(lower_top), vreg(lower_bot)),
            )),

            0b_1001_0010 => Some((
                2,
                Instruction::ADD(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1001_0011 => Some((
                2,
                Instruction::ADD(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1001_0100 => Some((
                2,
                Instruction::SUB(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1001_0101 => Some((
                2,
                Instruction::SUB(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),

            0b_1001_0110 => Some((1, Instruction::INC(reg(lower_top), vreg(lower_bot)))),
            0b_1001_0111 => Some((1, Instruction::DEC(reg(lower_top), vreg(lower_bot)))),
            0b_1001_1000 => Some((1, Instruction::NEG(reg(lower_top), vreg(lower_bot)))),
            0b_1001_1001 => Some((1, Instruction::NOT(reg(lower_top), vreg(lower_bot)))),

            0b_1001_1010 => Some((
                2,
                Instruction::MLT(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1001_1011 => Some((
                2,
                Instruction::MLT(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),

            0b_1001_1100 => Some((
                2,
                Instruction::DIV(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1001_1101 => Some((
                2,
                Instruction::DIV(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1001_1110 => Some((
                2,
                Instruction::SDIV(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1001_1111 => Some((
                2,
                Instruction::SDIV(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),

            0b_1010_0000 => Some((
                2,
                Instruction::MOD(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1010_0001 => Some((
                2,
                Instruction::MOD(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1010_0010 => Some((
                2,
                Instruction::SMOD(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1010_0011 => Some((
                2,
                Instruction::SMOD(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),

            0b_1010_0100 => Some((
                2,
                Instruction::AND(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1010_0101 => Some((
                2,
                Instruction::AND(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1010_0110 => Some((
                2,
                Instruction::NAND(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1010_0111 => Some((
                2,
                Instruction::NAND(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),

            0b_1010_1000 => Some((2, Instruction::OR(reg(lower_top), vreg(lower_bot), vimm(1)))),
            0b_1010_1001 => Some((2, Instruction::OR(reg(lower_top), vimm(1), vreg(lower_bot)))),
            0b_1010_1010 => Some((
                2,
                Instruction::NOR(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1010_1011 => Some((
                2,
                Instruction::NOR(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),

            0b_1010_1100 => Some((
                2,
                Instruction::XOR(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1010_1101 => Some((
                2,
                Instruction::XOR(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1010_1110 => Some((
                2,
                Instruction::XNOR(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1010_1111 => Some((
                2,
                Instruction::XNOR(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),

            0b_1011_0000 => Some((
                2,
                Instruction::BSL(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1011_0001 => Some((
                2,
                Instruction::BSL(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1011_0010 => Some((
                2,
                Instruction::BSR(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1011_0011 => Some((
                2,
                Instruction::BSR(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1011_0100 => Some((
                2,
                Instruction::BSS(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1011_0101 => Some((
                2,
                Instruction::BSS(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),

            0b_1011_0110 => Some((
                2,
                Instruction::BRC(vreg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1011_0111 => Some((
                2,
                Instruction::BRC(vreg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1011_1000 => Some((
                2,
                Instruction::BRC(vimm(1), vreg(lower_top), vreg(lower_bot)),
            )),

            0b_1011_1001 => Some((
                2,
                Instruction::BNC(vreg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1011_1010 => Some((
                2,
                Instruction::BNC(vreg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1011_1011 => Some((
                2,
                Instruction::BNC(vimm(1), vreg(lower_top), vreg(lower_bot)),
            )),

            0b_1011_1100 => Some((
                2,
                Instruction::BRE(vreg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1011_1101 => Some((
                2,
                Instruction::BRE(vreg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1011_1110 => Some((
                2,
                Instruction::BRE(vimm(1), vreg(lower_top), vreg(lower_bot)),
            )),

            0b_1011_1111 => Some((
                2,
                Instruction::BNE(vreg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1100_0000 => Some((
                2,
                Instruction::BNE(vreg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1100_0001 => Some((
                2,
                Instruction::BNE(vimm(1), vreg(lower_top), vreg(lower_bot)),
            )),

            0b_1100_0010 => Some((1, Instruction::BOD(vreg(lower_top), vreg(lower_bot)))),
            0b_1100_0011 => Some((1, Instruction::BEV(vreg(lower_top), vreg(lower_bot)))),

            0b_1100_0100 => Some((1, Instruction::BRZ(vreg(lower_top), vreg(lower_bot)))),
            0b_1100_0101 => Some((1, Instruction::BNZ(vreg(lower_top), vreg(lower_bot)))),

            0b_1100_0110 => Some((1, Instruction::BRP(vreg(lower_top), vreg(lower_bot)))),
            0b_1100_0111 => Some((1, Instruction::BRN(vreg(lower_top), vreg(lower_bot)))),

            0b_1100_1000 => Some((
                2,
                Instruction::SBRG(vreg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1100_1001 => Some((
                2,
                Instruction::SBRG(vreg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1100_1010 => Some((
                2,
                Instruction::SBRG(vimm(1), vreg(lower_top), vreg(lower_bot)),
            )),

            0b_1100_1011 => Some((
                2,
                Instruction::SBGE(vreg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1100_1100 => Some((
                2,
                Instruction::SBGE(vreg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1100_1101 => Some((
                2,
                Instruction::SBGE(vimm(1), vreg(lower_top), vreg(lower_bot)),
            )),

            0b_1100_1110 => Some((
                2,
                Instruction::SBRL(vreg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1100_1111 => Some((
                2,
                Instruction::SBRL(vreg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1101_0000 => Some((
                2,
                Instruction::SBRL(vimm(1), vreg(lower_top), vreg(lower_bot)),
            )),

            0b_1101_0001 => Some((
                2,
                Instruction::SBLE(vreg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1101_0010 => Some((
                2,
                Instruction::SBLE(vreg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1101_0011 => Some((
                2,
                Instruction::SBLE(vimm(1), vreg(lower_top), vreg(lower_bot)),
            )),

            0b_1101_0100 => Some((
                2,
                Instruction::SETE(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1101_0101 => Some((
                2,
                Instruction::SETE(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1101_0110 => Some((
                2,
                Instruction::SETNE(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1101_0111 => Some((
                2,
                Instruction::SETNE(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),

            0b_1101_1000 => Some((
                2,
                Instruction::SETC(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1101_1001 => Some((
                2,
                Instruction::SETC(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1101_1010 => Some((
                2,
                Instruction::SETNC(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1101_1011 => Some((
                2,
                Instruction::SETNC(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),

            0b_1101_1100 => Some((
                2,
                Instruction::SETG(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1101_1101 => Some((
                2,
                Instruction::SETG(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1101_1110 => Some((
                2,
                Instruction::SETGE(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1101_1111 => Some((
                2,
                Instruction::SETGE(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),

            0b_1110_0000 => Some((
                2,
                Instruction::SETL(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1110_0001 => Some((
                2,
                Instruction::SETL(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1110_0010 => Some((
                2,
                Instruction::SETLE(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1110_0011 => Some((
                2,
                Instruction::SETLE(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),

            0b_1110_0100 => Some((
                2,
                Instruction::SSETG(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1110_0101 => Some((
                2,
                Instruction::SSETG(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1110_0110 => Some((
                2,
                Instruction::SSETGE(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1110_0111 => Some((
                2,
                Instruction::SSETGE(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),

            0b_1110_1000 => Some((
                2,
                Instruction::SSETL(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1110_1001 => Some((
                2,
                Instruction::SSETL(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1110_1010 => Some((
                2,
                Instruction::SSETLE(reg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1110_1011 => Some((
                2,
                Instruction::SSETLE(reg(lower_top), vimm(1), vreg(lower_bot)),
            )),

            0b_1110_1100 => Some((1, Instruction::MOV(reg(lower_top), reg(lower_bot)))),
            0b_1110_1101 => Some((1, Instruction::CPY(vreg(lower_top), vreg(lower_bot)))),

            0b_1110_1110 => None,
            0b_1110_1111 => None,

            0b_1111_0000 => Some((
                2,
                Instruction::BRG(vreg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1111_0001 => Some((
                2,
                Instruction::BRG(vreg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1111_0010 => Some((
                2,
                Instruction::BRG(vimm(1), vreg(lower_top), vreg(lower_bot)),
            )),

            0b_1111_0011 => Some((
                2,
                Instruction::BGE(vreg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1111_0100 => Some((
                2,
                Instruction::BGE(vreg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1111_0101 => Some((
                2,
                Instruction::BGE(vimm(1), vreg(lower_top), vreg(lower_bot)),
            )),

            0b_1111_0110 => Some((
                2,
                Instruction::BRL(vreg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1111_0111 => Some((
                2,
                Instruction::BRL(vreg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1111_1000 => Some((
                2,
                Instruction::BRL(vimm(1), vreg(lower_top), vreg(lower_bot)),
            )),

            0b_1111_1001 => Some((
                2,
                Instruction::BLE(vreg(lower_top), vreg(lower_bot), vimm(1)),
            )),
            0b_1111_1010 => Some((
                2,
                Instruction::BLE(vreg(lower_top), vimm(1), vreg(lower_bot)),
            )),
            0b_1111_1011 => Some((
                2,
                Instruction::BLE(vimm(1), vreg(lower_top), vreg(lower_bot)),
            )),

            0b_1111_1100 => None,
            0b_1111_1101 => None,
            0b_1111_1110 => None,
            0b_1111_1111 => None,
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
    ) -> ControlFlow {
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

        macro_rules! push {
            ($val:expr) => {{
                let sp: u16 = registers[Register::SP].into();
                if sp < bottom_of_stack {
                    panic!("Stack Overflow");
                }
                registers[Register::SP] -= 1;
                memory[registers[Register::SP]] = $val;
            }};
        }

        macro_rules! pop {
            () => {{
                if {
                    let sp: u16 = registers[Register::SP].into();
                    sp
                } >= memory.len()
                {
                    panic!("Stack Underflow");
                }
                registers[Register::SP] += 1;
                let val = memory[registers[Register::SP]];
                val
            }};
        }

        match self {
            Instruction::NOP => ControlFlow::Next,
            Instruction::HLT => ControlFlow::Halted,
            Instruction::IN(dest, port) => {
                registers[dest] = ports[port].read();
                ControlFlow::Next
            }
            Instruction::OUT(port, src) => {
                ports[port].write(eval!(src));
                ControlFlow::Next
            }
            Instruction::CAL { dest, ret } => {
                push!(ret);
                ControlFlow::Jump(eval!(dest))
            }
            Instruction::RET => ControlFlow::Jump(pop!()),
            Instruction::PSH(src) => {
                push!(eval!(src));
                ControlFlow::Next
            }
            Instruction::POP(dest) => {
                registers[dest] = pop!();
                ControlFlow::Next
            }
            Instruction::LOD(dest, src) => {
                registers[dest] = memory[eval!(src)];
                ControlFlow::Next
            }
            Instruction::LLOD(dest, src1, src2) => {
                registers[dest] = memory[eval!(src1) + eval!(src2)];
                ControlFlow::Next
            }
            Instruction::STR(dest, src) => {
                memory[eval!(dest)] = eval!(src);
                ControlFlow::Next
            }
            Instruction::LSTR(dest1, dest2, src2) => {
                memory[eval!(dest1) + eval!(dest2)] = eval!(src2);
                ControlFlow::Next
            }

            Instruction::CPY(dest, src) => {
                memory[eval!(dest)] = memory[eval!(src)];
                ControlFlow::Next
            }

            Instruction::IMM(dest, src) => {
                registers[dest] = src;
                ControlFlow::Next
            }
            Instruction::MOV(dest, src) => {
                registers[dest] = registers[src];
                ControlFlow::Next
            }

            Instruction::ADD(dest, src1, src2) => {
                registers[dest] = eval!(src1) + eval!(src2);
                ControlFlow::Next
            }
            Instruction::SUB(dest, src1, src2) => {
                registers[dest] = eval!(src1) - eval!(src2);
                ControlFlow::Next
            }
            Instruction::INC(dest, src) => {
                registers[dest] = eval!(src) + 1u16.into();
                ControlFlow::Next
            }
            Instruction::DEC(dest, src) => {
                registers[dest] = eval!(src) - 1u16.into();
                ControlFlow::Next
            }
            Instruction::NEG(dest, src) => {
                registers[dest] = -eval!(src);
                ControlFlow::Next
            }
            Instruction::NOT(dest, src) => {
                registers[dest] = !eval!(src);
                ControlFlow::Next
            }

            Instruction::MLT(dest, src1, src2) => {
                registers[dest] = eval!(src1) * eval!(src2);
                ControlFlow::Next
            }
            Instruction::DIV(dest, src1, src2) => {
                registers[dest] = Word::from(eval!(src1: u16) / eval!(src2: u16));
                ControlFlow::Next
            }
            Instruction::SDIV(dest, src1, src2) => {
                registers[dest] = Word::from(eval!(src1: i16) / eval!(src2: i16));
                ControlFlow::Next
            }
            Instruction::MOD(dest, src1, src2) => {
                registers[dest] = Word::from(eval!(src1: u16) % eval!(src2: u16));
                ControlFlow::Next
            }
            Instruction::SMOD(dest, src1, src2) => {
                registers[dest] = Word::from(eval!(src1: i16) % eval!(src2: i16));
                ControlFlow::Next
            }

            Instruction::AND(dest, src1, src2) => {
                registers[dest] = eval!(src1) & eval!(src2);
                ControlFlow::Next
            }
            Instruction::NAND(dest, src1, src2) => {
                registers[dest] = !(eval!(src1) & eval!(src2));
                ControlFlow::Next
            }

            Instruction::OR(dest, src1, src2) => {
                registers[dest] = eval!(src1) | eval!(src2);
                ControlFlow::Next
            }
            Instruction::NOR(dest, src1, src2) => {
                registers[dest] = !(eval!(src1) | eval!(src2));
                ControlFlow::Next
            }

            Instruction::XOR(dest, src1, src2) => {
                registers[dest] = eval!(src1) ^ eval!(src2);
                ControlFlow::Next
            }
            Instruction::XNOR(dest, src1, src2) => {
                registers[dest] = !(eval!(src1) ^ eval!(src2));
                ControlFlow::Next
            }

            Instruction::LSH(dest, src) => {
                registers[dest] = eval!(src) << 1;
                ControlFlow::Next
            }
            Instruction::RSH(dest, src) => {
                registers[dest] = Word::from(eval!(src: u16) >> 1);
                ControlFlow::Next
            }
            Instruction::SRS(dest, src) => {
                registers[dest] = Word::from(eval!(src: i16) >> 1);
                ControlFlow::Next
            }

            // shifting by >= u16::BITS will produce "overflow" and panic for some reason
            // so therefore i have to cmp::min(15, x) to do essentialy a saturating shift
            // unchecked_shr/shl and overflowing_shr/shl are not suitable here
            Instruction::BSL(dest, src1, src2) => {
                registers[dest] = eval!(src1) << Word::from(cmp::min(15, eval!(src2: u16)));
                ControlFlow::Next
            }
            Instruction::BSR(dest, src1, src2) => {
                registers[dest] = Word::from(eval!(src1: u16) >> cmp::min(15, eval!(src2: u16)));
                ControlFlow::Next
            }
            Instruction::BSS(dest, src1, src2) => {
                registers[dest] = Word::from(eval!(src1: i16) >> cmp::min(15, eval!(src2: u16)));
                ControlFlow::Next
            }

            Instruction::JMP(dest) => ControlFlow::Jump(eval!(dest)),
            Instruction::BRG(dest, src1, src2) => {
                if eval!(src1: u16) > eval!(src2: u16) {
                    ControlFlow::Jump(eval!(dest))
                } else {
                    ControlFlow::Next
                }
            }
            Instruction::BGE(dest, src1, src2) => {
                if eval!(src1: u16) >= eval!(src2: u16) {
                    ControlFlow::Jump(eval!(dest))
                } else {
                    ControlFlow::Next
                }
            }
            Instruction::BRL(dest, src1, src2) => {
                if eval!(src1: u16) < eval!(src2: u16) {
                    ControlFlow::Jump(eval!(dest))
                } else {
                    ControlFlow::Next
                }
            }
            Instruction::BLE(dest, src1, src2) => {
                if eval!(src1: u16) <= eval!(src2: u16) {
                    ControlFlow::Jump(eval!(dest))
                } else {
                    ControlFlow::Next
                }
            }

            Instruction::SBRG(dest, src1, src2) => {
                if eval!(src1: i16) > eval!(src2: i16) {
                    ControlFlow::Jump(eval!(dest))
                } else {
                    ControlFlow::Next
                }
            }
            Instruction::SBGE(dest, src1, src2) => {
                if eval!(src1: i16) >= eval!(src2: i16) {
                    ControlFlow::Jump(eval!(dest))
                } else {
                    ControlFlow::Next
                }
            }
            Instruction::SBRL(dest, src1, src2) => {
                if eval!(src1: i16) < eval!(src2: i16) {
                    ControlFlow::Jump(eval!(dest))
                } else {
                    ControlFlow::Next
                }
            }
            Instruction::SBLE(dest, src1, src2) => {
                if eval!(src1: i16) <= eval!(src2: i16) {
                    ControlFlow::Jump(eval!(dest))
                } else {
                    ControlFlow::Next
                }
            }

            Instruction::BRE(dest, src1, src2) => {
                if eval!(src1) == eval!(src2) {
                    ControlFlow::Jump(eval!(dest))
                } else {
                    ControlFlow::Next
                }
            }
            Instruction::BNE(dest, src1, src2) => {
                if eval!(src1) != eval!(src2) {
                    ControlFlow::Jump(eval!(dest))
                } else {
                    ControlFlow::Next
                }
            }

            Instruction::BRC(dest, src1, src2) => {
                if eval!(src1: u16).checked_add(eval!(src2: u16)).is_none() {
                    ControlFlow::Jump(eval!(dest))
                } else {
                    ControlFlow::Next
                }
            }
            Instruction::BNC(dest, src1, src2) => {
                if eval!(src1: u16).checked_add(eval!(src2: u16)).is_some() {
                    ControlFlow::Jump(eval!(dest))
                } else {
                    ControlFlow::Next
                }
            }

            Instruction::BRZ(dest, src) => {
                if eval!(src) == 0u16 {
                    ControlFlow::Jump(eval!(dest))
                } else {
                    ControlFlow::Next
                }
            }
            Instruction::BNZ(dest, src) => {
                if eval!(src) != 0u16 {
                    ControlFlow::Jump(eval!(dest))
                } else {
                    ControlFlow::Next
                }
            }

            Instruction::BRP(dest, src) => {
                if eval!(src: i16) >= 0 {
                    ControlFlow::Jump(eval!(dest))
                } else {
                    ControlFlow::Next
                }
            }
            Instruction::BRN(dest, src) => {
                if eval!(src: i16) < 0 {
                    ControlFlow::Jump(eval!(dest))
                } else {
                    ControlFlow::Next
                }
            }

            Instruction::BOD(dest, src) => {
                if eval!(src: u16) & 1 == 1 {
                    ControlFlow::Jump(eval!(dest))
                } else {
                    ControlFlow::Next
                }
            }
            Instruction::BEV(dest, src) => {
                if eval!(src: u16) & 1 == 0 {
                    ControlFlow::Jump(eval!(dest))
                } else {
                    ControlFlow::Next
                }
            }

            Instruction::SETG(dest, src1, src2) => {
                registers[dest] = (eval!(src1: u16) > eval!(src2: u16)).into();
                ControlFlow::Next
            }
            Instruction::SETGE(dest, src1, src2) => {
                registers[dest] = (eval!(src1: u16) >= eval!(src2: u16)).into();
                ControlFlow::Next
            }
            Instruction::SETL(dest, src1, src2) => {
                registers[dest] = (eval!(src1: u16) < eval!(src2: u16)).into();
                ControlFlow::Next
            }
            Instruction::SETLE(dest, src1, src2) => {
                registers[dest] = (eval!(src1: u16) <= eval!(src2: u16)).into();
                ControlFlow::Next
            }

            Instruction::SSETG(dest, src1, src2) => {
                registers[dest] = (eval!(src1: i16) > eval!(src2: i16)).into();
                ControlFlow::Next
            }
            Instruction::SSETGE(dest, src1, src2) => {
                registers[dest] = (eval!(src1: i16) >= eval!(src2: i16)).into();
                ControlFlow::Next
            }
            Instruction::SSETL(dest, src1, src2) => {
                registers[dest] = (eval!(src1: i16) < eval!(src2: i16)).into();
                ControlFlow::Next
            }
            Instruction::SSETLE(dest, src1, src2) => {
                registers[dest] = (eval!(src1: i16) <= eval!(src2: i16)).into();
                ControlFlow::Next
            }

            Instruction::SETE(dest, src1, src2) => {
                registers[dest] = (eval!(src1) == eval!(src2)).into();
                ControlFlow::Next
            }
            Instruction::SETNE(dest, src1, src2) => {
                registers[dest] = (eval!(src1) != eval!(src2)).into();
                ControlFlow::Next
            }

            Instruction::SETC(dest, src1, src2) => {
                registers[dest] = eval!(src1: u16)
                    .checked_add(eval!(src2: u16))
                    .is_none()
                    .into();
                ControlFlow::Next
            }
            Instruction::SETNC(dest, src1, src2) => {
                registers[dest] = eval!(src1: u16)
                    .checked_add(eval!(src2: u16))
                    .is_some()
                    .into();
                ControlFlow::Next
            }
        }
    }
}
