# URCLvm

A project that is in its very early stages, where we are making a URCL-based ISA for interpreting directly. One of the huge features of this from my understanding will be self-modifying code, and it should also be able to be interpreted in URCL.

This is not "the standard" or anything (_yet_), this is my proposal for what we could use as a binary format. This repo also contains a reference implementation of an assembler for my proposed bytecode.

Here is the table of registers.

| the bits | reg |   initial value   |
| :------: | :-- | :---------------: |
|  `0000`  | SP  | End of memory + 1 |
|  `0001`  | $1  |         0         |
|  `0010`  | $2  |         0         |
|  `0011`  | $3  |         0         |
|  `0100`  | $4  |         0         |
|  `0101`  | $5  |         0         |
|  `0110`  | $6  |         0         |
|  `0111`  | $7  |         0         |
|  `1000`  | $8  |         0         |
|  `1001`  | $9  |         0         |
|  `1010`  | $10 |         0         |
|  `1011`  | $11 |         0         |
|  `1100`  | $12 |         0         |
|  `1101`  | $13 |         0         |
|  `1110`  | $14 |         0         |
|  `1111`  | $15 |         0         |

The first two words in a file that the VM loads should be the minimum heap size and minimum stack size. `MINHEAP` and `MINSTACK`. These two words are not loaded as part of the program in executable memory, and only exist to initialize the VM's memory buffers. The rest of the file is stored at the start of the memory buffers, which are guaranteed to be at least as big as the entire rest of the file + `MINHEAP` + `MINSTACK`. Generally, a VM wil give you exactly what is asked for: `MINHEAP` + `MINSTACK`. `MINSTACK` is used to check for stack overflow with the stack manipulating instructions. The SP is initialized to a value that is one more than the entire memory buffer. Reading/writing to that location may be out of bounds, but it may also be zero if the memory is of exactly the word size.

The 16-bit assumption is fine, but this should generally work with any value of BITS >= 16. Specifically, i think it's important that this format should work to encode URCL programs that are 32-bit or 64-bit, by just padding the opcode with zeroes.

In the bit diagrams, "AAAA" is used for the first operand as a register, and "BBBB" is used for the second operand as a register.

This proposal is a 2 operand architecture. Every instruction that in URCL is 3 operands, map to the first operand being the destination _and_ first source operand.

For IN/OUT, "AAAA" or "BBBB" are ports, but still refer to the first and second registers. For simplifying this format, they take up 4 instruction spaces, and use 4 bits for the port value. Technically, this is encoding the upper 2 bits of the port earlier than the rest.

Most instructions take the form of `op A A B` with the binary representation of either `XXXX XXXX AAAA BBBB` where `A` and `B` are registers, or `0000 XXXX XXXX AAAA` with an immediate for the second operand.

The opcode is as such 8 bits. However, there are not 256 possible opcodes.

The opcode range of `0000 XXXX` is reserved for "special instructions" that do not fit the standard format of `A = A op B` (i.e. `ADD`) or `A = op B` (i.e. `NEG`). This is for example, `NOP`, `HLT`, `RET`, `CPY imm, imm`, `JMP imm`, `CAL imm`, `STR imm, imm`.

That's 16 possible opcodes reserved, giving us a total of 240 ordinary opcodes.

This range is actually 256 total special opcodes though, because the last nybble is part of its opcode. Some special opcodes will take one register input, and take the equivalent of 16 special opcodes. The below table contains all special opcodes. The opcode column in the table contains the entire first word, and immediates come after.

The `POP` instruction without an operand pops a value from the top of the stack and discards it. This is equivalent to `POP $0` in URCL source code. It is a separate instruction because it is the only URCL instruction where `$0` is usable in such a way that it does something, and cannot be replaced by an immediate `0`.

|        Opcode         | Name & Operands |
| :-------------------: | :-------------- |
| `0000 0000 0000 0000` | `NOP`           |
| `0000 0000 0000 0001` | `PSH imm`       |
| `0000 0000 0000 0010` | `JMP imm`       |
| `0000 0000 0000 0011` | `CAL imm`       |
| `0000 0000 0000 0100` | `CPY imm, imm`  |
| `0000 0000 0000 0101` | `STR imm, imm`  |
| `0000 0000 0000 0110` | `POP`           |
| `0000 0000 0000 0111` | `HLT`           |
| `0000 0000 0000 1000` | `RET`           |
| `0000 0000 0000 1XXX` | Unassigned      |
| `0000 0000 0001 AAAA` | `PSH reg`       |
| `0000 0000 0010 AAAA` | `JMP reg`       |
| `0000 0000 0011 AAAA` | `CAL reg`       |
| `0000 0000 0100 AAAA` | `CPY imm, reg`  |
| `0000 0000 0101 AAAA` | `STR imm, reg`  |
| `0000 0000 0110 AAAA` | `POP reg`       |
| `0000 0000 0111 XXXX` | Unassigned      |
| `0000 0000 1XXX XXXX` | Unassigned      |

The ordinary opcodes `0001 XXXX` are reserved for I/O instructions. They need to be treated specially, because even though they fit in the standard format, they do not take registers as operands. They take ports, and the normal treatment where they can be eagerly read before decoding the opcode is not acceptable.

This means the layouts `0001 XXXX XXXX XXXX` and `0000 0001 XXXX XXXX` are not allowed for regular operands. This table contains the full word for each I/O instruction.

|        Opcode         | Name & Operands |
| :-------------------: | :-------------- |
| `0001 00BB AAAA BBBB` | `IN reg, port`  |
| `0001 01AA AAAA BBBB` | `OUT port, reg` |
| `0001 1XXX XXXX XXXX` | Reserved        |
| `0000 0001 00XX XXXX` | Reserved        |
| `0000 0001 01AA AAAA` | `OUT port, imm` |
| `0000 0001 1XXX XXXX` | Reserved        |

The following table contains all instructions that follow the standard format of `A = A op B`. or `A = op B`, where `B` may be an immediate or a register.

They have 2 bitwise layouts, depending on whether `B` is an immediate or a register.

`XXXX XXXX AAAA BBBB`: `B` is a register stored inline.  
`0000 XXXX XXXX AAAA`: `B` is an immediate stored in the next word.

To execute these instructions, you can determine exactly its operands just by looking at the format specified, without knowing what the opcode corresponds to. This allows an interpreter to be very space-efficient by not repeating the same code to decode operands.

There is one exception to the above rule, being that `001X XXXX` is reserved for branching instructions.

Binary branches (2 sources) take an additional immediate value, which is the branch destination. If it's the immediate variant, the branch destination comes after the immediate operand. The opcodes `0010 XXXX` are reserved for binary. The upper bit of the 4 variable bits negates the condition, so it's really just 3 bits for the condition.

To use a register as the destination for a binary branch type, you must negate the branch and jump to like 2 instructions ahead, and then use `JMP reg` to jump to the destination.

|   Opcode    | Branch     |
| :---------: | :--------- |
| `0010 0000` | `BRG`      |
| `0010 0001` | `BRL`      |
| `0010 0010` | `SBRG`     |
| `0010 0011` | `SBRL`     |
| `0010 0100` | `BRE`      |
| `0010 0101` | `BRC`      |
| `0010 011X` | Unassigned |
| `0010 1000` | `BLE`      |
| `0010 1001` | `BGE`      |
| `0010 1010` | `SBLE`     |
| `0010 1011` | `SBGE`     |
| `0010 1100` | `BNE`      |
| `0010 1101` | `BNC`      |
| `0010 111X` | Unassigned |

`0011 XXXX` is reserved for unary branches. These follow the normal operand format in terms of memory representation, but the branch destination is the _second_ operand (register or immediate), and the source always has to be the first (register). The same bit flips the condition for these too.

|   Opcode    | Branch     |
| :---------: | :--------- |
| `0011 0000` | `BRZ`      |
| `0011 0001` | `BEV`      |
| `0011 0010` | `BRP`      |
| `0011 0011` | Unassigned |
| `0011 01XX` | Unassigned |
| `0011 1000` | `BNZ`      |
| `0011 1001` | `BOD`      |
| `0011 1010` | `BRN`      |
| `0011 1011` | Unassigned |
| `0011 11XX` | Unassigned |

The rest of these instructions _do_ follow the normal operand format with either a register or an immediate, and no extra branch destination.

|   Opcode    | Name                |
| :---------: | ------------------- |
| `0100 0000` | `MOV`/`IMM`         |
| `0100 0001` | `AND`               |
| `0100 0010` | `OR`                |
| `0100 0011` | `XOR`               |
| `0100 0100` | `NOT`               |
| `0100 0101` | `NAND`              |
| `0100 0110` | `NOR`               |
| `0100 0111` | `XNOR`              |
|             |                     |
| `0100 1000` | `LSH`               |
| `0100 1001` | `RSH`               |
| `0100 1010` | `SRS`               |
| `0100 1011` | `BSL`               |
| `0100 1100` | `BSR`               |
| `0100 1101` | `BSS`               |
|             |                     |
| `0100 1110` | `ADD`               |
| `0100 1111` | `SUB`               |
| `0101 0000` | `INC`               |
| `0101 0001` | `DEC`               |
| `0101 0010` | `NEG`               |
|             |                     |
| `0101 0011` | `MLT`               |
|             |                     |
| `0101 0100` | `DIV`               |
| `0101 0101` | `SDIV`              |
| `0101 0110` | `MOD`               |
| `0101 0111` | `SMOD`              |
|             |                     |
| `0101 1000` | `UMLT`              |
| `0101 1001` | `SUMLT`             |
|             |                     |
| `0101 1010` | `CPY`               |
| `0101 1011` | `STR`               |
| `0101 1100` | `LOD`               |
|             |                     |
| `0101 1101` | Unassigned          |
| `0101 1110` | Unassigned          |
| `0101 1111` | Unassigned          |
|             |                     |
| `0110 0000` | `SETG`              |
| `0110 0001` | `SETL`              |
| `0110 0010` | `SSETG`             |
| `0110 0011` | `SSETL`             |
| `0110 0100` | `SETE`              |
| `0110 0101` | `SETC`              |
| `0110 011X` | Reserved for `SET*` |
| `0110 1000` | `SETLE`             |
| `0110 1001` | `SETGE`             |
| `0110 1010` | `SSETLE`            |
| `0110 1011` | `SSETGE`            |
| `0110 1100` | `SETNE`             |
| `0110 1101` | `SETNC`             |
| `0110 111X` | Reserved for `SET*` |
|             |                     |
| `0111 XXXX` | Unassigned          |
| `1XXX XXXX` | Unassigned          |
