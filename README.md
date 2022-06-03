# URCLvm

A project that is in its very early stages, where we are making a URCL-based ISA for interpreting directly. One of the huge features of this from my understanding will be self-modifying code, and it should also be able to be interpreted in URCL.

This is not "the standard" or anything (*yet*), this is my proposal for what we could use as a binary format. This repo also contains a reference implementation of an assembler for my proposed bytecode.

Here is the table of registers, exactly as in [Verlio's proposal](https://docs.google.com/spreadsheets/d/1ptrvfMMm1K4dEkAAVZm6SRPzRkUJop4As0RYx80wPvk/edit#gid=0)

| 4   | 3   | 2   | 1   | Reg |   Initial value   |
| --- | --- | --- | --- | :-- | :---------------: |
| 0   | 0   | 0   | 0   | $0  |         0         |
| 0   | 0   | 0   | 1   | $1  |         0         |
| 0   | 0   | 1   | 0   | $2  |         0         |
| 0   | 0   | 1   | 1   | $3  |         0         |
| 0   | 1   | 0   | 0   | $4  |         0         |
| 0   | 1   | 0   | 1   | $5  |         0         |
| 0   | 1   | 1   | 0   | $6  |         0         |
| 0   | 1   | 1   | 1   | $7  |         0         |
| 1   | 0   | 0   | 0   | $8  |         0         |
| 1   | 0   | 0   | 1   | $9  |         0         |
| 1   | 0   | 1   | 0   | $10 |         0         |
| 1   | 0   | 1   | 1   | $11 |         0         |
| 1   | 1   | 0   | 0   | $12 |         0         |
| 1   | 1   | 0   | 1   | $13 |         0         |
| 1   | 1   | 1   | 0   | $14 |         0         |
| 1   | 1   | 1   | 1   | SP  | End of memory + 1 |

I hate the "immediate bit" in Verlio's proposal, but i'm otherwise fine with it. What i think would be a better system is to have separate "opcodes" for the versions with registers and immediate values. When you do that, it becomes obvious that when the equivalent of the "immediate bit" is set to true, there is no reason to have 4 unused bits for what would've otherwise been the register. You can fit 16 other instructions with the same configuration of operands in there.

The first two words in a file that the VM loads should be the minimum heap size and minimum stack size. `MINHEAP` and `MINSTACK`. These two words are not loaded as part of the program in executable memory, and only exist to initialize the VM's memory buffers. The rest of the file is stored at the start of the memory buffers, which are guaranteed to be at least as big as the entire rest of the file + `MINHEAP` + `MINSTACK`. Generally, a VM wil give you exactly what is asked for: `MINHEAP` + `MINSTACK`. `MINSTACK` is used to check for stack overflow with the stack manipulating instructions. The SP is initialized to a value that is one more than the entire memory buffer. Reading/writing to that location may be out of bounds, but it may also be zero if the memory is of exactly the word size.

The 16-bit assumption is fine, but this should generally work with any value of BITS >= 16. Specifically, i think it's important that this format should work to encode URCL programs that are 32-bit or 64-bit, by just padding the opcode with zeroes.

In the bit diagrams, "AAAA" is used for the first operand as a register, "BBBB" is used for the second operand as a register, and "CCCC" is used for the third operand as a register.

For IN/OUT, "AAAA" or "BBBB" are ports, but still refer to the first and second registers. For simplifying this format, they take up 4 instruction spaces, and use 4 bytes for the port value. Technically, this is encoding the upper 2 bits of the port earlier than the rest.

The entire "opcode" column is stored as a single word, in the lower 16 bits of that word. The first half of the opcode is always guaranteed to contain data about the opcode. The second half is easy enough to parse based on the first byte.

For instructions that contain three register operands, the first register is stored in the bit diagrams on the left, and the second and third registers are stored in the next word, in the lower 4 bits of the upper and lower halves. On 16 bits, that looks like `0000BBBB0000CCCC`. The bits that are zero in that bit pattern must be zero, or it's undefined behaviour. This is so it's easy to parse these values in a VM by reading two bytes, rather than packing both values in a single half. For instructions that contain only one or two "reg"s, all of them will be stored explicitly in the bit diagrams. This is the equivalent of the "extended format" in Verlio's proposal. The "extended" format is **only** used for instructions that have 3 registers, with all others being encoded as a single word. For consistency, even on 32 bits where there is extra space for the second and third registers, they are still stored in the next word.

If any of the operands are immediate values, they are stored as additional words after the opcode. As is already clarified by the extended format above only being used for 3 register opcodes, this means that an instruction may be up to 4 words long if all three operands are immediates. No instructions with 3 immediate values are actually useful, and are only included for completeness.

| Opcode                | Name & Operand Types   | Notes                   |
| --------------------- | ---------------------- | ----------------------- |
| `0000 0000 0000 0000` | `NOP`                  |                         |
| `0000 0000 0000 0001` | `HLT`                  |                         |
| `0000 0000 0000 0010` | `RET`                  |                         |
| `0000 0000 0000 0011` | `PSH imm`              |                         |
|                       |                        |                         |
| `0000 0000 0000 0100` | `STR imm, imm`         |                         |
| `0000 0000 0000 0101` | `CPY imm, imm`         |                         |
|                       |                        |                         |
| `0000 0000 0000 0110` | `CAL imm`              |                         |
| `0000 0000 0000 0111` | `JMP imm`              |                         |
|                       |                        |                         |
| `0000 0000 0000 1000` | `BRG imm, imm, imm`    |                         |
| `0000 0000 0000 1001` | `BGE imm, imm, imm`    |                         |
| `0000 0000 0000 1010` | `BRL imm, imm, imm`    |                         |
| `0000 0000 0000 1011` | `BLE imm, imm, imm`    |                         |
|                       |                        |                         |
| `0000 0000 0000 1100` | `SBRG imm, imm, imm`   |                         |
| `0000 0000 0000 1101` | `SBGE imm, imm, imm`   |                         |
| `0000 0000 0000 1110` | `SBRL imm, imm, imm`   |                         |
| `0000 0000 0000 1111` | `SBLE imm, imm, imm`   |                         |
|                       |                        |                         |
| `0000 0000 0001 0000` | `BRE imm, imm, imm`    |                         |
| `0000 0000 0001 0001` | `BNE imm, imm, imm`    |                         |
| `0000 0000 0001 0010` | `BRC imm, imm, imm`    |                         |
| `0000 0000 0001 0011` | `BNC imm, imm, imm`    |                         |
|                       |                        |                         |
| `0000 0000 0001 0100` | `BRZ imm, imm`         |                         |
| `0000 0000 0001 0101` | `BNZ imm, imm`         |                         |
|                       |                        |                         |
| `0000 0000 0001 0110` | `BRP imm, imm`         |                         |
| `0000 0000 0001 0111` | `BRN imm, imm`         |                         |
| `0000 0000 0001 1000` | `BOD imm, imm`         |                         |
| `0000 0000 0001 1001` | `BEV imm, imm`         |                         |
|                       |                        |                         |
| `0000 0000 0001 1010` | `LSTR imm, imm, imm`   |                         |
| `0000 0000 0001 1011` | Unassigned             |                         |
| `0000 0000 0001 11XX` | Unassigned             |                         |
|                       |                        |                         |
| `0000 0000 XXXX XXXX` | Unassigned             |                         |
|                       |                        |                         |
| `0000 0001 0000 AAAA` | `BRG reg, reg, reg`    |                         |
| `0000 0001 0001 AAAA` | `BRG reg, imm, imm`    |                         |
| `0000 0001 0010 BBBB` | `BRG imm, reg, imm`    |                         |
| `0000 0001 0011 CCCC` | `BRG imm, imm, reg`    |                         |
|                       |                        |                         |
| `0000 0001 0100 AAAA` | `BGE reg, reg, reg`    |                         |
| `0000 0001 0101 AAAA` | `BGE reg, imm, imm`    |                         |
| `0000 0001 0110 BBBB` | `BGE imm, reg, imm`    |                         |
| `0000 0001 0111 CCCC` | `BGE imm, imm, reg`    |                         |
|                       |                        |                         |
| `0000 0001 1000 AAAA` | `BRL reg, reg, reg`    |                         |
| `0000 0001 1001 AAAA` | `BRL reg, imm, imm`    |                         |
| `0000 0001 1010 BBBB` | `BRL imm, reg, imm`    |                         |
| `0000 0001 1011 CCCC` | `BRL imm, imm, reg`    |                         |
|                       |                        |                         |
| `0000 0001 1100 AAAA` | `BLE reg, reg, reg`    |                         |
| `0000 0001 1101 AAAA` | `BLE reg, imm, imm`    |                         |
| `0000 0001 1110 BBBB` | `BLE imm, reg, imm`    |                         |
| `0000 0001 1111 CCCC` | `BLE imm, imm, reg`    |                         |
|                       |                        |                         |
| `0000 0010 0000 AAAA` | `SBRG reg, reg, reg`   |                         |
| `0000 0010 0001 AAAA` | `SBRG reg, imm, imm`   |                         |
| `0000 0010 0010 BBBB` | `SBRG imm, reg, imm`   |                         |
| `0000 0010 0011 CCCC` | `SBRG imm, imm, reg`   |                         |
|                       |                        |                         |
| `0000 0010 0100 AAAA` | `SBGE reg, reg, reg`   |                         |
| `0000 0010 0101 AAAA` | `SBGE reg, imm, imm`   |                         |
| `0000 0010 0110 BBBB` | `SBGE imm, reg, imm`   |                         |
| `0000 0010 0111 CCCC` | `SBGE imm, imm, reg`   |                         |
|                       |                        |                         |
| `0000 0010 1000 AAAA` | `SBRL reg, reg, reg`   |                         |
| `0000 0010 1001 AAAA` | `SBRL reg, imm, imm`   |                         |
| `0000 0010 1010 BBBB` | `SBRL imm, reg, imm`   |                         |
| `0000 0010 1011 CCCC` | `SBRL imm, imm, reg`   |                         |
|                       |                        |                         |
| `0000 0010 1100 AAAA` | `SBLE reg, reg, reg`   |                         |
| `0000 0010 1101 AAAA` | `SBLE reg, imm, imm`   |                         |
| `0000 0010 1110 BBBB` | `SBLE imm, reg, imm`   |                         |
| `0000 0010 1111 CCCC` | `SBLE imm, imm, reg`   |                         |
|                       |                        |                         |
| `0000 0011 0000 AAAA` | `BRE reg, reg, reg`    |                         |
| `0000 0011 0001 AAAA` | `BRE reg, imm, imm`    |                         |
| `0000 0011 0010 BBBB` | `BRE imm, reg, imm`    |                         |
| `0000 0011 0011 CCCC` | `BRE imm, imm, reg`    |                         |
|                       |                        |                         |
| `0000 0011 0100 AAAA` | `BNE reg, reg, reg`    |                         |
| `0000 0011 0101 AAAA` | `BNE reg, imm, imm`    |                         |
| `0000 0011 0110 BBBB` | `BNE imm, reg, imm`    |                         |
| `0000 0011 0111 CCCC` | `BNE imm, imm, reg`    |                         |
|                       |                        |                         |
| `0000 0011 1000 AAAA` | `BRC reg, reg, reg`    |                         |
| `0000 0011 1001 AAAA` | `BRC reg, imm, imm`    |                         |
| `0000 0011 1010 BBBB` | `BRC imm, reg, imm`    |                         |
| `0000 0011 1011 CCCC` | `BRC imm, imm, reg`    |                         |
|                       |                        |                         |
| `0000 0011 1100 AAAA` | `BNC reg, reg, reg`    |                         |
| `0000 0011 1101 AAAA` | `BNC reg, imm, imm`    |                         |
| `0000 0011 1110 BBBB` | `BNC imm, reg, imm`    |                         |
| `0000 0011 1111 CCCC` | `BNC imm, imm, reg`    |                         |
|                       |                        |                         |
| `0000 0100 0000 AAAA` | `BRP reg, imm`         |                         |
| `0000 0100 0001 BBBB` | `BRP imm, reg`         |                         |
| `0000 0100 0010 AAAA` | `BRN reg, imm`         |                         |
| `0000 0100 0011 BBBB` | `BRN imm, reg`         |                         |
|                       |                        |                         |
| `0000 0100 0100 AAAA` | `BRZ reg, imm`         |                         |
| `0000 0100 0101 BBBB` | `BRZ imm, reg`         |                         |
| `0000 0100 0110 AAAA` | `BNZ reg, imm`         |                         |
| `0000 0100 0111 BBBB` | `BNZ imm, reg`         |                         |
|                       |                        |                         |
| `0000 0100 1000 AAAA` | `ADD reg, reg, reg`    |                         |
| `0000 0100 1001 AAAA` | `ADD reg, imm, imm`    |                         |
| `0000 0100 1010 AAAA` | `SUB reg, reg, reg`    |                         |
| `0000 0100 1011 AAAA` | `SUB reg, imm, imm`    |                         |
|                       |                        |                         |
| `0000 0100 1100 AAAA` | `INC reg, imm`         |                         |
| `0000 0100 1101 AAAA` | `DEC reg, imm`         |                         |
| `0000 0100 1110 AAAA` | `NEG reg, imm`         |                         |
| `0000 0100 1111 AAAA` | `NOT reg, imm`         |                         |
|                       |                        |                         |
| `0000 0101 0000 AAAA` | `CPY reg, imm`         |                         |
| `0000 0101 0001 BBBB` | `CPY imm, reg`         |                         |
|                       |                        |                         |
| `0000 0101 0010 AAAA` | `MLT reg, reg, reg`    |                         |
| `0000 0101 0011 AAAA` | `MLT reg, imm, imm`    |                         |
|                       |                        |                         |
| `0000 0101 0100 AAAA` | `DIV reg, reg, reg`    |                         |
| `0000 0101 0101 AAAA` | `DIV reg, imm, imm`    |                         |
| `0000 0101 0110 AAAA` | `SDIV reg, reg, reg`   |                         |
| `0000 0101 0111 AAAA` | `SDIV reg, imm, imm`   |                         |
|                       |                        |                         |
| `0000 0101 1000 AAAA` | `MOD reg, reg, reg`    |                         |
| `0000 0101 1001 AAAA` | `MOD reg, imm, imm`    |                         |
| `0000 0101 1010 AAAA` | `SMOD reg, reg, reg`   |                         |
| `0000 0101 1011 AAAA` | `SMOD reg, imm, imm`   |                         |
|                       |                        |                         |
| `0000 0101 1100 AAAA` | `AND reg, reg, reg`    |                         |
| `0000 0101 1101 AAAA` | `AND reg, imm, imm`    |                         |
| `0000 0101 1110 AAAA` | `NAND reg, reg, reg`   |                         |
| `0000 0101 1111 AAAA` | `NAND reg, imm, imm`   |                         |
|                       |                        |                         |
| `0000 0110 0000 AAAA` | `OR reg, reg, reg`     |                         |
| `0000 0110 0001 AAAA` | `OR reg, imm, imm`     |                         |
| `0000 0110 0010 AAAA` | `NOR reg, reg, reg`    |                         |
| `0000 0110 0011 AAAA` | `NOR reg, imm, imm`    |                         |
|                       |                        |                         |
| `0000 0110 0100 AAAA` | `XOR reg, reg, reg`    |                         |
| `0000 0110 0101 AAAA` | `XOR reg, imm, imm`    |                         |
| `0000 0110 0110 AAAA` | `XNOR reg, reg, reg`   |                         |
| `0000 0110 0111 AAAA` | `XNOR reg, imm, imm`   |                         |
|                       |                        |                         |
| `0000 0110 1000 AAAA` | `LLOD reg, reg, reg`   |                         |
| `0000 0110 1001 AAAA` | `LLOD reg, imm, imm`   |                         |
| `0000 0110 1010 AAAA` | `IMM reg, imm`         |                         |
|                       |                        |                         |
| `0000 0110 1011 AAAA` | `RSH reg, imm`         |                         |
| `0000 0110 1100 AAAA` | `LSH reg, imm`         |                         |
| `0000 0110 1101 AAAA` | `SRS reg, imm`         |                         |
|                       |                        |                         |
| `0000 0110 1110 AAAA` | `BSR reg, reg, reg`    |                         |
| `0000 0110 1111 AAAA` | `BSR reg, imm, imm`    |                         |
| `0000 0111 0000 AAAA` | `BSL reg, reg, reg`    |                         |
| `0000 0111 0001 AAAA` | `BSL reg, imm, imm`    |                         |
| `0000 0111 0010 AAAA` | `BSS reg, reg, reg`    |                         |
| `0000 0111 0011 AAAA` | `BSS reg, imm, imm`    |                         |
|                       |                        |                         |
| `0000 0111 0100 AAAA` | `SETG reg, reg, reg`   |                         |
| `0000 0111 0101 AAAA` | `SETG reg, imm, imm`   |                         |
| `0000 0111 0110 AAAA` | `SETGE reg, reg, reg`  |                         |
| `0000 0111 0111 AAAA` | `SETGE reg, imm, imm`  |                         |
|                       |                        |                         |
| `0000 0111 1000 AAAA` | `SETL reg, reg, reg`   |                         |
| `0000 0111 1001 AAAA` | `SETL reg, imm, imm`   |                         |
| `0000 0111 1010 AAAA` | `SETLE reg, reg, reg`  |                         |
| `0000 0111 1011 AAAA` | `SETLE reg, imm, imm`  |                         |
|                       |                        |                         |
| `0000 0111 1100 AAAA` | `SETE reg, reg, reg`   |                         |
| `0000 0111 1101 AAAA` | `SETE reg, imm, imm`   |                         |
| `0000 0111 1110 AAAA` | `SETNE reg, reg, reg`  |                         |
| `0000 0111 1111 AAAA` | `SETNE reg, imm, imm`  |                         |
|                       |                        |                         |
| `0000 1000 0000 AAAA` | `SETC reg, reg, reg`   |                         |
| `0000 1000 0001 AAAA` | `SETC reg, imm, imm`   |                         |
| `0000 1000 0010 AAAA` | `SETNC reg, reg, reg`  |                         |
| `0000 1000 0011 AAAA` | `SETNC reg, imm, imm`  |                         |
|                       |                        |                         |
| `0000 1000 0100 AAAA` | `SSETG reg, reg, reg`  |                         |
| `0000 1000 0101 AAAA` | `SSETG reg, imm, imm`  |                         |
| `0000 1000 0110 AAAA` | `SSETGE reg, reg, reg` |                         |
| `0000 1000 0111 AAAA` | `SSETGE reg, imm, imm` |                         |
|                       |                        |                         |
| `0000 1000 1000 AAAA` | `SSETL reg, reg, reg`  |                         |
| `0000 1000 1001 AAAA` | `SSETL reg, imm, imm`  |                         |
| `0000 1000 1010 AAAA` | `SSETLE reg, reg, reg` |                         |
| `0000 1000 1011 AAAA` | `SSETLE reg, imm, imm` |                         |
|                       |                        |                         |
| `0000 1000 1100 AAAA` | `LSTR reg, reg, reg`   |                         |
| `0000 1000 1101 AAAA` | `LSTR reg, imm, imm`   |                         |
| `0000 1000 1110 BBBB` | `LSTR imm, reg, imm`   |                         |
| `0000 1000 1111 CCCC` | `LSTR imm, imm, reg`   |                         |
|                       |                        |                         |
| `0000 1001 0000 AAAA` | `PSH reg`              |                         |
| `0000 1001 0001 AAAA` | `POP reg`              |                         |
|                       |                        |                         |
| `0000 1001 0010 AAAA` | `CAL reg`              |                         |
| `0000 1001 0011 AAAA` | `JMP reg`              |                         |
|                       |                        |                         |
| `0000 1001 0100 AAAA` | `LOD reg, imm`         |                         |
| `0000 1001 0101 AAAA` | `STR reg, imm`         |                         |
| `0000 1001 0110 BBBB` | `STR imm, reg`         |                         |
| `0000 1001 0111 XXXX` | Unassigned             |                         |
|                       |                        |                         |
| `0000 1001 1000 AAAA` | `BOD reg, imm`         |                         |
| `0000 1001 1001 BBBB` | `BOD imm, reg`         |                         |
| `0000 1001 1010 AAAA` | `BEV reg, imm`         |                         |
| `0000 1001 1011 BBBB` | `BEV imm, reg`         |                         |
|                       |                        |                         |
| `0XXX XXXX XXXX XXXX` | Unassigned             |                         |
|                       |                        |                         |
| `0111 1111 1100 AAAA` | `OUT port, imm`        | General + Graphics      |
| `0111 1111 1101 AAAA` | `OUT port, imm`        | Text + Numbers          |
| `0111 1111 1110 AAAA` | `OUT port, imm`        | Storage + Miscellaneous |
| `0111 1111 1111 AAAA` | `OUT port, imm`        | User Defined            |
|                       |                        |                         |
| `1000 0000 AAAA BBBB` | `IN reg, port`         | General + Graphics      |
| `1000 0001 AAAA BBBB` | `IN reg, port`         | Text + Numbers          |
| `1000 0010 AAAA BBBB` | `IN reg, port`         | Storage + Miscellaneous |
| `1000 0011 AAAA BBBB` | `IN reg, port`         | User Defined            |
|                       |                        |                         |
| `1000 0100 AAAA BBBB` | `OUT port, reg`        | General + Graphics      |
| `1000 0101 AAAA BBBB` | `OUT port, reg`        | Text + Numbers          |
| `1000 0110 AAAA BBBB` | `OUT port, reg`        | Storage + Miscellaneous |
| `1000 0111 AAAA BBBB` | `OUT port, reg`        | User Defined            |
|                       |                        |                         |
| `1000 1000 AAAA BBBB` | `LOD reg, reg`         |                         |
| `1000 1001 AAAA BBBB` | `STR reg, reg`         |                         |
|                       |                        |                         |
| `1000 1010 AAAA BBBB` | `LLOD reg, reg, imm`   |                         |
| `1000 1011 AAAA CCCC` | `LLOD reg, imm, reg`   |                         |
|                       |                        |                         |
| `1000 1100 AAAA BBBB` | `RSH reg, reg`         |                         |
| `1000 1101 AAAA BBBB` | `LSH reg, reg`         |                         |
| `1000 1110 AAAA BBBB` | `SRS reg, reg`         |                         |
|                       |                        |                         |
| `1000 1111 AAAA BBBB` | `LSTR reg, reg, imm`   |                         |
| `1001 0000 AAAA CCCC` | `LSTR reg, imm, reg`   |                         |
| `1001 0001 BBBB CCCC` | `LSTR imm, reg, reg`   |                         |
|                       |                        |                         |
| `1001 0010 AAAA BBBB` | `ADD reg, reg, imm`    |                         |
| `1001 0011 AAAA CCCC` | `ADD reg, imm, reg`    |                         |
| `1001 0100 AAAA BBBB` | `SUB reg, reg, imm`    |                         |
| `1001 0101 AAAA CCCC` | `SUB reg, imm, reg`    |                         |
|                       |                        |                         |
| `1001 0110 AAAA BBBB` | `INC reg, reg`         |                         |
| `1001 0111 AAAA BBBB` | `DEC reg, reg`         |                         |
| `1001 1000 AAAA BBBB` | `NEG reg, reg`         |                         |
| `1001 1001 AAAA BBBB` | `NOT reg, reg`         |                         |
|                       |                        |                         |
| `1001 1010 AAAA BBBB` | `MLT reg, reg, imm`    |                         |
| `1001 1011 AAAA CCCC` | `MLT reg, imm, reg`    |                         |
|                       |                        |                         |
| `1001 1100 AAAA BBBB` | `DIV reg, reg, imm`    |                         |
| `1001 1101 AAAA CCCC` | `DIV reg, imm, reg`    |                         |
| `1001 1110 AAAA BBBB` | `SDIV reg, reg, imm`   |                         |
| `1001 1111 AAAA CCCC` | `SDIV reg, imm, reg`   |                         |
|                       |                        |                         |
| `1010 0000 AAAA BBBB` | `MOD reg, reg, imm`    |                         |
| `1010 0001 AAAA CCCC` | `MOD reg, imm, reg`    |                         |
| `1010 0010 AAAA BBBB` | `SMOD reg, reg, imm`   |                         |
| `1010 0011 AAAA CCCC` | `SMOD reg, imm, reg`   |                         |
|                       |                        |                         |
| `1010 0100 AAAA BBBB` | `AND reg, reg, imm`    |                         |
| `1010 0101 AAAA CCCC` | `AND reg, imm, reg`    |                         |
| `1010 0110 AAAA BBBB` | `NAND reg, reg, imm`   |                         |
| `1010 0111 AAAA CCCC` | `NAND reg, imm, reg`   |                         |
|                       |                        |                         |
| `1010 1000 AAAA BBBB` | `OR reg, reg, imm`     |                         |
| `1010 1001 AAAA CCCC` | `OR reg, imm, reg`     |                         |
| `1010 1010 AAAA BBBB` | `NOR reg, reg, imm`    |                         |
| `1010 1011 AAAA CCCC` | `NOR reg, imm, reg`    |                         |
|                       |                        |                         |
| `1010 1100 AAAA BBBB` | `XOR reg, reg, imm`    |                         |
| `1010 1101 AAAA CCCC` | `XOR reg, imm, reg`    |                         |
| `1010 1110 AAAA BBBB` | `XNOR reg, reg, imm`   |                         |
| `1010 1111 AAAA CCCC` | `XNOR reg, imm, reg`   |                         |
|                       |                        |                         |
| `1011 0000 AAAA BBBB` | `BSL reg, reg, imm`    |                         |
| `1011 0001 AAAA CCCC` | `BSL reg, imm, reg`    |                         |
| `1011 0010 AAAA BBBB` | `BSR reg, reg, imm`    |                         |
| `1011 0011 AAAA CCCC` | `BSR reg, imm, reg`    |                         |
| `1011 0100 AAAA BBBB` | `BSS reg, reg, imm`    |                         |
| `1011 0101 AAAA CCCC` | `BSS reg, imm, reg`    |                         |
|                       |                        |                         |
| `1011 0110 AAAA BBBB` | `BRC reg, reg, imm`    |                         |
| `1011 0111 AAAA CCCC` | `BRC reg, imm, reg`    |                         |
| `1011 1000 BBBB CCCC` | `BRC imm, reg, reg`    |                         |
|                       |                        |                         |
| `1011 1001 AAAA BBBB` | `BNC reg, reg, imm`    |                         |
| `1011 1010 AAAA CCCC` | `BNC reg, imm, reg`    |                         |
| `1011 1011 BBBB CCCC` | `BNC imm, reg, reg`    |                         |
|                       |                        |                         |
| `1011 1100 AAAA BBBB` | `BRE reg, reg, imm`    |                         |
| `1011 1101 AAAA CCCC` | `BRE reg, imm, reg`    |                         |
| `1011 1110 BBBB CCCC` | `BRE imm, reg, reg`    |                         |
|                       |                        |                         |
| `1011 1111 AAAA BBBB` | `BNE reg, reg, imm`    |                         |
| `1100 0000 AAAA CCCC` | `BNE reg, imm, reg`    |                         |
| `1100 0001 AAAA BBBB` | `BNE imm, reg, reg`    |                         |
|                       |                        |                         |
| `1100 0010 AAAA BBBB` | `BOD reg, reg`         |                         |
| `1100 0011 AAAA BBBB` | `BEV reg, reg`         |                         |
|                       |                        |                         |
| `1100 0100 AAAA BBBB` | `BRZ reg, reg`         |                         |
| `1100 0101 AAAA BBBB` | `BNZ reg, reg`         |                         |
|                       |                        |                         |
| `1100 0110 AAAA BBBB` | `BRP reg, reg`         |                         |
| `1100 0111 AAAA BBBB` | `BRN reg, reg`         |                         |
|                       |                        |                         |
| `1100 1000 AAAA BBBB` | `SBRG reg, reg, imm`   |                         |
| `1100 1001 AAAA CCCC` | `SBRG reg, imm, reg`   |                         |
| `1100 1010 BBBB CCCC` | `SBRG imm, reg, reg`   |                         |
|                       |                        |                         |
| `1100 1011 AAAA BBBB` | `SBGE reg, reg, imm`   |                         |
| `1100 1100 AAAA CCCC` | `SBGE reg, imm, reg`   |                         |
| `1100 1101 BBBB CCCC` | `SBGE imm, reg, reg`   |                         |
|                       |                        |                         |
| `1100 1110 AAAA BBBB` | `SBRL reg, reg, imm`   |                         |
| `1100 1111 AAAA CCCC` | `SBRL reg, imm, reg`   |                         |
| `1101 0000 BBBB CCCC` | `SBRL imm, reg, reg`   |                         |
|                       |                        |                         |
| `1101 0001 AAAA BBBB` | `SBLE reg, reg, imm`   |                         |
| `1101 0010 AAAA CCCC` | `SBLE reg, imm, reg`   |                         |
| `1101 0011 BBBB CCCC` | `SBLE imm, reg, reg`   |                         |
|                       |                        |                         |
| `1101 0100 AAAA BBBB` | `SETE reg, reg, imm`   |                         |
| `1101 0101 AAAA CCCC` | `SETE reg, imm, reg`   |                         |
| `1101 0110 AAAA BBBB` | `SETNE reg, reg, imm`  |                         |
| `1101 0111 AAAA CCCC` | `SETNE reg, imm, reg`  |                         |
|                       |                        |                         |
| `1101 1000 AAAA BBBB` | `SETC reg, reg, imm`   |                         |
| `1101 1001 AAAA CCCC` | `SETC reg, imm, reg`   |                         |
| `1101 1010 AAAA BBBB` | `SETNC reg, reg, imm`  |                         |
| `1101 1011 AAAA CCCC` | `SETNC reg, imm, reg`  |                         |
|                       |                        |                         |
| `1101 1100 AAAA BBBB` | `SETG reg, reg, imm`   |                         |
| `1101 1101 AAAA CCCC` | `SETG reg, imm, reg`   |                         |
| `1101 1110 AAAA BBBB` | `SETGE reg, reg, imm`  |                         |
| `1101 1111 AAAA CCCC` | `SETGE reg, imm, reg`  |                         |
|                       |                        |                         |
| `1110 0000 AAAA BBBB` | `SETL reg, reg, imm`   |                         |
| `1110 0001 AAAA CCCC` | `SETL reg, imm, reg`   |                         |
| `1110 0010 AAAA BBBB` | `SETLE reg, reg, imm`  |                         |
| `1110 0011 AAAA CCCC` | `SETLE reg, imm, reg`  |                         |
|                       |                        |                         |
| `1110 0100 AAAA BBBB` | `SSETG reg, reg, imm`  |                         |
| `1110 0101 AAAA CCCC` | `SSETG reg, imm, reg`  |                         |
| `1110 0110 AAAA BBBB` | `SSETGE reg, reg, imm` |                         |
| `1110 0111 AAAA CCCC` | `SSETGE reg, imm, reg` |                         |
|                       |                        |                         |
| `1110 1000 AAAA BBBB` | `SSETL reg, reg, imm`  |                         |
| `1110 1001 AAAA CCCC` | `SSETL reg, imm, reg`  |                         |
| `1110 1010 AAAA BBBB` | `SSETLE reg, reg, imm` |                         |
| `1110 1011 AAAA CCCC` | `SSETLE reg, imm, reg` |                         |
| `1110 1100 AAAA BBBB` | `MOV reg, reg`         |                         |
| `1110 1101 AAAA BBBB` | `CPY reg, reg`         |                         |
| `1110 111X XXXX XXXX` | Unassigned             |                         |
|                       |                        |                         |
| `1111 0000 AAAA BBBB` | `BRG reg, reg, imm`    |                         |
| `1111 0001 AAAA CCCC` | `BRG reg, imm, reg`    |                         |
| `1111 0010 BBBB CCCC` | `BRG imm, reg, reg`    |                         |
|                       |                        |                         |
| `1111 0011 AAAA BBBB` | `BGE reg, reg, imm`    |                         |
| `1111 0100 AAAA CCCC` | `BGE reg, imm, reg`    |                         |
| `1111 0101 BBBB CCCC` | `BGE imm, reg, reg`    |                         |
|                       |                        |                         |
| `1111 0110 AAAA BBBB` | `BRL reg, reg, imm`    |                         |
| `1111 0111 AAAA CCCC` | `BRL reg, imm, reg`    |                         |
| `1111 1000 BBBB CCCC` | `BRL imm, reg, reg`    |                         |
|                       |                        |                         |
| `1111 1001 AAAA BBBB` | `BLE reg, reg, imm`    |                         |
| `1111 1010 AAAA CCCC` | `BLE reg, imm, reg`    |                         |
| `1111 1011 BBBB CCCC` | `BLE imm, reg, reg`    |                         |
| `1111 11XX XXXX XXXX` | Unassigned             |                         |
