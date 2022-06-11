# I/O

All the I/O ports are implemented using the same input stream. They may return multiple words. You need to read all of them before you can read any other ports. All input/output (stdin/stdout) is done in terms of UTF-8, and the different encodings in these ports are translated by the ports.

## %TEXT

IN: Buffered read. Reads a line from stdin and returns the length of the line. You must then read from %TEXT again that many times to consume the whole string, before you can read from any other port. It returns UTF-8 data. This includes the newline, unless EOF is reached, in which case it's a partial or empty line.

OUT: Buffered write. Takes a length and then needs you to write that many words to %TEXT again. The data you write must be valid UTF-8.

## %ASCII7

IN: Reads one codepoint from stdin, and ensures it is ASCII.

OUT: Writes one word to stdout, and ensures it is ASCII.

## %ASCII8

Same as %ASCII7, except it uses ISO-8859-1. Not IEC 8859-1, Windows-1252, or ``iso-8859-1``. It's the Unicode range of U+0000 to U+00FF. [Here is Rust's explanation of that mess](https://doc.rust-lang.org/std/primitive.char.html#impl-From%3Cu8%3E).

## %UTF8

IN: Reads one codepoint from stdin, which must be valid UTF-8. You must then use this value to get the length of the entire codepoint. If you read ``0xxxxxxx``, that's the whole codepoint, but if you read ``110xxxxx``, then you need to read another byte to get the rest of the codepoint. I/O is locked until you've read the entire codepoint.

OUT: Writes one codepoint to stdout. Same rules as IN, except you have to enforce them.

## %TSPECIAL

IN: Checks for EOF in stdin. @MAX if at EOF, 0 otherwise.

OUT: No.

## %NUMB

IN: Reads a line from stdin, parses it as a number, and returns the number. This accepts anything in the range of -32768 to 65535, inclusive.

OUT: Writes an unsigned integer to stdout. No newline is written. Same as %UINT.

## %INT

IN: Reads a line from stdin, parses it as a signed integer, and returns the value. This accepts anything in the range of -32768 to 32767, inclusive.

OUT: Writes a signed integer to stdout. No newline is written.

## %UINT

IN: Reads a line from stdin, parses it as an unsigned integer, and returns the value. This accepts anything in the range of 0 to 65535, inclusive.

OUT: Writes an unsigned integer to stdout. No newline is written. Same as %NUMB.

## %BIN

IN: Reads a line from stdin, parses it as a binary number, and returns the value. This accepts anything in the range of 0b0000000000000000 to 0b1111111111111111, inclusive.

OUT: Writes a number to stdout as binary. No newline is written.

## %HEX

IN: Reads a line from stdin, parses it as a hexadecimal number, and returns the value. This accepts anything in the range of 0x00000000 to 0xFFFFFFFF, inclusive.

OUT: Writes a number to stdout as hexadecimal. No newline is written.

## %FLOAT

IN: Reads a line from stdin, parses it as a floating point number, and returns the value. This uses the [binary16 (half-precision)](https://en.wikipedia.org/wiki/Half-precision_floating-point_format) format. Note that URCL does not support floating point numbers, so any operation with them other than reading and writing must be implemented in code.

OUT: Writes a half-precision floating point number to stdout. No newline is written.

## %NSPECIAL

Number radix. It does not lock the other ports, and does not require them to be unlocked. That's because none of the numerical ports ever lock.

IN: Returns the current radix. Defaults to 10.

OUT: Sets the current radix for parsing with %NUMB, %INT, %UINT. Must be within the range 1..=36.

# Storage

%PAGE, %ADDR, and %BUS are implemented. They don't need an explanation.

# Misc

## %RNG

IN: Returns a random number.

OUT: Sets the seed. The actual seed of the RNG is 64 bits, so there are 2 different "RNG states" in memory. There is the actual seed that reading will use, which is initialized randomly at startup, and there is the temporary seed that is used to write to the port. That temporary value is initialized to zero, and whenever you write to %RNG, it is shifted to the left 16 bits, and the new value is added in. The sum is then used to initialize the actual seed. You need to write 4 times to %RNG to overwrite the entire seed. This does not lock any other ports or its own reading, so you do not need to overwrite the entire seed all at once.

## %SUPPORTED

IN: Returns 0 if the port is unsupported, @MAX if it is supported.

OUT: Sets the port to query upon reading. You must write to it first, or else it will panic.

# Graphics

I have also implemented graphics ports, but i have not had the chance to test them yet. Therefore, i am not sure if they work, and i'm not listing their behaviour here.