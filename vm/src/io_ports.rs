use std::{
    cell::{RefCell, RefMut},
    io::{BufRead, Write},
    rc::Rc, iter,
};

use super::*;
use half::f16;
use radix_fmt::radix;
use unicode_reader::CodePoints;

// This is here so i can use associated types and pass IO: IoPorts without explicitly capturing R and W separately.
trait IoPorts {
    type R: BufRead;
    type W: Write;
    fn pending(&mut self) -> &mut Option<&'static str>;
    fn input(&mut self) -> &mut Self::R;
    fn output(&mut self) -> &mut Self::W;
}

struct ActualIoPorts<R: BufRead, W: Write> {
    pending: Option<&'static str>,
    input: R,
    output: W,
}

impl<R: BufRead, W: Write> IoPorts for ActualIoPorts<R, W> {
    type R = R;
    type W = W;
    fn pending(&mut self) -> &mut Option<&'static str> {
        &mut self.pending
    }
    fn input(&mut self) -> &mut Self::R {
        &mut self.input
    }
    fn output(&mut self) -> &mut Self::W {
        &mut self.output
    }
}

struct ShareReadBytes<IO: IoPorts> {
    stdio: Rc<RefCell<IO>>,
}

impl<IO: IoPorts> Iterator for ShareReadBytes<IO> {
    type Item = io::Result<u8>;

    fn next(&mut self) -> Option<Self::Item> {
        Some(self.stdio.borrow_mut().input().read_u8())
    }
}

trait RcIoPorts {
    type IO: IoPorts;
    type Bytes: Iterator<Item = io::Result<u8>>;
    type Chars: Iterator<Item = io::Result<char>>;

    fn shared_reader(&self) -> Self::Bytes;
    fn codepoints(&self) -> Self::Chars;
    fn port<P: IoPort<IO = Self::IO>>(&self, name: &'static str, p: P) -> IoPortWrapper<P>;
}

impl<IO: IoPorts> RcIoPorts for Rc<RefCell<IO>> {
    type IO = IO;
    type Bytes = ShareReadBytes<IO>;
    type Chars = CodePoints<ShareReadBytes<IO>>;

    fn shared_reader(&self) -> Self::Bytes {
        ShareReadBytes {
            stdio: self.clone(),
        }
    }

    fn codepoints(&self) -> Self::Chars {
        CodePoints::from(self.shared_reader())
    }

    fn port<P: IoPort<IO = IO>>(&self, name: &'static str, p: P) -> IoPortWrapper<P> {
        IoPortWrapper {
            name,
            pending: PendingIo::None,
            p,
        }
    }
}

trait IoPort {
    type IO: IoPorts;
    fn read(&mut self) -> Vec<Word>;
    fn write(&mut self, words: &Vec<Word>) -> bool;
    fn io(&mut self) -> RefMut<'_, Self::IO>;
}

struct IoPortStateWrapper<IO: IoPorts, State, InT: IntoIterator, InF, OutF>
where
    InT::Item: Into<Word>,
    InF: for<'a> FnMut(RefMut<'a, IO>, RefMut<'a, State>) -> InT,
    OutF: for<'a> FnMut(RefMut<'a, IO>, RefMut<'a, State>, &'a Vec<Word>) -> bool,
{
    io: Rc<RefCell<IO>>,
    state: Rc<RefCell<State>>,
    read: InF,
    write: OutF,
}

impl<IO: IoPorts, State, InT: IntoIterator, InF, OutF> IoPort
    for IoPortStateWrapper<IO, State, InT, InF, OutF>
where
    InT::Item: Into<Word>,
    InF: for<'a> FnMut(RefMut<'a, IO>, RefMut<'a, State>) -> InT,
    OutF: for<'a> FnMut(RefMut<'a, IO>, RefMut<'a, State>, &'a Vec<Word>) -> bool,
{
    type IO = IO;
    fn read(&mut self) -> Vec<Word> {
        (self.read)(self.io.borrow_mut(), self.state.borrow_mut())
            .into_iter()
            .map(Into::into)
            .collect()
    }

    fn write(&mut self, words: &Vec<Word>) -> bool {
        (self.write)(self.io.borrow_mut(), self.state.borrow_mut(), words)
    }

    fn io(&mut self) -> RefMut<'_, Self::IO> {
        self.io.borrow_mut()
    }
}

struct IoPortImpl<IO: IoPorts, InT: IntoIterator, InF, OutF>
where
    InT::Item: Into<Word>,
    InF: for<'a> FnMut(RefMut<'a, IO>) -> InT,
    OutF: for<'a> FnMut(RefMut<'a, IO>, &'a Vec<Word>) -> bool,
{
    io: Rc<RefCell<IO>>,
    read: InF,
    write: OutF,
}

impl<IO: IoPorts, InT: IntoIterator, InF, OutF> IoPort for IoPortImpl<IO, InT, InF, OutF>
where
    InT::Item: Into<Word>,
    InF: for<'a> FnMut(RefMut<'a, IO>) -> InT,
    OutF: for<'a> FnMut(RefMut<'a, IO>, &'a Vec<Word>) -> bool,
{
    type IO = IO;
    fn read(&mut self) -> Vec<Word> {
        (self.read)(self.io.borrow_mut())
            .into_iter()
            .map(Into::into)
            .collect()
    }

    fn write(&mut self, words: &Vec<Word>) -> bool {
        (self.write)(self.io.borrow_mut(), words)
    }

    fn io(&mut self) -> RefMut<'_, Self::IO> {
        self.io.borrow_mut()
    }
}

enum PendingIo {
    None,
    Read(Vec<Word>),
    Write(Vec<Word>),
}

struct IoPortWrapper<P: IoPort> {
    name: &'static str,
    pending: PendingIo,
    p: P,
}

impl<P: IoPort> PortImpl for IoPortWrapper<P> {
    fn read(&mut self) -> Word {
        match self.pending {
            PendingIo::None => {
                if let Some(pending) = self.p.io().pending() {
                    panic!(
                        "Read from %{} during an incomplete I/O operation on %{pending}",
                        self.name
                    );
                }
                let words = self.p.read();
                self.pending = PendingIo::Read(words);
                *self.p.io().pending() = Some(self.name);
                self.read()
            }
            PendingIo::Read(ref mut pending) => {
                let value = pending.remove(0);
                if pending.is_empty() {
                    self.pending = PendingIo::None;
                    *self.p.io().pending() = None;
                }
                value
            }
            PendingIo::Write(_) => {
                panic!(
                    "Read from %{} during an incomplete write operation",
                    self.name
                );
            }
        }
    }

    fn write(&mut self, value: Word) {
        match self.pending {
            PendingIo::None => {
                if let Some(pending) = self.p.io().pending() {
                    panic!(
                        "Write to %{} during an incomplete I/O operation on %{pending}",
                        self.name
                    );
                }
                self.pending = PendingIo::Write(vec![]);
                *self.p.io().pending() = Some(self.name);
                self.write(value)
            }
            PendingIo::Read(_) => {
                panic!(
                    "Write to %{} during an incomplete read operation",
                    self.name
                );
            }
            PendingIo::Write(ref mut pending) => {
                pending.push(value);
                if self.p.write(&pending) {
                    self.pending = PendingIo::None;
                    *self.p.io().pending() = None;
                }
            }
        }
    }
}

pub fn init(
    ports: &mut [Option<Box<dyn PortImpl>>; 64],
    (input, output): (impl BufRead + 'static, impl Write + 'static),
) {
    macro_rules! port {
        ($port:ident) => {
            ports[Port::$port as usize]
        };
    }

    let io = Rc::new(RefCell::new(ActualIoPorts {
        pending: None,
        input,
        output,
    }));
    let codepoints = Rc::new(RefCell::new(io.codepoints()));
    let numb_radix = Rc::new(RefCell::new(10u16));

    port!(TSPECIAL) = Some(Box::new(io.port(
        "TSPECIAL",
        IoPortImpl {
            io: io.clone(),
            read: |mut io| {
                vec![io
                    .input()
                    .fill_buf()
                    .expect("Failed to read from stdin")
                    .is_empty()]
            },
            write: |_, _| {
                panic!("write to %TSPECIAL");
            },
        },
    )));
    port!(TEXT) = Some(Box::new(io.port(
        "TEXT",
        IoPortImpl {
            io: io.clone(),
            read: |mut io| {
                let mut string = String::new();
                io.input()
                    .read_line(&mut string)
                    .expect("Error reading from stdin");
                let mut buf = vec![string.len() as u16];
                buf.extend(string.bytes().map(u16::from));
                buf
            },
            write: |mut io, words| {
                assert!(!words.is_empty());
                let len = words.len() - 1;
                let mut iwords = words.iter().copied();
                let total_len: usize = iwords.next().unwrap().into();
                if total_len == len {
                    let bytes: Vec<u8> = iwords
                        .map(|w| w.into())
                        .map(|w: u16| u8::try_from(w))
                        .map(|w| w.expect("%TEXT received non-bytes"))
                        .collect();
                    let string = String::from_utf8(bytes).expect("%TEXT received invalid UTF-8");
                    io.output()
                        .write_all(string.as_bytes())
                        .expect("Error writing stdout");
                    true
                } else {
                    false
                }
            },
        },
    )));
    port!(NUMB) = Some(Box::new(io.port(
        "NUMB",
        IoPortStateWrapper {
            io: io.clone(),
            state: numb_radix.clone(),
            read: |mut io, numb_radix| {
                let mut string = String::new();
                io.input()
                    .read_line(&mut string)
                    .expect("Error reading from stdin");
                let num = u16::from_str_radix(&string, *numb_radix as u32)
                    .or_else(|_| i16::from_str_radix(&string, *numb_radix as u32).map(|n| n as u16))
                    .expect("Invalid number for %NUMB");
                vec![num]
            },
            write: |mut io, numb_radix, words| {
                assert_eq!(words.len(), 1);
                let val: u16 = words[0].into();
                io.output()
                    .write_all(radix(val, *numb_radix as u8).to_string().as_bytes())
                    .expect("Error writing stdout");
                true
            },
        },
    )));
    port!(ASCII8) = Some(Box::new(io.port(
        "ASCII8",
        IoPortStateWrapper {
            io: io.clone(),
            state: codepoints.clone(),
            read: |_, mut codepoints| {
                let ch = codepoints
                    .next()
                    .expect("EOF")
                    .expect("Error reading from stdin");
                let val: u8 = ch.try_into().expect("Character out of ASCII8 range");
                iter::once(val as u16)
            },
            write: |mut io, _, words| {
                assert_eq!(words.len(), 1);
                let val: u16 = words[0].into();
                if val > u8::MAX as u16 {
                    panic!("Invalid character {val} for %ASCII8");
                }
                let val = val as u8;
                let ch = val as char;
                io.output()
                    .write_all(ch.encode_utf8(&mut vec![0; 4]).as_bytes())
                    .expect("Error writing stdout");
                true
            },
        },
    )));
    port!(ASCII7) = Some(Box::new(io.port(
        "ASCII7",
        IoPortStateWrapper {
            io: io.clone(),
            state: codepoints.clone(),
            read: |_, mut codepoints| {
                let ch = codepoints
                    .next()
                    .expect("EOF")
                    .expect("Error reading from stdin");
                if !ch.is_ascii() {
                    panic!("Invalid character {ch} for %ASCII7");
                }
                let val: u8 = ch.try_into().unwrap();
                iter::once(val as u16)
            },
            write: |mut io, _, words| {
                assert_eq!(words.len(), 1);
                let val: u16 = words[0].into();
                let ch = std::char::from_u32(val as u32).expect("Invalid character");
                if !ch.is_ascii() {
                    panic!("Invalid character {ch} for %ASCII7");
                }
                io.output()
                    .write_all(ch.encode_utf8(&mut vec![0; 4]).as_bytes())
                    .expect("Error writing stdout");
                true
            },
        },
    )));
    port!(UTF8) = Some(Box::new(io.port(
        "UTF8",
        IoPortStateWrapper {
            io: io.clone(),
            state: codepoints.clone(),
            read: |_, mut codepoints| {
                let char = codepoints
                    .next()
                    .expect("EOF")
                    .expect("Error reading from stdin");
                let mut buf = vec![0; 4];
                let len = char.encode_utf8(&mut buf).len();
                buf.truncate(len);
                buf.into_iter().map(u16::from)
            },
            write: |mut io, _, words| {
                assert!(!words.is_empty());
                let bytes: Vec<u8> = words
                    .iter()
                    .copied()
                    .map(|w| w.into())
                    .map(|w: u16| u8::try_from(w))
                    .map(|w| w.expect("%UTF8 received non-bytes"))
                    .collect();
                let len = match bytes[0] {
                    0b_00000000..=0b_01111111 => 1,
                    0b_11000000..=0b_11011111 => 2,
                    0b_11100000..=0b_11101111 => 3,
                    0b_11110000..=0b_11110111 => 4,
                    _ => panic!("%UTF8 received invalid UTF-8: {:?}", bytes),
                };
                if bytes.len() < len {
                    false
                } else {
                    let mut codepoint: u32 = match len {
                        1 => bytes[0] & 0b_01111111,
                        2 => bytes[0] & 0b_00011111,
                        3 => bytes[0] & 0b_00001111,
                        4 => bytes[0] & 0b_00000111,
                        _ => unreachable!(),
                    } as u32;
                    for i in 1..len {
                        if bytes[i] & 0b_11000000 != 0b_10000000 {
                            panic!("%UTF8 received invalid UTF-8: {:?}", bytes);
                        }
                        codepoint <<= 6;
                        codepoint |= (bytes[i] & 0b_00111111) as u32;
                    }
                    let ch = std::char::from_u32(codepoint)
                        .expect("%UTF8 received an invalid codepoint");
                    io.output()
                        .write_all(ch.encode_utf8(&mut vec![0; 4]).as_bytes())
                        .expect("Error writing stdout");
                    true
                }
            },
        },
    )));

    port!(INT) = Some(Box::new(io.port(
        "INT",
        IoPortStateWrapper {
            io: io.clone(),
            state: numb_radix.clone(),
            read: move |mut io, numb_radix| {
                let mut string = String::new();
                io.input()
                    .read_line(&mut string)
                    .expect("Error reading from stdin");
                let num = i16::from_str_radix(&string, *numb_radix as u32)
                    .expect("Invalid number for %INT");
                iter::once(num)
            },
            write: |mut io, numb_radix, words| {
                assert_eq!(words.len(), 1);
                let val: i16 = words[0].into();
                io.output()
                    .write_all(radix(val, *numb_radix as u8).to_string().as_bytes())
                    .expect("Error writing stdout");
                true
            },
        },
    )));

    port!(UINT) = Some(Box::new(io.port(
        "UINT",
        IoPortStateWrapper {
            io: io.clone(),
            state: numb_radix.clone(),
            read: |mut io, numb_radix| {
                let mut string = String::new();
                io.input()
                    .read_line(&mut string)
                    .expect("Error reading from stdin");
                let num = u16::from_str_radix(&string, *numb_radix as u32)
                    .expect("Invalid number for %UINT");
                iter::once(num)
            },
            write: |mut io, numb_radix, words| {
                assert_eq!(words.len(), 1);
                let val: u16 = words[0].into();
                io.output()
                    .write_all(radix(val, *numb_radix as u8).to_string().as_bytes())
                    .expect("Error writing stdout");
                true
            },
        },
    )));

    port!(BIN) = Some(Box::new(io.port(
        "BIN",
        IoPortImpl {
            io: io.clone(),
            read: |mut io| {
                let mut string = String::new();
                io.input()
                    .read_line(&mut string)
                    .expect("Error reading from stdin");
                let num = u16::from_str_radix(&string, 2).expect("Invalid number for %BIN");
                iter::once(num)
            },
            write: |mut io, words| {
                assert_eq!(words.len(), 1);
                let val: u16 = words[0].into();
                io.output()
                    .write_all(radix(val, 2).to_string().as_bytes())
                    .expect("Error writing stdout");
                true
            },
        },
    )));

    port!(HEX) = Some(Box::new(io.port(
        "HEX",
        IoPortImpl {
            io: io.clone(),
            read: |mut io| {
                let mut string = String::new();
                io.input()
                    .read_line(&mut string)
                    .expect("Error reading from stdin");
                let num = u16::from_str_radix(&string, 16).expect("Invalid number for %HEX");
                iter::once(num)
            },
            write: |mut io, words| {
                assert_eq!(words.len(), 1);
                let val: u16 = words[0].into();
                io.output()
                    .write_all(radix(val, 16).to_string().as_bytes())
                    .expect("Error writing stdout");
                true
            },
        },
    )));

    port!(FLOAT) = Some(Box::new(io.port(
        "HEX",
        IoPortImpl {
            io: io.clone(),
            read: |mut io| {
                let mut string = String::new();
                io.input()
                    .read_line(&mut string)
                    .expect("Error reading from stdin");
                let num = f16::from_str(&string).expect("Invalid number for %FLOAT");
                iter::once(num)
            },
            write: |mut io, words| {
                assert_eq!(words.len(), 1);
                let val: f16 = words[0].into();
                io.output()
                    .write_all(val.to_string().as_bytes())
                    .expect("Error writing stdout");
                true
            },
        },
    )));

    port!(NSPECIAL) = Some(Box::new({
        AnonPort {
            state: numb_radix.clone(),
            read: |radix| *radix.borrow(),
            write: |radix, word| {
                let word: u16 = word.into();
                if !matches!(word, 1..=36) {
                    panic!("Invalid radix to %NSPECIAL: {} (must be in 1..=36)", word);
                }
                *radix.borrow_mut() = word;
            },
        }
    }))
}
