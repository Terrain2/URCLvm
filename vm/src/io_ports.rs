use std::{
    cell::{RefCell, RefMut},
    io::Write,
    iter,
    rc::Rc,
};

use super::*;
use half::f16;
use radix_fmt::radix;

// This is here so i can use associated types and pass IO: IoPorts without explicitly capturing R and W separately.
trait IoPorts {
    type W: Write;
    fn pending(&mut self) -> &mut Option<&'static str>;
    fn terminal(&mut self) -> &mut Terminal<Self::W>;
    fn char(&mut self) -> char;
    fn read_line(&mut self) -> String;
}

struct ActualIoPorts<W: Write> {
    pending: Option<&'static str>,
    terminal: Terminal<W>,
}

impl<W: Write> IoPorts for ActualIoPorts<W> {
    type W = W;
    fn pending(&mut self) -> &mut Option<&'static str> {
        &mut self.pending
    }
    fn terminal(&mut self) -> &mut Terminal<Self::W> {
        &mut self.terminal
    }

    fn char(&mut self) -> char {
        self.terminal
            .act(terminal::Action::EnableRawMode)
            .expect("Failed to enable raw mode");
        let ch = loop {
            if let terminal::Retrieved::Event(Some(terminal::Event::Key(key_event))) = self
                .terminal
                .get(terminal::Value::Event(None))
                .expect("Error reading stdin")
            {
                match key_event.code {
                    terminal::KeyCode::Char(c) => break c,
                    terminal::KeyCode::Backspace => break '\x7f',
                    terminal::KeyCode::Enter => break '\n',
                    terminal::KeyCode::Null => break '\0',
                    terminal::KeyCode::Tab => break '\t',
                    _ => continue,
                }
            }
        };
        self.terminal
            .act(terminal::Action::DisableRawMode)
            .expect("Failed to disable raw mode");
        ch
    }

    fn read_line(&mut self) -> String {
        self.terminal
            .act(terminal::Action::EnableRawMode)
            .expect("Failed to enable raw mode");
        let mut line = String::new();
        loop {
            if let terminal::Retrieved::Event(Some(terminal::Event::Key(key_event))) = self
                .terminal
                .get(terminal::Value::Event(None))
                .expect("Error reading stdin")
            {
                match key_event.code {
                    terminal::KeyCode::Enter => break,
                    terminal::KeyCode::Char(ch) => line.push(ch),
                    terminal::KeyCode::Backspace => drop(line.pop()),
                    _ => (),
                }
            }
        }
        self.terminal
            .act(terminal::Action::DisableRawMode)
            .expect("Failed to disable raw mode");
        line
    }
}

enum WriteResult {
    Finished,
    MoreDataPls,
}

trait RcIoPorts {
    type IO: IoPorts;

    fn port<P: IoPort<IO = Self::IO>>(&self, name: &'static str, p: P) -> IoPortWrapper<P>;
}

impl<IO: IoPorts> RcIoPorts for Rc<RefCell<IO>> {
    type IO = IO;

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
    fn write(&mut self, words: &Vec<Word>) -> WriteResult;
    fn io(&mut self) -> RefMut<'_, Self::IO>;
}

struct IoPortStateWrapper<IO: IoPorts, State, InT: IntoIterator, InF, OutF>
where
    InT::Item: Into<Word>,
    InF: for<'a> FnMut(RefMut<'a, IO>, RefMut<'a, State>) -> InT,
    OutF: for<'a> FnMut(RefMut<'a, IO>, RefMut<'a, State>, &'a Vec<Word>) -> WriteResult,
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
    OutF: for<'a> FnMut(RefMut<'a, IO>, RefMut<'a, State>, &'a Vec<Word>) -> WriteResult,
{
    type IO = IO;
    fn read(&mut self) -> Vec<Word> {
        (self.read)(self.io.borrow_mut(), self.state.borrow_mut())
            .into_iter()
            .map(Into::into)
            .collect()
    }

    fn write(&mut self, words: &Vec<Word>) -> WriteResult {
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
    OutF: for<'a> FnMut(RefMut<'a, IO>, &'a Vec<Word>) -> WriteResult,
{
    io: Rc<RefCell<IO>>,
    read: InF,
    write: OutF,
}

impl<IO: IoPorts, InT: IntoIterator, InF, OutF> IoPort for IoPortImpl<IO, InT, InF, OutF>
where
    InT::Item: Into<Word>,
    InF: for<'a> FnMut(RefMut<'a, IO>) -> InT,
    OutF: for<'a> FnMut(RefMut<'a, IO>, &'a Vec<Word>) -> WriteResult,
{
    type IO = IO;
    fn read(&mut self) -> Vec<Word> {
        (self.read)(self.io.borrow_mut())
            .into_iter()
            .map(Into::into)
            .collect()
    }

    fn write(&mut self, words: &Vec<Word>) -> WriteResult {
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
                if let WriteResult::Finished = self.p.write(&pending) {
                    self.pending = PendingIo::None;
                    *self.p.io().pending() = None;
                }
            }
        }
    }
}

pub fn init(ports: &mut [Option<Box<dyn PortImpl>>; 64], terminal: Terminal<impl Write + 'static>) {
    macro_rules! port {
        ($port:ident) => {
            ports[std_ports::$port]
        };
    }

    let io = Rc::new(RefCell::new(ActualIoPorts {
        pending: None,
        terminal,
    }));
    let numb_radix = Rc::new(RefCell::new(10u16));

    port!(TEXT) = Some(Box::new(io.port(
        "TEXT",
        IoPortImpl {
            io: io.clone(),
            read: |mut io| {
                let ch = io.char() as u32;
                if ch > u16::MAX as u32 {
                    panic!("Invalid character for %TEXT (out of range)");
                }
                vec![ch as u16]
            },
            write: |mut io, words| {
                let ch: u16 = words[0].into();
                let ch = char::try_from(ch as u32).unwrap_or_else(|_| {
                    panic!("Invalid value {ch} for %TEXT (i think it's a surrogate pair?)")
                });
                let mut utf8 = [0; 4];
                io.terminal()
                    .write(ch.encode_utf8(&mut utf8).as_bytes())
                    .expect("Error writing to stdout");
                WriteResult::Finished
            },
        },
    )));
    port!(NUMB) = Some(Box::new(io.port(
        "NUMB",
        IoPortStateWrapper {
            io: io.clone(),
            state: numb_radix.clone(),
            read: |mut io, numb_radix| {
                let string = io.read_line();
                let num = u16::from_str_radix(&string, *numb_radix as u32)
                    .or_else(|_| i16::from_str_radix(&string, *numb_radix as u32).map(|n| n as u16))
                    .expect("Invalid number for %NUMB");
                vec![num]
            },
            write: |mut io, numb_radix, words| {
                assert_eq!(words.len(), 1);
                let val: u16 = words[0].into();
                io.terminal()
                    .write_all(radix(val, *numb_radix as u8).to_string().as_bytes())
                    .expect("Error writing stdout");
                WriteResult::Finished
            },
        },
    )));
    port!(ASCII8) = Some(Box::new(io.port(
        "ASCII8",
        IoPortImpl {
            io: io.clone(),
            read: |mut io| {
                let val: u8 = io.char().try_into().expect("Character out of ASCII8 range");
                iter::once(val as u16)
            },
            write: |mut io, words| {
                assert_eq!(words.len(), 1);
                let val: u16 = words[0].into();
                if val > u8::MAX as u16 {
                    panic!("Invalid character {val} for %ASCII8");
                }
                let val = val as u8;
                let ch = val as char;
                io.terminal()
                    .write_all(ch.encode_utf8(&mut vec![0; 4]).as_bytes())
                    .expect("Error writing stdout");
                WriteResult::Finished
            },
        },
    )));
    port!(ASCII7) = Some(Box::new(io.port(
        "ASCII7",
        IoPortImpl {
            io: io.clone(),
            read: |mut io| {
                let ch = io.char();
                if !ch.is_ascii() {
                    panic!("Invalid character {ch} for %ASCII7");
                }
                let val: u8 = ch.try_into().unwrap();
                iter::once(val as u16)
            },
            write: |mut io, words| {
                assert_eq!(words.len(), 1);
                let val: u16 = words[0].into();
                let ch = std::char::from_u32(val as u32).expect("Invalid character");
                if !ch.is_ascii() {
                    panic!("Invalid character {ch} for %ASCII7");
                }
                io.terminal()
                    .write_all(ch.encode_utf8(&mut vec![0; 4]).as_bytes())
                    .expect("Error writing stdout");
                WriteResult::Finished
            },
        },
    )));
    port!(UTF8) = Some(Box::new(io.port(
        "UTF8",
        IoPortImpl {
            io: io.clone(),
            read: |mut io| {
                let char = io.char();
                let mut buf = vec![0; 4];
                let len = char.encode_utf8(&mut buf).len();
                buf.truncate(len);
                buf.into_iter().map(u16::from)
            },
            write: |mut io, words| {
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
                    WriteResult::MoreDataPls
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
                    io.terminal()
                        .write_all(ch.encode_utf8(&mut vec![0; 4]).as_bytes())
                        .expect("Error writing stdout");
                    WriteResult::Finished
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
                let string = io.read_line();
                let num = i16::from_str_radix(&string, *numb_radix as u32)
                    .expect("Invalid number for %INT");
                iter::once(num)
            },
            write: |mut io, numb_radix, words| {
                assert_eq!(words.len(), 1);
                let val: i16 = words[0].into();
                io.terminal()
                    .write_all(radix(val, *numb_radix as u8).to_string().as_bytes())
                    .expect("Error writing stdout");
                WriteResult::Finished
            },
        },
    )));

    port!(UINT) = Some(Box::new(io.port(
        "UINT",
        IoPortStateWrapper {
            io: io.clone(),
            state: numb_radix.clone(),
            read: |mut io, numb_radix| {
                let string = io.read_line();
                let num = u16::from_str_radix(&string, *numb_radix as u32)
                    .expect("Invalid number for %UINT");
                iter::once(num)
            },
            write: |mut io, numb_radix, words| {
                assert_eq!(words.len(), 1);
                let val: u16 = words[0].into();
                io.terminal()
                    .write_all(radix(val, *numb_radix as u8).to_string().as_bytes())
                    .expect("Error writing stdout");
                WriteResult::Finished
            },
        },
    )));

    port!(BIN) = Some(Box::new(io.port(
        "BIN",
        IoPortImpl {
            io: io.clone(),
            read: |mut io| {
                let string = io.read_line();
                let num = u16::from_str_radix(&string, 2).expect("Invalid number for %BIN");
                iter::once(num)
            },
            write: |mut io, words| {
                assert_eq!(words.len(), 1);
                let val: u16 = words[0].into();
                io.terminal()
                    .write_all(radix(val, 2).to_string().as_bytes())
                    .expect("Error writing stdout");
                WriteResult::Finished
            },
        },
    )));

    port!(HEX) = Some(Box::new(io.port(
        "HEX",
        IoPortImpl {
            io: io.clone(),
            read: |mut io| {
                let string = io.read_line();
                let num = u16::from_str_radix(&string, 16).expect("Invalid number for %HEX");
                iter::once(num)
            },
            write: |mut io, words| {
                assert_eq!(words.len(), 1);
                let val: u16 = words[0].into();
                io.terminal()
                    .write_all(radix(val, 16).to_string().as_bytes())
                    .expect("Error writing stdout");
                WriteResult::Finished
            },
        },
    )));

    port!(FLOAT) = Some(Box::new(io.port(
        "HEX",
        IoPortImpl {
            io: io.clone(),
            read: |mut io| {
                let string = io.read_line();
                let num = f16::from_str(&string).expect("Invalid number for %FLOAT");
                iter::once(num)
            },
            write: |mut io, words| {
                assert_eq!(words.len(), 1);
                let val: f16 = words[0].into();
                io.terminal()
                    .write_all(val.to_string().as_bytes())
                    .expect("Error writing stdout");
                WriteResult::Finished
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
