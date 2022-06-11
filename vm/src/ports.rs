use super::*;

use derive_more::Display;
use derive_try_from_primitive::TryFromPrimitive;
use std::{
    cell::{RefCell},
    io::{BufRead, Read, Seek, SeekFrom, Write},
    ops::{Index, IndexMut},
    rc::Rc,
};

#[path = "io_ports.rs"]
mod io_ports;

#[derive(Clone, Copy, Debug, Display, TryFromPrimitive)]
#[repr(u8)]
pub enum Port {
    // General
    CPUBUS = 0,
    TEXT = 1,
    NUMB = 2,
    SUPPORTED = 5,
    SPECIAL = 6,
    PROFILE = 7,
    // Graphics
    X = 8,
    Y = 9,
    COLOR = 10,
    BUFFER = 11,
    GSPECIAL = 15,
    // Text
    ASCII8 = 16,
    CHAR5 = 17,
    CHAR6 = 18,
    ASCII7 = 19,
    UTF8 = 20,
    TSPECIAL = 23,
    // Numbers
    INT = 24,
    UINT = 25,
    BIN = 26,
    HEX = 27,
    FLOAT = 28,
    FIXED = 29,
    NSPECIAL = 31,
    // Storage
    ADDR = 32,
    BUS = 33,
    PAGE = 34,
    SSPECIAL = 39,
    // Miscellaneous
    RNG = 40,
    NOTE = 41,
    INSTR = 42,
    NLEG = 43,
    WAIT = 44,
    NADDR = 45,
    DATA = 46,
    MSPECIAL = 47,
    // User defined
    UD1 = 48,
    UD2 = 49,
    UD3 = 50,
    UD4 = 51,
    UD5 = 52,
    UD6 = 53,
    UD7 = 54,
    UD8 = 55,
    UD9 = 56,
    UD10 = 57,
    UD11 = 58,
    UD12 = 59,
    UD13 = 60,
    UD14 = 61,
    UD15 = 62,
    UD16 = 63,
}

pub struct Ports {
    ports: [Option<Box<dyn PortImpl>>; 64],
}

impl From<[Option<Box<dyn PortImpl>>; 64]> for Ports {
    fn from(mut ports: [Option<Box<dyn PortImpl>>; 64]) -> Self {
        if let None = ports[Port::SUPPORTED as usize] {
            let mut supported = ports.each_ref().map(Option::is_some);
            supported[Port::SUPPORTED as usize] = true;
            ports[Port::SUPPORTED as usize] = Some(Box::new(SupportedPort(supported, None)));
        }
        Ports { ports }
    }
}

impl Index<Port> for Ports {
    type Output = Box<dyn PortImpl>;

    fn index(&self, port: Port) -> &Self::Output {
        match &self.ports[port as usize] {
            Some(port) => port,
            None => panic!("Usage of %{} is not supported", port),
        }
    }
}

impl IndexMut<Port> for Ports {
    fn index_mut(&mut self, port: Port) -> &mut Self::Output {
        match &mut self.ports[port as usize] {
            Some(port) => port,
            None => panic!("Usage of %{} is not supported", port),
        }
    }
}

pub trait PortImpl {
    fn read(&mut self) -> Word;
    fn write(&mut self, value: Word);
}

impl PortImpl for Box<dyn PortImpl> {
    fn read(&mut self) -> Word {
        self.as_mut().read()
    }

    fn write(&mut self, value: Word) {
        self.as_mut().write(value);
    }
}

pub struct AnonPort<State, InT, InF, OutF>
where
    InT: Into<Word>,
    InF: for<'a> FnMut(&State) -> InT,
    OutF: for<'a> FnMut(&State, Word),
{
    state: State,
    read: InF,
    write: OutF,
}

impl<State, InT, InF, OutF> PortImpl for AnonPort<State, InT, InF, OutF>
where
    InT: Into<Word>,
    InF: for<'a> FnMut(&State) -> InT,
    OutF: for<'a> FnMut(&State, Word),
{
    fn read(&mut self) -> Word {
        (self.read)(&self.state).into()
    }

    fn write(&mut self, value: Word) {
        (self.write)(&self.state, value);
    }
}

struct SupportedPort([bool; 64], Option<bool>);

impl PortImpl for SupportedPort {
    fn read(&mut self) -> Word {
        self.1
            .expect("Attempted to read from %SUPPORTED without setting the port")
            .into()
    }

    fn write(&mut self, value: Word) {
        let value: usize = value.into();
        self.1 = Some(self.0.get(value).map_or(false, |&b| b))
    }
}

impl Ports {
    pub fn standard<Pixel: image::Pixel>(
        io: Option<(impl BufRead + 'static, impl Write + 'static)>,
        storage: Option<impl Read + Write + Seek + 'static>,
        image: Option<(
            impl Image<Pixel> + 'static,
            impl Fn(u16) -> Pixel + 'static,
            impl Fn(Pixel) -> u16 + 'static,
        )>,
    ) -> Self {
        let mut ports = [(); 64].map(|_| None::<Box<dyn PortImpl>>);

        macro_rules! port {
            ($port:ident) => {
                ports[Port::$port as usize]
            };
        }

        if let Some(io) = io {
            io_ports::init(&mut ports, io);
        }

        struct RandPort {
            rng: fastrand::Rng,
            seed: Wrapping<u64>,
        }

        port!(RNG) = Some(Box::new(RandPort {
            rng: fastrand::Rng::new(),
            seed: Wrapping(0),
        }));

        impl PortImpl for RandPort {
            fn read(&mut self) -> Word {
                self.rng.u16(..).into()
            }

            fn write(&mut self, value: Word) {
                let value: u16 = value.into();
                self.seed <<= 16;
                self.seed |= value as u64;
                self.rng.seed(self.seed.0);
            }
        }

        if let Some(storage) = storage {
            struct StorageState<S: Read + Write + Seek> {
                storage: S,
                page: u16,
                addr: u16,
            }
            impl<S: Read + Write + Seek> StorageState<S> {
                fn seek(&mut self) {
                    self.storage
                        .seek(SeekFrom::Start(
                            (((self.page as u64) << 16) | self.addr as u64) << 1,
                        ))
                        .expect("Error seeking in storage");
                }
            }
            let state = Rc::new(RefCell::new(StorageState {
                storage,
                page: 0,
                addr: 0,
            }));
            port!(PAGE) = Some(Box::new({
                AnonPort {
                    state: state.clone(),
                    read: |state| state.borrow().page,
                    write: |state, word| state.borrow_mut().page = word.into(),
                }
            }));

            port!(ADDR) = Some(Box::new({
                AnonPort {
                    state: state.clone(),
                    read: |state| state.borrow().addr,
                    write: |state, word| state.borrow_mut().addr = word.into(),
                }
            }));

            port!(BUS) = Some(Box::new({
                AnonPort {
                    state: state.clone(),
                    read: |state| {
                        state.borrow_mut().seek();
                        state
                            .borrow_mut()
                            .storage
                            .read_u16::<BigEndian>()
                            .expect("Error reading from storage")
                    },
                    write: |state, word| {
                        state.borrow_mut().seek();
                        state
                            .borrow_mut()
                            .storage
                            .write_u16::<BigEndian>(word.into())
                            .expect("Error writing to storage");
                    },
                }
            }));
        }

        if let Some((image, word_to_color, color_to_word)) = image {
            let x = Rc::new(RefCell::new(0));
            let y = Rc::new(RefCell::new(0));
            let use_buffer = Rc::new(RefCell::new(true));
            let image = Rc::new(RefCell::new(image));

            port!(X) = Some(Box::new({
                AnonPort {
                    state: x.clone(),
                    read: |x| *x.borrow(),
                    write: |x, word| *x.borrow_mut() = word.into(),
                }
            }));

            port!(Y) = Some(Box::new({
                AnonPort {
                    state: y.clone(),
                    read: |y| *y.borrow(),
                    write: |y, word| *y.borrow_mut() = word.into(),
                }
            }));

            port!(COLOR) = Some(Box::new({
                AnonPort {
                    state: (image.clone(), x, y, color_to_word, word_to_color),
                    read: |(image, x, y, color_to_word, _)| {
                        let x = *x.borrow();
                        let y = *y.borrow();
                        let mut image = image.borrow_mut();
                        let pixel = image.pixel(x, y);
                        color_to_word(*pixel)
                    },
                    write: |(image, x, y, _, word_to_color), word| {
                        let x = *x.borrow();
                        let y = *y.borrow();
                        let mut image = image.borrow_mut();
                        let pixel = image.pixel(x, y);
                        *pixel = word_to_color(word.into())
                    },
                }
            }));

            port!(BUFFER) = Some(Box::new({
                AnonPort {
                    state: (use_buffer, image),
                    read: |(use_buffer, _)| if *use_buffer.borrow() { 1u16 } else { 0u16 },
                    write: |(use_buffer, image), word| {
                        let val = word != 0u16;
                        *use_buffer.borrow_mut() = val;
                        if val {
                            image.borrow_mut().save();
                        }
                    },
                }
            }));
        }

        Ports { ports }
    }
}
