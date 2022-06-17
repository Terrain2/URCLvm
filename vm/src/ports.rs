use super::*;

use derive_more::Display;
use std::{
    cell::RefCell,
    io::{Read, Seek, SeekFrom, Write},
    ops::{Index, IndexMut},
    rc::Rc,
};
use terminal::Terminal;

#[path = "io_ports.rs"]
mod io_ports;

#[derive(Clone, Copy, Debug, Display)]
pub struct Port(pub u8);

pub struct Ports {
    ports: [Option<Box<dyn PortImpl>>; 64],
}

#[allow(dead_code)] // unused ports
mod std_ports {
    pub const CPUBUS: usize = 0;
    pub const TEXT: usize = 1;
    pub const NUMB: usize = 2;
    pub const SUPPORTED: usize = 5;
    pub const SPECIAL: usize = 6;
    pub const PROFILE: usize = 7;
    // Graphics
    pub const X: usize = 8;
    pub const Y: usize = 9;
    pub const COLOR: usize = 10;
    pub const BUFFER: usize = 11;
    pub const GSPECIAL: usize = 15;
    // Text
    pub const ASCII8: usize = 16;
    pub const CHAR5: usize = 17;
    pub const CHAR6: usize = 18;
    pub const ASCII7: usize = 19;
    pub const UTF8: usize = 20;
    pub const TSPECIAL: usize = 23;
    // Numbers
    pub const INT: usize = 24;
    pub const UINT: usize = 25;
    pub const BIN: usize = 26;
    pub const HEX: usize = 27;
    pub const FLOAT: usize = 28;
    pub const FIXED: usize = 29;
    pub const NSPECIAL: usize = 31;
    // Storage
    pub const ADDR: usize = 32;
    pub const BUS: usize = 33;
    pub const PAGE: usize = 34;
    pub const SSPECIAL: usize = 39;
    // Miscellaneous
    pub const RNG: usize = 40;
    pub const NOTE: usize = 41;
    pub const INSTR: usize = 42;
    pub const NLEG: usize = 43;
    pub const WAIT: usize = 44;
    pub const NADDR: usize = 45;
    pub const DATA: usize = 46;
    pub const MSPECIAL: usize = 47;
}

impl From<[Option<Box<dyn PortImpl>>; 64]> for Ports {
    fn from(mut ports: [Option<Box<dyn PortImpl>>; 64]) -> Self {
        if let None = ports[std_ports::SUPPORTED] {
            let mut supported = ports.each_ref().map(Option::is_some);
            supported[std_ports::SUPPORTED] = true;
            ports[std_ports::SUPPORTED] = Some(Box::new(SupportedPort(supported, None)));
        }
        Ports { ports }
    }
}

impl Index<Port> for Ports {
    type Output = Box<dyn PortImpl>;

    fn index(&self, Port(port): Port) -> &Self::Output {
        match &self.ports[port as usize] {
            Some(port) => port,
            None => panic!("Usage of %{} is not supported", port),
        }
    }
}

impl IndexMut<Port> for Ports {
    fn index_mut(&mut self, Port(port): Port) -> &mut Self::Output {
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
        io: Option<Terminal<impl Write + 'static>>,
        storage: Option<(impl Read + Write + Seek + AdjustLength + 'static, u16)>,
        image: Option<(
            impl Image<Pixel> + 'static,
            impl Fn(u16) -> Pixel + 'static,
            impl Fn(Pixel) -> u16 + 'static,
        )>,
    ) -> Self {
        let mut ports = [(); 64].map(|_| None::<Box<dyn PortImpl>>);

        macro_rules! port {
            ($port:ident) => {
                ports[std_ports::$port]
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

        if let Some((storage, pages)) = storage {
            struct StorageState<S: Read + Write + Seek> {
                storage: S,
                page: u16,
                addr: u16,
                pages: u16,
            }
            impl<S: Read + Write + Seek + AdjustLength> StorageState<S> {
                fn seek(&mut self) {
                    self.storage
                        .seek(SeekFrom::Start(
                            (((self.page as u64) << 16) | (self.addr as u64)) << 1,
                        ))
                        .expect("Error seeking in storage");
                }
            }
            let state = Rc::new(RefCell::new(StorageState {
                storage,
                page: 0,
                addr: 0,
                pages,
            }));
            port!(PAGE) = Some(Box::new({
                AnonPort {
                    state: state.clone(),
                    read: |state| state.borrow().page,
                    write: |state, word| {
                        let page: u16 = word.into();
                        let pages = state.borrow().pages;
                        if page >= pages {
                            panic!(
                                "%PAGE got {page}, which is out of range, storage only has {pages}"
                            );
                        }
                        state.borrow_mut().page = page;
                    },
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

            port!(SSPECIAL) = Some(Box::new({
                AnonPort {
                    state: state.clone(),
                    read: |state| state.borrow().pages,
                    write: |state, word| {
                        let pages: u16 = word.into();
                        if pages == 0 {
                            panic!("%SSPECIAL received 0. You cannot delete storage.");
                        }
                        let mut state = state.borrow_mut();
                        state.pages = pages;
                        if state.page >= state.pages {
                            state.page = state.pages - 1;
                        }
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
