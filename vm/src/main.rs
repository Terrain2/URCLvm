#![feature(array_methods)]

use std::{
    cmp,
    fs::{self, OpenOptions},
    io::{self, Read},
    num::Wrapping,
    path::PathBuf,
    str::FromStr,
};

mod common;
mod instructions;
mod ports;
pub use common::*;
use image::{
    codecs::{
        bmp::{BmpDecoder, BmpEncoder},
        png::{PngDecoder, PngEncoder},
    },
    ColorType, ImageBuffer, ImageDecoder, ImageEncoder, Rgba, RgbaImage,
};
pub use instructions::*;
pub use ports::*;

use byteorder::{BigEndian, ReadBytesExt, WriteBytesExt};
use clap::{ArgEnum, Parser};

#[derive(Parser, Debug)]
struct Args {
    program: PathBuf,
    /// Path to the file to be used for storage ports.
    /// If not specified, those ports will panic.
    #[clap(long)]
    storage: Option<PathBuf>,
    /// Path to the file to be used for graphics ports.
    /// If not specified, those ports will panic.
    #[clap(long)]
    image: Option<PathBuf>,
    /// The image encoding to use when writing to the image file.
    /// Defaults to trying the file extension.
    #[clap(long, arg_enum)]
    encoding: Option<ImageEncoding>,
    /// Dimensions of the image to create, in pixels (WIDTHxHEIGHT).
    /// Ignored if the image exists or was not specified.
    #[clap(long)]
    dimensions: Option<ImgDim>,
}

#[derive(Debug)]
struct ImgDim {
    width: u16,
    height: u16,
}

impl FromStr for ImgDim {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (width, height) = s.split_once('x').ok_or("NxN dimensions required")?;
        Ok(Self {
            width: u16::from_str(width).map_err(|_| "Invalid width")?,
            height: u16::from_str(height).map_err(|_| "Invalid height")?,
        })
    }
}

#[derive(ArgEnum, Clone, Copy, Debug)]
enum ImageEncoding {
    Png,
    Qoi,
    Bmp,
}

pub trait Image<Pixel> {
    fn width(&self) -> u16;
    fn height(&self) -> u16;
    fn pixel(&mut self, x: u16, y: u16) -> &mut Pixel;
    fn save(&self);
}

struct ImageImpl<Save: Fn(&RgbaImage)> {
    image: RgbaImage,
    save: Save,
}

impl<Save: Fn(&RgbaImage)> Image<Rgba<u8>> for ImageImpl<Save> {

    fn width(&self) -> u16 {
        self.image.width() as u16
    }
    fn height(&self) -> u16 {
        self.image.height() as u16
    }

    fn pixel(&mut self, x: u16, y: u16) -> &mut Rgba<u8> {
        self.image.get_pixel_mut(x as u32, y as u32)
    }

    fn save(&self) {
        (self.save)(&self.image);
    }
}

fn main() {
    let args = Args::parse();
    let (memory, bottom_of_stack) = {
        let mut input = fs::File::open(args.program).expect("Could not open program file");

        let file_length = input.metadata().expect("Error reading program metadata").len();
        if file_length > usize::MAX as u64 {
            panic!("File is too large to load into memory");
        }
        let file_length = file_length as usize;
        if file_length & 1 == 1 {
            panic!("The input file has half a word at the end");
        }

        let file_length = file_length / 2;

        if file_length < 2 {
            panic!("The input file is not long enough to contain a header");
        }

        let file_length = file_length - 2;

        let file_length = file_length as u16;
        let minheap = input.read_u16::<BigEndian>().expect("Error reading program header");
        let minstack = input.read_u16::<BigEndian>().expect("Error reading program header");

        if file_length
            .checked_add(minheap)
            .map(|sum| sum.checked_add(minstack))
            .is_none()
        {
            panic!("The code, heap and stack will not fit in addressable memory");
        }

        let mut memory = VmMem::new(
            file_length
                + minheap
                + minstack
                // if minheap + minstack are zero, then executing the last instruction in file_length will eagerly read the next word to decode registers
                // if file_length == u16::MAX, reading that will overflow, so prefer that, rather than the length overflowing to zero
                + if minheap + minstack == 0 && file_length != u16::MAX {
                    1
                } else {
                    0
                },
        );

        for i in 0..file_length {
            memory[Word::from(i)] = Word::from(input.read_u16::<BigEndian>().expect("Error reading program data"));
        }

        let bottom_of_stack = memory.len() - minstack;
        (memory, bottom_of_stack)
    };

    let mut pc = Wrapping(0);

    let mut state = VmState {
        registers: Registers::default(),
        memory,
        ports: Ports::standard(
            Some((io::BufReader::new(io::stdin()), io::stdout())),
            args.storage.map(|s| {
                const FILE_SIZE: u64 = 2 * (u16::MAX as u64 + 1);
                let storage = OpenOptions::new()
                    .read(true)
                    .write(true)
                    .create(true)
                    .open(s)
                    .expect("Error opening storage file");
                let len = storage
                    .metadata()
                    .expect("Error reading storage file metadata")
                    .len();
                if len == 0 {
                    storage
                        .set_len(FILE_SIZE)
                        .expect("Error creating storage file")
                } else if len != FILE_SIZE {
                    panic!("Storage file is not the correct size: it MUST be {FILE_SIZE} bytes")
                }
                storage
            }),
            args.image.map(|path| {
                let encoding = args.encoding.unwrap_or_else(|| {
                    match path
                        .extension()
                        .expect("No file extension for image")
                        .to_str()
                        .expect("Invalid file extension")
                    {
                        "png" => ImageEncoding::Png,
                        "qoi" => ImageEncoding::Qoi,
                        "bmp" => ImageEncoding::Bmp,
                        ext => panic!("Unsupported image filetype: {ext}"),
                    }
                });
                let image = if path.exists() {
                    let mut img = fs::File::open(&path).expect("Error opening image file");
                    macro_rules! img {
                        ($decoder:ident) => {{
                            let decoder = $decoder::new(img).expect("Error loading image");
                            assert_eq!(
                                decoder.color_type(),
                                ColorType::Rgba8,
                                "Invalid color type in file"
                            );
                            let (width, height) = decoder.dimensions();
                            let mut img = RgbaImage::new(width, height);
                            decoder.read_image(&mut img).expect("Error reading image");
                            img
                        }};
                    }
                    let img = match encoding {
                        ImageEncoding::Png => img!(PngDecoder),
                        ImageEncoding::Bmp => img!(BmpDecoder),
                        ImageEncoding::Qoi => {
                            let mut buf = Vec::new();
                            img.read_to_end(&mut buf).expect("Error reading image");
                            let (header, pixels) =
                                qoi::decode_to_vec(buf).expect("Error decoding image");
                            assert!(header.channels == qoi::Channels::Rgba, "Image is not RGBA");
                            ImageBuffer::from_vec(header.width, header.height, pixels)
                                .expect("Error decoding image")
                        }
                    };
                    if let Some(ImgDim { width, height }) = args.dimensions {
                        assert!(
                            width as u32 == img.width() && height as u32 == img.height(),
                            "Image dimensions do not match specified dimensions"
                        );
                    }
                    assert!(
                        img.width() <= u16::MAX as u32 && img.height() <= u16::MAX as u32,
                        "Image dimensions are too large"
                    );
                    img
                } else if let Some(ImgDim { width, height }) = args.dimensions {
                    RgbaImage::from_pixel(width as u32, height as u32, Rgba([0, 0, 0, 0]))
                } else {
                    panic!("Image file does not exist, please specify --dimensions");
                };
                (
                    ImageImpl {
                        image,
                        save: move |image| {
                            let mut f = fs::File::create(&path).expect("Error saving image");
                            match encoding {
                                ImageEncoding::Png => {
                                    let png = PngEncoder::new(f);
                                    png.write_image(
                                        &image,
                                        image.width(),
                                        image.height(),
                                        ColorType::Rgba8,
                                    )
                                    .expect("Error saving image");
                                }
                                ImageEncoding::Bmp => {
                                    // dunno why bmp requires &mut
                                    let bmp = BmpEncoder::new(&mut f);
                                    bmp.write_image(
                                        &image,
                                        image.width(),
                                        image.height(),
                                        ColorType::Rgba8,
                                    )
                                    .expect("Error saving image");
                                }
                                ImageEncoding::Qoi => {
                                    qoi::Encoder::new(
                                        image.as_raw(),
                                        image.width(),
                                        image.height(),
                                    )
                                    .expect("Error encoding image")
                                    .encode_to_stream(&mut f)
                                    .expect("Error saving image");
                                }
                            }
                        },
                    },
                    |word| {
                        let r = u4::from(((word & 0xF000) >> 0xC) as u8);
                        let g = u4::from(((word & 0x0F00) >> 0x8) as u8);
                        let b = u4::from(((word & 0x00F0) >> 0x4) as u8);
                        let a = u4::from(((word & 0x000F) >> 0x0) as u8);
                        const fn channel(c: u4) -> u8 {
                            match c {
                                u4::b_0000 => 0b_00000000,
                                u4::b_0001 => 0b_00000011,
                                u4::b_0010 => 0b_00001100,
                                u4::b_0011 => 0b_00001111,
                                u4::b_0100 => 0b_00110000,
                                u4::b_0101 => 0b_00110011,
                                u4::b_0110 => 0b_00111100,
                                u4::b_0111 => 0b_00111111,
                                u4::b_1000 => 0b_11000000,
                                u4::b_1001 => 0b_11000011,
                                u4::b_1010 => 0b_11001100,
                                u4::b_1011 => 0b_11001111,
                                u4::b_1100 => 0b_11110000,
                                u4::b_1101 => 0b_11110011,
                                u4::b_1110 => 0b_11111100,
                                u4::b_1111 => 0b_11111111,
                            }
                        }
                        Rgba([channel(r), channel(g), channel(b), channel(a)])
                    },
                    |Rgba([r, g, b, a])| {
                        #[allow(non_camel_case_types)]
                        enum u2 {
                            b_00,
                            b_01,
                            b_10,
                            b_11,
                        }

                        const fn pair(b: u8) -> u2 {
                            match b {
                                0b_00 => u2::b_00,
                                0b_01 => u2::b_01,
                                0b_10 => u2::b_10,
                                0b_11 => u2::b_11,
                                _ => unreachable!(),
                            }
                        }
                        const fn pairs(c: u8) -> [u2; 4] {
                            let c1 = (c & 0b_11000000) >> 0x6;
                            let c2 = (c & 0b_00110000) >> 0x4;
                            let c3 = (c & 0b_00001100) >> 0x2;
                            let c4 = (c & 0b_00000011) >> 0x0;
                            [pair(c1), pair(c2), pair(c3), pair(c4)]
                        }
                        const fn round(c: u2) -> u16 {
                            match c {
                                u2::b_00 => 0,
                                u2::b_01 => 0,
                                u2::b_10 => 1,
                                u2::b_11 => 1,
                            }
                        }
                        const fn channel(c: u8) -> u16 {
                            let [c1, c2, c3, c4] = pairs(c);
                            let c1 = round(c1) << 3;
                            let c2 = round(c2) << 2;
                            let c3 = round(c3) << 1;
                            let c4 = round(c4) << 0;
                            c1 | c2 | c3 | c4
                        }
                        let r = channel(r) << 0xC;
                        let g = channel(g) << 0x8;
                        let b = channel(b) << 0x4;
                        let a = channel(a) << 0x0;
                        r | g | b | a
                    },
                )
            }),
        ),
    };

    state.registers[Register::SP] = Word::from(state.memory.len());

    loop {
        // zero reg can just be a normal reg that we reset constantly
        state.registers[Register::R0] = Word::default();

        if let Some((len, instruction)) = Instruction::decode(&state.memory, pc) {
            // println!("{instruction:?}");
            match instruction.execute(&mut state, bottom_of_stack) {
                ControlFlow::Next => {
                    pc += len;
                }
                ControlFlow::Jump(addr) => {
                    pc = addr.into();
                }
                ControlFlow::Halted => {
                    println!();
                    println!("Halted at 0x{:04x}", pc);
                    break;
                }
            }
        } else {
            panic!("Non-Instruction Execution at 0x{:04x}", pc);
        }
    }
}
