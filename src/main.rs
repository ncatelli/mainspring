extern crate scrap;
use scrap::prelude::v1::*;
use std::env;
use std::fmt;
use std::fs::File;
use std::io::prelude::*;

const EXIT_SUCCESS: i32 = 0;

const ROOT_HELP_STRING: &str = "Usage: mos6502_emulator [OPTIONS]
a MOS6502 emulator.

Flags:
    --help, -h          print help string
    --version, -v       

Subcommands:
    run                 execute a rom in an emulated environment";

const RUN_HELP_STRING: &str = "Usage: run [OPTIONS]
execute a rom in an emulated environment

Flags:
    --help, -h          print help string
    --load, -l          a file path for a rom to load";

type RuntimeResult<T> = Result<T, RuntimeError>;

enum RuntimeError {
    InvalidArguments(String),
    FileUnreadable,
    Undefined(String),
}

impl fmt::Debug for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidArguments(hs) => write!(f, "{}", hs),
            Self::FileUnreadable => write!(f, "source file unreadable"),
            Self::Undefined(s) => write!(f, "{}", s),
        }
    }
}

impl fmt::Display for RuntimeError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", &self)
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();

    let res = Cmd::new()
        .name("mos6502_emulator")
        .description("a MOS6502 emulator.")
        .author("Nate Catelli <ncatelli@packetfire.org>")
        .version("0.1.0")
        .flag(
            Flag::new()
                .name("version")
                .short_code("v")
                .action(Action::StoreTrue)
                .value_type(ValueType::Bool),
        )
        .handler(Box::new(|_| {
            println!("{}", ROOT_HELP_STRING);
            Ok(0)
        }))
        .subcommand(
            Cmd::new()
                .name("run")
                .description("execute a rom in an emulated environment")
                .flag(
                    Flag::new()
                        .name("load")
                        .short_code("l")
                        .help_string("a file path for a rom to load")
                        .value_type(ValueType::Str),
                )
                .handler(Box::new(|c| {
                    {
                        let load_flag = c.get("load");
                        match load_flag {
                            Some(Value::Str(rom_path)) => Ok(rom_path),
                            _ => Err(RuntimeError::InvalidArguments(RUN_HELP_STRING.to_string())),
                        }
                        .map(|rom_path| read_rom_file(&rom_path).map(|_bin_data| EXIT_SUCCESS))
                    }
                    .and_then(std::convert::identity)
                    .map_err(|e| format!("{}", e))
                })),
        )
        .run(args)
        .unwrap()
        .dispatch();

    match res {
        Ok(_) => (),
        Err(e) => {
            println!("{}", e);
            std::process::exit(1)
        }
    }
}

fn read_rom_file(filename: &str) -> RuntimeResult<Vec<u8>> {
    let mut f = File::open(filename).map_err(|_| RuntimeError::FileUnreadable)?;
    let mut buffer = Vec::new();

    match f.read(&mut buffer) {
        Ok(_) => Ok(buffer),
        Err(e) => Err(RuntimeError::Undefined(e.to_string())),
    }
}
