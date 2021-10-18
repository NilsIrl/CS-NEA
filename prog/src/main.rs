use prog::{ParseSettings, Program};
use std::{fs, io::Read, path::PathBuf};
use structopt::StructOpt;

#[derive(StructOpt)]
struct Opt {
    file: Option<PathBuf>,
}

fn main() {
    let opt = Opt::from_args();
    let source = match opt.file {
        Some(filename) => fs::read_to_string(filename).expect("TODO: file doesn't exist and stuff"),
        None => {
            let mut source = String::new();
            std::io::stdin()
                .read_to_string(&mut source)
                .expect("TODO: failed to read");
            source
        }
    };
    let ast = Program::from_str(&source, &ParseSettings::default()).unwrap();
    ast.interpret();
}
