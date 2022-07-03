// Copyright 2022 Joshua Wong.
// SPDX-License-Identifier: Apache-2.0 OR MIT

use std::{
    fs::File,
    io::{self, BufReader, Read, Write},
    path::{Path, PathBuf},
};

use anyhow::{Ok, Result};
use clap::{arg, command};
use lox::scanner::Scanner;

fn main() -> Result<()> {
    let matches = command!()
        .arg(arg!([filename] "Lox source file to compile"))
        .get_matches();
    if let Some(file) = matches.get_one::<PathBuf>("filename") {
        parse_file(file)
    } else {
        run_repl()
    }
}

fn parse_file<P: AsRef<Path>>(path: P) -> Result<()> {
    let file = File::open(path)?;
    let mut reader = BufReader::new(file);
    let mut buffer = String::new();
    let lines_read = reader.read_to_string(&mut buffer)?;
    println!("read {lines_read} lines");
    Scanner::new(&buffer)
        .into_iter()
        .for_each(|tok| println!("token: {tok:?}"));
    Ok(())
}

fn run_repl() -> Result<()> {
    let mut input = String::new();
    let mut bytes_read: usize = 0;
    loop {
        print!("lox> ");
        io::stdout().flush().expect("unable to flush buffer");
        let num_read = io::stdin().read_line(&mut input)?;
        Scanner::new(&input[bytes_read..])
            .into_iter()
            .for_each(|tok| println!("token: {tok:?}"));
        bytes_read += num_read;
    }
}
