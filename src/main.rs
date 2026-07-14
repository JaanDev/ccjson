use std::path::PathBuf;

use ccjson::JsonValue;
use clap::Parser;

#[derive(Parser)]
struct Args {
    filepath: PathBuf,
}

fn main() {
    let args = Args::parse();

    let json_object = JsonValue::build_from_file(&args.filepath).unwrap();

    println!("{json_object:#?}");
}
