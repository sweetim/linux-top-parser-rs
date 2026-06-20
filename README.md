# linux-top-parser-rs

A lightweight Rust library for parsing Linux `top` command output into structured data types and JSON.

## Features

- **Structured parsing** — Converts `top` output (single or multi-snapshot) into typed Rust structs
- **JSON serialization** — Built-in support for converting parsed data to JSON
- **Robust parsing** — Uses [`nom`](https://docs.rs/nom) parser combinator library for reliable parsing
- **Ordered process data** — Preserves process table column order using `indexmap`
- **CLI tool** — Ships `linux-top-parser-rs-cli` for piping `top` output straight into structured JSON
- **Ready-to-use examples** — Includes a working example program with sample data

## Installation

Add to your `Cargo.toml`:

```toml
[dependencies]
linux-top-parser-rs = "0.2.0"
```

## Quick Start

```rust
use linux_top_parser_rs::parse_multiple_top_info_blocks;
use std::fs;

fn main() {
    let input = fs::read_to_string("TOP_CPU_TEXT_FILE_PATH.txt")
        .expect("Failed to read file");

    let (_, top_info_blocks) = parse_multiple_top_info_blocks(&input)
        .expect("Parse failed");
}
```

## Running the Example

The repository includes a complete example program:

```bash
# Parse the default sample file
cargo run --example parse_top_output

# Parse a custom top output file
cargo run --example parse_top_output -- path/to/your/top_output.txt
```

Example implementation: [`examples/parse_top_output.rs`](examples/parse_top_output.rs)

## CLI

The workspace includes a command-line tool, `linux-top-parser-rs-cli`, that reads `top` output from stdin and prints structured JSON. It handles both single and multi-snapshot (`top -b`) output.

### Building / Installing

```bash
# Build the binary (found at target/release/linux-top-parser-rs-cli)
cargo build -p linux-top-parser-rs-cli --release

# Or install it to your cargo bin directory
cargo install --path linux-top-parser-rs-cli
```

### Usage

```bash
# Pipe live top output
top -b | linux-top-parser-rs-cli

# Or feed in a saved top output file
linux-top-parser-rs-cli < top_output.txt
```

### Options

| Option           | Description                                         | Default |
| ---------------- | --------------------------------------------------- | ------- |
| `-s, --summary`  | Output summary display only                         | `false` |
| `-p, --prettify` | Output top info with indentation and color          | `false` |
| `-f, --filter`   | Output processes that have > 0% CPU usage only      | `false` |
| `-h, --help`     | Print help                                          |         |
| `-V, --version`  | Print version                                       |         |

By default the tool prints a compact JSON array of all parsed blocks (summary + process table). Flags may be combined freely:

```bash
# Pretty, colored summary in your terminal
top -b | linux-top-parser-rs-cli -s -p

# Only processes actively using CPU
top -b | linux-top-parser-rs-cli -f

# Clean, indented JSON suitable for redirecting to a file
top -b | linux-top-parser-rs-cli -p > top.json
```

> Color is applied only when stdout is a terminal, so piping the output to a file always produces plain JSON.

CLI implementation: [`linux-top-parser-rs-cli/src/main.rs`](linux-top-parser-rs-cli/src/main.rs)

## Testing

Run the full test suite:

```bash
cargo test
```

The tests use sample `top` outputs from the repository root `data/` directory and validate against expected JSON outputs like `data/multi_expected.json` and `data/multi_all_expected.json`. When running tests from the crate, the test code resolves the workspace data directory by traversing to the parent of the crate's manifest directory.

### Running Tests with Coverage

```bash
# Install cargo-llvm-cov
cargo install cargo-llvm-cov

# Generate coverage report
cargo llvm-cov --html --open
```

## Data Structures

All types are defined in [`src/parser/using_nom.rs`](src/parser/using_nom.rs). Key structures:

### Supporting Types
- **`TaskStates`** — Counts for total, running, sleeping, stopped, and zombie processes
- **`CpuStates`** — CPU time percentages (user, system, nice, idle, iowait, hardware interrupts, software interrupts, steal)
- **`PhysicalMemory`** — Physical RAM metrics (total, free, used, buffers/cache)
- **`VirtualMemory`** — Swap space metrics
- **`LoadAverage`** & **`UpTimeAndLoadAverage`** — System load and uptime from the header line
