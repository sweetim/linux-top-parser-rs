# linux-top-parser-rs

A lightweight Rust library for parsing Linux `top` command output into structured data types and JSON.

## Features

- **Structured parsing** — Converts `top` output (single or multi-snapshot) into typed Rust structs
- **JSON serialization** — Built-in support for converting parsed data to JSON
- **Robust parsing** — Uses [`nom`](https://docs.rs/nom) parser combinator library for reliable parsing
- **Ordered process data** — Preserves process table column order using `indexmap`
- **Ready-to-use examples** — Includes a working example program with sample data

## Installation

Add to your `Cargo.toml`:

```toml
[dependencies]
linux-top-parser-rs = "0.1.0"
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

## Testing

Run the full test suite:

```bash
cargo test
```

The tests use sample `top` outputs from the `data/` directory and validate against expected JSON outputs like `data/multi_expected.json` and `data/multi_all_expected.json`.

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
