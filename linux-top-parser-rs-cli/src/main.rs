use std::io::{self, BufRead, BufReader, IsTerminal, Write};

use clap::Parser;
use linux_top_parser_rs::{parse_multiple_top_info_blocks, parse_summary_display, TopInfo};
use serde::Serialize;

/// Parse Linux `top` command output (piped via stdin) into structured JSON.
///
/// Input is consumed as a stream so both single-shot (`top -b -n 1`) and continuous
/// (`top -b`) invocations are supported. Each completed `top` block is emitted as one
/// newline-delimited JSON object as soon as it is parsed.
#[derive(Parser, Debug)]
#[command(
    name = "linux-top-parser-rs-cli",
    version,
    propagate_version = true,
    help_template = "{about}\n\nUsage: {usage}\n\nOptions:\n{options}"
)]
struct Cli {
    /// Output summary display only.
    #[arg(short, long)]
    summary: bool,

    /// Output top info with indentation and color.
    #[arg(short, long)]
    prettify: bool,

    /// Output processes that have > 0% CPU usage only.
    #[arg(short, long)]
    filter: bool,
}

fn main() {
    let cli = Cli::parse();

    let stdin = io::stdin();
    let reader = BufReader::new(stdin.lock());

    let stdout = io::stdout();
    let mut out = io::BufWriter::new(stdout.lock());

    // Per-frame streaming state. Each `top` block is framed deterministically by
    // counting process rows against the "Tasks: N total" announced in the summary: when
    // piped, top prints exactly N rows, so a block is complete the moment the Nth row
    // arrives. No timeout/idle heuristic is needed.
    let mut buffer = String::new();
    let mut expected_rows: Option<u32> = None;
    let mut header_seen = false;
    let mut rows: u32 = 0;
    let mut summary_emitted = false;
    let mut emitted_any = false;

    for line in reader.lines() {
        let line = match line {
            Ok(line) => line,
            Err(error) => {
                eprintln!("Failed to read from stdin: {error}");
                break;
            }
        };

        let trimmed = line.trim_start();

        // A new "top -" header starts the next block. Flush whatever is buffered as a
        // fallback for blocks that never reached their expected row count (e.g. output
        // truncated by a windowed terminal, or an unparsable Tasks count).
        if line.starts_with("top -") {
            if !buffer.trim().is_empty() && !summary_emitted {
                emitted_any |= emit_block(&buffer, &cli, &mut out);
            }
            buffer.clear();
            expected_rows = None;
            header_seen = false;
            rows = 0;
            summary_emitted = false;
            buffer.push_str(&line);
            buffer.push('\n');
            continue;
        }

        // Summary-only fast path: top delivers the summary at the first refresh tick but
        // the process list ~one interval later (it needs two /proc samples for CPU%).
        // Since the summary is already complete at the "MiB Swap:" line, emit it now
        // instead of waiting for the rest of the block. Process-list lines are skipped.
        if cli.summary {
            if !summary_emitted {
                buffer.push_str(&line);
                buffer.push('\n');
                if trimmed.starts_with("MiB Swap") {
                    emitted_any |= emit_summary(&buffer, cli.prettify, &mut out);
                    summary_emitted = true;
                    buffer.clear();
                }
            }
            continue;
        }

        // Read the expected process-row count from the "Tasks:" summary line.
        if expected_rows.is_none() {
            if let Some(total) = parse_tasks_total(&line) {
                expected_rows = Some(total);
            }
        }

        buffer.push_str(&line);
        buffer.push('\n');

        // After the column header, count process rows; emit as soon as the announced
        // total is reached (the block is then complete).
        if header_seen {
            if is_process_row(trimmed) {
                rows += 1;
                if rows >= expected_rows.unwrap_or(u32::MAX) {
                    emitted_any |= emit_block(&buffer, &cli, &mut out);
                    buffer.clear();
                    expected_rows = None;
                    header_seen = false;
                    rows = 0;
                }
            }
        } else if trimmed.starts_with("PID") {
            header_seen = true;
        }
    }

    // Flush any trailing block at EOF.
    if !buffer.trim().is_empty() {
        emitted_any |= emit_block(&buffer, &cli, &mut out);
    }

    if !emitted_any {
        eprintln!(
            "No input received on stdin.\n\nUsage:\n  top -b | {bin}",
            bin = env!("CARGO_BIN_NAME")
        );
        std::process::exit(1);
    }
}

/// Extract the total task count from a `Tasks:  N total, ...` summary line.
fn parse_tasks_total(line: &str) -> Option<u32> {
    let rest = line.trim_start().strip_prefix("Tasks:")?;
    let total = rest.find("total")?;
    rest[..total].trim().parse::<u32>().ok()
}

/// A process table row begins (after leading whitespace) with the numeric PID.
fn is_process_row(trimmed: &str) -> bool {
    !trimmed.is_empty() && trimmed.starts_with(|c: char| c.is_ascii_digit())
}

/// Parse just the summary section and write it as one JSON line. Used for the summary
/// fast path, which emits as soon as the summary section is complete.
fn emit_summary<W: Write>(input: &str, prettify: bool, out: &mut W) -> bool {
    match parse_summary_display(input) {
        Ok((_, summary)) => {
            let rendered = render(&summary, prettify);
            if writeln!(out, "{rendered}").is_err() {
                return true;
            }
            let _ = out.flush();
            true
        }
        Err(error) => {
            eprintln!("Failed to parse summary: {error}");
            false
        }
    }
}

/// Parse a single accumulated block, apply the requested filters and write each parsed
/// `TopInfo` as one JSON line. Returns `true` when at least one block was emitted.
fn emit_block<W: Write>(input: &str, cli: &Cli, out: &mut W) -> bool {
    // A block may be preceded by separator blank lines (top emits a blank line before
    // each refresh after the first); strip them so parsing starts at the "top -" header.
    let input = input.trim_start_matches(['\n', '\r']);

    let (_, mut blocks) = match parse_multiple_top_info_blocks(input) {
        Ok(result) => result,
        Err(error) => {
            eprintln!("Failed to parse top output block: {error}");
            return false;
        }
    };

    if blocks.is_empty() {
        return false;
    }

    if cli.filter {
        filter_processes(&mut blocks);
    }

    for block in &blocks {
        let rendered = if cli.summary {
            render(&block.summary_display, cli.prettify)
        } else {
            render(block, cli.prettify)
        };

        if writeln!(out, "{rendered}").is_err() {
            return true;
        }
    }

    let _ = out.flush();
    true
}

fn filter_processes(blocks: &mut [TopInfo]) {
    for block in blocks {
        block.field_values.retain(|row| {
            row.get("%CPU")
                .and_then(|value| value.trim().parse::<f32>().ok())
                .is_some_and(|cpu| cpu > 0.0)
        });
    }
}

fn render<T: Serialize>(value: &T, prettify: bool) -> String {
    if prettify {
        if io::stdout().is_terminal() {
            colored_json::to_colored_json_auto(value)
                .unwrap_or_else(|_| serde_json::to_string_pretty(value).unwrap_or_default())
        } else {
            serde_json::to_string_pretty(value).unwrap_or_default()
        }
    } else {
        serde_json::to_string(value).unwrap_or_default()
    }
}
