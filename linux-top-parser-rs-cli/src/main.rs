use std::io::{self, IsTerminal, Write};
use std::sync::mpsc;
use std::thread;
use std::time::Duration;

use clap::Parser;
use linux_top_parser_rs::{parse_multiple_top_info_blocks, TopInfo};
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

    /// Idle time (milliseconds) with no new input after which an in-progress refresh is
    /// considered complete and flushed. `top -b` writes each refresh as a fast burst
    /// (top's own stdio flushes are milliseconds apart) followed by silence for the
    /// refresh interval, so a short idle gap reliably marks the end of a refresh and
    /// makes output appear instantly instead of one refresh late. Set to 0 to disable
    /// and flush only on explicit block separators (one refresh of latency).
    #[arg(long, default_value_t = 200)]
    idle_ms: u64,
}

fn main() {
    let cli = Cli::parse();
    let idle = if cli.idle_ms == 0 {
        None
    } else {
        Some(Duration::from_millis(cli.idle_ms))
    };

    let (tx, rx) = mpsc::channel::<Vec<u8>>();

    let reader = thread::spawn(move || {
        if let Some(idle) = idle {
            read_bursts(idle, tx);
        } else {
            read_lines_blocking(tx);
        }
    });

    let stdout = io::stdout();
    let mut out = io::BufWriter::new(stdout.lock());

    let mut emitted_any = false;

    while let Ok(bytes) = rx.recv() {
        if bytes.is_empty() {
            continue;
        }
        let text = String::from_utf8_lossy(&bytes);
        if text.trim().is_empty() {
            continue;
        }
        emitted_any |= emit_block(&text, &cli, &mut out);
    }

    // The reader thread has hit EOF; join to surface any panic, then finalize.
    let _ = reader.join();

    if !emitted_any {
        eprintln!(
            "No input received on stdin.\n\nUsage:\n  top -b | {bin}",
            bin = env!("CARGO_BIN_NAME")
        );
        std::process::exit(1);
    }
}

/// Reader used when idle flushing is enabled.
///
/// `top -b` emits each refresh as a burst of bytes (its own stdio buffer may flush in
/// several chunks milliseconds apart), then goes silent for the refresh interval. This
/// reader uses non-blocking I/O to drain every currently-available byte into a single
/// "burst", and only publishes that burst once the stream stays idle for `idle`. Each
/// published burst therefore maps to exactly one refresh, so callers can flush one block
/// per burst without waiting for the next refresh (no latency) and without risking a
/// mid-refresh split (top's intra-burst flushes are far shorter than `idle`).
#[cfg(unix)]
fn read_bursts(idle: Duration, tx: mpsc::Sender<Vec<u8>>) {
    use std::os::unix::io::AsRawFd;

    let stdin = io::stdin();
    let fd = stdin.as_raw_fd();

    // Switch the underlying stdin descriptor to non-blocking so the drain loop can read
    // until EAGAIN instead of blocking on the next refresh.
    unsafe {
        let flags = libc::fcntl(fd, libc::F_GETFL);
        if flags != -1 {
            libc::fcntl(fd, libc::F_SETFL, flags | libc::O_NONBLOCK);
        }
    }

    let idle_ms = idle.as_millis().min(i32::MAX as u128) as libc::c_int;
    let mut burst: Vec<u8> = Vec::new();
    let mut buf = [0u8; 8192];

    loop {
        // While a burst is accumulating, poll with the idle timeout to detect its end.
        // While waiting for the first bytes of the next refresh, block indefinitely.
        let timeout: libc::c_int = if burst.is_empty() { -1 } else { idle_ms };
        let mut pfd = libc::pollfd {
            fd,
            events: libc::POLLIN,
            revents: 0,
        };
        let nready = unsafe { libc::poll(&mut pfd, 1, timeout) };

        if nready == -1 {
            if io::Error::last_os_error().kind() == io::ErrorKind::Interrupted {
                continue;
            }
            break;
        }
        if nready == 0 {
            if !burst.is_empty() {
                let _ = tx.send(std::mem::take(&mut burst));
            }
            continue;
        }

        // Data (or EOF/HUP): drain everything currently available into the burst.
        loop {
            let n = unsafe { libc::read(fd, buf.as_mut_ptr() as *mut libc::c_void, buf.len()) };
            if n > 0 {
                burst.extend_from_slice(&buf[..n as usize]);
                continue;
            }
            if n == 0 {
                // EOF: publish any pending burst, then signal end-of-stream by returning.
                if !burst.is_empty() {
                    let _ = tx.send(std::mem::take(&mut burst));
                }
                return;
            }
            let err = io::Error::last_os_error();
            if err.kind() == io::ErrorKind::WouldBlock {
                break; // drained for now; keep accumulating the same burst
            }
            return; // unrecoverable read error
        }
    }
}

/// Fallback reader (non-unix, or idle disabled): block on stdin line by line and publish
/// one burst per `top` block, delimited by the next `top -` header or EOF. This is fully
/// correct but lags one refresh interval because a block can only be confirmed complete
/// once the next refresh begins.
#[cfg(not(unix))]
fn read_bursts(_idle: Duration, _tx: mpsc::Sender<Vec<u8>>) {}

fn read_lines_blocking(tx: mpsc::Sender<Vec<u8>>) {
    use std::io::BufRead;
    let stdin = io::stdin();
    let reader = io::BufReader::new(stdin.lock());

    let mut buffer = String::new();
    let mut block_started = false;
    let mut header_seen = false;

    for line in reader.lines() {
        let line = match line {
            Ok(line) => line,
            Err(_) => break,
        };

        let is_block_start = line.starts_with("top -");
        let trimmed = line.trim_start();

        // Drop blank lines outside a block so they never start the next buffer.
        if !block_started && trimmed.is_empty() {
            continue;
        }

        // A new "top -" header closes the current block.
        if is_block_start && block_started {
            let _ = tx.send(buffer.as_bytes().to_vec());
            buffer.clear();
            header_seen = false;
        }

        // A blank line right after the process table also closes the current block.
        if header_seen && trimmed.is_empty() {
            let _ = tx.send(buffer.as_bytes().to_vec());
            buffer.clear();
            block_started = false;
            header_seen = false;
            continue;
        }

        buffer.push_str(&line);
        buffer.push('\n');

        if is_block_start {
            block_started = true;
        } else if !header_seen && trimmed.starts_with("PID") {
            header_seen = true;
        }
    }

    if !buffer.trim().is_empty() {
        let _ = tx.send(buffer.as_bytes().to_vec());
    }
}

/// Parse a single accumulated block, apply the requested filters and write each parsed
/// `TopInfo` as one JSON line. Returns `true` when at least one block was emitted.
fn emit_block<W: Write>(input: &str, cli: &Cli, out: &mut W) -> bool {
    // A refresh burst may begin with separator blank lines (top emits a blank line before
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
