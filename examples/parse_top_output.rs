use linux_top_parser_rs::parse_multiple_top_info_blocks;
use std::{env, fs, process};

fn main() {
    // Usage: cargo run --example usage -- <path-to-top-output>
    // If no path is provided, defaults to "data/single_all_cpu.txt"
    let args: Vec<String> = env::args().collect();
    let path = args
        .get(1)
        .map(|s| s.as_str())
        .unwrap_or("data/single_all_cpu.txt");

    let input = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Failed to read '{}': {}", path, e);
            process::exit(1);
        }
    };

    match parse_multiple_top_info_blocks(input.as_str()) {
        Ok((_, top_info_block)) => {
            for (block_index, top_info) in top_info_block.iter().enumerate() {
                println!("--- Top Info Block #{} ---", block_index + 1);

                // Summary / Up time and load
                let summary_display = &top_info.summary_display;
                let up_time_and_load = &summary_display.up_time_and_load_average;
                println!("Time: {}", up_time_and_load.time);
                println!("Up time (s): {}", up_time_and_load.up_time_s);
                println!("Total users: {}", up_time_and_load.total_number_of_users);
                println!(
                    "Load average: 1min={} 5min={} 15min={}",
                    up_time_and_load.load_average.last_1_min,
                    up_time_and_load.load_average.last_5_min,
                    up_time_and_load.load_average.last_15_min
                );

                // Tasks
                let task_states = &summary_display.task_states;
                println!(
                    "Tasks: total={} running={} sleeping={} stopped={} zombie={}",
                    task_states.total,
                    task_states.running,
                    task_states.sleeping,
                    task_states.stopped,
                    task_states.zombie
                );

                // CPU states
                for cpu_state in &summary_display.cpu_states {
                    println!(
                        "CPU {}: user={} system={} nice={} idle={} iowait={} hi={} si={} steal={}",
                        cpu_state.id,
                        cpu_state.user,
                        cpu_state.system,
                        cpu_state.nice,
                        cpu_state.idle,
                        cpu_state.io_wait,
                        cpu_state.hw_irq,
                        cpu_state.soft_irq,
                        cpu_state.steal
                    );
                }

                // Memory
                let physical_memory = &summary_display.physical_memory;
                println!(
                    "Physical memory: total={} free={} used={} buff/cache={}",
                    physical_memory.total,
                    physical_memory.free,
                    physical_memory.used,
                    physical_memory.buff_or_cache
                );
                let virtual_memory = &summary_display.virtual_memory;
                println!(
                    "Virtual memory: total={} free={} used={} avail={}",
                    virtual_memory.total,
                    virtual_memory.free,
                    virtual_memory.used,
                    virtual_memory.available
                );

                // Process table rows (field_values)
                println!("Process table ({} rows):", top_info.field_values.len());
                for (row_index, process_row) in top_info.field_values.iter().enumerate() {
                    println!("  Process #{}:", row_index + 1);
                    for (column_key, column_value) in process_row.iter() {
                        println!("    {}: {}", column_key, column_value);
                    }
                }
            }
        }
        Err(e) => {
            eprintln!("Parse error: {:?}", e);
            process::exit(2);
        }
    }
}
