pub mod parser;

pub use parser::using_nom::{
    parse_multiple_top_info_blocks, CpuStates, LoadAverage, PhysicalMemory, SummaryDisplay,
    TaskStates, TopInfo, UpTimeAndLoadAverage, VirtualMemory,
};
