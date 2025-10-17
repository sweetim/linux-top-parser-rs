use indexmap::IndexMap;

use super::common::{from_days, from_hours, from_minutes};
use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{
        crlf, digit0, digit1, line_ending, none_of, not_line_ending, space0, space1,
    },
    combinator::{map, map_res, opt, peek},
    error::Error,
    multi::{many0, many1, many_till, separated_list1},
    number::complete::float,
    sequence::{delimited, preceded, separated_pair, terminated},
    IResult, Parser,
};
use serde::{Deserialize, Serialize};

fn parse_field<'a>(
    key: &'static str,
) -> impl Parser<&'a str, Output = f32, Error = Error<&'a str>> {
    terminated(
        delimited(space0, float, space0),
        terminated(tag(key), opt(tag(","))),
    )
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TaskStates {
    pub total: u32,
    pub running: u32,
    pub sleeping: u32,
    pub stopped: u32,
    pub zombie: u32,
}

fn parse_task_states(input: &str) -> IResult<&str, TaskStates> {
    let (input, task_states) = (
        tag("Tasks:"),
        parse_field("total"),
        parse_field("running"),
        parse_field("sleeping"),
        parse_field("stopped"),
        parse_field("zombie"),
    )
        .parse(input)?;

    Ok((
        input,
        TaskStates {
            total: task_states.1 as u32,
            running: task_states.2 as u32,
            sleeping: task_states.3 as u32,
            stopped: task_states.4 as u32,
            zombie: task_states.5 as u32,
        },
    ))
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct CpuStates {
    #[serde(rename = "cpu")]
    pub id: i32,
    #[serde(rename = "us")]
    pub user: f32,
    #[serde(rename = "sy")]
    pub system: f32,
    #[serde(rename = "ni")]
    pub nice: f32,
    #[serde(rename = "id")]
    pub idle: f32,
    #[serde(rename = "wa")]
    pub io_wait: f32,
    #[serde(rename = "hi")]
    pub hw_irq: f32,
    #[serde(rename = "si")]
    pub soft_irq: f32,
    #[serde(rename = "st")]
    pub steal: f32,
}

fn parse_cpu_id(input: &str) -> IResult<&str, i32> {
    alt((
        map(tag("%Cpu(s):"), |_| -1),
        map_res(
            preceded(tag("%Cpu"), terminated(digit0, (space0, tag(":")))),
            str::parse::<i32>,
        ),
    ))
    .parse(input)
}

fn parse_cpu_states(input: &str) -> IResult<&str, CpuStates> {
    let (input, cpus) = (
        parse_cpu_id,
        parse_field("us"),
        parse_field("sy"),
        parse_field("ni"),
        parse_field("id"),
        parse_field("wa"),
        parse_field("hi"),
        parse_field("si"),
        parse_field("st"),
    )
        .parse(input)?;

    Ok((
        input,
        CpuStates {
            id: cpus.0,
            user: cpus.1,
            system: cpus.2,
            nice: cpus.3,
            idle: cpus.4,
            io_wait: cpus.5,
            hw_irq: cpus.6,
            soft_irq: cpus.7,
            steal: cpus.8,
        },
    ))
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct PhysicalMemory {
    pub total: f32,
    pub free: f32,
    pub used: f32,
    pub buff_or_cache: f32,
}

fn parse_physical_memory(input: &str) -> IResult<&str, PhysicalMemory> {
    let (input, physical_memory) = (
        tag("MiB Mem :"),
        parse_field("total"),
        parse_field("free"),
        parse_field("used"),
        parse_field("buff/cache"),
    )
        .parse(input)?;

    Ok((
        input,
        PhysicalMemory {
            total: physical_memory.1,
            free: physical_memory.2,
            used: physical_memory.3,
            buff_or_cache: physical_memory.4,
        },
    ))
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct VirtualMemory {
    pub total: f32,
    pub free: f32,
    pub used: f32,
    pub available: f32,
}

fn parse_virtual_memory(input: &str) -> IResult<&str, VirtualMemory> {
    let (input, virtual_memory) = (
        tag("MiB Swap:"),
        parse_field("total"),
        parse_field("free"),
        parse_field("used."),
        parse_field("avail Mem"),
    )
        .parse(input)?;

    Ok((
        input,
        VirtualMemory {
            total: virtual_memory.1,
            free: virtual_memory.2,
            used: virtual_memory.3,
            available: virtual_memory.4,
        },
    ))
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TopInfo {
    pub summary_display: SummaryDisplay,
    pub field_values: Vec<IndexMap<String, String>>,
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SummaryDisplay {
    pub up_time_and_load_average: UpTimeAndLoadAverage,
    pub task_states: TaskStates,
    pub cpu_states: Vec<CpuStates>,
    pub physical_memory: PhysicalMemory,
    pub virtual_memory: VirtualMemory,
}

fn parse_summary_display(input: &str) -> IResult<&str, SummaryDisplay> {
    map(
        (
            terminated(parse_up_time_and_load_average, line_ending),
            terminated(parse_task_states, line_ending),
            many1(terminated(parse_cpu_states, line_ending)),
            terminated(parse_physical_memory, line_ending),
            terminated(parse_virtual_memory, line_ending),
        ),
        |output| SummaryDisplay {
            up_time_and_load_average: output.0,
            task_states: output.1,
            cpu_states: output.2,
            physical_memory: output.3,
            virtual_memory: output.4,
        },
    )
    .parse(input)
}

#[derive(Debug)]
enum UpTimeDuration {
    Minutes(u32),
    HoursMinutes(u32, u32),
    DaysMinutes(u32, u32),
    DaysHoursMinutes(u32, u32, u32),
}

impl UpTimeDuration {
    fn to_seconds(&self) -> u32 {
        match *self {
            UpTimeDuration::Minutes(mins) => from_minutes(mins),
            UpTimeDuration::HoursMinutes(hours, mins) => from_hours(hours) + from_minutes(mins),
            UpTimeDuration::DaysMinutes(days, mins) => from_days(days) + from_minutes(mins),
            UpTimeDuration::DaysHoursMinutes(days, hours, mins) => {
                from_days(days) + from_hours(hours) + from_minutes(mins)
            }
        }
    }
}

fn parse_up_time_mins(input: &str) -> IResult<&str, u32> {
    delimited(space0, map_res(digit0, str::parse::<u32>), tag(" min,")).parse(input)
}

fn parse_up_time_hours_mins(input: &str) -> IResult<&str, (u32, u32)> {
    delimited(
        space0,
        separated_pair(
            map_res(digit0, str::parse::<u32>),
            tag(":"),
            map_res(digit0, str::parse::<u32>),
        ),
        tag(","),
    )
    .parse(input)
}

fn parse_up_time_days(input: &str) -> IResult<&str, u32> {
    delimited(
        space0,
        map_res(digit0, str::parse::<u32>),
        alt((tag(" day,"), tag(" days,"))),
    )
    .parse(input)
}

fn parse_up_time(input: &str) -> IResult<&str, u32> {
    let (input, _) = preceded(space0, tag("up")).parse(input)?;

    map(
        alt((
            map(parse_up_time_mins, UpTimeDuration::Minutes),
            map(parse_up_time_hours_mins, |(hours, mins)| {
                UpTimeDuration::HoursMinutes(hours, mins)
            }),
            map(
                (parse_up_time_days, parse_up_time_hours_mins),
                |(days, (hours, mins))| UpTimeDuration::DaysHoursMinutes(days, hours, mins),
            ),
            map((parse_up_time_days, parse_up_time_mins), |(days, mins)| {
                UpTimeDuration::DaysMinutes(days, mins)
            }),
        )),
        |up_time_duration: UpTimeDuration| up_time_duration.to_seconds(),
    )
    .parse(input)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
pub struct LoadAverage {
    #[serde(rename = "loadAverageLast_1_min")]
    pub last_1_min: f32,
    #[serde(rename = "loadAverageLast_5_min")]
    pub last_5_min: f32,
    #[serde(rename = "loadAverageLast_15_min")]
    pub last_15_min: f32,
}

impl LoadAverage {
    fn from_vec(input: Vec<f32>) -> Self {
        let mut input = input.into_iter();

        LoadAverage {
            last_1_min: input.next().unwrap_or_default(),
            last_5_min: input.next().unwrap_or_default(),
            last_15_min: input.next().unwrap_or_default(),
        }
    }
}

fn parse_load_average(input: &str) -> IResult<&str, LoadAverage> {
    let (input, _) = preceded(space0, tag("load average:")).parse(input)?;

    map(
        many1(preceded(space0, terminated(float, opt(tag(","))))),
        LoadAverage::from_vec,
    )
    .parse(input)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UpTimeAndLoadAverage {
    pub time: String,
    #[serde(rename = "upTime_s")]
    pub up_time_s: u32,
    pub total_number_of_users: u32,
    #[serde(flatten)]
    pub load_average: LoadAverage,
}

fn parse_up_time_and_load_average(input: &str) -> IResult<&str, UpTimeAndLoadAverage> {
    let (input, _) = tag("top -")(input)?;

    map(
        (
            parse_hh_mm_ss,
            parse_up_time,
            alt((parse_field("users"), parse_field("user"))),
            parse_load_average,
        ),
        |output| UpTimeAndLoadAverage {
            time: output.0.join(":"),
            up_time_s: output.1,
            total_number_of_users: output.2 as u32,
            load_average: output.3,
        },
    )
    .parse(input)
}

fn parse_hh_mm_ss(input: &str) -> IResult<&str, Vec<&str>> {
    preceded(space0, many1(terminated(digit1, opt(tag(":"))))).parse(input)
}

fn parse_columns_header(input: &str) -> IResult<&str, Vec<String>> {
    many1(alt((
        map(
            (
                opt(tag(" ")),
                alt((
                    tag("USER"),
                    tag("RUSER"),
                    tag("SUSER"),
                    tag("GROUP"),
                    tag("TTY"),
                    tag("TIME"),
                    tag("COMMAND"),
                    tag("WCHAN"),
                    tag("Flags"),
                    tag("CGROUPS"),
                    tag("SUPGIDS"),
                    tag("SUPGRPS"),
                    tag("ENVIRON"),
                    tag("LXC"),
                    tag("CGNAME"),
                )),
                many_till(tag(" "), peek((tag(" "), none_of(" ")))),
            ),
            |a| format!("{}{}{}", a.0.unwrap_or_default(), a.1, a.2 .0.join("")),
        ),
        map((space1, many1(none_of(" \n"))), |a| {
            format!("{}{}", a.0, a.1.iter().collect::<String>())
        }),
    )))
    .parse(input)
}

fn not_empty_line(input: &str) -> IResult<&str, &str> {
    let (rest, line) = not_line_ending(input)?;

    if line.trim().is_empty() {
        Err(nom::Err::Error(Error::new(
            rest,
            nom::error::ErrorKind::Fail,
        )))
    } else {
        Ok((rest, line))
    }
}

fn parse_top_info_block(input: &str) -> IResult<&str, TopInfo> {
    map(
        (
            parse_summary_display,
            many1(line_ending),
            terminated(parse_columns_header, line_ending),
            separated_list1(alt((line_ending, crlf)), not_empty_line),
        ),
        |a| {
            // Build a body vector that only includes actual process lines (not blank lines or
            // subsequent "top - ..." summaries). This prevents out-of-bounds slicing when the
            // header spans positions that don't exist on non-process lines.
            let body: Vec<String> =
                a.3.iter()
                    .map(|s| s.to_string())
                    // .filter(|s| {
                    //     let t = s.trim();
                    //     if t.is_empty() {
                    //         return false;
                    //     }
                    //     // Keep lines that start with a digit after trimming left (PID lines).
                    //     t.chars()
                    //         .next()
                    //         .map(|c| c.is_ascii_digit())
                    //         .unwrap_or(false)
                    // })
                    .collect();

            TopInfo {
                summary_display: a.0,
                field_values: parse_field_values(&a.2, &body),
            }
        },
    )
    .parse(input)
}

pub fn parse_multiple_top_info_blocks(input: &str) -> IResult<&str, Vec<TopInfo>> {
    many1(terminated(parse_top_info_block, many0(line_ending))).parse(input)
}

#[derive(Debug, PartialEq)]
struct ColumnsHeader {
    raw: String,
    title: String,
    start: usize,
    end: usize,
}

fn parse_field_values(header: &[String], body: &[String]) -> Vec<IndexMap<String, String>> {
    let header_start_end = header
        .iter()
        .enumerate()
        .fold(Vec::new(), |mut acc, (_, token)| {
            let raw = token.to_string();
            let title = raw.trim().to_string();
            let start = acc.last().map_or(0, |prev: &ColumnsHeader| prev.end);
            let end = start + token.len();

            acc.push(ColumnsHeader {
                raw,
                title,
                start,
                end,
            });

            acc
        });

    body.iter()
        .filter(|line| !line.is_empty())
        .map(|line| {
            header_start_end
                .iter()
                .map(|h| {
                    let key = h.title.clone();
                    let value = line[h.start..h.end].trim().to_string();
                    (key, value)
                })
                .collect::<IndexMap<_, _>>()
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use rstest::rstest;

    use super::*;

    #[rstest]
    #[case::with_ending_newline_and_empty_line("top - 15:29:38 up 15:54,  0 users,  load average: 0.14, 0.07, 0.06
Tasks:  60 total,   1 running,  39 sleeping,   0 stopped,  20 zombie
%Cpu(s):  0.4 us,  0.8 sy,  0.1 ni, 98.4 id,  0.2 wa,  0.3 hi,  0.4 si,  0.0 st
MiB Mem :   7947.3 total,    408.6 free,   4257.3 used,   3281.4 buff/cache
MiB Swap:   2048.0 total,   2048.0 free,      0.0 used.   3392.8 avail Mem

  PID USER      PR  NI    VIRT    RES    SHR S  %CPU  %MEM     TIME+ COMMAND                                                                                                                                                                                                                                                                                                                                                                                                                                                  P
18253 tim       20   0   23.8g 235884  37740 S   6.7   3.9   0:03.07 /home/tim/.nvm/versions/node/v18.12.0/bin/node --experimental-loader=file:///home/tim/.vscode-server/extensions/wallabyjs.wallaby-vscode-1.0.349/wallaby65f4bb/runners/node/hooks.mjs /home/tim/.vscode-server/extensions/wallabyjs.wallaby-vscode-1.0.349/wallaby65f4bb/server.js runner 0 40475 vitest@0.14.0,autoDetected  /home/tim/learn/linux-top-parser/node_modules /home/tim/.vscode-server/extensions/wallabyjs.wallaby-vscode-1.0.349/proje+  2

")]
    #[case::with_ending_newline("top - 15:29:38 up 15:54,  0 users,  load average: 0.14, 0.07, 0.06
Tasks:  60 total,   1 running,  39 sleeping,   0 stopped,  20 zombie
%Cpu(s):  0.4 us,  0.8 sy,  0.1 ni, 98.4 id,  0.2 wa,  0.3 hi,  0.4 si,  0.0 st
MiB Mem :   7947.3 total,    408.6 free,   4257.3 used,   3281.4 buff/cache
MiB Swap:   2048.0 total,   2048.0 free,      0.0 used.   3392.8 avail Mem

  PID USER      PR  NI    VIRT    RES    SHR S  %CPU  %MEM     TIME+ COMMAND                                                                                                                                                                                                                                                                                                                                                                                                                                                  P
18253 tim       20   0   23.8g 235884  37740 S   6.7   3.9   0:03.07 /home/tim/.nvm/versions/node/v18.12.0/bin/node --experimental-loader=file:///home/tim/.vscode-server/extensions/wallabyjs.wallaby-vscode-1.0.349/wallaby65f4bb/runners/node/hooks.mjs /home/tim/.vscode-server/extensions/wallabyjs.wallaby-vscode-1.0.349/wallaby65f4bb/server.js runner 0 40475 vitest@0.14.0,autoDetected  /home/tim/learn/linux-top-parser/node_modules /home/tim/.vscode-server/extensions/wallabyjs.wallaby-vscode-1.0.349/proje+  2
")]
    #[case::without_ending_newline("top - 15:29:38 up 15:54,  0 users,  load average: 0.14, 0.07, 0.06
Tasks:  60 total,   1 running,  39 sleeping,   0 stopped,  20 zombie
%Cpu(s):  0.4 us,  0.8 sy,  0.1 ni, 98.4 id,  0.2 wa,  0.3 hi,  0.4 si,  0.0 st
MiB Mem :   7947.3 total,    408.6 free,   4257.3 used,   3281.4 buff/cache
MiB Swap:   2048.0 total,   2048.0 free,      0.0 used.   3392.8 avail Mem

  PID USER      PR  NI    VIRT    RES    SHR S  %CPU  %MEM     TIME+ COMMAND                                                                                                                                                                                                                                                                                                                                                                                                                                                  P
18253 tim       20   0   23.8g 235884  37740 S   6.7   3.9   0:03.07 /home/tim/.nvm/versions/node/v18.12.0/bin/node --experimental-loader=file:///home/tim/.vscode-server/extensions/wallabyjs.wallaby-vscode-1.0.349/wallaby65f4bb/runners/node/hooks.mjs /home/tim/.vscode-server/extensions/wallabyjs.wallaby-vscode-1.0.349/wallaby65f4bb/server.js runner 0 40475 vitest@0.14.0,autoDetected  /home/tim/learn/linux-top-parser/node_modules /home/tim/.vscode-server/extensions/wallabyjs.wallaby-vscode-1.0.349/proje+  2")]
    fn it_can_parse_top_info_block(#[case] input: &str) {
        let actual = parse_top_info_block(input).unwrap().1;

        let expected = TopInfo {
            summary_display: SummaryDisplay {
                up_time_and_load_average: UpTimeAndLoadAverage {
                    time: String::from("15:29:38"),
                    up_time_s: 57240,
                    total_number_of_users: 0,
                    load_average: LoadAverage {
                        last_1_min: 0.14,
                        last_5_min: 0.07,
                        last_15_min: 0.06,
                    },
                },
                task_states: TaskStates {
                    total: 60,
                    running: 1,
                    sleeping: 39,
                    stopped: 0,
                    zombie: 20,
                },
                cpu_states: vec![CpuStates {
                    id: -1,
                    user: 0.4,
                    system: 0.8,
                    nice: 0.1,
                    idle: 98.4,
                    io_wait: 0.2,
                    hw_irq: 0.3,
                    soft_irq: 0.4,
                    steal: 0.0,
                }],
                physical_memory: PhysicalMemory {
                    total: 7947.3,
                    free: 408.6,
                    used: 4257.3,
                    buff_or_cache: 3281.4,
                },
                virtual_memory: VirtualMemory {
                    total: 2048.0,
                    free: 2048.0,
                    used: 0.0,
                    available: 3392.8,
                },
            },
            field_values: vec![{
                vec![
                ("PID", "18253"),
                ("USER", "tim"),
                ("PR", "20"),
                ("NI", "0"),
                ("VIRT", "23.8g"),
                ("RES", "235884"),
                ("SHR", "37740"),
                ("S", "S"),
                ("%CPU", "6.7"),
                ("%MEM", "3.9"),
                ("TIME+", "0:03.07"),
                ("COMMAND", "/home/tim/.nvm/versions/node/v18.12.0/bin/node --experimental-loader=file:///home/tim/.vscode-server/extensions/wallabyjs.wallaby-vscode-1.0.349/wallaby65f4bb/runners/node/hooks.mjs /home/tim/.vscode-server/extensions/wallabyjs.wallaby-vscode-1.0.349/wallaby65f4bb/server.js runner 0 40475 vitest@0.14.0,autoDetected  /home/tim/learn/linux-top-parser/node_modules /home/tim/.vscode-server/extensions/wallabyjs.wallaby-vscode-1.0.349/proje+"),
                ("P", "2"),
                ].into_iter().map(|(k, v)| (k.to_string(), v.to_string())).collect()
            }],
        };

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case::start_with_pad_left(
        "  PID USER      PR  NI    VIRT    RES    SHR S  %CPU  %MEM     TIME+ COMMAND              P ",
        vec![
        "  PID",
        " USER     ",
        " PR",
        "  NI",
        "    VIRT",
        "    RES",
        "    SHR",
        " S",
        "  %CPU",
        "  %MEM",
        "     TIME+",
        " COMMAND             ",
        " P",
        ]
    )]
    #[case::start_with_pad_right(
        "USER      PR  NI    VIRT    RES    SHR S  %CPU  %MEM     TIME+ COMMAND                                 P ",
        vec![
        "USER     ",
        " PR",
        "  NI",
        "    VIRT",
        "    RES",
        "    SHR",
        " S",
        "  %CPU",
        "  %MEM",
        "     TIME+",
        " COMMAND                                ",
        " P"
        ]
    )]
    #[case::with_new_line(
        "USER      PR  NI
",
        vec![
        "USER     ",
        " PR",
        "  NI",
        ]
    )]
    fn it_can_parse_header(#[case] input: &str, #[case] expected: Vec<&str>) {
        assert_eq!(expected, parse_columns_header(input).unwrap().1);
    }

    #[rstest]
    #[case::without_space("%Cpu0:", 0)]
    #[case::with_space("%Cpu12  :", 12)]
    #[case::all_cpus("%Cpu(s):", -1)]
    fn it_can_parse_cpu_count(#[case] input: &str, #[case] expected: i32) {
        let actual = parse_cpu_id(input).unwrap().1;

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case("top - 14:48:52 up 2 days, 13:23,  0 user,  load average: 0.06, 0.02, 0.00", ("14:48:52", 220980, 0, 0.06, 0.02, 0.0))]
    #[case::with_plural_users("top - 23:09:37 up 21 min,  0 users,  load average: 0.11, 0.10, 0.18", ("23:09:37", 1260, 0, 0.11, 0.1, 0.18))]
    fn it_can_parse_up_time_and_load_average(
        #[case] input: &str,
        #[case] actual: (&str, u32, u32, f32, f32, f32),
    ) {
        let (time, up_time_s, total_number_of_users, last_1_min, last_5_min, last_15_min) = actual;
        let expected = UpTimeAndLoadAverage {
            time: time.to_string(),
            up_time_s,
            total_number_of_users,
            load_average: LoadAverage {
                last_1_min,
                last_5_min,
                last_15_min,
            },
        };

        let actual = parse_up_time_and_load_average(input).unwrap().1;

        assert_eq!(actual, expected);
    }

    #[rstest]
    fn it_can_parse_hh_mm_ss(
        #[values("12:23:32", "    12:23:32", "12:23:32    ", "    12:23:32 ")] input: &str,
    ) {
        let actual = parse_hh_mm_ss(input).unwrap().1;
        let actual = actual.join(":");
        assert_eq!(actual, "12:23:32");
    }

    #[rstest]
    #[case("load average: 0.11, 0.10, 0.18", vec![0.11, 0.1, 0.18])]
    #[case::with_preceded_comma("    load average: 0.11, 0.10, 0.18", vec![0.11, 0.1, 0.18])]
    #[case::with_comma_terminated("load average: 0.11, 0.10, 0.18,", vec![0.11, 0.1, 0.18])]
    #[case("load average: 0.063, 0.02, 0.00", vec![0.063, 0.02, 0.0])]
    fn it_can_parse_load_average(#[case] input: &str, #[case] expected: Vec<f32>) {
        let actual = parse_load_average(input).unwrap().1;

        assert_eq!(actual, LoadAverage::from_vec(expected));
    }

    #[rstest]
    #[case("up 21 min,", 1260)]
    #[case::with_preceded_comma(" up 21 min,", 1260)]
    #[case("up 15:54,", 57240)]
    #[case("up 1 day, 23:52,", 172320)]
    #[case("up 30 days, 5 min,", 2592300)]
    #[case("up 2 days, 13:23,", 220980)]
    fn it_can_parse_up_time(#[case] input: &str, #[case] expected: u32) {
        let actual = parse_up_time(input).unwrap().1;

        assert_eq!(actual, expected);
    }

    #[rstest]
    #[case("  55 total,", "total", 55)]
    #[case("  55 total", "total", 55)]
    #[case("55 total,", "total", 55)]
    #[case("55total", "total", 55)]
    #[case("   1 running,", "running", 1)]
    #[case("   0 stopped,", "stopped", 0)]
    fn it_can_parse_field(#[case] input: &str, #[case] key: &'static str, #[case] expected: u32) {
        let (_, actual) = parse_field(key).parse(input).unwrap();
        assert_eq!(actual as u32, expected);
    }

    #[rstest]
    #[case("Tasks:  55 total,   1 running,  53 sleeping,   0 stopped,   1 zombie", (55, 1, 53, 0, 1))]
    #[case("Tasks: 51 total,0 running,53 sleeping,0 stopped,1012 zombie",          (51, 0, 53, 0, 1012))]
    fn it_can_parse_task_states(#[case] input: &str, #[case] expected: (u32, u32, u32, u32, u32)) {
        let (_, actual) = parse_task_states(input).unwrap();

        let (total, running, sleeping, stopped, zombie) = expected;

        assert_eq!(
            actual,
            TaskStates {
                total,
                running,
                sleeping,
                stopped,
                zombie,
            }
        );
    }

    #[rstest]
    #[case::all_cpus("%Cpu(s):  0.4 us,  0.8 sy,  0.1 ni, 98.4 id,  0.2 wa,  0.0 hi,  0.4 si,  0.3 st", -1)]
    #[case::single_cpu(
        "%Cpu6  :  0.4 us,  0.8 sy,  0.1 ni, 98.4 id,  0.2 wa,  0.0 hi,  0.4 si,  0.3 st",
        6
    )]
    fn it_can_parse_cpu_states(#[case] input: &str, #[case] cpu_count: i32) {
        let actual = parse_cpu_states(input).unwrap().1;

        assert_eq!(
            actual,
            CpuStates {
                id: cpu_count,
                user: 0.4,
                system: 0.8,
                nice: 0.1,
                idle: 98.4,
                io_wait: 0.2,
                hw_irq: 0.0,
                soft_irq: 0.4,
                steal: 0.3,
            }
        );
    }

    #[test]
    fn it_can_parse_physical_memory() {
        let input = "MiB Mem :   7947.3 total,    408.6 free,   4257.3 used,   3281.4 buff/cache";
        let actual = parse_physical_memory(input).unwrap().1;

        assert_eq!(
            actual,
            PhysicalMemory {
                total: 7947.3,
                free: 408.6,
                used: 4257.3,
                buff_or_cache: 3281.4,
            }
        )
    }

    #[test]
    fn it_can_parse_virtual_memory() {
        let input = "MiB Swap:   2048.0 total,   2048.0 free,      0.0 used.   3392.8 avail Mem";
        let actual = parse_virtual_memory(input).unwrap().1;

        assert_eq!(
            actual,
            VirtualMemory {
                total: 2048.0,
                free: 2048.0,
                used: 0.0,
                available: 3392.8,
            }
        )
    }

    #[rstest]
    fn it_can_parse_summary_display(#[values("single_all_cpu", "multi")] file_name: &str) {
        let folder_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("data");

        let input_file_path = folder_path.join(format!("{}.txt", file_name));
        let expected_file_path = folder_path.join(format!("{}_expected.json", file_name));

        let input = std::fs::read_to_string(input_file_path).unwrap();
        let expected = std::fs::read_to_string(expected_file_path).unwrap();

        let expected = serde_json::from_str::<Vec<TopInfo>>(expected.as_str()).unwrap();
        let actual = parse_multiple_top_info_blocks(input.as_str()).unwrap().1;
        println!("{:#?}", actual);
        assert_eq!(actual, expected);
    }
}
