use super::common::{from_days, from_hours, from_minutes};
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{digit0, digit1, line_ending, space0, space1},
    combinator::{map, map_res, opt},
    multi::{many0, many1},
    number::complete::float,
    sequence::{delimited, preceded, separated_pair, terminated, tuple},
    IResult,
};
use serde::{Deserialize, Serialize};

fn parse_field<'a>(key: &'static str) -> impl FnMut(&'a str) -> IResult<&'a str, f32> {
    terminated(
        delimited(space0, float, space0),
        terminated(tag(key), opt(tag(","))),
    )
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct TaskStates {
    total: u32,
    running: u32,
    sleeping: u32,
    stopped: u32,
    zombie: u32,
}

fn parse_task_states(input: &str) -> IResult<&str, TaskStates> {
    let (input, task_states) = tuple((
        tag("Tasks:"),
        parse_field("total"),
        parse_field("running"),
        parse_field("sleeping"),
        parse_field("stopped"),
        parse_field("zombie"),
    ))(input)?;

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
#[serde(rename_all = "camelCase")]
struct CpuStates {
    id: i32,
    user: f32,
    system: f32,
    nice: f32,
    idle: f32,
    io_wait: f32,
    hw_irq: f32,
    soft_irq: f32,
    steal: f32,
}

fn parse_cpu_id(input: &str) -> IResult<&str, i32> {
    alt((
        map(tag("%Cpu(s):"), |_| -1),
        map_res(
            preceded(tag("%Cpu"), terminated(digit0, tuple((space0, tag(":"))))),
            str::parse::<i32>,
        ),
    ))(input)
}

fn parse_cpu_states(input: &str) -> IResult<&str, CpuStates> {
    let (input, cpus) = tuple((
        parse_cpu_id,
        parse_field("us"),
        parse_field("sy"),
        parse_field("ni"),
        parse_field("id"),
        parse_field("wa"),
        parse_field("hi"),
        parse_field("si"),
        parse_field("st"),
    ))(input)?;

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
struct PhysicalMemory {
    total: f32,
    free: f32,
    used: f32,
    buff_or_cache: f32,
}

fn parse_physical_memory(input: &str) -> IResult<&str, PhysicalMemory> {
    let (input, physical_memory) = tuple((
        tag("MiB Mem :"),
        parse_field("total"),
        parse_field("free"),
        parse_field("used"),
        parse_field("buff/cache"),
    ))(input)?;

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
struct VirtualMemory {
    total: f32,
    free: f32,
    used: f32,
    available: f32,
}

fn parse_virtual_memory(input: &str) -> IResult<&str, VirtualMemory> {
    let (input, virtual_memory) = tuple((
        tag("MiB Swap:"),
        parse_field("total"),
        parse_field("free"),
        parse_field("used."),
        parse_field("avail Mem"),
    ))(input)?;

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
struct SummaryDisplay {
    up_time_and_load_average: UpTimeAndLoadAverage,
    task_states: TaskStates,
    cpu_states: Vec<CpuStates>,
    physical_memory: PhysicalMemory,
    virtual_memory: VirtualMemory,
}

fn parse_summary_display(input: &str) -> IResult<&str, SummaryDisplay> {
    map(
        tuple((
            terminated(parse_up_time_and_load_average, line_ending),
            terminated(parse_task_states, line_ending),
            many1(terminated(parse_cpu_states, line_ending)),
            terminated(parse_physical_memory, line_ending),
            terminated(parse_virtual_memory, line_ending),
        )),
        |output| SummaryDisplay {
            up_time_and_load_average: output.0,
            task_states: output.1,
            cpu_states: output.2,
            physical_memory: output.3,
            virtual_memory: output.4,
        },
    )(input)
}

#[derive(Debug)]
enum UpTimeDuration {
    Minutes(u32),
    HoursMinutes(u32, u32),
    Days(u32),
    DaysMinutes(u32, u32),
    DaysHoursMinutes(u32, u32, u32),
}

impl UpTimeDuration {
    fn to_seconds(&self) -> u32 {
        match *self {
            UpTimeDuration::Minutes(mins) => from_minutes(mins),
            UpTimeDuration::Days(days) => from_days(days),
            UpTimeDuration::HoursMinutes(hours, mins) => from_hours(hours) + from_minutes(mins),
            UpTimeDuration::DaysMinutes(days, mins) => from_days(days) + from_minutes(mins),
            UpTimeDuration::DaysHoursMinutes(days, hours, mins) => {
                from_days(days) + from_hours(hours) + from_minutes(mins)
            }
        }
    }
}

fn parse_up_time_mins(input: &str) -> IResult<&str, u32> {
    delimited(space0, map_res(digit0, str::parse::<u32>), tag(" min,"))(input)
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
    )(input)
}

fn parse_up_time_days(input: &str) -> IResult<&str, u32> {
    delimited(
        space0,
        map_res(digit0, str::parse::<u32>),
        alt((tag(" day,"), tag(" days,"))),
    )(input)
}

fn parse_up_time(input: &str) -> IResult<&str, u32> {
    let (input, _) = preceded(space0, tag("up"))(input)?;

    map(
        alt((
            map(parse_up_time_mins, |mins| UpTimeDuration::Minutes(mins)),
            map(parse_up_time_hours_mins, |(hours, mins)| {
                UpTimeDuration::HoursMinutes(hours, mins)
            }),
            map(
                tuple((parse_up_time_days, parse_up_time_hours_mins)),
                |(days, (hours, mins))| UpTimeDuration::DaysHoursMinutes(days, hours, mins),
            ),
            map(
                tuple((parse_up_time_days, parse_up_time_mins)),
                |(days, mins)| UpTimeDuration::DaysMinutes(days, mins),
            ),
        )),
        |up_time_duration: UpTimeDuration| up_time_duration.to_seconds(),
    )(input)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct LoadAverage {
    last_1_min: f32,
    last_5_min: f32,
    last_15_min: f32,
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
    let (input, _) = preceded(space0, tag("load average:"))(input)?;

    map(
        many1(preceded(space0, terminated(float, opt(tag(","))))),
        LoadAverage::from_vec,
    )(input)
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
struct UpTimeAndLoadAverage {
    time: String,
    #[serde(rename = "upTime_s")]
    up_time_s: u32,
    total_number_of_users: u32,
    load_average: LoadAverage,
}

fn parse_up_time_and_load_average(input: &str) -> IResult<&str, UpTimeAndLoadAverage> {
    let (input, _) = tag("top -")(input)?;

    map(
        tuple((
            parse_hh_mm_ss,
            parse_up_time,
            alt((parse_field("users"), parse_field("user"))),
            parse_load_average,
        )),
        |output| UpTimeAndLoadAverage {
            time: output.0.join(":"),
            up_time_s: output.1,
            total_number_of_users: output.2 as u32,
            load_average: output.3,
        },
    )(input)
}

fn parse_hh_mm_ss(input: &str) -> IResult<&str, Vec<&str>> {
    preceded(space0, many1(terminated(digit1, opt(tag(":")))))(input)
}
//  PID USER      PR  NI    VIRT    RES    SHR S  %CPU  %MEM     TIME+ COMMAND                                                                                                                                                                                                                                                                                                                                                                                                                                                  P
// 8253 tim       20   0   23.8g 235884  37740 S   6.7   2.9   0:03.07 /home/tim/.nvm/versions/node/v18.12.0/bin/node --experimental-loader=file:///home/tim/.vscode-server/extensions/wallabyjs.wallaby-vscode-1.0.349/wallaby65f4bb/runners/node/hooks.mjs /home/tim/.vscode-server/extensions/wallabyjs.wallaby-vscode-1.0.349/wallaby65f4bb/server.js runner 0 40475 vitest@0.14.0,autoDetected  /home/tim/learn/linux-top-parser/node_modules /home/tim/.vscode-server/extensions/wallabyjs.wallaby-vscode-1.0.349/proje+  2

// fn parse_header(input: &str) -> IResult<&str, Vec<&str>>{

//     // many1(
//     //     map(
//     //         pair(
//     //             tag(" "),
//     //             terminated(
//     //                 map(
//     //                     pair(
//     //                         space0,
//     //                         alpha1,
//     //                     ),
//     //                     |(a, b)| format!("{a}{b}")
//     //                 ),
//     //             peek(pair(tag(" "), alpha1))
//     //             )
//     //         ),
//     //         |(a, b)| format!("{a}{b}")
//     //     )
//     // )(input)

//     // many0(alt((
//     //     preceded(pair(opt(space1), recognize(take_while1(|c| c != ' '))), space1),
//     //     terminated(opt(space1), is_not(" \t\r\n")),
//     // )))(input)
//     recognize(many0(
//         alt((
//             multi_word_parser,
//             leading_whitespace_word_parser
//         ))
//     ))(input)

// }

fn word_boundary_parser(input: &str) -> IResult<&str, &str> {
    delimited(
        opt(preceded(take_while1(|c: char| !c.is_whitespace()), space1)),
        take_while1(|c: char| !c.is_whitespace()),
        opt(space1),
    )(input)
}

fn multi_word_parser(input: &str) -> IResult<&str, Vec<&str>> {
    many0(word_boundary_parser)(input)
}

fn leading_whitespace_word_parser(input: &str) -> IResult<&str, &str> {
    preceded(space1, take_while1(|c: char| !c.is_whitespace()))(input)
}

#[test]
fn it_can_parse_header() {
    let input =
        " USER      PR  NI    VIRT    RES    SHR S  %CPU  %MEM     TIME+ COMMAND              P";
    let expected = vec![
        "  PID",
        " USER     ",
        " PR",
        "  NI",
        "    VIRT",
        "    RES",
        "    SHR",
        " S ",
        " %CPU",
        "  %MEM",
        "     TIME+",
        " COMMAND             ",
        " P",
    ];

    // let expected = parse_header(input).unwrap();
    // println!("{:#?}", expected);
    // assert_eq!(expected, "");
}

#[cfg(test)]
mod tests {
    use pretty_assertions::assert_eq;
    use rstest::rstest;

    use super::*;

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
        let (_, actual) = parse_field(key)(input).unwrap();
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
    #[ignore]
    fn it_can_parse_summary_display(
        #[values("single_all_cpu", "single_split_cpu")] file_name: &str,
    ) {
        let folder_path = std::path::Path::new(env!("CARGO_MANIFEST_DIR"))
            .join("test")
            .join("data");

        let input_file_path = folder_path.join(format!("{}.txt", file_name));
        let expected_file_path = folder_path.join(format!("{}_expected.json", file_name));

        let input = std::fs::read_to_string(input_file_path).unwrap();
        let expected = std::fs::read_to_string(expected_file_path).unwrap();

        let expected = serde_json::from_str::<SummaryDisplay>(expected.as_str()).unwrap();
        let actual = parse_summary_display(input.as_str()).unwrap().1;

        assert_eq!(actual, expected);
    }
}
