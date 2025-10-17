pub fn from_days(days: u32) -> u32 {
    from_hours(days * 24)
}

pub fn from_hours(hours: u32) -> u32 {
    from_minutes(hours * 60)
}

pub fn from_minutes(minutes: u32) -> u32 {
    minutes * 60
}

#[cfg(test)]
mod tests {
    use rstest::rstest;

    use super::*;

    #[rstest]
    #[case(1, 86400)]
    #[case(2, 172800)]
    fn it_can_convert_from_days(#[case] input: u32, #[case] expected: u32) {
        assert_eq!(expected, from_days(input));
    }

    #[rstest]
    #[case(1, 3600)]
    #[case(2, 7200)]
    fn it_can_convert_from_hours(#[case] input: u32, #[case] expected: u32) {
        assert_eq!(expected, from_hours(input));
    }

    #[rstest]
    #[case(1, 60)]
    #[case(2, 120)]
    fn it_can_convert_from_mins(#[case] input: u32, #[case] expected: u32) {
        assert_eq!(expected, from_minutes(input));
    }
}
