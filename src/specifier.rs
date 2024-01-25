use crate::error::NextVerError;
use chrono::{Datelike, NaiveDate};
use core::fmt::Display;
use std::{fmt::Debug, ops::Deref, sync::LazyLock};

fn full_year(date: &NaiveDate) -> Result<u32, NextVerError> {
    // Note: Spec doesn't comment about years that are not 4-digit, so allow them
    let year = date.year();
    if year <= 0 {
        // negatives would require a sign to round trip, so disallow
        Err(NextVerError::NegativeYearValue { year })
    } else {
        Ok(year as u32)
    }
}

fn short_year(date: &NaiveDate) -> Result<u32, NextVerError> {
    let year = date.year();
    // while `year % 100` might seem like the right call, the spec allows this to be >=100 so that,
    // for example, `2001`, `2101`, `3001` are disambiguated as 1, 101, and 1001, respectively.
    let diff = year - 2000;
    if diff < 0 {
        // negatives would require a sign to round trip, so disallow
        Err(NextVerError::NegativeYearValue { year })
    } else {
        Ok(diff as u32)
    }
}

fn weeks_from_sunday(date: &impl Datelike) -> u32 {
    let days_from_sunday = date.weekday().num_days_from_sunday();

    // This formula taken from a internal (`pub(crate)`) API inside chrono::NaiveDate. I'm unsure
    // why it's not true `pub`, or why it isn't implemented for all Datelikes. hopefully safe.
    (6 + date.ordinal() - days_from_sunday) / 7
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[repr(u8)]
pub enum SemanticSpecifier {
    Major = 2,
    Minor = 1,
    Patch = 0,
}

impl SemanticSpecifier {
    pub(crate) fn increment(&self, cur_val: &u32, already_bumped: bool) -> u32 {
        if already_bumped {
            0
        } else {
            cur_val + 1
        }
    }

    fn format_pattern(&self) -> &'static str {
        match self {
            SemanticSpecifier::Major => "[MAJOR]",
            SemanticSpecifier::Minor => "[MINOR]",
            SemanticSpecifier::Patch => "[PATCH]",
        }
    }

    fn version_pattern(&self) -> &'static str {
        r"\d+"
    }

    fn format_value(&self, value: &u32) -> String {
        u32::to_string(value)
    }
}

impl Display for SemanticSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format_pattern())
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct CalendarSpecifierData {
    pub(crate) format_pattern: &'static str,
    pub(crate) version_pattern: &'static str,
    pub(crate) zero_pad_len: Option<u8>,
    pub(crate) update_fn: fn(&NaiveDate) -> Result<u32, NextVerError>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum CalendarSpecifier {
    Year(CalendarSpecifierData),
    Month(CalendarSpecifierData),
    Week(CalendarSpecifierData),
    Day(CalendarSpecifierData),
}

impl Display for CalendarSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.format_pattern)
    }
}

impl Deref for CalendarSpecifier {
    type Target = CalendarSpecifierData;

    fn deref(&self) -> &Self::Target {
        match self {
            CalendarSpecifier::Year(data)
            | CalendarSpecifier::Month(data)
            | CalendarSpecifier::Week(data)
            | CalendarSpecifier::Day(data) => data,
        }
    }
}

impl CalendarSpecifier {
    pub(crate) fn update(&self, date: &NaiveDate) -> Result<u32, NextVerError> {
        (self.update_fn)(date)
    }

    fn format_value(&self, value: &u32) -> String {
        match self.zero_pad_len {
            Some(len) => format!("{:0len$}", value, len = len as usize),
            None => u32::to_string(value),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Specifier {
    Semantic(SemanticSpecifier),
    Calendar(CalendarSpecifier),
}

impl Specifier {
    pub(crate) fn version_pattern_group(&self) -> String {
        format!("({})", self.version_pattern())
    }
}

impl Display for Specifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Specifier::Semantic(spec) => write!(f, "{}", spec),
            Specifier::Calendar(spec) => write!(f, "{}", spec),
        }
    }
}

impl Specifier {
    pub fn format_pattern(&self) -> &'static str {
        match self {
            Specifier::Semantic(spec) => spec.format_pattern(),
            Specifier::Calendar(spec) => spec.format_pattern,
        }
    }

    pub fn version_pattern(&self) -> &'static str {
        match self {
            Specifier::Semantic(spec) => spec.version_pattern(),
            Specifier::Calendar(spec) => spec.version_pattern,
        }
    }

    pub fn format_value(&self, value: &u32) -> String {
        match self {
            Specifier::Semantic(spec) => spec.format_value(value),
            Specifier::Calendar(spec) => spec.format_value(value),
        }
    }

    fn parse_no_zero(&'static self, value: &str) -> Result<u32, NextVerError> {
        if value.starts_with('0') {
            Err(NextVerError::SpecifierMayNotBeZeroPadded {
                specifier: self,
                value_str: value.to_string(),
            })
        } else {
            Ok(value.parse().unwrap())
        }
    }

    pub fn parse_value_str(&'static self, value: &str) -> Result<u32, NextVerError> {
        // unwrap is safe here because we'll know its definitely digit characters from
        // previous regex matching
        match self {
            Specifier::Semantic(..) => self.parse_no_zero(value),
            Specifier::Calendar(spec) => match spec.zero_pad_len {
                Some(_) => Ok(value.parse().unwrap()),
                None => self.parse_no_zero(value),
            },
        }
    }
}

pub static MAJOR: Specifier = Specifier::Semantic(SemanticSpecifier::Major);
pub static MINOR: Specifier = Specifier::Semantic(SemanticSpecifier::Minor);
pub static PATCH: Specifier = Specifier::Semantic(SemanticSpecifier::Patch);
pub static FULL_YEAR: Specifier =
    Specifier::Calendar(CalendarSpecifier::Year(CalendarSpecifierData {
        format_pattern: "[YYYY]",
        version_pattern: r"\d+",
        // value_format_fn: u32::to_string,
        zero_pad_len: None,
        update_fn: full_year,
    }));
pub static SHORT_YEAR: Specifier =
    Specifier::Calendar(CalendarSpecifier::Year(CalendarSpecifierData {
        format_pattern: "[YY]",
        version_pattern: r"\d+",
        // value_format_fn: u32::to_string,
        zero_pad_len: None,
        update_fn: short_year,
    }));
pub static ZERO_PADDED_YEAR: Specifier =
    Specifier::Calendar(CalendarSpecifier::Year(CalendarSpecifierData {
        format_pattern: "[0Y]",
        version_pattern: r"\d+",
        // value_format_fn: u32::to_string,
        zero_pad_len: Some(2),
        update_fn: short_year,
    }));
pub static SHORT_MONTH: Specifier =
    Specifier::Calendar(CalendarSpecifier::Month(CalendarSpecifierData {
        format_pattern: "[MM]",
        version_pattern: r"\d{1,2}",
        // value_format_fn: u32::to_string,
        zero_pad_len: None,
        update_fn: |date| Ok(date.month()),
    }));
pub static ZERO_PADDED_MONTH: Specifier =
    Specifier::Calendar(CalendarSpecifier::Month(CalendarSpecifierData {
        format_pattern: "[0M]",
        version_pattern: r"\d{2}",
        // value_format_fn: u32::to_string,
        zero_pad_len: Some(2),
        update_fn: |date| Ok(date.month()),
    }));
pub static SHORT_WEEK: Specifier =
    Specifier::Calendar(CalendarSpecifier::Week(CalendarSpecifierData {
        format_pattern: "[WW]",
        version_pattern: r"\d{1,2}",
        // value_format_fn: u32::to_string,
        zero_pad_len: None,
        update_fn: |date| Ok(weeks_from_sunday(date)),
    }));
pub static ZERO_PADDED_WEEK: Specifier =
    Specifier::Calendar(CalendarSpecifier::Week(CalendarSpecifierData {
        format_pattern: "[0W]",
        version_pattern: r"\d{2}",
        // value_format_fn: u32::to_string,
        zero_pad_len: Some(2),
        update_fn: |date| Ok(weeks_from_sunday(date)),
    }));
pub static SHORT_DAY: Specifier =
    Specifier::Calendar(CalendarSpecifier::Day(CalendarSpecifierData {
        format_pattern: "[DD]",
        version_pattern: r"\d{1,2}",
        // value_format_fn: u32::to_string,
        zero_pad_len: None,
        update_fn: |date| Ok(date.day()),
    }));
pub static ZERO_PADDED_DAY: Specifier =
    Specifier::Calendar(CalendarSpecifier::Day(CalendarSpecifierData {
        format_pattern: "[0D]",
        version_pattern: r"\d{2}",
        // value_format_fn: u32::to_string,
        zero_pad_len: Some(2),
        update_fn: |date| Ok(date.day()),
    }));

pub(crate) static ALL: &[&Specifier] = &[
    &MAJOR,
    &MINOR,
    &PATCH,
    &FULL_YEAR,
    &SHORT_YEAR,
    &ZERO_PADDED_YEAR,
    &SHORT_MONTH,
    &ZERO_PADDED_MONTH,
    &SHORT_WEEK,
    &ZERO_PADDED_WEEK,
    &SHORT_DAY,
    &ZERO_PADDED_DAY,
];

/// Used in some error messages
pub(crate) static YEAR_FORMAT_STRINGS: LazyLock<String> = LazyLock::new(|| {
    [&FULL_YEAR, &SHORT_YEAR, &ZERO_PADDED_YEAR]
        .iter()
        .map(|spec| spec.format_pattern())
        .collect::<Vec<_>>()
        .join(", ")
});

#[cfg(test)]
mod tests {
    use super::*;

    fn ymd(year: i32, month: u32, day: u32) -> NaiveDate {
        NaiveDate::from_ymd_opt(year, month, day).unwrap()
    }

    #[test]
    fn test_full_year() {
        let args = [
            (ymd(1, 1, 1), Ok(1)),
            (ymd(10, 1, 1), Ok(10)),
            (ymd(100, 1, 1), Ok(100)),
            (ymd(1000, 1, 1), Ok(1000)),
            (ymd(10000, 1, 1), Ok(10000)),
            (
                ymd(0, 1, 1),
                Err(NextVerError::NegativeYearValue { year: 0 }),
            ),
        ];
        for (date, expected) in args {
            let actual = full_year(&date);
            assert_eq!(expected, actual);
        }
    }

    #[test]
    fn test_short_year() {
        let args = [
            (ymd(2000, 1, 1), Ok(0)),
            (ymd(2010, 1, 1), Ok(10)),
            (ymd(2100, 1, 1), Ok(100)),
            (ymd(3000, 1, 1), Ok(1000)),
            (ymd(12000, 1, 1), Ok(10000)),
            (
                ymd(1999, 1, 1),
                Err(NextVerError::NegativeYearValue { year: 1999 }),
            ),
        ];
        for (date, expected) in args {
            let actual = short_year(&date);
            assert_eq!(expected, actual);
        }
    }
}
