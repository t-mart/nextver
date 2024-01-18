use std::ops::Deref;

use chrono::{Datelike, NaiveDate};

use crate::error::VersionBumpError;

fn full_year(date: &NaiveDate) -> Result<u32, VersionBumpError> {
    // Note: Spec doesn't comment about years that are not 4-digit, so allow them
    let year = date.year();
    if year <= 0 {
        // spec also doesn't comment on negative years, but that would affect parsing, so disallow
        Err(VersionBumpError::NegativeYearValue { year })
    } else {
        Ok(year as u32)
    }
}

fn short_year(date: &NaiveDate) -> Result<u32, VersionBumpError> {
    let year = date.year();
    // while `year % 100` might seem like the right call, the spec allows this to be >100 so that
    // `2001`, `2101`, `3001` are unambiguous.
    let diff = year - 2000;
    if diff < 0 {
        // spec also doesn't comment on negative years, but that would affect parsing, so disallow
        Err(VersionBumpError::NegativeYearValue { year })
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

fn sem_ver_incr(cur_val: &u32, semantic_already_bumped: bool) -> u32 {
    if semantic_already_bumped {
        0
    } else {
        cur_val + 1
    }
}

fn two_digit_pad(num: &u32) -> String {
    format!("{:02}", num)
}


/// A level for semantic specifiers, like `major`, `minor`, or `patch`.
#[derive(PartialEq, Eq, Debug, PartialOrd, Ord)]
#[repr(u8)]
pub enum SemanticLevel {
    /// The major level. It is greater than the minor and patch levels.
    Major = 2,
    /// The minor level. It is less than the major level and greater than the patch level.
    Minor = 1,
    /// The patch level. It is less than the major and minor levels.
    Patch = 0,
}

impl SemanticLevel {
    pub(crate) fn name(&self) -> &'static str {
        match self {
            SemanticLevel::Major => "major",
            SemanticLevel::Minor => "minor",
            SemanticLevel::Patch => "patch",
        }
    }
}

#[derive(Debug)]
pub(crate) enum Type {
    Major,
    Minor,
    Patch,
    Year,
    Month,
    Week,
    Day,
}

// Common fields for semantic and calendar specifiers
#[derive(Debug)]
pub(crate) struct CommonSpecifier {
    /// The literal pattern used to match this specifier in a format string. Must be surrounded
    /// by square brackets.
    pub(crate) format_pattern: &'static str,

    /// The regex pattern used to match this specifier in a version string.
    pub(crate) version_pattern: &'static str,

    /// A function that formats a value for this specifier as a string
    pub(crate) value_format_fn: fn(&u32) -> String,

    /// The level of this specifier. Used to check that specifiers are in the right order.
    pub(crate) type_: Type,
}

#[derive(Debug)]
pub(crate) enum Specifier {
    /// A semantic version specifier, like `major`, `minor`, or `patch`.
    Semantic {
        common: CommonSpecifier,

        /// Increments a value for a semantic specifier. The first argument is the current value,
        /// and the second argument is whether or not a previous specifier has already been
        /// incremented. This is used to determine if the value should be reset to 0.
        increment: fn(&u32, bool) -> u32,

        // used for identifying and comparing specifiers to increment
        semantic_level: SemanticLevel,
    },

    /// A calendar specifier, like `YYYY`, `MM`, or `DD`.
    Calendar {
        common: CommonSpecifier,

        /// Updates a value for a calendar specifier. The argument is a date which will be
        /// referenced to determine new values. Returns Err if something about the date is unusable
        /// for the specifier (e.g. a year before 0 for `YYYY`).
        update: fn(&NaiveDate) -> Result<u32, VersionBumpError>,
    },
}

impl Deref for Specifier {
    type Target = CommonSpecifier;

    /// Access the common fields of a specifier by dereferencing it
    fn deref(&self) -> &Self::Target {
        match self {
            Specifier::Semantic { common, .. } => common,
            Specifier::Calendar { common, .. } => common,
        }
    }
}

impl Specifier {
    pub(crate) fn format_value(&self, value: &u32) -> String {
        (self.value_format_fn)(value)
    }

    pub(crate) fn version_pattern_group(&self) -> String {
        format!("({})", self.version_pattern)
    }
}

pub(crate) static MAJOR: Specifier = Specifier::Semantic {
    common: CommonSpecifier {
        format_pattern: "[MAJOR]",
        version_pattern: r"\d+",
        value_format_fn: u32::to_string,
        type_: Type::Major,
    },
    increment: sem_ver_incr,
    semantic_level: SemanticLevel::Major,
};

pub(crate) static MINOR: Specifier = Specifier::Semantic {
    common: CommonSpecifier {
        format_pattern: "[MINOR]",
        version_pattern: r"\d+",
        value_format_fn: u32::to_string,
        type_: Type::Minor,
    },
    increment: sem_ver_incr,
    semantic_level: SemanticLevel::Minor,
};

pub(crate) static PATCH: Specifier = Specifier::Semantic {
    common: CommonSpecifier {
        format_pattern: "[PATCH]",
        version_pattern: r"\d+",
        value_format_fn: u32::to_string,
        type_: Type::Patch,
    },
    increment: sem_ver_incr,
    semantic_level: SemanticLevel::Patch,
};

/// Full year - 2006, 2016, 2106
pub(crate) static FULL_YEAR: Specifier = Specifier::Calendar {
    common: CommonSpecifier {
        format_pattern: "[YYYY]",
        version_pattern: r"\d+",
        value_format_fn: u32::to_string,
        type_: Type::Year,
    },
    update: full_year,
};

/// Short year - 6, 16, 106
pub(crate) static SHORT_YEAR: Specifier = Specifier::Calendar {
    common: CommonSpecifier {
        format_pattern: "[YY]",
        version_pattern: r"\d+",
        value_format_fn: u32::to_string,
        type_: Type::Year,
    },
    update: short_year,
};

/// Zero-padded year - 06, 16, 106
pub(crate) static ZERO_PADDED_YEAR: Specifier = Specifier::Calendar {
    common: CommonSpecifier {
        format_pattern: "[0Y]",
        version_pattern: r"\d+",
        value_format_fn: two_digit_pad,
        type_: Type::Year,
    },
    update: short_year,
};

/// Short month - 1, 2 ... 11, 12
pub(crate) static SHORT_MONTH: Specifier = Specifier::Calendar {
    common: CommonSpecifier {
        format_pattern: "[MM]",
        version_pattern: r"\d+",
        value_format_fn: u32::to_string,
        type_: Type::Month,
    },
    update: |date| Ok(date.month()),
};

/// Zero-padded month - 01, 02 ... 11, 12
pub(crate) static ZERO_PADDED_MONTH: Specifier = Specifier::Calendar {
    common: CommonSpecifier {
        format_pattern: "[0M]",
        version_pattern: r"\d+",
        value_format_fn: two_digit_pad,
        type_: Type::Month,
    },
    update: |date| Ok(date.month()),
};

/// Short week (since start of year) - 1, 2, 33, 52
pub(crate) static SHORT_WEEK: Specifier = Specifier::Calendar {
    common: CommonSpecifier {
        format_pattern: "[WW]",
        version_pattern: r"\d+",
        value_format_fn: u32::to_string,
        type_: Type::Week,
    },
    update: |date| Ok(weeks_from_sunday(date)),
};

/// Zero-padded week - 01, 02, 33, 52
pub(crate) static ZERO_PADDED_WEEK: Specifier = Specifier::Calendar {
    common: CommonSpecifier {
        format_pattern: "[0W]",
        version_pattern: r"\d+",
        value_format_fn: two_digit_pad,
        type_: Type::Week,
    },
    update: |date| Ok(weeks_from_sunday(date)),
};

/// Short day - 1, 2 ... 30, 31
pub(crate) static SHORT_DAY: Specifier = Specifier::Calendar {
    common: CommonSpecifier {
        format_pattern: "[DD]",
        version_pattern: r"\d+",
        value_format_fn: u32::to_string,
        type_: Type::Day,
    },
    update: |date| Ok(date.day()),
};

/// Zero-padded day - 01, 02 ... 30, 31
pub(crate) static ZERO_PADDED_DAY: Specifier = Specifier::Calendar {
    common: CommonSpecifier {
        format_pattern: "[0D]",
        version_pattern: r"\d+",
        value_format_fn: two_digit_pad,
        type_: Type::Day,
    },
    update: |date| Ok(date.day()),
};

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
            (ymd(0, 1, 1), Err(VersionBumpError::NegativeYearValue { year: 0 })),
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
            (ymd(1999, 1, 1), Err(VersionBumpError::NegativeYearValue { year: 1999 })),
        ];
        for (date, expected) in args {
            let actual = short_year(&date);
            assert_eq!(expected, actual);
        }
    }
}
