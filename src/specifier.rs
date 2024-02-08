use crate::error::VersionError;
use chrono::{Datelike, NaiveDate};
use core::fmt::{self, Debug, Display};

pub(crate) type SpecValue = u32;
pub(crate) type SpecValueResult = Result<SpecValue, VersionError>;
type NextDateFn = fn(&NaiveDate) -> SpecValueResult;

fn full_year_next(date: &NaiveDate) -> SpecValueResult {
    // Note: Spec doesn't comment about years that are not 4-digit, so allow them
    let year = date.year();
    if year <= 0 {
        // negatives would require a sign to round trip, so disallow
        Err(VersionError::NegativeYearValue { year })
    } else {
        Ok(year as SpecValue)
    }
}

fn short_year_next(date: &NaiveDate) -> SpecValueResult {
    let year = date.year();
    // while `year % 100` might seem like the right call, the spec allows this to be >=100 so that,
    // for example, `2001`, `2101`, `3001` are disambiguated as 1, 101, and 1001, respectively.
    let diff = year - 2000;
    if diff < 0 {
        // negatives would require a sign to round trip, so disallow
        Err(VersionError::NegativeYearValue { year })
    } else {
        Ok(diff as SpecValue)
    }
}

fn weeks_from_sunday_next(date: &NaiveDate) -> SpecValueResult {
    let days_from_sunday = date.weekday().num_days_from_sunday();

    // This formula taken from a internal (`pub(crate)`) API inside chrono::NaiveDate. I'm unsure
    // why it's not true `pub`. hopefully safe.
    Ok((6 + date.ordinal() - days_from_sunday) / 7)
}

fn month_next(date: &NaiveDate) -> SpecValueResult {
    Ok(date.month())
}

fn day_next(date: &NaiveDate) -> SpecValueResult {
    Ok(date.day())
}

pub(crate) trait Specifier: PartialEq + Eq + Debug + Display + Sized + 'static {
    fn format_pattern(&self) -> &'static [u8];

    fn zero_pad_len(&self) -> Option<usize>;

    fn first_variants() -> &'static [&'static Self];

    fn last_variants() -> &'static [&'static Self];

    fn can_be_first(&self) -> bool {
        Self::first_variants().contains(&self)
    }

    fn can_be_last(&self) -> bool {
        Self::last_variants().contains(&self)
    }

    fn format_value(&self, value: &SpecValue) -> String {
        match self.zero_pad_len() {
            Some(len) => format!("{:0len$}", value, len = len),
            None => SpecValue::to_string(value),
        }
    }

    fn all() -> &'static [&'static Self];

    fn parse_width(&self) -> ParseWidth;

    fn can_be_zero(&self) -> bool;

    fn can_be_left_adjacent_to(&self, other: &Self) -> bool;
}

#[non_exhaustive]
pub(crate) enum ParseWidth {
    AtLeastOne,
    AtLeastTwo,
    OneOrTwo,
    Two,
}

const MAJOR_FORMAT_PATTERN: &[u8] = b"<MAJOR>";

const MINOR_FORMAT_PATTERN: &[u8] = b"<MINOR>";

const PATCH_FORMAT_PATTERN: &[u8] = b"<PATCH>";

const SEM_ZERO_PAD_LEN: Option<usize> = None;
const SEM_PARSE_WIDTH: ParseWidth = ParseWidth::AtLeastOne;
const SEM_CAN_BE_ZERO: bool = true;

const YEAR_FULL_FORMAT_STRINGS: &[u8] = b"<YYYY>";
const YEAR_FULL_ZERO_PAD_LEN: Option<usize> = None;
const YEAR_FULL_NEXT_FN: NextDateFn = full_year_next;
const YEAR_FULL_CAN_BE_ZERO: bool = false;
const YEAR_FULL_PARSE_WIDTH: ParseWidth = ParseWidth::AtLeastOne;

const YEAR_SHORT_FORMAT_STRINGS: &[u8] = b"<YY>";
const YEAR_SHORT_ZERO_PAD_LEN: Option<usize> = None;
const YEAR_SHORT_CAN_BE_ZERO: bool = true;
const YEAR_SHORT_PARSE_WIDTH: ParseWidth = ParseWidth::AtLeastOne;

const YEAR_ZERO_PADDED_FORMAT_STRINGS: &[u8] = b"<0Y>";
const YEAR_ZERO_PADDED_ZERO_PAD_LEN: Option<usize> = Some(2);
const YEAR_ZERO_PADDED_CAN_BE_ZERO: bool = true;
const YEAR_ZERO_PADDED_PARSE_WIDTH: ParseWidth = ParseWidth::AtLeastTwo;

const YEAR_SHORT_AND_ZERO_PADDED_NEXT_FN: NextDateFn = short_year_next;

const MONTH_SHORT_FORMAT_STRINGS: &[u8] = b"<MM>";
const MONTH_SHORT_ZERO_PAD_LEN: Option<usize> = None;

const MONTH_ZERO_PADDED_FORMAT_STRINGS: &[u8] = b"<0M>";
const MONTH_ZERO_PADDED_ZERO_PAD_LEN: Option<usize> = Some(2);

const MONTH_CAN_BE_ZERO: bool = false;
const MONTH_NEXT_FN: NextDateFn = month_next;

const WEEK_SHORT_FORMAT_STRINGS: &[u8] = b"<WW>";
const WEEK_SHORT_ZERO_PAD_LEN: Option<usize> = None;

const WEEK_ZERO_PADDED_FORMAT_STRINGS: &[u8] = b"<0W>";
const WEEK_ZERO_PADDED_ZERO_PAD_LEN: Option<usize> = Some(2);

const WEEK_CAN_BE_ZERO: bool = true;
const WEEK_NEXT_FN: NextDateFn = weeks_from_sunday_next;

const DAY_SHORT_FORMAT_STRINGS: &[u8] = b"<DD>";
const DAY_SHORT_ZERO_PAD_LEN: Option<usize> = None;

const DAY_ZERO_PADDED_FORMAT_STRINGS: &[u8] = b"<0D>";
const DAY_ZERO_PADDED_ZERO_PAD_LEN: Option<usize> = Some(2);

const MONTH_WEEK_DAY_ZERO_PADDED_PARSE_WIDTH: ParseWidth = ParseWidth::Two;
const MONTH_WEEK_DAY_SHORT_PARSE_WIDTH: ParseWidth = ParseWidth::OneOrTwo;
const DAY_NEXT_FN: NextDateFn = day_next;
const DAY_CAN_BE_ZERO: bool = false;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[non_exhaustive]
pub(crate) enum YearType {
    Full,
    Short,
    ZeroPadded,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
#[non_exhaustive]
pub(crate) enum NonYearType {
    Short,
    ZeroPadded,
}

pub(crate) trait Level {
    type Specifier: Specifier;

    fn spec(&self) -> &Self::Specifier;
}

/// A semantic version specifier, such as `<MAJOR>` or `<MINOR>`.
#[derive(Debug, PartialEq, Eq, Clone)]
#[non_exhaustive]
pub(crate) enum SemSpecifier {
    /// A major version specifier, such as `<MAJOR>`.
    Major,
    /// A minor version specifier, such as `<MINOR>`.
    Minor,
    /// A patch version specifier, such as `<PATCH>`.
    Patch,
}

impl SemSpecifier {
    pub(crate) fn next_value(&self, old_value: &SpecValue, level: &SemLevel) -> SpecValue {
        match (self, level) {
            (_, _) if level.spec() == self => *old_value + 1,
            (_, SemLevel::Major) | (SemSpecifier::Patch, SemLevel::Minor) => 0,
            _ => *old_value,
        }
    }
}

impl Display for SemSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(unsafe { std::str::from_utf8_unchecked(self.format_pattern()) })
    }
}

impl Specifier for SemSpecifier {
    fn format_pattern(&self) -> &'static [u8] {
        use SemSpecifier::*;
        match self {
            Major => MAJOR_FORMAT_PATTERN,
            Minor => MINOR_FORMAT_PATTERN,
            Patch => PATCH_FORMAT_PATTERN,
        }
    }

    fn zero_pad_len(&self) -> Option<usize> {
        SEM_ZERO_PAD_LEN
    }

    fn first_variants() -> &'static [&'static SemSpecifier] {
        static FIRST: &[&SemSpecifier] = &[&SEM_MAJOR];
        FIRST
    }

    fn last_variants() -> &'static [&'static SemSpecifier] {
        SEM_ALL
    }

    fn all() -> &'static [&'static Self] {
        SEM_ALL
    }

    fn parse_width(&self) -> ParseWidth {
        SEM_PARSE_WIDTH
    }

    fn can_be_zero(&self) -> bool {
        SEM_CAN_BE_ZERO
    }

    fn can_be_left_adjacent_to(&self, other: &Self) -> bool {
        use SemSpecifier::*;
        matches!((self, other), (Major, Minor) | (Minor, Patch))
    }
}
pub(crate) const SEM_MAJOR: SemSpecifier = SemSpecifier::Major;
pub(crate) const SEM_MINOR: SemSpecifier = SemSpecifier::Minor;
pub(crate) const SEM_PATCH: SemSpecifier = SemSpecifier::Patch;
const SEM_ALL: &[&SemSpecifier] = &[&SEM_MAJOR, &SEM_MINOR, &SEM_PATCH];

/// A semantic specifier to increment in a [Sem](crate::Sem) format
#[derive(Debug, PartialEq, Eq, Clone)]
#[non_exhaustive]
pub enum SemLevel {
    /// Increment the major specifier and reset the minor and patch specifiers.
    Major,
    /// Increment the minor specifier and reset the patch specifier.
    Minor,
    /// Increment the patch specifier.
    Patch,
}

impl Level for SemLevel {
    type Specifier = SemSpecifier;

    fn spec(&self) -> &Self::Specifier {
        match self {
            Self::Major => &SEM_MAJOR,
            Self::Minor => &SEM_MINOR,
            Self::Patch => &SEM_PATCH,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[non_exhaustive]
pub(crate) enum CalSpecifier {
    Year(YearType),
    Month(NonYearType),
    Week(NonYearType),
    Day(NonYearType),
}

impl CalSpecifier {
    pub(crate) fn next_value(&self, date: &NaiveDate) -> SpecValueResult {
        match &self {
            CalSpecifier::Year(type_) => match type_ {
                YearType::Full => YEAR_FULL_NEXT_FN(date),
                YearType::Short => YEAR_SHORT_AND_ZERO_PADDED_NEXT_FN(date),
                YearType::ZeroPadded => YEAR_SHORT_AND_ZERO_PADDED_NEXT_FN(date),
            },
            CalSpecifier::Month(_) => MONTH_NEXT_FN(date),
            CalSpecifier::Week(_) => WEEK_NEXT_FN(date),
            CalSpecifier::Day(_) => DAY_NEXT_FN(date),
        }
    }
}

impl Display for CalSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(unsafe { std::str::from_utf8_unchecked(self.format_pattern()) })
    }
}

impl Specifier for CalSpecifier {
    fn format_pattern(&self) -> &'static [u8] {
        use CalSpecifier::*;
        match self {
            Year(type_) => match type_ {
                YearType::Full => YEAR_FULL_FORMAT_STRINGS,
                YearType::Short => YEAR_SHORT_FORMAT_STRINGS,
                YearType::ZeroPadded => YEAR_ZERO_PADDED_FORMAT_STRINGS,
            },
            Month(type_) => match type_ {
                NonYearType::Short => MONTH_SHORT_FORMAT_STRINGS,
                NonYearType::ZeroPadded => MONTH_ZERO_PADDED_FORMAT_STRINGS,
            },
            Week(type_) => match type_ {
                NonYearType::Short => WEEK_SHORT_FORMAT_STRINGS,
                NonYearType::ZeroPadded => WEEK_ZERO_PADDED_FORMAT_STRINGS,
            },
            Day(type_) => match type_ {
                NonYearType::Short => DAY_SHORT_FORMAT_STRINGS,
                NonYearType::ZeroPadded => DAY_ZERO_PADDED_FORMAT_STRINGS,
            },
        }
    }

    fn zero_pad_len(&self) -> Option<usize> {
        use CalSpecifier::*;
        match self {
            Year(type_) => match type_ {
                YearType::Full => YEAR_FULL_ZERO_PAD_LEN,
                YearType::Short => YEAR_SHORT_ZERO_PAD_LEN,
                YearType::ZeroPadded => YEAR_ZERO_PADDED_ZERO_PAD_LEN,
            },
            Month(type_) => match type_ {
                NonYearType::Short => MONTH_SHORT_ZERO_PAD_LEN,
                NonYearType::ZeroPadded => MONTH_ZERO_PADDED_ZERO_PAD_LEN,
            },
            Week(type_) => match type_ {
                NonYearType::Short => WEEK_SHORT_ZERO_PAD_LEN,
                NonYearType::ZeroPadded => WEEK_ZERO_PADDED_ZERO_PAD_LEN,
            },
            Day(type_) => match type_ {
                NonYearType::Short => DAY_SHORT_ZERO_PAD_LEN,
                NonYearType::ZeroPadded => DAY_ZERO_PADDED_ZERO_PAD_LEN,
            },
        }
    }

    fn first_variants() -> &'static [&'static Self] {
        static FIRST: &[&CalSpecifier] = &[&CAL_YEAR_FULL, &CAL_YEAR_SHORT, &CAL_YEAR_ZERO_PADDED];
        FIRST
    }

    fn last_variants() -> &'static [&'static Self] {
        CAL_ALL
    }

    fn all() -> &'static [&'static Self] {
        CAL_ALL
    }

    fn parse_width(&self) -> ParseWidth {
        match self {
            CalSpecifier::Year(YearType::Full) => YEAR_FULL_PARSE_WIDTH,
            CalSpecifier::Year(YearType::Short) => YEAR_SHORT_PARSE_WIDTH,
            CalSpecifier::Year(YearType::ZeroPadded) => YEAR_ZERO_PADDED_PARSE_WIDTH,
            CalSpecifier::Month(NonYearType::ZeroPadded)
            | CalSpecifier::Week(NonYearType::ZeroPadded)
            | CalSpecifier::Day(NonYearType::ZeroPadded) => MONTH_WEEK_DAY_ZERO_PADDED_PARSE_WIDTH,
            CalSpecifier::Month(NonYearType::Short)
            | CalSpecifier::Week(NonYearType::Short)
            | CalSpecifier::Day(NonYearType::Short) => MONTH_WEEK_DAY_SHORT_PARSE_WIDTH,
        }
    }

    fn can_be_zero(&self) -> bool {
        use CalSpecifier::*;
        match self {
            Year(type_) => match type_ {
                YearType::Full => YEAR_FULL_CAN_BE_ZERO,
                YearType::Short => YEAR_SHORT_CAN_BE_ZERO,
                YearType::ZeroPadded => YEAR_ZERO_PADDED_CAN_BE_ZERO,
            },
            Month(_) => MONTH_CAN_BE_ZERO,
            Week(_) => WEEK_CAN_BE_ZERO,
            Day(_) => DAY_CAN_BE_ZERO,
        }
    }

    fn can_be_left_adjacent_to(&self, other: &Self) -> bool {
        use CalSpecifier::*;
        matches!(
            (self, other),
            (Year(_), Month(_)) | (Year(_), Week(_)) | (Month(_), Day(_))
        )
    }
}
pub(crate) const CAL_YEAR_FULL: CalSpecifier = CalSpecifier::Year(YearType::Full);
pub(crate) const CAL_YEAR_SHORT: CalSpecifier = CalSpecifier::Year(YearType::Short);
pub(crate) const CAL_YEAR_ZERO_PADDED: CalSpecifier = CalSpecifier::Year(YearType::ZeroPadded);
pub(crate) const CAL_MONTH_SHORT: CalSpecifier = CalSpecifier::Month(NonYearType::Short);
pub(crate) const CAL_MONTH_ZERO_PADDED: CalSpecifier = CalSpecifier::Month(NonYearType::ZeroPadded);
pub(crate) const CAL_WEEK_SHORT: CalSpecifier = CalSpecifier::Week(NonYearType::Short);
pub(crate) const CAL_WEEK_ZERO_PADDED: CalSpecifier = CalSpecifier::Week(NonYearType::ZeroPadded);
pub(crate) const CAL_DAY_SHORT: CalSpecifier = CalSpecifier::Day(NonYearType::Short);
pub(crate) const CAL_DAY_ZERO_PADDED: CalSpecifier = CalSpecifier::Day(NonYearType::ZeroPadded);
const CAL_ALL: &[&CalSpecifier] = &[
    &CAL_YEAR_FULL,
    &CAL_YEAR_SHORT,
    &CAL_YEAR_ZERO_PADDED,
    &CAL_MONTH_SHORT,
    &CAL_MONTH_ZERO_PADDED,
    &CAL_WEEK_SHORT,
    &CAL_WEEK_ZERO_PADDED,
    &CAL_DAY_SHORT,
    &CAL_DAY_ZERO_PADDED,
];

#[derive(Debug, PartialEq, Eq, Clone)]
#[non_exhaustive]
pub(crate) enum CalSemSpecifier {
    Year(YearType),
    Month(NonYearType),
    Week(NonYearType),
    Day(NonYearType),
    Minor,
    Patch,
}

impl CalSemSpecifier {
    pub(crate) fn is_cal(&self) -> bool {
        use CalSemSpecifier::*;
        matches!(self, Year(_) | Month(_) | Week(_) | Day(_))
    }

    pub(crate) fn next_value(
        &self,
        old_value: &SpecValue,
        date: &NaiveDate,
        level: &CalSemLevel,
    ) -> SpecValueResult {
        match self {
            CalSemSpecifier::Year(type_) => match type_ {
                YearType::Full => YEAR_FULL_NEXT_FN(date),
                YearType::Short => YEAR_SHORT_AND_ZERO_PADDED_NEXT_FN(date),
                YearType::ZeroPadded => YEAR_SHORT_AND_ZERO_PADDED_NEXT_FN(date),
            },
            CalSemSpecifier::Month(_) => MONTH_NEXT_FN(date),
            CalSemSpecifier::Week(_) => WEEK_NEXT_FN(date),
            CalSemSpecifier::Day(_) => DAY_NEXT_FN(date),
            CalSemSpecifier::Minor => match level {
                CalSemLevel::Minor => Ok(*old_value + 1),
                CalSemLevel::Patch => Ok(*old_value),
            },
            CalSemSpecifier::Patch => match level {
                CalSemLevel::Minor => Ok(0),
                CalSemLevel::Patch => Ok(*old_value + 1),
            },
        }
    }
}

impl Display for CalSemSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(unsafe { std::str::from_utf8_unchecked(self.format_pattern()) })
    }
}

impl Specifier for CalSemSpecifier {
    fn format_pattern(&self) -> &'static [u8] {
        use CalSemSpecifier::*;
        match self {
            Year(type_) => match type_ {
                YearType::Full => YEAR_FULL_FORMAT_STRINGS,
                YearType::Short => YEAR_SHORT_FORMAT_STRINGS,
                YearType::ZeroPadded => YEAR_ZERO_PADDED_FORMAT_STRINGS,
            },
            Month(type_) => match type_ {
                NonYearType::Short => MONTH_SHORT_FORMAT_STRINGS,
                NonYearType::ZeroPadded => MONTH_ZERO_PADDED_FORMAT_STRINGS,
            },
            Week(type_) => match type_ {
                NonYearType::Short => WEEK_SHORT_FORMAT_STRINGS,
                NonYearType::ZeroPadded => WEEK_ZERO_PADDED_FORMAT_STRINGS,
            },
            Day(type_) => match type_ {
                NonYearType::Short => DAY_SHORT_FORMAT_STRINGS,
                NonYearType::ZeroPadded => DAY_ZERO_PADDED_FORMAT_STRINGS,
            },
            Minor => MINOR_FORMAT_PATTERN,
            Patch => PATCH_FORMAT_PATTERN,
        }
    }

    fn zero_pad_len(&self) -> Option<usize> {
        use CalSemSpecifier::*;
        match self {
            Year(type_) => match type_ {
                YearType::Full => YEAR_FULL_ZERO_PAD_LEN,
                YearType::Short => YEAR_SHORT_ZERO_PAD_LEN,
                YearType::ZeroPadded => YEAR_ZERO_PADDED_ZERO_PAD_LEN,
            },
            Month(type_) => match type_ {
                NonYearType::Short => MONTH_SHORT_ZERO_PAD_LEN,
                NonYearType::ZeroPadded => MONTH_ZERO_PADDED_ZERO_PAD_LEN,
            },
            Week(type_) => match type_ {
                NonYearType::Short => WEEK_SHORT_ZERO_PAD_LEN,
                NonYearType::ZeroPadded => WEEK_ZERO_PADDED_ZERO_PAD_LEN,
            },
            Day(type_) => match type_ {
                NonYearType::Short => DAY_SHORT_ZERO_PAD_LEN,
                NonYearType::ZeroPadded => DAY_ZERO_PADDED_ZERO_PAD_LEN,
            },
            Minor | Patch => SEM_ZERO_PAD_LEN,
        }
    }

    fn can_be_first(&self) -> bool {
        matches!(self, CalSemSpecifier::Year(_))
    }

    fn can_be_last(&self) -> bool {
        use CalSemSpecifier::*;
        matches!(self, Minor | Patch)
    }

    fn first_variants() -> &'static [&'static Self] {
        static FIRST: &[&CalSemSpecifier] = &[
            &CALSEM_YEAR_FULL,
            &CALSEM_YEAR_SHORT,
            &CALSEM_YEAR_ZERO_PADDED,
        ];
        FIRST
    }

    fn last_variants() -> &'static [&'static Self] {
        static LAST: &[&CalSemSpecifier] = &[&CALSEM_PATCH];
        LAST
    }

    fn all() -> &'static [&'static Self] {
        CALSEM_ALL
    }

    fn parse_width(&self) -> ParseWidth {
        match self {
            CalSemSpecifier::Year(YearType::Full) => YEAR_FULL_PARSE_WIDTH,
            CalSemSpecifier::Year(YearType::Short) => YEAR_SHORT_PARSE_WIDTH,
            CalSemSpecifier::Year(YearType::ZeroPadded) => YEAR_ZERO_PADDED_PARSE_WIDTH,

            CalSemSpecifier::Month(NonYearType::ZeroPadded)
            | CalSemSpecifier::Week(NonYearType::ZeroPadded)
            | CalSemSpecifier::Day(NonYearType::ZeroPadded) => {
                MONTH_WEEK_DAY_ZERO_PADDED_PARSE_WIDTH
            }
            CalSemSpecifier::Month(NonYearType::Short)
            | CalSemSpecifier::Week(NonYearType::Short)
            | CalSemSpecifier::Day(NonYearType::Short) => MONTH_WEEK_DAY_SHORT_PARSE_WIDTH,
            CalSemSpecifier::Minor | CalSemSpecifier::Patch => SEM_PARSE_WIDTH,
        }
    }

    fn can_be_zero(&self) -> bool {
        use CalSemSpecifier::*;
        match self {
            Year(type_) => match type_ {
                YearType::Full => YEAR_FULL_CAN_BE_ZERO,
                YearType::Short => YEAR_SHORT_CAN_BE_ZERO,
                YearType::ZeroPadded => YEAR_ZERO_PADDED_CAN_BE_ZERO,
            },
            Month(_) => MONTH_CAN_BE_ZERO,
            Week(_) => WEEK_CAN_BE_ZERO,
            Day(_) => DAY_CAN_BE_ZERO,
            Minor | Patch => SEM_CAN_BE_ZERO,
        }
    }

    fn can_be_left_adjacent_to(&self, other: &Self) -> bool {
        use CalSemSpecifier::*;
        matches!(
            (self, other),
            (Year(_), Month(_))
                | (Year(_), Week(_))
                | (Year(_), Minor)
                | (Year(_), Patch)
                | (Month(_), Day(_))
                | (Month(_), Minor)
                | (Month(_), Patch)
                | (Week(_), Minor)
                | (Week(_), Patch)
                | (Day(_), Minor)
                | (Day(_), Patch)
                | (Minor, Patch)
        )
    }
}
pub(crate) const CALSEM_YEAR_FULL: CalSemSpecifier = CalSemSpecifier::Year(YearType::Full);
pub(crate) const CALSEM_YEAR_SHORT: CalSemSpecifier = CalSemSpecifier::Year(YearType::Short);
pub(crate) const CALSEM_YEAR_ZERO_PADDED: CalSemSpecifier =
    CalSemSpecifier::Year(YearType::ZeroPadded);
pub(crate) const CALSEM_MONTH_SHORT: CalSemSpecifier = CalSemSpecifier::Month(NonYearType::Short);
pub(crate) const CALSEM_MONTH_ZERO_PADDED: CalSemSpecifier =
    CalSemSpecifier::Month(NonYearType::ZeroPadded);
pub(crate) const CALSEM_WEEK_SHORT: CalSemSpecifier = CalSemSpecifier::Week(NonYearType::Short);
pub(crate) const CALSEM_WEEK_ZERO_PADDED: CalSemSpecifier =
    CalSemSpecifier::Week(NonYearType::ZeroPadded);
pub(crate) const CALSEM_DAY_SHORT: CalSemSpecifier = CalSemSpecifier::Day(NonYearType::Short);
pub(crate) const CALSEM_DAY_ZERO_PADDED: CalSemSpecifier =
    CalSemSpecifier::Day(NonYearType::ZeroPadded);
pub(crate) const CALSEM_MINOR: CalSemSpecifier = CalSemSpecifier::Minor;
pub(crate) const CALSEM_PATCH: CalSemSpecifier = CalSemSpecifier::Patch;
const CALSEM_ALL: &[&CalSemSpecifier] = &[
    &CALSEM_YEAR_FULL,
    &CALSEM_YEAR_SHORT,
    &CALSEM_YEAR_ZERO_PADDED,
    &CALSEM_MONTH_SHORT,
    &CALSEM_MONTH_ZERO_PADDED,
    &CALSEM_WEEK_SHORT,
    &CALSEM_WEEK_ZERO_PADDED,
    &CALSEM_DAY_SHORT,
    &CALSEM_DAY_ZERO_PADDED,
    &CALSEM_MINOR,
    &CALSEM_PATCH,
];

/// A semantic-type specifier to increment in a [CalSem](crate::CalSem) format
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CalSemLevel {
    /// A minor version specifier, such as `<MINOR>`.
    Minor,
    /// A patch version specifier, such as `<PATCH>`.
    Patch,
}

impl Level for CalSemLevel {
    type Specifier = CalSemSpecifier;

    fn spec(&self) -> &Self::Specifier {
        match self {
            Self::Minor => &CALSEM_MINOR,
            Self::Patch => &CALSEM_PATCH,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use itertools::Itertools;

    #[test]
    fn sem_ordering() {
        use SemSpecifier::*;

        assert!(!Major.can_be_left_adjacent_to(&Major));
        assert!(Major.can_be_left_adjacent_to(&Minor));
        assert!(!Major.can_be_left_adjacent_to(&Patch));

        assert!(!Minor.can_be_left_adjacent_to(&Major));
        assert!(!Minor.can_be_left_adjacent_to(&Minor));
        assert!(Minor.can_be_left_adjacent_to(&Patch));

        assert!(!Patch.can_be_left_adjacent_to(&Major));
        assert!(!Patch.can_be_left_adjacent_to(&Minor));
        assert!(!Patch.can_be_left_adjacent_to(&Patch));
    }

    #[test]
    fn cal_ordering() {
        use CalSpecifier::*;
        use NonYearType::*;
        use YearType::{Full as YFull, Short as YShort, ZeroPadded as YZeroPadded};

        let years = || [Year(YFull), Year(YShort), Year(YZeroPadded)].iter();
        let months = || [Month(Short), Month(ZeroPadded)].iter();
        let weeks = || [Week(Short), Week(ZeroPadded)].iter();
        let days = || [Day(Short), Day(ZeroPadded)].iter();

        // year and year
        years()
            .cartesian_product(years())
            .for_each(|(year1, year2)| {
                assert!(!year1.can_be_left_adjacent_to(year2));
                assert!(!year2.can_be_left_adjacent_to(year1));
            });

        // year and month
        years()
            .cartesian_product(months())
            .for_each(|(year, month)| {
                assert!(year.can_be_left_adjacent_to(month));
                assert!(!month.can_be_left_adjacent_to(year));
            });

        // year and week
        years().cartesian_product(weeks()).for_each(|(year, week)| {
            assert!(year.can_be_left_adjacent_to(week));
            assert!(!week.can_be_left_adjacent_to(year));
        });

        // year and day
        years().cartesian_product(days()).for_each(|(year, day)| {
            assert!(!year.can_be_left_adjacent_to(day));
            assert!(!day.can_be_left_adjacent_to(year));
        });

        // month and month
        months()
            .cartesian_product(months())
            .for_each(|(month1, month2)| {
                assert!(!month1.can_be_left_adjacent_to(month2));
                assert!(!month2.can_be_left_adjacent_to(month1));
            });

        // month and week
        months()
            .cartesian_product(weeks())
            .for_each(|(month, week)| {
                assert!(!month.can_be_left_adjacent_to(week));
                assert!(!week.can_be_left_adjacent_to(month));
            });

        // month and day
        months().cartesian_product(days()).for_each(|(month, day)| {
            assert!(month.can_be_left_adjacent_to(day));
            assert!(!day.can_be_left_adjacent_to(month));
        });

        // week and week
        weeks()
            .cartesian_product(weeks())
            .for_each(|(week1, week2)| {
                assert!(!week1.can_be_left_adjacent_to(week2));
                assert!(!week2.can_be_left_adjacent_to(week1));
            });

        // week and day
        weeks().cartesian_product(days()).for_each(|(week, day)| {
            assert!(!week.can_be_left_adjacent_to(day));
            assert!(!day.can_be_left_adjacent_to(week));
        });

        // day and day
        days().cartesian_product(days()).for_each(|(day1, day2)| {
            assert!(!day1.can_be_left_adjacent_to(day2));
            assert!(!day2.can_be_left_adjacent_to(day1));
        });
    }

    #[test]
    fn cal_sem_ordering() {
        use CalSemSpecifier::*;
        use NonYearType::*;
        use YearType::{Full as YFull, Short as YShort, ZeroPadded as YZeroPadded};

        let years = || [Year(YFull), Year(YShort), Year(YZeroPadded)].iter();
        let months = || [Month(Short), Month(ZeroPadded)].iter();
        let weeks = || [Week(Short), Week(ZeroPadded)].iter();
        let days = || [Day(Short), Day(ZeroPadded)].iter();
        let minors = || [Minor].iter();
        let patches = || [Patch].iter();

        // year and year
        years()
            .cartesian_product(years())
            .for_each(|(year1, year2)| {
                assert!(!year1.can_be_left_adjacent_to(year2));
                assert!(!year2.can_be_left_adjacent_to(year1));
            });

        // year and month
        years()
            .cartesian_product(months())
            .for_each(|(year, month)| {
                assert!(year.can_be_left_adjacent_to(month));
                assert!(!month.can_be_left_adjacent_to(year));
            });

        // year and week
        years().cartesian_product(weeks()).for_each(|(year, week)| {
            assert!(year.can_be_left_adjacent_to(week));
            assert!(!week.can_be_left_adjacent_to(year));
        });

        // year and day
        years().cartesian_product(days()).for_each(|(year, day)| {
            assert!(!year.can_be_left_adjacent_to(day));
            assert!(!day.can_be_left_adjacent_to(year));
        });

        // year and minor
        years()
            .cartesian_product(minors())
            .for_each(|(year, minor)| {
                assert!(year.can_be_left_adjacent_to(minor));
                assert!(!minor.can_be_left_adjacent_to(year));
            });

        // year and patch
        years()
            .cartesian_product(patches())
            .for_each(|(year, patch)| {
                assert!(year.can_be_left_adjacent_to(patch));
                assert!(!patch.can_be_left_adjacent_to(year));
            });

        // month and month
        months()
            .cartesian_product(months())
            .for_each(|(month1, month2)| {
                assert!(!month1.can_be_left_adjacent_to(month2));
                assert!(!month2.can_be_left_adjacent_to(month1));
            });

        // month and week
        months()
            .cartesian_product(weeks())
            .for_each(|(month, week)| {
                assert!(!month.can_be_left_adjacent_to(week));
                assert!(!week.can_be_left_adjacent_to(month));
            });

        // month and day
        months().cartesian_product(days()).for_each(|(month, day)| {
            assert!(month.can_be_left_adjacent_to(day));
            assert!(!day.can_be_left_adjacent_to(month));
        });

        // month and minor
        months()
            .cartesian_product(minors())
            .for_each(|(month, minor)| {
                assert!(month.can_be_left_adjacent_to(minor));
                assert!(!minor.can_be_left_adjacent_to(month));
            });

        // month and patch
        months()
            .cartesian_product(patches())
            .for_each(|(month, patch)| {
                assert!(month.can_be_left_adjacent_to(patch));
                assert!(!patch.can_be_left_adjacent_to(month));
            });

        // week and week
        weeks()
            .cartesian_product(weeks())
            .for_each(|(week1, week2)| {
                assert!(!week1.can_be_left_adjacent_to(week2));
                assert!(!week2.can_be_left_adjacent_to(week1));
            });

        // week and day
        weeks().cartesian_product(days()).for_each(|(week, day)| {
            assert!(!week.can_be_left_adjacent_to(day));
            assert!(!day.can_be_left_adjacent_to(week));
        });

        // week and minor
        weeks()
            .cartesian_product(minors())
            .for_each(|(week, minor)| {
                assert!(week.can_be_left_adjacent_to(minor));
                assert!(!minor.can_be_left_adjacent_to(week));
            });

        // week and patch
        weeks()
            .cartesian_product(patches())
            .for_each(|(week, patch)| {
                assert!(week.can_be_left_adjacent_to(patch));
                assert!(!patch.can_be_left_adjacent_to(week));
            });

        // day and day
        days().cartesian_product(days()).for_each(|(day1, day2)| {
            assert!(!day1.can_be_left_adjacent_to(day2));
            assert!(!day2.can_be_left_adjacent_to(day1));
        });

        // day and minor
        days().cartesian_product(minors()).for_each(|(day, minor)| {
            assert!(day.can_be_left_adjacent_to(minor));
            assert!(!minor.can_be_left_adjacent_to(day));
        });

        // day and patch
        days()
            .cartesian_product(patches())
            .for_each(|(day, patch)| {
                assert!(day.can_be_left_adjacent_to(patch));
                assert!(!patch.can_be_left_adjacent_to(day));
            });

        // minor and minor
        minors()
            .cartesian_product(minors())
            .for_each(|(minor1, minor2)| {
                assert!(!minor1.can_be_left_adjacent_to(minor2));
                assert!(!minor2.can_be_left_adjacent_to(minor1));
            });

        // minor and patch
        minors()
            .cartesian_product(patches())
            .for_each(|(minor, patch)| {
                assert!(minor.can_be_left_adjacent_to(patch));
                assert!(!patch.can_be_left_adjacent_to(minor));
            });

        // patch and patch
        patches()
            .cartesian_product(patches())
            .for_each(|(patch1, patch2)| {
                assert!(!patch1.can_be_left_adjacent_to(patch2));
                assert!(!patch2.can_be_left_adjacent_to(patch1));
            });
    }
}
