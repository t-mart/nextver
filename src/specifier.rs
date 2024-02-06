use crate::error::VersionError;
use chrono::{Datelike, NaiveDate};
use core::{
    cmp::Ordering,
    fmt::{self, Debug, Display},
};

type Next = u32;
type NextResult = Result<u32, VersionError>;
type NextDateFn = fn(&NaiveDate) -> NextResult;

fn sem_next(cur_val: &u32, already_bumped: bool) -> Next {
    if already_bumped {
        0
    } else {
        cur_val + 1
    }
}

fn full_year_next(date: &NaiveDate) -> NextResult {
    // Note: Spec doesn't comment about years that are not 4-digit, so allow them
    let year = date.year();
    if year <= 0 {
        // negatives would require a sign to round trip, so disallow
        Err(VersionError::NegativeYearValue { year })
    } else {
        Ok(year as u32)
    }
}

fn short_year_next(date: &NaiveDate) -> NextResult {
    let year = date.year();
    // while `year % 100` might seem like the right call, the spec allows this to be >=100 so that,
    // for example, `2001`, `2101`, `3001` are disambiguated as 1, 101, and 1001, respectively.
    let diff = year - 2000;
    if diff < 0 {
        // negatives would require a sign to round trip, so disallow
        Err(VersionError::NegativeYearValue { year })
    } else {
        Ok(diff as u32)
    }
}

fn weeks_from_sunday_next(date: &NaiveDate) -> NextResult {
    let days_from_sunday = date.weekday().num_days_from_sunday();

    // This formula taken from a internal (`pub(crate)`) API inside chrono::NaiveDate. I'm unsure
    // why it's not true `pub`. hopefully safe.
    Ok((6 + date.ordinal() - days_from_sunday) / 7)
}

fn month_next(date: &NaiveDate) -> Result<u32, VersionError> {
    Ok(date.month())
}

fn day_next(date: &NaiveDate) -> Result<u32, VersionError> {
    Ok(date.day())
}

pub(crate) trait Specifier:
    PartialEq + Eq + PartialOrd + Debug + Display + Sized + 'static
{
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

    fn format_value(&self, value: &u32) -> String {
        match self.zero_pad_len() {
            Some(len) => format!("{:0len$}", value, len = len),
            None => u32::to_string(value),
        }
    }

    fn iter_all() -> impl Iterator<Item = &'static &'static Self>;

    fn parse_width(&self) -> ParseWidth;

    fn can_be_zero(&self) -> bool;
}

#[non_exhaustive]
pub(crate) enum ParseWidth {
    AtLeastOne,
    AtLeastTwo,
    OneOrTwo,
    Two,
}

const MAJOR_FORMAT_PATTERN: &[u8] = b"[MAJOR]";

const MINOR_FORMAT_PATTERN: &[u8] = b"[MINOR]";

const PATCH_FORMAT_PATTERN: &[u8] = b"[PATCH]";

const SEM_ZERO_PAD_LEN: Option<usize> = None;
const SEM_PARSE_WIDTH: ParseWidth = ParseWidth::AtLeastOne;
const SEM_CAN_BE_ZERO: bool = true;

const YEAR_FULL_FORMAT_STRINGS: &[u8] = b"[YYYY]";
const YEAR_FULL_ZERO_PAD_LEN: Option<usize> = None;
const YEAR_FULL_NEXT_FN: NextDateFn = full_year_next;
const YEAR_FULL_CAN_BE_ZERO: bool = false;
const YEAR_FULL_PARSE_WIDTH: ParseWidth = ParseWidth::AtLeastOne;

const YEAR_SHORT_FORMAT_STRINGS: &[u8] = b"[YY]";
const YEAR_SHORT_ZERO_PAD_LEN: Option<usize> = None;
const YEAR_SHORT_CAN_BE_ZERO: bool = true;
const YEAR_SHORT_PARSE_WIDTH: ParseWidth = ParseWidth::AtLeastOne;

const YEAR_ZERO_PADDED_FORMAT_STRINGS: &[u8] = b"[0Y]";
const YEAR_ZERO_PADDED_ZERO_PAD_LEN: Option<usize> = Some(2);
const YEAR_ZERO_PADDED_CAN_BE_ZERO: bool = true;
const YEAR_ZERO_PADDED_PARSE_WIDTH: ParseWidth = ParseWidth::AtLeastTwo;

const YEAR_SHORT_AND_ZERO_PADDED_NEXT_FN: NextDateFn = short_year_next;

const MONTH_SHORT_FORMAT_STRINGS: &[u8] = b"[MM]";
const MONTH_SHORT_ZERO_PAD_LEN: Option<usize> = None;

const MONTH_ZERO_PADDED_FORMAT_STRINGS: &[u8] = b"[0M]";
const MONTH_ZERO_PADDED_ZERO_PAD_LEN: Option<usize> = Some(2);

const MONTH_CAN_BE_ZERO: bool = false;
const MONTH_NEXT_FN: NextDateFn = month_next;

const WEEK_SHORT_FORMAT_STRINGS: &[u8] = b"[WW]";
const WEEK_SHORT_ZERO_PAD_LEN: Option<usize> = None;

const WEEK_ZERO_PADDED_FORMAT_STRINGS: &[u8] = b"[0W]";
const WEEK_ZERO_PADDED_ZERO_PAD_LEN: Option<usize> = Some(2);

const WEEK_CAN_BE_ZERO: bool = true;
const WEEK_NEXT_FN: NextDateFn = weeks_from_sunday_next;

const DAY_SHORT_FORMAT_STRINGS: &[u8] = b"[DD]";
const DAY_SHORT_ZERO_PAD_LEN: Option<usize> = None;

const DAY_ZERO_PADDED_FORMAT_STRINGS: &[u8] = b"[0D]";
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

pub(crate) trait NextArgument {
    type Specifier: Specifier;

    fn as_static_spec_ref(&self) -> &'static Self::Specifier;

    fn should_update(&self, other: &Self::Specifier) -> bool;
}

/// A semantic version specifier, such as `[MAJOR]` or `[MINOR]`.
#[derive(Debug, PartialEq, Eq, Clone)]
#[non_exhaustive]
pub enum SemSpecifier {
    /// A major version specifier, such as `[MAJOR]`.
    Major,
    /// A minor version specifier, such as `[MINOR]`.
    Minor,
    /// A patch version specifier, such as `[PATCH]`.
    Patch,
}

impl SemSpecifier {
    pub(crate) fn next(&self, cur_val: &u32, already_bumped: bool) -> u32 {
        sem_next(cur_val, already_bumped)
    }
}

impl NextArgument for SemSpecifier {
    type Specifier = Self;
    fn as_static_spec_ref(&self) -> &'static Self::Specifier {
        match self {
            Self::Major => &SEM_MAJOR,
            Self::Minor => &SEM_MINOR,
            Self::Patch => &SEM_PATCH,
        }
    }

    fn should_update(&self, other: &Self::Specifier) -> bool {
        matches!(
            (self, other),
            (Self::Major, _)
                | (Self::Minor, Self::Specifier::Minor)
                | (Self::Minor, Self::Specifier::Patch)
                | (Self::Patch, Self::Specifier::Patch)
        )
    }
}

// TODO: this feels sorta like an abuse of partial ord. we should probably just put a trait method
// on Specifier called "can_be_right_of" or something. i think that would actually be simpler to
// implement because it's only one-way
impl PartialOrd for SemSpecifier {
    /// Compare two adjacent semantic specifiers.
    ///
    /// This is only a *partial* ordering. We use this to determine if two specifiers can be
    /// adjacent. For specifiers `a` and `b` in a format string, `a` can only be to the immediate
    /// left of `b` if `a.partial_cmp(&b) == Some(Greater)`. In other words, *not* if they are:
    ///
    /// - `Some(Equal)`: this would be the same specifier twice
    /// - `Some(Less)`: this would mean `a` is less significant than `b`, which is not allowed.
    ///   Specifiers must "step down" in significance.
    /// - `None`: This means that `a` is greater than `b`, but by more than one step.
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use Ordering::*;
        use SemSpecifier::*;
        match (self, other) {
            (Major, Major) => Some(Equal),
            (Major, Minor) => Some(Greater),
            (Major, Patch) => None, // don't skip minor

            (Minor, Major) => Some(Less),
            (Minor, Minor) => Some(Equal),
            (Minor, Patch) => Some(Greater),

            (Patch, Major) => None, // dont skip minor
            (Patch, Minor) => Some(Less),
            (Patch, Patch) => Some(Equal),
        }
    }
}

impl Display for SemSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", unsafe {
            std::str::from_utf8_unchecked(self.format_pattern())
        })
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

    fn iter_all() -> impl Iterator<Item = &'static &'static Self> {
        SEM_ALL.iter()
    }

    fn parse_width(&self) -> ParseWidth {
        SEM_PARSE_WIDTH
    }

    fn can_be_zero(&self) -> bool {
        SEM_CAN_BE_ZERO
    }
}
pub(crate) const SEM_MAJOR: SemSpecifier = SemSpecifier::Major;
pub(crate) const SEM_MINOR: SemSpecifier = SemSpecifier::Minor;
pub(crate) const SEM_PATCH: SemSpecifier = SemSpecifier::Patch;
const SEM_ALL: &[&SemSpecifier] = &[&SEM_MAJOR, &SEM_MINOR, &SEM_PATCH];

#[derive(Debug, PartialEq, Eq, Clone)]
#[repr(u8)]
#[non_exhaustive]
pub(crate) enum CalSpecifier {
    Year(YearType) = 3,
    Month(NonYearType) = 2,
    Week(NonYearType) = 1,
    Day(NonYearType) = 0,
}

impl CalSpecifier {
    pub(crate) fn next(&self, date: &NaiveDate) -> Result<u32, VersionError> {
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

impl PartialOrd for CalSpecifier {
    /// Compare two adjacent calendar specifiers.
    ///
    /// This is only a *partial* ordering. We use this to determine if two specifiers can be
    /// adjacent. For specifiers `a` and `b` in a format string, `a` can only be to the immediate
    /// left of `b` if `a.partial_cmp(&b) == Some(Greater)`. In other words, *not* if they are:
    ///
    /// - `Some(Equal)`: this would be the same specifier twice
    /// - `Some(Less)`: this would mean `a` is less significant than `b`, which is not allowed.
    ///   Specifiers must "step down" in significance.
    /// - `None`: This means that `a` is greater than `b`, but by more than one step.
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use CalSpecifier::*;
        use Ordering::*;
        match (self, other) {
            (Year { .. }, Year { .. }) => Some(Equal),
            (Year { .. }, Day { .. }) => None,
            (Year { .. }, _) => Some(Greater),
            (Day { .. }, Year { .. }) => None,
            (_, Year { .. }) => Some(Less),

            (Month { .. }, Month { .. }) => Some(Equal),
            (Month { .. }, Week { .. }) => None,
            (Month { .. }, _) => Some(Greater),
            (Week { .. }, Month { .. }) => None,
            (_, Month { .. }) => Some(Less),

            (Week { .. }, Week { .. }) => Some(Equal),
            (Week { .. }, Day { .. }) => None,
            // (Week { .. }, _) => Some(Greater),
            (Day { .. }, Week { .. }) => None,
            // (_, Week { .. }) => Some(Less),
            (Day { .. }, Day { .. }) => Some(Equal),
            // (Day { .. }, _) => Some(Greater),
            // (_, Day { .. }) => Some(Less),
        }
    }
}

impl Display for CalSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", unsafe {
            std::str::from_utf8_unchecked(self.format_pattern())
        })
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

    fn iter_all() -> impl Iterator<Item = &'static &'static Self> {
        CAL_ALL.iter()
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

/// An "incrementable" specifier in a [CalSem](crate::CalSem) format: `[MINOR]` or `[PATCH]`.
///
/// Conversely, the other specifiers in a [CalSem](crate::CalSem) format are "calendar" specifiers,
/// which get "updated".
#[derive(Debug, PartialEq, Eq, Clone)]
pub enum CalSemIncrSpecifier {
    /// A minor version specifier, such as `[MINOR]`.
    Minor,
    /// A patch version specifier, such as `[PATCH]`.
    Patch,
}

impl NextArgument for CalSemIncrSpecifier {
    type Specifier = CalSemSpecifier;
    fn as_static_spec_ref(&self) -> &'static Self::Specifier {
        match self {
            Self::Minor => &CALSEM_MINOR,
            Self::Patch => &CALSEM_PATCH,
        }
    }

    fn should_update(&self, other: &Self::Specifier) -> bool {
        matches!(
            (self, other),
            (Self::Minor, Self::Specifier::Minor)
                | (Self::Minor, Self::Specifier::Patch)
                | (Self::Patch, Self::Specifier::Patch)
        )
    }
}

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

    pub(crate) fn next(
        &self,
        date: &NaiveDate,
        cur_val: &u32,
        already_bumped: bool,
    ) -> Result<u32, VersionError> {
        match &self {
            CalSemSpecifier::Year(type_) => match type_ {
                YearType::Full => YEAR_FULL_NEXT_FN(date),
                YearType::Short => YEAR_SHORT_AND_ZERO_PADDED_NEXT_FN(date),
                YearType::ZeroPadded => YEAR_SHORT_AND_ZERO_PADDED_NEXT_FN(date),
            },
            CalSemSpecifier::Month(_) => MONTH_NEXT_FN(date),
            CalSemSpecifier::Week(_) => WEEK_NEXT_FN(date),
            CalSemSpecifier::Day(_) => DAY_NEXT_FN(date),
            CalSemSpecifier::Minor | CalSemSpecifier::Patch => {
                Ok(sem_next(cur_val, already_bumped))
            }
        }
    }
}

impl Display for CalSemSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", unsafe {
            std::str::from_utf8_unchecked(self.format_pattern())
        })
    }
}

impl PartialOrd for CalSemSpecifier {
    /// Compare two adjacent calendar-semantic specifiers.
    ///
    /// This is only a *partial* ordering. We use this to determine if two specifiers can be
    /// adjacent. For specifiers `a` and `b` in a format string, `a` can only be to the immediate
    /// left of `b` if `a.partial_cmp(&b) == Some(Greater)`. In other words, *not* if they are:
    ///
    /// - `Some(Equal)`: this would be the same specifier twice
    /// - `Some(Less)`: this would mean `a` is less significant than `b`, which is not allowed.
    ///   Specifiers must "step down" in significance.
    /// - `None`: This means that `a` is greater than `b`, but by more than one step.
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use CalSemSpecifier::*;
        use Ordering::*;
        match (self, other) {
            (Year { .. }, Year { .. }) => Some(Equal),
            (Year { .. }, Day { .. }) => None,
            (Year { .. }, _) => Some(Greater),
            (Day { .. }, Year { .. }) => None,
            (_, Year { .. }) => Some(Less),

            (Month { .. }, Month { .. }) => Some(Equal),
            (Month { .. }, Week { .. }) => None,
            (Month { .. }, _) => Some(Greater),
            (Week { .. }, Month { .. }) => None,
            (_, Month { .. }) => Some(Less),

            (Week { .. }, Week { .. }) => Some(Equal),
            (Week { .. }, Day { .. }) => None,
            (Week { .. }, _) => Some(Greater),
            (Day { .. }, Week { .. }) => None,
            (_, Week { .. }) => Some(Less),

            (Day { .. }, Day { .. }) => Some(Equal),
            (Day { .. }, _) => Some(Greater),
            (_, Day { .. }) => Some(Less),

            (Minor { .. }, Minor { .. }) => Some(Equal),
            (Minor { .. }, _) => Some(Greater),
            (_, Minor { .. }) => Some(Less),

            (Patch { .. }, Patch { .. }) => Some(Equal),
        }
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
        // vec![Self::Minor, Self::Patch]
        static LAST: &[&CalSemSpecifier] = &[&CALSEM_MINOR, &CALSEM_PATCH];
        LAST
    }

    fn iter_all() -> impl Iterator<Item = &'static &'static Self> {
        CALSEM_ALL.iter()
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

#[cfg(test)]
mod tests {
    use super::*;
    use itertools::Itertools;

    #[test]
    fn sem_ordering() {
        use Ordering::*;
        use SemSpecifier::*;

        assert_eq!(Major.partial_cmp(&Major), Some(Equal));
        assert_eq!(Major.partial_cmp(&Minor), Some(Greater));
        assert_eq!(Major.partial_cmp(&Patch), None);

        assert_eq!(Minor.partial_cmp(&Major), Some(Less));
        assert_eq!(Minor.partial_cmp(&Minor), Some(Equal));
        assert_eq!(Minor.partial_cmp(&Patch), Some(Greater));

        assert_eq!(Patch.partial_cmp(&Major), None);
        assert_eq!(Patch.partial_cmp(&Minor), Some(Less));
        assert_eq!(Patch.partial_cmp(&Patch), Some(Equal));
    }

    #[test]
    fn cal_ordering() {
        use std::iter::once;
        use CalSpecifier::*;
        use NonYearType::*;
        use Ordering::*;
        use YearType::{Full as YFull, Short as YShort, ZeroPadded as YZeroPadded};

        let years = || once(vec![Year(YFull), Year(YShort), Year(YZeroPadded)]);
        let months = || once(vec![Month(Short), Month(ZeroPadded)]);
        let weeks = || once(vec![Week(Short), Week(ZeroPadded)]);
        let days = || once(vec![Day(Short), Day(ZeroPadded)]);

        // first, all within same type should be equal
        years()
            .chain(months())
            .chain(weeks())
            .chain(days())
            .for_each(|kind| {
                kind.iter().permutations(2).for_each(|pair| {
                    assert_eq!(pair[0].partial_cmp(pair[1]), Some(Equal));
                })
            });

        // then, compare in pairs
        // year > month
        years()
            .cartesian_product(months())
            .for_each(|(year, month)| {
                assert_eq!(year.partial_cmp(&month), Some(Greater));
                assert_eq!(month.partial_cmp(&year), Some(Less));
            });

        // year > week
        years().cartesian_product(weeks()).for_each(|(year, week)| {
            assert_eq!(year.partial_cmp(&week), Some(Greater));
            assert_eq!(week.partial_cmp(&year), Some(Less));
        });

        // year <> day
        years().cartesian_product(days()).for_each(|(year, day)| {
            assert_eq!(year.partial_cmp(&day), None);
            assert_eq!(day.partial_cmp(&year), None);
        });

        // month <> week
        months()
            .cartesian_product(weeks())
            .for_each(|(month, week)| {
                assert_eq!(month.partial_cmp(&week), None);
                assert_eq!(week.partial_cmp(&month), None);
            });

        // month > day
        months().cartesian_product(days()).for_each(|(month, day)| {
            assert_eq!(month.partial_cmp(&day), Some(Greater));
            assert_eq!(day.partial_cmp(&month), Some(Less));
        });

        // week <> day
        weeks().cartesian_product(days()).for_each(|(week, day)| {
            assert_eq!(week.partial_cmp(&day), None);
            assert_eq!(day.partial_cmp(&week), None);
        });
    }

    #[test]
    fn cal_sem_ordering() {
        use std::iter::once;
        use CalSemSpecifier::*;
        use NonYearType::*;
        use Ordering::*;
        use YearType::{Full as YFull, Short as YShort, ZeroPadded as YZeroPadded};

        let years = || once(vec![Year(YFull), Year(YShort), Year(YZeroPadded)]);
        let months = || once(vec![Month(Short), Month(ZeroPadded)]);
        let weeks = || once(vec![Week(Short), Week(ZeroPadded)]);
        let days = || once(vec![Day(Short), Day(ZeroPadded)]);
        let minors = || once(vec![Minor]);
        let patches = || once(vec![Patch]);

        // first, all within same type should be equal
        // don't need to do minors and patches because they're len = 1
        years()
            .chain(months())
            .chain(weeks())
            .chain(days())
            .for_each(|kind| {
                kind.iter().permutations(2).for_each(|pair| {
                    assert_eq!(pair[0].partial_cmp(pair[1]), Some(Equal));
                })
            });

        // then, compare in pairs
        // year > month
        years()
            .cartesian_product(months())
            .for_each(|(year, month)| {
                assert_eq!(year.partial_cmp(&month), Some(Greater));
                assert_eq!(month.partial_cmp(&year), Some(Less));
            });

        // year > week
        years().cartesian_product(weeks()).for_each(|(year, week)| {
            assert_eq!(year.partial_cmp(&week), Some(Greater));
            assert_eq!(week.partial_cmp(&year), Some(Less));
        });

        // year <> day
        years().cartesian_product(days()).for_each(|(year, day)| {
            assert_eq!(year.partial_cmp(&day), None);
            assert_eq!(day.partial_cmp(&year), None);
        });

        // year > minor
        years()
            .cartesian_product(minors())
            .for_each(|(year, minor)| {
                assert_eq!(year.partial_cmp(&minor), Some(Greater));
                assert_eq!(minor.partial_cmp(&year), Some(Less));
            });

        // year > patch
        years()
            .cartesian_product(patches())
            .for_each(|(year, patch)| {
                assert_eq!(year.partial_cmp(&patch), Some(Greater));
                assert_eq!(patch.partial_cmp(&year), Some(Less));
            });

        // month <> week
        months()
            .cartesian_product(weeks())
            .for_each(|(month, week)| {
                assert_eq!(month.partial_cmp(&week), None);
                assert_eq!(week.partial_cmp(&month), None);
            });

        // month > day
        months().cartesian_product(days()).for_each(|(month, day)| {
            assert_eq!(month.partial_cmp(&day), Some(Greater));
            assert_eq!(day.partial_cmp(&month), Some(Less));
        });

        // month > minor
        months()
            .cartesian_product(minors())
            .for_each(|(month, minor)| {
                assert_eq!(month.partial_cmp(&minor), Some(Greater));
                assert_eq!(minor.partial_cmp(&month), Some(Less));
            });

        // month > patch
        months()
            .cartesian_product(patches())
            .for_each(|(month, patch)| {
                assert_eq!(month.partial_cmp(&patch), Some(Greater));
                assert_eq!(patch.partial_cmp(&month), Some(Less));
            });

        // week <> day
        weeks().cartesian_product(days()).for_each(|(week, day)| {
            assert_eq!(week.partial_cmp(&day), None);
            assert_eq!(day.partial_cmp(&week), None);
        });

        // week > minor
        weeks()
            .cartesian_product(minors())
            .for_each(|(week, minor)| {
                assert_eq!(week.partial_cmp(&minor), Some(Greater));
                assert_eq!(minor.partial_cmp(&week), Some(Less));
            });

        // week > patch
        weeks()
            .cartesian_product(patches())
            .for_each(|(week, patch)| {
                assert_eq!(week.partial_cmp(&patch), Some(Greater));
                assert_eq!(patch.partial_cmp(&week), Some(Less));
            });

        // day > minor
        days().cartesian_product(minors()).for_each(|(day, minor)| {
            assert_eq!(day.partial_cmp(&minor), Some(Greater));
            assert_eq!(minor.partial_cmp(&day), Some(Less));
        });

        // day > patch
        days()
            .cartesian_product(patches())
            .for_each(|(day, patch)| {
                assert_eq!(day.partial_cmp(&patch), Some(Greater));
                assert_eq!(patch.partial_cmp(&day), Some(Less));
            });

        // minor > patch
        minors()
            .cartesian_product(patches())
            .for_each(|(minor, patch)| {
                assert_eq!(minor.partial_cmp(&patch), Some(Greater));
                assert_eq!(patch.partial_cmp(&minor), Some(Less));
            });
    }
}
