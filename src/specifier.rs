use crate::NextverError;
use chrono::{Datelike, NaiveDate};
use std::cmp::Ordering;
use std::fmt::{self, Debug, Display};

type Next = u32;
type NextResult = Result<u32, NextverError>;
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
        Err(NextverError::NegativeYearValue { year })
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
        Err(NextverError::NegativeYearValue { year })
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

fn month_next(date: &NaiveDate) -> Result<u32, NextverError> {
    Ok(date.month())
}

fn day_next(date: &NaiveDate) -> Result<u32, NextverError> {
    Ok(date.day())
}

pub(crate) trait Specifier:
    PartialEq + Eq + PartialOrd + Display + Debug + Sized + Clone + Copy
{
    fn format_pattern(&self) -> &'static str;

    fn version_pattern(&self) -> &'static str;

    fn zero_pad_len(&self) -> Option<usize>;

    fn first_variants() -> Vec<Self>;

    fn last_variants() -> Vec<Self>;

    fn can_be_first(&self) -> bool {
        Self::first_variants().contains(self)
    }

    fn can_be_last(&self) -> bool {
        Self::last_variants().contains(self)
    }

    fn format_value(&self, value: &u32) -> String {
        match self.zero_pad_len() {
            Some(len) => format!("{:0len$}", value, len = len),
            None => u32::to_string(value),
        }
    }
    fn parse_value_str(&self, value: &str) -> u32 {
        value.parse().unwrap()
    }

    fn iter_all() -> impl Iterator<Item = Self>;
}

// not zero padded, any length
const SEM_VERSION_PATTERN: &str = r"([1-9][0-9]*)";
// not zero padded, 1-2 digits, no 0
const CAL_SHORT_MONTH_DAY_PATTERN: &str = r"([1-9][0-9]?)";
// zero padded, 2 digits, no 00
const CAL_ZERO_PADDED_MONTH_DAY_PATTERN: &str = r"((?:0[1-9])|(?:[1-9][0-9]))";

const MAJOR_FORMAT_PATTERN: &str = "[MAJOR]";
const MAJOR_VERSION_PATTERN: &str = SEM_VERSION_PATTERN;

const MINOR_FORMAT_PATTERN: &str = "[MINOR]";
const MINOR_VERSION_PATTERN: &str = SEM_VERSION_PATTERN;

const PATCH_FORMAT_PATTERN: &str = "[PATCH]";
const PATCH_VERSION_PATTERN: &str = SEM_VERSION_PATTERN;

const SEM_ZERO_PAD_LEN: Option<usize> = None;

const YEAR_FULL_FORMAT_STRINGS: &str = "[YYYY]";
const YEAR_FULL_VERSION_PATTERN: &str = r"([1-9][0-9]*)"; // there is no year 0,
const YEAR_FULL_ZERO_PAD_LEN: Option<usize> = None;
const YEAR_FULL_NEXT_FN: NextDateFn = full_year_next;

const YEAR_SHORT_FORMAT_STRINGS: &str = "[YY]";
const YEAR_SHORT_VERSION_PATTERN: &str = r"((?:0)|(?:[1-9][0-9]*))"; // match year 0 (2000), but otherwise, must start [1-9]
const YEAR_SHORT_ZERO_PAD_LEN: Option<usize> = None;

const YEAR_ZERO_PADDED_FORMAT_STRINGS: &str = "[0Y]";
const YEAR_ZERO_PADDED_VERSION_PATTERN: &str = r"([0-9]{2,})"; // 00 is fine
const YEAR_ZERO_PADDED_ZERO_PAD_LEN: Option<usize> = Some(2);

const YEAR_SHORT_AND_ZERO_PADDED_NEXT_FN: NextDateFn = short_year_next;

const MONTH_SHORT_FORMAT_STRINGS: &str = "[MM]";
const MONTH_SHORT_VERSION_PATTERN: &str = CAL_SHORT_MONTH_DAY_PATTERN;
const MONTH_SHORT_ZERO_PAD_LEN: Option<usize> = None;

const MONTH_ZERO_PADDED_FORMAT_STRINGS: &str = "[0M]";
const MONTH_ZERO_PADDED_VERSION_PATTERN: &str = CAL_ZERO_PADDED_MONTH_DAY_PATTERN;
const MONTH_ZERO_PADDED_ZERO_PAD_LEN: Option<usize> = Some(2);

const MONTH_SHORT_AND_ZERO_PADDED_NEXT_FN: NextDateFn = month_next;

const WEEK_SHORT_FORMAT_STRINGS: &str = "[WW]";
const WEEK_SHORT_VERSION_PATTERN: &str = r"((?:[0-9])|(?:[1-9][0-9]))";
const WEEK_SHORT_ZERO_PAD_LEN: Option<usize> = None;

const WEEK_ZERO_PADDED_FORMAT_STRINGS: &str = "[0W]";
const WEEK_ZERO_PADDED_VERSION_PATTERN: &str = r"([0-9]{2})";
const WEEK_ZERO_PADDED_ZERO_PAD_LEN: Option<usize> = Some(2);

const WEEK_SHORT_AND_ZERO_PADDED_NEXT_FN: NextDateFn = weeks_from_sunday_next;

const DAY_SHORT_FORMAT_STRINGS: &str = "[DD]";
const DAY_SHORT_VERSION_PATTERN: &str = CAL_SHORT_MONTH_DAY_PATTERN;
const DAY_SHORT_ZERO_PAD_LEN: Option<usize> = None;

const DAY_ZERO_PADDED_FORMAT_STRINGS: &str = "[0D]";
const DAY_ZERO_PADDED_VERSION_PATTERN: &str = CAL_ZERO_PADDED_MONTH_DAY_PATTERN;
const DAY_ZERO_PADDED_ZERO_PAD_LEN: Option<usize> = Some(2);

const DAY_SHORT_AND_ZERO_PADDED_NEXT_FN: NextDateFn = day_next;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub(crate) enum YearType {
    Full,
    Short,
    ZeroPadded,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub(crate) enum NonYearType {
    Short,
    ZeroPadded,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum SemSpecifier {
    Major,
    Minor,
    Patch,
}

impl SemSpecifier {
    pub(crate) fn next(&self, cur_val: &u32, already_bumped: bool) -> u32 {
        sem_next(cur_val, already_bumped)
    }
}

impl PartialOrd for SemSpecifier {
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
        write!(f, "{}", self.format_pattern())
    }
}

impl Specifier for SemSpecifier {
    fn format_pattern(&self) -> &'static str {
        use SemSpecifier::*;
        match self {
            Major => MAJOR_FORMAT_PATTERN,
            Minor => MINOR_FORMAT_PATTERN,
            Patch => PATCH_FORMAT_PATTERN,
        }
    }

    fn version_pattern(&self) -> &'static str {
        use SemSpecifier::*;
        match self {
            Major => MAJOR_VERSION_PATTERN,
            Minor => MINOR_VERSION_PATTERN,
            Patch => PATCH_VERSION_PATTERN,
        }
    }

    fn zero_pad_len(&self) -> Option<usize> {
        SEM_ZERO_PAD_LEN
    }

    fn first_variants() -> Vec<Self> {
        vec![SemSpecifier::Major]
    }

    fn last_variants() -> Vec<Self> {
        Self::iter_all().collect()
    }

    fn iter_all() -> impl Iterator<Item = Self> {
        use SemSpecifier::*;
        [Major, Minor, Patch].into_iter()
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[repr(u8)]
pub(crate) enum CalSpecifier {
    Year(YearType) = 3,
    Month(NonYearType) = 2,
    Week(NonYearType) = 1,
    Day(NonYearType) = 0,
}

impl CalSpecifier {
    pub(crate) fn next(&self, date: &NaiveDate) -> Result<u32, NextverError> {
        match &self {
            CalSpecifier::Year(type_) => match type_ {
                YearType::Full => YEAR_FULL_NEXT_FN(date),
                YearType::Short => YEAR_SHORT_AND_ZERO_PADDED_NEXT_FN(date),
                YearType::ZeroPadded => YEAR_SHORT_AND_ZERO_PADDED_NEXT_FN(date),
            },
            CalSpecifier::Month(_) => MONTH_SHORT_AND_ZERO_PADDED_NEXT_FN(date),
            CalSpecifier::Week(_) => WEEK_SHORT_AND_ZERO_PADDED_NEXT_FN(date),
            CalSpecifier::Day(_) => DAY_SHORT_AND_ZERO_PADDED_NEXT_FN(date),
        }
    }
}

impl PartialOrd for CalSpecifier {
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
        write!(f, "{}", self.format_pattern())
    }
}

impl Specifier for CalSpecifier {
    fn format_pattern(&self) -> &'static str {
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

    fn version_pattern(&self) -> &'static str {
        use CalSpecifier::*;
        match self {
            Year(type_) => match type_ {
                YearType::Full => YEAR_FULL_VERSION_PATTERN,
                YearType::Short => YEAR_SHORT_VERSION_PATTERN,
                YearType::ZeroPadded => YEAR_ZERO_PADDED_VERSION_PATTERN,
            },
            Month(type_) => match type_ {
                NonYearType::Short => MONTH_SHORT_VERSION_PATTERN,
                NonYearType::ZeroPadded => MONTH_ZERO_PADDED_VERSION_PATTERN,
            },
            Week(type_) => match type_ {
                NonYearType::Short => WEEK_SHORT_VERSION_PATTERN,
                NonYearType::ZeroPadded => WEEK_ZERO_PADDED_VERSION_PATTERN,
            },
            Day(type_) => match type_ {
                NonYearType::Short => DAY_SHORT_VERSION_PATTERN,
                NonYearType::ZeroPadded => DAY_ZERO_PADDED_VERSION_PATTERN,
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

    fn first_variants() -> Vec<Self> {
        use CalSpecifier::*;
        vec![
            Year(YearType::Full),
            Year(YearType::Short),
            Year(YearType::ZeroPadded),
        ]
    }

    fn last_variants() -> Vec<Self> {
        Self::iter_all().collect()
    }

    fn iter_all() -> impl Iterator<Item = Self> {
        use CalSpecifier::*;
        [
            Year(YearType::Full),
            Year(YearType::Short),
            Year(YearType::ZeroPadded),
            Month(NonYearType::Short),
            Month(NonYearType::ZeroPadded),
            Week(NonYearType::Short),
            Week(NonYearType::ZeroPadded),
            Day(NonYearType::Short),
            Day(NonYearType::ZeroPadded),
        ]
        .into_iter()
    }
}

pub enum CalSemIncrSpecifier {
    Minor,
    Patch,
}

impl CalSemIncrSpecifier {
    pub(crate) fn spec(&self) -> CalSemSpecifier {
        match self {
            Self::Minor => CalSemSpecifier::Minor,
            Self::Patch => CalSemSpecifier::Patch,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
    ) -> Result<u32, NextverError> {
        match &self {
            CalSemSpecifier::Year(type_) => match type_ {
                YearType::Full => YEAR_FULL_NEXT_FN(date),
                YearType::Short => YEAR_SHORT_AND_ZERO_PADDED_NEXT_FN(date),
                YearType::ZeroPadded => YEAR_SHORT_AND_ZERO_PADDED_NEXT_FN(date),
            },
            CalSemSpecifier::Month(_) => MONTH_SHORT_AND_ZERO_PADDED_NEXT_FN(date),
            CalSemSpecifier::Week(_) => WEEK_SHORT_AND_ZERO_PADDED_NEXT_FN(date),
            CalSemSpecifier::Day(_) => DAY_SHORT_AND_ZERO_PADDED_NEXT_FN(date),
            CalSemSpecifier::Minor | CalSemSpecifier::Patch => {
                Ok(sem_next(cur_val, already_bumped))
            }
        }
    }
}

impl Display for CalSemSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.format_pattern())
    }
}

impl PartialOrd for CalSemSpecifier {
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
    fn format_pattern(&self) -> &'static str {
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

    fn version_pattern(&self) -> &'static str {
        use CalSemSpecifier::*;
        match self {
            Year(type_) => match type_ {
                YearType::Full => YEAR_FULL_VERSION_PATTERN,
                YearType::Short => YEAR_SHORT_VERSION_PATTERN,
                YearType::ZeroPadded => YEAR_ZERO_PADDED_VERSION_PATTERN,
            },
            Month(type_) => match type_ {
                NonYearType::Short => MONTH_SHORT_VERSION_PATTERN,
                NonYearType::ZeroPadded => MONTH_ZERO_PADDED_VERSION_PATTERN,
            },
            Week(type_) => match type_ {
                NonYearType::Short => WEEK_SHORT_VERSION_PATTERN,
                NonYearType::ZeroPadded => WEEK_ZERO_PADDED_VERSION_PATTERN,
            },
            Day(type_) => match type_ {
                NonYearType::Short => DAY_SHORT_VERSION_PATTERN,
                NonYearType::ZeroPadded => DAY_ZERO_PADDED_VERSION_PATTERN,
            },
            Minor => MINOR_VERSION_PATTERN,
            Patch => PATCH_VERSION_PATTERN,
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

    fn first_variants() -> Vec<Self> {
        use CalSemSpecifier::*;
        vec![
            Year(YearType::Full),
            Year(YearType::Short),
            Year(YearType::ZeroPadded),
        ]
    }

    fn last_variants() -> Vec<Self> {
        vec![Self::Minor, Self::Patch]
    }

    fn iter_all() -> impl Iterator<Item = Self> {
        use CalSemSpecifier::*;
        [
            Year(YearType::Full),
            Year(YearType::Short),
            Year(YearType::ZeroPadded),
            Month(NonYearType::Short),
            Month(NonYearType::ZeroPadded),
            Week(NonYearType::Short),
            Week(NonYearType::ZeroPadded),
            Day(NonYearType::Short),
            Day(NonYearType::ZeroPadded),
            Minor,
            Patch,
        ]
        .into_iter()
    }
}

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
        assert_eq!(Major.partial_cmp(&Patch), Some(Greater));

        assert_eq!(Minor.partial_cmp(&Major), Some(Less));
        assert_eq!(Minor.partial_cmp(&Minor), Some(Equal));
        assert_eq!(Minor.partial_cmp(&Patch), Some(Greater));

        assert_eq!(Patch.partial_cmp(&Major), Some(Less));
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
