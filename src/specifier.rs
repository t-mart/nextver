use crate::version::NextError;
use chrono::{Datelike, NaiveDate};
use core::fmt::{self, Debug, Display};

pub(crate) type SpecValue = u32;
pub(crate) type SpecValueResult = Result<SpecValue, NextError>;
type NextDateResultFn = fn(NaiveDate) -> SpecValueResult;
type NextDateFn = fn(NaiveDate) -> SpecValue;

fn full_year_next(date: NaiveDate) -> SpecValueResult {
    // Note: Spec doesn't comment about years that are not 4-digit, so allow them
    let year = date.year();
    if year < 0 {
        // negatives would require a sign to round trip, so disallow
        // NOTE: 0 is allowed here, but it refers to 1 BCE. this is how chrono (and every datetime
        // library) works
        Err(NextError::NegativeYearValue { year })
    } else {
        #[allow(clippy::cast_sign_loss)]
        Ok(year as SpecValue)
    }
}

fn short_year_next(date: NaiveDate) -> SpecValueResult {
    let year = date.year();
    // while `year % 100` might seem like the right call, the spec allows this to be >=100 so that,
    // for example, `2001`, `2101`, `3001` are disambiguated as 1, 101, and 1001, respectively.
    let diff = year - 2000;
    if diff < 0 {
        // negatives would require a sign to round trip, so disallow
        Err(NextError::NegativeYearValue { year })
    } else {
        #[allow(clippy::cast_sign_loss)]
        Ok(diff as SpecValue)
    }
}

fn weeks_from_sunday_next(date: NaiveDate) -> SpecValue {
    let days_from_sunday = date.weekday().num_days_from_sunday();

    // This formula taken from a internal (`pub(crate)`) API inside chrono::NaiveDate. I'm unsure
    // why it's not true `pub`. hopefully safe.
    (6 + date.ordinal() - days_from_sunday) / 7
}

fn month_next(date: NaiveDate) -> SpecValue {
    date.month()
}

fn day_next(date: NaiveDate) -> SpecValue {
    date.day()
}

pub(crate) trait Specifier: PartialEq + Eq + Debug + Display + Sized + 'static {
    fn format_pattern(&self) -> &'static [u8];

    fn format_width(&self) -> usize;

    fn has_zero_padding(&self) -> bool {
        self.format_width() > 0
    }

    fn format_value(&self, value: SpecValue) -> String {
        format!("{:0len$}", value, len = self.format_width())
    }

    fn parse_width(&self) -> ParseWidth;

    fn can_be_zero(&self) -> bool;

    fn first_variants() -> &'static [&'static Self];

    fn last_variants() -> &'static [&'static Self];

    fn can_be_first(&self) -> bool {
        Self::first_variants().contains(&self)
    }

    fn can_be_last(&self) -> bool {
        Self::last_variants().contains(&self)
    }

    fn all() -> &'static [&'static Self];

    fn can_be_left_adjacent_to(&self, other: &Self) -> bool;
}

pub(crate) enum ParseWidth {
    AtLeastOne,
    AtLeastTwo,
    OneOrTwo,
    Two,
}

impl ParseWidth {
    pub(crate) fn min_width(&self) -> usize {
        match self {
            Self::OneOrTwo | Self::AtLeastOne => 1,
            Self::AtLeastTwo | Self::Two => 2,
        }
    }

    pub(crate) fn max_width(&self) -> usize {
        match self {
            Self::AtLeastOne | Self::AtLeastTwo => usize::MAX,
            Self::OneOrTwo |Self::Two => 2,
        }
    }
}

const MAJOR_FORMAT_PATTERN: &[u8] = b"<MAJOR>";

const MINOR_FORMAT_PATTERN: &[u8] = b"<MINOR>";

const PATCH_FORMAT_PATTERN: &[u8] = b"<PATCH>";

const SEM_FORMAT_WIDTH: usize = 0;
const SEM_PARSE_WIDTH: ParseWidth = ParseWidth::AtLeastOne;
const SEM_CAN_BE_ZERO: bool = true;

const YEAR_FULL_FORMAT_STRINGS: &[u8] = b"<YYYY>";
const YEAR_FULL_FORMAT_WIDTH: usize = 0;
const YEAR_FULL_NEXT_FN: NextDateResultFn = full_year_next;
const YEAR_FULL_CAN_BE_ZERO: bool = true; // note that 0 is 1 BCE
const YEAR_FULL_PARSE_WIDTH: ParseWidth = ParseWidth::AtLeastOne;

const YEAR_SHORT_FORMAT_STRINGS: &[u8] = b"<YY>";
const YEAR_SHORT_FORMAT_WIDTH: usize = 0;
const YEAR_SHORT_CAN_BE_ZERO: bool = true;
const YEAR_SHORT_PARSE_WIDTH: ParseWidth = ParseWidth::AtLeastOne;

const YEAR_ZERO_PADDED_FORMAT_STRINGS: &[u8] = b"<0Y>";
const YEAR_ZERO_PADDED_FORMAT_WIDTH: usize = 2;
const YEAR_ZERO_PADDED_CAN_BE_ZERO: bool = true;
const YEAR_ZERO_PADDED_PARSE_WIDTH: ParseWidth = ParseWidth::AtLeastTwo;

const YEAR_SHORT_AND_ZERO_PADDED_NEXT_FN: NextDateResultFn = short_year_next;

const MONTH_SHORT_FORMAT_STRINGS: &[u8] = b"<MM>";
const MONTH_SHORT_FORMAT_WIDTH: usize = 0;

const MONTH_ZERO_PADDED_FORMAT_STRINGS: &[u8] = b"<0M>";
const MONTH_ZERO_PADDED_FORMAT_WIDTH: usize = 2;

const MONTH_CAN_BE_ZERO: bool = false;
const MONTH_NEXT_FN: NextDateFn = month_next;

const WEEK_SHORT_FORMAT_STRINGS: &[u8] = b"<WW>";
const WEEK_SHORT_FORMAT_WIDTH: usize = 0;

const WEEK_ZERO_PADDED_FORMAT_STRINGS: &[u8] = b"<0W>";
const WEEK_ZERO_PADDED_FORMAT_WIDTH: usize = 2;

const WEEK_CAN_BE_ZERO: bool = true;
const WEEK_NEXT_FN: NextDateFn = weeks_from_sunday_next;

const DAY_SHORT_FORMAT_STRINGS: &[u8] = b"<DD>";
const DAY_SHORT_FORMAT_WIDTH: usize = 0;

const DAY_ZERO_PADDED_FORMAT_STRINGS: &[u8] = b"<0D>";
const DAY_ZERO_PADDED_FORMAT_WIDTH: usize = 2;

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
    pub(crate) fn next_value(&self, cur_value: SpecValue, level: SemLevel) -> SpecValue {
        match (self, level) {
            (_, _) if level.spec() == self => cur_value + 1,
            (_, SemLevel::Major) | (SemSpecifier::Patch, SemLevel::Minor) => 0,
            _ => cur_value,
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
        use SemSpecifier as S;
        match self {
            S::Major => MAJOR_FORMAT_PATTERN,
            S::Minor => MINOR_FORMAT_PATTERN,
            S::Patch => PATCH_FORMAT_PATTERN,
        }
    }

    fn format_width(&self) -> usize {
        SEM_FORMAT_WIDTH
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
        use SemSpecifier as S;
        matches!((self, other), (S::Major, S::Minor) | (S::Minor, S::Patch))
    }
}
pub(crate) const SEM_MAJOR: SemSpecifier = SemSpecifier::Major;
pub(crate) const SEM_MINOR: SemSpecifier = SemSpecifier::Minor;
pub(crate) const SEM_PATCH: SemSpecifier = SemSpecifier::Patch;
const SEM_ALL: &[&SemSpecifier] = &[&SEM_MAJOR, &SEM_MINOR, &SEM_PATCH];

/// A semantic specifier to increment in a [`Sem`](crate::Sem) [`Version`](crate::Version).
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
#[non_exhaustive]
pub enum SemLevel {
    /// Refers to the major version specifier,`<MAJOR>`. It is greater than `<MINOR>` and `<PATCH>`.
    Major,
    /// Refers to the minor version specifier,`<MINOR>`. It is greater than `<PATCH>` and less than
    /// `<MAJOR>`.
    Minor,
    /// Refers to the patch version specifier,`<PATCH>`. It is less than `<MINOR>` and `<MAJOR>`.
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

impl AsRef<SemLevel> for SemLevel {
    fn as_ref(&self) -> &SemLevel {
        self
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
    pub(crate) fn next_value(&self, date: NaiveDate) -> SpecValueResult {
        match &self {
            CalSpecifier::Year(type_) => match type_ {
                YearType::Full => YEAR_FULL_NEXT_FN(date),
                YearType::Short | YearType::ZeroPadded => YEAR_SHORT_AND_ZERO_PADDED_NEXT_FN(date),
            },
            CalSpecifier::Month(_) => Ok(MONTH_NEXT_FN(date)),
            CalSpecifier::Week(_) => Ok(WEEK_NEXT_FN(date)),
            CalSpecifier::Day(_) => Ok(DAY_NEXT_FN(date)),
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
        use CalSpecifier as C;
        match self {
            C::Year(type_) => match type_ {
                YearType::Full => YEAR_FULL_FORMAT_STRINGS,
                YearType::Short => YEAR_SHORT_FORMAT_STRINGS,
                YearType::ZeroPadded => YEAR_ZERO_PADDED_FORMAT_STRINGS,
            },
            C::Month(type_) => match type_ {
                NonYearType::Short => MONTH_SHORT_FORMAT_STRINGS,
                NonYearType::ZeroPadded => MONTH_ZERO_PADDED_FORMAT_STRINGS,
            },
            C::Week(type_) => match type_ {
                NonYearType::Short => WEEK_SHORT_FORMAT_STRINGS,
                NonYearType::ZeroPadded => WEEK_ZERO_PADDED_FORMAT_STRINGS,
            },
            C::Day(type_) => match type_ {
                NonYearType::Short => DAY_SHORT_FORMAT_STRINGS,
                NonYearType::ZeroPadded => DAY_ZERO_PADDED_FORMAT_STRINGS,
            },
        }
    }

    fn format_width(&self) -> usize {
        use CalSpecifier as C;
        match self {
            C::Year(type_) => match type_ {
                YearType::Full => YEAR_FULL_FORMAT_WIDTH,
                YearType::Short => YEAR_SHORT_FORMAT_WIDTH,
                YearType::ZeroPadded => YEAR_ZERO_PADDED_FORMAT_WIDTH,
            },
            C::Month(type_) => match type_ {
                NonYearType::Short => MONTH_SHORT_FORMAT_WIDTH,
                NonYearType::ZeroPadded => MONTH_ZERO_PADDED_FORMAT_WIDTH,
            },
            C::Week(type_) => match type_ {
                NonYearType::Short => WEEK_SHORT_FORMAT_WIDTH,
                NonYearType::ZeroPadded => WEEK_ZERO_PADDED_FORMAT_WIDTH,
            },
            C::Day(type_) => match type_ {
                NonYearType::Short => DAY_SHORT_FORMAT_WIDTH,
                NonYearType::ZeroPadded => DAY_ZERO_PADDED_FORMAT_WIDTH,
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
        use CalSpecifier as C;
        match self {
            C::Year(type_) => match type_ {
                YearType::Full => YEAR_FULL_CAN_BE_ZERO,
                YearType::Short => YEAR_SHORT_CAN_BE_ZERO,
                YearType::ZeroPadded => YEAR_ZERO_PADDED_CAN_BE_ZERO,
            },
            C::Month(_) => MONTH_CAN_BE_ZERO,
            C::Week(_) => WEEK_CAN_BE_ZERO,
            C::Day(_) => DAY_CAN_BE_ZERO,
        }
    }

    fn can_be_left_adjacent_to(&self, other: &Self) -> bool {
        use CalSpecifier as C;
        matches!(
            (self, other),
            (C::Year(_), C::Month(_) | C::Week(_)) | (C::Month(_), C::Day(_))
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
pub(crate) enum CalSemCalSpecifier {
    Year(YearType),
    Month(NonYearType),
    Week(NonYearType),
    Day(NonYearType),
}

impl CalSemCalSpecifier {
    pub(crate) fn next_value(&self, date: NaiveDate) -> SpecValueResult {
        use CalSemCalSpecifier as CSC;
        match &self {
            CSC::Year(type_) => match type_ {
                YearType::Full => YEAR_FULL_NEXT_FN(date),
                YearType::Short | YearType::ZeroPadded => YEAR_SHORT_AND_ZERO_PADDED_NEXT_FN(date),
            },
            CSC::Month(_) => Ok(MONTH_NEXT_FN(date)),
            CSC::Week(_) => Ok(WEEK_NEXT_FN(date)),
            CSC::Day(_) => Ok(DAY_NEXT_FN(date)),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum CalSemSemSpecifier {
    Minor,
    Patch,
}

impl CalSemSemSpecifier {
    pub(crate) fn next_value(&self, cur_value: SpecValue, level: CalSemLevel) -> SpecValue {
        use CalSemSemSpecifier as CSS;
        match level {
            CalSemLevel::Minor => match self {
                CSS::Minor => cur_value + 1,
                CSS::Patch => 0,
            },
            CalSemLevel::Patch => match self {
                CSS::Minor => cur_value,
                CSS::Patch => cur_value + 1,
            },
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub(crate) enum CalSemSpecifier {
    Cal(CalSemCalSpecifier),
    Sem(CalSemSemSpecifier),
}

impl Display for CalSemSpecifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(unsafe { std::str::from_utf8_unchecked(self.format_pattern()) })
    }
}

impl Specifier for CalSemSpecifier {
    fn format_pattern(&self) -> &'static [u8] {
        use CalSemCalSpecifier as CSC;
        use CalSemSemSpecifier as CSS;
        use CalSemSpecifier as S;
        match self {
            S::Cal(CSC::Year(type_)) => match type_ {
                YearType::Full => YEAR_FULL_FORMAT_STRINGS,
                YearType::Short => YEAR_SHORT_FORMAT_STRINGS,
                YearType::ZeroPadded => YEAR_ZERO_PADDED_FORMAT_STRINGS,
            },
            S::Cal(CSC::Month(type_)) => match type_ {
                NonYearType::Short => MONTH_SHORT_FORMAT_STRINGS,
                NonYearType::ZeroPadded => MONTH_ZERO_PADDED_FORMAT_STRINGS,
            },
            S::Cal(CSC::Week(type_)) => match type_ {
                NonYearType::Short => WEEK_SHORT_FORMAT_STRINGS,
                NonYearType::ZeroPadded => WEEK_ZERO_PADDED_FORMAT_STRINGS,
            },
            S::Cal(CSC::Day(type_)) => match type_ {
                NonYearType::Short => DAY_SHORT_FORMAT_STRINGS,
                NonYearType::ZeroPadded => DAY_ZERO_PADDED_FORMAT_STRINGS,
            },
            S::Sem(CSS::Minor) => MINOR_FORMAT_PATTERN,
            S::Sem(CSS::Patch) => PATCH_FORMAT_PATTERN,
        }
    }

    fn format_width(&self) -> usize {
        use CalSemCalSpecifier as CSC;
        use CalSemSemSpecifier as CSS;
        use CalSemSpecifier as S;
        match self {
            S::Cal(CSC::Year(type_)) => match type_ {
                YearType::Full => YEAR_FULL_FORMAT_WIDTH,
                YearType::Short => YEAR_SHORT_FORMAT_WIDTH,
                YearType::ZeroPadded => YEAR_ZERO_PADDED_FORMAT_WIDTH,
            },
            S::Cal(CSC::Month(type_)) => match type_ {
                NonYearType::Short => MONTH_SHORT_FORMAT_WIDTH,
                NonYearType::ZeroPadded => MONTH_ZERO_PADDED_FORMAT_WIDTH,
            },
            S::Cal(CSC::Week(type_)) => match type_ {
                NonYearType::Short => WEEK_SHORT_FORMAT_WIDTH,
                NonYearType::ZeroPadded => WEEK_ZERO_PADDED_FORMAT_WIDTH,
            },
            S::Cal(CSC::Day(type_)) => match type_ {
                NonYearType::Short => DAY_SHORT_FORMAT_WIDTH,
                NonYearType::ZeroPadded => DAY_ZERO_PADDED_FORMAT_WIDTH,
            },
            S::Sem(CSS::Minor | CSS::Patch) => SEM_FORMAT_WIDTH,
        }
    }

    fn can_be_first(&self) -> bool {
        matches!(self, CalSemSpecifier::Cal(CalSemCalSpecifier::Year(_)))
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
        use CalSemCalSpecifier as CSC;
        use CalSemSemSpecifier as CSS;
        use CalSemSpecifier as S;
        match self {
            S::Cal(CSC::Year(YearType::Full)) => YEAR_FULL_PARSE_WIDTH,
            S::Cal(CSC::Year(YearType::Short)) => YEAR_SHORT_PARSE_WIDTH,
            S::Cal(CSC::Year(YearType::ZeroPadded)) => YEAR_ZERO_PADDED_PARSE_WIDTH,
            S::Cal(
                CSC::Month(NonYearType::ZeroPadded)
                | CSC::Week(NonYearType::ZeroPadded)
                | CSC::Day(NonYearType::ZeroPadded),
            ) => MONTH_WEEK_DAY_ZERO_PADDED_PARSE_WIDTH,
            S::Cal(
                CSC::Month(NonYearType::Short)
                | CSC::Week(NonYearType::Short)
                | CSC::Day(NonYearType::Short),
            ) => MONTH_WEEK_DAY_SHORT_PARSE_WIDTH,
            S::Sem(CSS::Minor | CSS::Patch) => SEM_PARSE_WIDTH,
        }
    }

    fn can_be_zero(&self) -> bool {
        use CalSemCalSpecifier as CSC;
        use CalSemSemSpecifier as CSS;
        use CalSemSpecifier as S;
        match self {
            S::Cal(CSC::Year(type_)) => match type_ {
                YearType::Full => YEAR_FULL_CAN_BE_ZERO,
                YearType::Short => YEAR_SHORT_CAN_BE_ZERO,
                YearType::ZeroPadded => YEAR_ZERO_PADDED_CAN_BE_ZERO,
            },
            S::Cal(CSC::Month(_)) => MONTH_CAN_BE_ZERO,
            S::Cal(CSC::Week(_)) => WEEK_CAN_BE_ZERO,
            S::Cal(CSC::Day(_)) => DAY_CAN_BE_ZERO,
            S::Sem(CSS::Minor | CSS::Patch) => SEM_CAN_BE_ZERO,
        }
    }

    fn can_be_left_adjacent_to(&self, other: &Self) -> bool {
        use CalSemCalSpecifier as CSC;
        use CalSemSemSpecifier as CSS;
        use CalSemSpecifier as S;
        matches!(
            (self, other),
            (
                S::Cal(CSC::Year(_)),
                S::Cal(CSC::Month(_) | CSC::Week(_)) | S::Sem(CSS::Minor | CSS::Patch)
            ) | (
                S::Cal(CSC::Month(_)),
                S::Cal(CSC::Day(_)) | S::Sem(CSS::Minor | CSS::Patch)
            ) | (S::Cal(CSC::Week(_) | CSC::Day(_)), S::Sem(CSS::Minor))
                | (
                    S::Cal(CSC::Week(_) | CSC::Day(_)) | S::Sem(CSS::Minor),
                    S::Sem(CSS::Patch)
                )
        )
    }
}
pub(crate) const CALSEM_YEAR_FULL: CalSemSpecifier =
    CalSemSpecifier::Cal(CalSemCalSpecifier::Year(YearType::Full));
pub(crate) const CALSEM_YEAR_SHORT: CalSemSpecifier =
    CalSemSpecifier::Cal(CalSemCalSpecifier::Year(YearType::Short));
pub(crate) const CALSEM_YEAR_ZERO_PADDED: CalSemSpecifier =
    CalSemSpecifier::Cal(CalSemCalSpecifier::Year(YearType::ZeroPadded));
pub(crate) const CALSEM_MONTH_SHORT: CalSemSpecifier =
    CalSemSpecifier::Cal(CalSemCalSpecifier::Month(NonYearType::Short));
pub(crate) const CALSEM_MONTH_ZERO_PADDED: CalSemSpecifier =
    CalSemSpecifier::Cal(CalSemCalSpecifier::Month(NonYearType::ZeroPadded));
pub(crate) const CALSEM_WEEK_SHORT: CalSemSpecifier =
    CalSemSpecifier::Cal(CalSemCalSpecifier::Week(NonYearType::Short));
pub(crate) const CALSEM_WEEK_ZERO_PADDED: CalSemSpecifier =
    CalSemSpecifier::Cal(CalSemCalSpecifier::Week(NonYearType::ZeroPadded));
pub(crate) const CALSEM_DAY_SHORT: CalSemSpecifier =
    CalSemSpecifier::Cal(CalSemCalSpecifier::Day(NonYearType::Short));
pub(crate) const CALSEM_DAY_ZERO_PADDED: CalSemSpecifier =
    CalSemSpecifier::Cal(CalSemCalSpecifier::Day(NonYearType::ZeroPadded));
pub(crate) const CALSEM_MINOR: CalSemSpecifier = CalSemSpecifier::Sem(CalSemSemSpecifier::Minor);
pub(crate) const CALSEM_PATCH: CalSemSpecifier = CalSemSpecifier::Sem(CalSemSemSpecifier::Patch);
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

/// A semantic-type specifier to increment in a [`CalSem`](crate::CalSem)
/// [`Version`](crate::Version).
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum CalSemLevel {
    /// Refers to the minor version specifier, `<MINOR>`. It is greater than `<PATCH>`.
    Minor,
    /// Refers to the patch version specifier, `<PATCH>`. It is less than `<MINOR>`.
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

impl AsRef<CalSemLevel> for CalSemLevel {
    fn as_ref(&self) -> &CalSemLevel {
        self
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
    #[allow(clippy::too_many_lines)]
    fn cal_sem_ordering() {
        use CalSemCalSpecifier::*;
        use CalSemSemSpecifier::*;
        use CalSemSpecifier::*;
        use NonYearType::*;
        use YearType::{Full as YFull, Short as YShort, ZeroPadded as YZeroPadded};

        let years = || [Cal(Year(YFull)), Cal(Year(YShort)), Cal(Year(YZeroPadded))].iter();
        let months = || [Cal(Month(Short)), Cal(Month(ZeroPadded))].iter();
        let weeks = || [Cal(Week(Short)), Cal(Week(ZeroPadded))].iter();
        let days = || [Cal(Day(Short)), Cal(Day(ZeroPadded))].iter();
        let minors = || [Sem(Minor)].iter();
        let patches = || [Sem(Patch)].iter();

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
