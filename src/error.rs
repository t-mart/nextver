use crate::Date;
#[cfg(doc)]
use chrono::NaiveDate;
#[cfg(doc)]
use std::str::FromStr;

// In error messages, strive (insist?) on using the “expect as precondition” style of error because
// it is more helpful for the user. In other words, include the word "SHOULD". See
// <https://doc.rust-lang.org/std/error/index.html#common-message-styles>.
//
// Additionally, "error messages are typically concise lowercase sentences without trailing
// punctuation": <https://doc.rust-lang.org/std/error/trait.Error.html#>

/// An error that occurred in a function that internally does format and version parsing (which have
/// their own errors).
#[non_exhaustive]
#[derive(thiserror::Error, Debug, PartialEq)]
pub enum CompositeError {
    /// An error from parsing a format string. See [`FormatError`] for more details.
    #[error("format error: {0}")]
    Format(#[from] FormatError),

    /// An error from parsing a version string. See [`VersionError`] for more details.
    #[error("version error: {0}")]
    Version(#[from] VersionError),
}

/// An error that occurred while parsing a version string.
#[non_exhaustive]
#[derive(thiserror::Error, Debug, PartialEq)]
pub enum VersionError {
    /// The format string's structure doesn't match the version string's.
    #[error("version `{version_string}` should match format `{format_string}`")]
    VersionFormatMismatch {
        /// The version string
        version_string: String,
        /// The format string
        format_string: String,
    },

    /// When updating a [Cal](crate::Cal) version, the date provided yielded an identical version.
    #[error("date provided should yield a version that is newer/greater than the current version")]
    NoCalendarChange,

    /// When updating a [Cal](crate::Cal) or [CalSem](crate::CalSem) version, the date year was negative.
    #[error("year `{year}` should not be negative when formatted`")]
    NegativeYearValue {
        /// The year value
        year: i32,
    },

    /// When updating/incrementing a [Sem](crate::Sem) or [CalSem](crate::CalSem) version, the
    /// semantic specifier level is not in the format.
    #[error("`{spec}` was not found in format, use one that is")]
    SemanticSpecifierNotInFormat {
        /// The semantic specifier
        spec: String,
    },

    /// The new date passed to a `next` call was *before* the date represented by the current
    /// version.
    #[error("new date ({new_date}) is before current date ({current_date})")]
    NewDateBeforeCurrentDate {
        /// The new date
        new_date: Date,
        /// The current date
        current_date: Date,
    },
}

/// An error that occurred while parsing a format string.
#[non_exhaustive]
#[derive(thiserror::Error, Debug, PartialEq)]
pub enum FormatError {
    /// The specifier is not terminated with a closing square bracket (`]`).
    #[error(
        "specifier in format should be terminated with a closing square bracket (`]`), got `{pattern}`"
    )]
    UnterminatedSpecifier {
        /// The unterminated specifier string
        pattern: String,
    },

    /// The specifier is not a valid specifier for the scheme
    #[error("specifier `{spec}` is not valid in {scheme_name} format")]
    UnacceptableSpecifier {
        /// The specifier
        spec: String,
        /// The scheme name
        scheme_name: &'static str,
    },

    /// Two adjacent specifiers were not decreasing or decreased by more than one "step". In other
    /// words, the specifiers are not in the correct order of significance.
    ///
    /// # Examples
    ///
    /// - `<MAJOR><MAJOR>`: not decreasing
    /// - `<MAJOR><MINOR><PATCH><MINOR>`: last minor does not decrease     
    /// - `<YYYY><DD>`: decreasing by more than one step (days are only relative to months)
    #[error("specifiers must step decrease by their significance, got `{next}` after `{prev}`")]
    SpecifiersMustStepDecrease {
        /// The first specifier
        prev: String,
        /// The second specifier
        next: String,
    },

    /// The first specifier in a format is not allowed to be there
    #[error(
        "in {scheme_name} format, first specifier should be {expected_first}, got `{first_spec}`"
    )]
    WrongFirstSpecifier {
        /// The specifier
        first_spec: String,
        /// The scheme name
        scheme_name: &'static str,
        /// A (possibly comma-separated) list of expected specifiers
        expected_first: String,
    },

    /// The last specifier in a format does not complete the format
    #[error(
        "in {scheme_name} format, last specifier should be {expected_last}, got `{last_spec}`"
    )]
    Incomplete {
        /// The last specifier
        last_spec: String,
        /// The scheme name
        scheme_name: &'static str,
        /// A (possibly comma-separated) list of expected specifiers
        expected_last: String,
    },

    /// The format string should contain at least one specifier
    #[error("format should contain at least one specifier")]
    NoSpecifiersInFormat,
}

/// Errors around dates, as used to update [Cal](crate::Cal) and [CalSem](crate::CalSem) versions.
#[non_exhaustive]
#[derive(thiserror::Error, Debug, PartialEq)]
pub enum DateError {
    /// Arguments to [Date::explicit] do not represent a valid date.
    #[error("year ({year}), month ({month}), and day ({day}) should represent a valid date")]
    InvalidDateArguments {
        /// The year value
        year: i32,
        /// The month value
        month: u32,
        /// The day value
        day: u32,
    },

    /// The date string could not be parsed.
    ///
    /// See [chrono::NaiveDate::from_str] and [chrono::ParseError].
    #[error("{0}, date should be parseable")]
    UnparseableDate(#[from] chrono::ParseError),
}
