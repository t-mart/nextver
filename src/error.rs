use crate::scheme::{Cal, CalSem};

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

    /// When updating a [Cal] version, the date provided yielded an identical version.
    #[error("date provided yielded an identical version")]
    NoCalendarChange,

    /// When updating a [Cal] or [CalSem] version, the date year was negative.
    #[error("year `{year}` should not be negative when formatted`")]
    NegativeYearValue {
        /// The year value
        year: i32,
    },

    /// When updating/incrementing a [Sem] or [CalSem] version, the semantic specifier level is not
    /// in the format.
    #[error("`{spec}` was not found in format, use one that is")]
    SemanticSpecifierNotInFormat {
        /// The semantic specifier
        spec: String,
    },

    /// The date passed to a [Cal] or [CalSem] version was invalid.
    #[error("explicit year ({year}), month ({month}), and day ({day}) arguments cannot be made into a valid date")]
    InvalidDateArguments {
        /// The year value
        year: i32,
        /// The month value
        month: u32,
        /// The day value
        day: u32,
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

    /// Two adjacent specifiers were not decreasing or decreased by more than one "step". In other
    /// words, the specifiers are not in the correct order of significance.
    /// 
    /// # Examples
    /// 
    /// - `[MAJOR][MAJOR]`: not decreasing
    /// - `[MAJOR][MINOR][PATCH][MINOR]`: last minor does not decrease     
    /// - `[YYYY][DD]`: decreasing by more than one step (days are only relative to months)
    #[error("specifiers must step decrease by their significance, got `{next}` after `{prev}`")]
    SpecifiersMustStepDecrease {
        /// The first specifier
        prev: String,
        /// The second specifier
        next: String,
    },

    /// The first specifier in a format is not allowed to be there
    #[error("in {scheme_name} format, first specifier should be {expected_first}, got `{spec}`")]
    WrongFirstSpecifier {
        /// The specifier
        spec: String,
        /// The scheme name
        scheme_name: &'static str,
        /// A (possibly comma-separated) list of expected specifiers
        expected_first: String,
    },

    /// In [CalSem] formats, the calendar specifiers must precede the semantic specifiers
    #[error("all calendar specifiers should precede all semantic specifiers in {scheme_name} format, got `{next}` after `{prev}`")]
    CalenderMustPrecedeSemantic {
        /// The first specifier
        prev: String,
        /// The second specifier
        next: String,
        /// The scheme name
        scheme_name: &'static str,
    },

    /// The specifier is not a valid specifier for the scheme
    #[error("specifier `{spec}` is not valid in {scheme_name} format")]
    UnacceptableSpecifier {
        /// The specifier
        spec: String,
        /// The scheme name
        scheme_name: &'static str,
    },

    /// The last specifier in a format is not allowed to be there
    #[error("{scheme_name} format should end with a semantic specifier, got `{last_spec}`")]
    FormatIncomplete {
        /// The last specifier
        last_spec: String,
        /// The scheme name
        scheme_name: &'static str,
    },

    /// The format string should contain at least one specifier
    #[error("format should contain at least one specifier")]
    NoSpecifiersInFormat,
}
