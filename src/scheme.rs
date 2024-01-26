use crate::{
    error::NextverError,
    format::Format,
    specifier::{CalSemSpecifier, CalSpecifier, SemSpecifier, Specifier, CalSemIncrSpecifier},
    version::{Date, Version},
};
use core::fmt::Debug;

pub(crate) mod priv_trait {
    use super::*;
    pub(crate) trait Scheme: Sized + Debug + PartialEq + Eq + Clone + Copy {
        /// The kinds of specifiers this scheme uses
        type Specifier: Specifier;

        /// The maximum number of specifiers that can be in a format_string. For a given scheme,
        /// this should equal the largest number of specifiers that can be in a valid format.
        ///
        /// See [Scheme::token_capacity].
        fn max_specifiers() -> usize;

        /// The maximum number of tokens that can be in a [Format] or [Version]. To account for
        /// literals being around the specifiers, this is equal to the maximum number of specifiers
        /// times 2, plus 1. Think fenceposts.
        ///
        /// This is useful for pre-allocating a vector to hold the tokens.
        ///
        /// See [Scheme::max_specifiers].
        fn max_tokens() -> usize {
            Self::max_specifiers() * 2 + 1
        }
    }
}

#[allow(private_bounds)]
pub trait Scheme: priv_trait::Scheme {
    /// Create a new format from a format string for this scheme.
    fn new_format(format_str: &str) -> Result<Format<Self>, NextverError>;

    /// Returns a human readable name of the scheme for error messages.
    fn name() -> &'static str;

    /// Parses a version string against a format string, and returns a [Version] object if the
    /// version string matches the format string. Otherwise, returns a [NextverError].
    ///
    /// This is a convenience method that creates a temporary [Format] object and parses the version
    /// string against it.
    fn new_version<'vs>(
        format_str: &str,
        version_str: &'vs str,
    ) -> Result<Version<'vs, Self>, NextverError> {
        let format = Self::new_format(format_str)?;
        let version = Version::parse(version_str, &format)?;
        Ok(version)
    }

    /// Returns Ok(`true`) if the given version string is valid for the given format string, or else
    /// Ok(`false`). Returns an error if the format string could not be parsed.
    ///
    /// This is a convenience method that creates a temporary [Format] object and validates that the
    /// version string matches it.
    fn is_valid(
        format_str: &str,
        version_str: &str,
    ) -> Result<bool, NextverError> {
        let format = Self::new_format(format_str)?;
        let version = Version::parse(version_str, &format);
        Ok(version.is_ok())
    }
}

/// Scheme for formats that have only semantic specifiers, such as `[MAJOR].[MINOR].[PATCH]`.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Sem;

impl Sem {
    pub fn next(
        format_str: &str,
        version_str: &str,
        specifier: &SemSpecifier,
    ) -> Result<String, NextverError> {
        let format = Self::new_format(format_str)?;
        let version = Version::parse(version_str, &format)?;
        let next_version = version.next(specifier)?;
        Ok(next_version.to_string())
    }
}

impl Scheme for Sem {
    fn name() -> &'static str {
        "semantic"
    }

    fn new_format(format_str: &str) -> Result<Format<Self>, NextverError> {
        Format::parse(format_str)
    }
}

impl priv_trait::Scheme for Sem {
    type Specifier = SemSpecifier;

    fn max_specifiers() -> usize {
        // longest exemplar is [MAJOR][MINOR][PATCH]
        3
    }
}

/// Scheme for formats that have only calendar specifiers, such as `[YYYY].[MM].[DD]`.
///
/// This scheme is less useful than [CalSem] because there is no way to increment it twice in the
/// same period of its least significant specifier. For example, a version with format
/// `[YYYY].[MM].[DD]` can only be incremented/updated once per day.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct Cal;

impl Cal {
    pub fn next(format_str: &str, version_str: &str, date: &Date) -> Result<String, NextverError> {
        let format = Self::new_format(format_str)?;
        let version = Version::parse(version_str, &format)?;
        let next_version = version.next(date)?;
        Ok(next_version.to_string())
    }
}

impl Scheme for Cal {
    fn name() -> &'static str {
        "calendar"
    }

    fn new_format(format_str: &str) -> Result<Format<Self>, NextverError> {
        Format::parse(format_str)
    }
}

impl priv_trait::Scheme for Cal {
    type Specifier = CalSpecifier;

    fn max_specifiers() -> usize {
        // longest exemplar is [YYYY][MM][DD]
        3
    }
}

/// Scheme for formats that have both calendar and semantic specifiers, such as
/// `[YYYY].[MM].[DD].[PATCH]`.
///
/// You would have such a format if you want to be able to increase
/// your version multiple times within the period of your smallest calendar specifier, such a
/// second time in the same day, continuing with the previous example.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub struct CalSem;

impl CalSem {
    pub fn next(
        format_str: &str,
        version_str: &str,
        date: &Date,
        semantic_specifier: &CalSemIncrSpecifier,
    ) -> Result<String, NextverError> {
        let format = Self::new_format(format_str)?;
        let version = Version::parse(version_str, &format)?;
        let next_version = version.next(date, semantic_specifier)?;
        Ok(next_version.to_string())
    }
}

impl CalSem {
    pub fn new_format(format_str: &str) -> Result<Format<Self>, NextverError> {
        Format::parse(format_str)
    }
}

impl Scheme for CalSem {
    fn name() -> &'static str {
        "calendar-semantic"
    }

    fn new_format(format_str: &str) -> Result<Format<Self>, NextverError> {
        Format::parse(format_str)
    }
}

impl priv_trait::Scheme for CalSem {
    type Specifier = CalSemSpecifier;

    fn max_specifiers() -> usize {
        // longest exemplar is [YYYY][MM][DD][MINOR][PATCH]
        5
    }
}
