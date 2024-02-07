use crate::{
    error::{CompositeError, FormatError},
    format::Format,
    specifier::{CalSemIncrSpecifier, CalSemSpecifier, CalSpecifier, SemSpecifier, Specifier},
    version::{Date, Version},
};

pub(crate) mod priv_trait {
    use super::Specifier as SpecifierT;
    use core::fmt::Debug;

    pub(crate) trait Scheme: Sized + Debug + PartialEq + Eq {
        /// The kinds of specifiers this scheme uses
        type Specifier: SpecifierT;

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

        /// The specifiers that can be used as the first specifier in a format string, comma
        /// separated, for use in error messages.
        fn first_variants_string() -> String {
            arr_to_english_or(Self::Specifier::first_variants())
        }

        /// The specifiers that can be used as the last specifier in a format string, comma
        /// separated, for use in error messages.
        fn last_variants_string() -> String {
            arr_to_english_or(Self::Specifier::last_variants())
        }
    }

    // fn arr_to_english_or(specs: &[&impl Specifier]) -> String {
    fn arr_to_english_or(specs: &'static [&'static impl SpecifierT]) -> String {
        let backtick = |s: &str| format!("`{}`", s);
        let spec_strings = specs
            .iter()
            .map(|spec| spec.to_string())
            .collect::<Vec<_>>();
        match spec_strings.len() {
            0 => String::new(),
            1 => backtick(&spec_strings[0]),
            2 => format!(
                "{} or {}",
                backtick(&spec_strings[0]),
                backtick(&spec_strings[1])
            ),
            _ => {
                let mut joined = spec_strings[..spec_strings.len() - 1]
                    .iter()
                    .map(|s| backtick(s))
                    .collect::<Vec<_>>()
                    .join(", ");
                joined.push_str(&format!(
                    ", or {}",
                    backtick(&spec_strings[spec_strings.len() - 1])
                ));
                joined
            }
        }
    }
}

///
#[allow(private_bounds)]
pub trait Scheme: priv_trait::Scheme {
    /// Create a new format from a format string for this scheme.
    ///
    /// This is equivalent to [Format::parse].
    fn new_format(format_str: &str) -> Result<Format<Self>, FormatError> {
        Format::parse(format_str)
    }

    /// Returns a human readable name of the scheme for error messages.
    fn name() -> &'static str;

    /// Parses a version string against a format string, and returns a [Version] object if the
    /// version string matches the format string. Otherwise, returns a [NextverError].
    ///
    /// This is a convenience method that creates a temporary [Format] object with
    /// [Scheme::new_format] and parses the version string against it with [Format::parse_version].
    ///
    /// Returns a result of [Version] or [CompositeError] if either of the format or version
    /// creations fail.
    fn new_version<'vs>(
        format_str: &str,
        version_str: &'vs str,
    ) -> Result<Version<'vs, Self>, CompositeError> {
        let format = Self::new_format(format_str)?;
        let version = format.new_version(version_str)?;
        Ok(version)
    }

    /// Returns Ok(`true`) if the given version string is valid for the given format string, or else
    /// Ok(`false`). Returns an error if the format string could not be parsed.
    ///
    /// This is a convenience method that creates a temporary [Format] object with
    /// [Scheme::new_format] and validates that the result of [Format::parse_version] is ok.
    ///
    /// Returns a result of [bool] or [FormatError] if either of the format creation fails.
    fn is_valid(format_str: &str, version_str: &str) -> Result<bool, FormatError> {
        let format = Self::new_format(format_str)?;
        let version = format.new_version(version_str);
        Ok(version.is_ok())
    }
}

/// Scheme for formats that have only semantic specifiers, such as `[MAJOR].[MINOR].[PATCH]`.
#[derive(Debug, PartialEq, Eq)]
pub struct Sem;

impl Sem {
    /// Increments the version string (formatted by the format string) by the given semantic
    /// specifier and returns the new version's string.
    ///
    /// See [Version::next](struct.Version.html#impl-Version<'vs,+Sem>).
    ///
    /// This is a convenience method that creates a temporary [Version] with [Scheme::new_version],
    /// and increments it with [Version::next].
    pub fn next_version_string(
        format_str: &str,
        version_str: &str,
        specifier: &SemSpecifier,
    ) -> Result<String, CompositeError> {
        let version = Self::new_version(format_str, version_str)?;
        let next_version = version.next(specifier)?;
        Ok(next_version.to_string())
    }
}

impl Scheme for Sem {
    fn name() -> &'static str {
        "semantic"
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
#[derive(Debug, PartialEq, Eq)]
pub struct Cal;

impl Cal {
    /// Increments the version string (formatted by the format string) by the given date and returns
    /// the new version's string.
    ///
    /// See [Version::next](struct.Version.html#impl-Version<'vs,+Cal>).
    ///
    /// This is a convenience method that creates a temporary [Version] with [Scheme::new_version],
    /// and increments it with [Version::next].
    pub fn next_version_string(
        format_str: &str,
        version_str: &str,
        date: &Date,
    ) -> Result<String, CompositeError> {
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
#[derive(Debug, PartialEq, Eq)]
pub struct CalSem;

impl CalSem {
    /// Increments the version string (formatted by the format string) by the given date and
    /// semantic specifier, and returns the new version's string.
    ///
    /// See [Version::next](struct.Version.html#impl-Version<'vs,+CalSem>).
    ///
    /// This is a convenience method that creates a temporary [Version] with [Scheme::new_version],
    /// and increments it with [Version::next].
    pub fn next_version_string(
        format_str: &str,
        version_str: &str,
        date: &Date,
        semantic_specifier: &CalSemIncrSpecifier,
    ) -> Result<String, CompositeError> {
        let format = Self::new_format(format_str)?;
        let version = Version::parse(version_str, &format)?;
        let next_version = version.next(date, semantic_specifier)?;
        Ok(next_version.to_string())
    }
}

impl Scheme for CalSem {
    fn name() -> &'static str {
        "calendar-semantic"
    }
}

impl priv_trait::Scheme for CalSem {
    type Specifier = CalSemSpecifier;

    fn max_specifiers() -> usize {
        // longest exemplar is [YYYY][MM][DD][MINOR][PATCH]
        5
    }
}
