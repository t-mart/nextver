use crate::{
    error::{CompositeError, FormatError},
    format::Format,
    specifier::{CalSemLevel, CalSemSpecifier, CalSpecifier, SemSpecifier, Specifier},
    version::{Date, Version},
    SemLevel,
};

pub(crate) mod priv_trait {
    use super::Specifier as SpecifierT;
    use core::fmt::Debug;

    /// A private trait that is implemented by the public [`super::Scheme`] trait. This is used to
    /// define methods that are only meant to be used internally, and not by the user.
    pub(crate) trait Scheme: Sized + Debug + PartialEq + Eq {
        /// The kinds of specifiers this scheme uses
        type Specifier: SpecifierT;

        /// The maximum number of specifiers that can be in a format_string. For a given scheme,
        /// this should equal the largest number of specifiers that can be in a valid format.
        ///
        /// See [`Scheme::MAX_TOKENS`].
        const MAX_SPECIFIERS: usize;

        /// The maximum number of tokens that can be in a [`Format`] or [`Version`]. To account for
        /// literals being around the specifiers, this is equal to the maximum number of specifiers
        /// times 2, plus 1. Think fenceposts.
        ///
        /// This is useful for pre-allocating a vector to hold the tokens.
        ///
        /// See [`Scheme::MAX_SPECIFIERS`].
        const MAX_TOKENS: usize = Self::MAX_SPECIFIERS * 2 + 1;

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

        /// Returns a human readable name of the scheme for error messages.
        fn name() -> &'static str;
    }

    fn arr_to_english_or(specs: &'static [&'static impl SpecifierT]) -> String {
        let spec_strings = specs
            .iter()
            .map(|spec| format!("`{spec}`"))
            .collect::<Vec<_>>();
        match spec_strings.as_slice() {
            [] => String::new(),
            [a] => a.to_string(),
            [a, b] => format!("{a} or {b}"),
            [firsts @ .., last] => {
                let mut joined = firsts.join(", ");
                joined.push_str(&format!(", or {}", last));
                joined
            }
        }
    }
}

/// A trait for versioning schemes, which dictate the kinds of specifiers/values allowed in
/// formats/versions and the rules for incrementing them.
#[allow(private_bounds)]
pub trait Scheme: priv_trait::Scheme {
    /// Parse a format string containing specifier and literal tokens into a [`Format`].
    /// 
    /// The format string is made up of specifiers and literals. Specifiers indicate numeric values
    /// that can change, while literals are fixed text.
    /// 
    /// See the specifier table [here](crate#table) for a list of all specifiers, which
    /// appear as `<...>` in the format string. To escape a literal `<`, use `<<`. `>` must not be
    /// escaped.
    ///
    /// # Example
    ///
    /// ```
    /// use nextver::prelude::*;
    ///
    /// let format_str = "<YYYY>.<MM>.<PATCH>";
    /// let format = CalSem::new_format(format_str)?;
    /// assert_eq!(format_str, &format.to_string());
    /// # Ok::<(), Box<dyn std::error::Error>>(())
    /// ```
    ///
    /// # Errors
    ///
    /// Returns an `Err` of one of the following [`FormatError`] variants:
    ///
    /// - [`FormatError::UnterminatedSpecifier`] if an open bracket is not closed with a closing
    ///   bracket.
    /// - [`FormatError::UnacceptableSpecifier`] if a specifier is not known to the scheme.
    /// - [`FormatError::SpecifiersMustStepDecrease`] if specifiers are not in order. See each
    ///   scheme's documentation for more details.
    /// - [`FormatError::WrongFirstSpecifier`] if the first specifier is not acceptable for the
    ///   scheme.
    /// - [`FormatError::Incomplete`] if the last specifier is not acceptable for the scheme.
    /// - [`FormatError::NoSpecifiersInFormat`] if there are no specifiers in the format.
    fn new_format(format_str: &str) -> Result<Format<Self>, FormatError> {
        Format::parse(format_str)
    }

    /// Parses a version string against a format string, and returns a [`Version`] object if the
    /// version string matches the format string. Otherwise, returns a
    /// [`NextError`](crate::NextError).
    ///
    /// This is a convenience method that creates a temporary [`Format`] object with
    /// [`Scheme::new_format`] and parses the version string against it with
    /// [`Format::new_version`].
    ///
    /// Note: For calendar schemes, the values in `version_str` are *not* validated to be actual
    /// dates. For example, `2021.02.31` is valid for the format `<YYYY>.<MM>.<DD>`, even though
    /// February 31st does not exist.
    ///
    /// Returns a result of [`Version`] or [`CompositeError`] if either of the format or version
    /// operations fail.
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
    /// Returns a result of [`bool`] or [`FormatError`] if either of the format creation fails.
    fn is_valid(format_str: &str, version_str: &str) -> Result<bool, FormatError> {
        let format = Self::new_format(format_str)?;
        let version = format.new_version(version_str);
        Ok(version.is_ok())
    }
}

/// Scheme for formats that have only semantic specifiers, such as `<MAJOR>.<MINOR>.<PATCH>`.
///
/// Sem behaves almost exactly like the [SemVer](https://semver.org/) scheme, but with a few
/// differences.
///
/// See the available specifiers for this scheme in the [table](crate#table).
///
/// # Rules
///
/// - The first specifier must be `MAJOR`.
/// - `MINOR` and `PATCH` are not required. If `MINOR` is present, it must be after `MAJOR`, and if
///   `PATCH` is present, it must be after `MINOR`.
/// - As for all schemes, arbitrary literals can be placed in the format string. For example, dots,
///   hyphens, or any other character(s) can be used, such as `v<MAJOR>#<MINOR>-p<PATCH>`.
///
/// # Example Formats
///
/// - `<MAJOR>.<MINOR>.<PATCH>`: Major, minor, and patch. Dot-separated.
/// - `v<MAJOR>.<MINOR>`: `v` followed by major and minor. Dot-separated.
#[derive(Debug, PartialEq, Eq)]
pub struct Sem;

impl Sem {
    /// Increments the version string (formatted by the format string) by the given semantic
    /// specifier and returns the new version's string.
    ///
    /// This is a convenience method that creates a temporary [`Format`] and [`Version`] with
    /// [`Scheme::new_version`], and increments it with
    /// [`Version::next`](struct.Version.html#method.next).
    ///
    /// # Example
    ///
    /// ```
    /// use nextver::prelude::*;
    ///
    /// let next_str = Sem::next_version_string(
    ///   "<MAJOR>.<MINOR>.<PATCH>",
    ///   "1.2.3",
    ///    &SemLevel::Minor
    /// ).unwrap();
    ///
    /// assert_eq!("1.3.0", next_str);
    /// ```
    ///
    /// # Errors
    ///
    /// Returns a [`CompositeError`] of all error surface area from [`Self::new_version`] and
    /// [`Version::next`](struct.Version.html#method.next).
    pub fn next_version_string(
        format_str: &str,
        version_str: &str,
        level: &SemLevel,
    ) -> Result<String, CompositeError> {
        let version = Self::new_version(format_str, version_str)?;
        let next_version = version.next(level)?;
        Ok(next_version.to_string())
    }
}

impl Scheme for Sem {}

impl priv_trait::Scheme for Sem {
    type Specifier = SemSpecifier;

    // longest exemplar is <MAJOR><MINOR><PATCH>
    const MAX_SPECIFIERS: usize = 3;

    fn name() -> &'static str {
        "semantic"
    }
}

/// Scheme for formats that have only calendar specifiers, such as `<YYYY>.<MM>.<DD>`.
///
/// This scheme is less useful than [`CalSem`] because there is no way to increment it twice in the
/// same period of its least significant specifier. For example, a version with format
/// `<YYYY>.<MM>.<DD>` can only be incremented/updated once per day.
///
/// See the available specifiers for this scheme in the [table](crate#table).
///
/// # Rules
///
/// - The first specifier must be a year (`YYYY`, `YY`, or `0Y`).
/// - For adjacent specifiers `a` and `b`, `b` must be relative to `a`:
///   - month specifiers are relative to year ones (e.g., `<YYYY>.<MM>`)
///   - day specifiers are relative to month ones (e.g., `<YYYY>.<MM>.<DD>`)
///   - week specifiers are relative to year ones (and *not month ones*) (e.g., `<YYYY>.<WW>`)
/// - As for all schemes, arbitrary literals can be placed in the format string. For example, dots,
///   hyphens, or any other character(s) can be used, such as `y<YYYY>m<MM>d<DD>`.
///
/// # Example Formats
///
/// - `<YYYY>.<0M>.<0D>`: Full year, zero-padded month, and zero-padded day. Dot-separated.
/// - `<0Y>.<0M>.<0D>`: Zero-padded year, zero-padded month, and zero-padded day. Dot-separated.
/// - `<YYYY>-<0W>`: Full year and zero-padded week. Hyphen-separated.
#[derive(Debug, PartialEq, Eq)]
pub struct Cal;

impl Cal {
    /// Increments the version string (formatted by the format string) by the given date and returns
    /// the new version's string.
    ///
    /// This is a convenience method that creates a temporary [`Format`] and [`Version`] with
    /// [`Scheme::new_version`], and increments it with
    /// [`Version::next`](struct.Version.html#method.next-1).
    ///
    /// # Example
    ///
    /// ```
    /// use nextver::prelude::*;
    ///
    /// let date = Date::utc_now(); // assume today is 2024-02-23
    /// # let date = Date::explicit(2024, 2, 23).unwrap();
    ///
    /// let next_str = Cal::next_version_string(
    ///   "<YYYY>.<0M>.<0D>",
    ///   "2001.02.03",
    ///   &date
    /// ).unwrap();
    ///
    /// assert_eq!("2024.02.23", next_str);
    /// ```
    ///
    /// # Errors
    ///
    /// Returns a [`CompositeError`] of all error surface area from [`Self::new_version`] and
    /// [`Version::next`](struct.Version.html#method.next-1).
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

impl Scheme for Cal {}

impl priv_trait::Scheme for Cal {
    type Specifier = CalSpecifier;

    // longest exemplar is <YYYY><MM><DD>
    const MAX_SPECIFIERS: usize = 3;

    fn name() -> &'static str {
        "calendar"
    }
}

/// Scheme for formats that have both calendar and semantic specifiers, such as
/// `<YYYY>.<MM>.<PATCH>`.
///
/// You would have such a format if you want to be able to increase your version multiple times
/// within the period of your smallest calendar specifier, such a second time in the same day.
///
/// See the available specifiers for this scheme in the [table](crate#table).
///
/// # Rules
///
/// - The first specifier must be a year (`YYYY`, `YY`, or `0Y`).
/// - For adjacent *calendar* specifiers `a` and `b`, `b` must be relative to `a`:
///   - month specifiers are relative to year ones (e.g., `<YYYY>.<MM>`)
///   - day specifiers are relative to month ones (e.g., `<YYYY>.<MM>.<DD>`)
///   - week specifiers are relative to year ones (and *not month ones*) (e.g., `<YYYY>.<WW>`)
/// - The format must end with the `PATCH` semantic specifier.
///   - `MINOR` may optionally come before `PATCH` if more granularity is desired.
/// - As for all schemes, arbitrary literals can be placed in the format string. For example, dots,
///   hyphens, or any other character(s) can be used, such as `y<YYYY>m<MM>d<DD>-p<PATCH>`.
///
/// # Example Formats
///
/// - `<YYYY>.<0M>.<0D>.<PATCH>`: Full year, zero-padded month, zero-padded day, and patch.
///   Dot-separated.
/// - `<0Y>.<0M>.<0D>.<PATCH>`: Zero-padded year, zero-padded month, zero-padded day, and patch.
///   Dot-separated.
/// - `<YYYY>.<0W>-<MINOR>.<PATCH>`: Full year, zero-padded week, minor, and patch. Dot- and
///   hyphen-separated.
#[derive(Debug, PartialEq, Eq)]
pub struct CalSem;

impl CalSem {
    /// Increments the version string (formatted by the format string) by the given date and
    /// semantic specifier, and returns the new version's string.
    ///
    /// This is a convenience method that creates a temporary [`Format`] and [`Version`] with
    /// [`Scheme::new_version`], and increments it with
    /// [`Version::next`](struct.Version.html#method.next-2).
    ///
    /// # Example
    ///
    /// ```
    /// use nextver::prelude::*;
    ///
    /// let date = Date::utc_now(); // assume today is 2024-02-23
    /// # let date = Date::explicit(2024, 2, 23).unwrap();
    ///
    /// let next_str = CalSem::next_version_string(
    ///   "<YYYY>.<0M>.<PATCH>",
    ///   "2024.01.42",
    ///   &date,
    ///   &CalSemLevel::Patch
    /// ).unwrap();
    ///
    /// assert_eq!("2024.02.0", next_str);
    /// ```
    ///
    /// # Errors
    ///
    /// Returns a [`CompositeError`] of all error surface area from [`Self::new_version`] and
    /// [`Version::next`](struct.Version.html#method.next-2).
    pub fn next_version_string(
        format_str: &str,
        version_str: &str,
        date: &Date,
        level: &CalSemLevel,
    ) -> Result<String, CompositeError> {
        let format = Self::new_format(format_str)?;
        let version = Version::parse(version_str, &format)?;
        let next_version = version.next(date, level)?;
        Ok(next_version.to_string())
    }
}

impl Scheme for CalSem {}

impl priv_trait::Scheme for CalSem {
    type Specifier = CalSemSpecifier;

    // longest exemplar is <YYYY><MM><DD><MINOR><PATCH>
    const MAX_SPECIFIERS: usize = 5;

    fn name() -> &'static str {
        "calendar-semantic"
    }
}
