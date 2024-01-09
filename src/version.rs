use chrono::{Datelike, Local, NaiveDate, Utc};
use core::fmt;
use std::borrow::Cow;
use std::cmp::Ordering;

use crate::{format::FormatToken, specifier::*, Format, VersionBumpError};

/// Ways to specify a reference date.
pub enum Date {
    /// Use the current date in UTC, as determined when this variant is used.
    UtcNow,
    /// Use the current date in the local timezone, as determined when this variant is used.
    LocalNow,
    /// Use the given date.
    Explicit { year: i32, month: u32, day: u32 },
}

impl Date {
    fn get_date(&self) -> Result<NaiveDate, VersionBumpError> {
        match self {
            Self::UtcNow => Ok(Utc::now().date_naive()),
            Self::LocalNow => Ok(Local::now().date_naive()),
            Self::Explicit { year, month, day } => NaiveDate::from_ymd_opt(*year, *month, *day)
                .ok_or(VersionBumpError::InvalidDateArguments {
                    year: *year,
                    month: *month,
                    day: *day,
                }),
        }
    }
}

impl<T: Datelike> From<T> for Date {
    fn from(datelike: T) -> Self {
        Date::Explicit {
            year: datelike.year(),
            month: datelike.month(),
            day: datelike.day(),
        }
    }
}

#[derive(Debug)]
pub(crate) enum VersionToken {
    Value {
        value: u32,
        spec: &'static Specifier,
    },
    Fixed(String),
}

impl PartialEq for VersionToken {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                VersionToken::Value {
                    value: self_value,
                    spec: self_spec,
                },
                VersionToken::Value {
                    value: other_value,
                    spec: other_spec,
                },
            ) => std::ptr::eq(*self_spec, *other_spec) && self_value == other_value,
            (VersionToken::Fixed(self_text), VersionToken::Fixed(other_text)) => {
                self_text == other_text
            }
            // if the tokens are different types, we can't compare them
            _ => false,
        }
    }
}

impl PartialOrd for VersionToken {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (
                VersionToken::Value {
                    value: self_value,
                    spec: self_spec,
                },
                VersionToken::Value {
                    value: other_value,
                    spec: other_spec,
                },
            ) => {
                if !std::ptr::eq(*self_spec, *other_spec) {
                    return None;
                }
                self_value.partial_cmp(other_value)
            }
            (VersionToken::Fixed(self_text), VersionToken::Fixed(other_text)) => {
                self_text.partial_cmp(other_text)
            }
            // if the tokens are different types, we can't compare them
            _ => None,
        }
    }
}

impl From<&VersionToken> for FormatToken {
    fn from(val: &VersionToken) -> Self {
        match val {
            VersionToken::Value { spec, .. } => FormatToken::Specifier(spec),
            VersionToken::Fixed(text) => FormatToken::Literal(text.clone()),
        }
    }
}

/// A Version object represents the parsing of a version string against a [Format], where specifier
/// text is converted into numeric values. Versions can be displayed, incremented, and compared.
///
/// Note that Version objects only implement a partial ordering. This is because the ordering
/// only makes sense when they have the same format. Therefore, inequality (and equality)
/// comparisons between versions with different formats will always return `false`.
///
/// # Examples
///
/// Quick start:
///
/// ```
/// use version_bump::{Version, VersionBumpError, SemanticLevel};
///
/// let version = Version::from_parsed_format("[MAJOR].[MINOR].[PATCH]", "1.2.3").unwrap();
/// let incremented = version.increment(Some(&SemanticLevel::Minor), None).unwrap();
/// assert_eq!("1.3.0", incremented.to_string());
/// assert!(version < incremented);
///
/// let invalid = Version::from_parsed_format("[MAJOR].[MINOR].[PATCH]", "1.foo.3");
/// assert!(matches!(invalid, Err(VersionBumpError::VersionFormatMismatch)));
/// ```
///
/// Or, use a previously created [Format] object:
///
/// ```
/// use version_bump::{Format, Version};
///
/// let format = Format::parse("[MAJOR].[MINOR].[PATCH]").unwrap();
/// let version = Version::parse("1.2.3", format.clone());
/// assert!(version.is_ok());
/// ```
///
/// You can increment by semantic level, calendar date, or both:
///
/// ```
/// use version_bump::{Version, SemanticLevel, Date};
///
/// // Mix and match specifiers
/// let version = Version::from_parsed_format("[YYYY].[PATCH]", "2023.123").unwrap();
///
/// // Increment by semantic level
/// let incremented = version.increment(Some(&SemanticLevel::Patch), None).unwrap();
/// assert_eq!("2023.124", incremented.to_string());
/// assert!(version < incremented);
///
/// // Increment by date
/// let date = Date::Explicit { year: 2024, month: 2, day: 3 };
/// let incremented = version.increment(None, Some(&date)).unwrap();
/// assert_eq!("2024.123", incremented.to_string());
/// assert!(version < incremented);
///
/// // Increment by both
/// let incremented = version.increment(Some(&SemanticLevel::Patch), Some(&date)).unwrap();
/// assert_eq!("2024.124", incremented.to_string());
/// assert!(version < incremented);
/// ```
#[derive(Debug, PartialEq, PartialOrd)]
pub struct Version {
    pub(crate) tokens: Vec<VersionToken>,
}

impl Version {
    /// Parses a version string against a [Format], and returns a [Version] object if the version
    /// string matches the format. Otherwise, returns a [VersionBumpError].
    ///
    /// If you just need a one-off parse or validation, you can use [Version::from_parsed_format] or
    /// [Version::is_valid] instead, which create [Format] objects implicitly.
    ///
    /// Note that calendar specifier values are not validated to be real dates. For example,
    /// `2021.2.30` will parse, even though February 30th does not exist. This is because specifiers
    /// for the year, month, and day might not exist in the same format string, so a full date may
    /// not always be realizable. (However, when incrementing, the date provided will be validated.)
    ///
    /// # Errors
    ///
    /// - If the version string does not match the format string, returns a
    ///   [VersionBumpError::VersionFormatMismatch].
    pub fn parse(version_str: &str, format: Format) -> Result<Self, VersionBumpError> {
        let Some(captures) = format.get_regex().captures(version_str) else {
            return Err(VersionBumpError::VersionFormatMismatch);
        };

        let mut tokens = Vec::new();

        // skip the first capture because it's the implicit group of the whole regex
        let group_captures = captures.iter().skip(1);

        for (match_, format_token) in group_captures.zip(&format.tokens) {
            let Some(match_) = match_ else {
                // would happen if the group was optional (e.g. `(\d)?`). we don't currently
                // construct our format regex this way, but just in case, plus we get a destructure
                // on the Option
                continue;
            };

            let text = match_.as_str().to_owned();

            let token = match format_token {
                FormatToken::Specifier(specifier) => {
                    let value = text.parse().unwrap();
                    VersionToken::Value {
                        value,
                        spec: specifier,
                    }
                }
                FormatToken::Literal(format_text) => {
                    if !text.eq(format_text) {
                        return Err(VersionBumpError::VersionFormatMismatch);
                    }
                    VersionToken::Fixed(text)
                }
            };

            tokens.push(token);
        }

        Ok(Self { tokens })
    }

    /// Increments the version according to the given [SemanticLevel] and/or [Date].
    ///
    /// # Arguments
    ///
    /// - `semantic_level`: The semantic level to increment. The corresponding semantic specifier
    ///   must be present in the format string, or an error is returned.
    ///     - If `Some`, the semantic token of `semantic_level` is incremented and all semantic
    ///       tokens of lower levels are reset to zero (e.g. `1.2.3` -> `2.0.0`).
    ///     - If `None`, the semantic tokens in the format string are ignored.
    /// - `date`: The reference date to incrememnt calendar date tokens. If `None`, the calendar
    ///   tokens in the format string are ignored.
    ///
    /// ## Notes
    ///   
    ///   - Unlike semantic levels, if a date is provided, all calendar tokens are updated.
    ///   
    ///   - You can pass any arbitary date, even it if represents a date before that of the tokens
    ///     in the version string.
    ///
    ///   - It is always okay to provide a date, even if the format string does not contain calendar
    ///     specifiers.
    ///
    /// # Examples
    ///
    /// ```
    /// use version_bump::{Format, SemanticLevel, Version};
    ///
    /// let format = Format::parse("[MAJOR].[MINOR].[PATCH]").unwrap();
    /// let version = Version::parse("1.2.3", format).unwrap();
    /// let new_version = version.increment(Some(&SemanticLevel::Major), None).unwrap();
    /// assert_eq!("2.0.0", new_version.to_string());
    ///
    /// let new_version = version.increment(Some(&SemanticLevel::Patch), None).unwrap();
    /// assert_eq!("1.2.4", new_version.to_string());
    /// ```
    ///
    /// ```
    /// use version_bump::{Date, SemanticLevel, Format, Version};
    ///
    /// let format = Format::parse("[YYYY].[PATCH]").unwrap();
    /// let version = Version::parse("2023.1", format).unwrap();
    ///
    /// let date = Date::Explicit {
    ///    year: 2024,
    ///    month: 2,
    ///    day: 3,
    /// };
    ///
    /// let new_version = version.increment(None, Some(&date)).unwrap();
    /// assert_eq!("2024.1", new_version.to_string());
    ///
    /// let new_version = version.increment(Some(&SemanticLevel::Patch), None).unwrap();
    /// assert_eq!("2023.2", new_version.to_string());
    ///
    /// let new_version = version.increment(Some(&SemanticLevel::Patch), Some(&date)).unwrap();
    /// assert_eq!("2024.2", new_version.to_string());
    /// ```
    ///
    /// # Errors
    ///
    /// - If `semantic_level` is provided but the format string does not contain its respective
    ///   specifier, returns a [VersionBumpError::SemanticLevelSpecifierNotInFormat].
    ///
    /// - If `date` provided is a [Date::Explicit] and the date values is do not represent a real
    ///   date, returns a [VersionBumpError::InvalidDateArguments].
    ///
    ///  - Returns a [VersionBumpError::NegativeYearValue]...
    ///
    ///    - If the `date` provided is before year 0 and this version's format uses the `[YYYY]`
    ///      specifier.
    ///    - If the `date` provided is before the year 2000 and this version's format uses the `[YY]`
    ///      or `[0Y]` specifiers.
    ///
    ///    This is because the formatted values would be negative, which would affect parsing. [See
    ///    specifiers for more](struct.Format.html#specifier-table).
    pub fn increment(
        &self,
        semantic_level: Option<&SemanticLevel>,
        date: Option<&Date>,
    ) -> Result<Version, VersionBumpError> {
        if semantic_level.is_none() && date.is_none() {
            return Err(VersionBumpError::NothingToIncrement);
        }

        let naive_date = date.map(|date| date.get_date()).transpose()?;

        let mut new_tokens = Vec::new();
        let mut semantic_level_found = semantic_level.is_none();
        let mut semantic_already_bumped = false;

        for token in &self.tokens {
            let new_token = match token {
                VersionToken::Value { value, spec } => {
                    let incremented = 'incremented_block: {
                        match spec {
                            Specifier::Semantic {
                                level: cur_level,
                                incr_fn,
                                ..
                            } => {
                                if let Some(semantic_level) = semantic_level {
                                    if semantic_level >= cur_level {
                                        if semantic_level == cur_level {
                                            semantic_level_found = true;
                                        }
                                        let incremented = incr_fn(value, semantic_already_bumped);
                                        semantic_already_bumped = true;
                                        break 'incremented_block incremented;
                                    }
                                }
                            }
                            Specifier::Calendar { incr_fn, .. } => {
                                if let Some(naive_date) = naive_date {
                                    let incremented = incr_fn(&naive_date)?;
                                    break 'incremented_block incremented;
                                }
                            }
                        }
                        *value // value as is if we didn't break
                    };
                    VersionToken::Value {
                        value: incremented,
                        spec,
                    }
                }
                VersionToken::Fixed(text) => VersionToken::Fixed(text.clone()),
            };
            new_tokens.push(new_token);
        }

        if let Some(semantic_level) = semantic_level {
            if !semantic_level_found {
                return Err(VersionBumpError::SemanticLevelSpecifierNotInFormat {
                    name: semantic_level.name(),
                });
            }
        }

        let new_version = Version { tokens: new_tokens };

        if self.eq(&new_version) {
            return Err(VersionBumpError::NoChange);
        }

        Ok(new_version)
    }

    /// Parses a version string against a format string, and returns a [Version] object if the
    /// version string matches the format string. Otherwise, returns a [VersionBumpError].
    ///
    /// This is a convenience method that creates a temporary [Format] object and parses the version
    /// string against it.
    pub fn from_parsed_format(
        format_str: &str,
        version_str: &str,
    ) -> Result<Self, VersionBumpError> {
        let format = Format::parse(format_str)?;
        Self::parse(version_str, format)
    }

    /// Returns `true` if the given version string is valid for the given format string.
    ///
    /// This is a convenience method that creates a temporary [Format] object and validates the
    /// version string matches it.
    pub fn is_valid(format: &str, version: &str) -> bool {
        Self::from_parsed_format(format, version).is_ok()
    }

    /// Compare this version to another version string. This is a convenience method that parses
    /// the other version string against the format of this version.
    pub fn partial_cmp_with_string(&self, other_version_str: &str) -> Option<Ordering> {
        let other = Self::parse(other_version_str, self.into()).ok()?;
        self.partial_cmp(&other)
    }
}

impl fmt::Display for Version {
    /// Returns the rendered version string
    fn fmt<'a>(&'a self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for token in &self.tokens {
            let rendered: Cow<'a, String> = match token {
                VersionToken::Value { value, spec } => Cow::Owned(spec.format_value(value)),
                VersionToken::Fixed(text) => Cow::Borrowed(text),
            };
            write!(f, "{}", rendered)?
        }

        Ok(())
    }
}

/// Returns a new [Format] object equal to the one used to parse this version.
impl From<&Version> for Format {
    fn from(version: &Version) -> Self {
        Format::from_tokens(version.tokens.iter().map(|token| token.into()).collect())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // shows variance in all fields between zero-padded and non-zero-padded
    static DATE_2002_02_02: Date = Date::Explicit {
        year: 2002,
        month: 2,
        day: 2,
    };
    // before year 2000
    static DATE_1998_01_01: Date = Date::Explicit {
        year: 1998,
        month: 1,
        day: 1,
    };
    // before year 0
    static DATE_BCE: Date = Date::Explicit {
        year: -1,
        month: 1,
        day: 1,
    };
    // invalid
    static DATE_INVALID: Date = Date::Explicit {
        year: 2001,
        month: 2,
        day: 30,
    };

    #[test]
    fn test_parse_ok() {
        let args = [
            ("[MAJOR]", vec!["1", "2", "10"]),
            ("[MINOR]", vec!["1", "2", "10"]),
            ("[PATCH]", vec!["1", "2", "10"]),
            ("[YYYY]", vec!["2001", "2002", "2010", "100000"]),
            ("[YY]", vec!["1", "2", "10"]),
            ("[0Y]", vec!["01", "02", "10"]),
            ("[MM]", vec!["1", "2", "10"]),
            ("[0M]", vec!["01", "02", "10"]),
            ("[WW]", vec!["1", "2", "10"]),
            ("[0W]", vec!["01", "02", "10"]),
            ("[DD]", vec!["1", "2", "10"]),
            ("[0D]", vec!["01", "02", "10"]),
            (r"\[MAJOR]", vec!["[MAJOR]"]),
            (
                "The quick brown fox jumps over the lazy dog",
                vec!["The quick brown fox jumps over the lazy dog"],
            ),
        ];

        for (format, version_strs) in args {
            let format = Format::parse(format).unwrap();
            for version_str in version_strs {
                let version = Version::parse(version_str, format.clone());
                assert!(version.is_ok());
                let version = version.unwrap();
                assert_eq!(version.to_string(), version_str.to_string());
                assert_eq!(Into::<Format>::into(&version), format);
            }
        }
    }

    #[test]
    fn test_increment_ok() {
        let args = [
            (
                "[MAJOR].[MINOR].[PATCH]",
                "1.2.3",
                (Some(&SemanticLevel::Major), None),
                "2.0.0",
            ),
            (
                "[MAJOR].[MINOR].[PATCH]",
                "1.2.3",
                (Some(&SemanticLevel::Minor), None),
                "1.3.0",
            ),
            (
                "[MAJOR].[MINOR].[PATCH]",
                "1.2.3",
                (Some(&SemanticLevel::Patch), None),
                "1.2.4",
            ),
            (
                "[YYYY].[MM].[DD]",
                "2001.1.1",
                (None, Some(&DATE_2002_02_02)),
                "2002.2.2",
            ),
            (
                "[YYYY].[PATCH]",
                "2001.1",
                (None, Some(&DATE_2002_02_02)),
                "2002.1",
            ),
            (
                "[YYYY].[PATCH]",
                "2001.1",
                (Some(&SemanticLevel::Patch), None),
                "2001.2",
            ),
            (
                "[YYYY].[PATCH]",
                "2001.1",
                (Some(&SemanticLevel::Patch), Some(&DATE_2002_02_02)),
                "2002.2",
            ),
        ];
        for (fmt_str, ver_str, (sem_level, date), expected) in args {
            let format = Format::parse(fmt_str).unwrap();
            let version = Version::parse(ver_str, format).unwrap();
            let next_version = version.increment(sem_level, date).unwrap();
            assert_eq!(expected.to_string(), next_version.to_string());

            // next version should always be greater than current version
            assert_eq!(Some(Ordering::Greater), next_version.partial_cmp(&version));
        }
    }

    #[test]
    fn test_nothing_to_increment() {
        let format = Format::parse("[MAJOR].[MINOR].[PATCH]").unwrap();
        let version = Version::parse("1.2.3", format).unwrap();
        let actual = version.increment(None, None);
        assert_eq!(Err(VersionBumpError::NothingToIncrement), actual);
    }

    #[test]
    fn test_semantic_level_not_found() {
        let args = [
            ("[MINOR]", SemanticLevel::Major),
            ("[PATCH]", SemanticLevel::Major),
            ("[MAJOR]", SemanticLevel::Minor),
            ("[PATCH]", SemanticLevel::Minor),
            ("[MAJOR]", SemanticLevel::Patch),
            ("[MINOR]", SemanticLevel::Patch),
        ];
        for (format, sem_level) in args {
            let format = Format::parse(format).unwrap();
            let version = Version::parse("1", format).unwrap();
            let actual = version.increment(Some(&sem_level), None);
            assert_eq!(
                Err(VersionBumpError::SemanticLevelSpecifierNotInFormat {
                    name: sem_level.name(),
                }),
                actual
            );
        }
    }

    #[test]
    fn test_invalid_explicit_date() {
        // there are various ways a date can be invalid, but we only test one here. the details
        // are left to chrono.
        let format = Format::parse("[YYYY].[MM].[DD]").unwrap();
        let version = Version::parse("2001.1.1", format).unwrap();
        let actual = version.increment(None, Some(&DATE_INVALID));
        assert!(matches!(
            actual,
            Err(VersionBumpError::InvalidDateArguments { .. })
        ));
    }

    #[test]
    fn test_full_year_before_0() {
        let format = Format::parse("[YYYY]").unwrap();
        let version = Version::parse("1997", format).unwrap();
        // note we're doing a backwards year jump here. atypical, but possible.
        let actual = version.increment(None, Some(&DATE_BCE));
        dbg!(&actual);
        assert!(matches!(
            actual,
            Err(VersionBumpError::NegativeYearValue { .. })
        ));
    }

    #[test]
    fn test_full_year_before_2000() {
        let format_strings = ["[YY]", "[0Y]"];
        for format_string in format_strings {
            let format = Format::parse(format_string).unwrap();
            let version = Version::parse("97", format).unwrap();
            let actual = version.increment(None, Some(&DATE_1998_01_01));
            assert!(matches!(
                actual,
                Err(VersionBumpError::NegativeYearValue { .. })
            ));
        }
    }

    #[test]
    fn test_no_change() {
        let args = [
            ("[YYYY]", "2002"),
            ("[YYYY].[MM].[DD]", "2002.2.2"),
            ("all literal", "all literal"),
        ];
        for (format, version) in args {
            let format = Format::parse(format).unwrap();
            let version = Version::parse(version, format).unwrap();
            let actual = version.increment(None, Some(&DATE_2002_02_02));
            assert_eq!(Err(VersionBumpError::NoChange), actual);
        }
    }

    /// tests that consecutive greedy specifiers can match against the next token's digits. this
    /// behavior is expected, even if it's not ideal.
    #[test]
    fn test_greedy_consecutive_specifiers() {
        let args: &[(&str, [u32; 2], [u32; 2])] = &[
            ("[YYYY][MM]", [2024, 11], [20241, 1]),
            ("[MAJOR][MINOR]", [1, 23], [12, 3]),
        ];
        for (format, [expected1, expected2], actual) in args {
            let format = Format::parse(format).unwrap();
            let version_str = format!("{expected1}{expected2}");
            let version = Version::parse(&version_str, format).unwrap();
            for (idx, actual_val) in actual.iter().enumerate() {
                assert!(
                    matches!(version.tokens[idx], VersionToken::Value { value, .. } if value == *actual_val)
                );
            }
        }
    }

    #[test]
    fn test_cmp() {
        let args = [
            (
                "[YYYY].[MM].[0D]",
                ("2001.1.01", "2001.1.02"),
                Ordering::Less,
            ),
            (
                "[YYYY].[MM].[0D]",
                ("2001.1.01", "2001.1.01"),
                Ordering::Equal,
            ),
            (
                "[MAJOR].[MINOR].[PATCH]",
                ("1.2.3", "1.2.4"),
                Ordering::Less,
            ),
            (
                "[MAJOR].[MINOR].[PATCH]",
                ("1.2.3", "1.2.3"),
                Ordering::Equal,
            ),
        ];
        for (fmt_str, (ver_str_a, ver_str_b), expected) in args {
            let format = Format::parse(fmt_str).unwrap();
            let version_a = Version::parse(ver_str_a, format.clone()).unwrap();
            let version_b = Version::parse(ver_str_b, format).unwrap();
            assert_eq!(Some(expected), version_a.partial_cmp(&version_b));
        }
    }

    #[test]
    fn test_cmp_uncomparable() {
        let version = "10"; // same version

        // but different formats
        let format_a = Format::parse("[0D]").unwrap();
        let format_b = Format::parse("[DD]").unwrap();

        let version_a = Version::parse(version, format_a).unwrap();
        let version_b = Version::parse(version, format_b).unwrap();
        assert!(version_a.partial_cmp(&version_b).is_none());
        assert!(!version_a.eq(&version_b));
    }

    #[test]
    fn test_cmp_diff_but_equal_formats() {
        let version = "10"; // same version
        let format = "[0D]"; // same format

        // different objects of the same format
        let format_a = Format::parse(format).unwrap();
        let format_b = Format::parse(format).unwrap();

        let version_a = Version::parse(version, format_a).unwrap();
        let version_b = Version::parse(version, format_b).unwrap();
        assert_eq!(Some(Ordering::Equal), version_a.partial_cmp(&version_b));
    }
}
