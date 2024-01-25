use crate::scheme::{Cal, CalSem, Scheme, Sem};
use crate::{format::FormatToken, specifier::*, Format, NextverError};
use chrono::{Local, NaiveDate, Utc};
use core::{
    fmt::{self, Display},
    panic,
};
use std::cmp::Ordering;
use std::marker::PhantomData;

/**
Ways to specify a date.

```
use nextver::Date;

let explicit = Date::Explicit(2021, 2, 3);
let utc_now = Date::UtcNow;
let local_now = Date::LocalNow;
```
**/
#[derive(Debug, Clone)]
pub enum Date {
    /// Use the current date in UTC, as determined when this variant is used.
    UtcNow,

    /// Use the current date in the local timezone, as determined when this variant is used.
    LocalNow,

    /// Build a date from explicit values.
    ///
    /// Note that it is possible to create invalid dates, but no validation will be done until this
    /// date is used by the library. If you are concerned about this use [Date::from] with a
    /// [chrono::Datelike] instead.
    Explicit(i32, u32, u32),
}

impl Date {
    fn get_date(&self) -> Result<NaiveDate, NextverError> {
        match self {
            Self::UtcNow => Ok(Utc::now().date_naive()),
            Self::LocalNow => Ok(Local::now().date_naive()),
            Self::Explicit(year, month, day) => NaiveDate::from_ymd_opt(*year, *month, *day).ok_or(
                NextverError::InvalidDateArguments {
                    year: *year,
                    month: *month,
                    day: *day,
                },
            ),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) enum VersionToken<'vs> {
    Value {
        value: u32,
        spec: &'static Specifier,
    },
    Fixed(&'vs str),
}

impl<'vs> Display for VersionToken<'vs> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VersionToken::Value { value, spec } => {
                let rendered = spec.format_value(value);
                write!(f, "{}", rendered)
            }
            VersionToken::Fixed(text) => write!(f, "{}", text),
        }
    }
}

impl<'vs> PartialEq for VersionToken<'vs> {
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

impl<'vs> PartialOrd for VersionToken<'vs> {
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
/// use nextver::{Version, NextverError, SemanticLevel};
///
/// let version = Version::from_parsed_format("[MAJOR].[MINOR].[PATCH]", "1.2.3").unwrap();
/// let incremented = version.increment(Some(&SemanticLevel::Minor), None).unwrap();
/// assert_eq!("1.3.0", incremented.to_string());
/// assert!(version < incremented);
///
/// let invalid = Version::from_parsed_format("[MAJOR].[MINOR].[PATCH]", "1.foo.3");
/// assert!(matches!(invalid, Err(NextverError::VersionFormatMismatch {..})));
/// ```
///
/// Or, use a previously created [Format] object:
///
/// ```
/// use nextver::{Format, Version};
///
/// let format = Format::parse("[MAJOR].[MINOR].[PATCH]").unwrap();
/// let version = Version::parse("1.2.3", format.clone());
/// assert!(version.is_ok());
/// ```
///
/// You can increment by semantic level, calendar date, or both:
///
/// ```
/// use nextver::{Version, SemanticLevel, Date};
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
pub struct Version<'vs, S: Scheme> {
    pub(crate) tokens: Vec<VersionToken<'vs>>,
    scheme: PhantomData<S>,
}

impl<'vs, S: Scheme> Version<'vs, S> {
    pub(crate) fn new(tokens: Vec<VersionToken<'vs>>) -> Self {
        Self {
            tokens,
            scheme: PhantomData,
        }
    }

    /// Parses a version string against a [Format], and returns a [Version] object if the version
    /// string matches the format. Otherwise, returns a [NextverError].
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
    ///   [NextverError::VersionFormatMismatch].
    pub(crate) fn parse(version_str: &'vs str, format: &Format<S>) -> Result<Self, NextverError> {
        let Some(captures) = format.get_regex().captures(version_str) else {
            return Err(NextverError::VersionFormatMismatch {
                version_string: version_str.to_owned(),
                format_string: format.to_string(),
            });
        };

        let mut tokens = Vec::with_capacity(format.tokens.len());

        // skip the first capture because it's the implicit group of the whole regex
        let group_captures = captures.iter().skip(1);

        for (match_, format_token) in group_captures.zip(&format.tokens) {
            let Some(match_) = match_ else {
                // would happen if the group was optional (e.g. `(\d)?`) and empty. we don't
                // currently construct our format patterns this way, but just in case. Plus we get a
                // destructure on the Option
                continue;
            };

            let text = match_.as_str();

            let token = match format_token {
                FormatToken::Specifier(specifier) => {
                    let value = specifier.parse_value_str(text);
                    VersionToken::Value {
                        value,
                        spec: specifier,
                    }
                }
                FormatToken::Literal(format_text) => {
                    if !text.eq(*format_text) {
                        return Err(NextverError::VersionFormatMismatch {
                            version_string: version_str.to_owned(),
                            format_string: format.to_string(),
                        });
                    }
                    VersionToken::Fixed(text)
                }
            };

            tokens.push(token);
        }

        Ok(Self {
            tokens,
            scheme: PhantomData,
        })
    }

    fn new_map_value_tokens<F>(&self, mut f: F) -> Result<Self, NextverError>
    where
        F: FnMut((&u32, &'static Specifier)) -> Result<u32, NextverError>,
    {
        let mut new_tokens = Vec::with_capacity(self.tokens.len());

        for token in &self.tokens {
            let new_token = match token {
                VersionToken::Value { value, spec } => {
                    let new_value = f((value, spec))?;
                    VersionToken::Value {
                        value: new_value,
                        spec,
                    }
                }
                _ => token.clone(),
            };
            new_tokens.push(new_token);
        }

        Ok(Version::new(new_tokens))
    }
}

impl<'vs> Version<'vs, Sem> {
    /// Returns a new version where the semantic value of the given [SemanticLevel] is incremented,
    /// and all lesser semantic values are reset to zero.
    ///
    /// It is absolutely ok to call this method if this version contains calendar values — they just
    /// won't be updated.
    ///
    /// # Arguments
    ///
    /// - `semantic_level`: The semantic level to increment by one. All lesser levels' values will
    ///   be reset to zero.
    ///
    /// # Example
    ///
    /// ```
    /// use nextver::{Format, SemanticLevel, Version};
    ///
    /// let format = Format::parse("[MAJOR].[MINOR].[PATCH]").unwrap();
    /// let version = Version::parse("1.2.3", format).unwrap();
    /// let new_version = version.increment(&SemanticLevel::Major).unwrap();
    /// assert_eq!("2.0.0", new_version.to_string());
    /// assert!(version < new_version);
    ///
    /// let newer_version = new_version.increment(&SemanticLevel::Patch).unwrap();
    /// assert_eq!("2.0.1", newer_version.to_string());
    /// assert!(new_version < newer_version);
    /// ```
    ///
    /// # Errors
    ///
    /// - Returns a [NextverError::SemanticLevelSpecifierNotInFormat] if `specifier` is not in
    ///   format.
    pub fn next(&self, specifier: &SemanticSpecifier) -> Result<Self, NextverError> {
        // track if the semantic level was found in the format string.
        let mut spec_found = false;

        // track if we should increment or reset to 0
        let mut already_bumped = false;

        let new_version = self.new_map_value_tokens(|(value, spec)| {
            let new_value = if let Specifier::Semantic(this_spec) = spec {
                if specifier >= this_spec {
                    if specifier == this_spec {
                        spec_found = true;
                    }
                    let incremented = this_spec.increment(value, already_bumped);
                    already_bumped = true;
                    incremented
                } else {
                    *value
                }
            } else {
                // we should never get here because our format is guaranteed to be semantic-only. to
                // avoid this would require a rearchitecting to make specifiers be generic to the
                // various schemes (consider that some specifiers are shared between schemes). not
                // sure this is possible cleanly.
                panic!("Non-semantic specifier in semantic version")
            };
            Ok(new_value)
        })?;

        if !spec_found {
            return Err(NextverError::SemanticSpecifierNotInFormat {
                spec: specifier.clone(),
            });
        }

        Ok(new_version)
    }
}

impl<'vs> Version<'vs, Cal> {
    /// Returns a new [Version] where all calendar values in this version are updated to match the
    /// given [Date].
    ///
    /// Although uncommon, you can call this method with a Date before that of this version's
    /// calendar values. (Depending on your format, a complete date might not even be present.) In
    /// this case, note that the new version may compare *less* than this one.
    ///
    /// # Arguments
    ///
    /// - `date`: The reference date to incrememnt calendar date values.
    ///
    /// # Examples
    ///
    /// ```
    /// use nextver::{Format, Date, Version};
    ///
    /// let format = Format::parse("[YYYY].[0M].[0D]").unwrap();
    /// let version = Version::parse("2023.12.04", format).unwrap();
    /// let new_version = version.update(&Date::Explicit{year: 2024, month: 1, day: 2}).unwrap();
    /// assert_eq!("2024.01.02", new_version.to_string());
    /// assert!(version < new_version);
    /// ```
    ///
    /// # Errors
    ///
    /// - If `date` provided is a [Date::Explicit] and the date values is do not represent a valid
    ///   date, returns a [NextverError::InvalidDateArguments].
    ///
    ///  - Returns a [NextverError::NegativeYearValue]...
    ///
    ///    - If the `date` provided is before year 0 and this version's format uses the `[YYYY]`
    ///      specifier.
    ///
    ///    - If the `date` provided is before the year 2000 and this version's format uses the
    ///      `[YY]` or `[0Y]` specifiers.
    ///
    ///    This is because the formatted values would be negative, which would affect parsing. [See
    ///    specifiers for more](struct.Format.html#specifier-table).
    pub fn next(&self, date: &Date) -> Result<Self, NextverError> {
        let naive_date = date.get_date()?;

        // track if the calendar was updated, so we can return NoCalendarChange if it wasn't.
        let mut cal_updated = false;

        let new_version = self.new_map_value_tokens(|(old_value, spec)| {
            let new_value = if let Specifier::Calendar(this_spec) = spec {
                let updated = this_spec.update(&naive_date)?;
                if updated != *old_value {
                    cal_updated = true;
                }
                updated
            } else {
                panic!("Non-calendar specifier in calendar version")
            };
            Ok(new_value)
        })?;

        if !cal_updated {
            return Err(NextverError::NoCalendarChange);
        }

        Ok(new_version)
    }
}

/// A semantic specifier, used as an argument to [Version<CalSem>::next]. It only includes `Minor`
/// and `Patch` variants because `major` is not permitted in a [CalSem] format.
///
/// Note that this name is a bit of a misnomer because it's not just a "CalSem specifier" — it's
/// a "CalSem semantic specifier". But that's a bit of a mouthful.
pub enum CalSemSpecifier {
    Minor,
    Patch,
}

impl CalSemSpecifier {
    fn spec(&self) -> &'static SemanticSpecifier {
        match self {
            Self::Minor => &SemanticSpecifier::Minor,
            Self::Patch => &SemanticSpecifier::Patch,
        }
    }
}

impl<'vs> Version<'vs, CalSem> {
    /// Returns a new [Version] where all calendar values in this version are updated to match the
    /// given [Date]. If the calendar values would not change, the version is incremented by the
    /// given [CalSemSemanticSpecifier].
    ///
    /// TODO: fill out rest of this doc
    pub fn next(
        &self,
        date: &Date,
        semantic_specifier: &CalSemSpecifier,
    ) -> Result<Self, NextverError> {
        // this is like a combination Version<Cal>::update and Version<Sem>::increment

        // map to a regular semantic specifier
        let semantic_specifier = semantic_specifier.spec();

        let naive_date = date.get_date()?;

        // track if the semantic level was found in the format string.
        let mut spec_found = false;

        // track if we should increment or reset to 0
        let mut already_bumped = false;

        // track if the calendar was updated, so we can return NoCalendarChange if it wasn't.
        let mut cal_updated = false;

        let new_version = self.new_map_value_tokens(|(old_value, spec)| {
            let new_value = match spec {
                Specifier::Calendar(this_spec) => {
                    let updated = this_spec.update(&naive_date)?;
                    if updated != *old_value {
                        cal_updated = true;
                    }
                    updated
                }
                Specifier::Semantic(this_spec) => {
                    if !cal_updated {
                        if semantic_specifier >= this_spec {
                            if semantic_specifier == this_spec {
                                spec_found = true;
                            }
                            let incremented = this_spec.increment(old_value, already_bumped);
                            already_bumped = true;
                            incremented
                        } else {
                            *old_value
                        }
                    } else {
                        *old_value
                    }
                }
            };
            Ok(new_value)
        })?;

        if !spec_found {
            return Err(NextverError::SemanticSpecifierNotInFormat {
                spec: semantic_specifier.clone(),
            });
        }

        Ok(new_version)
    }
}

impl<'vs, S: Scheme> Display for Version<'vs, S> {
    /// Returns the rendered version string
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for token in &self.tokens {
            write!(f, "{}", token)?
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    // use chrono::format;

    use super::*;

    #[test]
    fn test_major_minor_patch_parse() {
        let version_strs = [
            ("01.2.3", false), // zero-padding disallowed
            ("1.02.3", false), // zero-padding disallowed
            ("1.2.03", false), // zero-padding disallowed
            ("1.2.3", true),
            ("10.20.30", true),
            ("11.22.33", true),
        ];

        for (version_str, passes) in &version_strs {
            let format = Sem::new_format("[MAJOR].[MINOR].[PATCH]").unwrap();
            let version = Version::parse(version_str, &format);
            if *passes {
                assert!(version.is_ok());
            } else {
                assert!(matches!(
                    version,
                    Err(NextverError::VersionFormatMismatch { .. })
                ));
            }
        }
    }

    /// test full year
    #[test]
    fn test_full_year_parse() {
        let format_str = "[YYYY]";
        let args = [
            ("01", false),  // zero-padding disallowed
            ("001", false), // zero-padding disallowed
            ("0", false),   // no year 0 (that'd be 1 BCE)
            ("1", true),
            ("10", true),
            ("100", true),
        ];

        for (version_str, passes) in &args {
            let format = Cal::new_format(format_str).unwrap();
            let version = Version::parse(version_str, &format);
            if *passes {
                assert!(version.is_ok());
            } else {
                assert!(matches!(
                    version,
                    Err(NextverError::VersionFormatMismatch { .. })
                ));
            }
        }
    }

    /// test short year
    #[test]
    fn test_short_year_parse() {
        let format_str = "[YY]";
        let args = [
            ("01", false),  // zero-padding disallowed
            ("001", false), // zero-padding disallowed
            ("0", true),
            ("1", true),
            ("10", true),
            ("100", true),
        ];

        for (version_str, passes) in &args {
            let format = Cal::new_format(format_str).unwrap();
            let version = Version::parse(version_str, &format);
            if *passes {
                assert!(version.is_ok());
            } else {
                assert!(matches!(
                    version,
                    Err(NextverError::VersionFormatMismatch { .. })
                ));
            }
        }
    }

    /// test zero-padded year
    #[test]
    fn test_zp_year_parse() {
        let format_str = "[0Y]";
        let args = [
            ("0", false), // must be 2-digits
            ("1", false), // must be 2-digits
            ("00", true),
            ("01", true),
            ("10", true),
            ("100", true),
        ];

        for (version_str, passes) in &args {
            let format = Cal::new_format(format_str).unwrap();
            let version = Version::parse(version_str, &format);
            if *passes {
                assert!(version.is_ok());
            } else {
                assert!(matches!(
                    version,
                    Err(NextverError::VersionFormatMismatch { .. })
                ));
            }
        }
    }

    // test zero-padded week
    #[test]
    fn test_zp_week_parse() {
        let args = [
            ("[YYYY].[0W]", "2024.0", false),   // must be two-digit
            ("[YYYY].[0W]", "2024.122", false), // must be two-digit
            ("[YYYY].[0W]", "2024.00", true),   // there is a week zero
            ("[YYYY].[0W]", "2024.01", true),
            ("[YYYY].[0W]", "2024.10", true),
            ("[YYYY].[0W]", "2024.12", true),
        ];

        for (format_str, version_str, passes) in &args {
            let format = Cal::new_format(format_str).unwrap();
            let version = Version::parse(version_str, &format);
            if *passes {
                assert!(version.is_ok());
            } else {
                assert!(matches!(
                    version,
                    Err(NextverError::VersionFormatMismatch { .. })
                ));
            }
        }
    }

    // test short week
    #[test]
    fn test_short_week_parse() {
        let args = [
            ("[YYYY].[WW]", "2024.01", false),  // zero-padding disallowed
            ("[YYYY].[WW]", "2024.00", false),  // zero-padding disallowed
            ("[YYYY].[WW]", "2024.122", false), // must be two-digit
            ("[YYYY].[WW]", "2024.0", true),
            ("[YYYY].[WW]", "2024.10", true),
            ("[YYYY].[WW]", "2024.12", true),
        ];

        for (format_str, version_str, passes) in &args {
            let format = Cal::new_format(format_str).unwrap();
            let version = Version::parse(version_str, &format);
            if *passes {
                assert!(version.is_ok());
            } else {
                assert!(matches!(
                    version,
                    Err(NextverError::VersionFormatMismatch { .. })
                ));
            }
        }
    }

    /// test zero-padded month and day (same characteristics: no month/day 0, 2 digits).
    #[test]
    fn test_zp_month_day_parse() {
        let args = [
            // month
            ("[YYYY].[0M]", "2024.0", false),   // must be two-digit
            ("[YYYY].[0M]", "2024.00", false),  // no month 0
            ("[YYYY].[0M]", "2024.122", false), // must be two-digit
            ("[YYYY].[0M]", "2024.01", true),
            ("[YYYY].[0M]", "2024.10", true),
            ("[YYYY].[0M]", "2024.12", true),
            // day
            ("[YYYY].[MM].[0D]", "2024.1.0", false), // must be two-digit
            ("[YYYY].[MM].[0D]", "2024.1.00", false), // no day 0
            ("[YYYY].[MM].[0D]", "2024.1.122", false), // must be two-digit
            ("[YYYY].[MM].[0D]", "2024.1.01", true),
            ("[YYYY].[MM].[0D]", "2024.1.10", true),
            ("[YYYY].[MM].[0D]", "2024.1.12", true),
        ];

        for (format_str, version_str, passes) in &args {
            let format = Cal::new_format(format_str).unwrap();
            let version = Version::parse(version_str, &format);
            if *passes {
                assert!(version.is_ok());
            } else {
                assert!(matches!(
                    version,
                    Err(NextverError::VersionFormatMismatch { .. })
                ));
            }
        }
    }

    /// test short month, day, and week. (same characteristics: no month/day 0, 1-2 digits).
    #[test]
    fn test_short_mdw_parse() {
        let args = [
            // month
            ("[YYYY].[MM]", "2024.0", false),   // no month 0
            ("[YYYY].[MM]", "2024.00", false),  // zero-padding disallowed
            ("[YYYY].[MM]", "2024.01", false),  // zero-padding disallowed
            ("[YYYY].[MM]", "2024.122", false), // must be two-digit
            ("[YYYY].[MM]", "2024.1", true),
            ("[YYYY].[MM]", "2024.10", true),
            ("[YYYY].[MM]", "2024.12", true),
            // day
            ("[YYYY].[MM].[DD]", "2024.1.0", false), // no month 0
            ("[YYYY].[MM].[DD]", "2024.1.00", false), // zero-padding disallowed
            ("[YYYY].[MM].[DD]", "2024.1.01", false), // zero-padding disallowed
            ("[YYYY].[MM].[DD]", "2024.1.122", false), // must be two-digit
            ("[YYYY].[MM].[DD]", "2024.1.1", true),
            ("[YYYY].[MM].[DD]", "2024.1.10", true),
            ("[YYYY].[MM].[DD]", "2024.1.12", true),
        ];

        for (format_str, version_str, passes) in &args {
            let format = Cal::new_format(format_str).unwrap();
            let version = Version::parse(version_str, &format);
            if *passes {
                assert!(version.is_ok());
            } else {
                assert!(matches!(
                    version,
                    Err(NextverError::VersionFormatMismatch { .. })
                ));
            }
        }
    }

    #[test]
    fn test_unicode_literal() {
        let format_str = "👍[MAJOR]👯‍♀️";
        let version_str = "👍1👯‍♀️";
        let format = Sem::new_format(format_str).unwrap();
        let version = Version::parse(version_str, &format);
        assert!(version.is_ok());
    }

    //     // shows variance in all fields between zero-padded and non-zero-padded
    //     static DATE_2002_02_02: Date = Date::Explicit {
    //         year: 2002,
    //         month: 2,
    //         day: 2,
    //     };
    //     // before year 2000
    //     static DATE_1998_01_01: Date = Date::Explicit {
    //         year: 1998,
    //         month: 1,
    //         day: 1,
    //     };
    //     // 1 BCE
    //     static DATE_BCE: Date = Date::Explicit {
    //         year: 0,
    //         month: 1,
    //         day: 1,
    //     };
    //     // invalid
    //     static DATE_INVALID: Date = Date::Explicit {
    //         year: 2001,
    //         month: 2,
    //         day: 30,
    //     };

    //     #[test]
    //     fn test_parse_ok() {
    //         let args = [
    //             ("[MAJOR]", vec!["1", "2", "10"]),
    //             ("[MINOR]", vec!["1", "2", "10"]),
    //             ("[PATCH]", vec!["1", "2", "10"]),
    //             ("[YYYY]", vec!["2001", "2002", "2010", "100000"]),
    //             ("[YY]", vec!["1", "2", "10"]),
    //             ("[0Y]", vec!["01", "02", "10"]),
    //             ("[MM]", vec!["1", "2", "10"]),
    //             ("[0M]", vec!["01", "02", "10"]),
    //             ("[WW]", vec!["1", "2", "10"]),
    //             ("[0W]", vec!["01", "02", "10"]),
    //             ("[DD]", vec!["1", "2", "10"]),
    //             ("[0D]", vec!["01", "02", "10"]),
    //             (r"\[MAJOR]", vec!["[MAJOR]"]),
    //             (
    //                 "The quick brown fox jumps over the lazy dog",
    //                 vec!["The quick brown fox jumps over the lazy dog"],
    //             ),
    //         ];

    //         for (format, version_strs) in args {
    //             let format = Format::parse(format).unwrap();
    //             for version_str in version_strs {
    //                 let version = Version::parse(version_str, format.clone());
    //                 assert!(version.is_ok());
    //                 let version = version.unwrap();
    //                 assert_eq!(version.to_string(), version_str.to_string());
    //                 assert_eq!(Format::from(&version), format);
    //             }
    //         }
    //     }

    //     // #[test]
    //     // fn test_increment_ok() {
    //     //     let args = [
    //     //         (
    //     //             "[MAJOR].[MINOR].[PATCH]",
    //     //             "1.2.3",
    //     //             (Some(&SemanticLevel::Major), None),
    //     //             "2.0.0",
    //     //         ),
    //     //         (
    //     //             "[MAJOR].[MINOR].[PATCH]",
    //     //             "1.2.3",
    //     //             (Some(&SemanticLevel::Minor), None),
    //     //             "1.3.0",
    //     //         ),
    //     //         (
    //     //             "[MAJOR].[MINOR].[PATCH]",
    //     //             "1.2.3",
    //     //             (Some(&SemanticLevel::Patch), None),
    //     //             "1.2.4",
    //     //         ),
    //     //         (
    //     //             "[YYYY].[MM].[DD]",
    //     //             "2001.1.1",
    //     //             (None, Some(&DATE_2002_02_02)),
    //     //             "2002.2.2",
    //     //         ),
    //     //         (
    //     //             "[YYYY].[PATCH]",
    //     //             "2001.1",
    //     //             (None, Some(&DATE_2002_02_02)),
    //     //             "2002.1",
    //     //         ),
    //     //         (
    //     //             "[YYYY].[PATCH]",
    //     //             "2001.1",
    //     //             (Some(&SemanticLevel::Patch), None),
    //     //             "2001.2",
    //     //         ),
    //     //         (
    //     //             "[YYYY].[PATCH]",
    //     //             "2001.1",
    //     //             (Some(&SemanticLevel::Patch), Some(&DATE_2002_02_02)),
    //     //             "2002.2",
    //     //         ),
    //     //     ];
    //     //     for (fmt_str, ver_str, (sem_level, date), expected) in args {
    //     //         let format = Format::parse(fmt_str).unwrap();
    //     //         let version = Version::parse(ver_str, format).unwrap();
    //     //         let next_version = version.increment(sem_level, date).unwrap();
    //     //         assert_eq!(expected.to_string(), next_version.to_string());

    //     //         // next version should always be greater than current version
    //     //         assert_eq!(Some(Ordering::Greater), next_version.partial_cmp(&version));
    //     //     }
    //     // }

    //     #[test]
    //     fn test_increment_semantic_ok() {
    //         let args = [
    //             (
    //                 "[MAJOR].[MINOR].[PATCH]",
    //                 "1.2.3",
    //                 SemanticLevel::Major,
    //                 "2.0.0",
    //             ),
    //             (
    //                 "[MAJOR].[MINOR].[PATCH]",
    //                 "1.2.3",
    //                 SemanticLevel::Minor,
    //                 "1.3.0",
    //             ),
    //             (
    //                 "[MAJOR].[MINOR].[PATCH]",
    //                 "1.2.3",
    //                 SemanticLevel::Patch,
    //                 "1.2.4",
    //             ),
    //         ];
    //         for (fmt_str, ver_str, sem_level, expected) in args {
    //             let format = Format::parse(fmt_str).unwrap();
    //             let version = Version::parse(ver_str, format).unwrap();
    //             let next_version = version.increment(&sem_level).unwrap();
    //             assert_eq!(expected.to_string(), next_version.to_string());

    //             // next version should always be greater than current version
    //             assert_eq!(Some(Ordering::Greater), next_version.partial_cmp(&version));
    //         }
    //     }

    //     #[test]
    //     fn test_increment_calendar_ok() {
    //         let args = [("[YYYY].[MM].[DD]", "2001.1.1", &DATE_2002_02_02, "2002.2.2")];
    //         for (fmt_str, ver_str, date, expected) in args {
    //             let format = Format::parse(fmt_str).unwrap();
    //             let version = Version::parse(ver_str, format).unwrap();
    //             let next_version = version.update(date).unwrap();
    //             assert_eq!(expected.to_string(), next_version.to_string());

    //             // next version should always be greater than current version
    //             assert_eq!(Some(Ordering::Greater), next_version.partial_cmp(&version));
    //         }
    //     }

    //     #[test]
    //     fn test_semantic_level_not_found() {
    //         let args = [
    //             ("[MINOR]", SemanticLevel::Major),
    //             ("[PATCH]", SemanticLevel::Major),
    //             ("[MAJOR]", SemanticLevel::Minor),
    //             ("[PATCH]", SemanticLevel::Minor),
    //             ("[MAJOR]", SemanticLevel::Patch),
    //             ("[MINOR]", SemanticLevel::Patch),
    //         ];
    //         for (format, sem_level) in args {
    //             let format = Format::parse(format).unwrap();
    //             let version = Version::parse("1", format).unwrap();
    //             let actual = version.increment(&sem_level);
    //             assert_eq!(
    //                 Err(NextverError::SemanticLevelNotInFormat {
    //                     name: sem_level.name(),
    //                 }),
    //                 actual
    //             );
    //         }
    //     }

    //     #[test]
    //     fn test_invalid_explicit_date() {
    //         // there are various ways a date can be invalid, but we only test one here. the details
    //         // are left to chrono.
    //         let format = Format::parse("[YYYY].[MM].[DD]").unwrap();
    //         let version = Version::parse("2001.1.1", format).unwrap();
    //         let actual = version.update(&DATE_INVALID);
    //         assert!(matches!(
    //             actual,
    //             Err(NextverError::InvalidDateArguments { .. })
    //         ));
    //     }

    //     #[test]
    //     fn test_full_year_before_0() {
    //         let format = Format::parse("[YYYY]").unwrap();
    //         let version = Version::parse("1997", format).unwrap();
    //         // note we're doing a backwards year jump here. atypical, but possible.
    //         let actual = version.update(&DATE_BCE);
    //         dbg!(&actual);
    //         assert!(matches!(
    //             actual,
    //             Err(NextverError::NegativeYearValue { .. })
    //         ));
    //     }

    //     #[test]
    //     fn test_full_year_before_2000() {
    //         let format_strings = ["[YY]", "[0Y]"];
    //         for format_string in format_strings {
    //             let format = Format::parse(format_string).unwrap();
    //             let version = Version::parse("97", format).unwrap();
    //             let actual = version.update(&DATE_1998_01_01);
    //             assert!(matches!(
    //                 actual,
    //                 Err(NextverError::NegativeYearValue { .. })
    //             ));
    //         }
    //     }

    //     #[test]
    //     fn test_no_change() {
    //         let args = [
    //             ("[YYYY]", "2002"),
    //             ("[YYYY].[MM].[DD]", "2002.2.2"),
    //             ("all literal", "all literal"),
    //         ];
    //         for (format, version) in args {
    //             let format = Format::parse(format).unwrap();
    //             let version = Version::parse(version, format).unwrap();
    //             let actual = version.update(&DATE_2002_02_02);
    //             assert_eq!(Err(NextverError::NoCalendarChange), actual);
    //         }
    //     }

    //     /// tests that consecutive greedy specifiers can match against the next token's digits. this
    //     /// behavior is expected, even if it's not ideal.
    //     #[test]
    //     fn test_greedy_consecutive_specifiers() {
    //         let args: &[(&str, [u32; 2], [u32; 2])] = &[
    //             ("[YYYY][MM]", [2024, 11], [20241, 1]),
    //             ("[MAJOR][MINOR]", [1, 23], [12, 3]),
    //         ];
    //         for (format, [expected1, expected2], actual) in args {
    //             let format = Format::parse(format).unwrap();
    //             let version_str = format!("{expected1}{expected2}");
    //             let version = Version::parse(&version_str, format).unwrap();
    //             for (idx, actual_val) in actual.iter().enumerate() {
    //                 assert!(
    //                     matches!(version.tokens[idx], VersionToken::Value { value, .. } if value == *actual_val)
    //                 );
    //             }
    //         }
    //     }

    //     #[test]
    //     fn test_cmp() {
    //         let args = [
    //             (
    //                 "[YYYY].[MM].[0D]",
    //                 ("2001.1.01", "2001.1.02"),
    //                 Ordering::Less,
    //             ),
    //             (
    //                 "[YYYY].[MM].[0D]",
    //                 ("2001.1.01", "2001.1.01"),
    //                 Ordering::Equal,
    //             ),
    //             (
    //                 "[MAJOR].[MINOR].[PATCH]",
    //                 ("1.2.3", "1.2.4"),
    //                 Ordering::Less,
    //             ),
    //             (
    //                 "[MAJOR].[MINOR].[PATCH]",
    //                 ("1.2.3", "1.2.3"),
    //                 Ordering::Equal,
    //             ),
    //         ];
    //         for (fmt_str, (ver_str_a, ver_str_b), expected) in args {
    //             let format = Format::parse(fmt_str).unwrap();
    //             let version_a = Version::parse(ver_str_a, format.clone()).unwrap();
    //             let version_b = Version::parse(ver_str_b, format).unwrap();
    //             assert_eq!(Some(expected), version_a.partial_cmp(&version_b));
    //         }
    //     }

    //     #[test]
    //     fn test_cmp_uncomparable() {
    //         let version = "10"; // same version

    //         // but different formats
    //         let format_a = Format::parse("[0D]").unwrap();
    //         let format_b = Format::parse("[DD]").unwrap();

    //         let version_a = Version::parse(version, format_a).unwrap();
    //         let version_b = Version::parse(version, format_b).unwrap();
    //         assert!(version_a.partial_cmp(&version_b).is_none());
    //         assert!(!version_a.eq(&version_b));
    //     }

    //     #[test]
    //     fn test_cmp_diff_but_equal_formats() {
    //         let version = "10"; // same version
    //         let format = "[0D]"; // same format

    //         // different objects of the same format
    //         let format_a = Format::parse(format).unwrap();
    //         let format_b = Format::parse(format).unwrap();

    //         let version_a = Version::parse(version, format_a).unwrap();
    //         let version_b = Version::parse(version, format_b).unwrap();
    //         assert_eq!(Some(Ordering::Equal), version_a.partial_cmp(&version_b));
    //     }

    //     #[test]
    //     fn test_empty() {
    //         let format = Format::parse("").unwrap();
    //         let version = Version::parse("", format).unwrap();
    //         assert_eq!("", version.to_string());
    //     }

    //     #[test]
    //     fn test_increment_calendar_with_semantic_fallback_falls_back() {
    //         let format = Format::parse("[YYYY].[PATCH]").unwrap();
    //         let version = Version::parse("2023.1", format).unwrap();
    //         let actual = version.update_or_increment(
    //             &Date::Explicit {
    //                 year: 2023, // same year
    //                 month: 2,
    //                 day: 3,
    //             },
    //             &SemanticLevel::Patch,
    //         );
    //         assert_eq!("2023.2", actual.unwrap().to_string());
    //     }

    //     #[test]
    //     fn test_increment_calendar_with_semantic_fallback_calendar_increment() {
    //         let format = Format::parse("[YYYY].[PATCH]").unwrap();
    //         let version = Version::parse("2023.1", format).unwrap();
    //         let actual = version.update_or_increment(
    //             &Date::Explicit {
    //                 year: 2024, // updated year
    //                 month: 2,
    //                 day: 3,
    //             },
    //             &SemanticLevel::Patch,
    //         );
    //         assert_eq!("2024.1", actual.unwrap().to_string());
    //     }
}
