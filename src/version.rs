use crate::{
    error::{DateError, VersionError},
    format::{Format, FormatToken},
    scheme::{Cal, CalSem, Scheme, Sem},
    specifier::{CalSemLevel, Level, ParseWidth, SpecValue, SpecValueResult, Specifier},
    SemLevel,
};
use chrono::{Local, NaiveDate, Utc};
use core::{
    fmt::{self, Display},
    ops::Deref,
    ptr,
    str::{self, FromStr},
};

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum VersionToken<'vs, S: Scheme> {
    Value {
        value: SpecValue,
        spec: &'static S::Specifier,
    },
    Literal(&'vs [u8]),
}

impl<'vs, S: Scheme> Clone for VersionToken<'vs, S> {
    fn clone(&self) -> Self {
        match self {
            VersionToken::Value { value, spec } => VersionToken::Value {
                value: *value,
                spec: *spec,
            },
            VersionToken::Literal(text) => VersionToken::Literal(text),
        }
    }
}

impl<'vs, S: Scheme> Display for VersionToken<'vs, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            VersionToken::Value { value, spec } => {
                let formatted = spec.format_value(value);
                f.write_str(&formatted)
            }
            VersionToken::Literal(text) => {
                let text_str = unsafe { str::from_utf8_unchecked(text) };
                f.write_str(text_str)
            }
        }
    }
}

impl<'vs, S: Scheme> PartialOrd for VersionToken<'vs, S> {
    /// Compares two version tokens. This is only a partial ordering it is only meaningful to
    /// compare two version tokens when they come from the equivalent formats.
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        // it only makes sense to compare values if they are the same type, thus, only a partial
        // ordering.
        use VersionToken::*;
        match (self, other) {
            (Literal(a), Literal(b)) => {
                // there is no ordering for literals: they're either equal or not
                if a.eq(b) {
                    Some(std::cmp::Ordering::Equal)
                } else {
                    None
                }
            }

            (
                Value {
                    value: val_a,
                    spec: spec_a,
                },
                Value {
                    value: val_b,
                    spec: spec_b,
                },
            ) => {
                if ptr::eq(*spec_a, *spec_b) {
                    val_a.partial_cmp(val_b)
                } else {
                    None
                }
            }

            _ => None,
        }
    }
}

/// Just like a [FormatToken], but holds the literal text unescaped, so it can be quickly matched
/// against a version string (instead of having to recompute the unescaped text each time).
enum UnescapedFormatToken<S: Scheme> {
    Specifier(&'static S::Specifier),
    Literal(String),
}

impl<'fs, S: Scheme> From<&FormatToken<'fs, S>> for UnescapedFormatToken<S> {
    fn from(token: &FormatToken<'fs, S>) -> Self {
        match token {
            FormatToken::Specifier(spec) => UnescapedFormatToken::Specifier(*spec),
            FormatToken::Literal(literal) => {
                let unescaped = unsafe { std::str::from_utf8_unchecked(literal) }
                    .replace("<<", "<")
                    .replace(">>", ">");
                UnescapedFormatToken::Literal(unescaped)
            }
        }
    }
}

/// A Version object represents a specific point in a project's development, comprised of *values*
/// and *literal text*. It's structure is defined by a [Format]. Versions can be
/// [displayed](Version::to_string), [incremented](Version::next), and
/// [compared](Version::partial_cmp).
///
/// Version objects are created with the [Scheme::new_version] or [Format::new_version]
/// methods.
///
/// Note that Version objects only implement a partial ordering. This is because the ordering only
/// makes sense when they have the same format. Therefore, comparisons between versions with
/// different formats will always return `false`.
///
/// # Examples
///
/// ```
/// use nextver::prelude::*;
///
/// let cur = Sem::new_version("<MAJOR>.<MINOR>.<PATCH>", "1.2.3").unwrap();
/// let next = version.next(Some(&SemSpecifier::Minor)).unwrap();
/// assert_eq!("1.3.0", incremented.to_string());
/// assert!(cur < next);
///```
///
/// Or, use a previously created [Format] object:
///
/// ```
/// use nextver::prelude::*;
///
/// let format = Format::parse("<MAJOR>.<MINOR>.<PATCH>").unwrap();
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
/// let version = Version::from_parsed_format("<YYYY>.<PATCH>", "2023.123").unwrap();
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
#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Version<'vs, S: Scheme> {
    pub(crate) tokens: Vec<VersionToken<'vs, S>>,
}

impl<'vs, S: Scheme> Version<'vs, S> {
    pub(crate) fn new(tokens: Vec<VersionToken<'vs, S>>) -> Self {
        Self { tokens }
    }

    /// Parses a version string against a [Format], and returns a [Version] object if the version
    /// string matches the format. Otherwise, returns a [VersionError].
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
    ///   [VersionError::VersionFormatMismatch].
    pub(crate) fn parse(version_str: &'vs str, format: &Format<S>) -> Result<Self, VersionError> {
        let unescaped_format_tokens: Vec<UnescapedFormatToken<S>> =
            format.tokens.iter().map(|t| t.into()).collect();
        Self::parse_rec(version_str.as_bytes(), &unescaped_format_tokens, &[])
            .map(|tokens| Version::new(tokens))
            .ok_or(VersionError::VersionFormatMismatch {
                version_string: version_str.to_owned(),
                format_string: format.to_string(),
            })
    }

    fn parse_rec(
        version_str: &'vs [u8],
        fmt_tokens: &[UnescapedFormatToken<S>],
        ver_tokens: &[VersionToken<'vs, S>],
    ) -> Option<Vec<VersionToken<'vs, S>>> {
        if version_str.is_empty() && fmt_tokens.is_empty() {
            return Some(ver_tokens.to_vec());
        }

        let first_fmt_token = fmt_tokens.first()?;

        match first_fmt_token {
            UnescapedFormatToken::Literal(literal) => {
                if version_str.starts_with(literal.as_bytes()) {
                    let mut new_ver_tokens = ver_tokens.to_vec();
                    let (literal, version_str) = version_str.split_at(literal.len());
                    new_ver_tokens.push(VersionToken::Literal(literal));
                    Self::parse_rec(version_str, &fmt_tokens[1..], &new_ver_tokens)
                } else {
                    None
                }
            }
            UnescapedFormatToken::Specifier(specifier) => {
                let min_width = match specifier.parse_width() {
                    ParseWidth::OneOrTwo | ParseWidth::AtLeastOne => 1,
                    ParseWidth::AtLeastTwo | ParseWidth::Two => 2,
                };
                let max_width = match specifier.parse_width() {
                    ParseWidth::OneOrTwo | ParseWidth::Two => 2.min(version_str.len()),
                    ParseWidth::AtLeastOne | ParseWidth::AtLeastTwo => version_str.len(),
                };
                let mut value: SpecValue = 0;
                let mut continue_iterating = true;

                for idx in 0..max_width {
                    let next = version_str[idx];
                    if !next.is_ascii_digit() {
                        return None; // not digit, so this'll never be valid
                    }
                    value = value * 10 + (next - b'0') as SpecValue;

                    let cur_width = idx + 1;
                    if cur_width < min_width {
                        // keep going
                        continue;
                    }

                    // cases:
                    // - can be zero=true, zero-padded=true (0Y, 0W)
                    //
                    //   iterate to end normally
                    //
                    // - can be zero=true, zero-padded=false (MAJOR, MINOR, PATCH, YY,
                    //   WW)
                    //
                    //   if we encounter a zero first, try this iteration, but no
                    //   subsequent (set continue_iterating to false). else, iterate to
                    //   end normally
                    //
                    // - can be zero=false, zero-padded=true (0M, 0D)
                    //
                    //   if we encounter a zero first, skip/continue until we find
                    //   non-zero. else, iterate to end normally.
                    //
                    // - can be zero=false, zero-padded=false (YYYY, MM, DD)
                    //
                    //   if we encounter a zero first, return None. else, iterate to end
                    //   normally
                    if value == 0 {
                        match (specifier.can_be_zero(), specifier.zero_pad_len().is_some()) {
                            (true, true) => {
                                // can be zero, and zero-padded: iterate to end normally
                            }
                            (true, false) => {
                                // can be zero, and not zero-padded: just this iteration,
                                // and no more
                                continue_iterating = false;
                            }
                            (false, true) => {
                                // can't be zero, and zero-padded: continue until non-zero
                                continue;
                            }
                            (false, false) => {
                                // can't be zero, and not zero-padded: return None
                                return None;
                            }
                        }
                    }
                    let mut new_ver_tokens = ver_tokens.to_vec();
                    new_ver_tokens.push(VersionToken::Value {
                        value,
                        spec: specifier,
                    });
                    if let Some(new_ver_tokens) =
                        Self::parse_rec(&version_str[idx + 1..], &fmt_tokens[1..], &new_ver_tokens)
                    {
                        return Some(new_ver_tokens);
                    }
                    if !continue_iterating {
                        break;
                    }
                }
                None
            }
        }
    }

    fn new_map_value_tokens<F>(&self, mut f: F) -> Result<Self, VersionError>
    where
        F: FnMut((&SpecValue, &S::Specifier)) -> SpecValueResult,
    {
        let mut new_tokens = Vec::with_capacity(self.tokens.len());

        for token in &self.tokens {
            let new_token = match token {
                VersionToken::Value { value, spec } => {
                    let new_value = f((value, spec))?;
                    VersionToken::Value {
                        value: new_value,
                        spec: *spec,
                    }
                }
                VersionToken::Literal(_) => token.clone(),
            };
            new_tokens.push(new_token);
        }

        Ok(Version::new(new_tokens))
    }
}

impl<'vs, S: Scheme> PartialOrd for Version<'vs, S> {
    /// Compares two versions. This is only a partial ordering it is only meaningful to compare two
    /// versions when they come from the same format.
    ///
    /// Returns `None` when either of the following are true:
    ///
    /// - The number of *tokens* in the versions are different. Tokens are either literal text or
    ///   specifier values.
    /// - For two given tokens, they are not of the same type. E.g., one is a literal, one is a
    ///   value.
    /// - For two given literal tokens, the text is not the same.
    /// - For two given value tokens, they are not of the same specifier type. E.g., one is a
    ///  `<YYYY>` value, one is a `<YY>` value.
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.tokens.len() != other.tokens.len() {
            None
        } else {
            self.tokens.partial_cmp(&other.tokens)
        }
    }
}

// TODO: I hate that Sem::next takes a SemSpecifier, but CalSem::next takes a CalSemIncrSpecifier.
// Figure out how to better name/architect these things.

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
    /// let format = Format::parse("<MAJOR>.<MINOR>.<PATCH>").unwrap();
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
    /// - Returns a [VersionError::SemanticLevelSpecifierNotInFormat] if `specifier` is not in
    ///   format.
    pub fn next(&self, level: &SemLevel) -> Result<Self, VersionError> {
        let mut spec_found = false;
        let level_spec = level.spec();

        let new_version = self.new_map_value_tokens(|(old_value, this_spec)| {
            if level_spec == this_spec {
                spec_found = true;
            };
            let new_value = this_spec.next_value(old_value, level);
            Ok(new_value)
        })?;

        if !spec_found {
            return Err(VersionError::SemanticSpecifierNotInFormat {
                spec: level_spec.to_string(),
            });
        }

        Ok(new_version)
    }
}

/**
Ways to specify a date.

```
use nextver::Date;

let explicit = Date::Explicit(2021, 2, 3);
let utc_now = Date::UtcNow;
let local_now = Date::LocalNow;
```
**/
#[derive(Debug, Clone, PartialEq)]
pub struct Date(NaiveDate);

impl Date {
    /// Returns a new [Date] representing the current date in UTC at the time of this call.
    pub fn utc_now() -> Self {
        Self(Utc::now().date_naive())
    }

    /// Returns a new [Date] representing the current date in the system's local timezone at the
    /// time of this call.
    pub fn local_now() -> Self {
        Self(Local::now().date_naive())
    }

    /// Returns result of a new [Date] representing the given date, or
    /// [DateError::InvalidDateArguments].
    pub fn explicit(year: i32, month: u32, day: u32) -> Result<Self, DateError> {
        NaiveDate::from_ymd_opt(year, month, day)
            .map(Self)
            .ok_or(DateError::InvalidDateArguments { year, month, day })
    }
}

impl FromStr for Date {
    type Err = DateError;

    /// Parses a date string into a [Date]. The string must be in the format `YYYY-MM-DD`, where
    /// `YYYY` is the year zero-padded to 4 digits, `MM` is the month zero-padded to 2 digits, and
    /// `DD` is the day zero-padded to 2 digits.
    ///
    /// See [NaiveDate::from_str].
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Self(NaiveDate::from_str(s)?))
    }
}

impl Deref for Date {
    type Target = NaiveDate;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for Date {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl<'vs> Version<'vs, Cal> {
    /// Returns a new [Version] where all calendar values in this version are updated to match the
    /// given [Date].
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
    /// let format = Format::parse("<YYYY>.<0M>.<0D>").unwrap();
    /// let version = Version::parse("2023.12.04", format).unwrap();
    /// let new_version = version.update(&Date::Explicit{year: 2024, month: 1, day: 2}).unwrap();
    /// assert_eq!("2024.01.02", new_version.to_string());
    /// assert!(version < new_version);
    /// ```
    ///
    /// # Errors
    ///
    /// - If `date` provided is a [Date::Explicit] and the date values is do not represent a valid
    ///   date, returns a [VersionError::InvalidDateArguments].
    ///
    ///  - Returns a [VersionError::NegativeYearValue]...
    ///
    ///    - If the `date` provided is before year 0 and this version's format uses the `<YYYY>`
    ///      specifier.
    ///
    ///    - If the `date` provided is before the year 2000 and this version's format uses the
    ///      `<YY>` or `<0Y>` specifiers.
    ///
    ///    This is because the formatted values would be negative, which would affect parsing. [See
    ///    specifiers for more](struct.Format.html#specifier-table).
    pub fn next(&self, date: &Date) -> Result<Self, VersionError> {
        // track if the calendar was updated, so we can return NoCalendarChange if it wasn't.
        let mut cal_updated = false;

        let new_version = self.new_map_value_tokens(|(old_value, this_spec)| {
            let new_value = {
                let updated = this_spec.next_value(date)?;
                if updated != *old_value {
                    cal_updated = true;
                }
                updated
            };
            Ok(new_value)
        })?;

        if !cal_updated {
            return Err(VersionError::NoCalendarChange);
        }

        Ok(new_version)
    }
}

impl<'vs> Version<'vs, CalSem> {
    /// Returns a new [Version] where all calendar values in this version are updated to match the
    /// given [Date]. If the calendar values would not change, the version is incremented by the
    /// given [CalSemSemanticSpecifier].
    ///
    /// TODO: fill out rest of this doc
    pub fn next(&self, date: &Date, level: &CalSemLevel) -> Result<Self, VersionError> {
        // track if the semantic level was found in the format string.
        let mut sem_spec_found = false;
        let level_spec = level.spec();

        // track if the calendar was updated, so we know if we need to do semantic updates
        let mut cal_updated = false;

        let new_version = self.new_map_value_tokens(|(old_value, this_spec)| {
            let new_value = if this_spec.is_cal() {
                let new_value = this_spec.next_value(old_value, date, level)?;
                if new_value != *old_value {
                    cal_updated = true;
                }
                new_value
            } else {
                // level_spec can only possibly be equal for non-cal, so check only in this branch
                if level_spec == this_spec {
                    sem_spec_found = true;
                }
                if !cal_updated {
                    this_spec.next_value(old_value, date, level)?
                } else {
                    *old_value
                }
            };

            Ok(new_value)
        })?;

        if !sem_spec_found {
            return Err(VersionError::SemanticSpecifierNotInFormat {
                spec: level_spec.to_string(),
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
            let format = Sem::new_format("<MAJOR>.<MINOR>.<PATCH>").unwrap();
            let version = Version::parse(version_str, &format);
            if *passes {
                assert!(version.is_ok());
            } else {
                assert!(matches!(
                    version,
                    Err(VersionError::VersionFormatMismatch { .. })
                ));
            }
        }
    }

    /// test full year
    #[test]
    fn test_full_year_parse() {
        let format_str = "<YYYY>";
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
                    Err(VersionError::VersionFormatMismatch { .. })
                ));
            }
        }
    }

    /// test short year
    #[test]
    fn test_short_year_parse() {
        let format_str = "<YY>";
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
                    Err(VersionError::VersionFormatMismatch { .. })
                ));
            }
        }
    }

    /// test zero-padded year
    #[test]
    fn test_zp_year_parse() {
        let format_str = "<0Y>";
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
                    Err(VersionError::VersionFormatMismatch { .. })
                ));
            }
        }
    }

    // test zero-padded week
    #[test]
    fn test_zp_week_parse() {
        let args = [
            ("<YYYY>.<0W>", "2024.0", false),   // must be two-digit
            ("<YYYY>.<0W>", "2024.122", false), // must be two-digit
            ("<YYYY>.<0W>", "2024.00", true),   // there is a week zero
            ("<YYYY>.<0W>", "2024.01", true),
            ("<YYYY>.<0W>", "2024.10", true),
            ("<YYYY>.<0W>", "2024.12", true),
        ];

        for (format_str, version_str, passes) in &args {
            let format = Cal::new_format(format_str).unwrap();
            let version = Version::parse(version_str, &format);
            if *passes {
                assert!(version.is_ok());
            } else {
                assert!(matches!(
                    version,
                    Err(VersionError::VersionFormatMismatch { .. })
                ));
            }
        }
    }

    // test short week
    #[test]
    fn test_short_week_parse() {
        let args = [
            ("<YYYY>.<WW>", "2024.01", false),  // zero-padding disallowed
            ("<YYYY>.<WW>", "2024.00", false),  // zero-padding disallowed
            ("<YYYY>.<WW>", "2024.122", false), // must be two-digit
            ("<YYYY>.<WW>", "2024.0", true),
            ("<YYYY>.<WW>", "2024.10", true),
            ("<YYYY>.<WW>", "2024.12", true),
        ];

        for (format_str, version_str, passes) in &args {
            let format = Cal::new_format(format_str).unwrap();
            let version = Version::parse(version_str, &format);
            if *passes {
                assert!(version.is_ok());
            } else {
                assert!(matches!(
                    version,
                    Err(VersionError::VersionFormatMismatch { .. })
                ));
            }
        }
    }

    /// test zero-padded month and day (same characteristics: no month/day 0, 2 digits).
    #[test]
    fn test_zp_month_day_parse() {
        let args = [
            // month
            ("<YYYY>.<0M>", "2024.0", false),   // must be two-digit
            ("<YYYY>.<0M>", "2024.00", false),  // no month 0
            ("<YYYY>.<0M>", "2024.122", false), // must be two-digit
            ("<YYYY>.<0M>", "2024.01", true),
            ("<YYYY>.<0M>", "2024.10", true),
            ("<YYYY>.<0M>", "2024.12", true),
            // day
            ("<YYYY>.<MM>.<0D>", "2024.1.0", false), // must be two-digit
            ("<YYYY>.<MM>.<0D>", "2024.1.00", false), // no day 0
            ("<YYYY>.<MM>.<0D>", "2024.1.122", false), // must be two-digit
            ("<YYYY>.<MM>.<0D>", "2024.1.01", true),
            ("<YYYY>.<MM>.<0D>", "2024.1.10", true),
            ("<YYYY>.<MM>.<0D>", "2024.1.12", true),
        ];

        for (format_str, version_str, passes) in &args {
            let format = Cal::new_format(format_str).unwrap();
            let version = Version::parse(version_str, &format);
            if *passes {
                assert!(version.is_ok());
            } else {
                assert!(matches!(
                    version,
                    Err(VersionError::VersionFormatMismatch { .. })
                ));
            }
        }
    }

    /// test short month, day, and week. (same characteristics: no month/day 0, 1-2 digits).
    #[test]
    fn test_short_mdw_parse() {
        let args = [
            // month
            ("<YYYY>.<MM>", "2024.0", false),   // no month 0
            ("<YYYY>.<MM>", "2024.00", false),  // zero-padding disallowed
            ("<YYYY>.<MM>", "2024.01", false),  // zero-padding disallowed
            ("<YYYY>.<MM>", "2024.122", false), // must be two-digit
            ("<YYYY>.<MM>", "2024.1", true),
            ("<YYYY>.<MM>", "2024.10", true),
            ("<YYYY>.<MM>", "2024.12", true),
            // day
            ("<YYYY>.<MM>.<DD>", "2024.1.0", false), // no month 0
            ("<YYYY>.<MM>.<DD>", "2024.1.00", false), // zero-padding disallowed
            ("<YYYY>.<MM>.<DD>", "2024.1.01", false), // zero-padding disallowed
            ("<YYYY>.<MM>.<DD>", "2024.1.122", false), // must be two-digit
            ("<YYYY>.<MM>.<DD>", "2024.1.1", true),
            ("<YYYY>.<MM>.<DD>", "2024.1.10", true),
            ("<YYYY>.<MM>.<DD>", "2024.1.12", true),
        ];

        for (format_str, version_str, passes) in &args {
            let format = Cal::new_format(format_str).unwrap();
            let version = Version::parse(version_str, &format);
            if *passes {
                assert!(version.is_ok());
            } else {
                assert!(matches!(
                    version,
                    Err(VersionError::VersionFormatMismatch { .. })
                ));
            }
        }
    }

    #[test]
    fn test_unicode_literal() {
        let format_str = "👍<MAJOR>👯‍♀️";
        let version_str = "👍1👯‍♀️";
        let format = Sem::new_format(format_str).unwrap();
        let version = Version::parse(version_str, &format);
        assert!(version.is_ok());
    }
}
