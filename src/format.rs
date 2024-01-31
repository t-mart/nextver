use crate::{
    error::{FormatError, VersionError},
    scheme::Scheme,
    specifier::Specifier,
    version::Version,
};
use core::fmt::{self, Display};
use regex::Regex;
use std::borrow::Cow;
use std::cell::OnceCell;
use std::cmp::Ordering;

#[derive(Debug, Eq, PartialEq)]
pub(crate) enum FormatToken<'fs, S: Scheme> {
    Specifier(&'static S::Specifier),
    Literal(&'fs str),
}

impl<'fs, S: Scheme> Clone for FormatToken<'fs, S> {
    fn clone(&self) -> Self {
        match self {
            FormatToken::Specifier(spec) => FormatToken::Specifier(*spec),
            FormatToken::Literal(text) => FormatToken::Literal(text),
        }
    }
}

impl<'fs, S: Scheme> FormatToken<'fs, S> {
    fn version_pattern_group(&self) -> Cow<'static, str> {
        match self {
            FormatToken::Specifier(spec) => Cow::Borrowed(spec.version_pattern()),
            FormatToken::Literal(text) => Cow::Owned(format!("({})", regex::escape(text))),
        }
    }
}

impl<'fs, S: Scheme> Display for FormatToken<'fs, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FormatToken::Specifier(spec) => write!(f, "{}", spec),
            FormatToken::Literal(text) => write!(f, "{}", text.replace('[', r"\[")),
        }
    }
}

/**
A Format describes the structure of a version. It's created from a string that contains
specifiers indicating what each part of the string means. Later, the Format can be used to parse
a version string into a [Version](crate::version::Version) struct.

The format string is made up of specifiers and literals. Specifiers indicate numeric values that
can change, while literals are fixed text.

See the specifier table [here](crate#specifier-table) for a list of all specifiers.

## Specifier Order

- Semantic and calendar specifiers must appear in strictly decreasing order of
  their period going left-to-right, such as `[YYYY]` before `[MM]` and `[MAJOR]`
  before `[MINOR]`. This also implies that multiple specifiers of the same
  period are forbidden.

  This is for ordering, so that incremented versions always compare greater than
  their originals.

  - `[YYYY].[DD].[MM]` ❌ day before month
  - `[MAJOR].[PATCH].[MINOR]` ❌ patch before minor
  - `[YYYY].[MM].[0M]` ❌ two month specifiers
  - `[MAJOR].[MINOR].[MINOR]` ❌ two minor specifiers

  This also has implications for week and month/day specifiers. Both week and
  month are year-relative, and therefore have different periods. And
  transitively, days are month-relative, so they are also forbidden from being
  in formats with weeks.

  - `[YYYY].[WW].[DD]` ❌ week and day
  - `[YYYY].[MM].[WW]` ❌ month and week

- There must be one and only one non-cyclic specifier in a format, and it must
  be the first one. The non-cyclic specifiers are `[MAJOR]` and any derived from
  the year (`[YYYY]`, `[YY]`, and `[0Y]`).

  This is also for ordering. Month/week/day and minor/patch values are cyclic,
  and comparisons may be unexpected *unless* those values are grounded by a
  first non-cyclic value.

  - `[YYYY].[MAJOR]` ❌ two non-cyclic specifiers

- All calendar specifiers must precede semantic ones if both are present in a
  format.

  Calendar values move independently of semantic ones. Partitioning this way
  maintains logical clarity.

  - `[YYYY].[MINOR].[MM]` ❌ calendar, then semantic, then calendar
  - `[MAJOR].[DD].[PATCH]` ❌ semantic, then calendar, then semantic

- For the full year specifier (`[YYYY]`), years less than `1` (the [BCE
  era](https://en.wikipedia.org/wiki/Common_Era)) are forbidden, making them
  at least 1 digit, usually 4, possibly more, and definitely non-negative.

  If BCE years were allowed, the resulting value would be negative, and require
  a sign character that would affect parsing.

- For the short year (`[YY]`) and zero-padded year (`[0Y]`) specifiers, years
  less than `2000` are forbidden.

  This is for the same reasons as the previous. While CalVer specifies that
  years in this format are relative to the year `2000`, it does not clearly
  forbid lesser years, but we do.

**/
#[derive(Debug, Clone)]
pub struct Format<'fs, S: Scheme> {
    pub(crate) tokens: Vec<FormatToken<'fs, S>>,
    regex: OnceCell<Regex>,
}

impl<'fs, S: Scheme> Format<'fs, S> {
    pub(crate) fn new(tokens: Vec<FormatToken<'fs, S>>, regex: OnceCell<Regex>) -> Self {
        Self { tokens, regex }
    }

    pub(crate) fn from_tokens(tokens: Vec<FormatToken<'fs, S>>) -> Self {
        Self::new(tokens, OnceCell::new())
    }

    /// Get the regex pattern for this format. Caches the regex if it hasn't been created yet.
    pub(crate) fn get_regex(&self) -> &Regex {
        self.regex.get_or_init(|| {
            let mut str_cap = 0usize;
            let mut tokens = Vec::with_capacity(self.tokens.len());
            tokens.extend(self.tokens.iter().map(|token| {
                let pattern = token.version_pattern_group();
                str_cap += pattern.len();
                pattern
            }));

            // +2 for ^ and $
            let mut pattern = String::with_capacity(str_cap + 2);
            pattern.push('^');
            for token in tokens {
                pattern.push_str(&token);
            }
            pattern.push('$');
            Regex::new(&pattern).unwrap()
        })
    }

    /// Parse a format string into a Format struct.
    ///
    /// # Errors
    ///
    /// Returns an `Err` of ...
    ///
    /// - [`NextverError::UnknownSpecifier`] if an unknown specifier is found (e.g., `[FOO]`).
    /// - [`NextverError::UnterminatedSpecifier`] if a specifier is not terminated with a
    ///   closing square bracket (`]`).
    pub(crate) fn parse(format_str: &'fs str) -> Result<Self, FormatError> {
        let mut format = format_str;
        let mut tokens = Vec::with_capacity(S::max_tokens());
        let mut last_spec: Option<&'static S::Specifier> = None;

        while !format.is_empty() {
            let matched_spec =
                S::Specifier::iter_all().find(|spec| format.starts_with(spec.format_pattern()));

            let consume_len = if let Some(spec) = matched_spec {
                // check that specifiers are in order
                if let Some(last_spec) = last_spec {
                    // if !(last_spec > spec) {
                    if matches!(
                        last_spec.partial_cmp(spec),
                        Some(Ordering::Less) | Some(Ordering::Equal) | None
                    ) {
                        return Err(FormatError::SpecifiersMustStepDecrease {
                            prev: last_spec.to_string(),
                            next: spec.to_string(),
                        });
                    }
                } else {
                    // check that this is an ok first spec
                    if !spec.can_be_first() {
                        return Err(FormatError::WrongFirstSpecifier {
                            first_spec: spec.to_string(),
                            scheme_name: S::name(),
                            expected_first: S::first_variants_string(),
                        });
                    }
                }
                last_spec = Some(spec);
                tokens.push(FormatToken::Specifier(*spec));
                spec.format_pattern().len()
            } else {
                // check if its escaped brackets, an unknown/unterminated specifier, or finally,
                // just a literal.
                let (literal, consume_len) = if format.starts_with('[') {
                    // determine if unknown or unterminated specifier. we technically don't need to
                    // error here: could just parse this as a literal because, above, we've already
                    // exhausted all known specifiers, but this helps the user.
                    let mut next_chars = format.chars().skip(1);
                    let closing_index = next_chars
                        .position(|c| c == ']')
                        .map(|index| {
                            // 1 for opening bracket that we skipped
                            index + 1
                        })
                        .ok_or_else(|| {
                            // didn't find closing bracket
                            FormatError::UnterminatedSpecifier {
                                pattern: format.to_owned(),
                            }
                        })?;
                    // found closing, but unknown for this scheme
                    return Err(FormatError::UnacceptableSpecifier {
                        spec: format[..closing_index + 1].to_owned(),
                        scheme_name: S::name(),
                    });
                } else if format.starts_with(r"\[") {
                    // escaped opening bracket
                    (&format[1..2], 2)
                } else {
                    // any other literal.
                    //
                    // gotta use chars() to find byte length to consume (for non-ascii, it's more
                    // than 1 byte). otherwise, rust complains we're splitting at a non-char
                    // boundary index
                    let len_next_char = format.chars().next().unwrap().len_utf8();
                    (&format[0..len_next_char], len_next_char)
                };

                // we can add this literal to the last token if it was also a literal.
                // this will help us cut down on the total number of tokens and therefore, regex
                // groups later.
                if let Some(FormatToken::Literal(last_literal)) = tokens.last_mut() {
                    // fast str "concat": we just increase the length of the last literal by the
                    // size of the new literal. this works because the additional char is in
                    // contiguous memory and we know that the length of the underlying string is at
                    // least this long.
                    *last_literal = unsafe {
                        std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                            last_literal.as_ptr(),              //same ptr
                            last_literal.len() + literal.len(), // new len
                        ))
                    };
                } else {
                    tokens.push(FormatToken::Literal(literal));
                }

                consume_len
            };

            format = &format[consume_len..];
        }

        if let Some(last_spec) = last_spec {
            if !last_spec.can_be_last() {
                return Err(FormatError::Incomplete {
                    last_spec: last_spec.to_string(),
                    scheme_name: S::name(),
                    expected_last: S::last_variants_string(),
                });
            }
        } else {
            return Err(FormatError::NoSpecifiersInFormat);
        }

        Ok(Self::from_tokens(tokens))
    }

    /// Create a new version from this format and a version string.
    ///
    /// This is equivalent to calling `format.parse_version(version_str)`.
    pub fn new_version<'vs>(&self, version_str: &'vs str) -> Result<Version<'vs, S>, VersionError> {
        Version::parse(version_str, self)
    }
}

impl<'fs, S: Scheme> PartialEq for Format<'fs, S> {
    /// Returns true if two formats would have the same string representation.
    ///
    /// *Internally, [Format] holds a [OnceCell] of a cached regex, and this is ignored in this
    /// comparison.*
    // In nightly, clippy currently calling this an `unconditional_recursion`. Almost certainly a
    // false positive. Bug reports stating the same.
    fn eq(&self, other: &Self) -> bool {
        self.tokens.eq(&other.tokens)
    }
}

impl<'fs, S: Scheme> Display for Format<'fs, S> {
    /// Display a format as a format string.
    ///
    /// # Example
    ///
    /// ```
    /// use nextver::prelude::*;
    ///
    /// let format_str = "[YYYY].[MM].[DD]";
    /// let format = Cal::new_format(format_str).unwrap();
    /// assert_eq!(format_str, format.to_string());
    /// ```
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for token in &self.tokens {
            write!(f, "{}", token)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        scheme::{priv_trait::Scheme as _, Cal, CalSem, Scheme, Sem},
        specifier::{
            CALSEM_DAY_SHORT, CALSEM_DAY_ZERO_PADDED, CALSEM_MINOR, CALSEM_MONTH_SHORT,
            CALSEM_MONTH_ZERO_PADDED, CALSEM_PATCH, CALSEM_WEEK_SHORT, CALSEM_WEEK_ZERO_PADDED,
            CALSEM_YEAR_FULL, CALSEM_YEAR_SHORT, CALSEM_YEAR_ZERO_PADDED, CAL_DAY_SHORT,
            CAL_DAY_ZERO_PADDED, CAL_MONTH_SHORT, CAL_MONTH_ZERO_PADDED, CAL_WEEK_SHORT,
            CAL_WEEK_ZERO_PADDED, CAL_YEAR_FULL, CAL_YEAR_SHORT, CAL_YEAR_ZERO_PADDED, SEM_MAJOR,
            SEM_MINOR, SEM_PATCH,
        },
    };
    use itertools::Itertools;
    use rstest::*;
    use std::iter;

    #[fixture]
    fn literal_parts() -> [&'static str; 3] {
        static LITERAL_PARTS: [&str; 3] = [
            "the quick brown fox jumps over the lazy dog",
            "😉",
            "the end",
        ];
        LITERAL_PARTS
    }

    #[rstest]
    fn test_sem_literal(literal_parts: [&'static str; 3]) {
        let [first, middle, last] = literal_parts;
        let format_str = &format!("{}[MAJOR]{}[MINOR]{}", &first, &middle, &last);

        let sem_format = Sem::new_format(format_str);
        assert_eq!(
            Ok(vec![
                FormatToken::Literal(first),
                FormatToken::Specifier(&SEM_MAJOR),
                FormatToken::Literal(middle),
                FormatToken::Specifier(&SEM_MINOR),
                FormatToken::Literal(last),
            ]),
            sem_format.map(|f| f.tokens)
        );
    }

    #[rstest]
    fn test_cal_literal(literal_parts: [&'static str; 3]) {
        let [first, middle, last] = literal_parts;
        let format_str = &format!("{}[YYYY]{}[MM]{}", &first, &middle, &last);

        let sem_format = Cal::new_format(format_str);
        assert_eq!(
            Ok(vec![
                FormatToken::Literal(first),
                FormatToken::Specifier(&CAL_YEAR_FULL),
                FormatToken::Literal(middle),
                FormatToken::Specifier(&CAL_MONTH_SHORT),
                FormatToken::Literal(last),
            ]),
            sem_format.map(|f| f.tokens)
        );
    }

    #[rstest]
    fn test_calsem_literal(literal_parts: [&'static str; 3]) {
        let [first, middle, last] = literal_parts;
        let format_str = &format!("{}[YYYY]{}[MINOR]{}", &first, &middle, &last);

        let sem_format = CalSem::new_format(format_str);
        assert_eq!(
            Ok(vec![
                FormatToken::Literal(first),
                FormatToken::Specifier(&CALSEM_YEAR_FULL),
                FormatToken::Literal(middle),
                FormatToken::Specifier(&CALSEM_MINOR),
                FormatToken::Literal(last),
            ]),
            sem_format.map(|f| f.tokens)
        );
    }

    /// A vec of tuples of (specifier, pattern). while we could just derive the pattern from the
    /// specifier, i want this to be an explicit value so that we can test patterns are what we
    /// expect them to be.
    // Clippy false positive, possibly just for nightly.
    // TODO: try to remove `allow` in the future.
    #[allow(type_alias_bounds)]
    type SpecifierTupleVec<S: Scheme> = Vec<(&'static S::Specifier, &'static str)>;

    /// Returns an iterator of two-tuples of (specifier, pattern) for all valid calendar specifiers.
    ///
    /// These are:
    ///
    /// - `[<year>]`
    /// - `[<year>]`, `[<month>]`
    /// - `[<year>]`, `[<month>]`, `[<day>]`
    /// - `[<year>]`, `[<week>]`
    #[fixture]
    fn all_valid_cal_specs_product() -> impl Iterator<Item = SpecifierTupleVec<Cal>> {
        let years = iter::once(vec![
            (&CAL_YEAR_FULL, "[YYYY]"),
            (&CAL_YEAR_SHORT, "[YY]"),
            (&CAL_YEAR_ZERO_PADDED, "[0Y]"),
        ]);
        let months = iter::once(vec![
            (&CAL_MONTH_SHORT, "[MM]"),
            (&CAL_MONTH_ZERO_PADDED, "[0M]"),
        ]);
        let weeks = iter::once(vec![
            (&CAL_WEEK_SHORT, "[WW]"),
            (&CAL_WEEK_ZERO_PADDED, "[0W]"),
        ]);
        let days = iter::once(vec![
            (&CAL_DAY_SHORT, "[DD]"),
            (&CAL_DAY_ZERO_PADDED, "[0D]"),
        ]);

        let years_product = years.clone().multi_cartesian_product();
        let years_months_product = years
            .clone()
            .chain(months.clone())
            .multi_cartesian_product();
        let years_months_days_product = years
            .clone()
            .chain(months)
            .chain(days)
            .multi_cartesian_product();
        let years_weeks_product = years.chain(weeks).multi_cartesian_product();

        years_product
            .chain(years_months_product)
            .chain(years_months_days_product)
            .chain(years_weeks_product)
    }

    /// Returns an iterator of two-tuples of (specifier, pattern) for all valid calendar specifiers.
    ///
    /// These are:
    ///
    /// - `[<year>]`, REST
    /// - `[<year>]`, `[<month>]`, REST
    /// - `[<year>]`, `[<month>]`, `[<day>]`, REST
    /// - `[<year>]`, `[<week>]`, REST
    ///
    /// where REST is:
    ///
    /// - `[MINOR]`
    /// - `[MINOR]`, `[PATCH]`
    /// - `[PATCH]`
    #[fixture]
    fn all_valid_calsem_specs_product() -> impl Iterator<Item = SpecifierTupleVec<CalSem>> {
        let years = iter::once(vec![
            (&CALSEM_YEAR_FULL, "[YYYY]"),
            (&CALSEM_YEAR_SHORT, "[YY]"),
            (&CALSEM_YEAR_ZERO_PADDED, "[0Y]"),
        ]);
        let months = iter::once(vec![
            (&CALSEM_MONTH_SHORT, "[MM]"),
            (&CALSEM_MONTH_ZERO_PADDED, "[0M]"),
        ]);
        let weeks = iter::once(vec![
            (&CALSEM_WEEK_SHORT, "[WW]"),
            (&CALSEM_WEEK_ZERO_PADDED, "[0W]"),
        ]);
        let days = iter::once(vec![
            (&CALSEM_DAY_SHORT, "[DD]"),
            (&CALSEM_DAY_ZERO_PADDED, "[0D]"),
        ]);

        let years_product = years.clone().multi_cartesian_product();
        let years_months_product = years
            .clone()
            .chain(months.clone())
            .multi_cartesian_product();
        let years_months_days_product = years
            .clone()
            .chain(months)
            .chain(days)
            .multi_cartesian_product();
        let years_weeks_product = years.chain(weeks).multi_cartesian_product();

        years_product
            .chain(years_months_product)
            .chain(years_months_days_product)
            .chain(years_weeks_product)
            // augment each of these products with the three possible semantic suffix combinations
            .flat_map(|iter| {
                vec![
                    [iter.clone(), vec![(&CALSEM_MINOR, "[MINOR]")]].concat(),
                    [
                        iter.clone(),
                        vec![(&CALSEM_MINOR, "[MINOR]")],
                        vec![(&CALSEM_PATCH, "[PATCH]")],
                    ]
                    .concat(),
                    [iter, vec![(&CALSEM_PATCH, "[PATCH]")]].concat(),
                ]
            })
    }

    /// test all semantic format sequences parse ok. these are:
    ///
    /// - `[MAJOR]`
    /// - `[MAJOR]`, `[MINOR]`
    /// - `[MAJOR]`, `[MINOR]`, `[PATCH]`
    #[test]
    fn test_sem_parse_ok() {
        let args = [
            (
                "[MAJOR][MINOR][PATCH]",
                vec![&SEM_MAJOR, &SEM_MINOR, &SEM_PATCH],
            ),
            ("[MAJOR][MINOR]", vec![&SEM_MAJOR, &SEM_MINOR]),
            ("[MAJOR]", vec![&SEM_MAJOR]),
        ];

        for (format_string, spec) in args {
            let tokens = spec
                .iter()
                .map(|spec| FormatToken::Specifier(*spec))
                .collect();
            let actual = Sem::new_format(format_string);
            assert_eq!(Ok(tokens), actual.map(|f| f.tokens));
        }
    }

    /// test all calendar format sequences parse ok. these are:
    ///
    /// - `[<year>]`
    /// - `[<year>]`, `[<month>]`
    /// - `[<year>]`, `[<month>]`, `[<day>]`
    /// - `[<year>]`, `[<week>]`
    #[rstest]
    fn test_cal_parse_ok(
        all_valid_cal_specs_product: impl Iterator<Item = SpecifierTupleVec<Cal>>,
    ) {
        for spec_sequence in all_valid_cal_specs_product {
            let mut format_string = String::new();
            let mut tokens = vec![];
            for (spec, pattern) in spec_sequence {
                format_string.push_str(pattern);
                tokens.push(FormatToken::Specifier(spec));
            }
            let actual = Cal::new_format(&format_string);
            assert_eq!(Ok(tokens), actual.map(|f| f.tokens));
        }
    }

    /// test all calendar-semantic format sequences parse ok. these are:
    ///
    /// - `[<year>]`, `[MINOR]`
    /// - `[<year>]`, `[MINOR]`, `[PATCH]`
    /// - `[<year>]`, `[PATCH]`
    /// - `[<year>]`, `[<month>]`, `[MINOR]`
    /// - `[<year>]`, `[<month>]`, `[MINOR]`, `[PATCH]`
    /// - `[<year>]`, `[<month>]`, `[PATCH]`
    /// - `[<year>]`, `[<month>]`, `[<day>]`, `[MINOR]`
    /// - `[<year>]`, `[<month>]`, `[<day>]`, `[MINOR]`, `[PATCH]`
    /// - `[<year>]`, `[<month>]`, `[<day>]`, `[PATCH]`
    /// - `[<year>]`, `[<week>]`, `[MINOR]`
    /// - `[<year>]`, `[<week>]`, `[MINOR]`, `[PATCH]`
    /// - `[<year>]`, `[<week>]`, `[PATCH]`
    #[rstest]
    fn test_calsem_parse_ok(
        all_valid_calsem_specs_product: impl Iterator<Item = SpecifierTupleVec<CalSem>>,
    ) {
        for spec_sequence in all_valid_calsem_specs_product {
            let mut format_string = String::new();
            let mut tokens = vec![];
            for (spec, pattern) in spec_sequence {
                format_string.push_str(pattern);
                tokens.push(FormatToken::Specifier(spec));
            }
            let actual = CalSem::new_format(&format_string);
            assert_eq!(Ok(tokens), actual.map(|f| f.tokens));
        }
    }

    #[test]
    fn test_bad_sem_format() {
        use crate::error::FormatError::*;

        // not exhaustive, just a sample
        let args = [
            ("", NoSpecifiersInFormat),
            ("foo", NoSpecifiersInFormat),
            (
                "[MINOR]",
                WrongFirstSpecifier {
                    first_spec: SEM_MINOR.to_string(),
                    scheme_name: Sem::name(),
                    expected_first: Sem::first_variants_string(),
                },
            ),
            (
                "[MAJOR][MAJOR]",
                SpecifiersMustStepDecrease {
                    prev: SEM_MAJOR.to_string(),
                    next: SEM_MAJOR.to_string(),
                },
            ),
            (
                "[MAJOR][PATCH]",
                SpecifiersMustStepDecrease {
                    prev: SEM_MAJOR.to_string(),
                    next: SEM_PATCH.to_string(),
                },
            ),
            (
                "[MAJOR][YYYY]",
                UnacceptableSpecifier {
                    spec: "[YYYY]".to_string(),
                    scheme_name: Sem::name(),
                },
            ),
            (
                "[MAJOR",
                UnterminatedSpecifier {
                    pattern: "[MAJOR".to_string(),
                },
            ),
        ];

        for (format, err) in args {
            let actual = Sem::new_format(format);
            assert_eq!(Err(err), actual);
        }
    }

    #[test]
    fn test_bad_cal_format() {
        use crate::error::FormatError::*;

        // not exhaustive, just a sample
        let args = [
            ("", NoSpecifiersInFormat),
            ("foo", NoSpecifiersInFormat),
            (
                "[MM]",
                WrongFirstSpecifier {
                    first_spec: CAL_MONTH_SHORT.to_string(),
                    scheme_name: Cal::name(),
                    expected_first: Cal::first_variants_string(),
                },
            ),
            (
                "[YYYY][MINOR]",
                UnacceptableSpecifier {
                    spec: "[MINOR]".to_string(),
                    scheme_name: Cal::name(),
                },
            ),
            (
                "[YYYY][MM][WW]",
                SpecifiersMustStepDecrease {
                    prev: CAL_MONTH_SHORT.to_string(),
                    next: CAL_WEEK_SHORT.to_string(),
                },
            ),
            (
                "[YYYY][DD]",
                SpecifiersMustStepDecrease {
                    prev: CAL_YEAR_FULL.to_string(),
                    next: CAL_DAY_SHORT.to_string(),
                },
            ),
            (
                "[YYYY",
                UnterminatedSpecifier {
                    pattern: "[YYYY".to_string(),
                },
            ),
        ];

        for (format, err) in args {
            let actual = Cal::new_format(format);
            assert_eq!(Err(err), actual);
        }
    }

    #[test]
    fn test_bad_calendar_semantic_format() {
        use crate::error::FormatError::*;

        // not exhaustive, just a sample
        let args = [
            ("", NoSpecifiersInFormat),
            ("foo", NoSpecifiersInFormat),
            (
                "[YYYY",
                UnterminatedSpecifier {
                    pattern: "[YYYY".to_string(),
                },
            ),
            (
                "[YYYY][FOO]",
                UnacceptableSpecifier {
                    spec: "[FOO]".to_string(),
                    scheme_name: CalSem::name(),
                },
            ),
            (
                "[YYYY]",
                Incomplete {
                    last_spec: CALSEM_YEAR_FULL.to_string(),
                    scheme_name: CalSem::name(),
                    expected_last: CalSem::last_variants_string(),
                },
            ),
            (
                "[MM]",
                WrongFirstSpecifier {
                    first_spec: CAL_MONTH_SHORT.to_string(),
                    scheme_name: CalSem::name(),
                    expected_first: CalSem::first_variants_string(),
                },
            ),
            (
                "[YYYY][MM][WW][PATCH]",
                SpecifiersMustStepDecrease {
                    prev: CALSEM_MONTH_SHORT.to_string(),
                    next: CALSEM_WEEK_SHORT.to_string(),
                },
            ),
            (
                "[YYYY][DD][MINOR]",
                SpecifiersMustStepDecrease {
                    prev: CALSEM_YEAR_FULL.to_string(),
                    next: CAL_DAY_SHORT.to_string(),
                },
            ),
        ];

        for (format, err) in args {
            let actual = CalSem::new_format(format);
            assert_eq!(Err(err), actual);
        }
    }

    #[test]
    fn test_bracket_escape() {
        let format = r"[YYYY]\[YYYY]";
        let actual = Cal::new_format(format);
        assert_eq!(
            Ok(vec![
                FormatToken::Specifier(&CAL_YEAR_FULL),
                FormatToken::Literal("[YYYY]"),
            ])
            .as_ref(),
            actual.as_ref().map(|f| &f.tokens)
        );
        let round_tripped_format = actual.unwrap().to_string();
        assert_eq!(format, round_tripped_format);
    }
}
