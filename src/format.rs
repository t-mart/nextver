use crate::scheme::Scheme;
use crate::specifier::{Specifier, ALL};
use crate::{NextVerError, Version};
use core::fmt::{self, Display};
use regex::Regex;
use std::borrow::Cow;
use std::cell::OnceCell;
use std::marker::PhantomData;

/// State machine for parsing the specifiers in a format string.
pub(crate) enum ParseState {
    Initial,
    Major,
    Minor,
    Patch,
    Year(&'static Specifier),
    Month(&'static Specifier),
    Week(&'static Specifier),
    Day(&'static Specifier),
}

impl ParseState {
    pub(crate) fn new() -> Self {
        ParseState::Initial
    }
}

#[derive(Debug)]
pub(crate) enum FormatToken<'fs> {
    Specifier(&'static Specifier),
    Literal(&'fs str),
}

impl<'fs> FormatToken<'fs> {
    fn version_pattern_group(&self) -> Cow<'static, str> {
        match self {
            FormatToken::Specifier(spec) => Cow::Borrowed(spec.version_pattern()),
            FormatToken::Literal(text) => Cow::Owned(format!("({})", regex::escape(text))),
        }
    }
}

impl<'fs> Clone for FormatToken<'fs> {
    fn clone(&self) -> Self {
        match self {
            FormatToken::Specifier(spec) => FormatToken::Specifier(spec),
            FormatToken::Literal(text) => FormatToken::Literal(text),
        }
    }
}

impl<'fs> PartialEq for FormatToken<'fs> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (FormatToken::Specifier(spec1), FormatToken::Specifier(spec2)) => {
                std::ptr::eq(*spec1, *spec2)
            }
            (FormatToken::Literal(text1), FormatToken::Literal(text2)) => text1 == text2,
            _ => false,
        }
    }
}

impl<'fs> Display for FormatToken<'fs> {
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
    pub(crate) tokens: Vec<FormatToken<'fs>>,
    regex: OnceCell<Regex>,
    scheme: PhantomData<S>,
}

impl<'fs, S: Scheme> Format<'fs, S> {
    pub(crate) fn new(tokens: Vec<FormatToken<'fs>>, regex: OnceCell<Regex>) -> Self {
        Self {
            tokens,
            regex,
            scheme: PhantomData,
        }
    }

    pub(crate) fn from_tokens(tokens: Vec<FormatToken<'fs>>) -> Self {
        Self::new(tokens, OnceCell::new())
    }

    /// Get the regex pattern for this format. Caches the regex if it hasn't been created yet.
    pub(crate) fn get_regex(&self) -> &Regex {
        self.regex.get_or_init(|| {
            let tokens_subpattern = self
                .tokens
                .iter()
                .map(|format_token| {
                    debug_assert!({
                        let pattern = format_token.version_pattern_group();
                        pattern.starts_with('(') && pattern.ends_with(')') && pattern.len() > 2
                    });
                    format_token.version_pattern_group()
                })
                .collect::<Vec<_>>()
                .join("");
            let pattern = format!("^{tokens_subpattern}$");
            Regex::new(&pattern).unwrap()
        })
    }

    /// Parse a format string into a Format struct.
    ///
    /// # Errors
    ///
    /// Returns an `Err` of ...
    ///
    /// - [`NextVerError::UnknownSpecifier`] if an unknown specifier is found (e.g., `[FOO]`).
    /// - [`NextVerError::UnterminatedSpecifier`] if a specifier is not terminated with a
    ///   closing square bracket (`]`).
    pub(crate) fn parse(format_str: &'fs str) -> Result<Self, NextVerError> {
        let mut format = format_str;
        let mut parse_state = ParseState::new();
        let mut tokens = Vec::with_capacity(S::max_tokens());
        let mut last_spec = None;

        while !format.is_empty() {
            let matched_spec = 'spec_find: {
                for spec in ALL {
                    let format_pattern = spec.format_pattern();

                    debug_assert!({
                        format_pattern.starts_with('[')
                            && format_pattern.ends_with(']')
                            && format_pattern.len() > 2
                    });

                    if format.starts_with(format_pattern) {
                        break 'spec_find Some(spec);
                    }
                }
                None
            };

            let consume_len = if let Some(spec) = matched_spec {
                // check that specifiers are in order]
                S::advance_parse_state(&mut parse_state, spec)?;
                tokens.push(FormatToken::Specifier(spec));
                last_spec = Some(spec);
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
                            NextVerError::UnterminatedSpecifier {
                                pattern: format.to_owned(),
                            }
                        })?;
                    // found closing, but unknown
                    return Err(NextVerError::UnknownSpecifier {
                        pattern: format[..closing_index + 1].to_owned(),
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
                    // size of the new literal. this works because the addition is in contiguous
                    // memory and we know that the length of the underlying string is at least this
                    // long.
                    let ptr = last_literal.as_ptr();
                    let new_len = last_literal.len() + literal.len();
                    *last_literal = unsafe {
                        std::str::from_utf8_unchecked(std::slice::from_raw_parts(ptr, new_len))
                    };
                } else {
                    tokens.push(FormatToken::Literal(literal));
                }

                consume_len
            };

            format = &format[consume_len..];
        }

        if let Some(last_spec) = last_spec {
            if !S::parse_state_is_final(&parse_state) {
                return Err(NextVerError::FormatIncomplete { last_spec });
            }
        } else {
            return Err(NextVerError::NoSpecifiersInFormat);
        }

        Ok(Self::from_tokens(tokens))
    }

    pub fn parse_version<'vs>(
        &self,
        version_str: &'vs str,
    ) -> Result<Version<'vs, S>, NextVerError> {
        Version::parse(version_str, self)
    }
}

impl<'fs, S: Scheme> PartialEq for Format<'fs, S> {
    /// Returns true if the two formats would have the same string representation.
    fn eq(&self, other: &Self) -> bool {
        self.tokens == other.tokens
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
        scheme::{Cal, CalSem, Scheme, Sem},
        specifier::{
            FULL_YEAR, MAJOR, MINOR, PATCH, SHORT_DAY, SHORT_MONTH, SHORT_WEEK, SHORT_YEAR,
            YEAR_FORMAT_STRINGS, ZERO_PADDED_DAY, ZERO_PADDED_MONTH, ZERO_PADDED_WEEK,
            ZERO_PADDED_YEAR,
        },
    };
    use itertools::Itertools;
    use rstest::*;
    use std::iter;

    // a vec of tuples of (specifier, pattern)
    // while we could just derive the pattern from the specifier, i want this to be an explicit
    // value so that we can test patterns are what we expect them to be.
    type SpecifierTupleVec = Vec<(&'static Specifier, &'static str)>;

    #[fixture]
    fn years() -> impl Iterator<Item = SpecifierTupleVec> + Clone {
        iter::once(vec![
            (&FULL_YEAR, "[YYYY]"),
            (&SHORT_YEAR, "[YY]"),
            (&ZERO_PADDED_YEAR, "[0Y]"),
        ])
    }

    #[fixture]
    fn months() -> impl Iterator<Item = SpecifierTupleVec> + Clone {
        iter::once(vec![(&SHORT_MONTH, "[MM]"), (&ZERO_PADDED_MONTH, "[0M]")])
    }

    #[fixture]
    fn weeks() -> impl Iterator<Item = SpecifierTupleVec> + Clone {
        iter::once(vec![(&SHORT_WEEK, "[WW]"), (&ZERO_PADDED_WEEK, "[0W]")])
    }

    #[fixture]
    fn days() -> impl Iterator<Item = SpecifierTupleVec> + Clone {
        iter::once(vec![(&SHORT_DAY, "[DD]"), (&ZERO_PADDED_DAY, "[0D]")])
    }

    /// Returns an iterator of two-tuples of (specifier, pattern) for all valid calendar specifiers.
    ///
    /// These are:
    ///
    /// - `[<year>]`
    /// - `[<year>]`, `[<month>]`
    /// - `[<year>]`, `[<month>]`, `[<day>]`
    /// - `[<year>]`, `[<week>]`
    ///
    /// E.g., eliding the specifiers:
    ///
    /// - `[YYYY]`
    /// - `[YY]`
    /// - `[0Y]`
    /// - `[YYYY]`, `[MM]`
    /// - `[YYYY]`, `[0M]`
    /// - `[YY]`, `[MM]`
    /// - `[YY]`, `[0M]`
    /// - `[0Y]`, `[MM]`
    /// - `[0Y]`, `[0M]`
    /// - `[YYYY]`, `[MM]`, `[DD]`
    /// - `[YYYY]`, `[MM]`, `[0D]`
    /// and so on.
    #[fixture]
    fn all_calendar_specs_product(
        years: impl Iterator<Item = SpecifierTupleVec> + Clone,
        months: impl Iterator<Item = SpecifierTupleVec> + Clone,
        weeks: impl Iterator<Item = SpecifierTupleVec> + Clone,
        days: impl Iterator<Item = SpecifierTupleVec> + Clone,
    ) -> impl Iterator<Item = SpecifierTupleVec> {
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

    #[test]
    fn test_literal() {
        let literal_front = "the quick brown fox jumps over the lazy dog";
        let literal_middle = "😉";
        let literal_back = "the end";
        let format_string = format!("{literal_front}[MAJOR]{literal_middle}[MINOR]{literal_back}");
        let format = Sem::new_format(&format_string);
        assert_eq!(
            Ok(vec![
                FormatToken::Literal(literal_front),
                FormatToken::Specifier(&MAJOR),
                FormatToken::Literal(literal_middle),
                FormatToken::Specifier(&MINOR),
                FormatToken::Literal(literal_back),
            ]),
            format.map(|f| f.tokens)
        );
    }

    /// test all semantic formats. these are:
    ///
    /// - `[MAJOR]`
    /// - `[MAJOR]`, `[MINOR]`
    /// - `[MAJOR]`, `[MINOR]`, `[PATCH]`
    #[test]
    fn test_semantic_parse_ok() {
        let args = [
            ("[MAJOR][MINOR][PATCH]", vec![&MAJOR, &MINOR, &PATCH]),
            ("[MAJOR][MINOR]", vec![&MAJOR, &MINOR]),
            ("[MAJOR]", vec![&MAJOR]),
        ];

        for (format_string, spec) in args {
            let tokens = spec
                .iter()
                .map(|spec| FormatToken::Specifier(spec))
                .collect();
            let actual = Sem::new_format(format_string);
            assert_eq!(Ok(tokens), actual.map(|f| f.tokens));
        }
    }

    /// test all calendar formats. these are:
    ///
    /// - `[<year>]`
    /// - `[<year>]`, `[<month>]`
    /// - `[<year>]`, `[<month>]`, `[<day>]`
    /// - `[<year>]`, `[<week>]`
    #[rstest]
    fn test_calendar_parse_ok(all_calendar_specs_product: impl Iterator<Item = SpecifierTupleVec>) {
        for spec_sequence in all_calendar_specs_product {
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

    /// test all calendar-semantic formats. these are:
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
    fn test_calendar_semantic_parse_ok(
        all_calendar_specs_product: impl Iterator<Item = SpecifierTupleVec>,
    ) {
        let all_with_semantic_suffixes = all_calendar_specs_product.flat_map(|i| {
            vec![
                [i.clone(), vec![(&MINOR, "[MINOR]")]].concat(),
                [
                    i.clone(),
                    vec![(&MINOR, "[MINOR]")],
                    vec![(&PATCH, "[PATCH]")],
                ]
                .concat(),
                [i, vec![(&PATCH, "[PATCH]")]].concat(),
            ]
        });

        for spec_sequence in all_with_semantic_suffixes {
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
    fn test_bad_semantic_format() {
        use NextVerError::*;

        // not exhaustive, just a sample
        let args = [
            ("", NoSpecifiersInFormat),
            ("foo", NoSpecifiersInFormat),
            (
                "[MINOR]",
                WrongFirstSpecifier {
                    spec: &MINOR,
                    scheme_name: Sem::name(),
                    expected_first: "[MAJOR]",
                },
            ),
            (
                "[MAJOR][MAJOR]",
                SpecifiersMustStrictlyDecrease {
                    prev: &MAJOR,
                    next: &MAJOR,
                },
            ),
            (
                "[MAJOR][PATCH]",
                SpecifierMustBeRelative {
                    prev: &MAJOR,
                    next: &PATCH,
                },
            ),
            (
                "[MAJOR][YYYY]",
                UnacceptableSpecifier {
                    spec: &FULL_YEAR,
                    scheme_name: Sem::name(),
                },
            ),
        ];

        for (format, err) in args {
            let actual = Sem::new_format(format);
            assert_eq!(Err(err), actual);
        }
    }

    #[test]
    fn test_bad_calendar_format() {
        use NextVerError::*;

        // not exhaustive, just a sample
        let args = [
            ("", NoSpecifiersInFormat),
            ("foo", NoSpecifiersInFormat),
            (
                "[YYYY][MINOR]",
                UnacceptableSpecifier {
                    spec: &MINOR,
                    scheme_name: Cal::name(),
                },
            ),
            (
                "[YYYY][MM][WW]",
                SpecifierMustBeRelative {
                    prev: &SHORT_MONTH,
                    next: &SHORT_WEEK,
                },
            ),
            (
                "[YYYY][DD]",
                SpecifiersMustStrictlyDecrease {
                    prev: &FULL_YEAR,
                    next: &SHORT_DAY,
                },
            ),
            (
                "[MM]",
                WrongFirstSpecifier {
                    spec: &SHORT_MONTH,
                    scheme_name: Cal::name(),
                    expected_first: &YEAR_FORMAT_STRINGS,
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
        use NextVerError::*;

        // not exhaustive, just a sample
        let args = [
            ("", NoSpecifiersInFormat),
            ("foo", NoSpecifiersInFormat),
            (
                "[YYYY]",
                FormatIncomplete {
                    last_spec: &FULL_YEAR,
                },
            ),
            ("[YYYY][MAJOR]", MajorInCalSemFormat),
            (
                "[MM]",
                WrongFirstSpecifier {
                    spec: &SHORT_MONTH,
                    scheme_name: CalSem::name(),
                    expected_first: &YEAR_FORMAT_STRINGS,
                },
            ),
            (
                "[YYYY][MM][WW][PATCH]",
                SpecifierMustBeRelative {
                    prev: &SHORT_MONTH,
                    next: &SHORT_WEEK,
                },
            ),
            (
                "[YYYY][DD][MINOR]",
                SpecifierMustBeRelative {
                    prev: &FULL_YEAR,
                    next: &SHORT_DAY,
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
                FormatToken::Specifier(&FULL_YEAR),
                FormatToken::Literal("[YYYY]"),
            ])
            .as_ref(),
            actual.as_ref().map(|f| &f.tokens)
        );
        let round_tripped_format = actual.unwrap().to_string();
        assert_eq!(format, round_tripped_format);
    }

    #[test]
    fn test_unknown_specifier() {
        let formats = ["[foo]", "[]", "[]]"];
        for format in formats {
            let actual = Sem::new_format(format);
            assert!(matches!(actual, Err(NextVerError::UnknownSpecifier { .. })));
        }
    }

    #[test]
    fn test_unterminated_specifier() {
        let formats = ["[foo", "[", "[major"];
        for format in formats {
            let actual = Sem::new_format(format);
            assert_eq!(
                actual,
                Err(NextVerError::UnterminatedSpecifier {
                    pattern: format.to_owned(),
                })
            )
        }
    }
}
