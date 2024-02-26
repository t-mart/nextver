use crate::{
    error::{FormatError, VersionError},
    scheme::Scheme,
    specifier::Specifier,
    version::Version,
};
use core::{
    fmt::{self, Display},
    str,
};

#[derive(Debug, PartialEq, Eq)]
pub(crate) enum FormatToken<'fs, S: Scheme> {
    Specifier(&'static S::Specifier),

    /// A literal holds an array of bytes from the format string. Note that this make contained
    /// escaped brackets, so these bytes are not necessarily what will match the version string.
    Literal(&'fs [u8]),
}

impl<'fs, S: Scheme> Clone for FormatToken<'fs, S> {
    // manually implemented because the derive macro would want Scheme to be Clone, which really
    // feels unnecessary.
    fn clone(&self) -> Self {
        match self {
            FormatToken::Specifier(spec) => FormatToken::Specifier(*spec),
            FormatToken::Literal(text) => FormatToken::Literal(text),
        }
    }
}

impl<'fs, S: Scheme> Display for FormatToken<'fs, S> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            FormatToken::Specifier(spec) => write!(f, "{}", spec),
            FormatToken::Literal(text) => {
                let text_str = unsafe { str::from_utf8_unchecked(text) };
                f.write_str(text_str)
            }
        }
    }
}

/// A Format describes the structure of a version, comprised of *specifiers* and *literal text*.
///
/// Later, the `Format` can be used to parse a version string into a
/// [`Version`](crate::version::Version) struct.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Format<'fs, S: Scheme> {
    pub(crate) tokens: Vec<FormatToken<'fs, S>>,
}

impl<'fs, S: Scheme> Format<'fs, S> {
    pub(crate) fn parse(format_str: &'fs str) -> Result<Self, FormatError> {
        let mut format = format_str.as_bytes();
        let mut tokens = Vec::with_capacity(S::MAX_TOKENS);
        let mut last_spec: Option<&'static S::Specifier> = None;

        while !format.is_empty() {
            let matched_spec = S::Specifier::all()
                .iter()
                .find(|spec| format.starts_with(spec.format_pattern()));

            let consume_len = if let Some(&spec) = matched_spec {
                // check that specifiers are in order
                if let Some(last_spec) = last_spec {
                    if !last_spec.can_be_left_adjacent_to(spec) {
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
                tokens.push(FormatToken::Specifier(spec));
                spec.format_pattern().len()
            } else {
                // check if its escaped brackets, an unknown/unterminated specifier, or finally,
                // just a literal.
                let (literal, consume_len) = if format.starts_with(b"<<") {
                    // escaped opening bracket
                    (&format[0..2], 2)
                } else if format.starts_with(&[b'<']) {
                    // determine if unknown or unterminated specifier. we technically don't need to
                    // error here: could just parse this as a literal because, above, we've already
                    // exhausted all known specifiers, but this helps the user.
                    let closing_index = format[1..]
                        .iter()
                        .position(|c| *c == b'>')
                        .map(|index| {
                            // 1 for opening bracket that we skipped
                            index + 1
                        })
                        .ok_or_else(|| {
                            // didn't find closing bracket
                            FormatError::UnterminatedSpecifier {
                                pattern: unsafe { std::str::from_utf8_unchecked(format) }
                                    .to_string(),
                            }
                        })?;
                    // found closing, but unknown for this scheme
                    return Err(FormatError::UnacceptableSpecifier {
                        spec: unsafe {
                            std::str::from_utf8_unchecked(&format[..closing_index + 1])
                        }
                        .to_string(),
                        scheme_name: S::name(),
                    });
                } else {
                    // any other literal.
                    (&format[0..1], 1)
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
                        core::slice::from_raw_parts(
                            last_literal.as_ptr(),              //same ptr
                            last_literal.len() + literal.len(), // new len
                        )
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

        Ok(Self { tokens })
    }

    /// Parses a version string with this format and return a [`Version`] object.
    ///
    /// A version string is valid for a format if it matches the format exactly. This means that:
    ///
    /// - Specifiers (e.g. `<MAJOR>`) in the format are replaced with the numeric values. See the
    ///   [table](crate#table) for how these values are expressed (such as zero-padding).
    ///
    ///   Note: The values in `version_str` are *not* validated to be actual dates. For example,
    ///   `2021.02.31` is valid for the format `<YYYY>.<MM>.<DD>`, even though February 31st does
    ///   not exist.
    ///
    /// - Literal text in the format must match the version string exactly.
    ///
    /// # Errors
    ///
    /// - If the version string does not match the format string, returns a
    ///   [`VersionError::VersionFormatMismatch`].
    pub fn new_version<'vs>(&self, version_str: &'vs str) -> Result<Version<'vs, S>, VersionError> {
        Version::parse(version_str, self)
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
    /// let format_str = "<YYYY>.<MM>.<DD>";
    /// let format = Cal::new_format(format_str).unwrap();
    /// assert_eq!(format_str, format.to_string());
    /// ```
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        for token in &self.tokens {
            f.write_str(&token.to_string())?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        scheme::{Cal, CalSem, Sem},
        specifier::{
            CALSEM_MINOR, CALSEM_MONTH_SHORT, CALSEM_WEEK_SHORT, CALSEM_YEAR_FULL, CAL_DAY_SHORT,
            CAL_MONTH_SHORT, CAL_WEEK_SHORT, CAL_YEAR_FULL, SEM_MAJOR, SEM_MINOR, SEM_PATCH,
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
        let format_str = &format!("{}<MAJOR>{}<MINOR>{}", &first, &middle, &last);
        let sem_format = Sem::new_format(format_str);
        assert_eq!(Ok(format_str), sem_format.map(|f| f.to_string()).as_ref());
    }

    #[rstest]
    fn test_cal_literal(literal_parts: [&'static str; 3]) {
        let [first, middle, last] = literal_parts;
        let format_str = &format!("{}<YYYY>{}<MM>{}", &first, &middle, &last);
        let cal_format = Cal::new_format(format_str);
        assert_eq!(Ok(format_str), cal_format.map(|f| f.to_string()).as_ref());
    }

    #[rstest]
    fn test_calsem_literal(literal_parts: [&'static str; 3]) {
        let [first, middle, last] = literal_parts;
        let format_str = &format!("{}<YYYY>{}<PATCH>{}", &first, &middle, &last);
        let calsem_format = CalSem::new_format(format_str);
        assert_eq!(
            Ok(format_str),
            calsem_format.map(|f| f.to_string()).as_ref()
        );
    }

    /// test all semantic format sequences parse ok. these are:
    ///
    /// - `<MAJOR>`
    /// - `<MAJOR>`, `<MINOR>`
    /// - `<MAJOR>`, `<MINOR>`, `<PATCH>`
    #[test]
    fn test_sem_parse_ok() {
        let format_strings = ["<MAJOR><MINOR><PATCH>", "<MAJOR><MINOR>", "<MAJOR>"];

        for format_string in format_strings {
            let actual = Sem::new_format(format_string);
            assert_eq!(Ok(format_string), actual.map(|f| f.to_string()).as_deref());
        }
    }

    /// Returns an iterator of two-tuples of (specifier, pattern) for all valid calendar specifiers.
    ///
    /// These are:
    ///
    /// - `[<year>]`
    /// - `[<year>]`, `[<month>]`
    /// - `[<year>]`, `[<month>]`, `[<day>]`
    /// - `[<year>]`, `[<week>]`
    #[fixture]
    fn all_valid_cal_specs_product() -> impl Iterator<Item = Vec<&'static str>> {
        let years = || iter::once(vec!["<YYYY>", "<YY>", "<0Y>"]);
        let months = || iter::once(vec!["<MM>", "<0M>"]);
        let weeks = || iter::once(vec!["<WW>", "<0W>"]);
        let days = || iter::once(vec!["<DD>", "<0D>"]);

        let years_product = years().multi_cartesian_product();
        let years_months_product = years().chain(months()).multi_cartesian_product();
        let years_months_days_product = years()
            .chain(months())
            .chain(days())
            .multi_cartesian_product();
        let years_weeks_product = years().chain(weeks()).multi_cartesian_product();

        years_product
            .chain(years_months_product)
            .chain(years_months_days_product)
            .chain(years_weeks_product)
    }

    #[rstest]
    fn test_cal_parse_ok(all_valid_cal_specs_product: impl Iterator<Item = Vec<&'static str>>) {
        for spec_sequence in all_valid_cal_specs_product {
            let format_string = &spec_sequence.join("");
            let actual = Cal::new_format(format_string);
            assert_eq!(Ok(format_string), actual.map(|f| f.to_string()).as_ref());
        }
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
    /// where REST is either:
    ///
    /// - `<MINOR>`, `<PATCH>`
    /// - `<PATCH>`
    #[fixture]
    fn all_valid_calsem_specs_product(
        all_valid_cal_specs_product: impl Iterator<Item = Vec<&'static str>>,
    ) -> impl Iterator<Item = Vec<&'static str>> {
        all_valid_cal_specs_product
            // augment each of these products with the three possible semantic suffix combinations
            .flat_map(|iter| {
                vec![
                    [iter.clone(), vec!["<MINOR>"], vec!["<PATCH>"]].concat(),
                    [iter, vec!["<PATCH>"]].concat(),
                ]
            })
    }

    #[rstest]
    fn test_calsem_parse_ok(
        all_valid_calsem_specs_product: impl Iterator<Item = Vec<&'static str>>,
    ) {
        for spec_sequence in all_valid_calsem_specs_product {
            let format_string = &spec_sequence.join("");
            let actual = CalSem::new_format(format_string);
            assert_eq!(Ok(format_string), actual.map(|f| f.to_string()).as_ref());
        }
    }

    #[test]
    fn test_bad_sem_format() {
        use crate::error::FormatError::*;
        use crate::scheme::priv_trait::Scheme;

        // not exhaustive, just a sample
        let args = [
            ("", NoSpecifiersInFormat),
            ("foo", NoSpecifiersInFormat),
            (
                "<MINOR>",
                WrongFirstSpecifier {
                    first_spec: SEM_MINOR.to_string(),
                    scheme_name: Sem::name(),
                    expected_first: Sem::first_variants_string(),
                },
            ),
            (
                "<MAJOR><MAJOR>",
                SpecifiersMustStepDecrease {
                    prev: SEM_MAJOR.to_string(),
                    next: SEM_MAJOR.to_string(),
                },
            ),
            (
                "<MAJOR><PATCH>",
                SpecifiersMustStepDecrease {
                    prev: SEM_MAJOR.to_string(),
                    next: SEM_PATCH.to_string(),
                },
            ),
            (
                "<MAJOR><YYYY>",
                UnacceptableSpecifier {
                    spec: "<YYYY>".to_string(),
                    scheme_name: Sem::name(),
                },
            ),
            (
                "<MAJOR",
                UnterminatedSpecifier {
                    pattern: "<MAJOR".to_string(),
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
        use crate::scheme::priv_trait::Scheme;

        // not exhaustive, just a sample
        let args = [
            ("", NoSpecifiersInFormat),
            ("foo", NoSpecifiersInFormat),
            (
                "<MM>",
                WrongFirstSpecifier {
                    first_spec: CAL_MONTH_SHORT.to_string(),
                    scheme_name: Cal::name(),
                    expected_first: Cal::first_variants_string(),
                },
            ),
            (
                "<YYYY><MINOR>",
                UnacceptableSpecifier {
                    spec: "<MINOR>".to_string(),
                    scheme_name: Cal::name(),
                },
            ),
            (
                "<YYYY><MM><WW>",
                SpecifiersMustStepDecrease {
                    prev: CAL_MONTH_SHORT.to_string(),
                    next: CAL_WEEK_SHORT.to_string(),
                },
            ),
            (
                "<YYYY><DD>",
                SpecifiersMustStepDecrease {
                    prev: CAL_YEAR_FULL.to_string(),
                    next: CAL_DAY_SHORT.to_string(),
                },
            ),
            (
                "<YYYY",
                UnterminatedSpecifier {
                    pattern: "<YYYY".to_string(),
                },
            ),
        ];

        for (format, err) in args {
            let actual = Cal::new_format(format);
            assert_eq!(Err(err), actual);
        }
    }

    #[test]
    fn test_bad_calsem_format() {
        use crate::error::FormatError::*;
        use crate::scheme::priv_trait::Scheme;

        // not exhaustive, just a sample
        let args = [
            ("", NoSpecifiersInFormat),
            ("foo", NoSpecifiersInFormat),
            (
                "<YYYY",
                UnterminatedSpecifier {
                    pattern: "<YYYY".to_string(),
                },
            ),
            (
                "<YYYY><FOO>",
                UnacceptableSpecifier {
                    spec: "<FOO>".to_string(),
                    scheme_name: CalSem::name(),
                },
            ),
            (
                "<YYYY>",
                Incomplete {
                    last_spec: CALSEM_YEAR_FULL.to_string(),
                    scheme_name: CalSem::name(),
                    expected_last: CalSem::last_variants_string(),
                },
            ),
            (
                "<MM>",
                WrongFirstSpecifier {
                    first_spec: CAL_MONTH_SHORT.to_string(),
                    scheme_name: CalSem::name(),
                    expected_first: CalSem::first_variants_string(),
                },
            ),
            (
                "<YYYY><MM><WW><PATCH>",
                SpecifiersMustStepDecrease {
                    prev: CALSEM_MONTH_SHORT.to_string(),
                    next: CALSEM_WEEK_SHORT.to_string(),
                },
            ),
            (
                "<YYYY><DD><MINOR>",
                SpecifiersMustStepDecrease {
                    prev: CALSEM_YEAR_FULL.to_string(),
                    next: CAL_DAY_SHORT.to_string(),
                },
            ),
            (
                "<YYYY><MINOR>",
                Incomplete {
                    last_spec: CALSEM_MINOR.to_string(),
                    scheme_name: CalSem::name(),
                    expected_last: CalSem::last_variants_string(),
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
        let format = r"<YYYY><<YYYY>";
        let actual = Cal::new_format(format);
        assert_eq!(
            Ok(vec![
                FormatToken::Specifier(&CAL_YEAR_FULL),
                FormatToken::Literal(b"<<YYYY>"),
            ])
            .as_ref(),
            actual.as_ref().map(|f| &f.tokens)
        );
        let round_tripped_format = actual.unwrap().to_string();
        assert_eq!(format, round_tripped_format);
    }

    #[test]
    fn test_sem_eq() {
        let format1 = Sem::new_format("<MAJOR><MINOR>").unwrap();
        let format2 = Sem::new_format("<MAJOR><MINOR>").unwrap();
        assert_eq!(format1, format2);
    }

    #[test]
    fn test_sem_neq() {
        let format1 = Sem::new_format("<MAJOR><MINOR>").unwrap();
        let format2 = Sem::new_format("<MAJOR>").unwrap();
        assert_ne!(format1, format2);
    }

    #[test]
    fn test_cal_eq() {
        let format1 = Cal::new_format("<YYYY><MM>").unwrap();
        let format2 = Cal::new_format("<YYYY><MM>").unwrap();
        assert_eq!(format1, format2);
    }

    #[test]
    fn test_cal_neq() {
        let format1 = Cal::new_format("<YYYY><MM>").unwrap();
        let format2 = Cal::new_format("<YYYY><0M>").unwrap();
        assert_ne!(format1, format2);
    }

    #[test]
    fn test_calsem_eq() {
        let format1 = CalSem::new_format("<YYYY><PATCH>").unwrap();
        let format2 = CalSem::new_format("<YYYY><PATCH>").unwrap();
        assert_eq!(format1, format2);
    }

    #[test]
    fn test_calsem_neq() {
        let format1 = CalSem::new_format("<0Y><PATCH>").unwrap();
        let format2 = CalSem::new_format("<YYYY><PATCH>").unwrap();
        assert_ne!(format1, format2);
    }
}
