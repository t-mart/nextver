use crate::specifier::{Specifier, Type, ALL, MAJOR, MINOR, PATCH};
use crate::VersionBumpError;
use core::fmt;
use regex::Regex;
use std::cell::OnceCell;

// fn update_and_validate_order(
//     last: &mut Option<LastSpecifier>,
//     cur_name: &'static str,
//     cur_order_key: u8,
// ) -> Result<(), VersionBumpError> {
//     let last = last.get_or_insert_with(|| LastSpecifier {
//         name: cur_name,
//         order_key: cur_order_key,
//     });
//     if cur_order_key < last.order_key {
//         return Err(VersionBumpError::SpecifiersOutOfOrder {
//             expected_first_spec: cur_name,
//             expected_last_spec: last.name,
//         });
//     }
//     Ok(())
// }

/// State machine for parsing the specifiers in a format string.
enum ParseState {
    Initial,
    Major,
    Minor,
    Patch,
    Year(&'static str),
    Month(&'static str),
    Week(&'static str),
    Day(&'static str),
}

impl ParseState {
    fn new() -> Self {
        ParseState::Initial
    }

    fn advance(&mut self, next: &'static Specifier) -> Result<(), VersionBumpError> {
        use ParseState::*;
        use VersionBumpError::*;

        let type_ = &next.type_;
        let pat = next.format_pattern;
        *self = match self {
            Initial => match type_ {
                Type::Year => ParseState::Year(pat),
                Type::Major => ParseState::Major,
                _ => return Err(NonCyclicSpecifierNotFirst { spec: pat }),
            },
            Major => match type_ {
                Type::Major => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev_spec: MAJOR.format_pattern,
                        next_spec: MAJOR.format_pattern,
                    })
                }
                Type::Minor => Minor,
                Type::Patch => Patch,
                Type::Year | Type::Month | Type::Week | Type::Day => {
                    return Err(CalenderMustPrecedeSemantic {
                        prev_spec: MAJOR.format_pattern,
                        next_spec: pat,
                    })
                }
            },
            Minor => match type_ {
                Type::Major => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev_spec: MINOR.format_pattern,
                        next_spec: MAJOR.format_pattern,
                    })
                }
                Type::Minor => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev_spec: MINOR.format_pattern,
                        next_spec: MINOR.format_pattern,
                    })
                }
                Type::Patch => Patch,
                Type::Year | Type::Month | Type::Week | Type::Day => {
                    return Err(CalenderMustPrecedeSemantic {
                        prev_spec: MINOR.format_pattern,
                        next_spec: pat,
                    })
                }
            },
            Patch => match type_ {
                Type::Major => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev_spec: PATCH.format_pattern,
                        next_spec: MAJOR.format_pattern,
                    })
                }
                Type::Minor => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev_spec: PATCH.format_pattern,
                        next_spec: MINOR.format_pattern,
                    })
                }
                Type::Patch => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev_spec: PATCH.format_pattern,
                        next_spec: PATCH.format_pattern,
                    })
                }
                Type::Year | Type::Month | Type::Week | Type::Day => {
                    return Err(CalenderMustPrecedeSemantic {
                        prev_spec: MINOR.format_pattern,
                        next_spec: pat,
                    })
                }
            },
            Year(last_year) => match next.type_ {
                Type::Year => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev_spec: last_year,
                        next_spec: pat,
                    })
                }
                Type::Month => Month(pat),
                Type::Week => Week(pat),
                Type::Day => Day(pat),
                Type::Major => return Err(NonCyclicSpecifierNotFirst { spec: pat }),
                Type::Minor => Minor,
                Type::Patch => Patch,
            },
            Month(last_month) => match next.type_ {
                Type::Year | Type::Month | Type::Week => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev_spec: last_month,
                        next_spec: pat,
                    })
                }
                Type::Day => Day(pat),
                Type::Major => return Err(NonCyclicSpecifierNotFirst { spec: pat }),
                Type::Minor => Minor,
                Type::Patch => Patch,
            },
            Week(last_week) => match next.type_ {
                Type::Year | Type::Month | Type::Week | Type::Day => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev_spec: last_week,
                        next_spec: pat,
                    })
                }
                Type::Major => return Err(NonCyclicSpecifierNotFirst { spec: pat }),
                Type::Minor => Minor,
                Type::Patch => Patch,
            },
            Day(last_day) => match next.type_ {
                Type::Year | Type::Month | Type::Week | Type::Day => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev_spec: last_day,
                        next_spec: pat,
                    })
                }
                Type::Major => return Err(NonCyclicSpecifierNotFirst { spec: pat }),
                Type::Minor => Minor,
                Type::Patch => Patch,
            },
        };
        Ok(())
    }
}

#[derive(Debug)]
pub(crate) enum FormatToken {
    Specifier(&'static Specifier),
    Literal(String),
}

impl FormatToken {
    fn version_pattern_group(&self) -> String {
        match self {
            FormatToken::Specifier(spec) => spec.version_pattern_group(),
            FormatToken::Literal(text) => format!("({})", regex::escape(text)),
        }
    }
}

impl Clone for FormatToken {
    fn clone(&self) -> Self {
        match self {
            FormatToken::Specifier(spec) => FormatToken::Specifier(spec),
            FormatToken::Literal(text) => FormatToken::Literal(text.clone()),
        }
    }
}

impl PartialEq for FormatToken {
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

/**
A Format describes the structure of a version. It's created from a string that contains
specifiers indicating what each part of the string means. Later, the Format can be used to parse
a version string into a [Version](crate::version::Version) struct.

The format string is made up of specifiers and literals. Specifiers indicate numeric values that
can change, while literals are fixed text.

# Example

```
use version_bump::Format;

let format = Format::parse("[YYYY].[PATCH].[[escaped]]");
```

# Specifiers

To specify an specifier, surround the specifier name with square brackets (e.g. `[MAJOR]`).

You can mix specifiers of different types (e.g. `[YYYY].[MINOR].[PATCH]`) as long as they are in
the right [order](#specifier-order).

## [Specifier Table](#specifier-table)

In the "Example" column below, we reference a major of `1`, minor of `2`, patch of `3` and a
date of `2001-02-03` (in the 4th week).

| Specifier | Example | Type   | [Greedy?](#greedy-specifiers) | Description | Reference |
|------------|--------|-------|------------|-------------|-----------|
| `[MAJOR]`  | `1` | Semantic - Major | **Yes** | The major part of a version | [1] |
| `[MINOR]`  | `2` | Semantic - Minor | **Yes** | The minor part of a version | [1] |
| `[PATCH]`  | `3` | Semantic - Patch | **Yes** | The patch part of a version | [1] |
| `[YYYY]`   | `2001` | Calendar - Year  | **Yes** | Full year. This will be at least 1 digit (e.g. year `1` or year `10000`). We do not support BCE years for this specifier. | [2] |
| `[YY]`     | `1` | Calendar - Year  | **Yes** | The last two digits of the year (for years <= `2099`). In general, this is the same as `year - 2000` so that, for example, years `2001`, `2101`, ... `3001` are unambiguous. We do not support years less than `2000` for this specifier. | [2] |
| `[0Y]`     | `01` | Calendar - Year  | **Yes** | Same as `YY` but zero-padded to at least 2 characters. | [2] |
| `[MM]`     | `1` | Calendar - Month |  **Yes** | Month number (`1`–`12`). | [2] |
| `[0M]`     | `01` | Calendar - Month | No | Same as `MM` but zero-padded to 2 characters. | [2] |
| `[WW]`     | `4` | Calendar - Week |  **Yes** | Week of the year (`0`–`53`). Week 1 starts with the first Sunday in that year. | [2] |
| `[0W]`     | `04` | Calendar - Week | No | Same as `WW` but zero-padded to 2 characters. | [2] |
| `[DD]`     | `3` | Calendar - Day |  **Yes** | Day number (`1`–`31`). | [2] |
| `[0D]`     | `03` | Calendar - Day | No | Same as `DD` but zero-padded to 2 characters. | [2] |

[1]: https://semver.org/
[2]: https://calver.org/

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

## Escaping

If you want to escape an opening bracket, precede it with a backslash (`\`). Only the opening
bracket can be escaped. Literal closing brackets must not be escaped.

```
use version_bump::{Format};

// Hint: use raw strings to avoid having to escape the backslash
let format = Format::parse(r"\[YYYY]");
// Or, escape the backslash itself in a regular string
let format = Format::parse("\\[YYYY]");
```

### Greedy Specifiers

Greedy specifiers greedily match digits (sometimes with an upper bound and sometimes not). A
caveat with their use is that if you specify two consecutive greedy specifiers, the former
specifier can consume some of the digits of the latter. This is probably not what you want. For
this reason, it's recommended to use a literal separator or a non-greedy latter specifier in
such cases.

```
use version_bump::Format;

// always consistent because of literal separator
let format = Format::parse("[YYYY].[MM]");

// inconsistent: YYYY will consume first digit of a two-digit MM
let format = Format::parse("[YYYY][MM]");

// always consistent: zero-padded `0M` is always two digits
let format = Format::parse("[YYYY][0M]");
```
**/
#[derive(Debug, Clone)]
pub struct Format {
    pub(crate) tokens: Vec<FormatToken>,
    regex: OnceCell<Regex>,
}

impl Format {
    pub(crate) fn new(tokens: Vec<FormatToken>, regex: OnceCell<Regex>) -> Self {
        Self { tokens, regex }
    }

    pub(crate) fn from_tokens(tokens: Vec<FormatToken>) -> Self {
        Self::new(tokens, OnceCell::new())
    }

    /// Get the regex pattern for this format. Caches the regex if it hasn't been created yet.
    pub(crate) fn get_regex(&self) -> &Regex {
        self.regex.get_or_init(|| {
            let tokens_subpattern = self
                .tokens
                .iter()
                .map(|format_token| format_token.version_pattern_group())
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
    /// - [`VersionBumpError::SpecifiersOutOfOrder`] if the specifiers are not in descending order
    ///  of significance.
    /// - [`VersionBumpError::UnknownSpecifier`] if an unknown specifier is found (e.g., `[FOO]`).
    /// - [`VersionBumpError::UnterminatedSpecifier`] if a specifier is not terminated with a
    ///   closing square bracket (`]`).
    pub fn parse(format_str: &str) -> Result<Self, VersionBumpError> {
        let mut format = format_str;
        let mut parse_state = ParseState::new();
        let mut tokens = vec![];

        while !format.is_empty() {
            let matched_spec = 'spec_find: {
                for spec in ALL {
                    let format_pattern = spec.format_pattern;
                    if format.starts_with(format_pattern) {
                        format = &format[format_pattern.len()..];
                        break 'spec_find Some(spec);
                    }
                }
                None
            };

            if let Some(spec) = matched_spec {
                // check that specifiers are in order
                parse_state.advance(spec)?;
                tokens.push(FormatToken::Specifier(spec));
            } else {
                // check if its escaped brackets, an unknown/unterminated specifier, or finally,
                // just a literal.
                let (literal, consume_len) = if format.starts_with('[') {
                    // check if its an unknown/unterminated specifier. we technically don't need to
                    // error here: could just parse this as a literal because, above, we've already
                    // exhausted all known specifiers, but it's probably not what the user intended.
                    let mut next_chars = format.chars().skip(1);
                    let closing_index = next_chars
                        .position(|c| c == ']')
                        .map(|index| index + 1)
                        .ok_or_else(|| {
                            // didn't find closing bracket
                            VersionBumpError::UnterminatedSpecifier {
                                pattern: format.to_owned(),
                            }
                        })?;
                    // found closing, but unknown
                    return Err(VersionBumpError::UnknownSpecifier {
                        pattern: format[..closing_index].to_owned(),
                    });
                } else if format.starts_with(r"\[") {
                    // escaped opening bracket
                    ('[', 2)
                } else {
                    // any other literal
                    let next_char = format.chars().next().unwrap();
                    (next_char, 1)
                };
                format = &format[consume_len..];

                // we can add this literal to the last token if it was also a literal.
                // this will help us cut down on the total number of tokens and therefore, regex
                // groups later.
                if let Some(FormatToken::Literal(last_literal)) = tokens.last_mut() {
                    last_literal.push(literal);
                } else {
                    tokens.push(FormatToken::Literal(literal.to_string()));
                }
            }
        }

        Ok(Self::from_tokens(tokens))
    }
}

impl PartialEq for Format {
    /// Returns true if the two formats would have the same string representation.
    fn eq(&self, other: &Self) -> bool {
        self.tokens == other.tokens
    }
}

impl fmt::Display for Format {
    /// Display a format as a string.
    ///
    /// # Example
    ///
    /// ```
    /// use version_bump::Format;
    ///
    /// let format_str = "[YYYY].[MINOR].[PATCH]";
    /// let format = Format::parse(format_str).unwrap();
    /// assert_eq!(format_str, format.to_string());
    /// ```
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let format_string = self
            .tokens
            .iter()
            .map(|token| match token {
                // FormatToken::Specifier(specifier) => specifier.format_pattern.to_owned(),
                // FormatToken::Literal(text) => text.to_owned(),
                FormatToken::Specifier(specifier) => specifier.format_pattern,
                FormatToken::Literal(text) => text,
            })
            .collect::<Vec<_>>()
            .join("");

        write!(f, "{}", format_string)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::specifier::{
        FULL_YEAR, MAJOR, MINOR, PATCH, SHORT_DAY, SHORT_MONTH, SHORT_WEEK, SHORT_YEAR,
        ZERO_PADDED_DAY, ZERO_PADDED_MONTH, ZERO_PADDED_WEEK, ZERO_PADDED_YEAR,
    };

    #[test]
    fn test_parse_ok() {
        let format_string = "[YYYY].[MINOR].[PATCH]";
        let format = Format::parse(format_string);
        assert_eq!(
            Ok(Format {
                tokens: vec![
                    FormatToken::Specifier(&FULL_YEAR),
                    FormatToken::Literal(".".to_string()),
                    FormatToken::Specifier(&MINOR),
                    FormatToken::Literal(".".to_string()),
                    FormatToken::Specifier(&PATCH),
                ],
                regex: OnceCell::new(),
            }),
            format
        );
        assert_eq!(format_string, format.unwrap().to_string());
    }

    #[test]
    fn test_all_specs() {
        let formats = vec![
            ("[MAJOR][MINOR][PATCH]", vec![&MAJOR, &MINOR, &PATCH]),
            ("[YYYY][MM][DD]", vec![&FULL_YEAR, &SHORT_MONTH, &SHORT_DAY]),
            (
                "[YY][0M][0D]",
                vec![&SHORT_YEAR, &ZERO_PADDED_MONTH, &ZERO_PADDED_DAY],
            ),
            ("[0Y][WW]", vec![&ZERO_PADDED_YEAR, &SHORT_WEEK]),
            ("[YYYY][0W]", vec![&FULL_YEAR, &ZERO_PADDED_WEEK]),
        ];

        for (format, spec) in formats {
            let tokens = spec
                .iter()
                .map(|spec| FormatToken::Specifier(spec))
                .collect();
            let actual = Format::parse(format);
            assert_eq!(
                Ok(Format {
                    tokens,
                    regex: OnceCell::new(),
                }),
                actual
            );
        }
    }

    #[test]
    fn test_consecutive_literal() {
        let format = Format::parse("[MAJOR].foo.[MINOR]");
        assert_eq!(
            Ok(Format {
                tokens: vec![
                    FormatToken::Specifier(&MAJOR),
                    FormatToken::Literal(".foo.".to_string()),
                    FormatToken::Specifier(&MINOR),
                ],
                regex: OnceCell::new(),
            }),
            format
        );
    }

    #[test]
    fn test_disordered_format() {
        use VersionBumpError::*;
        // these cases are non-exhaustive, but should be sufficient to test the logic
        #[rustfmt::skip]
        let args = [
            ("[MINOR]", NonCyclicSpecifierNotFirst { spec: "[MINOR]" }),
            ("[PATCH]", NonCyclicSpecifierNotFirst { spec: "[PATCH]" }),
            ("[MM]", NonCyclicSpecifierNotFirst { spec: "[MM]" } ),
            ("[WW]", NonCyclicSpecifierNotFirst { spec: "[WW]" } ),
            ("[DD]", NonCyclicSpecifierNotFirst { spec: "[DD]" } ),
            ("[YYYY][MAJOR]", NonCyclicSpecifierNotFirst { spec: "[MAJOR]" } ),

            ("[MAJOR][MAJOR]", SpecifiersMustStrictlyDecrease { prev_spec: "[MAJOR]", next_spec: "[MAJOR]" }),
            ("[MAJOR][MINOR][MINOR]", SpecifiersMustStrictlyDecrease { prev_spec: "[MINOR]", next_spec: "[MINOR]" }),
            ("[MAJOR][PATCH][MINOR]", SpecifiersMustStrictlyDecrease { prev_spec: "[PATCH]", next_spec: "[MINOR]" }),
            ("[YYYY][YYYY]", SpecifiersMustStrictlyDecrease { prev_spec: "[YYYY]", next_spec: "[YYYY]" }),
            ("[YYYY][MM][MM]", SpecifiersMustStrictlyDecrease { prev_spec: "[MM]", next_spec: "[MM]" }),
            ("[YYYY][DD][MM]", SpecifiersMustStrictlyDecrease { prev_spec: "[DD]", next_spec: "[MM]" }),
            ("[YYYY][MM][WW]", SpecifiersMustStrictlyDecrease { prev_spec: "[MM]", next_spec: "[WW]" }),
            ("[YYYY][WW][DD]", SpecifiersMustStrictlyDecrease { prev_spec: "[WW]", next_spec: "[DD]" }),

            ("[MAJOR][YYYY]", CalenderMustPrecedeSemantic { prev_spec: "[MAJOR]", next_spec: "[YYYY]" }),
            ("[MAJOR][MM]", CalenderMustPrecedeSemantic { prev_spec: "[MAJOR]", next_spec: "[MM]" }),
            ("[MAJOR][WW]", CalenderMustPrecedeSemantic { prev_spec: "[MAJOR]", next_spec: "[WW]" }),
            ("[MAJOR][DD]", CalenderMustPrecedeSemantic { prev_spec: "[MAJOR]", next_spec: "[DD]" }),
        ];

        for (format, err) in args {
            let actual = Format::parse(format);
            assert_eq!(Err(err), actual);
        }
    }

    #[test]
    fn test_bracket_escape() {
        let format = r"[YYYY]\[YYYY]";
        let actual = Format::parse(format);
        assert_eq!(
            Ok(Format {
                tokens: vec![
                    FormatToken::Specifier(&FULL_YEAR),
                    FormatToken::Literal("[YYYY]".to_string()),
                ],
                regex: OnceCell::new(),
            }),
            actual
        );
    }

    #[test]
    fn test_unknown_specifier() {
        let formats = ["[foo]", "[]", "[]]"];
        for format in formats {
            let actual = Format::parse(format);
            assert!(matches!(
                actual,
                Err(VersionBumpError::UnknownSpecifier { .. })
            ));
        }
    }

    #[test]
    fn test_unterminated_specifier() {
        let formats = ["[foo", "[", "[major"];
        for format in formats {
            let actual = Format::parse(format);
            assert_eq!(
                actual,
                Err(VersionBumpError::UnterminatedSpecifier {
                    pattern: format.to_owned(),
                })
            )
        }
    }

    #[test]
    fn test_empty() {
        let actual = Format::parse("");
        assert_eq!(
            Ok(Format {
                tokens: vec![],
                regex: OnceCell::new(),
            }),
            actual
        );
    }
}
