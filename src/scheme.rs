use crate::format::ParseState;
use crate::specifier::{
    CalendarSpecifier, SemanticSpecifier, Specifier, MAJOR, MINOR, PATCH, YEAR_FORMAT_STRINGS,
};
use crate::{Format, Version, VersionBumpError};
use core::fmt::Debug;

pub(crate) mod priv_trait {
    use super::*;
    pub(crate) trait Scheme: Sized + Debug {
        /// Returns a human readable string of the first specifier(s) that should be present in the
        /// format string. Used for error messages.
        fn expected_first(&self) -> &'static str;

        /// Advances the parse state according to the next specifier.
        fn advance_parse_state(
            &self,
            parse_state: &mut ParseState,
            next: &'static Specifier,
        ) -> Result<(), VersionBumpError>;

        /// Returns true if the current state is an acceptable final state.
        ///
        /// Note: This must be called! Currently, at least one scheme requires the presence of certain
        /// specifiers at its end.
        fn parse_state_is_final(&self, parse_state: &ParseState) -> bool;
    }
}

#[allow(private_bounds)]
pub trait Scheme: priv_trait::Scheme {
    /// Create a new format from a format string for this scheme.
    fn new_format(format_str: &str) -> Result<Format<Self>, VersionBumpError>;

    /// Returns a human readable name of the scheme for error messages.
    fn name(&self) -> &'static str;

    /// Parses a version string against a format string, and returns a [Version] object if the
    /// version string matches the format string. Otherwise, returns a [VersionBumpError].
    ///
    /// This is a convenience method that creates a temporary [Format] object and parses the version
    /// string against it.
    fn new_version(format_str: &str, version_str: &str) -> Result<Version<Self>, VersionBumpError> {
        let format = Self::new_format(format_str)?;
        let version = Version::parse(version_str, &format)?;
        Ok(version)
    }

    /// Returns `true` if the given version string is valid for the given format string.
    ///
    /// This is a convenience method that creates a temporary [Format] object and validates that the
    /// version string matches it.
    fn is_valid(format_str: &str, version_str: &str) -> bool {
        Self::new_version(format_str, version_str).is_ok()
    }
}

#[derive(Debug)]
pub struct Sem;

impl Scheme for Sem {
    fn name(&self) -> &'static str {
        "semantic"
    }

    fn new_format(format_str: &str) -> Result<Format<Self>, VersionBumpError> {
        Format::parse(format_str, Sem)
    }
}

impl priv_trait::Scheme for Sem {
    fn expected_first(&self) -> &'static str {
        MAJOR.format_pattern()
    }

    fn advance_parse_state(
        &self,
        parse_state: &mut ParseState,
        next: &'static Specifier,
    ) -> Result<(), VersionBumpError> {
        use ParseState::*;
        use VersionBumpError::*;

        let Specifier::Semantic(next_sem) = next else {
            return Err(UnacceptableSpecifier {
                spec: next,
                scheme_name: self.name(),
            });
        };

        // let type_ = &next.type_;
        *parse_state = match parse_state {
            Initial => match next_sem {
                SemanticSpecifier::Major => Major,
                _ => {
                    return Err(WrongFirstSpecifier {
                        spec: next,
                        scheme_name: self.name(),
                        expected_first: self.expected_first(),
                    })
                }
            },
            Major => match next_sem {
                SemanticSpecifier::Major => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: &MAJOR,
                        next: &MAJOR,
                    })
                }
                SemanticSpecifier::Minor => Minor,
                SemanticSpecifier::Patch => {
                    return Err(SpecifierMustBeRelative {
                        prev: &MAJOR,
                        next: &PATCH,
                    })
                }
            },
            Minor => match next_sem {
                SemanticSpecifier::Major => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: &MINOR,
                        next: &MAJOR,
                    })
                }
                SemanticSpecifier::Minor => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: &MINOR,
                        next: &MINOR,
                    })
                }
                SemanticSpecifier::Patch => Patch,
            },
            Patch => match next_sem {
                SemanticSpecifier::Major => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: &PATCH,
                        next: &MAJOR,
                    })
                }
                SemanticSpecifier::Minor => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: &PATCH,
                        next: &MINOR,
                    })
                }
                SemanticSpecifier::Patch => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: &PATCH,
                        next: &PATCH,
                    })
                }
            },
            _ => unreachable!(),
        };
        Ok(())
    }

    fn parse_state_is_final(&self, _: &ParseState) -> bool {
        true
    }
}

#[derive(Debug)]
pub struct Cal;

impl Scheme for Cal {
    fn name(&self) -> &'static str {
        "calendar"
    }

    fn new_format(format_str: &str) -> Result<Format<Self>, VersionBumpError> {
        Format::parse(format_str, Cal)
    }
}

impl priv_trait::Scheme for Cal {
    fn expected_first(&self) -> &'static str {
        &YEAR_FORMAT_STRINGS
    }

    fn advance_parse_state(
        &self,
        parse_state: &mut ParseState,
        next: &'static Specifier,
    ) -> Result<(), VersionBumpError> {
        use ParseState::*;
        use VersionBumpError::*;

        let Specifier::Calendar(next_cal) = next else {
            return Err(UnacceptableSpecifier {
                spec: next,
                scheme_name: self.name(),
            });
        };

        *parse_state = match parse_state {
            Initial => match next_cal {
                CalendarSpecifier::Year { .. } => Year(next),
                _ => {
                    return Err(WrongFirstSpecifier {
                        spec: next,
                        scheme_name: self.name(),
                        expected_first: self.expected_first(),
                    })
                }
            },
            Year(last_year) => match next_cal {
                CalendarSpecifier::Year { .. } | CalendarSpecifier::Day { .. } => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: last_year,
                        next,
                    })
                }
                CalendarSpecifier::Month { .. } => Month(next),
                CalendarSpecifier::Week { .. } => Week(next),
            },
            Month(last_month) => match next_cal {
                CalendarSpecifier::Year { .. } | CalendarSpecifier::Month { .. } => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: last_month,
                        next,
                    })
                }
                CalendarSpecifier::Week { .. } => {
                    return Err(SpecifierMustBeRelative {
                        prev: last_month,
                        next,
                    })
                }
                CalendarSpecifier::Day { .. } => Day(next),
            },
            Week(last_week) => match next_cal {
                CalendarSpecifier::Year { .. }
                | CalendarSpecifier::Month { .. }
                | CalendarSpecifier::Week { .. } => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: last_week,
                        next,
                    })
                }
                CalendarSpecifier::Day { .. } => {
                    return Err(SpecifierMustBeRelative {
                        prev: last_week,
                        next,
                    })
                }
            },
            Day(last_day) => match next_cal {
                CalendarSpecifier::Year { .. }
                | CalendarSpecifier::Month { .. }
                | CalendarSpecifier::Week { .. }
                | CalendarSpecifier::Day { .. } => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: last_day,
                        next,
                    })
                }
            },
            _ => unreachable!(),
        };
        Ok(())
    }

    fn parse_state_is_final(&self, _: &ParseState) -> bool {
        true
    }
}

/// This method is designed for formats that have both calendar and semantic specifiers, such as
/// `[YYYY].[MM].[DD].[PATCH]`. You would have such a format if you want to be able to increase
/// your version multiple times within the period of your smallest calendar specifier, such a
/// second time in the same day, continuing with the previous example.
#[derive(Debug)]
pub struct CalSem;

impl CalSem {
    pub fn new_format(format_str: &str) -> Result<Format<Self>, VersionBumpError> {
        Format::parse(format_str, CalSem)
    }
}

impl Scheme for CalSem {
    fn name(&self) -> &'static str {
        "calendar-semantic"
    }

    fn new_format(format_str: &str) -> Result<Format<Self>, VersionBumpError> {
        Format::parse(format_str, CalSem)
    }
}

impl priv_trait::Scheme for CalSem {
    fn expected_first(&self) -> &'static str {
        &YEAR_FORMAT_STRINGS
    }

    fn advance_parse_state(
        &self,
        parse_state: &mut ParseState,
        next: &'static Specifier,
    ) -> Result<(), VersionBumpError> {
        use ParseState::*;
        use VersionBumpError::*;
        *parse_state = match parse_state {
            Initial => match next {
                Specifier::Calendar(CalendarSpecifier::Year { .. }) => Year(next),
                _ => {
                    return Err(WrongFirstSpecifier {
                        spec: next,
                        scheme_name: self.name(),
                        expected_first: self.expected_first(),
                    })
                }
            },
            Year(last_year) => match next {
                Specifier::Calendar(next_cal) => match next_cal {
                    CalendarSpecifier::Year { .. } => {
                        return Err(SpecifiersMustStrictlyDecrease {
                            prev: last_year,
                            next,
                        })
                    }
                    CalendarSpecifier::Month { .. } => Month(next),
                    CalendarSpecifier::Week { .. } => Week(next),
                    CalendarSpecifier::Day { .. } => {
                        return Err(SpecifierMustBeRelative {
                            prev: last_year,
                            next,
                        })
                    }
                },
                Specifier::Semantic(next_sem) => match next_sem {
                    SemanticSpecifier::Major => {
                        return Err(MajorInCalSemFormat);
                    }
                    SemanticSpecifier::Minor => Minor,
                    SemanticSpecifier::Patch => Patch,
                },
            },
            Month(last_month) => match next {
                Specifier::Calendar(next_cal) => match next_cal {
                    CalendarSpecifier::Year { .. } | CalendarSpecifier::Month { .. } => {
                        return Err(SpecifiersMustStrictlyDecrease {
                            prev: last_month,
                            next,
                        })
                    }
                    CalendarSpecifier::Week { .. } => {
                        return Err(SpecifierMustBeRelative {
                            prev: last_month,
                            next,
                        })
                    }
                    CalendarSpecifier::Day { .. } => Day(next),
                },
                Specifier::Semantic(next_sem) => match next_sem {
                    SemanticSpecifier::Major => {
                        return Err(MajorInCalSemFormat);
                    }
                    SemanticSpecifier::Minor => Minor,
                    SemanticSpecifier::Patch => Patch,
                },
            },
            Week(last_week) => match next {
                Specifier::Calendar(next_cal) => match next_cal {
                    CalendarSpecifier::Year { .. }
                    | CalendarSpecifier::Month { .. }
                    | CalendarSpecifier::Week { .. } => {
                        return Err(SpecifiersMustStrictlyDecrease {
                            prev: last_week,
                            next,
                        })
                    }
                    CalendarSpecifier::Day { .. } => {
                        return Err(SpecifierMustBeRelative {
                            prev: last_week,
                            next,
                        })
                    }
                },
                Specifier::Semantic(next_sem) => match next_sem {
                    SemanticSpecifier::Major => {
                        return Err(MajorInCalSemFormat);
                    }
                    SemanticSpecifier::Minor => Minor,
                    SemanticSpecifier::Patch => Patch,
                },
            },
            Day(last_day) => match next {
                Specifier::Calendar(..) => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: last_day,
                        next,
                    });
                }
                Specifier::Semantic(next_sem) => match next_sem {
                    SemanticSpecifier::Major => {
                        return Err(MajorInCalSemFormat);
                    }
                    SemanticSpecifier::Minor => Minor,
                    SemanticSpecifier::Patch => Patch,
                },
            },
            Minor => match next {
                Specifier::Semantic(next_sem) => match next_sem {
                    SemanticSpecifier::Major => {
                        return Err(SpecifiersMustStrictlyDecrease {
                            prev: &MINOR,
                            next: &MAJOR,
                        })
                    }
                    SemanticSpecifier::Minor => {
                        return Err(SpecifiersMustStrictlyDecrease {
                            prev: &MINOR,
                            next: &MINOR,
                        })
                    }
                    SemanticSpecifier::Patch => Patch,
                },
                _ => return Err(CalenderMustPrecedeSemantic { prev: &MINOR, next }),
            },
            Patch => match next {
                Specifier::Semantic(next_sem) => match next_sem {
                    SemanticSpecifier::Major => {
                        return Err(SpecifiersMustStrictlyDecrease {
                            prev: &PATCH,
                            next: &MAJOR,
                        })
                    }
                    SemanticSpecifier::Minor => {
                        return Err(SpecifiersMustStrictlyDecrease {
                            prev: &PATCH,
                            next: &MINOR,
                        })
                    }
                    SemanticSpecifier::Patch => {
                        return Err(SpecifiersMustStrictlyDecrease {
                            prev: &PATCH,
                            next: &PATCH,
                        })
                    }
                },
                _ => return Err(CalenderMustPrecedeSemantic { prev: &PATCH, next }),
            },
            _ => unreachable!(), // for major
        };
        Ok(())
    }

    fn parse_state_is_final(&self, parse_state: &ParseState) -> bool {
        use ParseState::*;
        matches!(parse_state, Major | Minor | Patch)
    }
}
