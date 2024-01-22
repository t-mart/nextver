use crate::format::ParseState;
use crate::specifier::{
    Specifier, Type, FULL_YEAR, MAJOR, MINOR, PATCH, SHORT_YEAR, ZERO_PADDED_YEAR,
};
use crate::{Format, VersionBumpError};

// TODO: use downcast_rs to go from trait object to concrete type in CLI

pub trait Scheme: Sized {
    /// Returns the name of the scheme for error messages.
    fn name(&self) -> &'static str;

    /// Returns a human readable string of the first specifier(s) that should be present in the
    /// format string. Used for error messages.
    fn expected_first(&self) -> String;

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
    fn parse_state_is_final(&self, ps: &ParseState) -> bool;
}

pub struct Sem;

impl Sem {
    pub fn new_format(format_str: &str) -> Result<Format<Self>, VersionBumpError> {
        Format::parse(format_str, Sem)
    }
}

impl Scheme for Sem {
    fn name(&self) -> &'static str {
        "semantic"
    }

    fn expected_first(&self) -> String {
        MAJOR.to_string()
    }

    fn advance_parse_state(
        &self,
        parse_state: &mut ParseState,
        next: &'static Specifier,
    ) -> Result<(), VersionBumpError> {
        use ParseState::*;
        use VersionBumpError::*;

        if !matches!(next, Specifier::Semantic { .. }) {
            return Err(UnacceptableSpecifier {
                spec: next,
                scheme_name: self.name().to_owned(),
            });
        }

        let type_ = &next.type_;
        *parse_state = match parse_state {
            Initial => match type_ {
                Type::Major => Major,
                _ => {
                    return Err(WrongFirstSpecifier {
                        spec: next,
                        scheme_name: self.name().to_owned(),
                        expected_first: self.expected_first(),
                    })
                }
            },
            Major => match type_ {
                Type::Major => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: &MAJOR,
                        next: &MAJOR,
                    })
                }
                Type::Minor => Minor,
                Type::Patch => Patch,
                _ => unreachable!(),
            },
            Minor => match type_ {
                Type::Major => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: &MINOR,
                        next: &MAJOR,
                    })
                }
                Type::Minor => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: &MINOR,
                        next: &MINOR,
                    })
                }
                Type::Patch => Patch,
                _ => unreachable!(),
            },
            Patch => match type_ {
                Type::Major => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: &PATCH,
                        next: &MAJOR,
                    })
                }
                Type::Minor => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: &PATCH,
                        next: &MINOR,
                    })
                }
                Type::Patch => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: &PATCH,
                        next: &PATCH,
                    })
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };
        Ok(())
    }

    fn parse_state_is_final(&self, _: &ParseState) -> bool {
        true
    }
}

fn cal_expected_first() -> String {
    [&ZERO_PADDED_YEAR, &SHORT_YEAR, &FULL_YEAR]
        .iter()
        .map(|spec| spec.format_pattern)
        .collect::<Vec<_>>()
        .join(", ")
}
pub struct Cal;

impl Cal {
    pub fn new_format(format_str: &str) -> Result<Format<Self>, VersionBumpError> {
        Format::parse(format_str, Cal)
    }
}

impl Scheme for Cal {
    fn name(&self) -> &'static str {
        "calendar"
    }

    fn expected_first(&self) -> String {
        cal_expected_first()
    }

    fn advance_parse_state(
        &self,
        parse_state: &mut ParseState,
        next: &'static Specifier,
    ) -> Result<(), VersionBumpError> {
        use ParseState::*;
        use VersionBumpError::*;

        if !matches!(next, Specifier::Calendar { .. }) {
            return Err(UnacceptableSpecifier {
                spec: next,
                scheme_name: self.name().to_owned(),
            });
        }

        let type_ = &next.type_;
        *parse_state = match parse_state {
            Initial => match type_ {
                Type::Year => Year(next),
                _ => {
                    return Err(WrongFirstSpecifier {
                        spec: next,
                        scheme_name: self.name().to_owned(),
                        expected_first: self.expected_first(),
                    })
                }
            },
            Year(last_year) => match type_ {
                Type::Year | Type::Day => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: last_year,
                        next,
                    })
                }
                Type::Month => Month(next),
                Type::Week => Week(next),
                _ => unreachable!(),
            },
            Month(last_month) => match type_ {
                Type::Year | Type::Month => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: last_month,
                        next,
                    })
                }
                Type::Week => {
                    return Err(SpecifierMustBeRelative {
                        prev: last_month,
                        next,
                    })
                }
                Type::Day => Day(next),
                _ => unreachable!(),
            },
            Week(last_week) => match type_ {
                Type::Year | Type::Month | Type::Week => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: last_week,
                        next,
                    })
                }
                Type::Day => {
                    return Err(SpecifierMustBeRelative {
                        prev: last_week,
                        next,
                    })
                }
                _ => unreachable!(),
            },
            Day(last_day) => match type_ {
                Type::Year | Type::Month | Type::Week | Type::Day => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: last_day,
                        next,
                    })
                }
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };
        Ok(())
    }

    fn parse_state_is_final(&self, _: &ParseState) -> bool {
        true
    }
}

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

    fn expected_first(&self) -> String {
        format!("{}, {}", cal_expected_first(), MAJOR)
    }

    fn advance_parse_state(
        &self,
        parse_state: &mut ParseState,
        next: &'static Specifier,
    ) -> Result<(), VersionBumpError> {
        use ParseState::*;
        use VersionBumpError::*;
        
        let type_ = &next.type_;
        *parse_state = match parse_state {
            Initial => match type_ {
                Type::Year => Year(next),
                _ => {
                    return Err(WrongFirstSpecifier {
                        spec: next,
                        scheme_name: self.name().to_string(),
                        expected_first: self.expected_first(),
                    })
                }
            },
            Year(last_year) => match type_ {
                Type::Year => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: last_year,
                        next,
                    })
                }
                Type::Month => Month(next),
                Type::Week => Week(next),
                Type::Day => {
                    return Err(SpecifierMustBeRelative {
                        prev: last_year,
                        next,
                    })
                }
                Type::Major => {
                    return Err(MajorInCalSemFormat {
                        spec: &MAJOR,
                        scheme_name: self.name().to_string(),
                    });
                }
                Type::Minor => Minor,
                Type::Patch => Patch,
            },
            Month(last_month) => match type_ {
                Type::Year | Type::Month => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: last_month,
                        next,
                    })
                }
                Type::Week => {
                    return Err(SpecifierMustBeRelative {
                        prev: last_month,
                        next,
                    })
                }
                Type::Day => Day(next),
                Type::Major => {
                    return Err(MajorInCalSemFormat {
                        spec: &MAJOR,
                        scheme_name: self.name().to_string(),
                    });
                }
                Type::Minor => Minor,
                Type::Patch => Patch,
            },
            Week(last_week) => match type_ {
                Type::Year | Type::Month | Type::Week => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: last_week,
                        next,
                    })
                }
                Type::Day => {
                    return Err(SpecifierMustBeRelative {
                        prev: last_week,
                        next,
                    })
                }
                Type::Major => {
                    return Err(MajorInCalSemFormat {
                        spec: &MAJOR,
                        scheme_name: self.name().to_string(),
                    });
                }
                Type::Minor => Minor,
                Type::Patch => Patch,
            },
            Day(last_day) => match type_ {
                Type::Year | Type::Month | Type::Week | Type::Day => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: last_day,
                        next,
                    })
                }
                Type::Major => {
                    return Err(MajorInCalSemFormat {
                        spec: &MAJOR,
                        scheme_name: self.name().to_string(),
                    });
                }
                Type::Minor => Minor,
                Type::Patch => Patch,
            },
            Minor => match type_ {
                Type::Major => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: &MINOR,
                        next: &MAJOR,
                    })
                }
                Type::Minor => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: &MINOR,
                        next: &MINOR,
                    })
                }
                Type::Patch => Patch,
                _ => return Err(CalenderMustPrecedeSemantic { prev: &MINOR, next }),
            },
            Patch => match type_ {
                Type::Major => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: &PATCH,
                        next: &MAJOR,
                    })
                }
                Type::Minor => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: &PATCH,
                        next: &MINOR,
                    })
                }
                Type::Patch => {
                    return Err(SpecifiersMustStrictlyDecrease {
                        prev: &PATCH,
                        next: &PATCH,
                    })
                }
                _ => return Err(CalenderMustPrecedeSemantic { prev: &PATCH, next }),
            },
            _ => unreachable!(), // for major
        };
        Ok(())
    }

    fn parse_state_is_final(&self, ps: &ParseState) -> bool {
        use ParseState::*;
        matches!(ps, Major | Minor | Patch)
    }
}
