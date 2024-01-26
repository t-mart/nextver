use crate::{version, Cal, CalSem, Scheme, Sem};
use std::cmp::Ordering;
use std::marker::PhantomData;

#[derive(Debug, PartialEq, Eq)]
enum YearType {
    Full,
    Short,
    ZeroPadded,
}

#[derive(Debug, PartialEq, Eq)]
enum NonYearType {
    Short,
    ZeroPadded,
}

#[derive(Debug, PartialEq, Eq)]
enum Specifier<S: Scheme> {
    SemMajor {
        scheme: PhantomData<S>,
    },
    SemMinor {
        scheme: PhantomData<S>,
    },
    SemPatch {
        scheme: PhantomData<S>,
    },
    CalYear {
        scheme: PhantomData<S>,
        type_: YearType,
    },
    CalMonth {
        scheme: PhantomData<S>,
        type_: NonYearType,
    },
    CalWeek {
        scheme: PhantomData<S>,
        type_: NonYearType,
    },
    CalDay {
        scheme: PhantomData<S>,
        type_: NonYearType,
    },
}

// not zero padded, any length
const SEM_VERSION_PATTERN: &str = r"([1-9][0-9]*)";
// not zero padded, 1-2 digits, no 0
const CAL_SHORT_MONTH_DAY_PATTERN: &str = r"([1-9][0-9]?)";
// zero padded, 2 digits, no 00
const CAL_ZERO_PADDED_MONTH_DAY_PATTERN: &str = r"((?:0[1-9])|(?:[1-9][0-9]))";

impl<S: Scheme> Specifier<S> {
    pub fn format_pattern(&self) -> &'static str {
        use Specifier::*;
        let format_pattern = match self {
            SemMajor { .. } => "[MAJOR]",
            SemMinor { .. } => "[MINOR]",
            SemPatch { .. } => "[PATCH]",
            CalYear { type_, .. } => match type_ {
                YearType::Full => "[YYYY]",
                YearType::Short => "[YY]",
                YearType::ZeroPadded => "[0Y]",
            },
            CalMonth { type_, .. } => match type_ {
                NonYearType::Short => "[MM]",
                NonYearType::ZeroPadded => "[0M]",
            },
            CalWeek { type_, .. } => match type_ {
                NonYearType::Short => "[WW]",
                NonYearType::ZeroPadded => "[0W]",
            },
            CalDay { type_, .. } => match type_ {
                NonYearType::Short => "[DD]",
                NonYearType::ZeroPadded => "[0D]",
            },
        };
        debug_assert!(
            format_pattern.starts_with('[')
                && format_pattern.ends_with(']')
                && format_pattern.len() > 2
        );
        format_pattern
    }

    pub fn version_pattern(&self) -> &'static str {
        use Specifier::*;
        let version_pattern = match self {
            SemMajor { .. } => SEM_VERSION_PATTERN,
            SemMinor { .. } => SEM_VERSION_PATTERN,
            SemPatch { .. } => SEM_VERSION_PATTERN,
            CalYear { type_, .. } => match type_ {
                YearType::Full => r"([0-9]{4})",
                YearType::Short => r"([0-9]{2})",
                YearType::ZeroPadded => r"([0-9]{2})",
            },
            CalMonth { type_, .. } => match type_ {
                NonYearType::Short => CAL_SHORT_MONTH_DAY_PATTERN,
                NonYearType::ZeroPadded => CAL_ZERO_PADDED_MONTH_DAY_PATTERN,
            },
            CalWeek { type_, .. } => match type_ {
                NonYearType::Short => r"([0-9]{1,2})",
                NonYearType::ZeroPadded => r"([0-9]{2})",
            },
            CalDay { type_, .. } => match type_ {
                NonYearType::Short => CAL_SHORT_MONTH_DAY_PATTERN,
                NonYearType::ZeroPadded => CAL_ZERO_PADDED_MONTH_DAY_PATTERN,
            },
        };
        debug_assert!(
            version_pattern.starts_with('(')
                && version_pattern.ends_with(')')
                && version_pattern.len() > 2
        );
        version_pattern
    }

    pub fn zero_pad_len(&self) -> Option<usize> {
        use Specifier::*;
        match self {
            CalYear {
                type_: YearType::ZeroPadded,
                ..
            }
            | CalMonth {
                type_: NonYearType::ZeroPadded,
                ..
            }
            | CalWeek {
                type_: NonYearType::ZeroPadded,
                ..
            }
            | CalDay {
                type_: NonYearType::ZeroPadded,
                ..
            } => Some(2),
            _ => None,
        }
    }

    pub fn format_value(&self, value: &u32) -> String {
        match self.zero_pad_len() {
            Some(len) => format!("{:0len$}", value, len = len),
            None => u32::to_string(value),
        }
    }

    pub fn parse_value_str(&'static self, value: &str) -> u32 {
        value.parse().unwrap()
    }
}

impl PartialOrd for Specifier<Sem> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use Ordering::*;
        use Specifier::*;
        match (self, other) {
            (SemMajor { .. }, SemMajor { .. }) => Some(Equal),
            (SemMajor { .. }, _) => Some(Greater),
            (_, SemMajor { .. }) => Some(Less),

            (SemMinor { .. }, SemMinor { .. }) => Some(Equal),
            (SemMinor { .. }, _) => Some(Greater),
            (_, SemMinor { .. }) => Some(Less),

            (SemPatch { .. }, SemPatch { .. }) => Some(Equal),

            _ => None,
        }
    }
}

impl PartialOrd for Specifier<Cal> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use Ordering::*;
        use Specifier::*;

        // some nuance here: comparing week to month is invalid (they're both year-relative), and
        // transitively, so is comparing week to day (day is month-relative).
        match (self, other) {
            (CalYear { .. }, CalYear { .. }) => Some(Equal),
            (CalYear { .. }, CalDay { .. }) => None,
            (CalYear { .. }, _) => Some(Greater),
            (CalDay { .. }, CalYear { .. }) => None,
            (_, CalYear { .. }) => Some(Less),

            (CalMonth { .. }, CalMonth { .. }) => Some(Equal),
            (CalMonth { .. }, CalWeek { .. }) => None,
            (CalMonth { .. }, _) => Some(Greater),
            (CalWeek { .. }, CalMonth { .. }) => None,
            (_, CalMonth { .. }) => Some(Less),

            (CalWeek { .. }, CalWeek { .. }) => Some(Equal),
            (CalWeek { .. }, CalDay { .. }) => None,
            (CalWeek { .. }, _) => Some(Greater),
            (CalDay { .. }, CalWeek { .. }) => None,
            (_, CalWeek { .. }) => Some(Less),

            (CalDay { .. }, CalDay { .. }) => Some(Equal),

            _ => None,
        }
    }
}

impl PartialOrd for Specifier<CalSem> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        use Ordering::*;
        use Specifier::*;
        match (self, other) {
            (CalYear { .. }, CalYear { .. }) => Some(Equal),
            (CalYear { .. }, CalDay { .. }) => None,
            (CalYear { .. }, _) => Some(Greater),
            (CalDay { .. }, CalYear { .. }) => None,
            (_, CalYear { .. }) => Some(Less),

            (CalMonth { .. }, CalMonth { .. }) => Some(Equal),
            (CalMonth { .. }, CalWeek { .. }) => None,
            (CalMonth { .. }, _) => Some(Greater),
            (CalWeek { .. }, CalMonth { .. }) => None,
            (_, CalMonth { .. }) => Some(Less),

            (CalWeek { .. }, CalWeek { .. }) => Some(Equal),
            (CalWeek { .. }, CalDay { .. }) => None,
            (CalWeek { .. }, _) => Some(Greater),
            (CalDay { .. }, CalWeek { .. }) => None,
            (_, CalWeek { .. }) => Some(Less),

            (CalDay { .. }, CalDay { .. }) => Some(Equal),
            (CalDay { .. }, _) => Some(Greater),
            (_, CalDay { .. }) => Some(Less),

            (SemMinor { .. }, SemMinor { .. }) => Some(Equal),
            (SemMinor { .. }, _) => Some(Greater),
            (_, SemMinor { .. }) => Some(Less),

            (SemPatch { .. }, SemPatch { .. }) => Some(Equal),

            _ => None,
        }
    }
}

static SEM_MAJOR: Specifier<Sem> = Specifier::SemMajor {
    scheme: PhantomData,
};
static SEM_MINOR: Specifier<Sem> = Specifier::SemMinor {
    scheme: PhantomData,
};
static SEM_PATCH: Specifier<Sem> = Specifier::SemPatch {
    scheme: PhantomData,
};
static SEM_ALL: &[&Specifier<Sem>] = &[&SEM_MAJOR, &SEM_MINOR, &SEM_PATCH];

static CAL_YEAR_FULL: Specifier<Cal> = Specifier::CalYear {
    scheme: PhantomData,
    type_: YearType::Full,
};
static CAL_YEAR_SHORT: Specifier<Cal> = Specifier::CalYear {
    scheme: PhantomData,
    type_: YearType::Short,
};
static CAL_YEAR_ZERO_PADDED: Specifier<Cal> = Specifier::CalYear {
    scheme: PhantomData,
    type_: YearType::ZeroPadded,
};
static CAL_MONTH_SHORT: Specifier<Cal> = Specifier::CalMonth {
    scheme: PhantomData,
    type_: NonYearType::Short,
};
static CAL_MONTH_ZERO_PADDED: Specifier<Cal> = Specifier::CalMonth {
    scheme: PhantomData,
    type_: NonYearType::ZeroPadded,
};
static CAL_WEEK_SHORT: Specifier<Cal> = Specifier::CalWeek {
    scheme: PhantomData,
    type_: NonYearType::Short,
};
static CAL_WEEK_ZERO_PADDED: Specifier<Cal> = Specifier::CalWeek {
    scheme: PhantomData,
    type_: NonYearType::ZeroPadded,
};
static CAL_DAY_SHORT: Specifier<Cal> = Specifier::CalDay {
    scheme: PhantomData,
    type_: NonYearType::Short,
};
static CAL_DAY_ZERO_PADDED: Specifier<Cal> = Specifier::CalDay {
    scheme: PhantomData,
    type_: NonYearType::ZeroPadded,
};
static CAL_ALL: &[&Specifier<Cal>] = &[
    &CAL_YEAR_FULL,
    &CAL_YEAR_SHORT,
    &CAL_YEAR_ZERO_PADDED,
    &CAL_MONTH_SHORT,
    &CAL_MONTH_ZERO_PADDED,
    &CAL_WEEK_SHORT,
    &CAL_WEEK_ZERO_PADDED,
    &CAL_DAY_SHORT,
    &CAL_DAY_ZERO_PADDED,
];

static CAL_SEM_MINOR: Specifier<CalSem> = Specifier::SemMinor {
    scheme: PhantomData,
};
static CAL_SEM_PATCH: Specifier<CalSem> = Specifier::SemPatch {
    scheme: PhantomData,
};
static CAL_SEM_YEAR_FULL: Specifier<CalSem> = Specifier::CalYear {
    scheme: PhantomData,
    type_: YearType::Full,
};
static CAL_SEM_YEAR_SHORT: Specifier<CalSem> = Specifier::CalYear {
    scheme: PhantomData,
    type_: YearType::Short,
};
static CAL_SEM_YEAR_ZERO_PADDED: Specifier<CalSem> = Specifier::CalYear {
    scheme: PhantomData,
    type_: YearType::ZeroPadded,
};
static CAL_SEM_MONTH_SHORT: Specifier<CalSem> = Specifier::CalMonth {
    scheme: PhantomData,
    type_: NonYearType::Short,
};
static CAL_SEM_MONTH_ZERO_PADDED: Specifier<CalSem> = Specifier::CalMonth {
    scheme: PhantomData,
    type_: NonYearType::ZeroPadded,
};
static CAL_SEM_WEEK_SHORT: Specifier<CalSem> = Specifier::CalWeek {
    scheme: PhantomData,
    type_: NonYearType::Short,
};
static CAL_SEM_WEEK_ZERO_PADDED: Specifier<CalSem> = Specifier::CalWeek {
    scheme: PhantomData,
    type_: NonYearType::ZeroPadded,
};
static CAL_SEM_DAY_SHORT: Specifier<CalSem> = Specifier::CalDay {
    scheme: PhantomData,
    type_: NonYearType::Short,
};
static CAL_SEM_DAY_ZERO_PADDED: Specifier<CalSem> = Specifier::CalDay {
    scheme: PhantomData,
    type_: NonYearType::ZeroPadded,
};
static CAL_SEM_ALL: &[&Specifier<CalSem>] = &[
    &CAL_SEM_MINOR,
    &CAL_SEM_PATCH,
    &CAL_SEM_YEAR_FULL,
    &CAL_SEM_YEAR_SHORT,
    &CAL_SEM_YEAR_ZERO_PADDED,
    &CAL_SEM_MONTH_SHORT,
    &CAL_SEM_MONTH_ZERO_PADDED,
    &CAL_SEM_WEEK_SHORT,
    &CAL_SEM_WEEK_ZERO_PADDED,
    &CAL_SEM_DAY_SHORT,
    &CAL_SEM_DAY_ZERO_PADDED,
];

#[cfg(test)]
mod tests {
    use super::*;
    // use Specifier::*;
    use itertools::Itertools;

    #[test]
    fn sem_ordering() {
        use Ordering::*;

        assert_eq!(SEM_MAJOR.partial_cmp(&SEM_MAJOR), Some(Equal));
        assert_eq!(SEM_MAJOR.partial_cmp(&SEM_MINOR), Some(Greater));
        assert_eq!(SEM_MAJOR.partial_cmp(&SEM_PATCH), Some(Greater));

        assert_eq!(SEM_MINOR.partial_cmp(&SEM_MAJOR), Some(Less));
        assert_eq!(SEM_MINOR.partial_cmp(&SEM_MINOR), Some(Equal));
        assert_eq!(SEM_MINOR.partial_cmp(&SEM_PATCH), Some(Greater));

        assert_eq!(SEM_PATCH.partial_cmp(&SEM_MAJOR), Some(Less));
        assert_eq!(SEM_PATCH.partial_cmp(&SEM_MINOR), Some(Less));
        assert_eq!(SEM_PATCH.partial_cmp(&SEM_PATCH), Some(Equal));
    }

    #[test]
    fn cal_ordering() {
        use std::iter::once;
        use Ordering::*;

        let years = || once(vec![&CAL_YEAR_FULL, &CAL_YEAR_SHORT, &CAL_YEAR_ZERO_PADDED]);
        let months = || once(vec![&CAL_MONTH_SHORT, &CAL_MONTH_ZERO_PADDED]);
        let weeks = || once(vec![&CAL_WEEK_SHORT, &CAL_WEEK_ZERO_PADDED]);
        let days = || once(vec![&CAL_DAY_SHORT, &CAL_DAY_ZERO_PADDED]);

        // first, all within same type should be equal
        years()
            .chain(months())
            .chain(weeks())
            .chain(days())
            .for_each(|kind| {
                kind.iter().permutations(2).for_each(|pair| {
                    assert_eq!(pair[0].partial_cmp(pair[1]), Some(Equal));
                })
            });

        // then, compare in pairs
        // year > month
        years()
            .cartesian_product(months())
            .for_each(|(year, month)| {
                assert_eq!(year.partial_cmp(&month), Some(Greater));
                assert_eq!(month.partial_cmp(&year), Some(Less));
            });

        // year > week
        years().cartesian_product(weeks()).for_each(|(year, week)| {
            assert_eq!(year.partial_cmp(&week), Some(Greater));
            assert_eq!(week.partial_cmp(&year), Some(Less));
        });

        // year <> day
        years().cartesian_product(days()).for_each(|(year, day)| {
            assert_eq!(year.partial_cmp(&day), None);
            assert_eq!(day.partial_cmp(&year), None);
        });

        // month <> week
        months()
            .cartesian_product(weeks())
            .for_each(|(month, week)| {
                assert_eq!(month.partial_cmp(&week), None);
                assert_eq!(week.partial_cmp(&month), None);
            });

        // month > day
        months().cartesian_product(days()).for_each(|(month, day)| {
            assert_eq!(month.partial_cmp(&day), Some(Greater));
            assert_eq!(day.partial_cmp(&month), Some(Less));
        });

        // week <> day
        weeks().cartesian_product(days()).for_each(|(week, day)| {
            assert_eq!(week.partial_cmp(&day), None);
            assert_eq!(day.partial_cmp(&week), None);
        });
    }

    #[test]
    fn cal_sem_ordering() {
        use std::iter::once;
        use Ordering::*;

        let years = || {
            once(vec![
                &CAL_SEM_YEAR_FULL,
                &CAL_SEM_YEAR_SHORT,
                &CAL_SEM_YEAR_ZERO_PADDED,
            ])
        };
        let months = || once(vec![&CAL_SEM_MONTH_SHORT, &CAL_SEM_MONTH_ZERO_PADDED]);
        let weeks = || once(vec![&CAL_SEM_WEEK_SHORT, &CAL_SEM_WEEK_ZERO_PADDED]);
        let days = || once(vec![&CAL_SEM_DAY_SHORT, &CAL_SEM_DAY_ZERO_PADDED]);
        let minors = || once(vec![&CAL_SEM_MINOR]);
        let patches = || once(vec![&CAL_SEM_PATCH]);

        // first, all within same type should be equal
        // don't need to do minors and patches because they're len = 1
        years()
            .chain(months())
            .chain(weeks())
            .chain(days())
            .for_each(|kind| {
                kind.iter().permutations(2).for_each(|pair| {
                    assert_eq!(pair[0].partial_cmp(pair[1]), Some(Equal));
                })
            });

        // then, compare in pairs
        // year > month
        years()
            .cartesian_product(months())
            .for_each(|(year, month)| {
                assert_eq!(year.partial_cmp(&month), Some(Greater));
                assert_eq!(month.partial_cmp(&year), Some(Less));
            });

        // year > week
        years().cartesian_product(weeks()).for_each(|(year, week)| {
            assert_eq!(year.partial_cmp(&week), Some(Greater));
            assert_eq!(week.partial_cmp(&year), Some(Less));
        });

        // year <> day
        years().cartesian_product(days()).for_each(|(year, day)| {
            assert_eq!(year.partial_cmp(&day), None);
            assert_eq!(day.partial_cmp(&year), None);
        });

        // year > minor
        years()
            .cartesian_product(minors())
            .for_each(|(year, minor)| {
                assert_eq!(year.partial_cmp(&minor), Some(Greater));
                assert_eq!(minor.partial_cmp(&year), Some(Less));
            });

        // year > patch
        years()
            .cartesian_product(patches())
            .for_each(|(year, patch)| {
                assert_eq!(year.partial_cmp(&patch), Some(Greater));
                assert_eq!(patch.partial_cmp(&year), Some(Less));
            });

        // month <> week
        months()
            .cartesian_product(weeks())
            .for_each(|(month, week)| {
                assert_eq!(month.partial_cmp(&week), None);
                assert_eq!(week.partial_cmp(&month), None);
            });

        // month > day
        months().cartesian_product(days()).for_each(|(month, day)| {
            assert_eq!(month.partial_cmp(&day), Some(Greater));
            assert_eq!(day.partial_cmp(&month), Some(Less));
        });

        // month > minor
        months()
            .cartesian_product(minors())
            .for_each(|(month, minor)| {
                assert_eq!(month.partial_cmp(&minor), Some(Greater));
                assert_eq!(minor.partial_cmp(&month), Some(Less));
            });

        // month > patch
        months()
            .cartesian_product(patches())
            .for_each(|(month, patch)| {
                assert_eq!(month.partial_cmp(&patch), Some(Greater));
                assert_eq!(patch.partial_cmp(&month), Some(Less));
            });

        // week <> day
        weeks().cartesian_product(days()).for_each(|(week, day)| {
            assert_eq!(week.partial_cmp(&day), None);
            assert_eq!(day.partial_cmp(&week), None);
        });

        // week > minor
        weeks()
            .cartesian_product(minors())
            .for_each(|(week, minor)| {
                assert_eq!(week.partial_cmp(&minor), Some(Greater));
                assert_eq!(minor.partial_cmp(&week), Some(Less));
            });

        // week > patch
        weeks()
            .cartesian_product(patches())
            .for_each(|(week, patch)| {
                assert_eq!(week.partial_cmp(&patch), Some(Greater));
                assert_eq!(patch.partial_cmp(&week), Some(Less));
            });

        // day > minor
        days().cartesian_product(minors()).for_each(|(day, minor)| {
            assert_eq!(day.partial_cmp(&minor), Some(Greater));
            assert_eq!(minor.partial_cmp(&day), Some(Less));
        });

        // day > patch
        days()
            .cartesian_product(patches())
            .for_each(|(day, patch)| {
                assert_eq!(day.partial_cmp(&patch), Some(Greater));
                assert_eq!(patch.partial_cmp(&day), Some(Less));
            });

        // minor > patch
        minors()
            .cartesian_product(patches())
            .for_each(|(minor, patch)| {
                assert_eq!(minor.partial_cmp(&patch), Some(Greater));
                assert_eq!(patch.partial_cmp(&minor), Some(Less));
            });
    }
}
