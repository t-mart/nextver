use crate::{
    scheme::{CalSem, Scheme},
    specifier::{SemanticSpecifier, Specifier, MAJOR},
};

// TODO: we need to break this down. first pass: format errors, version errors, increment errors.
// and then, make a top-level error that accepts these categories as variants.
//
// TODO: Make a conveniece Error type

/// Errors that can occur in this library
#[non_exhaustive]
#[derive(thiserror::Error, Debug, PartialEq)]
pub enum VersionBumpError {
    #[error("Version should change when incrementing calendar specifiers, but did not")]
    NoCalendarChange,

    #[error("Year `{year}` should not be negative when formatted`")]
    NegativeYearValue { year: i32 },

    #[error("Version `{version_string}` should match format `{format_string}`")]
    VersionFormatMismatch {
        version_string: String,
        format_string: String,
    },

    #[error("Unknown specifier pattern `{pattern}` in format")]
    UnknownSpecifier { pattern: String },

    #[error(
        "Specifier in format should be terminated with a closing square bracket (`]`): {pattern}"
    )]
    UnterminatedSpecifier { pattern: String },

    #[error("To increment `{spec}`, it should be present in format")]
    SemanticSpecifierNotInFormat { spec: SemanticSpecifier },

    #[error("Calendar specifiers should be present in format")]
    CalendarNotInFormat,

    #[error("Explicit year ({year}), month ({month}), and day ({day}) arguments cannot be made into a valid date")]
    InvalidDateArguments { year: i32, month: u32, day: u32 },

    #[error("Specifiers must strictly decrease, got `{next}` after `{prev}`")]
    SpecifiersMustStrictlyDecrease {
        prev: &'static Specifier,
        next: &'static Specifier,
    },

    #[error("In {scheme_name} format, first specifier should be {expected_first} got `{spec}`")]
    WrongFirstSpecifier {
        spec: &'static Specifier,
        scheme_name: &'static str,
        expected_first: &'static str,
    },

    #[error("All calendar specifiers should precede all semantic specifiers, got `{next}` after `{prev}`")]
    CalenderMustPrecedeSemantic {
        prev: &'static Specifier,
        next: &'static Specifier,
    },

    #[error("{} should not be in a {} format", &MAJOR, CalSem.name())]
    MajorInCalSemFormat,

    #[error("Specifier `{next}` should be relative to its predecessor `{prev}`")]
    SpecifierMustBeRelative {
        prev: &'static Specifier,
        next: &'static Specifier,
    },

    #[error("Unacceptable specifier `{spec}` in {scheme_name} format")]
    UnacceptableSpecifier {
        spec: &'static Specifier,
        scheme_name: &'static str,
    },

    #[error("Format should end with a semantic specifier, got `{last_spec}`")]
    FormatIncomplete { last_spec: &'static Specifier },

    #[error("Format should contain at least one specifier")]
    NoSpecifiersInFormat,
}
