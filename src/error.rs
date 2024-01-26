// TODO: we need to break this down and have functions return a subset of these that can actually
// happen. first pass: format errors, version errors, increment errors. and then, make a top-level
// error that accepts these categories as variants.
//
// TODO: Make a conveniece Error type
//
// TODO: See where we can use static strs. maybe even lifetimed strs if we break out the categories

/// Errors that can occur in this library
#[non_exhaustive]
#[derive(thiserror::Error, Debug, PartialEq)]
pub enum NextverError {
    #[error("date provided yielded an identical version")]
    NoCalendarChange,

    #[error("year `{year}` should not be negative when formatted`")]
    NegativeYearValue { year: i32 },

    #[error("version `{version_string}` should match format `{format_string}`")]
    VersionFormatMismatch {
        version_string: String,
        format_string: String,
    },

    #[error("unknown specifier pattern `{pattern}` in format")]
    UnknownSpecifier { pattern: String },

    #[error(
        "specifier in format should be terminated with a closing square bracket (`]`), got `{pattern}`"
    )]
    UnterminatedSpecifier { pattern: String },

    #[error("`{spec}` was not found in format, try using a format that is present")]
    SemanticSpecifierNotInFormat { spec: String },

    #[error("calendar specifiers should be present in format")]
    CalendarNotInFormat,

    #[error("explicit year ({year}), month ({month}), and day ({day}) arguments cannot be made into a valid date")]
    InvalidDateArguments { year: i32, month: u32, day: u32 },

    #[error("specifiers must strictly decrease, got `{next}` after `{prev}`")]
    SpecifiersMustStrictlyDecrease {
        prev: String,
        next: String,
    },

    #[error("in {scheme_name} format, first specifier should be {expected_first}, got `{spec}`")]
    WrongFirstSpecifier {
        spec: String,
        scheme_name: String,
        expected_first: String,
    },

    #[error("all calendar specifiers should precede all semantic specifiers, got `{next}` after `{prev}`")]
    CalenderMustPrecedeSemantic {
        prev: String,
        next: String,
    },

    #[error("specifier `{next}` should be relative to its predecessor `{prev}`")]
    SpecifierMustBeRelative {
        prev: String,
        next: String,
    },

    #[error("unacceptable specifier `{spec}` in {scheme_name} format")]
    UnacceptableSpecifier {
        spec: String,
        scheme_name: &'static str,
    },

    #[error("format should end with a semantic specifier, got `{last_spec}`")]
    FormatIncomplete { last_spec: String },

    #[error("format should contain at least one specifier")]
    NoSpecifiersInFormat,
}
