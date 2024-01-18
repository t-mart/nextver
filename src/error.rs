/// Errors that can occur in this library
// TODO: can we use static references to specifiers? would we run into any cyclic dependency issues?
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

    #[error("Specifier in format should be terminated with a closing square bracket (`]`): {pattern}")]
    UnterminatedSpecifier { pattern: String },

    #[error("To increment `{name}`, it should be present in format")]
    SemanticLevelNotInFormat { name: &'static str },

    #[error("Calendar specifiers should be present in format")]
    CalendarNotInFormat,

    #[error("Explicit year ({year}), month ({month}), and day ({day}) arguments cannot be made into a valid date")]
    InvalidDateArguments { year: i32, month: u32, day: u32 },

    #[error("Specifiers must strictly decrease, got `{next_spec}` after `{prev_spec}`")]
    SpecifiersMustStrictlyDecrease {
        prev_spec: &'static str,
        next_spec: &'static str,
    },

    #[error("One and only one non-cyclic specifier should be in format, and should be first, got `{spec}`")]
    NonCyclicSpecifierNotFirst { spec: &'static str },

    #[error("All calendar specifiers should precede all semantic specifiers, got `{next_spec}` after `{prev_spec}`")]
    CalenderMustPrecedeSemantic {
        prev_spec: &'static str,
        next_spec: &'static str,
    },
}
