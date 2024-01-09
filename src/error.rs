#[derive(thiserror::Error, Debug, PartialEq)]
pub enum VersionBumpError {
    #[error("Version should change, but did not when incrementing")]
    NoChange,

    #[error("Year `{year}` should not be negative when formatted`")]
    NegativeYearValue { year: i32 },

    #[error("Version should match format")]
    VersionFormatMismatch,

    #[error("At least one of args semantic_level or date should be Some")]
    NothingToIncrement,

    #[error("Unknown specifier `{spec_pattern}` in format")]
    UnknownSpecifier { spec_pattern: String },

    #[error("Specifier in format should be terminated with a closing square bracket (`]`): {unterminated}")]
    UnterminatedSpecifier { unterminated: String },

    #[error("To increment `{name}`, it should be present in format")]
    SemanticLevelSpecifierNotInFormat { name: &'static str },

    #[error("Specifier for `{expected_first}` should come before specifier for `{expected_last}` in format")]
    SpecifiersOutOfOrder {
        expected_first: &'static str,
        expected_last: &'static str,
    },

    #[error("Explicit year ({year}), month ({month}), and day ({day}) arguments cannot be made into a valid date")]
    InvalidDateArguments { year: i32, month: u32, day: u32 },
}
