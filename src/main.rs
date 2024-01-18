use chrono::{Datelike, NaiveDate};
use clap::{arg, command, value_parser, ArgAction, Args, Command, Parser, Subcommand, ValueEnum};

use version_bump::{Date, SemanticLevel, Version, VersionBumpError};

#[derive(thiserror::Error, Debug)]
pub enum VersionBumpCliError {
    #[error("{0}")]
    LibraryError(#[from] version_bump::VersionBumpError),

    #[error("{0}")]
    UnparseableDate(#[from] chrono::ParseError),

    #[error("{0}")]
    CliParseError(#[from] clap::Error),
}

#[derive(Clone, PartialEq, Eq, ValueEnum, Debug)]
enum SemanticLevelArg {
    Major,
    Minor,
    Patch,
}

impl SemanticLevelArg {
    fn _to_semantic_level(&self) -> SemanticLevel {
        match self {
            SemanticLevelArg::Major => SemanticLevel::Major,
            SemanticLevelArg::Minor => SemanticLevel::Minor,
            SemanticLevelArg::Patch => SemanticLevel::Patch,
        }
    }
}

/// Doc comment
#[derive(Args, Debug)]
#[group(required = false, multiple = false)]
struct DateArg {
    /// [DATE PROVIDER] Use the current UTC date to update calendar specifiers. Exclusive with other
    /// date providers.
    #[arg(short, long)]
    utc: bool,

    /// [DATE PROVIDER] Use the current local date to update calendar specifiers. Exclusive with
    /// other date providers.
    #[arg(short, long)]
    local: bool,

    /// [DATE PROVIDER] Use a date in format `YYYY-MM-DD` to update calendar specifiers. Exclusive
    /// with other date providers.
    #[arg(short, long, value_name = "YYYY-MM-DD")]
    date: Option<String>,
}

impl DateArg {
    fn to_date(&self) -> Result<Date, VersionBumpCliError> {
        // this struct acts like an enumeration since multiple is false. therefore, we don't need
        // to use match statement (in fact, that'd be more cumbersome)
        if self.utc {
            return Ok(Date::UtcNow);
        }
        if self.local {
            return Ok(Date::LocalNow);
        }
        if let Some(date) = &self.date {
            // this is a little inefficient, because internally, version_bump makes a NaiveDate
            // but i don't want to do our own string parsing here.
            return Ok(NaiveDate::parse_from_str(date, "%Y-%m-%d").map(Date::from)?);
        }
        unreachable!();
    }
}

// #[derive(Args, Debug)]
// #[group(required = true, multiple = true)]
// struct IncrementArgs {
//     /// The semantic level...
//     #[arg(short, long, value_enum)]
//     semantic: Option<SemanticLevelArg>,

//     /// The date...
//     // #[arg(short, long)]
//     // date: Option<String>,
//     #[command(flatten)]
//     date: Option<DateArg>,
//     // date: DateArg,
// }

// TODO: somehow figure out how to augment the help subcommand to add a section `format`, which
// describes the format specifiers.

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Validates that a version matches a format
    Validate {
        /// The format string to validate against
        format: String,

        /// The version string to validate
        #[arg(short, long)]
        version: String,
    },

    /// Increments a version formatted by `FORMAT` according to semantic and/or calendar rules.
    ///
    /// At least one of `--semantic` or the date providers must be specified. Otherwise, nothing
    /// would change.
    ///
    /// Note that, however, it is possible to use a date provider, but the resulting version may not
    /// increment anything at all (because its fields are identical to the current version's)
    ///
    /// # Date Providers
    ///
    /// Use one of `--utc`, `--local`, or `--date YYYY-MM-DD` to update date specifiers using the
    /// referenced date Or, omit them to not increment the date.
    Increment {
        /// The version string to increment
        version: String,

        /// The format string to validate against
        #[arg(short, long, required = true)]
        format: String,

        /// The semantic level to increment. Omit to not increment any semantic specifiers.
        #[arg(short, long, value_enum)]
        semantic_level: Option<SemanticLevelArg>,

        #[command(flatten)]
        date: Option<DateArg>,
    },
}

fn main() {
    let cli = Cli::parse();

    let result = do_work(cli);
}

fn do_work(cli: Cli) -> Result<(), VersionBumpCliError> {
    match cli.command {
        Some(Commands::Validate { format, version }) => {
            dbg!(format, version);
            Ok(())
        }
        Some(Commands::Increment {
            format,
            version,
            semantic_level,
            date,
        }) => {
            dbg!(&format, &version, &semantic_level, &date);
            let semantic_level = semantic_level
                .map(|semantic_level| semantic_level._to_semantic_level())
                .as_ref();
            let date = date.map(|date| date.to_date()).transpose()?;
            Ok(())
        }
        _ => {Ok(())}
    }
}

// #[cfg(test)]
// mod tests {
//     use super::*;

//     #[test]
//     fn test_incr_just_date() {
//         let res = Cli::try_parse_from([
//             "version-bump",
//             "increment",
//             "2020.1",
//             "--format",
//             "[YYYY].[PATCH]",
//         ]);
//         match res.unwrap().command {
//             Some(Commands::Increment {
//                 format,
//                 version,
//                 semantic_level,
//                 date,
//             }) => {
//                 assert_eq!(format, "[YYYY].[PATCH]");
//                 assert_eq!(version, "2020.1");
//                 assert_eq!(semantic_level, None);
//                 dbg!(date);
//             }
//             _ => panic!("wrong command"),
//         }
//     }

//     #[test]
//     fn test_incr_just_semantic() {
//         let res = Cli::try_parse_from([
//             "version-bump",
//             "increment",
//             "--format",
//             "[YYYY].[PATCH]",
//             "--version",
//             "2020.1",
//             "--semantic",
//             "minor",
//             "--utc",
//         ]);
//         match res.unwrap().command {
//             Some(Commands::Increment {
//                 format,
//                 version,
//                 semantic_level,
//                 date,
//             }) => {
//                 assert_eq!(format, "[YYYY].[PATCH]");
//                 assert_eq!(version, "2020.1");
//                 assert_eq!(semantic_level, Some(SemanticLevelArg::Minor));
//             }
//             _ => panic!("wrong command"),
//         }
//     }

//     // #[test]
//     // fn test_incr_date_and_semantic() {
//     //     let res = Cli::try_parse_from([
//     //         "version-bump",
//     //         "increment",
//     //         "--format",
//     //         "[YYYY].[PATCH]",
//     //         "--version",
//     //         "2020.1",
//     //         "--semantic",
//     //         "minor",
//     //         "--utc",
//     //     ]);

//     //     match res.unwrap().command {
//     //         Some(Commands::Increment {
//     //             format,
//     //             version,
//     //             increment_args: IncrementArgs { semantic, date },
//     //         }) => {
//     //             assert_eq!(format, "[YYYY].[PATCH]");
//     //             assert_eq!(version, "2020.1");
//     //             assert_eq!(semantic, Some(SemanticLevelArg::Minor));
//     //             match date {
//     //                 DateArg {
//     //                     utc: true,
//     //                     local: false,
//     //                 } => {}
//     //                 _ => panic!("wrong date"),
//     //             }
//     //         }
//     //         _ => panic!("wrong command"),
//     //     }
//     // }

//     #[test]
//     fn test_incr_neither() {
//         let res = Cli::try_parse_from([
//             "version-bump",
//             "increment",
//             "--format",
//             "[YYYY].[PATCH]",
//             "--version",
//             "2020.1",
//         ]);
//         assert!(res.is_err())
//     }
// }
