#![feature(lazy_cell)]

use chrono::{Datelike, NaiveDate};
use clap::{arg, command, value_parser, ArgAction, Args, Command, Parser, Subcommand, ValueEnum};

// APIs to support:
//
// "bump" subcommand:
// - Version<Sem> increment
// - Version<CalSem> update_or_increment
// - Version<CalSem> update
//
// "validate" subcommand:
// - Version<Sem/CalSem/Cal> validate

// use nextver::{Cal, CalSem, Date, Sem, SemanticSpecifier, Version, VersionBumpError, Format};
mod error;
mod format;
mod scheme;
mod specifier;
mod version;

use crate::error::VersionBumpError;
use crate::format::Format;
use crate::scheme::{Cal, CalSem, Scheme, Sem};
use crate::specifier::SemanticSpecifier;
use crate::version::{CalSemSpecifier, Date, Version};

#[derive(thiserror::Error, Debug)]
pub enum VersionBumpCliError {
    #[error("{0}")]
    LibraryError(#[from] VersionBumpError),

    #[error("{0}")]
    UnparseableDate(#[from] chrono::ParseError),
    // #[error("{0}")]
    // CliParseError(#[from] clap::Error),
}

#[derive(Clone, PartialEq, Eq, ValueEnum, Debug)]
enum SchemeArg {
    Sem,
    Cal,
    CalSem,
    Guess,
}

impl SchemeArg {
    fn validate(&self, format_str: &str, version_str: &str) -> Result<bool, VersionBumpCliError> {
        match self {
            SchemeArg::Sem => Self::validate_for_scheme(format_str, version_str, Sem),
            SchemeArg::Cal => Self::validate_for_scheme(format_str, version_str, Cal),
            SchemeArg::CalSem => Self::validate_for_scheme(format_str, version_str, CalSem),
            SchemeArg::Guess => {
                // this order is important: try the most permissive one last (CalSem)
                Self::validate_for_scheme(format_str, version_str, Sem)
                    .or_else(|_| Self::validate_for_scheme(format_str, version_str, Cal))
                    .or_else(|_| Self::validate_for_scheme(format_str, version_str, CalSem))
            }
        }
    }

    fn validate_for_scheme<S: Scheme>(
        format_str: &str,
        version_str: &str,
        scheme: S,
    ) -> Result<bool, VersionBumpCliError> {
        let format = Format::parse(format_str, scheme)?;
        Ok(Version::parse(version_str, &format).is_ok())
    }
}

#[derive(Clone, PartialEq, Eq, ValueEnum, Debug)]
enum SemanticArg {
    Major,
    Minor,
    Patch,
}

impl SemanticArg {
    fn _to_semantic_level(&self) -> SemanticSpecifier {
        match self {
            SemanticArg::Major => SemanticSpecifier::Major,
            SemanticArg::Minor => SemanticSpecifier::Minor,
            SemanticArg::Patch => SemanticSpecifier::Patch,
        }
    }
}

/// Doc comment
#[derive(Args, Debug)]
#[group(required = false, multiple = false)]
struct DateArg {
    /// [DATE PROVIDER] Use the current UTC date to update calendar specifiers. Exclusive with other
    /// date providers.
    #[arg(long)]
    utc: bool,

    /// [DATE PROVIDER] Use the current local date to update calendar specifiers. Exclusive with
    /// other date providers.
    #[arg(long)]
    local: bool,

    /// [DATE PROVIDER] Use a date in format `YYYY-MM-DD` to update calendar specifiers. Exclusive
    /// with other date providers.
    #[arg(long, value_name = "YYYY-MM-DD")]
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
            // this is a little inefficient, because internally, nextver makes a NaiveDate
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

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand, Debug)]
#[command(arg_required_else_help(true))]
enum Commands {
    /// Validates that a version matches a format
    Valid {
        /// The format string to validate against
        #[arg(short, long)]
        format: String,

        /// The version string to validate
        version: String,

        /// The semantic level to increment. Omit to not increment any semantic specifiers.
        #[arg(short, long, value_enum, default_value_t=SchemeArg::Guess)]
        scheme: SchemeArg,
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
    Bump {
        /// The version string to increment
        version: String,

        /// The format string to validate against
        #[arg(short, long, required = true)]
        format: String,

        /// The semantic level to increment. Omit to not increment any semantic specifiers.
        #[arg(short, long, value_enum)]
        level: Option<SemanticArg>,

        #[command(flatten)]
        date: Option<DateArg>,
    },
}

type Output = (String, i32);

fn main() {
    let cli = Cli::parse();

    println!("{:?}", &cli);

    match do_work(cli) {
        Ok((output, exit_code)) => {
            println!("{output}");
            std::process::exit(exit_code);
        }
        Err(e) => {
            eprintln!("{}", e);
            std::process::exit(1);
        }
    }
}

fn do_work(cli: Cli) -> Result<Output, VersionBumpCliError> {
    match cli.command {
        Some(Commands::Valid {
            format: format_str,
            version: version_str,
            scheme,
        }) => Ok(if scheme.validate(&format_str, &version_str)? {
            ("true".to_string(), 0)
        } else {
            ("false".to_string(), 1)
        }),
        Some(Commands::Bump {
            format,
            version,
            level,
            date,
        }) => {
            dbg!(&format, &version, &level, &date);
            let semantic_level = level
                .map(|semantic_level| semantic_level._to_semantic_level())
                .as_ref();
            // let date = date.map(|date| date.to_date()).transpose()?;
            Ok(("todo".to_string(), 0))
        }
        None => unreachable!("clap should catch this no-subcommand case"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_some_sheeeeyt() {
        let res = Cli::try_parse_from([
            "nextver",
            "valid",
            "2024.12",
            "--format",
            "[YYYY].[MINOR]f",
        ])
        .unwrap();

        dbg!(do_work(res).unwrap());
    }
}
