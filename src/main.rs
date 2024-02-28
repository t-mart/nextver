//! A CLI for the nextver library.
#![warn(missing_docs)]
#![warn(clippy::pedantic)]
#![warn(clippy::cargo)]

use clap::{arg, command, Parser, Subcommand, ValueEnum};
use core::str::FromStr;
use nextver::prelude::*;

#[derive(thiserror::Error, Debug, PartialEq)]
enum NextVerCliError {
    #[error(transparent)]
    LibraryCompositeError(#[from] CompositeError),

    #[error(transparent)]
    LibraryFormatError(#[from] FormatError),

    #[error(transparent)]
    LibraryNextError(#[from] NextError),

    #[error("format string was invalid for all schemes")]
    NoValidScheme,

    #[error("major semantic specifier level should not be used with calsem scheme")]
    MajorSpecifierWithCalsem,

    // This is a CLI usage concern that would normally be delegated to clap, but clap doesn't seem
    // to be able to model options that are conditionally-required based on runtime things (i.e.,
    // our "guess" logic), and, indeed, that seems like a lot to ask. Nor can we use clap APIs to
    // render this error with the same styling/context/etc that clap does internally. So, we just do
    // it ourselves. as a consequence, we now have coupling between this text and the clap option
    // names.
    #[error("this scheme requires a semantic specifier, use `-l`/`--sem-level`")]
    NoSemanticSpecifier,
}

#[derive(Clone, PartialEq, Eq, ValueEnum, Debug)]
enum SchemeArg {
    /// interpret as semantic scheme
    Sem,
    /// interpret as calendar scheme
    Cal,
    /// interpret as calendar-semantic scheme
    CalSem,
    /// try to guess the scheme by trying all schemes
    Guess,
}

// stdout text and exit code
type Output = (String, ExitCode);

fn validate(
    scheme: &SchemeArg,
    format_str: &str,
    version_str: &str,
) -> Result<Output, NextVerCliError> {
    let is_valid = match scheme {
        SchemeArg::Sem => Result::<_, NextVerCliError>::Ok(Sem::is_valid(format_str, version_str)?),

        SchemeArg::Cal => Ok(Cal::is_valid(format_str, version_str)?),

        SchemeArg::CalSem => Ok(CalSem::is_valid(format_str, version_str)?),

        SchemeArg::Guess => {
            let any = Sem::is_valid(format_str, version_str).unwrap_or(false)
                || Cal::is_valid(format_str, version_str).unwrap_or(false)
                || CalSem::is_valid(format_str, version_str).unwrap_or(false);
            if !any {
                return Err(NextVerCliError::NoValidScheme);
            }
            Ok(true)
        }
    }?;
    if is_valid {
        Ok((true.to_string(), ExitCode::Success))
    } else {
        Ok((false.to_string(), ExitCode::Failure))
    }
}

fn next(
    scheme: &SchemeArg,
    format_str: &str,
    version_str: &str,
    date: Date,
    spec: Option<&SemLevelArg>,
) -> Result<Output, NextVerCliError> {
    // functions to get the semantic specifier from the option, that error if we need it but
    // don't have it
    let sem_spec = || {
        spec.map(SemLevelArg::to_sem_level)
            .ok_or(NextVerCliError::NoSemanticSpecifier)
    };
    let cal_sem_spec = || {
        spec.map(SemLevelArg::to_calsem_specifier)
            .transpose()?
            .ok_or(NextVerCliError::NoSemanticSpecifier)
    };

    let next_version = match scheme {
        SchemeArg::Sem => Sem::next_version_string(format_str, version_str, sem_spec()?)?,

        SchemeArg::Cal => Cal::next_version_string(format_str, version_str, date)?,

        SchemeArg::CalSem => {
            CalSem::next_version_string(format_str, version_str, date, cal_sem_spec()?)?
        }

        SchemeArg::Guess => {
            if let Ok(sem_ver) = Sem::new_version(format_str, version_str) {
                sem_ver.next(sem_spec()?)?.to_string()
            } else if let Ok(cal_ver) = Cal::new_version(format_str, version_str) {
                cal_ver.next(date)?.to_string()
            } else if let Ok(cal_sem_ver) = CalSem::new_version(format_str, version_str) {
                cal_sem_ver.next(date, cal_sem_spec()?)?.to_string()
            } else {
                return Err(NextVerCliError::NoValidScheme);
            }
        }
    };
    Ok((next_version, ExitCode::Success))
}

#[derive(Clone, PartialEq, Eq, ValueEnum, Debug)]
enum SemLevelArg {
    /// increment the major semantic specifier
    Major,
    /// increment the minor semantic specifier
    Minor,
    /// increment the patch semantic specifier
    Patch,
}

impl SemLevelArg {
    fn to_sem_level(&self) -> SemLevel {
        use SemLevelArg::{Major, Minor, Patch};
        match self {
            Major => SemLevel::Major,
            Minor => SemLevel::Minor,
            Patch => SemLevel::Patch,
        }
    }

    fn to_calsem_specifier(&self) -> Result<CalSemLevel, NextVerCliError> {
        use SemLevelArg::{Major, Minor, Patch};
        match self {
            Major => Err(NextVerCliError::MajorSpecifierWithCalsem),
            Minor => Ok(CalSemLevel::Minor),
            Patch => Ok(CalSemLevel::Patch),
        }
    }
}

const UNPARSEABLE_DATE_ERROR: &str = "Could not parse provided date as `utc`, `local`, or `Y-M-D`";

fn parse_date(s: &str) -> Result<Date, &'static str> {
    match s {
        "utc" => Ok(Date::utc_now()),
        "local" => Ok(Date::local_now()),
        ymd => Ok(Date::from_str(ymd).map_err(|_| UNPARSEABLE_DATE_ERROR)?),
    }
}

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Subcommands>,
}

#[derive(Subcommand, Debug)]
#[command(arg_required_else_help(true))]
enum Subcommands {
    /// Validates that a version matches a format
    Valid {
        /// The version string to validate as matching a format
        version: String,

        /// A string defining the structure of the version string
        #[arg(short, long)]
        format: String,

        /// Interpret the format as the given scheme.
        #[arg(short, long, value_enum, default_value_t=SchemeArg::Guess)]
        scheme: SchemeArg,
    },

    /// Increments a version formatted by `FORMAT` according to semantic and/or calendar rules.
    Next {
        /// The current version string
        version: String,

        /// A string defining the structure of the version string
        #[arg(short, long)]
        format: String,

        /// The semantic specifier to increment. Cal formats ignore this option entirely, and calsem
        /// formats only accept `minor` or `patch`.
        #[arg(short = 'l', long, value_enum)]
        sem_level: Option<SemLevelArg>,

        /// The date to update calendar specifiers. Only has an effect if the format/version
        /// contain them. Can be either of the fixed strings `utc` or `local`, which use the current
        /// date in those timezones, or a date in the format `Y-M-D`, for an explicit date made
        /// from a year, month, and day.
        #[arg(short, long, value_name = "utc|local|Y-M-D", value_parser = parse_date, default_value = "utc")]
        date: Date,

        /// Interpret the format as the given scheme.
        #[arg(short, long, value_enum, default_value_t=SchemeArg::Guess)]
        scheme: SchemeArg,
    },
}

#[derive(Debug, PartialEq)]
#[repr(u8)]
enum ExitCode {
    Success = 0,
    Failure = 1,
    CliUsageError = 2,
}

impl From<&clap::error::Error> for ExitCode {
    fn from(e: &clap::error::Error) -> Self {
        match e.exit_code() {
            0 => ExitCode::Success,
            2 => ExitCode::CliUsageError,
            _ => panic!("clap should only return exit codes 0-3"),
        }
    }
}

impl From<ExitCode> for std::process::ExitCode {
    fn from(val: ExitCode) -> Self {
        std::process::ExitCode::from(val as u8)
    }
}

fn main() -> std::process::ExitCode {
    let cli = Cli::parse();

    match run(cli) {
        Ok((output, exit_code)) => {
            println!("{output}");
            exit_code
        }
        Err(e) => {
            eprintln!("{e}");

            // special handling for CLI usage error
            if e == NextVerCliError::NoSemanticSpecifier {
                ExitCode::CliUsageError
            } else {
                ExitCode::Failure
            }
        }
    }
    .into()
}

fn run(cli: Cli) -> Result<Output, NextVerCliError> {
    match cli.command {
        Some(Subcommands::Valid {
            format: format_str,
            version: version_str,
            scheme,
        }) => validate(&scheme, &format_str, &version_str),
        Some(Subcommands::Next {
            format,
            version,
            sem_level: level,
            date,
            scheme,
        }) => next(&scheme, &format, &version, date, level.as_ref()),
        None => unreachable!("clap should catch this no-subcommand case"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_calsem_date_diff() {
        let res = Cli::try_parse_from([
            "nextver",
            "next",
            "2024.07.0",
            "--format",
            "<YYYY>.<0W>.<PATCH>",
            "--date",
            "2024-02-26",
            "--sem-level",
            "patch",
        ])
        .unwrap();

        assert_eq!(Ok(("2024.08.0".to_string(), ExitCode::Success,)), run(res));
    }

    #[test]
    fn test_basic_calsem_date_same() {
        let res = Cli::try_parse_from([
            "nextver",
            "next",
            "2024.08.0",
            "--format",
            "<YYYY>.<0W>.<PATCH>",
            "--date",
            "2024-02-26",
            "--sem-level",
            "patch",
        ])
        .unwrap();

        assert_eq!(Ok(("2024.08.1".to_string(), ExitCode::Success,)), run(res));
    }
}
