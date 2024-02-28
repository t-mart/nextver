//! # nextver
//!
//! A library for parsing and incrementing arbitrarily-formatted versions.
//!
//! Instead of conforming to a specific versioning scheme, this library allows you to define your
//! own version format, parse version strings against it, and increment versions according to
//! semantic and/or calendar rules.
//!
//! Also comes with a [CLI](#cli).
//!
//! ## Examples
//!
//! *Below, the text in `<` and `>` brackets is a specifier. See what they mean [here](#table).*
//!
//! Quickly get a next version:
//!
//! ```
//! use nextver::prelude::*;
//!
//! let next = Sem::next_version_string(
//!   "<MAJOR>.<MINOR>.<PATCH>",  // format string
//!   "1.2.3",                    // current version string
//!   SemLevel::Minor            // the specifier to increment
//! )?;
//! assert_eq!(next, "1.3.0");
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```
//!
//! ```
//! use nextver::prelude::*;
//!
//! let date = Date::utc_now();       // assume today is 2024-02-23
//! # let date = Date::explicit(2024, 2, 23)?;
//!
//! let next = CalSem::next_version_string(
//!   "<YYYY>.<0M>-<PATCH>",          // format string
//!   "2023.12-42",                   // current version string
//!   date,                          // the date to update to
//!   CalSemLevel::Patch             // the specifier to increment if no calendar update
//! )?;
//! assert_eq!(next, "2024.02-0");
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```
//!
//! Or, break down the steps for reusability and comparisons:
//!
//! ```
//! use nextver::prelude::*;
//!
//! let date = Date::utc_now();  // assume today is 2024-02-23
//! # let date = Date::explicit(2024, 2, 23)?;
//!
//! let format = CalSem::new_format("<YYYY>.<MM>.<PATCH>")?;
//! let cur = format.new_version("2023.12.42")?;
//! let next = cur.next(date, CalSemLevel::Patch)?;
//! assert_eq!(next.to_string(), "2024.2.0");
//! // comparisons are also possible with Version objects
//! assert!(next > cur);
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```
//!
//! ## API Overview
//!
//! nextver is built around three main concepts: Schemes, formats, and versions.
//!
//! - **Schemes** dictate the kinds of values allowed in versions and the rules for incrementing
//!   them. (See the [table](#table) below.) They are modeled by the [`Scheme`] trait and
//!   implemented by the the following structs:
//!
//!   - [`Sem`]: A semantic versioning scheme. It is similar to [SemVer](https://semver.org/).
//!   - [`CalSem`]: A calendar-semantic versioning scheme. It is similar to
//!     [CalVer](https://calver.org/), but with an explicitly-required semantic part(s).
//!   - [`Cal`]: A calendar versioning scheme. Its like [`CalSem`] but without semantic specifiers.
//!     (This scheme is less useful in practice because there is no way to increment a version twice
//!     within the same period of its least significant specifier.)
//!
//! - **Formats** define the structure of a version string. They are modeled by the [`Format`]
//!   struct. They contains a sequence of *specifier* and *literal text* tokens. For example,
//!   `<MAJOR>.<MINOR>.<PATCH>` is a format string that can be turned into a [`Format`] object.
//!
//! - **Versions** are like Formats, but with actual values instead of specifiers. They represent a
//!   a point in a project's development. These are modeled by the [`Version`] struct. They can be
//!   incremented to new versions and compared amongst each other.
//!
//! ## Format String Syntax
//!
//! Use any sequence of *specifiers* (listed below) and *literal text* in a format string.
//! Specifiers are bracketed with `<` and `>`. Literal text is anything else (also see
//! [escaping](#escaping-brackets)).
//!
//! ### Table
//!
//! In the "Example" column below, we reference a major of `1`, minor of `2`, patch of `3` and a
//! date of `2001-02-03` (which is in the 4th week).
//!
//! | Specifier | Example | [`Sem`] | [`CalSem`] | [`Cal`] | [Parse Width](#parse-width) | [Min. Format Width](#minimum-format-width) | Description |
//! |---|---|---|---|---|---|---|---|
//! | `<MAJOR>` | `1` | ✅ | ❌ | ❌ | >=1 | - | The major part of a version |
//! | `<MINOR>` | `2` | ✅ | ✅ | ❌ | >=1 | - | The minor part of a version |
//! | `<PATCH>` | `3` | ✅ | ✅ | ❌ | >=1 | - | The patch part of a version |
//! | `<YYYY>` | `2001` | ❌ | ✅ | ✅ | >=1 | - | Full year, years less than 1 BCE are unsupported ([`0` refers to 1 BCE](https://en.wikipedia.org/wiki/Year_zero)) |
//! | `<YY>` | `1` | ❌ | ✅ | ✅ | >=1 | - | Year minus `2000`. For now, has same effect as `year % 100`, but the year 2100 will be `100`, and so on |
//! | `<0Y>` | `01` | ❌ | ✅ | ✅ | >=2 | 2 | Same as `YY` but zero-padded |
//! | `<MM>` | `1` | ❌ | ✅ | ✅ | 1 or 2 | - | Month of year (`1`–`12`) |
//! | `<0M>` | `01` | ❌ | ✅ | ✅ | 2 | 2 | Same as `MM` but zero-padded |
//! | `<WW>` | `4` | ❌ | ✅ | ✅ | 1 or 2 | - | Week of the year (`0`–`53`), week 1 starts with the first Sunday in that year. |
//! | `<0W>` | `04` | ❌ | ✅ | ✅ | 2 | 2 | Same as `WW` but zero-padded |
//! | `<DD>` | `3` | ❌ | ✅ | ✅ | 1 or 2 | - | Day of the month (`1`–`31`) |
//! | `<0D>` | `03` | ❌ | ✅ | ✅ | 2 | 2 | Same as `DD` but zero-padded |
//!
//! Specifiers are case-sensitive. For example, `<major>` or `<yYyY>` are not a valid specifiers.
//!
//! ### Parse Width
//!
//! The parse width comes into play when reading an existing version string. It is the number of
//! characters a specifier can consume. Some specifiers are flexible in this regard, while others
//! are not. For example, `<YYYY>` can consume 1 to an infinite number of characters, while `<0M>`
//! can only consume 2 characters.
//!
//! When the parse width is variable, the specifier is currently implemented to consume as **few**
//! characters as possible (non-greedy). Therefore, exercise caution when using adjacent specifiers
//! that have this quality. **For an unsurprising parse, use a literal separator between them.**
//!
//! ```
//! use nextver::prelude::*;
//!
//! // Let's say I have a version values of major=111, minor=222, patch=333
//! let (major, minor, patch) = (111, 222, 333);
//!
//! // And, my format string contains no separators between the specifiers
//! let format_str = "<MAJOR><MINOR><PATCH>";
//! let version_str = format!("{}{}{}", major, minor, patch);
//!
//! // There is no clear way to parse this because the starts and ends of specifiers are ambiguous.
//! // So, nextver is going to interpret: major=1, minor=1, patch=1222333, despite our intentions
//! let next_str = Sem::next_version_string(format_str, &version_str, SemLevel::Minor)?;
//!
//! // thus, the next version is: major=1, minor=2, patch=0
//! assert_eq!("120", next_str);
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```
//!
//! ### Minimum Format Width
//!
//! The format width is the minimum number of characters the specifier value will be formatted to
//! with zero-padding. Values with fewer characters than this number will be zero-padded to meet
//! this width, and values with more characters will be left as-is. (In the table above, a `-` value
//! means no zero padding applied.)
//!
//! Take, for example, the `<0Y>` specifier that has a format width of `2`. Given a year, it may be
//! formatted as follows:
//!
//! - `2001` → `01`, zero-padded to meet the format width
//! - `2010` → `10`, as-is formatting
//! - `2100` → `100`, as-is formatting
//!
//! ### Escaping Brackets
//!
//! If you want to use a literal `<` in your format, escape it as `<<`.
//!
//! `>` must **not** be escaped.
//!
//! ```
//! use nextver::prelude::*;
//!
//! // double `<<` in format string (and single closing `>` because it must not be escaped)
//! let format_str = "<MAJOR>-<<some literal text>";
//! // single `<` in version string
//! let version_str = "1-<some literal text>";
//! let format = Sem::new_format(format_str)?;
//! let version = format.new_version(version_str)?;
//!
//! assert_eq!(&version.to_string(), version_str);
//! # Ok::<(), Box<dyn std::error::Error>>(())
//! ```
//!
//! ## Prelude
//!
//! nextver provides a prelude module for convenience. It contains everything needed to interact
//! with the library.
//!
//! Use it with:
//!
//! ```
//! use nextver::prelude::*;
//! ```
//!
//! ## CLI
//!
//! This crate provides a CLI that can be used to do some API functions straight from the command
//! line.
//!
//! ### Installation
//!
//! ```sh
//! cargo install nextver
//! ```
//!
//! ### Usage
//!
//! ```sh
//! nextver --help
//! ```
#![warn(missing_docs)]
#![warn(clippy::pedantic)]
#![warn(clippy::cargo)]

mod format;
mod scheme;
mod specifier;
mod version;

pub use crate::format::{Format, FormatError};
pub use crate::scheme::{Cal, CalSem, CompositeError, Scheme, Sem};
pub use crate::specifier::{CalSemLevel, SemLevel};
pub use crate::version::{Date, DateError, NextError, Version, VersionError};

/// A convenience module appropriate for glob imports (`use nextver::prelude::*;`).
pub mod prelude {
    #[doc(no_inline)]
    pub use crate::{
        Cal, CalSem, CalSemLevel, CompositeError, Date, DateError, Format, FormatError, NextError,
        Scheme, Sem, SemLevel, Version, VersionError,
    };
}
