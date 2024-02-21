//! # nextver
//!
//! A library for parsing and incrementing arbitrarily-formatted versions.
//!
//! Instead of conforming to a specific versioning scheme, this library allows you to define your
//! own version format, parse version strings against it, and increment versions according to
//! semantic and/or calendar rules.
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
//! let next = Sem::next(
//!   "<MAJOR>.<MINOR>.<PATCH>",  // format string
//!   "1.2.3",                    // current version string
//!   &SemanticSpecifier::Minor   // the specifier to increment
//! ).unwrap();
//! assert_eq!(next, "1.3.0");
//!
//! let next = CalSem::next(
//!   "<YYYY>.<0M>-<PATCH>",          // format string
//!   "2023.12-42",                   // current version string
//!   &"2024-01-02".parse().unwrap(), // the date to update to, or simply `Date::UtcNow`/`Date::LocalNow`
//!   &CalSemSpecifier::Patch         // the specifier to increment, if no calendar update would occur
//! ).unwrap();
//! assert_eq!(next, "2024.01-0");
//! ```
//!
//! Or, break down the steps for reusability:
//!
//! ```
//! use nextver::prelude::*;
//!
//! let format = CalSem::new_format("<YYYY>.<MM>.<PATCH>").unwrap();
//! let version = format.parse_version("2023.12.42").unwrap();
//! let next = version.next(&"2024-01-02".parse().unwrap(), &CalSemSpecifier::Patch).unwrap();
//! assert!(next > version);
//! ```
//!
//! ## Important Terms
//!
//! - **Version**: A string that represents a specific point in a project's development, comprised
//!   of *values* and *literal text*. It's modeled by the [`Version`] struct. Versions can be
//!   incremented to new ones and compared amongst each other.
//! - **Format**: A string that defines the structure of a version string. It contains a sequence of
//!   *specifiers* and *literal text*. It's modeled by the [`Format`] struct.
//! - **Specifier**: A token in a format that dictates how to interpret and format its respective
//!   part in a version. These are `<bracketed>` in a format string.
//!
//! ## Schemes
//!
//! nextver defines three versioning schemes. Each one has its own set of specifiers and rules. See
//! their documentation for more details.
//!
//!
//! - [`Sem`]: Only semantic specifiers. For flexibility, it is a superset of
//!   [SemVer](https://semver.org/) in that it allows the omission of `<MINOR>` and `<PATCH>`
//!   specifiers.
//! - [`CalSem`]: Calendar specifiers followed by semantic ones. It's similar to
//!   [CalVer](https://calver.org/) but with some clarifications. See its documentation for more
//!   details.
//! - [`Cal`]: Only calendar specifiers. This one is probably less useful because there is no way to
//!   increment it twice in the same period of its least significant specifier. For example, a
//!   version with format `<YYYY>.<MM>.<DD>` can only be incremented/updated once per day.
//!
//! ## Specifiers
//!
//! ### Table
//!
//! In the "Example" column below, we reference a major of `1`, minor of `2`, patch of `3` and a
//! date of `2001-02-03` (in the 4th week).
//!
//! | Specifier | Example | Sem | CalSem | Cal | [Parse Width](#parse-width) | [Format Width](#format-width) | Description | Reference |
//! |---|---|---|---|---|---|---|---|---|---|
//! | `<MAJOR>` | `1` | ✅ | ❌ | ❌ | >=1 | None | The major part of a version | [1] |
//! | `<MINOR>` | `2` | ✅ | ✅ | ❌ | >=1 | None | The minor part of a version | [1] |
//! | `<PATCH>` | `3` | ✅ | ✅ | ❌ | >=1 | None | The patch part of a version | [1] |
//! | `<YYYY>` | `2001` | ❌ | ✅ | ✅ | >=1 | None | Full year, years less than 1 BCE are unsupported ([`0` refers to `1 BCE`](https://en.wikipedia.org/wiki/Year_zero)) | [2] |
//! | `<YY>` | `1` | ❌ | ✅ | ✅ | >=1 | None | Year minus `2000`. For now, has same effect as `year % 100`, but the year 2100 will be `100`, and so on | [2] |
//! | `<0Y>` | `01` | ❌ | ✅ | ✅ | >=2 | 2 | Same as `YY` but zero-padded | [2] |
//! | `<MM>` | `1` | ❌ | ✅ | ✅ | 1 or 2 | None | Month of year (`1`–`12`) | [2] |
//! | `<0M>` | `01` | ❌ | ✅ | ✅ | 2 | 2 | Same as `MM` but zero-padded | [2] |
//! | `<WW>` | `4` | ❌ | ✅ | ✅ | 1 or 2 | None | Week of the year (`0`–`53`), week 1 starts with the first Sunday in that year. | [2] |
//! | `<0W>` | `04` | ❌ | ✅ | ✅ | 2 | 2 | Same as `WW` but zero-padded | [2] |
//! | `<DD>` | `3` | ❌ | ✅ | ✅ | 1 or 2 | None | Day of the month (`1`–`31`) | [2] |
//! | `<0D>` | `03` | ❌ | ✅ | ✅ | 2 | 2 | Same as `DD` but zero-padded | [2] |
//!
//! [1]: https://semver.org/
//! [2]: https://calver.org/
//!
//! ### Parse Width
//!
//! The parse width of a specifier is the number of characters it can consume from a version string.
//! Some specifiers are flexible in this regard, while others are not. For example, `<YYYY>` can
//! consume 1 to an infinite number of characters, while `<0M>` can only consume 2 characters.
//!
//! When the parse width is variable, the specifier is currently implemented to consume as few
//! characters as possible (non-greedy). Therefore, exercise caution when using adjacent specifiers
//! that have this quality. For an unambiguous parse, use a literal separator between them:
//!
//! ```
//! use nextver::prelude::*;
//!
//! // always unambiguous because of literal separator
//! let format = Cal::new_format("<YYYY>.<MM>");
//!
//! // ambiguous: MM could consume 1 or 2 characters
//! let format = Cal::new_format("<YYYY><MM>");
//! ```
//! 
//! ### Format Width
//!
//! The format width is the minimum number of characters the specifier value will be formatted to.
//! Values with fewer characters will be zero-padded to meet this width, and values with more
//! characters will be left as-is.
//!
//! ### Escaping Brackets
//!
//! If you want to use a literal `<` in your format, escape it as `<<`.
//!
//! `>` must not be escaped.
//!
//! ```
//! use nextver::prelude::*;
//!
//! let format = Sem::new_format("<MAJOR>-<<literal-text>");
//! // matches a version like "1-<literal-text>"
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
#![warn(missing_docs)]

mod error;
mod format;
mod scheme;
mod specifier;
mod version;

pub use crate::error::{CompositeError, DateError, FormatError, VersionError};
pub use crate::format::Format;
pub use crate::scheme::{Cal, CalSem, Scheme, Sem};
pub use crate::specifier::{CalSemLevel, SemLevel};
pub use crate::version::{Date, Version};

/// A convenience module appropriate for glob imports (`use nextver::prelude::*;`).
pub mod prelude {
    #[doc(no_inline)]
    pub use crate::Cal;
    #[doc(no_inline)]
    pub use crate::CalSem;
    #[doc(no_inline)]
    pub use crate::CalSemLevel;
    #[doc(no_inline)]
    pub use crate::CompositeError;
    #[doc(no_inline)]
    pub use crate::Date;
    #[doc(no_inline)]
    pub use crate::DateError;
    #[doc(no_inline)]
    pub use crate::Format;
    #[doc(no_inline)]
    pub use crate::FormatError;
    #[doc(no_inline)]
    pub use crate::Scheme;
    #[doc(no_inline)]
    pub use crate::Sem;
    #[doc(no_inline)]
    pub use crate::SemLevel;
    #[doc(no_inline)]
    pub use crate::Version;
    #[doc(no_inline)]
    pub use crate::VersionError;
}
