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
//! *Below, the text in `[brackets]` is a specifier. See what they mean [here](#table).*
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
//! - **Specifier**: A pattern in a format that dictates how to interpret and format its respective
//!   part in a version. These are `[bracketed]` in a format string.
//!
//! ## Schemes
//!
//! nextver defines three versioning schemes:
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
//!
//! ## Specifiers
//!
//! ### Table
//!
//! In the "Example" column below, we reference a major of `1`, minor of `2`, patch of `3` and a
//! date of `2001-02-03` (in the 4th week).
//!
//! | Specifier | Example | Type | Sem | CalSem | Cal | [Greedy?](#greedy-specifiers) | Description | Reference |
//! |---|---|---|---|---|---|---|---|---|
//! | `<MAJOR>` | `1` | Semantic - Major | ✅ | ❌ | ❌ | **Yes** | The major part of a version | [1] |
//! | `<MINOR>` | `2` | Semantic - Minor | ✅ | ✅ | ❌ | **Yes** | The minor part of a version | [1] |
//! | `<PATCH>` | `3` | Semantic - Patch | ✅ | ✅ | ❌ | **Yes** | The patch part of a version | [1] |
//! | `<YYYY>` | `2001` | Calendar - Year | ❌ | ✅ | ✅ | **Yes** | Full year. This will be at least 1 digit (e.g. year `1` or year `10000`). We do not support BCE years for this specifier. | [2] |
//! | `<YY>` | `1` | Calendar - Year | ❌ | ✅ | ✅ | **Yes** | The last two digits of the year (for years <= `2099`). In general, this is the same as `year - 2000` so that, for example, years `2001`, `2101`, ... `3001` are disambiguated. We do not support years less than `2000` for this specifier. | [2] |
//! | `<0Y>` | `01` | Calendar - Year | ❌ | ✅ | ✅ | **Yes** | Same as `YY` but zero-padded to at least 2 characters. | [2] |
//! | `<MM>` | `1` | Calendar - Month | ❌ | ✅ | ✅ | **Yes** | Month (`1`–`12`). | [2] |
//! | `<0M>` | `01` | Calendar - Month | ❌ | ✅ | ✅ | No | Same as `MM` but zero-padded to 2 characters. | [2] |
//! | `<WW>` | `4` | Calendar - Week | ❌ | ✅ | ✅ | **Yes** | Week of the year (`0`–`53`). Week 1 starts with the first Sunday in that year. | [2] |
//! | `<0W>` | `04` | Calendar - Week | ❌ | ✅ | ✅ | No | Same as `WW` but zero-padded to 2 characters. | [2] |
//! | `<DD>` | `3` | Calendar - Day | ❌ | ✅ | ✅ | **Yes** | Day of the month (`1`–`31`). | [2] |
//! | `<0D>` | `03` | Calendar - Day | ❌ | ✅ | ✅ | No | Same as `DD` but zero-padded to 2 characters. | [2] |
//!
//! [1]: https://semver.org/
//! [2]: https://calver.org/
//!
//! ### Greedy Specifiers
//!
//! Greedy specifiers greedily match digits (sometimes with an upper bound and sometimes not). A
//! caveat with their use is that if you specify two consecutive greedy specifiers (without a
//! literal separator), the former specifier can consume some of the digits of the latter. *This is
//! probably not what you want.* For this reason, it's recommended to use a literal separator or a
//! non-greedy latter specifier in such cases.
//!
//! ```
//! use nextver::prelude::*;
//!
//! // always consistent because of literal separator
//! let format = Cal::new_format("<YYYY>.<MM>");
//!
//! // inconsistent: YYYY will consume first digit of a two-digit MM
//! let format = Cal::new_format("<YYYY><MM>");
//!
//! // always consistent: zero-padded `0M` is non-greedy, always two digits
//! let format = Cal::new_format("<YYYY><0M>");
//! ```
//!
//! ### Escaping Brackets
//!
//! If you want to use a literal `[` in your format, you must escape it with a preceeding backslash.
//!
//! `]` does not need to be escaped.
//!
//! ```
//! use nextver::prelude::*;
//!
//! // double backslash in regular strings
//! let format = Sem::new_format("<MAJOR>-\\[literal-text]");
//! // or use raw strings to just use one backslash
//! let format = Sem::new_format(r"<MAJOR>-\[literal-text]");
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
