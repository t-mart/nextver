#![doc = include_str!("../README.md")]
#![warn(missing_docs)]

mod error;
mod format;
mod scheme;
mod specifier;
mod version;

pub use crate::error::{CompositeError, DateError, FormatError, NextError, VersionError};
pub use crate::format::Format;
pub use crate::scheme::{Cal, CalSem, Scheme, Sem};
pub use crate::specifier::{CalSemLevel, SemLevel};
pub use crate::version::{Date, Version};

/// A convenience module appropriate for glob imports (`use nextver::prelude::*;`).
pub mod prelude {
    #[doc(no_inline)]
    pub use crate::{
        Cal, CalSem, CalSemLevel, CompositeError, Date, DateError, Format, FormatError, NextError,
        Scheme, Sem, SemLevel, Version, VersionError,
    };
}
