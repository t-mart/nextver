#[cfg(doc)]
use chrono::NaiveDate;
#[cfg(doc)]
use std::str::FromStr;

// In error messages, strive (insist?) on using the “expect as precondition” style of error because
// it is more helpful for the user. In other words, include the word "SHOULD". See
// <https://doc.rust-lang.org/std/error/index.html#common-message-styles>.
//
// Additionally, "error messages are typically concise lowercase sentences without trailing
// punctuation": <https://doc.rust-lang.org/std/error/trait.Error.html#>
