# nextver

A library for parsing and incrementing arbitrarily-formatted versions.

Instead of conforming to a specific versioning scheme, this library allows you to define your
own version format, parse version strings against it, and increment versions according to
semantic and/or calendar rules.

**See the docs here: <https://docs.rs/nextver>

## Examples

Below, the text in `[brackets]` is a specifier. See what they mean [here](https://docs.rs/nextver#table).

```rust
// quickly get a next version
use nextver::prelude::*;

let next = Sem::next(
  "[MAJOR].[MINOR].[PATCH]",  // format string
  "1.2.3",                    // current version string
  &SemanticSpecifier::Minor   // the specifier to increment
).unwrap();
assert_eq!(next.to_string(), "1.3.0");
 
let next = CalSem::next(
  "[YYYY].[MM]-[PATCH]",       // format string
  "2023.12-42",                // current version string
  &Date::Explicit(2024, 1, 2), // the date to update to, or simply `Date::UtcNow`/`Date::LocalNow`
  &CalSemSpecifier::Patch      // the specifier to increment, if no calendar update would occur
).unwrap();
assert_eq!(next.to_string(), "2024.01-0");
```

```rust
// or, break down the steps for reusability
use nextver::prelude::*;
 
let format = CalSem::new_format("[YYYY].[MM].[PATCH]");
let version = Version::parse("2023.12.42", &format).unwrap();
let next = version.next(&Date::Explicit(2024, 1, 2), &CalSemSpecifier::Patch).unwrap();
assert!(next > version);
```

Jump to the specifiers table [here](struct.Format.html#specifier-table).

## CLI

nextver also comes with a CLI tool, `nextver`, which can be used to quickly increment versions.

Install it with cargo:

```sh
cargo install nextver
```

Then, run it:

```sh
nextver next "1.2.3" --format "[MAJOR].[MINOR].[PATCH]" --specifier minor
# 1.3.0
```

Then, run it with `nextver --help` to see the available options.

## Possible Improvements

- Support optional suffixed data, such as pre-release or build metadata.
