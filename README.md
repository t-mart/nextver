# nextver

A library for parsing and incrementing arbitrarily-formatted versions.

Instead of conforming to a specific versioning scheme, this library allows you
to define your own version format, parse version strings against it, and
increment versions according to semantic and/or calendar rules.

Also comes with a [CLI](#cli).

This an abridged version of the documentation. For the full documentation, see
[docs.rs](https://docs.rs/nextver).

## Example

*Below, the text in `<` and `>` brackets is a specifier. See what they mean
[here](#table).*

Quickly get a next version:

```rust
use nextver::prelude::*;

let next = Sem::next_version_string(
  "<MAJOR>.<MINOR>.<PATCH>",  // format string
  "1.2.3",                    // current version string
  SemLevel::Minor             // the specifier to increment
)?;
assert_eq!(next, "1.3.0");
```

## API Overview

nextver is built around three main concepts: Schemes, formats, and versions.

- **Schemes** dictate the kinds of values allowed in versions and the rules for
  incrementing them. (See the [table](#table) below.) They are modeled by the
  [`Scheme`] trait and implemented by the the following structs:

  - [`Sem`]: A semantic versioning scheme. It is similar to
    [SemVer](https://semver.org/).
  - [`CalSem`]: A calendar-semantic versioning scheme. It is similar to
    [CalVer](https://calver.org/), but with an explicitly-required semantic
    part(s).
  - [`Cal`]: A calendar versioning scheme. Its like [`CalSem`] but without
    semantic specifiers. (This scheme is less useful in practice because there
    is no way to increment a version twice within the same period of its least
    significant specifier.)

- **Formats** define the structure of a version string. They are modeled by the
  [`Format`] struct. They contains a sequence of *specifier* and *literal text*
  tokens. For example, `<MAJOR>.<MINOR>.<PATCH>` is a format string that can be
  turned into a [`Format`] object.

- **Versions** are like Formats, but with actual values instead of specifiers.
  They represent a a point in a project's development. These are modeled by the
  [`Version`] struct. They can be incremented to new versions and compared
  amongst each other.

## Format String Syntax

Use any sequence of *specifiers* (listed below) and *literal text* in a format
string. Specifiers are bracketed with `<` and `>`.

### Table

In the "Example" column below, we reference a major of `1`, minor of `2`, patch
of `3` and a date of `2001-02-03` (which is in the 4th week).

| Specifier | Example | `Sem` | `CalSem` | `Cal` | Parse Width | Format Width | Description |
|---|---|---|---|---|---|---|---|
| `<MAJOR>` | `1` | ✅ | ❌ | ❌ | >=1 | None | The major part of a version |
| `<MINOR>` | `2` | ✅ | ✅ | ❌ | >=1 | None | The minor part of a version |
| `<PATCH>` | `3` | ✅ | ✅ | ❌ | >=1 | None | The patch part of a version |
| `<YYYY>` | `2001` | ❌ | ✅ | ✅ | >=1 | None | Full year, years less than 1 BCE are unsupported ([`0` refers to 1 BCE](https://en.wikipedia.org/wiki/Year_zero)) |
| `<YY>` | `1` | ❌ | ✅ | ✅ | >=1 | None | Year minus `2000`. For now, has same effect as `year % 100`, but the year 2100 will be `100`, and so on |
| `<0Y>` | `01` | ❌ | ✅ | ✅ | >=2 | 2 | Same as `YY` but zero-padded |
| `<MM>` | `1` | ❌ | ✅ | ✅ | 1 or 2 | None | Month of year (`1`–`12`) |
| `<0M>` | `01` | ❌ | ✅ | ✅ | 2 | 2 | Same as `MM` but zero-padded |
| `<WW>` | `4` | ❌ | ✅ | ✅ | 1 or 2 | None | Week of the year (`0`–`53`), week 1 starts with the first Sunday in that year. |
| `<0W>` | `04` | ❌ | ✅ | ✅ | 2 | 2 | Same as `WW` but zero-padded |
| `<DD>` | `3` | ❌ | ✅ | ✅ | 1 or 2 | None | Day of the month (`1`–`31`) |
| `<0D>` | `03` | ❌ | ✅ | ✅ | 2 | 2 | Same as `DD` but zero-padded |

Specifiers are case-sensitive. For example, `<major>` is not a valid specifier.

## CLI

This crate provides a CLI that can be used to do some API functions straight
from the command line.

### Installation

```sh
cargo install nextver
```

### Usage

```sh
nextver --help
```

## DevOps

To:

- create and push a new git tag,
- create a new GitHub release with binaries attached, and
- publish a new version to <https://crates.io> (and update docs on
  <https://docs.rs>)

simply push a commit to the `master` branch with an updated version number in
`Cargo.toml`. The workflow file at `.github/workflows/release+build+publish.yml`
will take care of the rest. Make sure to pull afterwards.
