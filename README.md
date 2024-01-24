# nextver

A library for parsing and incrementing arbitrarily-formatted versions.

See the docs: <https://docs.rs/nextver>

## Quick start

TODO: fix this to work

```rust
use nextver::{Version, VersionBumpError, SemanticLevel};

let version = Version::from_parsed_format("[MAJOR].[MINOR].[PATCH]", "1.2.3").unwrap();
let incremented = version.increment(Some(&SemanticLevel::Minor), None).unwrap();
assert_eq!("1.3.0", incremented.to_string());
assert!(version < incremented);

// you can use arbitrary literals
let arbitrary = Version::from_parsed_format("v[MAJOR]-[PATCH]", "v1-2").unwrap();

// you can use dates
let dated = Version::from_parsed_format("[YYYY].[PATCH]", "2024.18").unwrap();

// errors tell you what went wrong
let invalid = Version::from_parsed_format("[MAJOR].[MINOR].[PATCH]", "1.foo.3");
assert!(matches!(invalid, Err(VersionBumpError::VersionFormatMismatch)));
```

Jump to the specifiers table [here](struct.Format.html#specifier-table).

## CLI

TODO: fill this in

## Deviations from CalVer

This library is almost conformant to [CalVer](https://calver.org), with a few
differences and clarifications, that you will hopefully find reasonable.

- Semantic and calendar specifiers must appear in strictly decreasing order of
  their period going left-to-right, such as `[YYYY]` before `[MM]` and `[MAJOR]`
  before `[MINOR]`. This also implies that multiple specifiers of the same
  period are forbidden.
  
  This is for ordering, so that incremented versions always compare greater than
  their originals.

  - `[YYYY].[DD].[MM]` ❌ day before month
  - `[MAJOR].[PATCH].[MINOR]` ❌ patch before minor
  - `[YYYY].[MM].[0M]` ❌ two month specifiers
  - `[MAJOR].[MINOR].[MINOR]` ❌ two minor specifiers

  This also has implications for week and month/day specifiers. Both week and
  month are year-relative, and therefore have different periods. And
  transitively, days are month-relative, so they are also forbidden from being
  in formats with weeks.

  - `[YYYY].[WW].[DD]` ❌ week and day
  - `[YYYY].[MM].[WW]` ❌ month and week

- There must be one and only one non-cyclic specifier in a format, and it must
  be the first one. The non-cyclic specifiers are `[MAJOR]` and any derived from
  the year (`[YYYY]`, `[YY]`, and `[0Y]`).
  
  This is also for ordering. Month/week/day and minor/patch values are cyclic,
  and comparisons may be unexpected *unless* those values are grounded by a
  first non-cyclic value.

  - `[YYYY].[MAJOR]` ❌ two non-cyclic specifiers

- For calendar specifiers, a succeeding specifier must relative to the previous
  one. This only manifests in the `[DD]` specifier, which must be preceded by
  `[MM]`.

  This is because a day is month-relative, and would otherwise cycle multiple
  times in a year.

  - `[YYYY].[DD]` ❌ day without month

- All calendar specifiers must precede semantic ones if both are present in a
  format.

  Calendar values move independently of semantic ones. Partitioning this way
  maintains logical clarity.

  - `[YYYY].[MINOR].[MM]` ❌ calendar, then semantic, then calendar
  - `[MAJOR].[DD].[PATCH]` ❌ semantic, then calendar, then semantic

- For the full year specifier (`[YYYY]`), years less than `1` (the [BCE
  era](https://en.wikipedia.org/wiki/Common_Era)) are forbidden, making them
  at least 1 digit, usually 4, possibly more, and definitely non-negative.
  
  If BCE years were allowed, the resulting value would be negative, and require
  a sign character that would affect parsing.

- For the short year (`[YY]`) and zero-padded year (`[0Y]`) specifiers, years
  less than `2000` are forbidden.
  
  This is for the same reasons as the previous. While CalVer specifies that
  years in this format are relative to the year `2000`, it does not clearly
  forbid lesser years, but we do.

And, these are not only suggestions — we enforce them in code by returning
errors in such cases.

Having said that, if any of these are disagreeable, open an issue to discuss. We
just feel these are the kind of versions we would want to work with.

TODO: incorporate this

Another way of saying all this is that these are the only allowed formats:

- Semantic
  - `[MAJOR]`
  - `[MAJOR]`, `[MINOR]`
  - `[MAJOR]`, `[MINOR]`, `[PATCH]`
- Calendar
  - `[<year>]`
  - `[<year>]`, `[<month>]`
  - `[<year>]`, `[<month>]`, `[<day>]`
  - `[<year>]`, `[<week>]`
- Calendar + Semantic
  - `[<year>]`, `[MINOR]`
  - `[<year>]`, `[MINOR]`, `[PATCH]`
  - `[<year>]`, `[PATCH]`
  - `[<year>]`, `[<month>]`, `[MINOR]`
  - `[<year>]`, `[<month>]`, `[MINOR]`, `[PATCH]`
  - `[<year>]`, `[<month>]`, `[PATCH]`
  - `[<year>]`, `[<month>]`, `[<day>]`, `[MINOR]`
  - `[<year>]`, `[<month>]`, `[<day>]`, `[MINOR]`, `[PATCH]`
  - `[<year>]`, `[<month>]`, `[<day>]`, `[PATCH]`
  - `[<year>]`, `[<week>]`, `[MINOR]`
  - `[<year>]`, `[<week>]`, `[MINOR]`, `[PATCH]`
  - `[<year>]`, `[<week>]`, `[PATCH]`

## Possible Improvements

- Support optional suffixed data, such as pre-release or build metadata.
