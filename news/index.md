# Changelog

## readapra 0.2.3

- The ABN field in the MADIS, QADICP and ADIPOPS datasets will now
  always be numeric.

## readapra 0.2.2

CRAN release: 2026-03-22

- Removed dependency on polite, robotstxt, ratelimitr and memoise.
- The returned ADIPOPS, MADIS and QADICP tibbles are now sorted by date.

## readapra 0.2.1

CRAN release: 2025-02-22

- Fixed issue where inconsistent formatting would cause multiplication
  of data.
- Removed usage of the native pipe operator (\|\>).

## readapra 0.2.0

CRAN release: 2025-01-21

- Replaced `read_x()` and `read_x_local()` style functions with
  [`read_apra()`](https://javanderwal.github.io/readapra/reference/read_apra.md)
  and
  [`read_apra_local()`](https://javanderwal.github.io/readapra/reference/read_apra_local.md).
- Added the
  [`download_apra()`](https://javanderwal.github.io/readapra/reference/download_apra.md)
  function.
- Added the `apra_stat_pubs` dataset.

## readapra 0.1.1

- Removed dontrun status from examples in documentation.
- Added link to APRA’s website in the package description.

## readapra 0.1.0

- `read_x()` and `read_x_local()` style functions added for all of
  APRA’s Authorised Deposit-taking Institution statistical publications.
