# Read APRA Publication Statistics Locally

Import from a local file a specific statistical publication produced by
APRA.

Please consult the
[apra_stat_pubs](https://javanderwal.github.io/readapra/reference/apra_stat_pubs.md)
dataset to see which of APRA's statistical publications are available in
`readapra`.

## Usage

``` r
read_apra_local(file_path, stat_pub, cur_hist = "current")
```

## Arguments

- file_path:

  path to the local file from which the statistical publication data
  will be imported.

- stat_pub:

  character vector detailing a statistical publication to be imported.
  Must match a valid value in the `apra_stat_pubs_acronym` variable of
  the
  [apra_stat_pubs](https://javanderwal.github.io/readapra/reference/apra_stat_pubs.md)
  dataset.

- cur_hist:

  character vector detailing whether to import a current or historic
  statistical publication. Must match a valid value in the `cur_hist`
  variable of the
  [apra_stat_pubs](https://javanderwal.github.io/readapra/reference/apra_stat_pubs.md)
  dataset.

## Value

A tibble containing the statistical publication data.

## Examples

``` r
# \donttest{
# Download the current MADIS data and get the file path:
current_madis_file_path <-
  download_apra(stat_pub = "madis", cur_hist = "current")

# Import the current MADIS data:
current_madis_data <-
  read_apra_local(
    stat_pub = "madis",
    cur_hist = "current",
    file_path = current_madis_file_path
  )

# Examine the current MADIS data:
print(current_madis_data)
#> # A tibble: 287,550 × 9
#>    statistics_publication_name               date           abn institution_name
#>    <chr>                                     <date>       <dbl> <chr>           
#>  1 Monthly Authorised Deposit-taking Instit… 2019-03-31 7.07e10 ABN AMRO Bank N…
#>  2 Monthly Authorised Deposit-taking Instit… 2019-03-31 7.07e10 ABN AMRO Bank N…
#>  3 Monthly Authorised Deposit-taking Instit… 2019-03-31 7.07e10 ABN AMRO Bank N…
#>  4 Monthly Authorised Deposit-taking Instit… 2019-03-31 7.07e10 ABN AMRO Bank N…
#>  5 Monthly Authorised Deposit-taking Instit… 2019-03-31 7.07e10 ABN AMRO Bank N…
#>  6 Monthly Authorised Deposit-taking Instit… 2019-03-31 7.07e10 ABN AMRO Bank N…
#>  7 Monthly Authorised Deposit-taking Instit… 2019-03-31 7.07e10 ABN AMRO Bank N…
#>  8 Monthly Authorised Deposit-taking Instit… 2019-03-31 7.07e10 ABN AMRO Bank N…
#>  9 Monthly Authorised Deposit-taking Instit… 2019-03-31 7.07e10 ABN AMRO Bank N…
#> 10 Monthly Authorised Deposit-taking Instit… 2019-03-31 7.07e10 ABN AMRO Bank N…
#> # ℹ 287,540 more rows
#> # ℹ 5 more variables: balance_sheet_category <chr>, series <chr>,
#> #   frequency <chr>, unit <chr>, value <dbl>
# }
```
