
<!-- README.md is generated from README.Rmd. Please edit that file -->

# readapra <img src="man/figures/readapra_hex_sticker.png" align="right" height="139"/>

<!-- badges: start -->

[![Codecov test
coverage](https://codecov.io/gh/javanderwal/readapra/graph/badge.svg)](https://app.codecov.io/gh/javanderwal/readapra)
[![R-CMD-check](https://github.com/javanderwal/readapra/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/javanderwal/readapra/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The R package `readapra` provides a series of function to easily scrape
data from the [Australian Prudential Regulation Authority’s
(APRA)](https://www.apra.gov.au/) statistical publications and return
them as a [tibble](https://tibble.tidyverse.org/) object.

## Installation

You can install the development version from GitHub using the
[*remotes*](https://remotes.r-lib.org/) package:

``` r
remotes::install_github("javanderwal/readapra")
```

## Features

Currently the `readapra` package only contains functions related to the
scraping of Authorised Deposit- taking Institution (ADI) statistical
publications produced by APRA. The intention is to have future versions
of `readapra` also be able to scrape Insurance and Superannuation
statistical publications produced by APRA.

#### ADI statistical publications

The following ADI statistical publications can be scraped using
`readapra`:

- [Quarterly Authorised Deposit-taking Institution Performance
  Statistics
  (QADIPS)](https://www.apra.gov.au/quarterly-authorised-deposit-taking-institution-statistics)

- [Quarterly Authorised Deposit-taking Institution Centralised
  Publication
  (QADICP)](https://www.apra.gov.au/quarterly-authorised-deposit-taking-institution-statistics)

- [Quarterly Authorised Deposit-taking Institution Property Exposures
  Statistics
  (QADIPEXS)](https://www.apra.gov.au/quarterly-authorised-deposit-taking-institution-statistics)
  (Both the current and historic series)

- [Monthly Authorised Deposit- taking Institution Statistics
  (MADIS)](https://www.apra.gov.au/monthly-authorised-deposit-taking-institution-statistics)
  (Both the current and historic series)

- [Authorised Deposit-taking Institution Points of Presence Statistics
  (ADIPOPS)](https://www.apra.gov.au/authorised-deposit-taking-institutions-points-of-presence-statistics)

## Example

Using the `readapra` package it is extremely easy to download and import
APRA’s statistical publication data into R. This allows for the easy
visualisation of the data using packages such as ggplot2.

``` r
library(readapra)
library(dplyr)
library(ggplot2)
```

First we extract the Monthly Authorised Deposit-taking Institution
Statistics (MADIS) data using the `read_qadips` function:

``` r
madis_data <- read_madis("current")
```

We then clean up the data a bit further using some functions from
*dplyr*:

``` r
major_bank_assets <-
  madis_data %>%
  filter(
    abn %in% c(48123123124, 33007457141, 12004044937, 11005357522),
    series == "Total residents assets"
  )
```

And then finally we can plot the data, showing total resident assets for
the individual major Australian banks.

``` r
ggplot(
  data = major_bank_assets, 
  mapping = aes(date, value, colour = institution_name)
  ) + 
  geom_line() + 
  theme_classic() +
  theme(legend.position = "bottom") +
  guides(colour = guide_legend(ncol = 1))
```

<img src="man/figures/README-plot_madis-1.png" width="70%" style="display: block; margin: auto;" />

## Managing Network Connections

Corporate networks, especially those using Virtual Private Networks
(VPNs), may restrict the ability to download files within an R session.
A possible fix for this is to utilise the the `"wininet"` method for
downloading files. Users can specify the `"wininet"` method (or any
other download method) for `readapra` to use by setting the
`"R_READAPRA_DL_METHOD"` environment variable.

To set the `"R_READAPRA_DL_METHOD"` environment variable for your
current session, use the following code:

``` r
Sys.setenv("R_READAPRA_DL_METHOD" = "wininet")
```

You can add `"R_READAPRA_DL_METHOD" = "wininet"` to your `.Renviron`
file to ensure this download setting persists across R sessions. You can
conveniently access and edit your `.Renviron` file with the usethis
package:

``` r
usethis::edit_r_environ()
```

## Disclaimer

This package is not affiliated with or endorsed by the Australian
Prudential Regulation Authority (APRA). All data is provided subject to
any conditions and restrictions set out on the APRA website.
