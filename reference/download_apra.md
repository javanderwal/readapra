# Download a Statistical Publication File from APRA's Website

Download a statistical publication file from APRA's website. By default
files are saved to a temporary directory.

## Usage

``` r
download_apra(
  stat_pub,
  cur_hist = "current",
  path = tempdir(),
  overwrite = TRUE,
  quiet = FALSE,
  ...
)
```

## Arguments

- stat_pub:

  character vector detailing a statistical publication to be downloaded.
  Must match a valid value in the `apra_stat_pubs_acronym` variable of
  the
  [apra_stat_pubs](https://javanderwal.github.io/readapra/reference/apra_stat_pubs.md)
  dataset.

- cur_hist:

  character vector detailing whether to download a current or historic
  statistical publication. Must match a valid value in the `cur_hist`
  variable of the
  [apra_stat_pubs](https://javanderwal.github.io/readapra/reference/apra_stat_pubs.md)
  dataset.

- path:

  path to where the downloaded file should be saved. Uses
  [`base::tempdir()`](https://rdrr.io/r/base/tempfile.html) by default.

- overwrite:

  whether to overwrite a previously downloaded statistical publication
  file when re-running this function.

- quiet:

  whether to suppress the download progress bar.

- ...:

  additional arguments to be passed to
  [`utils::download.file()`](https://rdrr.io/r/utils/download.file.html).

## Value

A character vector detailing the file path to the downloaded file.

## Examples

``` r
# \donttest{
# Download a statistical publication file:
download_path <-
  download_apra(stat_pub = "qadips", cur_hist = "current")

# View the file path of the statistical publication file:
print(download_path)
#> [1] "/tmp/Rtmpjw6XTX/Quarterly%20authorised%20deposit-taking%20institution%20performance-September%202004%20to%20December%202025.xlsx"
# }
```
