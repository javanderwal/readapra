#' Read Quarterly ADI Centralised Publication
#'
#' @description
#' Download and import Read in the Quarterly Authorised Deposit-taking
#' Institution Centralised Publication (QADICP) from APRA's website.
#'
#' @return A tibble containing the Quarterly ADI Centralised Publication data.
#' @export
#'
#' @examples
#' read_qadicp()
read_qadicp <- function() {
  temp_file_path <-
    download_apra(
      publication = "qadicp",
      cur_hist = "current"
    )
  tidyxl_data <- read_tidyxl_data(temp_file_path)
  formatting_data <- read_tidyxl_formatting_data(temp_file_path)
  qadicp_data(tidyxl_data, formatting_data)
}

#' Read Quarterly ADI Centralised Publication locally
#'
#' @description
#' Import the Quarterly Authorised Deposit-taking Institution Centralised
#' Publication (QADICP) from a local file.
#'
#' @param file_path The file path to the local QADICP .xlsx file.
#'
#' @return A tibble containing the Quarterly ADI Centralised Publication data.
#' @export
#'
#' @examples
#' \dontrun{
#' read_qadicp_local(file_path = ~path/to/xlsx/file)
#' }
read_qadicp_local <- function(file_path) {
  tidyxl_data <- read_tidyxl_data(file_path)
  formatting_data <- read_tidyxl_formatting_data(file_path)
  qadicp_data(tidyxl_data, formatting_data)
}

#' Combines the various QADICP data tibbles together alongside final formatting
#'
#' @param tidyxl_data The data sourced using the tidyxl package
#' @param formatting_data The formatting data sourced using the tidyxl package
#'
#' @keywords internal
#' @noRd
#'
qadicp_data <- function(tidyxl_data, formatting_data) {
  table_4_data <-
    tidyxl_data |>
    dplyr::filter(stringr::str_detect(tolower(sheet), "table.*4"))

  cleaned_qadicp_data <-
    attempt_cleaned_vertical_data(table_4_data, formatting_data) |>
    add_qadicp_category() |>
    dplyr::mutate(
      statistics_publication_name = "Quarterly Authorised Deposit-taking Institution Centralised Publication",
      .before = date
    )

  return(cleaned_qadicp_data)
}

#' Adds a balance sheet category to the MADIS data. Certain MADIS series have
#' the same name, adding this column helps distinguish them.
#'
#' @param qadicp_data The cleaned qadicp data
#'
#' @keywords internal
#' @noRd
#'
add_qadicp_category <- function(qadicp_data) {
  qadicp_data_w_categories <-
      dplyr::mutate(
        .data = qadicp_data,
        series_category = dplyr::case_when(
          series %in% c(
            "Total Common Equity Tier 1 capital", "Total Tier 1 capital",
            "Total capital base", "Total risk-weighted assets",
            "Common Equity Tier 1 capital ratio", "Tier 1 capital ratio",
            "Total capital ratio"
            ) ~ "Regulatory capital",
          series %in% c(
            "Credit risk ", "Operational risk", "Market risk",
            "Other risk charges"
            ) ~ " Risk-weighted assets (RWA)",
          stringr::str_detect(series, "Of which:") ~ " Risk-weighted assets (RWA)",
          series %in% c(
            "Mean Liquidity coverage ratio (LCR)",
            "Net stable funding ratio (NSFR)",
            "Average Minimum liquidity holdings ratio (MLH)"
            ) ~ "Liquidity ratios",

          .default = "Unknown category"
        ),
        .before = series
      )

  return(qadicp_data_w_categories)
}
