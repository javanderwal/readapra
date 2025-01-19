#' Read Quarterly ADI Performance Statistics
#'
#' @description
#' Import the Quarterly Authorised Deposit-taking Institution
#' Performance Statistics (QADIPS) from a local file.
#'
#' @param file_path path to the local .xlsx file.
#' @param cur_hist character vector determining whether to download the current
#' publication (`"current"`) or the historic publication (`"historic"`).
#'
#' @return A tibble containing the Quarterly ADI Performance Statistics data.
#'
#' @noRd
#'
read_qadips <- function(file_path, cur_hist, call = rlang::caller_env()) {
  tidyxl_data <- read_tidyxl_data(file_path, call = call)
  formatting_data <- read_tidyxl_formatting_data(file_path)
  qadips_data(tidyxl_data, formatting_data, call = call)
}

#' Combines the various QADIP data tibbles together alongside final formatting
#'
#' @param tidyxl_data cell data extracted from a .xlsx file using the tidyxl
#' package
#' @param formatting_data formatting data extracted from a .xlsx file using the
#' tidyxl package
#' @param call the caller environment
#'
#' @keywords internal
#' @noRd
#'
qadips_data <- function(
    tidyxl_data,
    formatting_data,
    call = rlang::caller_env()) {
  qadips_name <- "Quarterly Authorised Deposit-taking Institution Performance Statistics"
  qadips_frequency <- "Quarterly"

  qadips_tab_data <-
    attempt_format_horizontal_data(
      tidyxl_data = tidyxl_data,
      formatting_data = formatting_data,
      stat_pub_name = qadips_name,
      sheet_str_detect = "Tab",
      frequency = qadips_frequency,
      call = call
    )

  qadips_key_stats_data <-
    attempt_format_horizontal_data(
      tidyxl_data = tidyxl_data,
      formatting_data = formatting_data,
      stat_pub_name = qadips_name,
      sheet_str_detect = "Key",
      frequency = qadips_frequency,
      series_hierarchy_fn = qadip_key_stats_names,
      error_or_warning = "warning",
      message = "Could not extract data from the {.code Key stats} sheet. Data from the {.code Key stats} sheet has been omitted.",
      series_hierarchy_args = list(tidyxl_data = tidyxl_data),
      call = call
    )

  qadips_data <- dplyr::bind_rows(qadips_key_stats_data, qadips_tab_data)

  return(qadips_data)
}
