#' Read Quarterly ADI Performance Statistics
#'
#' @description
#' Download and import the Quarterly Authorised Deposit-taking Institution
#' Performance Statistics (QADIPS) from APRA's website.
#'
#' @return A tibble containing the Quarterly ADI Performance Statistics data.
#' @export
#'
#' @examples
#' read_qadips()
read_qadips <- function(
    path = tempdir(),
    overwrite = TRUE,
    quiet = FALSE,
    ...) {
  temp_file_path <- download_apra(
    publication = "qadips",
    cur_hist = "current",
    path = path,
    quiet = quiet,
    overwrite = overwrite,
    ...
  )
  read_qadips_local(temp_file_path)
}

#' Read Quarterly ADI Performance Statistics locally
#'
#' @description
#' Import the Quarterly Authorised Deposit-taking Institution
#' Performance Statistics (QADIPS) from a local file.
#'
#' @param file_path The file path to the local QADIPS .xlsx file.
#'
#' @return A tibble containing the Quarterly ADI Performance Statistics data.
#' @export
#'
#' @examples
#' \dontrun{
#' read_qadips_local(file_path = ~path/to/xlsx/file)
#' }
read_qadips_local <- function(file_path) {
  check_valid_file_path(file_path)
  tidyxl_data <- read_tidyxl_data(file_path)
  formatting_data <- read_tidyxl_formatting_data(file_path)
  qadips_data(tidyxl_data, formatting_data)
}

#' Combines the various QADIP data tibbles together alongside final formatting
#'
#' @param tidyxl_data The QADIP data sourced using the tidyxl package
#' @param formatting_data The QADIP excel formatting data sourced using the
#' tidyxl package
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
      call = call
    )

  qadips_data <- dplyr::bind_rows(qadips_key_stats_data,qadips_tab_data)

  return(qadips_data)
}

