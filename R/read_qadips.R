#' Read Quarterly ADI Performance Statistics
#'
#' @description
#' Download and import the Quarterly Authorised Deposit-taking Institution
#' Performance Statistics (QADIPS) from APRA's website.
#'
#' @param path path to where the downloaded file should be saved. Uses
#' [base::tempdir()] by default.
#' @param overwrite whether to overwrite the downloaded file when re-downloading
#' the file.
#' @param quiet whether to suppress the download progress bar.
#' @param ... additional arguments to be passed to [utils::download.file()].
#'
#' @return A tibble containing the Quarterly ADI Performance Statistics data.
#' @export
#'
#' @examples
#' \dontrun{
#' read_qadips()
#' }
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
#' @param file_path path to the local .xlsx file.
#'
#' @return A tibble containing the Quarterly ADI Performance Statistics data.
#' @export
#'
#' @examples
#' \dontrun{
#' read_qadips_local(file_path = "path/to/xlsx/file.xlsx")
#' }
read_qadips_local <- function(file_path) {
  check_valid_file_path(file_path)
  tidyxl_data <- read_tidyxl_data(file_path)
  formatting_data <- read_tidyxl_formatting_data(file_path)
  qadips_data(tidyxl_data, formatting_data)
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

  qadips_data <- dplyr::bind_rows(qadips_key_stats_data,qadips_tab_data)

  return(qadips_data)
}

