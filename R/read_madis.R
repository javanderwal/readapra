#' Read Monthly ADI Statistics
#'
#' @description
#' Download and import the Monthly Authorised Deposit-taking Institution
#' Statistics (MADIS) from APRA's website. Both the current and historic
#' versions of this statistical publication are available.
#'
#' @param cur_hist character vector determining whether to download the current
#' publication (`"current"`) or the historic publication (`"historic"`).
#' @param path path to where the downloaded file should be saved. Uses
#' [base::tempdir()] by default.
#' @param overwrite whether to overwrite the downloaded file when re-downloading
#' the file.
#' @param quiet whether to suppress the download progress bar.
#' @param ... additional arguments to be passed to [utils::download.file()].
#'
#' @return A tibble containing the Monthly ADI Statistics data.
#' @export
#'
#' @examples
#' \dontrun{
#' read_madis(cur_hist = "current")
#' }
read_madis <- function(
    cur_hist,
    path = tempdir(),
    overwrite = TRUE,
    quiet = FALSE,
    ...) {
  rlang::arg_match(cur_hist, c("current", "historic"))
  temp_file_path <-
    download_apra(
      publication = "madis",
      cur_hist = cur_hist,
      path = path,
      quiet = quiet,
      overwrite = overwrite,
      ...
    )
  read_madis_local(temp_file_path, cur_hist)
}

#' Read Monthly ADI Statistics locally
#'
#' @description
#' Import the Monthly Authorised Deposit-taking Institution Statistics (MADIS)
#' from a local file. Both the current and historic versions of this
#' statistical publication are available.
#'
#' @param file_path path to the local .xlsx file.
#' @param cur_hist character vector determining whether to download the current
#' publication (`"current"`) or the historic publication (`"historic"`).
#'
#' @return A tibble containing the Monthly ADI Statistics data.
#' @export
#'
#' @examples
#' \dontrun{
#' read_madis_local(
#'   file_path = "path/to/xlsx/file.xlsx"
#'   cur_hist = "current"
#' )
#' }
read_madis_local <- function(file_path, cur_hist) {
  rlang::arg_match(cur_hist, c("current", "historic"))
  check_valid_file_path(file_path)
  tidyxl_data <- read_tidyxl_data(file_path, "table.*1")
  formatting_data <- read_tidyxl_formatting_data(file_path)
  madis_data(tidyxl_data, formatting_data, cur_hist)
}

#' Extracts the MADIS data and cleans it
#'
#' @param tidyxl_data cell data extracted from a .xlsx file using the tidyxl
#' package
#' @param formatting_data formatting data extracted from a .xlsx file using the
#' tidyxl package
#' @param cur_hist character vector determining whether to download the current
#' publication (`"current"`) or the historic publication (`"historic"`).
#'
#' @noRd
#'
madis_data <- function(tidyxl_data, formatting_data, cur_hist) {
  madis_data <-
    attempt_format_vertical_data(
      tidyxl_data = tidyxl_data,
      formatting_data = formatting_data,
      stat_pub_name = "Monthly Authorised Deposit-taking Institution Statistics",
      frequency = "Monthly",
      drop_col = FALSE
    )
  madis_data <- add_madis_balance_sheet(madis_data, cur_hist)
  madis_data <- dplyr::select(.data = madis_data, !col)

  return(madis_data)
}

#' Adds a balance sheet category to the MADIS data. Certain MADIS series have
#' the same name, adding this column helps distinguish them.
#'
#' @param madis_data the cleaned MADIS data
#' @param cur_hist character vector determining whether to download the current
#' publication (`"current"`) or the historic publication (`"historic"`).
#'
#' @keywords internal
#' @noRd
#'
add_madis_balance_sheet <- function(
    madis_data,
    cur_hist,
    call = rlang::caller_env()) {
  original_col <- madis_data$col
  madis_data$col <- madis_data$col - min(madis_data$col) + 1

  if (cur_hist == "current") {
    madis_data <-
      dplyr::left_join(
        x = madis_data,
        y = madis_current_balance_sheet,
        by = dplyr::join_by("series", "col")
      )
  }

  if (cur_hist == "historic") {
    madis_data <-
      dplyr::left_join(
        x = madis_data,
        y = madis_historic_balance_sheet,
        by = dplyr::join_by("series", "col")
      )
  }

  if (any(is.na(madis_data$balance_sheet_category))) {
    cli::cli_warn(
      message = "Could not successfully attach contents of {.code balance_sheet_category} column. The contents of this column are incomplete.",
      class = "readapra_warning_nas_in_madis_balance_sheet_category"
    )
  }

  madis_data <- dplyr::relocate(
    .data = madis_data, balance_sheet_category, .before = series
  )

  madis_data$col <- original_col

  return(madis_data)
}
