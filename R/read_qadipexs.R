#' Read Quarterly ADI Property Exposure Statistics
#'
#' @description
#' Download and import the Quarterly Authorised Deposit-taking Institution
#' Property Exposure Statistics (QADIPEXS) from APRA's website. Both the
#' current and historic versions of this statistical publication are available.
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
#' @return A tibble containing the Quarterly ADI Property Exposure Statistics
#' data.
#' @export
#'
#' @examples
#' \dontrun{
#' read_qadipexs(cur_hist = "current")
#' }
read_qadipexs <- function(
    cur_hist,
    path = tempdir(),
    overwrite = TRUE,
    quiet = FALSE,
    ...) {
  rlang::arg_match(cur_hist, c("current", "historic"))
  temp_file_path <- download_apra(
    publication = "qadipexs",
    cur_hist = cur_hist,
    path = path,
    quiet = quiet,
    overwrite = overwrite,
    ...
  )
  read_qadipexs_local(temp_file_path, cur_hist)
}

#' Read Quarterly ADI Property Exposure Statistics locally
#'
#' @description
#' Import the Quarterly Authorised Deposit-taking Institution
#' Property Exposure Statistics (QPEXS) from a local file. Both the current and
#' historic versions of this statistical publication are available.
#'
#' @param file_path path to the local .xlsx file.
#' @param cur_hist character vector determining whether to download the current
#' publication (`"current"`) or the historic publication (`"historic"`).
#'
#' @return A tibble containing the Quarterly ADI Performance Statistics data.
#' @export
#'
#' @examples
#' \dontrun{
#' read_qadipexs_local(
#'   file_path = "path/to/xlsx/file.xlsx",
#'   cur_hist = "current"
#' )
#' }
read_qadipexs_local <- function(file_path, cur_hist) {
  rlang::arg_match(cur_hist, c("current", "historic"))
  check_valid_file_path(file_path)
  tidyxl_data <- read_tidyxl_data(file_path)
  formatting_data <- read_tidyxl_formatting_data(file_path)
  qadipexs_data(tidyxl_data, formatting_data, cur_hist)
}

#' Extracts the QADIPEXS data from the various sheets and conducts final
#' formatting tweaks
#'
#' @param tidyxl_data The data sourced using the tidyxl package
#' @param formatting_data The formatting data sourced using the tidyxl package
#'
#' @keywords internal
#' @noRd
#'
qadipexs_data <- function(
    tidyxl_data,
    formatting_data,
    cur_hist,
    call = rlang::caller_env()) {
  qadipexs_data <-
    attempt_format_horizontal_data(
      tidyxl_data = tidyxl_data,
      formatting_data = formatting_data,
      stat_pub_name = "Quarterly Authorised Deposit-taking Institution Property Exposures Statistics",
      sheet_str_detect = "Tab",
      frequency = "Quarterly",
      call = call
    )

  if (cur_hist == "current") {
      qadipexs_data <-
    dplyr::mutate(
      .data = qadipexs_data,
      unit = dplyr::case_when(
        series %in% c(
          "Impaired assets to exposures",
          "Specific provisions to exposures",
          "Specific provisions to impaired exposures",
          "Specific provisions and security held to impaired exposures",
          "Non-performing to total exposures",
          "Specific provisions to total exposures",
          "Specific provisions to non-performing exposures",
          "Specific provisions and security held to non-performing exposures",
          "Weighted average variable rate of new loans funded",
          "Weighted average assessment rate used for serviceability"
        )
        ~ "Percent",
        .default = unit
      )
    )
  }

  return(qadipexs_data )
}
