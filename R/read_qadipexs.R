#' Read Quarterly ADI Property Exposure Statistics
#'
#' @description
#' Download and import the Quarterly Authorised Deposit-taking Institution
#' Property Exposure Statistics (QADIPEXS) from APRA's website.
#'
#' @param cur_hist Whether to access the current or historical series. Valid
#' values are `"current"` and `"historical"`.
#'
#' @return A tibble containing the Quarterly ADI Property Exposure Statistics
#' data.
#' @export
#'
#' @examples
#' \dontrun{
#' read_qadipexs("current")
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
  read_qadipexs_local(temp_file_path)
}

#' Read Quarterly ADI Property Exposure Statistics locally
#'
#' @description
#' Import the Quarterly Authorised Deposit-taking Institution
#' Property Exposure Statistics (QPEXS) from a local file.
#'
#' @param file_path The file path to the local QPEXS .xlsx file.
#'
#' @return A tibble containing the Quarterly ADI Performance Statistics data.
#' @export
#'
#' @examples
#' \dontrun{
#' read_qadipexs_local(file_path = ~path/to/xlsx/file)
#' }
read_qadipexs_local <- function(file_path) {
  check_valid_file_path(file_path)
  tidyxl_data <- read_tidyxl_data(file_path)
  formatting_data <- read_tidyxl_formatting_data(file_path)
  qadipexs_data(tidyxl_data, formatting_data)
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

  return(qadipexs_data )
}
