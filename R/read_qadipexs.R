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
#' read_qpexs("current")
read_qadipexs <- function(cur_hist) {
  rlang::arg_match(cur_hist, c("current", "historic"))

  if (cur_hist == "current") {
    backup_match_string <- "property|exposure"
    backup_remove_string <- "historic"
  } else {
    backup_match_string <- "(property|exposure).*historic"
    backup_remove_string <- NULL
  }

  temp_file_path <- download_apra(
    publication = "qpex",
    cur_hist = cur_hist,
    backup_match = backup_match_string,
    backup_remove = backup_remove_string
  )
  tidyxl_data <- read_tidyxl_data(temp_file_path)
  formatting_data <- read_tidyxl_formatting_data(temp_file_path)
  qpexs_data(tidyxl_data, formatting_data)
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
#' read_qpexs_local(file_path = ~path/to/xlsx/file)
#' }
read_qadipexs_local <- function(file_path) {
  check_valid_file_path(file_path)
  tidyxl_data <- read_tidyxl_data(file_path)
  formatting_data <- read_tidyxl_formatting_data(file_path)
  qpexs_data(tidyxl_data, formatting_data)
}

#' Extracts the QPEXS data from the various sheets and conducts final formatting
#' tweaks
#'
#' @param tidyxl_data The data sourced using the tidyxl package
#' @param formatting_data The formatting data sourced using the tidyxl package
#'
#' @keywords internal
#' @noRd
#'
qpexs_data <- function(tidyxl_data, formatting_data) {
  attempt_horizontal_tab_data(tidyxl_data, formatting_data) |>
    dplyr::mutate(
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
    ) |>
    dplyr::mutate(
      statistics_publication_name = "Quarterly Authorised Deposit-taking Institution Property Exposures Statistics",
      .before = date
    )
}
