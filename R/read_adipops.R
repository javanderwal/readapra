#' Read ADI Points of Presence Statistics
#'
#' @description
#' Import the Authorised Deposit-taking Institution Points of Presence
#' Read ADI Points of Presence Statistics from a local file.
#'
#' @param file_path path to the local .xlsx file.
#' @param cur_hist character vector determining whether to download the current
#' publication (`"current"`) or the historic publication (`"historic"`).
#'
#' @return A tibble containing the ADI Points of Presence Statistics data.
#'
#' @noRd
#'
read_adipops <- function(file_path, cur_hist, call = rlang::caller_env()) {
  tidyxl_data <- read_tidyxl_data(file_path, sheets = "table.*1", call = call)
  formatting_data <- read_tidyxl_formatting_data(file_path)
  adipops_data(tidyxl_data, formatting_data, call = call)
}

#' Extracts the ADIPOPS data and cleans it.
#'
#' @param tidyxl_data cell data extracted from a .xlsx file using the tidyxl
#' package
#' @param formatting_data formatting data extracted from a .xlsx file using the
#' tidyxl package
#'
#' @noRd
#'
adipops_data <- function(tidyxl_data,
                         formatting_data,
                         call = rlang::caller_env()) {
  adipops_data <-
    attempt_format_vertical_data(
      tidyxl_data = tidyxl_data,
      formatting_data = formatting_data,
      stat_pub_name = "Authorised Deposit-taking Institutions' Points of Presence Statistics",
      frequency = "Quarterly",
      call = call
    )
  adipops_data <- convert_adipops_units(adipops_data)
  return(adipops_data)
}

#' Cleans up the units column in the ADIPOPS data so it matches other publications.
#'
#' @param data the ADIPOPS data to be cleaned.
#'
#' @noRd
#'
convert_adipops_units <- function(data) {
  adipops_correct_units <-
    data |>
    dplyr::mutate(unit = dplyr::case_when(
      stringr::str_detect(
        string = series,
        pattern = stringr::regex("number", ignore_case = TRUE)
      ) ~ unit,
      .default = series
    ))
  return(adipops_correct_units)
}
