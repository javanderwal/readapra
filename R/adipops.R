#' Read ADI Points of Presence Statistics
#'
#' @description
#' Read in the Authorised Deposit-taking Institution Points of Presence
#' Statistics (ADIPOPS) from APRA's website.
#'
#' @return A tibble containing the ADIPOPS data
#' @export
#'
#' @examples
#' read_adipops("current")
read_adipops <- function() {
  temp_file_path <- download_apra(
    publication = "adipops",
    cur_hist = "current"
  )
  tidyxl_data <- read_tidyxl_data(file_path = temp_file_path, sheets = "table.*1")
  formatting_data <- read_tidyxl_formatting_data(temp_file_path)
  adipops_data(tidyxl_data, formatting_data)
}

#' Read ADI Points of Presence Statistics locally
#'
#' @description
#' Read in the Authorised Deposit-taking Institution Points of Presence
#' Statistics (ADIPOPS) from a local file.
#'
#' @return A tibble containing the ADIPOPS data.
#' @export
#'
#' @examples
#' \dontrun{
#' read_adipops_local(file_path = ~path/to/xlsx/file)
#' }
read_adipops_local <- function() {
  tidyxl_data <- read_tidyxl_data(file_path, sheets = "table.*1")
  formatting_data <- read_tidyxl_formatting_data(file_path)
  adipops_data(tidyxl_data, formatting_data)
}

#' Extracts the ADIPOPS data and cleans it.
#'
#' @param tidyxl_data The data sourced using the tidyxl package
#' @param formatting_data The formatting data sourced using the tidyxl package
#'
#' @keywords internal
#' @noRd
#'
adipops_data <- function(tidyxl_data, formatting_data) {
  cleaned_adipops_data <-
    attempt_cleaned_vertical_data(tidyxl_data, formatting_data) |>
    dplyr::mutate(
      statistics_publication_name = "Authorised Deposit-taking Institutions' Points of Presence statistics",
      .before = date
    ) |>
    convert_adipops_units()

  return(cleaned_adipops_data)
}

#' Cleans up the units column in the ADIPOPS data so it matches other publications.
#'
#' @param data The ADIPOPS data to be cleaned.
#'
#' @keywords internal
#' @noRd
#'
convert_adipops_units <- function(data) {
  adipops_correct_units <-
    data |>
    dplyr::mutate(unit = dplyr::case_when(
      stringr::str_detect(series, stringr::regex("number", ignore_case = TRUE)) ~ unit,
      .default = series
    ))
  return(adipops_correct_units)
}
