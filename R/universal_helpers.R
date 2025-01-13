#' Check whether a file path is valid and throw an error if not.
#'
#' @param file_path the file path to check
#'
#' @keywords internal
#' @noRd
#'
check_valid_file_path <- function(file_path) {
  if (!is.character(file_path)) {
    cli::cli_abort(
      message = c(
        "{.arg file_path} must be a string of length 1.",
        c("x" = "Supplied argument is of class {.cls {class(file_path)}}.")
      ),
      class = "read_apra_error_file_path_not_string"
    )
  }

  if (length(file_path) != 1) {
    cli::cli_abort(
      message = c(
        "{.arg file_path} must be a string of length 1.",
        c("x" = "Supplied argument is of length {.val {length(file_path)}}.")
      ),
      class = "read_apra_error_file_path_not_length_one"
    )
  }

  suppressWarnings(normalised_file_path <- normalizePath(file_path))

  if (!file_exists_wrapper(normalised_file_path)) {
    cli::cli_abort(
      message =
        c(
          c("x" = "No such file exists at: {.path {normalised_file_path}}."),
          c("!" = "Please check you have specified the correct file path.")
        ),
      class = "read_apra_error_file_path_does_not_exist"
    )
  }
}

#' Joins together the tidyxl_data and formatting_data
#'
#' @param tidyxl_data The standard data sourced using read_tidyxl_data()
#' @param formatting_data The formatting data sourced using
#' read_tidyxl_formatting_data()
#'
#' @keywords internal
#' @noRd
#'
joined_formatting_data <- function(tidyxl_data, formatting_data) {
  dplyr::left_join(
    tidyxl_data,
    tibble::tibble(
      unit = formatting_data$local$numFmt,
      local_format_id = 1:length(formatting_data$local$numFmt)
    ),
    by = dplyr::join_by(local_format_id)
  ) |>
    dplyr::select(sheet, row, col, unit)
}

#' Takes a tibble containing a column named "unit" and cleans it
#'
#' @param data Tibble containing a unit column to be cleaned
#'
#' @keywords internal
#' @noRd
#'
clean_unit_data <- function(data) {
  data |>
    dplyr::mutate(
      unit = dplyr::case_when(
        stringr::str_detect(unit, "\\%") ~ "Percent",
        stringr::str_detect(series, stringr::regex("Number", ignore_case = TRUE)) ~ "No.",
        .default = "$ million"
      )
    )
}

#' Tries to determine the sector for the relevant series using the sheet (and series) column
#'
#' @param data The data to be cleaned
#'
#' @keywords internal
#' @noRd
get_sector_from_sheet <- function(data) {
  dplyr::mutate(
    .data = data,
    sector = dplyr::case_when(
      stringr::str_detect(sheet, "\\s1[[:alpha:]]|Tab 1") ~ "ADIs (excludes 'other ADIs')",
      stringr::str_detect(sheet, "\\s2[[:alpha:]]|Tab 2") ~ "Banks",
      stringr::str_detect(sheet, "\\s3[[:alpha:]]|Tab 3") ~ "Credit unions and building societies",
      stringr::str_detect(sheet, "\\s4[[:alpha:]]|Tab 4") ~ "Major banks",
      stringr::str_detect(sheet, "\\s5[[:alpha:]]|Tab 5") ~ "Other domestic banks",
      stringr::str_detect(sheet, "\\s6[[:alpha:]]|Tab 6") ~ "Foreign subsidiary banks",
      stringr::str_detect(sheet, "\\s7[[:alpha:]]|Tab 7") ~ "Foreign branch banks",
      stringr::str_detect(sheet, "\\s8[[:alpha:]]|Tab 8") ~ "Mutual ADIs",
      stringr::str_detect(sheet, "A\\.1[[:alpha:]]") ~ "Building Societies",
      stringr::str_detect(sheet, "A\\.2[[:alpha:]]") ~ "Credit Unions",
      stringr::str_detect(tolower(series), "asset|number") ~ stringr::str_extract(series_hierarchy, "[^;]+$"),
      stringr::str_detect(series, "ADIs (excludes 'other ADIs')|Banks|Credit unions and building societies") ~ series,
      .default = "Unknown"
    ),
    .after = sheet
  )
}
