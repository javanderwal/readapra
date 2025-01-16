#' Check whether a file path is valid and throw an error if not.
#'
#' @param file_path the file path to check
#'
#' @keywords internal
#' @noRd
#'
check_valid_file_path <- function(file_path, call = rlang::caller_env()) {
  if (!is.character(file_path)) {
    cli::cli_abort(
      message = c(
        "{.arg file_path} must be a string of length 1.",
        c("x" = "Supplied argument is of class {.cls {class(file_path)}}.")
      ),
      class = "read_apra_error_file_path_not_string",
      call = call
    )
  }

  if (length(file_path) != 1) {
    cli::cli_abort(
      message = c(
        "{.arg file_path} must be a string of length 1.",
        c("x" = "Supplied argument is of length {.val {length(file_path)}}.")
      ),
      class = "read_apra_error_file_path_not_length_one",
      call = call
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
      class = "read_apra_error_file_path_does_not_exist",
      call = call
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

#' Removes escapes sequences from a vector of characters and any trailing spaces.
#'
#' @param x vector of characters
#'
#' @noRd
#'
remove_escape_sequences <- function(x) {
  superscript_pattern <- "[\u2070\u00B9\u00B2\u00B3\u2074\u2075\u2076\u2077\u2078\u2079\u1D43\u1D47\u1D9C\u1D48\u1D49\u1D4D\u1D4F\u02B0\u2071\u02B2\u1D4A\u02E1\u1D50\u207F\u1D52\u1D56\u1D57\u1D58\u1D5B\u02B3\u02E2\u1D5C\u1D5D\u1D5E\u1D5F\u02B7\u02E3\u02B8\u1DBB]"

  x <- stringr::str_replace_all(x, "[\\r\\n\\t]+", " ")
  x <- stringr::str_replace_all(x, "\\s{2,}", " ")
  x <- stringr::str_replace_all(x, superscript_pattern, "")
  x <- stringr::str_trim(x)
  return(x)
}
