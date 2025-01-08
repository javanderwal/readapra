#' Wrapper around read tidyxl's xlsx_cells()
#'
#' @param file_path Path to the downloaded xlsx file to import.
#'
#' @keywords internal
#'
read_tidyxl_data <- function(file_path) {
  tidyxl::xlsx_cells(
    path = file_path,
    check_filetype = TRUE,
    include_blank_cells = FALSE
  )
}

#' Wrapper around read tidyxl's xlsx_format()
#'
#' @param file_path Path to the downloaded xlsx file to import.
#'
#' @keywords internal
#'
read_tidyxl_formatting_data <- function(file_path) {
  tidyxl::xlsx_formats(file_path)
}

#' Check whether a file path is valid and throw an error if not.
#'
#' @param file_path the file path to check
#'
#' @keywords internal
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

#' Wrapper used for testing purposes
#'
#' @keywords internal
#'
file_exists_wrapper <- file.exists

