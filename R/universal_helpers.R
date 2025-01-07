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

#' Extracts and formats the data from a horizontal statistical publication
#'
#' @param tidyxl_data The tidyxl data sourced using the tidyxl package
#' @param formatting_data The excel formatting data sourced using the
#' tidyxl package
#'
#' @keywords internal
#'
horizontal_tab_data <- function(tidyxl_data, formatting_data) {
  tab_series_dependencies <- get_series_dependencies(
    tidyxl_data = tidyxl_data,
    formatting_data = formatting_data,
    sheet_str_detect = "Tab|^A\\."
  )
  tab_data <-
    get_joined_pub_data(
      tidyxl_data = tidyxl_data,
      dependency_names = tab_series_dependencies,
      formatting_data = formatting_data,
      sheet_str_detect = "Tab|^A\\."
    )
  return(tab_data)
}

#' Safely get the QADIPS tab sheet data
#'
#' @keywords internal
#'
safely_horizontal_tab_data <- purrr::safely(horizontal_tab_data)

#' Attempts to get the QADIPS Key Stats sheet data and if it encounters an error
#' it throws a warning and returns a empty tibble
#'
#' @param tidyxl_data The QADIPS data sourced using the tidyxl package
#' @param formatting_data The formatting data sourced using
#' read_tidyxl_formatting_data()
#'
attempt_horizontal_tab_data <- function(tidyxl_data, formatting_data) {
  results <- safely_horizontal_tab_data(tidyxl_data, formatting_data)
  if (!is.null(results$error)) {
    cli::cli_abort(
      message = "The .xlsx file was in an unrecognised structure and could not be imported.",
      class = "read_apra_error_horizontal_data_unreadable"
    )
  } else {
    return(results$result)
  }
}
