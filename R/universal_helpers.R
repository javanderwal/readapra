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
