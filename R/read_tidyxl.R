#' Wrapper around read tidyxl's xlsx_cells() which can also extract specific sheets.
#'
#' @param file_path path to the file to import
#' @param sheets a regex string identifying which sheets to extract
#' @param number_sheets the expected number of sheets to be extracted
#'
#' @noRd
#'
read_tidyxl_data <- function(file_path,
                             sheets = NA,
                             number_sheets = 1,
                             call = rlang::caller_env()) {
  if (!is.na(sheets)) {
    sheet_names <- tidyxl::xlsx_sheet_names(file_path)
    sheets <- stringr::str_subset(sheet_names, stringr::regex(sheets, ignore_case = T))
    if (length(sheets) != number_sheets) {
      cli::cli_abort(
        message = c(
          "Could not extract data from the downloaded file.",
          c("x" = "Could not determine which sheet to extract data from.")
        ),
        class = "readapra_error_multiple_sheets_selected",
        call = call
      )
    }
  }

  tidyxl::xlsx_cells(
    path = file_path,
    check_filetype = TRUE,
    sheets = sheets,
    include_blank_cells = FALSE
  )
}

#' Wrapper around read tidyxl's xlsx_format()
#'
#' @param file_path Path to the downloaded xlsx file to import.
#'
#' @noRd
#'
read_tidyxl_formatting_data <- function(file_path) {
  tidyxl::xlsx_formats(file_path)
}
