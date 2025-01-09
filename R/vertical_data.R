#' Extracts vertical sheet data using the supplied tidyxl_data and formatting_data
#'
#' @param tidyxl_data The data sourced using the tidyxl package
#' @param formatting_data The formatting data sourced using the tidyxl package
#'
#' @keywords internal
#' @noRd
#'
cleaned_vertical_data <- function(tidyxl_data, formatting_data, drop_col = TRUE) {
  existing_cols <- sort(unique(tidyxl_data$col))

  top_row_names <-
    tidyxl_data |>
    dplyr::filter(
      all(data_type == "character"), setequal(existing_cols, col),
      .by = "row"
    )

  data_below_top_row <-
    tidyxl_data |>
    dplyr::filter(row > unique(top_row_names$row))

  excel_structured_data <-
    purrr::map(
      .x = existing_cols,
      .f = ~ get_column_data(.x, data = data_below_top_row)
    ) |>
    purrr::reduce(dplyr::left_join, by = dplyr::join_by("row")) |>
    dplyr::select(!row) |>
    purrr::set_names(top_row_names$character)

  pivoted_data <-
    excel_structured_data |>
    tidyr::pivot_longer(
      cols = !tidyselect::matches("^abn$", ignore.case = TRUE) & dplyr::where(is.numeric),
      names_to = "series",
      values_to = "value"
    )

  new_names <-
    names(pivoted_data) |>
    tolower() |>
    stringr::str_replace_all(" ", "_") |>
    stringr::str_replace_all("_\\(y/n\\)", "")

  names(pivoted_data) <- new_names

  if ("period" %in% names(pivoted_data)) {
    names(pivoted_data)[names(pivoted_data) == "period"] <- "date"
  }

  column_binder <-
    tibble::tibble(series = top_row_names$character) |>
    dplyr::mutate(col = existing_cols)

  number_formatting_data <-
    joined_formatting_data(data_below_top_row, formatting_data) |>
    dplyr::select(col, unit) |>
    dplyr::distinct() |>
    dplyr::left_join(column_binder, by = "col") |>
    clean_unit_data()

  cleaned_data <-
    pivoted_data |>
    dplyr::left_join(number_formatting_data, by = "series") |>
    dplyr::relocate(unit, .before = value)

  if (drop_col) {
    cleaned_data <- dplyr::select(cleaned_data, !col)
  }

  return(cleaned_data)
}

#' Takes a tibble containing column data and extracts each column irrespective
#' of its data type
#'
#' @param col_num The column number in the sheet to extract
#' @param data The data to extract column number data from
#'
#' @keywords internal
#' @noRd
#'
get_column_data <- function(col_num, data) {
  data |>
    dplyr::filter(col == col_num) |>
    dplyr::mutate(date = lubridate::as_date(date)) |>
    dplyr::select(row, error, logical, numeric, date, character) |>
    dplyr::select(dplyr::where(~ !all(is.na(.))))
}

#' Safely get the vertical sheet data
#'
#' @keywords internal
#' @noRd
#'
safely_cleaned_vertical_data <- purrr::safely(cleaned_vertical_data)

#' Attempts to get the QADIPS Key Stats sheet data and if it encounters an error
#' it throws a warning and returns a empty tibble
#'
#' @param tidyxl_data The data sourced using the tidyxl package
#' @param formatting_data The formatting data sourced using the tidyxl package
#'
#' @keywords internal
#' @noRd
#'
attempt_cleaned_vertical_data <-
  function(tidyxl_data, formatting_data, drop_col = TRUE) {
    results <- safely_cleaned_vertical_data(
      tidyxl_data, formatting_data,
      drop_col = drop_col
    )
    if (!is.null(results$error)) {
      cli::cli_abort(
        message = "The .xlsx file was in an unrecognised structure and could not be imported.",
        class = "read_apra_error_horizontal_data_unreadable"
      )
    } else {
      return(results$result)
    }
  }
