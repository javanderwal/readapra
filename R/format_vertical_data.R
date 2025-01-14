#' Extracts vertical sheet data using the supplied tidyxl_data and formatting_data
#'
#' @param tidyxl_data cell data extracted from a .xlsx file using the tidyxl
#' package
#' @param formatting_data formatting data extracted from a .xlsx file using the
#' tidyxl package
#' @param stat_pub_name the name of the statistical publication series
#' @param drop_col whether or not to drop the column identifier column for each
#' series
#'
#' @noRd
#'
format_vertical_data <- function(
    tidyxl_data,
    formatting_data,
    stat_pub_name,
    drop_col = TRUE) {
  existing_cols <- sort(unique(tidyxl_data$col))

  row_names <- get_col_names(tidyxl_data, existing_cols)

  data_below_top_row <-
    dplyr::filter(.data = tidyxl_data, row > unique(row_names$row))

  restructured_data <-
    restructure_as_in_xlsx(existing_cols, row_names, data_below_top_row)

  pivoted_data <-
    tidyr::pivot_longer(
      data = restructured_data,
      cols = !tidyselect::contains("abn", ignore.case = TRUE) &
        dplyr::where(is.numeric),
      names_to = "series",
      values_to = "value"
    )

  name_cleaned_data <- clean_col_names(pivoted_data)

  extra_meta_data <-
    get_extra_meta_data(
      stat_pub_name = stat_pub_name,
      row_names, existing_cols, data_below_top_row, formatting_data
    )

  cleaned_data <- dplyr::left_join(
    x = name_cleaned_data,
    y = extra_meta_data,
    by = dplyr::join_by("col", "series")
  )
  cleaned_data <- dplyr::relocate(
    .data = cleaned_data, .data$unit, .before = .data$value
    )
  cleaned_data <- dplyr::relocate(
    .data = cleaned_data, .data$statistics_publication_name, .before = 1
  )

  if (drop_col) {
    cleaned_data <- dplyr::select(cleaned_data, !col)
  }

  return(cleaned_data)
}

#' Takes the tidyxl data and returns the data about the top row of names
#'
#' @param tidyxl_data cell data extracted from a .xlsx file using the tidyxl
#' package
#' @param existing_cols the number of valid columns in the .xlsx file
#'
#' @noRd
#'
get_col_names <- function(tidyxl_data, existing_cols) {
  row_names <-
    dplyr::filter(
      .data = tidyxl_data,
      all(.data$data_type == "character"), setequal(existing_cols, col),
      .by = "row"
    )

  if (length(row_names$character) < 1) {
    cli::cli_abort(
      message = "Could not extract row names from the xlsx file.",
      class = "readapra_error_could_not_get_row_names"
    )
  }

  return(row_names)
}

#' Restructures the tidy_xl data so that it resembles what was originally in
#' the .xlsx file
#'
#' @param existing_cols the number of valid columns in the .xlsx file
#' @param row_names the tibble containing the row names data
#' @param data_below_top_row the tibble containing the data below the row names
#'
#' @noRd
#'
restructure_as_in_xlsx <- function(
    existing_cols,
    row_names,
    data_below_top_row) {
  list_column_data <-
    purrr::map(
      .x = existing_cols,
      .f = ~ get_column_data(.x, data = data_below_top_row)
    )
  rejoined_column_data <-
    purrr::reduce(
      .x = list_column_data,
      .f = dplyr::left_join,
      by = dplyr::join_by("row")
    )

  renamed_column_data <-
    rlang::set_names(
      x = dplyr::select(.data = rejoined_column_data, !row),
      nm = paste0(row_names$character, "_", existing_cols)
    )
  return(renamed_column_data)
}

#' Takes a tibble containing column data and extracts each column irrespective
#' of its data type
#'
#' @param col_num the column number in the sheet to extract
#' @param data the data to extract column number data from
#'
#' @noRd
#'
get_column_data <- function(col_num, data) {
  col_data <- dplyr::filter(.data = data, col == col_num)
  col_data <- dplyr::mutate(.data = col_data, date = lubridate::as_date(date))
  col_data <- dplyr::select(.data = col_data, row, .data$error, logical, numeric, date, character)
  col_data <- dplyr::select(.data = col_data, dplyr::where(~ !all(is.na(.))))
}

#' Takes the pivoted data and cleans the column names and extracts the col column
#'
#' @param data data to be cleaned
#'
#' @noRd
#'
clean_col_names <- function(pivoted_data) {
  cleaned_names_data <- janitor::clean_names(pivoted_data)

  names(cleaned_names_data) <-
    stringr::str_remove_all(names(cleaned_names_data), "_\\d+$")

  cleaned_names_data$series <- remove_escape_sequences(cleaned_names_data$series)

  cleaned_names_data <-
    tidyr::separate_wider_regex(
      data = cleaned_names_data,
      cols = .data$series,
      patterns = c(series = ".*", "_", col = ".*")
    )

  # Second run to align with meta data
  cleaned_names_data$series <- remove_escape_sequences(cleaned_names_data$series)

  cleaned_names_data <-
    dplyr::relocate(cleaned_names_data, col, .after = tidyselect::last_col())

  cleaned_names_data$col <- as.numeric(cleaned_names_data$col)

  names(cleaned_names_data) <-
    dplyr::case_when(
      stringr::str_detect(
        string = "^period$",
        pattern = stringr::str_trim(
          stringr::regex(names(cleaned_names_data), ignore_case = TRUE)
        )
      ) ~ "date",
      .default = names(cleaned_names_data)
    )

  return(cleaned_names_data)
}

#' Generates additional meta data to attach to the output tibble using the
#' formatting data (i.e unit and column number)
#'
#' @param row_names the tibble containing the row names data
#' @param data_below_top_row the tibble containing the data below the row names
#'
#' @noRd
#'
get_extra_meta_data <- function(
    stat_pub_name,
    row_names,
    existing_cols,
    data_below_top_row,
    formatting_data) {
  column_binder <-
    dplyr::mutate(
      .data = tibble::tibble(series = row_names$character),
      col = existing_cols
    )

  extra_meta_data <- joined_formatting_data(data_below_top_row, formatting_data)
  extra_meta_data <- dplyr::select(.data = extra_meta_data, col, .data$unit)
  extra_meta_data <- dplyr::distinct(.data = extra_meta_data)
  extra_meta_data <- dplyr::left_join(column_binder, extra_meta_data, by = "col")
  extra_meta_data <- clean_unit_data(extra_meta_data)
  extra_meta_data$series <- remove_escape_sequences(extra_meta_data$series)
  extra_meta_data$statistics_publication_name <- stat_pub_name

  return(extra_meta_data)
}

#' Safely get the vertical sheet data
#'
#' @noRd
#'
safe_format_vertical_data <- purrr::safely(format_vertical_data)

#' Attempts to get the QADIPS Key Stats sheet data and if it encounters an error
#' it throws a warning and returns a empty tibble
#'
#' @param tidyxl_data cell data extracted from a .xlsx file using the tidyxl
#' package
#' @param formatting_data formatting data extracted from a .xlsx file using the
#' tidyxl package
#'
#' @keywords internal
#' @noRd
#'
attempt_format_vertical_data <-
  function(tidyxl_data, formatting_data, stat_pub_name, drop_col = TRUE) {
    outcome <- safe_format_vertical_data(
      tidyxl_data, formatting_data, stat_pub_name,
      drop_col = drop_col
    )
    if (!is.null(outcome$error)) {
      cli::cli_abort(
        message = "The .xlsx file was in an unrecognised structure and could not be imported.",
        class = "readapra_error_vertical_data_unreadable"
      )
    } else {
      return(outcome$result)
    }
  }
