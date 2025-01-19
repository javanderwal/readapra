#' Extracts and formats the data from a horizontal statistical publication
#'
#' @param tidyxl_data cell data extracted from a .xlsx file using the tidyxl
#' package
#' @param formatting_data formatting data extracted from a .xlsx file using the
#' tidyxl package
#' @param stat_pub_name the name of the statistical publication
#' @param sheet_str_detect the regex used to identify the sheets to extract
#' @param frequency the frequency of the statistical publication
#' @param series_hierarchy_fn a function to apply to the series_hierarchy data
#' @param series_hierarchy_args list of args to be passed to the function in
#' series_hierarchy_fn
#'
#' @keywords internal
#' @noRd
#'
format_horizontal_data <- function(tidyxl_data,
                                   formatting_data,
                                   stat_pub_name,
                                   sheet_str_detect,
                                   frequency,
                                   series_hierarchy_fn = NULL,
                                   series_hierarchy_args = NULL) {
  series_hierarchy_data <-
    get_series_hierarchy(
      tidyxl_data = tidyxl_data,
      formatting_data = formatting_data,
      sheet_str_detect = sheet_str_detect
    )

  if (!is.null(series_hierarchy_fn)) {
    series_hierarchy_data <-
      do.call(
        series_hierarchy_fn,
        c(list(series_hierarchy_data), series_hierarchy_args)
      )
  }

  relevant_tidyxl_data <-
    dplyr::filter(
      .data = tidyxl_data,
      stringr::str_detect(sheet, sheet_str_detect),
      data_type == "numeric" | character == "*" # APRA marks censored data by * rather than NA
    )

  relevant_tidyxl_data <-
    dplyr::select(
      .data = relevant_tidyxl_data,
      sheet, row, col, "value" = numeric
    )

  final_joined_data <-
    dplyr::left_join(
      x = relevant_tidyxl_data,
      y = series_hierarchy_data,
      by = dplyr::join_by(sheet, row)
    )

  tidyxl_number_formatting_data <-
    joined_formatting_data(tidyxl_data, formatting_data)

  final_joined_data <-
    dplyr::left_join(
      x = final_joined_data,
      y = tidyxl_number_formatting_data,
      by = dplyr::join_by(sheet, row, col)
    )

  dates_data <- get_horizontal_data_dates(tidyxl_data, sheet_str_detect)

  final_joined_data <-
    dplyr::left_join(
      x = final_joined_data,
      y = dates_data,
      by = dplyr::join_by(sheet, col),
      relationship = "many-to-one"
    )

  final_joined_data <- dplyr::filter(.data = final_joined_data, !is.na(date))

  final_joined_data <- clean_unit_data(final_joined_data)

  final_joined_data <-
    dplyr::select(
      .data = final_joined_data,
      date, sheet, sheet_details, series_hierarchy, series, unit, value
    )

  final_joined_data <-
    dplyr::mutate(
      .data = final_joined_data,
      frequency = frequency,
      .before = unit
    )

  final_joined_data <-
    dplyr::mutate(
      .data = final_joined_data,
      statistics_publication_name = stat_pub_name,
      .before = 1
    )

  return(final_joined_data)
}

#' Extracts the horizontal date data and cleans it
#'
#' @param tidyxl_data cell data extracted from a .xlsx file using the tidyxl
#' package
#' @param sheet_str_detect the regex used to identify the sheets to extract
#'
#' @keywords internal
#' @noRd
#'
get_horizontal_data_dates <- function(tidyxl_data, sheet_str_detect) {
  dates_data <-
    dplyr::mutate(
      .data = tidyxl_data,
      # if_else to convert non-date dates to dates.
      date = dplyr::if_else(
        condition = !is.na(character),
        true =
          lubridate::parse_date_time(
            x = character,
            orders = c("my", "ymd", "dmy"),
            quiet = TRUE
          ),
        false = date
      ),
      date = lubridate::as_date(date),
      date = lubridate::rollforward(date)
    )

  dates_data <-
    dplyr::filter(
      .data = dates_data,
      stringr::str_detect(sheet, sheet_str_detect),
      !grepl("year.*end|end.*year", character, ignore.case = TRUE),
      !is.na(date),
      col != 1
    )

  dates_data <-
    dplyr::distinct(dplyr::select(.data = dates_data, sheet, col, date))

  return(dates_data)
}

#' Safely get the QADIPS tab sheet data
#'
#' @keywords internal
#' @noRd
#'
safe_format_horizontal_data <- purrr::safely(format_horizontal_data)

#' Attempts to get the QADIPS Key Stats sheet data and if it encounters an error
#' it throws a warning and returns a empty tibble
#'
#' @param tidyxl_data cell data extracted from a .xlsx file using the tidyxl
#' package
#' @param formatting_data formatting data extracted from a .xlsx file using the
#' tidyxl package
#' @param stat_pub_name the name of the statistical publication
#' @param sheet_str_detect the regex used to identify the sheets to extract
#' @param frequency the frequency of the statistical publication
#' @param series_hierarchy_fn a function to apply to the series_hierarchy data
#' @param series_hierarchy_args list of args to be passed to the function in
#' series_hierarchy_fn
#' @param error_or_warning whether to display an error or warning if safely fails
#' @param message the message to display in the message or warning.
#' @param call the caller environment
#'
#' @keywords internal
#' @noRd
#'
attempt_format_horizontal_data <- function(tidyxl_data,
                                           formatting_data,
                                           stat_pub_name,
                                           sheet_str_detect,
                                           frequency,
                                           series_hierarchy_fn = NULL,
                                           series_hierarchy_args = NULL,
                                           error_or_warning = "error",
                                           message = "The .xlsx file was in an unrecognised structure and could not be imported.",
                                           call = rlang::caller_env()) {
  rlang::arg_match(error_or_warning, c("error", "warning"))

  outcome <-
    safe_format_horizontal_data(
      tidyxl_data = tidyxl_data,
      formatting_data = formatting_data,
      stat_pub_name = stat_pub_name,
      sheet_str_detect = sheet_str_detect,
      frequency = frequency,
      series_hierarchy_fn = series_hierarchy_fn,
      series_hierarchy_args = series_hierarchy_args
    )

  if (!is.null(outcome$error)) {
    if (error_or_warning == "error") {
      cli::cli_abort(
        message = message,
        class = "readapra_error_horizontal_data_unreadable",
        call = call
      )
    } else {
      cli::cli_warn(
        message = message,
        class = "readapra_warning_horizontal_data_unreadable",
        call = call
      )
    }
  } else {
    return(outcome$result)
  }
}
