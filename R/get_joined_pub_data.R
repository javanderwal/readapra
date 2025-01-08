#' Joins the series dependency and formatting data together
#'
#' @param tidyxl_data The standard data sourced using read_tidyxl_data()
#' @param dependency_names The series dependency data sourced using
#' get_series_dependencies()
#' @param formatting_data The formatting data sourced using
#' read_tidyxl_formatting_data()
#' @param sheet_str_detect A string giving the excel sheet names to extract data
#' from
#'
#' @keywords internal
#'
get_joined_pub_data <- function(tidyxl_data, series_hierarchy_data,
                                formatting_data, sheet_str_detect) {
  # Extract required tidyxl_data and join with series_hierarchy_data
  joined_pub_data <-
    tidyxl_data |>
    dplyr::filter(
      stringr::str_detect(sheet, sheet_str_detect),
      data_type == "numeric" |
        character == "*" # APRA marks censored data by * rather than NA
    ) |>
    dplyr::select(sheet, row, col, "value" = numeric) |>
    dplyr::left_join(series_hierarchy_data, by = dplyr::join_by(sheet, row))

  # Join the formatting data with the tidyxl_data
  number_formatting_data <- joined_formatting_data(tidyxl_data, formatting_data)

  # Join the joined_pub_data with the number_formatting_data
  joined_pub_data <-
    dplyr::left_join(
      x = joined_pub_data,
      y = number_formatting_data,
      by = dplyr::join_by(sheet, row, col)
    )

  # Extracting the date data
  pub_dates <-
    tidyxl_data |>
    dplyr::mutate(
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
    ) |>
    dplyr::filter(
      stringr::str_detect(sheet, sheet_str_detect),
      !grepl("year.*end|end.*year", character, ignore.case = TRUE),
      !is.na(date),
      col != 1
    ) |>
    dplyr::select(sheet, col, date) |>
    dplyr::distinct()

  # Join together the final publication data
  publication_data <-
    dplyr::left_join(
      x = joined_pub_data,
      y = pub_dates,
      by = dplyr::join_by(sheet, col),
      relationship = "many-to-one"
    ) |>
    dplyr::filter(!is.na(date))

  # Format the unit column
  publication_data <- clean_unit_data(publication_data)

  publication_data <-
    dplyr::select(
      .data = publication_data,
      date, sheet, series_hierarchy, series, unit, value
    ) |>
    dplyr::mutate(frequency = "Quarterly", .before = unit)

  return(publication_data)
}
