#' Joins the formatting data with the standard data and uses this combined data
#' to construct a "series dependency" variable.
#'
#' @param tidyxl_data The standard data sourced using read_tidyxl_data()
#' @param formatting_data The formatting data sourced using
#' read_tidyxl_formatting_data()
#' @param sheet_str_detect A string giving the excel sheet names to extract data
#' from
#'
#' @keywords internal
#'
get_series_dependencies <-
  function(tidyxl_data, formatting_data, sheet_str_detect) {
    series_dependency_data <-
      dplyr::left_join(
        tidyxl_data,
        tibble::tibble(
          indent = formatting_data$local$alignment$indent,
          local_format_id = 1:length(formatting_data$local$alignment$indent)
        ),
        by =
          dplyr::join_by(local_format_id)
      ) |>
      dplyr::filter(
        stringr::str_detect(sheet, sheet_str_detect),
        col == min(col)
      ) |>
      dplyr::select(sheet, row, col, data_type, character, numeric, indent)

    series_dependency_data <-
      series_dependency_data |>
      dplyr::mutate(character = clean_series_names(character))

    series_dependency_data <-
      dplyr::mutate(
        .data = series_dependency_data,
        which_identifier = dplyr::if_else(
          stringr::str_detect(character, "which"), 1, 0
        ),
        indent = dplyr::if_else(
          condition = stringr::str_detect(character, "^ +"),
          true = nchar(stringr::str_extract(character, "^ +")),
          false = indent
        ),
        character = dplyr::if_else(
          condition = stringr::str_detect(character, "^ +"),
          true = stringr::str_remove(character, "^ +"),
          false = character
        )
      )

    series_dependency_data <- dplyr::mutate(
      series_dependency_data,
      series_dependency = purrr::map_chr(
        seq_along(character),
        ~ iterate_series_dependencies(.x, character, indent, which_identifier)
      ),
      .by = sheet
    ) |>
      dplyr::select(sheet, row, series_dependency, series = character)

    return(series_dependency_data)
  }


#' Cleans the series column of any unusual/unwanted strings
#'
#' @param series The series column to be cleaned (character)
#'
#' @keywords internal
#'
clean_series_names <- function(series) {
  series_cleaned <- stringr::str_trim(series)
  series_cleaned <- stringr::str_remove_all(series_cleaned, "[\u2070-\u209F]") #Remove superscript
  return(series_cleaned)
}

#' Iterates through a vector of strings containing APRA data series names and
#' (e.g. Net interest income) and establishes its dependencies
#'
#' @param index The index column
#' @param character The character column
#' @param indent The column marking the indentation level
#' @param which_identifier The column marking rows containing "which" needing
#' indentation
#'
#' @keywords internal
#'
iterate_series_dependencies <- function(index, character, indent, which_identifier) {
  chain <- character[index]
  current_indent <- indent[index]
  for (j in index:1) {
    if (indent[j] < current_indent && which_identifier[j] == 1) {
      chain <- paste(character[j], chain, sep = " ")
    } else if (indent[j] < current_indent && which_identifier[j] == 0) {
      chain <- paste(character[j], chain, sep = "; ")
      current_indent <- indent[j]
    } else if (indent[j] == current_indent && indent[j] == 0) {
      break
    }
  }
  return(chain)
}

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
get_joined_pub_data <- function(tidyxl_data, dependency_names,
                                formatting_data, sheet_str_detect) {

  # Extract required tidyxl_data and join with dependency_names data
  joined_pub_data <-
    tidyxl_data |>
    dplyr::filter(
      stringr::str_detect(sheet, sheet_str_detect),
      data_type == "numeric" |
        character == "*" # APRA marks censored data by * rather than NA
    ) |>
    dplyr::select(sheet, row, col, "value" = numeric) |>
    dplyr::left_join(dependency_names, by = dplyr::join_by(sheet, row))

  # Join the formatting data with the tidyxl_data
  number_formatting_data <-
    dplyr::left_join(
      tidyxl_data,
      tibble::tibble(
        number_format = formatting_data$local$numFmt,
        local_format_id = 1:length(formatting_data$local$numFmt)
      ),
      by = dplyr::join_by(local_format_id)
    ) |>
    dplyr::select(sheet, row, col, unit = number_format)

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
  publication_data <-
    publication_data |>
    dplyr::mutate(
    unit = dplyr::case_when(
      stringr::str_detect(unit, "\\%") ~ "Percent",
      stringr::str_detect(series, stringr::regex("Number", ignore_case = TRUE)) ~ "No.",
      .default = "$ million"
    )
  )

  publication_data <-
    dplyr::select(
      .data = publication_data,
      date, sheet, series_dependency, series, unit, value
    ) |>
    dplyr::mutate(frequency = "Quarterly", .before = unit)

  return(publication_data)
}
