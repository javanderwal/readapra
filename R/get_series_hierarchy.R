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

    cleaned_series_hierarchy_data <-
      split(series_dependency_data, series_dependency_data$sheet) |>
      purrr::map(iterate_series_dependencies) |>
      dplyr::bind_rows() |>
      dplyr::arrange(match(sheet, unique(series_dependency_data$sheet))) |>
      dplyr::select(sheet, row, series_dependency, series = character)

    return(cleaned_series_hierarchy_data)
  }


#' Cleans the series column of any unusual/unwanted strings
#'
#' @param series The series column to be cleaned (character)
#'
#' @keywords internal
#'
clean_series_names <- function(series) {
  series_cleaned <- stringr::str_trim(series)
  series_cleaned <- stringr::str_remove_all(series_cleaned, "[\u2070-\u209F]") # Remove superscript
  return(series_cleaned)
}

#' Takes the data computed inside get_series_dependencies() and iterates through
#' each row to work out the series hierarchy
#'
#' @param data Data computed inside get_series_dependencies()
#'
#' @keywords internal
#'
iterate_series_dependencies <- function(data) {
  data <-
    adjust_indent(data) |>
    mutate(series_dependency = vector("list", n()))

  for (i in seq_len(nrow(data))) {
    current_indent <- data$adjusted_indent[i]
    current_text <- data$character[i]

    if (current_indent == 0) {
      data$series_dependency[[i]] <- current_text
    } else {
      # Find the parent row
      parent_index <- max(which(data$adjusted_indent < current_indent & seq_len(nrow(data)) < i))
      parent_path <- data$series_dependency[[parent_index]]
      # Combine current text with its parent.
      if (stringr::str_detect(parent_path, ":$")) {
        data$series_dependency[[i]] <- paste(parent_path, current_text)
      } else {
        data$series_dependency[[i]] <- paste(parent_path, current_text, sep = "; ")
      }
    }
  }
  data$series_dependency <- unlist(data$series_dependency)
  return(data)
}

#' Combines the indent and which_identifier columns generated inside
#' get_series_dependencies() to create an adjusted_indent column
#'
#' @param data Data computed inside get_series_dependencies()
#'
#' @keywords internal
#'
adjust_indent <- function(data) {
  data$adjusted_indent <- data$indent
  for (i in seq_len(nrow(data))) {
    if (data$which_identifier[i] == 1) {
      # Increment the current "which" row's indent
      data$adjusted_indent[i] <- data$adjusted_indent[i] + 1
      # Propagate the increment to subsequent children
      for (j in (i + 1):nrow(data)) {
        if (data$indent[j] > data$indent[i]) {
          data$adjusted_indent[j] <- data$adjusted_indent[j] + 1
        } else {
          break
        }
      }
    }
  }
  return(data)
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
