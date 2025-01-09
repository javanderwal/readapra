#' Joins the formatting data with the standard data and uses this combined data
#' to construct a "get_series_hierarchy" variable.
#'
#' @param tidyxl_data The standard data sourced using read_tidyxl_data()
#' @param formatting_data The formatting data sourced using
#' read_tidyxl_formatting_data()
#' @param sheet_str_detect A string giving the excel sheet names to extract data
#' from
#'
#' @keywords internal
#' @noRd
#'
get_series_hierarchy <-
  function(tidyxl_data, formatting_data, sheet_str_detect) {
    series_hierarchy_data <-
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

    series_hierarchy_data <-
      series_hierarchy_data |>
      dplyr::mutate(character = clean_series_names(character))

    series_hierarchy_data <-
      dplyr::mutate(
        .data = series_hierarchy_data,
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
      split(series_hierarchy_data, series_hierarchy_data$sheet) |>
      purrr::map(iterate_series_hierarchy) |>
      dplyr::bind_rows() |>
      dplyr::arrange(match(sheet, unique(series_hierarchy_data$sheet))) |>
      dplyr::select(sheet, row, series_hierarchy, series = character)

    return(cleaned_series_hierarchy_data)
  }


#' Cleans the series column of any unusual/unwanted strings
#'
#' @param series The series column to be cleaned (character)
#'
#' @keywords internal
#' @noRd
#'
clean_series_names <- function(series) {
  series_cleaned <- stringr::str_trim(series)
  series_cleaned <- stringr::str_remove_all(series_cleaned, "[\u2070-\u209F]") # Remove superscript
  return(series_cleaned)
}

#' Takes the data computed inside get_series_hierarchy() and iterates through
#' each row to work out the series hierarchy
#'
#' @param data Data computed inside get_series_hierarchy()
#'
#' @keywords internal
#' @noRd
#'
iterate_series_hierarchy <- function(data) {
  data <-
    adjust_indent(data) |>
    dplyr::mutate(series_hierarchy = vector("list", dplyr::n()))

  for (i in seq_len(nrow(data))) {
    current_indent <- data$adjusted_indent[i]
    current_text <- data$character[i]

    if (current_indent == 0) {
      data$series_hierarchy[[i]] <- current_text
    } else {
      # Find the parent row
      parent_index <- max(which(data$adjusted_indent < current_indent & seq_len(nrow(data)) < i))
      parent_path <- data$series_hierarchy[[parent_index]]
      # Combine current text with its parent.
      if (stringr::str_detect(parent_path, ":$")) {
        data$series_hierarchy[[i]] <- paste(parent_path, current_text)
      } else {
        data$series_hierarchy[[i]] <- paste(parent_path, current_text, sep = "; ")
      }
    }
  }
  data$series_hierarchy <- unlist(data$series_hierarchy)
  return(data)
}

#' Combines the indent and which_identifier columns generated inside
#' get_series_hierarchy() to create an adjusted_indent column
#'
#' @param data Data computed inside get_series_hierarchy()
#'
#' @keywords internal
#' @noRd
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
