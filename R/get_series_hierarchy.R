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
    indexed_formatting_data <-
      tibble::tibble(
        indent = formatting_data$local$alignment$indent,
        local_format_id = 1:length(formatting_data$local$alignment$indent)
      )

    series_hierarchy_data <-
      dplyr::left_join(tidyxl_data, indexed_formatting_data, "local_format_id")

    series_hierarchy_data <-
      dplyr::filter(
        .data = series_hierarchy_data,
        stringr::str_detect(sheet, sheet_str_detect), col == min(col)
      )

    series_hierarchy_data <-
      dplyr::select(
        .data = series_hierarchy_data,
        sheet, row, col, data_type, character, numeric, indent
      )

    series_hierarchy_data$character <-
      remove_escape_sequences(series_hierarchy_data$character)

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

    series_hierarchy_data <-
      dplyr::bind_rows(
        purrr::map(
          .x = split(series_hierarchy_data, series_hierarchy_data$sheet),
          .f = iterate_series_hierarchy
        )
      )

    series_hierarchy_data <-
      dplyr::arrange(
        .data = series_hierarchy_data,
        match(sheet, unique(series_hierarchy_data$sheet))
      )

    series_hierarchy_data <-
      dplyr::select(
        .data = series_hierarchy_data,
        sheet, sheet_details, row, series_hierarchy, series = character
      )

    return(series_hierarchy_data)
  }

#' Takes the data computed inside get_series_hierarchy() and iterates through
#' each row to work out the series hierarchy
#'
#' @param data data computed inside get_series_hierarchy()
#'
#' @keywords internal
#' @noRd
#'
iterate_series_hierarchy <- function(data) {
  data <- create_sheet_details_col(data)
  data <- adjust_indent(data)
  data <- dplyr::mutate(data, series_hierarchy = vector("list", dplyr::n()))

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

#' Creates a sheet_details column in the provided data based on the first row
#' of the character column
#'
#' @param data data to create the sheet_details column in.
#'
#' @keywords internal
#' @noRd
#'
create_sheet_details_col <- function(data) {
  data[["sheet_details"]] <- data$character[1]
  dplyr::relocate(.data = data, sheet_details, .after = sheet)
}

#' Combines the indent and which_identifier columns generated inside
#' get_series_hierarchy() to create an adjusted_indent column
#'
#' @param data data computed inside get_series_hierarchy()
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
