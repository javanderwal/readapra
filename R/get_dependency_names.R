read_tidyxl_formatting_data <- function(file_path) {
  tidyxl::xlsx_formats(file_path)
}

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
    dplyr::filter(stringr::str_detect(sheet, sheet_str_detect), col == min(col)) |>
    dplyr::select(sheet, row, col, data_type, character, numeric, indent)

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

  series_dependency_data <-
    dplyr::mutate(
      .data = series_dependency_data,
      series_dependency = purrr::map_chr(seq_along(character), function(i) {
        chain <- character[i]
        current_indent <- indent[i]
        for (j in i:1) {
          if (indent[j] < current_indent && which_identifier[j] == 1) {
            chain <- paste(character[j], chain, sep = " ")
          } else if (indent[j] < current_indent && which_identifier[j] == 0) {
            chain <- paste(character[j], chain, sep = "; ")
            current_indent <- indent[j]
          } else if (indent[j] == current_indent && indent[j] == 0) {
            break
          }
        }
        chain
      }),
      .by = sheet
    ) |>
    dplyr::select(sheet, row, series_dependency, series = character)

  return(series_dependency_data)
}

read_tidyxl_data <- function(file_path) {
  tidyxl::xlsx_cells(
    path = file_path,
    check_filetype = TRUE,
    include_blank_cells = FALSE
  )
}

get_joined_pub_data <- function(tidyxl_data, dependency_names,
                                formatting_data, sheet_str_detect) {
  joined_pub_data <-
    tidyxl_data |>
    dplyr::filter(
      stringr::str_detect(sheet, sheet_str_detect),
      data_type == "numeric" |
        character == "*"
    ) |>
    dplyr::select(sheet, row, col, "value" = numeric) |>
    dplyr::left_join(dependency_names, by = dplyr::join_by(sheet, row))

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

  joined_pub_data <-
    dplyr::left_join(
      x = joined_pub_data,
      y = number_formatting_data,
      by = dplyr::join_by(sheet, row, col)
  )

  pub_dates <-
    tidyxl_data |>
    dplyr::mutate(
      date = dplyr::if_else(
        condition = !is.na(character),
        true =
          lubridate::parse_date_time(
            x = character,
            orders = c("my", "ymd", "dmy"),
            quiet = TRUE
          ) ,
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

  publication_data <-
    dplyr::left_join(
      x = joined_pub_data,
      y = pub_dates,
      by = dplyr::join_by(sheet, col),
      relationship = "many-to-one"
    ) |>
    dplyr::filter(!is.na(date))

  publication_data <-
    dplyr::select(
      .data = publication_data,
      date, sheet, series_dependency, series, unit, value
    )

  return(publication_data)
}
