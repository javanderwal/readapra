table <-
  tidyxl::xlsx_cells(temp_file_path, check_filetype = TRUE, include_blank_cells = FALSE)

table2 <- tidyxl::xlsx_formats(temp_file_path)

new_names_data <-
  table |>
  dplyr::left_join(
    tibble::tibble(
      indent = table2$local$alignment$indent,
      local_format_id = 1:length(table2$local$alignment$indent)
    )
  ) |>
  dplyr::filter(stringr::str_detect(sheet, "Key"), col == min(col)) |>
  dplyr::select(sheet, row, col, data_type, character, numeric, indent) |>
  dplyr::mutate(
    which_identifier = dplyr::if_else(stringr::str_detect(character, "which"), 1, 0),
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
  ) |>
  dplyr::group_by(sheet) |>
  dplyr::mutate(
    dependency_name = purrr::map_chr(seq_along(character), function(i) {
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
    })
  ) |>
  dplyr::ungroup() |>
  dplyr::select(sheet, row, dependency_name)

qadip_data <-
  table |>
  dplyr::filter(stringr::str_detect(sheet, "Key"), data_type == "numeric") |>
  dplyr::select(sheet, row, col, "value" = numeric) |>
  dplyr::left_join(new_names_data, by = dplyr::join_by(sheet, row))

qadip_data_dates <-
  table |>
  dplyr::filter(
    stringr::str_detect(sheet, "Key"),
    stringr::str_detect(character, "[A-Za-z]{3} \\d{4}$")
  ) |>
  dplyr::select(sheet, col, character)

combined_qadip <-
  qadip_data |>
  dplyr::left_join(qadip_data_dates, by = dplyr::join_by(sheet, col)) |>
  dplyr::filter(!stringr::str_detect(character, "^Year End")) |>
  dplyr::mutate(character = lubridate::rollforward(lubridate::my(character)))

