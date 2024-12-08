read_qadip <- function() {
  temp_file_path <- download_apra(publication = "qadip", cur_hist = "current")
  tidyxl_data <- read_tidyxl_data(temp_file_path)
  formatting_data <- read_tidyxl_formatting_data(temp_file_path)
  qadip_data(tidyxl_data, formatting_data)
}

qadip_key_stats_names <- function(tidyxl_data, series_dependency_data) {
  cohort_rows <-
    series_dependency_data |>
    dplyr:::filter(series_dependency == "ADIs") |>
    dplyr::pull(row)

  key_figures_row <-
    dplyr::filter(tidyxl_data, character %in% c("Key figures"))$row

  if (length(cohort_rows) != 2) {
    cli::cli_abort()
  }

  cleaned_key_stats_names <-
    series_dependency_data |>
    dplyr::mutate(group_id = dplyr::row_number(), .by = series_dependency) |>
    dplyr::mutate(
      measure_type =
        dplyr::case_when(
          row >= key_figures_row ~ "summary_stat",
          row < key_figures_row & row >= max(cohort_rows) ~ "Number of entities",
          row > min(cohort_rows) & row <= max(cohort_rows) ~ "Assets"
        )
    ) |>
    dplyr::mutate(
      series =
        dplyr::case_when(
          group_id == 1 & measure_type == "summary_stat" ~
            "ADIs (excludes 'other ADIs')",
          group_id == 2 & measure_type == "summary_stat" ~
            "Banks",
          group_id == 3 & measure_type == "summary_stat" ~
            "Credit unions and building societies",
          .default = measure_type
        ),
      series_dependency = stringr::str_c(series, "; ", series_dependency)
    )

  max_number_summary_stat_groups <-
    dplyr::filter(cleaned_key_stats_names, measure_type == "summary_stat") |>
    dplyr::pull(group_id) |>
    max()

  if (max_number_summary_stat_groups != 3) {
    cli::cli_abort()
  }

  dplyr::select(
    .data = cleaned_key_stats_names,
    sheet, row, series_dependency, series
  )
}

qadip_key_stats_data <- function(tidyxl_data, formatting_data) {
  series_dependency_data <- get_series_dependencies(
    tidyxl_data = tidyxl_data,
    formatting_data = formatting_data,
    sheet_str_detect = "Key"
  )
  dependency_names_data <- qadip_key_stats_names(
    tidyxl_data = tidyxl_data,
    series_dependency_data = series_dependency_data
  )
  get_joined_pub_data(
    tidyxl_data = tidyxl_data,
    dependency_names = dependency_names_data,
    formatting_data = formatting_data,
    sheet_str_detect = "Key"
  )
}

qadip_tab_data <- function(tidyxl_data, formatting_data) {
  tab_series_dependencies <- get_series_dependencies(
    tidyxl_data = tidyxl_data,
    formatting_data = formatting_data,
    sheet_str_detect = "Tab|^A\\."
  )
  get_joined_pub_data(
    tidyxl_data = tidyxl_data,
    dependency_names = tab_series_dependencies,
    formatting_data = formatting_data,
    sheet_str_detect = "Tab|^A\\."
  )
}

qadip_data <- function(tidyxl_data, formatting_data) {
  bound_qadip_data <-
    dplyr::bind_rows(
      qadip_key_stats_data(tidyxl_data, formatting_data),
      qadip_tab_data(tidyxl_data, formatting_data)
    ) |>
    dplyr::mutate(
      unit = dplyr::case_when(
        stringr::str_detect(unit, "\\%") ~ "Percent",
        stringr::str_detect(series, "Number") ~ "No.",
        .default = "$ million"
      )
    )
  return(bound_qadip_data)
}
