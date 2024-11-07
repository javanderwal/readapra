read_madis <- function(cur_hist) {
  rlang::arg_match(cur_hist, c("current", "historic"))

  temp_file_path <-
    download_apra(publication = "madis", cur_hist = cur_hist)

  if (cur_hist == "current") {
    return(import_madis_current(temp_file_path))
  }

  if (cur_hist == "historic") {
    return(import_madis_historic(temp_file_path))
  }

  return(madis_data)
}

import_madis_historic <- function(file_path) {
  readxl::read_xlsx(
    path = file_path,
    sheet = "Table 1",
    trim_ws = TRUE
  ) |>
    dplyr::mutate(
      date = lubridate::as_date(Period),
      .keep = "unused",
      .before = 1
    ) |>
    dplyr::rename(
      "abn" = ABN,
      "institution_name" = `Institution Name`
    ) |>
    tidyr::pivot_longer(
      cols = !c(date, abn, institution_name),
      names_to = "metric"
    )
}

import_madis_current <- function(file_path) {
  readxl::read_xlsx(
    path = file_path,
    sheet = "Table 1",
    trim_ws = TRUE,
    skip = 1
  ) |>
    dplyr::mutate(
      date = lubridate::as_date(Period),
      .keep = "unused",
      .before = 1
    ) |>
    dplyr::rename(
      "abn" = ABN,
      "institution_name" = `Institution Name`
    ) |>
    tidyr::pivot_longer(
      cols = !c(date, abn, institution_name),
      names_to = "metric"
    )
}
