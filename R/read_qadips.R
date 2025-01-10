#' Read Quarterly ADI Performance Statistics
#'
#' @description
#' Download and import the Quarterly Authorised Deposit-taking Institution
#' Performance Statistics (QADIPS) from APRA's website.
#'
#' @return A tibble containing the Quarterly ADI Performance Statistics data.
#' @export
#'
#' @examples
#' read_qadips()
read_qadips <- function() {
  temp_file_path <- download_apra(
    publication = "qadips",
    cur_hist = "current",
    backup_match = "performance"
  )
  tidyxl_data <- read_tidyxl_data(temp_file_path)
  formatting_data <- read_tidyxl_formatting_data(temp_file_path)
  qadip_data(tidyxl_data, formatting_data)
}

#' Read Quarterly ADI Performance Statistics locally
#'
#' @description
#' Import the Quarterly Authorised Deposit-taking Institution
#' Performance Statistics (QADIPS) from a local file.
#'
#' @param file_path The file path to the local QADIPS .xlsx file.
#'
#' @return A tibble containing the Quarterly ADI Performance Statistics data.
#' @export
#'
#' @examples
#' \dontrun{
#' read_qadips_local(file_path = ~path/to/xlsx/file)
#' }
read_qadips_local <- function(file_path) {
  check_valid_file_path(file_path)
  tidyxl_data <- read_tidyxl_data(file_path)
  formatting_data <- read_tidyxl_formatting_data(file_path)
  qadip_data(tidyxl_data, formatting_data)
}

#' Combines the various QADIP data tibbles together alongside final formatting
#'
#' @param tidyxl_data The QADIP data sourced using the tidyxl package
#' @param formatting_data The QADIP excel formatting data sourced using the
#' tidyxl package
#'
#' @keywords internal
#' @noRd
#'
qadip_data <- function(tidyxl_data, formatting_data) {
  bound_qadip_data <-
    dplyr::bind_rows(
      attempt_qadip_key_stats_data(tidyxl_data, formatting_data),
      attempt_horizontal_tab_data(tidyxl_data, formatting_data)
    ) |>
    get_sector_from_sheet() |>
    dplyr::mutate(
      statistics_publication_name = "Quarterly Authorised Deposit-taking Institution Performance Statistics",
      .before = date
    )

  return(bound_qadip_data)
}

#' Extracts the data from the Key Stats sheet and formats it
#'
#' @param tidyxl_data The QADIP data sourced using the tidyxl package
#' @param formatting_data The formatting data sourced using
#' read_tidyxl_formatting_data()
#'
#' @keywords internal
#' @noRd
#'
qadip_key_stats_data <- function(tidyxl_data, formatting_data) {
  series_hierarchy_data <- get_series_hierarchy(
    tidyxl_data = tidyxl_data,
    formatting_data = formatting_data,
    sheet_str_detect = "Key"
  )
  series_hierarchy_data <- qadip_key_stats_names(
    tidyxl_data = tidyxl_data,
    series_hierarchy_data = series_hierarchy_data
  )
  get_joined_pub_data(
    tidyxl_data = tidyxl_data,
    series_hierarchy_data = series_hierarchy_data,
    formatting_data = formatting_data,
    sheet_str_detect = "Key"
  )
}

#' Safely get the QADIPS Key Stats sheet data
#'
#' @keywords internal
#' @noRd
#'
safely_qadip_key_stats_data <- purrr::safely(qadip_key_stats_data)

#' Attempts to get the QADIPS Key Stats sheet data and if it encounters an error
#' it throws a warning and returns a empty tibble
#'
#' @param tidyxl_data The QADIP data sourced using the tidyxl package
#' @param formatting_data The formatting data sourced using
#' read_tidyxl_formatting_data()
#'
#' @keywords internal
#' @noRd
#'
attempt_qadip_key_stats_data <- function(tidyxl_data, formatting_data) {
  results <- safely_qadip_key_stats_data(tidyxl_data, formatting_data)
  if (!is.null(results$error)) {
    cli::cli_warn(
      message = "Could not extract data from the \"Key Stats\" sheet. Data from the \"Key Stats\" sheet has been omitted.",
      class = "read_apra_warning_key_stats_inaccessible"
    )
    return(tibble::tibble())
  } else {
    return(results$result)
  }
}

#' Gets dependency names for the Key Stats sheet in the QADIP. This sheet is
#' structured differently from the other sheets and needs a lot of tweaking to
#' get working.
#'
#' @param tidyxl_data The QADIP data sourced using the tidyxl package
#' @param series_dependency_data The series dependency data sourced using
#' get_series_dependencies()
#'
#' @keywords internal
#'
qadip_key_stats_names <- function(tidyxl_data, series_hierarchy_data) {
  cohort_rows <-
    series_hierarchy_data |>
    dplyr:::filter(series_hierarchy == "ADIs") |>
    dplyr::pull(row)

  key_figures_row <-
    dplyr::filter(tidyxl_data, character %in% c("Key figures"))$row

  if (length(cohort_rows) != 2 | length(key_figures_row) != 1) {
    cli::cli_abort(
      message = "Structure of the 'Key Stats' sheet has changed. Unable to extract data.",
      class = "read_apra_error_key_figures_or_cohort_rows_wrong"
    )
  }

  cleaned_key_stats_names <-
    series_hierarchy_data |>
    dplyr::mutate(group_id = dplyr::row_number(), .by = series_hierarchy) |>
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
      series_hierarchy = stringr::str_c(series, "; ", series_hierarchy)
    )

  max_number_summary_stat_groups <-
    dplyr::filter(cleaned_key_stats_names, measure_type == "summary_stat") |>
    dplyr::pull(group_id) |>
    max()

  if (max_number_summary_stat_groups != 3) {
    cli::cli_abort(
      message = "Structure of the 'Key Stats' sheet has changed. Unable to extract data.",
      class = "read_apra_error_wrong_number_summary_stat_groups"
    )
  }

  key_stats_names_data <-
    dplyr::select(
      .data = cleaned_key_stats_names,
      sheet, row, series_hierarchy, series
    )

  return(key_stats_names_data)
}
