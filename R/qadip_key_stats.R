#' Gets dependency names for the Key Stats sheet in the QADIP. This sheet is
#' structured differently from the other sheets and needs a lot of tweaking to
#' get working.
#'
#' @param tidyxl_data The QADIP data sourced using the tidyxl package
#' @param series_hierarchy_data The series dependency data sourced using
#' get_series_dependencies()
#'
#' @keywords internal
#' @noRd
#'
qadip_key_stats_names <- function(series_hierarchy_data, tidyxl_data) {
  cohort_rows <-
    dplyr::filter(.data = series_hierarchy_data, series_hierarchy == "ADIs")$row

  key_figures_row <-
    dplyr::filter(.data = tidyxl_data, character %in% c("Key figures"))$row

  if (length(cohort_rows) != 2 | length(key_figures_row) != 1) {
    cli::cli_abort(
      message = "Structure of the 'Key Stats' sheet has changed. Unable to extract data.",
      class = "readapra_error_key_figures_or_cohort_rows_wrong"
    )
  }

  cleaned_key_stats_names <-
    dplyr::mutate(
      .data = series_hierarchy_data,
      group_id = dplyr::row_number(),
      .by = series_hierarchy
    )

  cleaned_key_stats_names <-
    dplyr::mutate(
      .data = cleaned_key_stats_names,
      measure_type =
        dplyr::case_when(
          row >= key_figures_row ~ "summary_stat",
          row < key_figures_row & row >= max(cohort_rows) ~ "Number of entities",
          row > min(cohort_rows) & row <= max(cohort_rows) ~ "Assets"
        )
    )

  cleaned_key_stats_names <-
    dplyr::mutate(
      .data = cleaned_key_stats_names,
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
    max(dplyr::filter(cleaned_key_stats_names, measure_type == "summary_stat")$group_id)

  if (max_number_summary_stat_groups != 3) {
    cli::cli_abort(
      message = "Structure of the 'Key Stats' sheet has changed. Unable to extract data.",
      class = "readapra_error_wrong_number_summary_stat_groups"
    )
  }

  key_stats_names_data <-
    dplyr::select(
      .data = cleaned_key_stats_names,
      sheet, sheet_details, row, series_hierarchy, series
    )

  return(key_stats_names_data)
}
