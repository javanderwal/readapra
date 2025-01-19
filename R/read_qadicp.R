#' Read Quarterly ADI Centralised Publication
#'
#' @description
#' Import the Quarterly Authorised Deposit-taking Institution Centralised
#' Publication (QADICP) from a local file.
#'
#' @param file_path path to the local .xlsx file.
#' @param cur_hist character vector determining whether to download the current
#' publication (`"current"`) or the historic publication (`"historic"`).
#'
#' @return A tibble containing the Quarterly ADI Centralised Publication data.
#'
#' @noRd
#'
read_qadicp <- function(file_path, cur_hist, call = rlang::caller_env()) {
  tidyxl_data <- read_tidyxl_data(file_path, "table.*4", call = call)
  formatting_data <- read_tidyxl_formatting_data(file_path)
  qadicp_data(tidyxl_data, formatting_data, call = call)
}

#' Combines the various QADICP data tibbles together alongside final formatting
#'
#' @param tidyxl_data cell data extracted from a .xlsx file using the tidyxl
#' package
#' @param formatting_data formatting data extracted from a .xlsx file using the
#' tidyxl package
#'
#' @noRd
#'
qadicp_data <- function(
    tidyxl_data,
    formatting_data,
    call = rlang::caller_env()) {
  qadicp_data <-
    attempt_format_vertical_data(
      tidyxl_data = tidyxl_data,
      formatting_data = formatting_data,
      stat_pub_name = "Quarterly Authorised Deposit-taking Institution Centralised Publication",
      frequency = "Quarterly",
      drop_col = FALSE,
      call = call
    )
  qadicp_data <- add_qadicp_regulatory_category(qadicp_data, call = call)
  qadicp_data <- dplyr::select(.data = qadicp_data, !col)
  return(qadicp_data)
}

#' Adds a balance sheet category to the MADIS data. Certain MADIS series have
#' the same name, adding this column helps distinguish them.
#'
#' @param qadicp_data The cleaned qadicp data
#'
#' @keywords internal
#' @noRd
#'
add_qadicp_regulatory_category <- function(
    qadicp_data,
    call = rlang::caller_env()
    ) {
  original_col <- qadicp_data$col
  qadicp_data$col <- qadicp_data$col - min(qadicp_data$col) + 1

  qadicp_data <-
    dplyr::left_join(
      x = qadicp_data,
      y = qadicp_risk_metric_category,
      by = dplyr::join_by("series", "col")
    )

  if (any(is.na(qadicp_data$risk_metric_category))) {
    cli::cli_warn(
      message = "Could not successfully attach contents of {.code risk_metric_category} column. The contents of this column are incomplete.",
      class = "readapra_warning_nas_in_qadicp_risk_metric_category"
    )
  }

  qadicp_data <- dplyr::relocate(
    .data = qadicp_data, risk_metric_category, .before = series
  )

  qadicp_data$col <- original_col

  return(qadicp_data)
}
