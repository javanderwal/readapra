#' Read Quarterly ADI Centralised Publication
#'
#' @description
#' Download and import the Quarterly Authorised Deposit-taking
#' Institution Centralised Publication (QADICP) from APRA's website.
#'
#' @param cur_hist character vector determining whether to download the current
#' publication (`"current"`) or the historic publication (`"historic"`).
#' @param path path to where the downloaded file should be saved. Uses
#' [base::tempdir()] by default.
#' @param overwrite whether to overwrite the downloaded file when re-downloading
#' the file.
#' @param quiet whether to suppress the download progress bar.
#' @param ... additional arguments to be passed to [utils::download.file()].
#'
#' @return A tibble containing the Quarterly ADI Centralised Publication data.
#' @export
#'
#' @examples
#' \dontrun{
#' read_qadicp()
#' }
read_qadicp <- function(
    path = tempdir(),
    overwrite = TRUE,
    quiet = FALSE,
    ...) {
  temp_file_path <-
    download_apra(
      publication = "qadicp",
      cur_hist = "current",
      path = path,
      quiet = quiet,
      overwrite = overwrite,
      ...
    )
  read_qadicp_local(temp_file_path)
}

#' Read Quarterly ADI Centralised Publication locally
#'
#' @description
#' Import the Quarterly Authorised Deposit-taking Institution Centralised
#' Publication (QADICP) from a local file.
#'
#' @param file_path path to the .xlsx file.
#'
#' @return A tibble containing the Quarterly ADI Centralised Publication data.
#' @export
#'
#' @examples
#' \dontrun{
#' read_qadicp_local(file_path = ~ path / to / xlsx / file)
#' }
read_qadicp_local <- function(file_path) {
  check_valid_file_path(file_path)
  tidyxl_data <- read_tidyxl_data(file_path, "table.*4")
  formatting_data <- read_tidyxl_formatting_data(file_path)
  qadicp_data(tidyxl_data, formatting_data)
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
qadicp_data <- function(tidyxl_data, formatting_data) {
  qadicp_data <-
    attempt_format_vertical_data(
      tidyxl_data = tidyxl_data,
      formatting_data = formatting_data,
      stat_pub_name = "Quarterly Authorised Deposit-taking Institution Centralised Publication",
      drop_col = FALSE
    )
  qadicp_data <- add_qadicp_category(qadicp_data)
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
add_qadicp_category <- function(qadicp_data) {
  qadicp_data_w_categories <-
    dplyr::mutate(
      .data = qadicp_data,
      series_category = dplyr::case_when(
        col %in% 7:13 ~ "Regulatory capital",
        col %in% 14:20 ~ " Risk-weighted assets (RWA)",
        col %in% 21:23 ~ "Liquidity ratios",
        .default = NA
      ),
      .before = series
    )

  return(qadicp_data_w_categories)
}
