#' Read Quarterly ADI Centralised Publication
#'
#' @description
#' Download and import Read in the Quarterly Authorised Deposit-taking
#' Institution Centralised Publication (QADICP) from APRA's website.
#'
#' @return A tibble containing the Quarterly ADI Centralised Publication data.
#' @export
#'
#' @examples
#' read_qadicp()
read_qadicp <- function() {
  temp_file_path <-
    download_apra(
      publication = "qadicp",
      cur_hist = "current",
      backup_match = "centralised"
    )
  tidyxl_data <- read_tidyxl_data(temp_file_path)
  formatting_data <- read_tidyxl_formatting_data(temp_file_path)
  qadicp_data(tidyxl_data, formatting_data)
}

#' Read Quarterly ADI Centralised Publication locally
#'
#' @description
#' Import the Quarterly Authorised Deposit-taking Institution Centralised
#' Publication (QADICP) from a local file.
#'
#' @return A tibble containing the Quarterly ADI Centralised Publication data.
#' @export
#'
#' @examples
#' \dontrun{
#' read_qadicp_local(file_path = ~path/to/xlsx/file)
#' }
read_qadicp_local <- function(file_path) {
  tidyxl_data <- read_tidyxl_data(file_path)
  formatting_data <- read_tidyxl_formatting_data(file_path)
  qadicp_data(tidyxl_data, formatting_data)
}

#' Combines the various QADICP data tibbles together alongside final formatting
#'
#' @param tidyxl_data The data sourced using the tidyxl package
#' @param formatting_data The formatting data sourced using the tidyxl package
#'
#' @keywords internal
#' @noRd
#'
qadicp_data <- function(tidyxl_data, formatting_data) {
  table_4_data <-
    tidyxl_data |>
    dplyr::filter(stringr::str_detect(tolower(sheet), "table.*4"))

  cleaned_qadicp_data <-
    attempt_cleaned_vertical_data(table_4_data, formatting_data) |>
    dplyr::mutate(
      statistics_publication_name = "Quarterly Authorised Deposit-taking Institution Centralised Publication",
      .before = date
    )

  return(cleaned_qadicp_data)
}


