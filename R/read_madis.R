#' Read Monthly ADI Statistics
#'
#' @description
#' Read in the Monthly Authorised Deposit-taking Institution Statistics (MADIS)
#' from APRA's website.
#'
#' @param cur_hist Character; valid values are `"current"` or `"historical"`.
#'
#' @return A tibble containing the Monthly ADI Statistics data
#' @export
#'
#' @examples
#' read_madis("current")
read_madis <- function(cur_hist) {
  rlang::arg_match(cur_hist, c("current", "historic"))
  temp_file_path <-
    download_apra(
      publication = "madis",
      cur_hist = cur_hist
      )
  tidyxl_data <- read_tidyxl_data(temp_file_path, "table.*1")
  formatting_data <- read_tidyxl_formatting_data(temp_file_path)
  madis_data(tidyxl_data, formatting_data, cur_hist)
}

#' Read Monthly ADI Statistics locally
#'
#' @description
#' Read in the Monthly Authorised Deposit-taking Institution Statistics (MADIS)
#' from a local file.
#'
#' @param file_path The file path to the local MADIS .xlsx file.
#' @param cur_hist Whether to access the current or historical series. Valid
#' values are `"current"` and `"historical"`.
#'
#' @return A tibble containing the Monthly ADI Statistics data.
#' @export
#'
#' @examples
#' \dontrun{
#' read_madis_local(file_path = ~path/to/xlsx/file, cur_hist = "current")
#' }
read_madis_local <- function(file_path, cur_hist) {
  rlang::arg_match(cur_hist, c("current", "historic"))
  tidyxl_data <- read_tidyxl_data(file_path, "table.*1")
  formatting_data <- read_tidyxl_formatting_data(file_path)
  madis_data(tidyxl_data, formatting_data, cur_hist)
}

#' Extracts the MADIS data and cleans it
#'
#' @param tidyxl_data The data sourced using the tidyxl package
#' @param formatting_data The formatting data sourced using the tidyxl package
#' @param cur_hist Whether to access the current or historical series. Valid
#' values are `"current"` and `"historical"`.
#'
#' @keywords internal
#' @noRd
#'
madis_data <- function(tidyxl_data, formatting_data, cur_hist) {
  cleaned_madis_data <-
    attempt_cleaned_vertical_data(tidyxl_data, formatting_data, drop_col = FALSE) |>
    dplyr::mutate(
      statistics_publication_name = "Monthly Authorised Deposit-taking Institution Statistics",
      .before = date
    ) |>
    add_madis_balance_sheet(cur_hist) |>
    dplyr::select(!col)

  return(cleaned_madis_data)
}

#' Adds a balance sheet category to the MADIS data. Certain MADIS series have
#' the same name, adding this column helps distinguish them.
#'
#' @param madis_data The cleaned MADIS data
#' @param cur_hist Whether to access the current or historical series. Valid
#' values are `"current"` and `"historical"`.
#'
#' @keywords internal
#' @noRd
#'
add_madis_balance_sheet <- function(madis_data, cur_hist) {
  if (cur_hist == "current") {
    madis_data <-
      dplyr::mutate(
        .data = madis_data,
        balance_sheet_category = dplyr::case_when(
          col %in% 4:9 ~ "Selected assets on Australian books of selected individual ADIs",
          col %in% 10:19 ~ "Loans and finance leases on Australian books of selected individual ADIs ",
          col %in% 20:24 ~ "Selected liabilities on Australian books of selected individual ADIs",
          col %in% 25:30 ~ "Deposits on Australian books of selected individual ADIs",
          .default = "Unknown category"
        ),
        .before = series
      )
  }

  if (cur_hist == "historic") {
    madis_data <-
      dplyr::mutate(
        .data = madis_data,
        balance_sheet_category = dplyr::case_when(
          col %in% 4:14 ~ "Selected assets on Australian books of selected individual ADIs",
          col %in% 15:24 ~ "Loans and finance leases on Australian books of selected individual ADIs ",
          col %in% 25:30 ~ "Selected liabilities on Australian books of selected individual ADIs",
          col %in% 31:38 ~ "Deposits on Australian books of selected individual ADIs",
          .default = "Unknown category"
        ),
        .before = series
      )
  }

  return(madis_data)
}
