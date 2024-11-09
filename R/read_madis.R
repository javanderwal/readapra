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
read_madis <- function(cur_hist) {
  rlang::arg_match(cur_hist, c("current", "historic"))

  temp_file_path <- download_apra(publication = "madis", cur_hist = cur_hist)

  madis_data <- clean_madis_raw(
    file_path = temp_file_path,
    cur_hist = cur_hist,
    call = rlang::caller_env()
  )

  return(madis_data)
}

#' Read Monthly ADI Statistics locally
#'
#' @description
#' Read in the Monthly Authorised Deposit-taking Institution Statistics (MADIS)
#' from a local file.
#'
#' @param cur_hist Character; valid values are `"current"` or `"historical"`.
#'
#' @return A tibble containing the Monthly ADI Statistics data
#' @export
#'
#' @examples
read_madis_local <- function(file_path, cur_hist) {
  rlang::arg_match(cur_hist, c("current", "historic"))

  madis_data <- clean_madis_raw(
    file_path = file_path,
    cur_hist = cur_hist,
    call = rlang::caller_env()
  )

  return(madis_data)
}

#' This function imports the raw MADIS data as a tibble and then proceeds to
#' tidy it into a usable format for the end user.
#' @keywords internal
clean_madis_raw <- function(file_path, cur_hist, call = rlang::caller_env()) {
  stat_pub_name <- "Monthly ADI Statistics"

  madis_data <- clean_imported_data(
    file_path = file_path,
    sheet = "Table 1",
    stat_pub_name = stat_pub_name
  )

  # Numbering column names for later balance sheet identification
  names(madis_data) <-
    stringr::str_c(1:length(names(madis_data)), "_", names(madis_data))

  if (names(madis_data)[[1]] != "1_Period" ||
    names(madis_data)[[2]] != "2_ABN" ||
    names(madis_data)[[3]] != "3_Institution Name") {
    cli::cli_abort(
      message = "Cannot import {.field {stat_pub_name}}.
      Spreadsheet is not in a recognised format.",
      call = call
    )
  }

  madis_data <-
    dplyr::rename(
      .data = madis_data,
      "date" = `1_Period`,
      "abn" = `2_ABN`,
      "institution_name" = `3_Institution Name`,
    )

  madis_data <-
    tidyr::pivot_longer(
      data = madis_data,
      cols = !c(date, abn, institution_name),
      names_to = "series"
    )

  madis_data <-
    tidyr::separate_wider_delim(
      data = madis_data,
      cols = series,
      delim = "_",
      names = c("column_number", "series")
    )

  # Balance sheet identificaiton
  if (cur_hist == "current") {
    madis_data <-
      dplyr::mutate(
        .data = madis_data,
        balance_sheet_category = dplyr::case_when(
          column_number %in% 4:9 ~ "Selected assets on Australian books of selected individual ADIs",
          column_number %in% 10:19 ~ "Loans and finance leases on Australian books of selected individual ADIs ",
          column_number %in% 20:24 ~ "Selected liabilities on Australian books of selected individual ADIs",
          column_number %in% 25:30 ~ "Deposits on Australian books of selected individual ADIs",
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
          column_number %in% 4:14 ~ "Selected assets on Australian books of selected individual ADIs",
          column_number %in% 15:24 ~ "Loans and finance leases on Australian books of selected individual ADIs ",
          column_number %in% 25:30 ~ "Selected liabilities on Australian books of selected individual ADIs",
          column_number %in% 31:38 ~ "Deposits on Australian books of selected individual ADIs",
          .default = "Unknown category"
        ),
        .before = series
      )
  }

  # Adding information to tibble
  madis_data <-
    tibble::add_column(
      .data = madis_data,
      frequency = "Monthly",
      unit = "$ millions",
      .after = "series"
    )

  madis_data <-
    tibble::add_column(
      .data = madis_data,
      stat_pub_name = "Monthly Authorised Deposit-taking Institution Statistics (MADIS)",
      .before = "date"
    )

  return(madis_data)
}
