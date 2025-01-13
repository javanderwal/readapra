#' Creates data that mimics a .xlsx file
#'
#' @noRd
#'
create_test_xlsx_data <- function() {
  tibble::tibble(
    date = lubridate::rollforward(
      seq.Date(
        as.Date("2015-12-01"),
        as.Date("2016-12-01"),
        by = "1 month"
      )
    ),
    name = "Entity A",
    std_value = 1:13,
    per_value = seq(0.2, 0.8, 0.05)
  )
}


#' Creates a temporary .xlsx file for testing purposes.
#'
#' @noRd
#'
create_test_xlsx_file <- function() {
  temp_file <- tempfile(fileext = ".xlsx")
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, "Sheet1")
  openxlsx::addWorksheet(wb, "Sheet2")
  openxlsx::writeData(wb, "Sheet1", create_test_xlsx_data())
  openxlsx::addStyle(
    wb = wb,
    sheet = "Sheet1",
    style = openxlsx::createStyle(numFmt = "0%"),
    cols = 4,
    rows = 2:14,
    gridExpand = TRUE
  )
  openxlsx::saveWorkbook(wb, temp_file, overwrite = TRUE)
  return(temp_file)
}







#' Generate the dummy horizontal tidyxl_input data
#'
#' @keywords internal
#'
dummy_horizontal_tidyxl_input <- function() {
  sheet_1a <-
    dplyr::bind_rows(
      # Numeric data
      tibble::tibble(
        sheet = rep("Tab 1a", 32),
        row = c(rep(1, 16), rep(2, 16)),
        col = rep(1:16, 2),
        character = c(c("example_1", rep(NA, 15)), c("example_2", rep(NA, 15))),
        numeric = c(c(NA, 1:15), c(NA, 16:30)),
        data_type = rep(c("character", rep("numeric", 15)), 2),
        local_format_id = 1:32
      ) |>
        # Checking that * values are included as NAs
        dplyr::mutate(
          data_type = dplyr::if_else(row == 1 & col %in% 4:6, "character", data_type),
          character = dplyr::if_else(row == 1 & col %in% 4:6, "*", character),
          numeric = dplyr::if_else(row == 1 & col %in% 4:6, NA, numeric)
        ),
      # Date data
      tibble::tibble(
        sheet = rep("Tab 1a", 16),
        row = rep(0, 16),
        col = 2:17,
        data_type = rep("character", 16),
        character =
          c(
            format(seq.Date(as.Date("2004-03-01"), as.Date("2007-06-01"), "1 quarter"), "%b %Y"),
            "Year End Jun 2006", "Year End Jun 2007" # Checking that year-ended removed
          ),
        date = NA
      ) |>
        dplyr::mutate(
          date = dplyr::if_else(col %in% 1:3, lubridate::my(character), NA),
          character = dplyr::if_else(col %in% 1:3, NA, character)
        )
    )

  sheet_1b <-
    sheet_1a |>
    dplyr::mutate(sheet = "A.1b")

  return(dplyr::bind_rows(sheet_1a, sheet_1b))
}

#' Generates the dummy horizontal dependency_names data
#'
#' @keywords internal
#'
dummy_horizontal_series_hierarchy <- function() {
  tibble::tribble(
    ~sheet, ~row, ~series_hierarchy, ~series,
    "Tab 1a", 1L, "example_1", "example_1",
    "Tab 1a", 2L, "example_1; example_2", "example_2",
    "A.1b", 1L, "example_1", "example_1",
    "A.1b", 2L, "example_1; example_2", "example_2"
  )
}

#' Generates the dummy horizontal formatting_data data
#'
#' @keywords internal
#'
dummy_horizontal_formatting_input <- function() {
  list(
    local = list(
      numFmt = rep("#;##0", 64),
      alignment = list(
        indent = rep(c(rep(0, 16), 1, rep(0, 31), 1))
      )
    )
  )
}

#' Generates the get_joined_pub_data output
#'
#' @keywords internal
#'
expected_get_joined_pub_data_test_output <- function() {
  sheet_1a <-
    tibble::tibble(
      date = rep(lubridate::rollforward(seq.Date(as.Date("2004-03-01"), as.Date("2007-06-01"), "1 quarter")), 2),
      sheet = rep("Tab 1a", 28),
      series_hierarchy = c(rep("example_1", 14), rep("example_1; example_2", 14)),
      series = c(rep("example_1", 14), rep("example_2", 14)),
      frequency = "Quarterly",
      unit = "$ million",
      value = c(1:2, rep(NA, 3), 6:14, 16:29)
    )

  sheet_1b <-
    sheet_1a |>
    dplyr::mutate(sheet = "A.1b")

  return(dplyr::bind_rows(sheet_1a, sheet_1b))
}



#' Generates the dummy horizontal test_output
#'
#' @keywords internal
#'
expected_horizontal_test_output <- function() {
  sheet_1a <-
    tibble::tibble(
    date = rep(lubridate::rollforward(seq.Date(as.Date("2004-03-01"), as.Date("2007-06-01"), "1 quarter")), 2),
    sheet = rep("Tab 1a", 28),
    series_hierarchy = c(rep("example_1", 14), rep("example_1; example_2", 14)),
    series = c(rep("example_1", 14), rep("example_2", 14)),
    frequency = "Quarterly",
    unit = "$ million",
    value = c(1:2, rep(NA, 3), 6:14, 16:29)
  )

  sheet_1b <-
    sheet_1a |>
    dplyr::mutate(sheet = "A.1b")

  return(dplyr::bind_rows(sheet_1a, sheet_1b))
}

#' Generates data for iterate_series_hierarchy() and adjust_indent() tests
#'
#' @keywords internal
#'
iterate_series_hierarchy_test_data <- function() {
  tibble::tribble(
    ~character, ~indent, ~which_identifier, ~expected_output,
    "Impaired facilities", 0, 0, "Impaired facilities",
    "Non-accrual items with provisions", 1, 0, "Impaired facilities; Non-accrual items with provisions",
    "Non-accrual items without provisions", 1, 0, "Impaired facilities; Non-accrual items without provisions",
    "Restructured items with provisions", 1, 0, "Impaired facilities; Restructured items with provisions",
    "Restructured items without provisions", 1, 0, "Impaired facilities; Restructured items without provisions",
    "Other real estate owned", 1, 0, "Impaired facilities; Other real estate owned",
    "Other assets acquired through security enforcement", 1, 0, "Impaired facilities; Other assets acquired through security enforcement",
    "Total Impaired facilities", 0, 0, "Total Impaired facilities",
    "of which: Facilities in Australia", 0, 1, "Total Impaired facilities; of which: Facilities in Australia",
    "Provisions held", 0, 0, "Provisions held",
    "Specific provisions", 1, 0, "Provisions held; Specific provisions",
    "Security held", 1, 0, "Provisions held; Security held",
    "Total provisions held", 0, 0, "Total provisions held",
    "of which: Facilities in Australia", 0, 1, "Total provisions held; of which: Facilities in Australia",
    "Past due items", 0, 0, "Past due items",
    "of which: Facilities in Australia", 0, 1, "Past due items; of which: Facilities in Australia",
    "Interest income", 0, 0, "Interest income",
    "of which:", 0, 1, "Interest income; of which:",
    "Cash and liquid assets", 1, 0, "Interest income; of which: Cash and liquid assets",
    "Loans and advances", 1, 0, "Interest income; of which: Loans and advances",
    "Housing loans", 2, 0, "Interest income; of which: Loans and advances; Housing loans",
    "Term loans", 2, 0, "Interest income; of which: Loans and advances; Term loans",
    "Other", 2, 0, "Interest income; of which: Loans and advances; Other",
    "Other interest earning assets", 1, 0, "Interest income; of which: Other interest earning assets",
    "Credit exposures", 0, 0, "Credit exposures",
    "Non-performing", 1, 0, "Credit exposures; Non-performing",
    "Well-secured", 2, 0, "Credit exposures; Non-performing; Well-secured",
    "Not well-secured or unsecured", 2, 0, "Credit exposures; Non-performing; Not well-secured or unsecured",
    "Of which: >=90 days past-due", 2, 1, "Credit exposures; Non-performing; Not well-secured or unsecured; Of which: >=90 days past-due",
    "Well-secured", 3, 0, "Credit exposures; Non-performing; Not well-secured or unsecured; Of which: >=90 days past-due; Well-secured",
    "Not well-secured or unsecured", 3, 0, "Credit exposures; Non-performing; Not well-secured or unsecured; Of which: >=90 days past-due; Not well-secured or unsecured",
    "Of which: Facilities in Australia", 2, 0, "Credit exposures; Non-performing; Of which: Facilities in Australia",
    "Restructured", 1, 0, "Credit exposures; Restructured"
  )
}
