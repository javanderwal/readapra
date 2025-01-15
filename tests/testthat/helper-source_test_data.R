convert_test_data_date <- function(data, date_col) {
  col <- rlang::ensym(date_col)
  data[[col]] <-
    as.Date(
      lubridate::parse_date_time(
        x = data[[col]],
        orders = c("ymd", "dmy")
      )
    )
  return(data)
}

vertical_data_input <- function() {
  convert_test_data_date(
    readr::read_csv(
      test_path("test_data", "vertical_data_input.csv"),
      show_col_types = FALSE
    ),
    date
  )
}

vertical_formatting_input <- function() {
  list(local = list(numFmt = c("General", "mm-dd-yy", "0.0%", "General")))
}

vertical_data_output <- function() {
  convert_test_data_date(
    readr::read_csv(
      test_path("test_data", "vertical_data_output.csv"),
      show_col_types = FALSE
    ),
    date
  )
}

data_below_top_row_input <- function() {
  convert_test_data_date(
    readr::read_csv(
      test_path("test_data", "data_below_top_row_input.csv"),
      show_col_types = FALSE
    ),
    date
  )
}

clean_col_names_output <- function() {
  convert_test_data_date(
    readr::read_csv(
      test_path("test_data", "clean_col_names_output.csv"),
      show_col_types = FALSE
    ),
    date
  )
}

clean_col_names_input <- function() {
  data <-
    convert_test_data_date(
      readr::read_csv(
        test_path("test_data", "clean_col_names_input.csv"),
        show_col_types = FALSE,
      ),
      Period_1
    )
  data$series <-
    dplyr::if_else(
      data$series == "Field: \\r\\n Very messy regex_17",
      "Field: \r\n Very messy regex_17",
      data$series
    )
  data
}

existing_cols_input <- function() 1:8

row_names_input <- function() {
  tibble::tribble(
    ~col, ~data_type, ~character,
    1L, "character", "Period",
    2L, "character", "ABN",
    3L, "character", "Entity",
    4L, "character", "Field 1",
    5L, "character", "Field 2",
    6L, "character", "Field 3",
    7L, "character", "Field 4",
    8L, "character", "Field 5"
  )
}

restructure_as_in_xlsx_output <- function() {
  convert_test_data_date(
    readr::read_csv(
      test_path("test_data", "restructure_as_in_xlsx_output.csv"),
      show_col_types = FALSE
    ),
    Period_1
  )
}

get_extra_meta_data_output <- function() {
  tibble::tribble(
    ~series, ~col, ~unit, ~statistics_publication_name, ~frequency,
    "Period", 1L, "$ million", "Test name", "Test frequency",
    "ABN", 2L, "$ million", "Test name", "Test frequency",
    "Entity", 3L, "$ million", "Test name", "Test frequency",
    "Field 1", 4L, "Percent", "Test name", "Test frequency",
    "Field 2", 5L, "$ million", "Test name", "Test frequency",
    "Field 3", 6L, "$ million", "Test name", "Test frequency",
    "Field 4", 7L, "$ million", "Test name", "Test frequency",
    "Field 5", 8L, "Percent", "Test name", "Test frequency"
  )
}

iterate_series_hierarchy_input <- function() {
  readr::read_csv(test_path("test_data", "iterate_series_hierarchy_input.csv"),
    show_col_types = FALSE
  )
}

tidyxl_data_input <- function() {
  readRDS(test_path("test_data", "horizontal_test_data.rds"))$tidyxl_data
}

formatting_data_input <- function() {
  readRDS(test_path("test_data", "horizontal_test_data.rds"))$formatting_data
}

get_series_hierarchy_output <- function() {
  readr::read_csv(test_path("test_data", "get_series_hierarchy_output.csv"),
    show_col_types = FALSE
  )
}

format_horizontal_data_table_output <- function() {
  convert_test_data_date(
    readr::read_csv(
      test_path("test_data", "format_horizontal_data_table_output.csv"),
      show_col_types = FALSE
    ),
    date
  )
}

format_horizontal_data_key_stats_output <- function() {
  convert_test_data_date(
    readr::read_csv(
      test_path("test_data", "format_horizontal_data_key_stats_output.csv"),
      show_col_types = FALSE
    ),
    date
  )
}

get_horizontal_data_dates_output <- function() {
  convert_test_data_date(
    readr::read_csv(
      test_path("test_data", "get_horizontal_data_dates_output.csv"),
      show_col_types = FALSE
    ),
    date
  )
}

qadip_key_stats_names_series_hierarchy_input <- function() {
  readr::read_csv(
    test_path("test_data", "qadip_key_stats_names_series_hierarchy_input.csv"),
    show_col_types = FALSE
  )
}

qadip_key_stats_names_output <- function() {
  readr::read_csv(
    test_path("test_data", "qadip_key_stats_names_output.csv"),
    show_col_types = FALSE
  )
}

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
