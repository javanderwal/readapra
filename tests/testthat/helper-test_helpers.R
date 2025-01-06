#' Generate the dummy horizontal tidyxl_input data
#'
#' @keywords internal
#'
dummy_horizontal_tidyxl_input <- function() {
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
}

#' Generates the dummy horizontal dependency_names data
#'
#' @keywords internal
#'
dummy_horizontal_dependency_input <- function() {
  tibble::tibble(
    sheet = rep("Tab 1a", 2),
    row = c(1, 2),
    series = c("example_1", "example_2"),
    series_dependency = c("SD_example_1", "SD_example_2")
  )
}

#' Generates the dummy horizontal formatting_data data
#'
#' @keywords internal
#'
dummy_horizontal_formatting_input <- function() {
  list(
    local = list(
      numFmt = rep("#;##0", 32),
      alignment = list(
        indent = rep(0, 48)
      )
    )
  )
}

#' Generates the dummy horizontal test_output
#'
#' @keywords internal
#'
expected_horizontal_test_output <- function() {
  tibble::tibble(
    date = rep(lubridate::rollforward(seq.Date(as.Date("2004-03-01"), as.Date("2007-06-01"), "1 quarter")),2),
    sheet = rep("Tab 1a", 28),
    series_dependency = c(rep("SD_example_1", 14), rep("SD_example_2", 14)),
    series = c(rep("example_1",14), rep("example_2",14)),
    unit = "#;##0",
    value = c(1:2, rep(NA, 3), 6:14, 16:29)
  )
}



