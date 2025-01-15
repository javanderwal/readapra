test_that("format_vertical_data() behaves as expected", {

  # Standard case
  expect_equal(
    format_vertical_data(
      tidyxl_data = vertical_data_input(),
      formatting_data = vertical_formatting_input(),
      stat_pub_name = "Test",
      frequency = "Test frequency"
      ),
    vertical_data_output()
  )

  # Case with col
  expect_equal(
    format_vertical_data(
      tidyxl_data = vertical_data_input(),
      formatting_data = vertical_formatting_input(),
      drop_col = FALSE,
      stat_pub_name = "Test",
      frequency = "Test frequency"
      ),
    dplyr::mutate(.data = vertical_data_output(), col = rep(4:8, 6))
  )
})

test_that("get_col_names() behaves as expected", {
  expect_equal(
    get_col_names(vertical_data_input(), 1:8)$character,
    c("Period", "ABN", "Entity", "Field 1", "Field 2", "Field 3", "Field 4", "Field 5")
  )

  expect_error(
    get_col_names(vertical_data_input(), 0),
    regexp = "Could not extract row names from the xlsx file.",
    class = "readapra_error_could_not_get_row_names"
  )
})

test_that("restructure_as_in_xlsx() behaves as expected", {
  expect_equal(
    restructure_as_in_xlsx(
      existing_cols = existing_cols_input(),
      row_names = row_names_input(),
      data_below_top_row = data_below_top_row_input()
    ),
    restructure_as_in_xlsx_output()
  )
})

test_that("clean_col_names() behaves as expected", {
  expect_equal(
    clean_col_names(pivoted_data = clean_col_names_input()),
    clean_col_names_output()
  )
})

test_that("extra_meta_data() behaves as expected", {
  expect_equal(
    get_extra_meta_data(
      stat_pub_name = "Test name",
      frequency = "Test frequency",
      row_names = row_names_input(),
      existing_cols = existing_cols_input(),
      data_below_top_row = data_below_top_row_input(),
      formatting_data = vertical_formatting_input()
    ),
    get_extra_meta_data_output()
  )
})

test_that("attempt_format_vertical_data() behaves as expected", {

  # Standard case
  expect_equal(
    attempt_format_vertical_data(
      tidyxl_data = vertical_data_input(),
      formatting_data = vertical_formatting_input(),
      stat_pub_name = "Test",
      frequency = "Test frequency"
    ),
    vertical_data_output()
  )

  # Case with col
  expect_equal(
    attempt_format_vertical_data(
      tidyxl_data = vertical_data_input(),
      formatting_data = vertical_formatting_input(),
      drop_col = FALSE,
      stat_pub_name = "Test",
      frequency = "Test frequency"
    ),
    dplyr::mutate(.data = vertical_data_output(), col = rep(4:8, 6))
  )

  # Error in safe_format_vertical_data
  local_mocked_bindings(
    safe_format_vertical_data = function(...) {
      list(result = NULL, error = "Error")
    }
  )

  expect_error(
    attempt_format_vertical_data(
      tidyxl_data = vertical_data_input(),
      formatting_data = vertical_formatting_input(),
      stat_pub_name = "Test",
      frequency = "Test frequency"
    ),
    regexp = "The .xlsx file was in an unrecognised structure and could not be imported.",
    class = "readapra_error_vertical_data_unreadable"
  )
})
