test_that("format_vertical_data() behaves as expected", {

  # Standard case
  expect_equal(
    format_vertical_data(
      tidyxl_data = vertical_data_input(),
      formatting_data = vertical_formatting_input(),
      stat_pub_name = "Test"),
    vertical_data_output()
  )

  # Case with col
  expect_equal(
    format_vertical_data(
      tidyxl_data = vertical_data_input(),
      formatting_data = vertical_formatting_input(),
      drop_col = FALSE,
      stat_pub_name = "Test"
      ),
    dplyr::mutate(.data = vertical_data_output(), col = rep(4:8, 6))
  )
})

test_that("attempt_format_vertical_data() behaves as expected", {

  # Standard case
  expect_equal(
    attempt_format_vertical_data(
      tidyxl_data = vertical_data_input(),
      formatting_data = vertical_formatting_input(),
      stat_pub_name = "Test"
      ),
    vertical_data_output()
  )

  # Case with col
  expect_equal(
    attempt_format_vertical_data(
      tidyxl_data = vertical_data_input(),
      formatting_data = vertical_formatting_input(),
      drop_col = FALSE,
      stat_pub_name = "Test"
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
      stat_pub_name = "Test"
    ),
    regexp = "The .xlsx file was in an unrecognised structure and could not be imported.",
    class = "readapra_error_vertical_data_unreadable"
  )
})


