test_that("format_horizontal_data() behaves as expected", {
  tidyxl_data_input <- tidyxl_data_input()
  formatting_data_input <- formatting_data_input()

  # Testing table data
  output_table <-
    format_horizontal_data(
      tidyxl_data = tidyxl_data_input,
      formatting_data = formatting_data_input,
      stat_pub_name = "Test Pub",
      sheet_str_detect = "Tab",
      frequency = "Test Quarter"
    )

  expect_equal(output_table, format_horizontal_data_table_output())

  # Testing key stats data
  output_key_stats <-
    format_horizontal_data(
      tidyxl_data = tidyxl_data_input,
      formatting_data = formatting_data_input,
      stat_pub_name = "Test Pub",
      sheet_str_detect = "Key",
      frequency = "Test Quarter",
      series_hierarchy_fn = qadip_key_stats_names,
      series_hierarchy_args = list(tidyxl_data = tidyxl_data_input)
    )

  expect_equal(output_key_stats, format_horizontal_data_key_stats_output())
})

test_that("get_horizontal_data_dates() behaves as expected", {
  expect_equal(
    get_horizontal_data_dates(tidyxl_data_input(), "Tab"),
    get_horizontal_data_dates_output()
  )
})

test_that("attempt_format_horizontal_data() behaves as expected", {
  tidyxl_data_input <- tidyxl_data_input()
  formatting_data_input <- formatting_data_input()

  # Testing table data
  output_table <-
    attempt_format_horizontal_data(
      tidyxl_data = tidyxl_data_input,
      formatting_data = formatting_data_input,
      stat_pub_name = "Test Pub",
      sheet_str_detect = "Tab",
      frequency = "Test Quarter",
    )

  expect_equal(output_table, format_horizontal_data_table_output())

  # Testing key stats data
  output_key_stats <-
    attempt_format_horizontal_data(
      tidyxl_data = tidyxl_data_input,
      formatting_data = formatting_data_input,
      stat_pub_name = "Test Pub",
      sheet_str_detect = "Key",
      frequency = "Test Quarter",
      series_hierarchy_fn = qadip_key_stats_names,
      series_hierarchy_args = list(tidyxl_data = tidyxl_data_input)
    )

  expect_equal(output_key_stats, format_horizontal_data_key_stats_output())

  # Mocking for error/warning tests
  local_mocked_bindings(safe_format_horizontal_data = function(...) {
    list(result = NULL, error = "test_error")
  })

  # Testing error
  expect_error(
    attempt_format_horizontal_data(
      tidyxl_data = tidyxl_data_input,
      formatting_data = formatting_data_input,
      stat_pub_name = "Test Pub",
      sheet_str_detect = "Tab",
      frequency = "Test Quarter",
      error_or_warning = "error",
      message = "A test error message"
    ),
    regexp = "A test error message",
    class = "readapra_error_horizontal_data_unreadable"
  )

  # Testing warning
  expect_warning(
    attempt_format_horizontal_data(
      tidyxl_data = tidyxl_data_input,
      formatting_data = formatting_data_input,
      stat_pub_name = "Test Pub",
      sheet_str_detect = "Key",
      frequency = "Test Quarter",
      error_or_warning = "warning",
      message = "A test warning message"
    ),
    regexp = "A test warning message",
    class = "readapra_warning_horizontal_data_unreadable"
  )
})
