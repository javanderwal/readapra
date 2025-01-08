test_that("get_joined_pub_data()", {
  tidyxl_input <- dummy_horizontal_tidyxl_input()
  series_hierarchy_input <- dummy_horizontal_series_hierarchy()
  formatting_input <- dummy_horizontal_formatting_input()

  test_output <-
    get_joined_pub_data(
      tidyxl_data = tidyxl_input,
      series_hierarchy_data = series_hierarchy_input,
      formatting_data = formatting_input,
      sheet_str_detect = "Tab|^A\\."
    )

  expect_equal(test_output, expected_get_joined_pub_data_test_output())
})
