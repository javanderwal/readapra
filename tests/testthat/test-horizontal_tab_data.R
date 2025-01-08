test_that("horizontal_tab_data()", {

  # Testing the standard working case
  tidyxl_input <- dummy_horizontal_tidyxl_input()
  formatting_input <- dummy_horizontal_formatting_input()

  test_output <-
    horizontal_tab_data(
      tidyxl_data = tidyxl_input,
      formatting_data = formatting_input
      )

  expect_equal(test_output, expected_get_joined_pub_data_test_output())

  # Checking that the error throws as expected.
  local_mocked_bindings(safely_horizontal_tab_data = function(...)
    list(result = NULL, error = "Test error")
    )

  expect_error(
    attempt_horizontal_tab_data(tidyxl_input, formatting_input),
    class = "read_apra_error_horizontal_data_unreadable"
  )
})
