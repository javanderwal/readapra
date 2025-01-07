test_that("attempt_horizontal_tab_data()", {
  tidyxl_input <- dummy_horizontal_tidyxl_input()
  formatting_input <- dummy_horizontal_formatting_input()

  test_output <-
    attempt_horizontal_tab_data(tidyxl_data = tidyxl_input, formatting_data = formatting_input) |>
    dplyr::mutate(series_dependency = paste0("SD_", series_dependency))

  expected_output <- expected_horizontal_test_output()

  expect_equal(test_output, expected_output)
})
