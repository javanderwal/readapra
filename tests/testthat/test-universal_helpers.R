test_that("attempt_horizontal_tab_data()", {
  tidyxl_input <- dummy_horizontal_tidyxl_input()
  formatting_input <- dummy_horizontal_formatting_input()

  test_output <-
    attempt_horizontal_tab_data(tidyxl_data = tidyxl_input, formatting_data = formatting_input) |>
    dplyr::mutate(series_dependency = paste0("SD_", series_dependency))

  expected_output <- expected_horizontal_test_output()

  expect_equal(test_output, expected_output)
})

test_that("check_valid_file_path()", {

  expect_error(
    check_valid_file_path(file_path = 123),
    class ="read_apra_error_file_path_not_string"
  )

  expect_error(
    check_valid_file_path(file_path = c("a", "b")),
    class ="read_apra_error_file_path_not_length_one"
  )

  local_mocked_bindings(file_exists_wrapper = function(...) FALSE)

  expect_error(
    check_valid_file_path(file_path = "C:/Users/"),
    class = "read_apra_error_file_path_does_not_exist"
  )
})
