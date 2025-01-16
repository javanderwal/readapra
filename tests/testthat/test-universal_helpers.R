test_that("check_valid_file_path()", {
  expect_error(
    check_valid_file_path(file_path = 123),
    class = "read_apra_error_file_path_not_string"
  )

  expect_error(
    check_valid_file_path(file_path = c("a", "b")),
    class = "read_apra_error_file_path_not_length_one"
  )

  local_mocked_bindings(file_exists_wrapper = function(...) FALSE)

  expect_error(
    check_valid_file_path(file_path = "C:/Users/"),
    class = "read_apra_error_file_path_does_not_exist"
  )
})
