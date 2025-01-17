test_that("check_valid_file_path() behaves as expected", {
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

test_that("is_arg_logical() behaves as expected", {
  expect_error(
    is_arg_logical("Test"),
    class = "readapra_error_arg_is_not_logical_length_one"
  )

  expect_no_error(is_arg_logical(TRUE))
  expect_no_error(is_arg_logical(FALSE))
})
