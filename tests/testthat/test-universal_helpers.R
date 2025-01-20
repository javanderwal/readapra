test_that("check_valid_file_path() behaves as expected", {
  # Skip due to divergence across OS systems.
  skip_on_os(c("mac", "linux", "solaris"))

  wrapper_check_valid_file_path <- function(...) {
    check_valid_file_path(...)
  }

  # Invalid file_path
  expect_error(
    wrapper_check_valid_file_path("C:/Users/test_user/test_file.xlsx"),
    class = "read_apra_error_file_path_does_not_exist"
  )

  testthat::expect_snapshot(
    {
      wrapper_check_valid_file_path("C:/Users/test_user/test_file.xlsx")
    },
    error = TRUE
  )

  # Wrong object type
  expect_error(
    wrapper_check_valid_file_path(data.frame()),
    class = "readapra_error_input_arg_not_vector"
  )

  testthat::expect_snapshot(
    {
      wrapper_check_valid_file_path(data.frame())
    },
    error = TRUE
  )

  # Wrong vector type
  expect_error(
    wrapper_check_valid_file_path(1:5),
    class = "readapra_error_input_arg_not_character"
  )

  testthat::expect_snapshot(
    {
      wrapper_check_valid_file_path(1:5)
    },
    error = TRUE
  )

  # Wrong length
  expect_error(
    wrapper_check_valid_file_path(c("test_a", "test_b")),
    class = "readapra_error_input_arg_not_length_1"
  )

  testthat::expect_snapshot(
    {
      wrapper_check_valid_file_path(c("test_a", "test_b"))
    },
    error = TRUE
  )
})

test_that("check_logical_length_one() behaves as expected", {
  wrapper_check_logical_length_one <- function(...) {
    check_logical_length_one(...)
  }

  # Wrong object type
  expect_error(
    wrapper_check_logical_length_one(data.frame()),
    class = "readapra_error_input_arg_not_vector"
  )

  testthat::expect_snapshot(
    {
      wrapper_check_logical_length_one(data.frame())
    },
    error = TRUE
  )

  # Wrong vector type
  expect_error(
    wrapper_check_logical_length_one(1:5),
    class = "readapra_error_input_arg_not_logical"
  )

  testthat::expect_snapshot(
    {
      wrapper_check_logical_length_one(1:5)
    },
    error = TRUE
  )

  # Wrong length
  expect_error(
    wrapper_check_logical_length_one(c(TRUE, FALSE, TRUE)),
    class = "readapra_error_input_arg_not_length_1"
  )

  testthat::expect_snapshot(
    {
      wrapper_check_logical_length_one(c(TRUE, FALSE, TRUE))
    },
    error = TRUE
  )
})

test_that("check_character_length_one() behaves as expected", {
  wrapper_check_character_length_one <- function(...) {
    check_character_length_one(...)
  }

  # Wrong object type
  expect_error(
    wrapper_check_character_length_one(data.frame()),
    class = "readapra_error_input_arg_not_vector"
  )

  testthat::expect_snapshot(
    {
      wrapper_check_character_length_one(data.frame())
    },
    error = TRUE
  )

  # Wrong vector type
  expect_error(
    wrapper_check_character_length_one(1:5),
    class = "readapra_error_input_arg_not_character"
  )

  testthat::expect_snapshot(
    {
      wrapper_check_character_length_one(1:5)
    },
    error = TRUE
  )

  # Wrong length
  expect_error(
    wrapper_check_character_length_one(c("test_a", "test_b")),
    class = "readapra_error_input_arg_not_length_1"
  )

  testthat::expect_snapshot(
    {
      wrapper_check_character_length_one(c("test_a", "test_b"))
    },
    error = TRUE
  )
})

test_that("replace_columns_patterns() behaves as expected", {
  input_data <-
    tibble::tribble(
      ~col1, ~col2,
      "This is (a test) example a", "This is (a test) example a",
      "A dollar ($) sign example", "A dollar ($) sign example",
      "A percentage sign % example", "A percentage sign % example",
      "Some of example z", "Some of example z",
      "An unaltered value", "An unaltered value"
    )

  match_replacement_inputs <-
    tibble::tribble(
      ~match, ~replacement,
      "This is (a test) example a", "This is (a test) example",
      "A dollar ($) sign example", "A dollar sign example",
      "A percentage sign % example", "A percentage sign example",
      "Some of example z", "Some of example"
    )

  expected_output <-
    tibble::tribble(
      ~col1, ~col2,
      "This is (a test) example", "This is (a test) example",
      "A dollar sign example", "A dollar sign example",
      "A percentage sign example", "A percentage sign example",
      "Some of example", "Some of example",
      "An unaltered value", "An unaltered value"
    )

  expect_equal(
    replace_columns_patterns(
      data = input_data,
      cols = c("col1", "col2"),
      match = match_replacement_inputs$match,
      replace = match_replacement_inputs$replacement
    ),
    expected_output
  )
})
