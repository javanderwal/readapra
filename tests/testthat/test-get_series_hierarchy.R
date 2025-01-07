test_that("get_series_dependencies()", {
  tidyxl_input <-
    tibble::tribble(
      ~sheet, ~row, ~col, ~data_type, ~character, ~local_format_id, ~numeric,
      "Tab 1a", 1L, 1L, "character", "This", 1, 1,
      "Tab 1a", 2L, 1L, "character", "is", 2, 1,
      "Tab 1a", 3L, 1L, "character", "a", 3, 1,
      "Tab 1a", 4L, 1L, "character", "sentence", 4, 1,
      "Tab 1a", 5L, 1L, "character", "Alone", 5, 1,
      "Tab 1a", 6L, 1L, "character", "That", 6, 1,
      "Tab 1a", 7L, 1L, "character", "which", 7, 1,
      "Tab 1a", 8L, 1L, "character", "works poorly", 8, 1,
      "Tab 1a", 9L, 1L, "character", "works well", 9, 1,
      "Tab 1a", 5L, 1L, "character", "Alone2", 10, 1,
      "Tab 1b", 5L, 1L, "character", "Removed", 11, 1
    )

  formatting_input <-
    list(
      local = list(
        alignment = list(
          indent = c(0, 1, 2, 3, 0, 0, 0, 1, 1, 0, 0, 0)
        )
      )
    )

  test_input <-
    get_series_dependencies(
      tidyxl_data = tidyxl_input,
      formatting_data = formatting_input,
      sheet_str_detect = "Tab 1a"
    )

  expected_output <-
    tibble::tribble(
      ~sheet, ~row, ~series_dependency, ~series,
      "Tab 1a", 1L, "This", "This",
      "Tab 1a", 2L, "This; is", "is",
      "Tab 1a", 3L, "This; is; a", "a",
      "Tab 1a", 4L, "This; is; a; sentence", "sentence",
      "Tab 1a", 5L, "Alone", "Alone",
      "Tab 1a", 6L, "That", "That",
      "Tab 1a", 7L, "which", "which",
      "Tab 1a", 8L, "That; which works poorly", "works poorly",
      "Tab 1a", 9L, "That; which works well", "works well",
      "Tab 1a", 5L, "Alone2", "Alone2"
    )

  expect_equal(test_input, expected_output)
})

test_that("iterate_series_dependencies()", {
  input_data <-
    tibble::tribble(
      ~sheet, ~row, ~col, ~data_type, ~character, ~indent, ~which_identifier,
      "Tab 1a", 1L, 1L, "character", "This", 0L, 0L,
      "Tab 1a", 2L, 1L, "character", "is", 1L, 0L,
      "Tab 1a", 3L, 1L, "character", "a", 2L, 0L,
      "Tab 1a", 4L, 1L, "character", "sentence", 3L, 0L,
      "Tab 1a", 5L, 1L, "character", "Alone", 0L, 0L,
      "Tab 1a", 6L, 1L, "character", "That", 0L, 0L,
      "Tab 1a", 7L, 1L, "character", "which", 0L, 1L,
      "Tab 1a", 8L, 1L, "character", "works poorly", 1L, 0L,
      "Tab 1a", 9L, 1L, "character", "works well", 1L, 0L,
      "Tab 1a", 5L, 1L, "character", "Alone2", 0L, 0L,
    )

  output_data <-
    dplyr::mutate(
      example_input,
      series_dependency = purrr::map_chr(
        seq_along(character),
        ~ iterate_series_dependencies(.x, character, indent, which_identifier)
      )
    )
    dplyr::pull(series_dependency)

  expected_output_data <-
    c(
      "This", "This; is", "This; is; a", "This; is; a; sentence", "Alone",
      "That", "which", "That; which works poorly", "That; which works well", "Alone2"
    )

  expect_equal(output_data, expected_output_data)
})

test_that("get_joined_pub_data()", {
  tidyxl_input <- dummy_horizontal_tidyxl_input()
  dependency_input <- dummy_horizontal_dependency_input()
  formatting_input <- dummy_horizontal_formatting_input()

  test_output <-
    get_joined_pub_data(
    tidyxl_data = tidyxl_input,
    dependency_names = dependency_input,
    formatting_data = formatting_input,
    sheet_str_detect = "Tab 1a"
  )

  expected_output <- expected_horizontal_test_output()

  expect_equal(test_output, expected_output)
})
