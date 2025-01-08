test_that("get_series_hierarchy()", {

  test_input <-
    get_series_hierarchy(
    tidyxl_data = dummy_horizontal_tidyxl_input(),
    formatting_data = dummy_horizontal_formatting_input(),
    sheet_str_detect = "Tab|^A\\."
    )
  expected_output <- dummy_horizontal_series_hierarchy()
  expect_equal(test_input, expected_output)
})

test_that("iterate_series_hierarchy()", {
  test_data <- iterate_series_hierarchy_test_data()

  expect_equal(
    iterate_series_hierarchy(test_data)$series_hierarchy,
    test_data$expected_output
  )
})

test_that("adjust_indent()", {
  test_data <- iterate_series_hierarchy_test_data()

  expected_output <-
    test_data |>
    dplyr::mutate(
      adjusted_indent =
        c(
          0L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 1L,
          0L, 1L, 1L, 0L, 1L, 0L, 1L, 0L, 1L,
          2L, 2L, 3L, 3L, 3L, 2L, 0L, 1L, 2L,
          2L, 3L, 4L, 4L, 2L, 1L
        )
    )

  expect_equal(
    adjust_indent(test_data),
    expected_output
  )
})
