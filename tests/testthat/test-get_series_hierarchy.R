test_that("get_series_hierarchy()", {
  test_input <-
    get_series_hierarchy(
      tidyxl_data = tidyxl_data_input(),
      formatting_data = formatting_data_input(),
      sheet_str_detect = "Tab|^A\\.|Key Stats"
    )

  expect_equal(test_input, get_series_hierarchy_output())
})

test_that("iterate_series_hierarchy()", {
  test_data <- iterate_series_hierarchy_input()

  expect_equal(
    iterate_series_hierarchy(test_data)$series_hierarchy,
    test_data$expected_output
  )

  expect_equal(
    iterate_series_hierarchy(test_data)$sheet_details,
    rep("Dummy Sheet Details", 34)
  )
})

test_that("adjust_indent()", {
  test_data <- iterate_series_hierarchy_input()

  expected_output <-
    dplyr::mutate(
      .data = test_data,
      adjusted_indent =
        c(
          0L, 0L, 1L, 1L, 1L, 1L, 1L, 1L, 0L, 1L,
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
