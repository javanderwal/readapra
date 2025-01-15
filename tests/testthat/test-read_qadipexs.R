test_that("read_qadipexs() behaves as expected", {
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()

  with_tempdir({
    current_qadipexs <- read_qadipexs(cur_hist = "current", quiet = TRUE)
    historic_qadipexs <- read_qadipexs(cur_hist = "historic", quiet = TRUE)
  })

  # Testing current QADIPEXS
  expect_s3_class(current_qadipexs, "tbl_df")
  expect_equal(
    purrr::map(current_qadipexs, class),
    list(
      statistics_publication_name = "character",
      date = "Date",
      sheet = "character",
      sheet_details = "character",
      series_hierarchy = "character",
      series = "character",
      frequency = "character",
      unit = "character",
      value = "numeric"
    )
  )

  # Testing historic QADIPEXS
  expect_s3_class(historic_qadipexs, "tbl_df")
  expect_equal(
    purrr::map(historic_qadipexs, class),
    list(
      statistics_publication_name = "character",
      date = "Date",
      sheet = "character",
      sheet_details = "character",
      series_hierarchy = "character",
      series = "character",
      frequency = "character",
      unit = "character",
      value = "numeric"
    )
  )
})
