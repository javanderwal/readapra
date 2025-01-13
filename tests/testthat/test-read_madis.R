test_that("read_madis() real download", {
  skip_if_offline()
  skip_on_cran()

  with_tempdir({
    current_madis <- read_madis(cur_hist = "current", quiet = TRUE)
    historic_madis <- read_madis(cur_hist = "historic", quiet = TRUE)
  })

  # Testing current MADIS
  expect_s3_class(current_madis, "tbl_df")
  expect_equal(
    purrr::map(current_madis, class),
    list(
      statistics_publication_name = "character",
      date = "Date",
      abn = "numeric",
      institution_name = "character",
      balance_sheet_category = "character",
      series = "character",
      unit = "character",
      value = "numeric"
    )
  )

  # Testing historic MADIS
  expect_s3_class(historic_madis, "tbl_df")
  expect_equal(
    purrr::map(historic_madis, class),
    list(
      statistics_publication_name = "character",
      date = "Date",
      abn = "numeric",
      institution_name = "character",
      balance_sheet_category = "character",
      series = "character",
      unit = "character",
      value = "numeric"
    )
  )
})
