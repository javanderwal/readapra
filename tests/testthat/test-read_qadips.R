test_that("read_qadips() behaves as expected", {
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()

  with_tempdir({
    qadips <- read_qadips(quiet = TRUE)
    expect_s3_class(qadips, "tbl_df")
    expect_equal(
      purrr::map(qadips, class),
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
})
