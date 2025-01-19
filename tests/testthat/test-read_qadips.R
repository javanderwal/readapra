test_that("read_qadips() behaves as expected", {
  skip_if_offline()
  skip_on_cran()

  with_tempdir({
    qadips_path <- download_apra("qadips", "current", quiet = TRUE)
    qadips <- read_qadips(qadips_path, "current")
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
