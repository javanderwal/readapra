test_that("read_qadicp() real download", {
  skip_if_offline()
  skip_on_cran()

  with_tempdir({
    qadicp_data <- read_qadicp(quiet = TRUE)
    expect_s3_class(qadicp_data, "tbl_df")
    expect_equal(
      purrr::map(qadicp_data, class),
      list(
        statistics_publication_name = "character",
        date = "Date",
        abn = "numeric",
        entity = "character",
        entity_quarter_end = "Date",
        mutual_bank_y_n = "character",
        sector = "character",
        series_category = "character",
        series = "character",
        unit = "character",
        value = "numeric"
      )
    )
  })
})
