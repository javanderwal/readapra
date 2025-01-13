test_that("read_adipops() real download", {
  skip_if_offline()
  skip_on_cran()

  with_tempdir({
    adipops <- read_adipops(quiet = TRUE)
    expect_s3_class(adipops, "tbl_df")
    expect_equal(
      purrr::map(adipops, class),
      list(
        statistics_publication_name = "character",
        date = "Date",
        sector = "character",
        abn = "numeric",
        institution_name = "character",
        service_channel_type = "character",
        states_and_territories = "character",
        sa4_name = "character",
        sa3_name = "character",
        sa2_name = "character",
        suburb_or_locality = "character",
        remoteness = "character",
        series = "character",
        unit = "character",
        value = "numeric"
      )
    )
  })
})
