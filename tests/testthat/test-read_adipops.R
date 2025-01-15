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
        frequency = "character",
        unit = "character",
        value = "numeric"
      )
    )
  })
})

test_that("convert_adipops_units() behaves as expected", {
  input_data <-
    tibble::tibble(
      stat_pub = "ADIPOPS",
      date = as.Date("2024-12-31"),
      test_field = paste("field", sort(rep(1:4, 3))),
      series = rep(c("Latitude", "Longitude", "Number"), 4),
      unit = rep(c("$ million", "$ million", "No."), 4),
      value = rnorm(12)
    )

  output_data <- input_data
  output_data$unit <- rep(c("Latitude", "Longitude", "No."), 4)

  expect_equal(
    convert_adipops_units(input_data),
    output_data
  )
})
