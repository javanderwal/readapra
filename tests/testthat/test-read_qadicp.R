test_that("read_qadicp() real download", {
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()

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
        risk_metric_category = "character",
        series = "character",
        frequency = "character",
        unit = "character",
        value = "numeric"
      )
    )
  })
})

test_that("add_qadicp_regulatory_category() behaves as expected", {
  set.seed(123)
  qadicp_input_data <-
    tibble::tibble(
      date = as.Date("2024-12-31"),
      abn = 1111111,
      entity = "Entity A",
      series = qadicp_risk_metric_category$series,
      value = rnorm(17),
      col = 201:217
    )

  qadicp_output_data <-
    dplyr::mutate(
      .data = qadicp_input_data,
      risk_metric_category = qadicp_risk_metric_category$risk_metric_category,
      .before = series
    )

  # Standard test
  expect_equal(
    add_qadicp_regulatory_category(qadicp_input_data),
    qadicp_output_data
  )

  # Warning test
  qadicp_input_data$series <-
    dplyr::if_else(
      condition = qadicp_input_data$series == "Total Tier 1 capital",
      true = "Total T1 capital",
      false = qadicp_input_data$series
    )

  expect_warning(
    add_qadicp_regulatory_category(qadicp_input_data),
    class = "readapra_warning_nas_in_qadicp_risk_metric_category"
  )
})
