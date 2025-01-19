test_that("read_madis() real download", {
  skip_if_offline()
  skip_on_cran()

  with_tempdir({
    current_madis_path <- download_apra("madis", "current", quiet = TRUE)
    current_madis <- read_madis(current_madis_path, "current")
    historic_madis_path <- download_apra("madis", "historic", quiet = TRUE)
    historic_madis <- read_madis(historic_madis_path, "historic")
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
      frequency = "character",
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
      frequency = "character",
      unit = "character",
      value = "numeric"
    )
  )
})

test_that("add_madis_balance_sheet() behaves as expected", {
  # MADIS current testing ---------------------------------------------------
  set.seed(123)
  madis_current_input_data <-
    tibble::tibble(
      date = as.Date("2024-12-31"),
      abn = 1111111,
      entity = "Entity A",
      series = madis_current_balance_sheet$series,
      value = rnorm(27),
      col = 201:227
    )

  madis_current_output_data <-
    dplyr::mutate(
      .data = madis_current_input_data,
      balance_sheet_category = madis_current_balance_sheet$balance_sheet_category,
      .before = series
    )

  # Standard test
  expect_equal(
    add_madis_balance_sheet(madis_current_input_data, "current"),
    madis_current_output_data
  )

  # Warning test
  madis_current_input_data$series <-
    dplyr::if_else(
      condition = madis_current_input_data$series == "Trading securities",
      true = "Trade Securities",
      false = madis_current_input_data$series
    )

  expect_warning(
    add_madis_balance_sheet(madis_current_input_data, "current"),
    class = "readapra_warning_nas_in_madis_balance_sheet_category"
  )

  # MADIS historic testing --------------------------------------------------
  set.seed(123)
  madis_historic_input_data <-
    tibble::tibble(
      date = as.Date("2024-12-31"),
      abn = 1111111,
      entity = "Entity A",
      series = madis_historic_balance_sheet$series,
      value = rnorm(35),
      col = 201:235
    )

  madis_historic_output_data <-
    dplyr::mutate(
      .data = madis_historic_input_data,
      balance_sheet_category = madis_historic_balance_sheet$balance_sheet_category,
      .before = series
    )

  # Standard test
  expect_equal(
    add_madis_balance_sheet(madis_historic_input_data, "historic"),
    madis_historic_output_data
  )

  # Warning test
  madis_historic_input_data$series <-
    dplyr::if_else(
      condition = madis_historic_input_data$series == "Trading securities",
      true = "Trade Securities",
      false = madis_historic_input_data$series
    )

  expect_warning(
    add_madis_balance_sheet(madis_historic_input_data, "historic"),
    class = "readapra_warning_nas_in_madis_balance_sheet_category"
  )
})
