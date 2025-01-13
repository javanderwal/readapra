test_that("read_tidyxl_data() works as expected", {
  with_tempdir({
    temp_xlsx_file <- create_test_xlsx_file()
    extracted_data <- read_tidyxl_data(temp_xlsx_file)

    expect_error(
      read_tidyxl_data(temp_xlsx_file, "Sheet"),
      class = "readapra_error_multiple_sheets_selected"
    )
  })

    expect_equal(
      extracted_data,
      readRDS(test_path("fixtures", "read_tidyxl_data.rds"))$data
    )
})

test_that("read_tidyxl_formatting_data works as expected", {
  with_tempdir({
    temp_xlsx_file <- create_test_xlsx_file()
    extracted_formatting <- read_tidyxl_formatting_data(temp_xlsx_file)
  })
    expect_equal(
      extracted_formatting,
      readRDS(test_path("fixtures", "read_tidyxl_data.rds"))$formatting
    )
})
