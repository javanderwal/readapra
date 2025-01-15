test_that("read_tidyxl_data() works as expected", {
  with_tempdir({
    temp_xlsx_file <- create_test_xlsx_file()

    expect_error(
      read_tidyxl_data(temp_xlsx_file, "Sheet"),
      class = "readapra_error_multiple_sheets_selected"
    )
  })
})
