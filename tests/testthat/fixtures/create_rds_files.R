# Data for read_tidyxl_data() works as expected ---------------------------
test_xlsx <- create_test_xlsx_file()

expected_outputs <-
  list(
    data = tidyxl::xlsx_cells(test_xlsx),
    formatting = tidyxl::xlsx_formats(test_xlsx)
  )

saveRDS(
  object = expected_outputs,
  file = "~/R/readapra/tests/testthat/fixtures/read_tidyxl_data.rds"
)
