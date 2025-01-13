# Data for read_tidyxl_data() works as expected ---------------------------
test_xlsx <- create_test_xlsx_file()

expected_outputs <-
  list(
    data = read_tidyxl_data(test_xlsx),
    formatting = read_tidyxl_formatting_data(test_xlsx)
  )

saveRDS(
  object = expected_outputs,
  file = "~/R/readapra/tests/testthat/fixtures/read_tidyxl_data.rds"
)
