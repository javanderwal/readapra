tidyxl_data <-
  tidyxl::xlsx_cells(test_path("test_data", "horizontal_test_data.xlsx"),
    include_blank_cells = FALSE
  )

formatting_data <-
  tidyxl::xlsx_formats(test_path("test_data", "horizontal_test_data.xlsx"))

formatting_data <- list(local = formatting_data$local)

saveRDS(
  object = tibble::lst(tidyxl_data, formatting_data),
  file = test_path("test_data", "horizontal_test_data.rds")
)
