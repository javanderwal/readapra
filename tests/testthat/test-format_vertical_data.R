test_that("format_vertical_data() behaves as expected", {

  # Standard case
  expect_equal(
    format_vertical_data(
      tidyxl_data = vertical_data_input(),
      formatting_data = vertical_formatting_input(),
      stat_pub_name = "Test"),
    vertical_data_output()
  )

  # Case with col
  expect_equal(
    format_vertical_data(
      tidyxl_data = vertical_data_input(),
      formatting_data = vertical_formatting_input(),
      drop_col = FALSE,
      stat_pub_name = "Test"
      ),
    dplyr::mutate(.data = vertical_data_output(), col = rep(4:8, 6))
  )
})

test_that("get_col_names() behaves as expected", {
  expect_equal(
    get_col_names(vertical_data_input(), 1:8)$character,
    c("Period", "ABN", "Entity", "Field 1", "Field 2", "Field 3", "Field 4", "Field 5")
  )

  expect_error(
    get_col_names(vertical_data_input(), 0),
    regexp = "Could not extract row names from the xlsx file.",
    class = "readapra_error_could_not_get_row_names"
  )
})

# test_that("restructure_as_in_xlsx() behaves as expected", {
#   expect_equal(
#     get_col_names(vertical_data_input(), 1:8)$character,
#     c("Period", "ABN", "Entity", "Field 1", "Field 2", "Field 3", "Field 4", "Field 5")
#   )
#
#   expect_error(
#     get_col_names(vertical_data_input(), 0),
#     regexp = "Could not extract row names from the xlsx file.",
#     class = "readapra_error_could_not_get_row_names"
#   )
# })


