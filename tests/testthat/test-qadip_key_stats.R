test_that("qadip_key_stats_names() behaves as expected", {
  input_data <-
    qadip_key_stats_names(
      qadip_key_stats_names_series_hierarchy_input(),
      tidyxl_data_input()
    )

  expect_equal(input_data, qadip_key_stats_names_output())
})
