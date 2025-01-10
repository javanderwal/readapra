test_that("qadip_data()", {
  input_data <-
    tibble::tibble(
      date = seq.Date(as.Date("2004-09-01"), as.Date("2024-06-30"), by = "1 quarter"),
      sheet = rep("Key Stats", 80),
      series_hierarchy = rep("Dummy hierarchy", 80),
      series = c(rep("Series_A", 20), rep("Series_B", 20), rep("Number Test", 20), rep("Series_D", 20)),
      unit = c(rep("Percent", 20), rep("$ million", 20), rep("No.", 20), rep("$ million", 20)),
      value = 1:80
    )

  output_data <-
    tibble::tibble(
      statistics_publication_name = "Quarterly Authorised Deposit-taking Institution Performance Statistics",
      date = seq.Date(as.Date("2004-09-01"), as.Date("2024-06-30"), by = "1 quarter"),
      sheet = rep("Key Stats", 80),
      sector = c(rep("Unknown", 40), rep("Dummy hierarchy", 20), rep("Unknown", 20)),
      series_hierarchy = rep("Dummy hierarchy", 80),
      series = c(rep("Series_A", 20), rep("Series_B", 20), rep("Number Test", 20), rep("Series_D", 20)),
      unit = c(rep("Percent", 20), rep("$ million", 20), rep("No.", 20), rep("$ million", 20)),
      value = 1:80
    )

  local_mocked_bindings(attempt_qadip_key_stats_data = function(...) input_data)
  local_mocked_bindings(attempt_horizontal_tab_data = function(...) NULL)

  expect_equal(qadip_data(), output_data)
})

test_that("qadip_key_stats_names() success", {
  series_hierarchy_input <-
    tibble::tibble(
      sheet = "Key Stats",
      row = seq(from = 1, by = 5, length.out = 21),
      series_hierarchy =
        c(
          "Key Statistics",
          rep(c("ADIs", "ADIs; Banks", "ADIs; Majors", "ADIs; All ADIs"), 2),
          "Key figures",
          "Net interest income ($m)", "Total assets ($m)", "Number of entities",
          "Key statistics (continued)", "Key figures (continued)",
          "Net interest income ($m)", "Total assets ($m)", "Number of entities",
          "Net interest income ($m)", "Total assets ($m)", "Number of entities"
        ),
      series =
        c(
          "Key Statistics",
          rep(c("ADIs", "Banks", "Majors", "All ADIs"), 2),
          "Key figures",
          "Net interest income ($m)", "Total assets ($m)", "Number of entities",
          "Key statistics (continued)", "Key figures (continued)",
          "Net interest income ($m)", "Total assets ($m)", "Number of entities",
          "Net interest income ($m)", "Total assets ($m)", "Number of entities"
        )
    )

  tidyxl_input <-
    tibble::tibble(
      row = seq(from = 1, by = 5, length.out = 21),
      character = series_hierarchy_input$series
    )

  test_output <-
    qadip_key_stats_names(
      tidyxl_data = tidyxl_input,
      series_hierarchy_data = series_hierarchy_input
    )

  expected_output <-
    tibble::tibble(
      sheet = "Key Stats",
      row = seq(from = 1, by = 5, length.out = 21),
      series_hierarchy =
        c(
          NA, NA, "Assets; ADIs; Banks", "Assets; ADIs; Majors",
          "Assets; ADIs; All ADIs", "Number of entities; ADIs",
          "Number of entities; ADIs; Banks", "Number of entities; ADIs; Majors",
          "Number of entities; ADIs; All ADIs",
          "ADIs (excludes 'other ADIs'); Key figures",
          "ADIs (excludes 'other ADIs'); Net interest income ($m)",
          "ADIs (excludes 'other ADIs'); Total assets ($m)",
          "ADIs (excludes 'other ADIs'); Number of entities",
          "ADIs (excludes 'other ADIs'); Key statistics (continued)",
          "ADIs (excludes 'other ADIs'); Key figures (continued)",
          "Banks; Net interest income ($m)", "Banks; Total assets ($m)",
          "Banks; Number of entities",
          "Credit unions and building societies; Net interest income ($m)",
          "Credit unions and building societies; Total assets ($m)",
          "Credit unions and building societies; Number of entities"
        ),
      series =
        c(
          rep(NA, 2),
          rep("Assets", 3),
          rep("Number of entities", 4),
          rep("ADIs (excludes 'other ADIs')", 6),
          rep("Banks", 3),
          rep("Credit unions and building societies", 3)
        )
    )

  expect_equal(test_output, expected_output)
})

test_that("qadip_key_stats_names() sheet change errors", {
  # Testing wrong number of cohort rows

  series_hierarchy_input_wrong_number_cohort_rows <-
    tibble::tibble(
      sheet = "Key Stats",
      row = seq(from = 1, by = 5, length.out = 25),
      series_hierarchy =
        c(
          "Key Statistics",
          rep(c("ADIs", "ADIs; Banks", "ADIs; Majors", "ADIs; All ADIs"), 3),
          "Key figures",
          "Net interest income ($m)", "Total assets ($m)", "Number of entities",
          "Key statistics (continued)", "Key figures (continued)",
          "Net interest income ($m)", "Total assets ($m)", "Number of entities",
          "Net interest income ($m)", "Total assets ($m)", "Number of entities"
        ),
      series =
        c(
          "Key Statistics",
          rep(c("ADIs", "Banks", "Majors", "All ADIs"), 3),
          "Net interest income ($m)", "Total assets ($m)", "Number of entities",
          "Key figures",
          "Key statistics (continued)", "Key figures (continued)",
          "Net interest income ($m)", "Total assets ($m)", "Number of entities",
          "Net interest income ($m)", "Total assets ($m)", "Number of entities"
        )
    )

  tidyxl_input_wrong_number_cohort_rows <-
    tibble::tibble(
      row = seq(from = 1, by = 5, length.out = 25),
      character = series_hierarchy_input_wrong_number_cohort_rows$series
    )

  expect_error(
    qadip_key_stats_names(
      tidyxl_data = tidyxl_input_wrong_number_cohort_rows,
      series_hierarchy_data = series_hierarchy_input_wrong_number_cohort_rows
    ),
    class = "read_apra_error_key_figures_or_cohort_rows_wrong"
  )

  # Testing missing key figures row

  series_hierarchy_input_missing_key_figures <-
    tibble::tibble(
      sheet = "Key Stats",
      row = seq(from = 1, by = 5, length.out = 20),
      series_hierarchy =
        c(
          "Key Statistics",
          rep(c("ADIs", "ADIs; Banks", "ADIs; Majors", "ADIs; All ADIs"), 2),
          "Net interest income ($m)", "Total assets ($m)", "Number of entities",
          "Key statistics (continued)", "Key figures (continued)",
          "Net interest income ($m)", "Total assets ($m)", "Number of entities",
          "Net interest income ($m)", "Total assets ($m)", "Number of entities"
        ),
      series =
        c(
          "Key Statistics",
          rep(c("ADIs", "Banks", "Majors", "All ADIs"), 2),
          "Net interest income ($m)", "Total assets ($m)", "Number of entities",
          "Key statistics (continued)", "Key figures (continued)",
          "Net interest income ($m)", "Total assets ($m)", "Number of entities",
          "Net interest income ($m)", "Total assets ($m)", "Number of entities"
        )
    )

  tidyxl_input_missing_key_figures <-
    tibble::tibble(
      row = seq(from = 1, by = 5, length.out = 20),
      character = series_hierarchy_input_missing_key_figures$series
    )

  expect_error(
    qadip_key_stats_names(
      tidyxl_data = tidyxl_input_missing_key_figures,
      series_hierarchy_data = series_hierarchy_input_missing_key_figures
    ),
    class = "read_apra_error_key_figures_or_cohort_rows_wrong"
  )

  # Testing missing summary statistics for a stat group (Cohort, i.e. Credit unions)

  series_hierarchy_input_wrong_number_summary_stat_groups <-
    tibble::tibble(
      sheet = "Key Stats",
      row = seq(from = 1, by = 5, length.out = 18),
      series_hierarchy =
        c(
          "Key Statistics",
          rep(c("ADIs", "ADIs; Banks", "ADIs; Majors", "ADIs; All ADIs"), 2),
          "Key figures",
          "Net interest income ($m)", "Total assets ($m)", "Number of entities",
          "Key statistics (continued)", "Key figures (continued)",
          "Net interest income ($m)", "Total assets ($m)", "Number of entities"
        ),
      series =
        c(
          "Key Statistics",
          rep(c("ADIs", "Banks", "Majors", "All ADIs"), 2),
          "Key figures",
          "Net interest income ($m)", "Total assets ($m)", "Number of entities",
          "Key statistics (continued)", "Key figures (continued)",
          "Net interest income ($m)", "Total assets ($m)", "Number of entities"
        )
    )

  tidyxl_input_wrong_number_summary_stat_groups <-
    tibble::tibble(
      row = seq(from = 1, by = 5, length.out = 18),
      character = series_hierarchy_input_wrong_number_summary_stat_groups$series
    )

  expect_error(
    qadip_key_stats_names(
      tidyxl_data = tidyxl_input_wrong_number_summary_stat_groups,
      series_hierarchy_data = series_hierarchy_input_wrong_number_summary_stat_groups
    ),
    class = "read_apra_error_wrong_number_summary_stat_groups"
  )
})

test_that("attempt_qadip_key_stats_data()", {
  local_mocked_bindings(
    safely_qadip_key_stats_data = function(...) {
      list(result = NULL, error = "dummy_error")
    }
  )

  expect_equal(
    suppressWarnings(attempt_qadip_key_stats_data("input_a", "input_b")),
    tibble::tibble()
  )

  expect_warning(
    attempt_qadip_key_stats_data("input_a", "input_b"),
    class = "read_apra_warning_key_stats_inaccessible"
  )
})
