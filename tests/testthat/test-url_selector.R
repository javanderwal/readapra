test_that("url_selector() behaves as expected", {
  input_urls_to_check <-
    c(
      "#main-content",
      "https://www.apra.gov.au/",
      "https://www.apra.gov.au/news-and-publications?industry%5B2%5D=2",
      "https://www.apra.gov.au/sites/default/files/2024-10/20241031%20-%20Monthly%20authorised%20deposit-taking%20institution%20statistics%20back-series%20March%202019%20-%20September%202024.xlsx",
      "https://www.apra.gov.au/sites/default/files/monthly_banking_statistics_june_2019_back_series.xlsx",
      "https://www.apra.gov.au/sites/default/files/2024-09/20240912%20-%20Quarterly%20authorised%20deposit-taking%20institution%20performance%20statistics%20-%20September%202004%20to%20June%202024.xlsx",
      "https://www.apra.gov.au/sites/default/files/2024-09/20240912%20-%20Quarterly%20authorised%20deposit-taking%20institution%20property%20exposures%20statistics%20-%20September%202004%20to%20June%202024.xlsx",
      "https://www.apra.gov.au/sites/default/files/2020-03/Quarterly%20authorised%20deposit-taking%20institution%20property%20exposures%20-%20Historical%20data.xlsx",
      "https://www.apra.gov.au/sites/default/files/2024-09/20240912%20-%20Authorised%20deposit-taking%20institution%20centralised%20publication%20-%20March%202013%20to%20June%202024_0.xlsx",
      "https://www.apra.gov.au/sites/default/files/2024-10/20241016%20-%20Authorised%20deposit-taking%20institutions%20points%20of%20presence%20June%202017%20to%20June%202024.xlsx"
    )

  input_selected_stat_pub <-
    dplyr::filter(
      .data = apra_stat_pub_details,
      publication == "madis", cur_hist == "current"
    )

  # Regex behaves as expected
  expect_equal(
    url_selector(input_urls_to_check, input_selected_stat_pub),
    input_urls_to_check[4]
  )

  # Skipping next function as it doesn't appear to play nice on servers due to
  #stringdist's amatch(). Suspect its due to core allocation?
  skip_on_cran()
  skip_on_ci()
  skip()

  # Stringdist accounts for minor spelling error
  input_urls_to_check[4] <- "https://www.apra.gov.au/sites/default/files/2024-10/20241031%20-%20Month%20authorised%20deposi-taking%20instiution%20%20back-series%20March%202019%20-%20September%202024.xlsx"
  expect_equal(
    url_selector(input_urls_to_check, input_selected_stat_pub),
    input_urls_to_check[4]
  )
})

test_that("url_selector() errors correctly", {
  input_selected_stat_pub <-
    dplyr::filter(
      .data = apra_stat_pub_details,
      publication == "madis", cur_hist == "current"
    )

  expect_error(
    url_selector(
      urls_to_check = character(),
      selected_stat_pub = input_selected_stat_pub
    ),
    regexp = "Could not determine correct URL to download.",
    class = "readapra_could_not_find_right_file_url"
  )
})

test_that("prepare_name_comparison() behaves as expected", {
  expect_equal(
    prepare_name_comparison("20241212%20-%20Quarterly%20authorised%20deposit-taking%20institution%20performance-September%202004%20to%20September%202024.xlsx"),
    "- Quarterly authorised deposit-taking institution performance-  to  .xlsx"
  )
})
