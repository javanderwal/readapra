test_that("download_apra()", {
  local_mocked_bindings(polite_bow = function(...) "bow_object")
  local_mocked_bindings(sys_sleep_wrapper = function(...) NULL)

  test_url_strings <-
    c(
      "https://www.apra.gov.au/sites/default/files/2024-12/20241212%20-%20Quarterly%20authorised%20deposit-taking%20institution%20performance%20statistics%20-%20September%202004%20to%20September%202024.xlsx",
      "https://www.apra.gov.au/sites/default/files/2024-12/20241212%20-%20Authorised%20deposit-taking%20institution%20centralised%20publication%20-%20March%202013%20to%20September%202024.xlsx",
      "https://www.apra.gov.au/sites/default/files/2024-12/20241212%20-%20Quarterly%20authorised%20deposit-taking%20institution%20property%20exposures%20statistics%20-%20September%202004%20to%20September%202024.xlsx",
      "https://www.apra.gov.au/sites/default/files/2020-03/Quarterly%20authorised%20deposit-taking%20institution%20property%20exposures%20-%20Historical%20data.xlsx"
    )

  # Testing working case
  local_mocked_bindings(safely_download_file = function(...) list(result = "returned_file_path", error = NULL))
  local_mocked_bindings(scrape_urls = function(...) test_url_strings)

  expect_equal(
    download_apra(
      publication = "qadips",
      cur_hist = "current",
      backup_match = "performance",
      backup_remove = NULL
    ),
    "returned_file_path"
  )

  # Testing could not download case
  local_mocked_bindings(safely_download_file = function(...) list(result = NULL, error = "returned_error"))
  local_mocked_bindings(get_http_status = function(...) "Client error: (404) Not Found")

  expect_error(
    download_apra(
      publication = "qadips",
      cur_hist = "current",
      backup_match = "performance",
      backup_remove = NULL
    ),
    class = "read_apra_error_could_not_download"
  )

  # Testing no performance string
  local_mocked_bindings(scrape_urls = function(...) test_url_strings[-1])

  expect_error(
    download_apra(
      publication = "qadips",
      cur_hist = "current",
      backup_match = "performance",
      backup_remove = NULL
    ),
    class = "read_apra_error_back_url_no_matches"
  )

  # Testing duplicate performance string
  local_mocked_bindings(scrape_urls = function(...) c(test_url_strings, test_url_strings[1]))

  expect_error(
    download_apra(
      publication = "qadips",
      cur_hist = "current",
      backup_match = "performance",
      backup_remove = NULL
    ),
    class = "read_apra_error_back_url_multiple_matches"
  )
})

test_that("backup_link_identifier()", {
  example_urls <-
    c(
      "https://www.apra.gov.au/sites/default/files/2024-12/20241212%20-%20Quarterly%20authorised%20deposit-taking%20institution%20performance-September%202004%20to%20September%202024.xlsx",
      "https://www.apra.gov.au/sites/default/files/2024-12/20241212%20-%20Authorised%20deposit-taking%20institution%20centralised%20publication%20-%20March%202013%20to%20September%202024.xlsx",
      "https://www.apra.gov.au/sites/default/files/2024-12/20241212%20-%20Quarterly%20authorised%20deposit-taking%20institution%20property%20exposures%20statistics%20-%20September%202004%20to%20September%202024.xlsx",
      "https://www.apra.gov.au/sites/default/files/2020-03/Quarterly%20authorised%20deposit-taking%20institution%20property%20exposures%20-%20Historical%20data.xlsx"
    )

  # Correctly selecting the QADIP
  expect_equal(
    backup_link_identifier(example_urls, "performance"), example_urls[1]
  )

  # Correctly selecting the current QPEX
  expect_equal(
    backup_link_identifier(example_urls, "property|exposures", "historic"), example_urls[3]
  )

  # Correctly selecting the historic QPEX
  expect_equal(
    backup_link_identifier(example_urls, "property|exposures|historic"), example_urls[4]
  )

  # Not identifying any matches
  expect_error(
    backup_link_identifier(example_urls, "not_found"),
    class = "read_apra_error_back_url_no_matches"
  )

  # Multiple evenly ranked matches
  expect_error(
    backup_link_identifier(example_urls, "quarterly"),
    class = "read_apra_error_back_url_multiple_matches"
  )
})
