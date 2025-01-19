test_that("download_apra() real file download", {
  skip_if_offline()
  skip_on_cran()

  with_tempdir({
    # Test correct QADIPS download
    expect_true(
      file.exists(
        download_apra(
          stat_pub = "qadips",
          cur_hist = "current",
          quiet = TRUE
        )
      )
    )

    # Test correct MADIS download
    expect_true(
      file.exists(
        download_apra(
          stat_pub = "madis",
          cur_hist = "historic",
          quiet = TRUE
        )
      )
    )
  })
})

test_that("download_apra() download error handling", {

  # Error if can't get connection
  local_mocked_bindings(
    get_http_status = function(...) {
      list(
        category = "Client error",
        reason = "Not Found",
        message = "Client error: (404) Not Found"
      )
    }
  )

  expect_error(
    download_apra(stat_pub = "qadips", cur_hist = "current"),
    regexp = "Could not scrape url to download from:",
    class = "readapra_error_http_status_error"
  )

  # Error if can't download
  local_mocked_bindings(
    get_http_status = function(...) {
      list(
        category = "Success",
        reason = "OK",
        message = "Success: (200) OK"
      )
    }
  )
  local_mocked_bindings(bow_wrapper = function(...) NULL)
  local_mocked_bindings(scrape_urls = function(...) NULL)
  local_mocked_bindings(url_selector = function(...) NULL)
  local_mocked_bindings(sys_sleep_wrapper = function(...) NULL)
  local_mocked_bindings(safe_polite_file_download = function(...) {
    list(result = NULL, error = "Error")
  })

  expect_error(
    download_apra(stat_pub = "qadips", cur_hist = "current"),
    regexp = "Could not download file from:",
    class = "readapra_error_could_not_download_file"
  )
})

test_that("scrape_urls() behaves as expected", {
  enable(quiet = TRUE)

  mock_html <-
    "
<html>
  <body>
    <a href='https://example.com/link1'>Link 1</a>
    <a href='https://example.com/link2'>Link 2</a>
  </body>
</html>
"

  to_return(
    .data = stub_request("get", "https://example.com"),
    body = mock_html,
    status = 200,
    headers = list("Content-Type" = "text/html; charset=utf-8")
  )

  wi_th(
    .data = stub_request("get", uri = "https://example.com/robots.txt"),
    headers = list(
      "Accept" = "application/json, text/xml, application/xml, */*",
      "user-agent" = "polite R package"
    )
  )

  session <- polite::bow("https://example.com")
  urls <- scrape_urls(session)

  expect_equal(urls, c("https://example.com/link1", "https://example.com/link2"))

  disable(quiet = TRUE)
})

test_that("check_http_status() behaves as expected", {
  enable(quiet = TRUE)

  to_return(.data = stub_request("get", "https://example.com/success"), status = 200)
  to_return(.data = stub_request("get", "https://example.com/notfound"), status = 404)
  to_return(.data = stub_request("get", "https://example.com/servererror"), status = 501)

  expect_no_error(check_http_status("https://example.com/success"))

  expect_error(
    check_http_status(
      url = "https://example.com/notfound",
      error_message = "Encountered error:"
    ),
    regexp = "Encountered error: ",
    class = "readapra_error_http_status_error"
  )

  expect_error(
    check_http_status(
      url = "https://example.com/servererror",
      error_message = "Encountered error:"
    ),
    regexp = "Encountered error: ",
    class = "readapra_error_http_status_error"
  )

  disable(quiet = TRUE)
})

test_that("get_http_status() returns http details", {
  enable(quiet = TRUE)

  to_return(.data = stub_request("get", "https://example.com/success"), status = 200)
  to_return(.data = stub_request("get", "https://example.com/notfound"), status = 404)
  to_return(.data = stub_request("get", "https://example.com/servererror"), status = 501)

  expect_equal(
    get_http_status("https://example.com/success"),
    list(
      category = "Success",
      reason = "OK",
      message = "Success: (200) OK"
    )
  )

  expect_equal(
    get_http_status("https://example.com/notfound"),
    list(
      category = "Client error",
      reason = "Not Found",
      message = "Client error: (404) Not Found"
    )
  )

  expect_equal(
    get_http_status("https://example.com/servererror"),
    list(
      category = "Server error",
      reason = "Not Implemented",
      message = "Server error: (501) Not Implemented"
    )
  )

  disable(quiet = TRUE)
})

test_that("download_file() real file download", {
  skip_if_offline()
  skip_on_cran()

  with_tempdir({
    expect_true(
      file.exists(
        polite_file_download(
          bow = bow_wrapper("https://httpbin.org/bytes/512"),
          url = "https://httpbin.org/bytes/512", quiet = TRUE
        )
      )
    )
  })
})

test_that("attempt_polite_file_download() error behaviour", {
  # Invalid method supplied
  expect_error(
    attempt_polite_file_download(
      url = "dummy_url",
      bow = "dummy_bow",
      method = "invalid_method"
    )
  )

  # Checking safe fall backs
  local_mocked_bindings(sys_sleep_wrapper = function(...) NULL)
  local_mocked_bindings(safe_polite_file_download = function(...) list(result = NULL, error = "Error"))
  local_mocked_bindings(get_http_status = function(...) list(message = "get_http_status_message"))

  expect_error(
    attempt_polite_file_download(url = "dummy_url", bow = "dummy_bow"),
    class = "readapra_error_could_not_download_file"
  )
})

test_that("attempt_polite_file_download() real file download", {
  skip_if_offline()
  skip_on_cran()

  with_tempdir({
    expect_true(
      file.exists(
        attempt_polite_file_download(
          bow = bow_wrapper("https://httpbin.org/bytes/512"),
          url = "https://httpbin.org/bytes/512",
          quiet = TRUE
        )
      )
    )
  })
})

test_that("attempt_polite_file_download() wininet download warning", {
  skip_if_offline()
  skip_on_cran()
  skip_on_ci()

  with_tempdir({
    with_envvar(c(R_READAPRA_DL_METHOD = "wininet"), {
      expect_warning(
        attempt_polite_file_download(
          bow = bow_wrapper("https://httpbin.org/bytes/512"),
          url = "https://httpbin.org/bytes/512",
          quiet = TRUE
        ),
        regexp = "wininet.*deprecated"
      )
    })
  })
})
