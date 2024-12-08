download_apra <- function(publication, cur_hist) {
  rlang::arg_match(publication, unique(apra_stat_pub_details$publication))
  rlang::arg_match(cur_hist, c("current", "historic"))

  selected_stat_pub <-
    dplyr::filter(
      apra_stat_pub_details,
      publication == {{ publication }},
      cur_hist == {{ cur_hist }}
    )

  url_session <- polite::bow(selected_stat_pub$webpage)

  if (selected_stat_pub$static_link) {
    temp_link <- selected_stat_pub$link
  } else {
    temp_link <-
      polite::scrape(url_session) |>
      rvest::html_elements("a") |>
      rvest::html_attr("href") |>
      stringr::str_subset(".xlsx$") |>
      stringr::str_subset(selected_stat_pub$link)
  }

  download_outcome <- safely_download_file(url_session, temp_link)

  if (!is.null(download_outcome$error)) {
    Sys.sleep(10)
    download_outcome <- safely_download_file(url_session, temp_link)
  }

  if (!is.null(download_outcome$error)) {
    cli::cli_abort("Could not download: {.url temp_link} ")
  }

  return(download_outcome$result)
}

get_http_status_apra <- function(url) {
  http_response <- httr::GET(url)
  if (httr::http_error(http_response)) {
    status_message <- httr::http_status(http_response)$message
    cli::cli_abort(
      c("Could not download {.url {url}}.", c("x" = status_message))
    )
  }
}

download_file <-
  function(bow, url, ...) {
  temp_link <-
    polite::nod(bow, url) |>
    polite::rip(overwrite = TRUE, ...)
}

safely_download_file <- purrr::safely(download_file)


