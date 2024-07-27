get_apra_statistics_url <- function(statistics, period) {
  list_statistics_urls <-
    purrr::map2(.x = statistics, .y = period, .f = function(x = .x, y = .y) {
      selected_statistics <-
        dplyr::filter(
          .data = apra_scraped_download_links,
          statistics == x,
          period == y
        )

      if (selected_statistics$stable_url) {
        url <- selected_statistics$url
      } else {
        url <-
          scrape_apra_statistics_url(
            selected_statistics$url,
            selected_statistics$css_selector
          )
      }
      statistics_url_details <-
        tibble::tibble(
          details = stringr::str_c(x, y, sep = "_"),
          url = url
        )
    })
  tibble_statistics_url <- dplyr::bind_rows(list_statistics_urls)
  return(tibble_statistics_url)
}

scrape_apra_statistics_url <- function(url, css_selector) {
  session <- polite::bow(url)
  html_page <- polite::scrape(session)
  url_link <-
    rvest::html_attr(
      x = rvest::html_element(html_page, css_selector),
      name = "href"
    )
  return(url_link)
}
