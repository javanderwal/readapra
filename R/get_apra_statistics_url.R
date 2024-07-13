get_apra_statistics_url <- function(statistics, period) {
  selected_statistics <-
    dplyr::filter(
      .data = apra_scraped_download_links,
      statistics == {{ statistics }},
      period == {{ period }}
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
  return(url)
}

scrape_apra_statistics_url <- function(url, css_selector) {
  session <- polite::bow(url)
  html_page <- polite::scrape(session)
  url_link <-
    html_attr(html_element(html_page, css_selector), "href")
  return(url_link)
}
