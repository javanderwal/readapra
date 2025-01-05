#' Downloads the required files from APRA's website
#'
#' @param publication The publication that you want to download
#' @param cur_hist Whether to download the current or historic publication
#' @param backup_match String matches used to include a URL
#' @param backup_remove String matches used to exclude a URL
#'
#' @keywords internal
download_apra <- function(publication, cur_hist, backup_match, backup_remove = NULL) {
  rlang::arg_match(publication, unique(apra_stat_pub_details$publication))
  rlang::arg_match(cur_hist, c("current", "historic"))

  selected_stat_pub <-
    dplyr::filter(
      apra_stat_pub_details,
      publication == {{ publication }},
      cur_hist == {{ cur_hist }}
    )

  url_session <- polite::bow(selected_stat_pub$webpage)

  xlsx_urls <-
    polite::scrape(url_session) |>
    rvest::html_elements("a") |>
    rvest::html_attr("href") |>
    stringr::str_subset(".xlsx$")

  temp_link <-
    stringr::str_subset(xlsx_urls, selected_stat_pub$link)

  if (length(temp_link) != 1) {
    temp_link <-
      backup_link_identifier(
        x = xlsx_urls,
        to_match = backup_match,
        to_remove = backup_remove
      )
  }

  download_outcome <- safely_download_file(url_session, temp_link)

  if (!is.null(download_outcome$error)) {
    Sys.sleep(5)
    download_outcome <- safely_download_file(url_session, temp_link)
  }

  if (!is.null(download_outcome$error)) {
    cli::cli_abort("Could not download: {.url {temp_link}} ")
  }

  return(download_outcome$result)
}

#' If the original method for selecting the URL to download fails this function
#' will attempt to identify the correct URL using string matching. Necessary
#' because APRA frequently botches the URL names.
#'
#' @param x A vector of URL strings to evaluate
#' @param to_match String matches used to include a URL
#' @param to_remove String matches used to exclude a URL
#'
#' @keywords internal
#'
backup_link_identifier <- function(x, to_match, to_remove = NULL) {
  if (!is.null(to_remove)) {
    with_removed <-
      x[stringr::str_detect(tolower(x), to_remove, negate = TRUE)]
  } else {
    with_removed <- x
  }

  number_matches <-
    stringr::str_count(tolower(with_removed), pattern = to_match)

  if (all(number_matches == 0)) {
    cli::cli_abort("No good")
  }

  which_to_keep <-
    which(number_matches == max(number_matches, na.rm = TRUE))

  if (length(which_to_keep) != 1) {
    cli::cli_abort("No good")
  }

  with_removed[which_to_keep]
}

#' Check the http status of a URL and if said URL is unavailable throw an error
#' with the http status code
#'
#' @param url The URL link to be checked
#'
#' @keywords internal
#'
get_http_status <- function(url) {
  http_response <- httr::GET(url)
  if (httr::http_error(http_response)) {
    status_message <- httr::http_status(http_response)$message
    cli::cli_abort(
      c("Could not download {.url {url}}.", c("x" = status_message))
    )
  }
}

#' Politely download a file
#'
#' @param bow The bow object obtained using the polite package
#' @param url The URL to the file to be downloaded
#' @param ... Other parameters passed on to download.file
#'
#' @keywords internal
#'
download_file <-
  function(bow, url, ...) {
    temp_link <-
      polite::nod(bow, url) |>
      polite::rip(overwrite = TRUE, ...)
  }

#' Safely and politely download a file
#'
#' @keywords internal
#'
safely_download_file <- purrr::safely(download_file)
