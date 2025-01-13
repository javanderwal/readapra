#' Downloads the required files from APRA's website
#'
#' @param publication The publication that you want to download
#' @param cur_hist Whether to download the current or historic publication
#' @param path path where to save the destfile.
#' @param overwrite if `TRUE` will overwrite file on disk
#' @param quiet If `TRUE`, suppress status messages (if any), and the progress bar.
#' @param call Caller environment for error handling
#' @param ... Other parameters passed on to download.file
#'
#' @noRd
#'
download_apra <- function(
    publication,
    cur_hist,
    path = tempdir(),
    overwrite = TRUE,
    quiet = FALSE,
    call = rlang::caller_env(),
    ...) {
  rlang::arg_match(
    arg = publication,
    values = unique(apra_stat_pub_details$publication),
    error_call = call
  )
  rlang::arg_match(
    arg = cur_hist,
    values = c("current", "historic"),
    error_call = call
  )

  selected_stat_pub <-
    dplyr::filter(
      apra_stat_pub_details,
      publication == {{ publication }},
      cur_hist == {{ cur_hist }}
    )

  check_http_status(
    url = selected_stat_pub$webpage,
    error_message = "Could not scrape url to download from:",
    call = call
  )

  bow_obj <- bow_wrapper(selected_stat_pub$webpage)
  extracted_urls <- scrape_urls(bow_obj)
  url_to_download <- url_selector(extracted_urls, selected_stat_pub)

  download_location <-
    attempt_polite_file_download(
      url = url_to_download,
      bow = bow_obj,
      path = path,
      overwrite = overwrite,
      quiet = quiet,
      call = call
    )

  return(download_location)
}

#' Scrape URLs from a webpage
#'
#' @param url_session The polite bow object to extract the URLs from
#'
#' @noRd
#'
scrape_urls <- function(url_session) {
  scraped_urls <- polite::scrape(url_session)
  scraped_urls <- rvest::html_elements(scraped_urls, "a")
  scraped_urls <- rvest::html_attr(scraped_urls, "href")
  return(scraped_urls)
}

#' Use the httr package to get status information about a URL
#'
#' @param url The URL link to get status information about
#'
#' @noRd
#'
get_http_status <- function(url) {
  old_dl_method <- getOption("download.file.method")
  on.exit(options(old_dl_method))
  readapra_dl_method <- Sys.getenv("R_READAPRA_DL_METHOD", unset = "auto")
  options("download.file.method" = readapra_dl_method)
  httr::http_status(httr::GET(url))
}

#' Check the http status of a URL and throw an error if unable to get a success
#'
#' @param url The URL link to be checked
#' @param call The caller environment
#'
#' @noRd
#'
check_http_status <- function(url,
                              error_message = "Could not access:",
                              call = rlang::caller_env()) {
  url_status <- get_http_status(url)
  if (url_status$category != "Success") {
    cli::cli_abort(
      message = c(
        "{error_message} {.url {url}}",
        c("x" = url_status$message)
      ),
      class = "readapra_error_http_status_error",
      call = call
    )
  }

  return()
}

#' Politely download a file
#'
#' @param url the URL of the file to be downloaded
#' @param bow host introduction object of class polite, session created by bow() or nod()
#' @param method Method to be used for downloading files.
#' @param path path where to save the destfile.
#' @param overwrite if `TRUE` will overwrite file on disk
#' @param quiet If `TRUE`, suppress status messages (if any), and the progress bar.
#' @param ... Other parameters passed on to download.file
#'
#' @noRd
#'
polite_file_download <- function(url,
                                 bow,
                                 method = Sys.getenv("R_READAPRA_DL_METHOD", unset = "auto"),
                                 path = tempdir(),
                                 overwrite = TRUE,
                                 quiet = FALSE,
                                 ...) {
  temp_link <-
    polite::nod(bow, url) |>
    polite::rip(
      method = method,
      path = path,
      overwrite = overwrite,
      quiet = quiet,
      ...
    )

  return(temp_link)
}

#' Safely and politely download a file
#'
#' @noRd
#'
safe_polite_file_download <- purrr::safely(polite_file_download)

#' Attempt to politely download a file
#'
#' @param url the URL of the file to be downloaded
#' @param bow host introduction object of class polite, session created by bow() or nod()
#' @param path path where to save the destfile.
#' @param overwrite if `TRUE` will overwrite file on disk
#' @param quiet If `TRUE`, suppress status messages (if any), and the progress bar.
#' @param ... Other parameters passed on to download.file
#'
#' @noRd
#'
attempt_polite_file_download <- function(url,
                                         bow,
                                         path = tempdir(),
                                         overwrite = TRUE,
                                         quiet = FALSE,
                                         call = rlang::caller_env(),
                                         ...) {
  download_outcome <-
    safe_polite_file_download(
      url = url,
      bow = bow,
      path = path,
      overwrite = overwrite,
      quiet = quiet,
      ...
    )

  if (!is.null(download_outcome$error)) {
    sys_sleep_wrapper(5)
    download_outcome <-
      safe_polite_file_download(
        url = url,
        bow = bow,
        path = path,
        overwrite = overwrite,
        quiet = quiet,
        ...
      )
  }

  if (!is.null(download_outcome$error)) {
    cli::cli_abort(
      message = c(
        "Could not download file from: {.url {url}}",
        get_http_status(url)$message
      ),
      class = "readapra_error_could_not_download_file",
      call = call
    )
  }

  return(download_outcome$result)
}
