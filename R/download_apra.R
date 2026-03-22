#' Download a Statistical Publication File from APRA's Website
#'
#' @description
#' Download a statistical publication file from APRA's website. By
#' default files are saved to a temporary directory.
#'
#' @param stat_pub character vector detailing a statistical publication to be
#' downloaded. Must match a valid value in the
#' `apra_stat_pubs_acronym` variable of the [apra_stat_pubs] dataset.
#' @param cur_hist character vector detailing whether to download a current
#' or historic statistical publication. Must match a
#' valid value in the `cur_hist` variable of the [apra_stat_pubs]
#' dataset.
#' @param path path to where the downloaded file should be saved. Uses
#' [base::tempdir()] by default.
#' @param overwrite whether to overwrite a previously downloaded statistical
#' publication file when re-running this function.
#' @param quiet whether to suppress the download progress bar.
#' @param ... additional arguments to be passed to [utils::download.file()].
#'
#' @return A character vector detailing the file path to the downloaded file.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Download a statistical publication file:
#' download_path <-
#'   download_apra(stat_pub = "qadips", cur_hist = "current")
#'
#' # View the file path of the statistical publication file:
#' print(download_path)
#' }
#'
download_apra <- function(
    stat_pub,
    cur_hist = "current",
    path = tempdir(),
    overwrite = TRUE,
    quiet = FALSE,
    ...) {
  download_apra_with_caller(
    stat_pub = stat_pub,
    cur_hist = cur_hist,
    path = path,
    overwrite = overwrite,
    quiet = quiet,
    ...
  )
}

#' Download a Publication Statistics File from APRA's Website with caller
#' environment control
#'
#' @param stat_pub character vector containing the acronym of the publication
#' statistics you want to download.
#' @param cur_hist character vector determining whether to download the current
#' publication (`"current"`) or the historic publication (`"historic"`). Please
#' note that not all statistical publications have a historic publication.
#' @param path path to where the downloaded file should be saved. Uses
#' [base::tempdir()] by default.
#' @param overwrite whether to overwrite the downloaded file when re-downloading
#' the file.
#' @param quiet whether to suppress the download progress bar.
#' @param call Caller environment for error handling
#' @param ... additional arguments to be passed to [utils::download.file()].
#'
#' @noRd
#'
download_apra_with_caller <- function(stat_pub,
                                      cur_hist,
                                      path = tempdir(),
                                      overwrite = TRUE,
                                      quiet = FALSE,
                                      call = rlang::caller_env(),
                                      ...) {
  check_valid_file_path(path, call)
  check_standard_user_inputs(stat_pub, cur_hist, call = call)
  check_logical_length_one(overwrite, call = call)
  check_logical_length_one(quiet, call = call)

  selected_stat_pub <-
    dplyr::filter(
      apra_stat_pubs_details,
      stat_pub_acronym == stat_pub,
      cur_hist == {{ cur_hist }}
    )

  check_http_status(
    url = selected_stat_pub$webpage_link,
    error_message = "Could not scrape url to download from:",
    call = call
  )

  extracted_urls <- scrape_urls(selected_stat_pub$webpage_link)
  url_to_download <- url_selector(extracted_urls, selected_stat_pub)

  download_location <-
    attempt_file_download(
      url = url_to_download,
      path = path,
      overwrite = overwrite,
      quiet = quiet,
      call = call
    )

  return(download_location)
}

#' Scrape URLs from a webpage
#'
#' @param url The URL of the webpage to extract links from
#'
#' @noRd
#'
scrape_urls <- function(url) {
  resp <- httr::GET(
    url,
    httr::user_agent("readapra R package")
  )

  if (httr::http_error(resp)) {
    cli::cli_abort(
      c(
        "Could not scrape URL: {.url {url}}",
        "x" = httr::http_status(resp)$message
      )
    )
  }

  page <- httr::content(resp, as = "parsed")

  urls <- rvest::html_elements(page, "a") |>
    rvest::html_attr("href")

  urls <- urls[
    !is.na(urls) &
      nzchar(urls) &
      !grepl("^#", urls) &
      !grepl("^javascript:", urls)
  ]

  urls <- xml2::url_absolute(urls, base = url)

  return(urls)
}

#' Get the download method from the R_READAPRA_DL_METHOD environment variable
#'
#' @noRd
#'
get_dl_method <- function() {
  Sys.getenv("R_READAPRA_DL_METHOD", unset = "auto")
}

#' Use the httr package to get status information about a URL
#'
#' @param url The URL link to get status information about
#'
#' @noRd
#'
get_http_status <- function(url) {
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

#' Download a file using utils::download.file
#'
#' @param url the URL of the file to be downloaded
#' @param path path where to save the destfile.
#' @param overwrite if `TRUE` will overwrite file on disk
#' @param quiet If `TRUE`, suppress the download progress bar.
#' @param ... Other parameters passed on to [utils::download.file()]
#'
#' @noRd
#'
file_download <- function(url,
                          path = tempdir(),
                          overwrite = TRUE,
                          quiet = FALSE,
                          ...) {
  destfile <- file.path(path, basename(url))

  if (file.exists(destfile) && !overwrite) {
    return(destfile)
  }

  utils::download.file(
    url = url,
    destfile = destfile,
    method = get_dl_method(),
    quiet = quiet,
    mode = "wb",
    ...
  )

  return(destfile)
}

#' Safely download a file
#'
#' @noRd
#'
safe_file_download <- purrr::safely(file_download)

#' Attempt to download a file, retrying once on failure
#'
#' @param url the URL of the file to be downloaded
#' @param path path where to save the destfile.
#' @param overwrite if `TRUE` will overwrite file on disk
#' @param quiet If `TRUE`, suppress the download progress bar.
#' @param call The caller environment
#' @param ... Other parameters passed on to [utils::download.file()]
#'
#' @noRd
#'
attempt_file_download <- function(url,
                                  path = tempdir(),
                                  overwrite = TRUE,
                                  quiet = FALSE,
                                  call = rlang::caller_env(),
                                  ...) {
  download_outcome <-
    safe_file_download(
      url = url,
      path = path,
      overwrite = overwrite,
      quiet = quiet,
      ...
    )

  if (!is.null(download_outcome$error)) {
    sys_sleep_wrapper(5)
    download_outcome <-
      safe_file_download(
        url = url,
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
