#' Takes a vector of URLs and tries to identify the URL to use as a download
#' link use the filtered URL from the apra_stat_pub_details data
#'
#' @param urls_to_check a vector of URLs from which the correct on needs to be
#' extracted
#' @param selected_stat_pub the filtered apra_stat_pub_details containging the
#' required URL
#' @param call caller environment for error message handling
#'
#' @noRd
#'
url_selector <- function(
    urls_to_check,
    selected_stat_pub,
    call = rlang::caller_env()) {
  regex_matched_url <-
    stringr::str_subset(
      urls_to_check,
      selected_stat_pub$regex_link
    )

  if (length(regex_matched_url == 1)) {
    return(regex_matched_url)
  }

  file_to_find <- prepare_name_comparison(selected_stat_pub$file_name)
  files_to_compare <- prepare_name_comparison(basename(urls_to_check))

  stringdist_match_result <-
    stringdist::amatch(
      x = file_to_find,
      table = files_to_compare,
      nomatch = NULL,
      method = "jw",
      maxDist = Inf
    )

  if (is.null(stringdist_match_result) |
    length(stringdist_match_result) != 1 |
    length(files_to_compare) < 1) {
    cli::cli_abort(
      message = "Could not determine correct URL to download.",
      class = "readapra_could_not_find_right_file_url",
      call = call
    )
  }

  return(urls_to_check[stringdist_match_result])
}

#' Take an encoded file name and cleans it by removing and months or numbers
#' in the string
#'
#' @param url the url to prepare for a name comparison
#'
#' @noRd
#'
prepare_name_comparison <- function(url) {
  decoded_url <- utils::URLdecode(url)

  month_pattern <-
    paste0("\\b(", paste(c(month.name, month.abb), collapse = "|"), ")\\b")

  url_no_month <-
    stringr::str_remove_all(
      string = decoded_url,
      pattern = stringr::regex(month_pattern, ignore_case = TRUE)
    )

  url_no_numbers <- stringr::str_remove_all(url_no_month, "\\d+")

  url_no_whitespace <- stringr::str_trim(url_no_numbers)

  return(url_no_whitespace)
}
