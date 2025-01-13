#' Wrapper of polite bow() for testing purposes
#'
#' @noRd
#'
bow_wrapper <- polite::bow

#' Wrapper of base Sys.sleep for testing purposes
#'
#' @noRd
#'
sys_sleep_wrapper <- function(time) {
  base::Sys.sleep(time = time)
}

#' Wrapper of polite scrape for testing purposes
#'
#' @noRd
#'
scrape_wrapper <- polite::scrape

#' Wrapper of base file.exists for testing purposes
#'
#' @keywords internal
#' @noRd
#'
file_exists_wrapper <- function(...) {
  base::file.exists(...)
}

