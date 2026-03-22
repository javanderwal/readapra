#' Wrapper of base Sys.sleep for testing purposes
#'
#' @noRd
#'
sys_sleep_wrapper <- function(time) {
  base::Sys.sleep(time = time)
}

#' Wrapper of base file.exists for testing purposes
#'
#' @keywords internal
#' @noRd
#'
file_exists_wrapper <- function(...) {
  base::file.exists(...)
}
