#' Read ADI Points of Presence Statistics
#'
#' @description
#' Download and import the Authorised Deposit-taking Institution Points of
#' Presence Statistics (ADIPOPS) from APRA's website.
#'
#' @param path path to where the downloaded file should be saved. Uses
#' [base::tempdir()] by default.
#' @param overwrite whether to overwrite the downloaded file when re-downloading
#' the file.
#' @param quiet whether to suppress the download progress bar.
#' @param ... additional arguments to be passed to [utils::download.file()].
#'
#' @return A tibble containing the ADIPOPS data.
#' @export
#'
#' @examples
#' \dontrun{
#' read_adipops("current")
#' }
read_adipops <- function(
    path = tempdir(),
    overwrite = TRUE,
    quiet = FALSE,
    ...) {
  temp_file_path <- download_apra(
    publication = "adipops",
    cur_hist = "current",
    path = path,
    quiet = quiet,
    overwrite = overwrite,
    ...
  )
  read_adipops_local(temp_file_path)
}

#' Read ADI Points of Presence Statistics locally
#'
#' @description
#' Import the Authorised Deposit-taking Institution Points of Presence
#' Statistics (ADIPOPS) from a local file.
#'
#' @param file_path path to the .xlsx file.
#'
#' @return A tibble containing the ADIPOPS data.
#' @export
#'
#' @examples
#' \dontrun{
#' read_adipops_local(file_path = ~ path / to / xlsx / file)
#' }
read_adipops_local <- function(file_path) {
  check_valid_file_path(file_path)
  tidyxl_data <- read_tidyxl_data(file_path, sheets = "table.*1")
  formatting_data <- read_tidyxl_formatting_data(file_path)
  adipops_data(tidyxl_data, formatting_data)
}

#' Extracts the ADIPOPS data and cleans it.
#'
#' @param tidyxl_data cell data extracted from a .xlsx file using the tidyxl
#' package
#' @param formatting_data formatting data extracted from a .xlsx file using the
#' tidyxl package
#'
#' @noRd
#'
adipops_data <- function(tidyxl_data, formatting_data) {
  adipops_data <-
    attempt_format_vertical_data(
      tidyxl_data = tidyxl_data,
      formatting_data = formatting_data,
      stat_pub_name = "Authorised Deposit-taking Institutions' Points of Presence statistics"
    )
  adipops_data <- convert_adipops_units(adipops_data)
  return(adipops_data)
}

#' Cleans up the units column in the ADIPOPS data so it matches other publications.
#'
#' @param data the ADIPOPS data to be cleaned.
#'
#' @noRd
#'
convert_adipops_units <- function(data) {
  adipops_correct_units <-
    data |>
    dplyr::mutate(unit = dplyr::case_when(
      stringr::str_detect(
        string = .data$series,
        pattern = stringr::regex("number", ignore_case = TRUE)
      ) ~ unit,
      .default = .data$series
    ))
  return(adipops_correct_units)
}
