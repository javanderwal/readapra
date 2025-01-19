#' Read APRA's Statistical Publications
#'
#' @description
#' Download and import a specific statistical publication produced by APRA.
#'
#' Please consult the [apra_stat_pubs] dataset to see which of APRA's
#' statistical publications are available in `readapra`.
#'
#' @param stat_pub character vector detailing a statistical publication to be
#' downloaded and imported. Must match a valid value in the
#' `apra_stat_pubs_acronym` variable of the [apra_stat_pubs] dataset.
#' @param cur_hist character vector detailing whether to download and import
#' a current or historic statistical publication. Must match a
#' valid value in the `cur_hist` variable of the [apra_stat_pubs]
#' dataset.
#' @param path path to where the downloaded file should be saved. Uses
#' [base::tempdir()] by default.
#' @param overwrite whether to overwrite a previously downloaded statistical
#' publication file when re-running this function.
#' @param quiet whether to suppress the download progress bar.
#' @param ... additional arguments to be passed to [utils::download.file()].
#'
#' @return A tibble containing the statistical publication data.
#' @export
#'
#' @examples
#' \donttest{
#' # Download and import the current MADIS data:
#' current_madis_data <-
#'   read_apra(publication = "madis", cur_hist = "current")
#'
#' # Examine the current MADIS data:
#' print(current_madis_data)
#' }
#'
read_apra <- function(stat_pub,
                      cur_hist = "current",
                      path = tempdir(),
                      quiet = FALSE,
                      overwrite = TRUE,
                      ...) {
  temp_file_path <-
    download_apra_with_caller(
      stat_pub = stat_pub,
      cur_hist = cur_hist,
      path = path,
      quiet = quiet,
      overwrite = overwrite,
      ...
    )

  read_apra_local(temp_file_path, stat_pub, cur_hist)
}

#' Read APRA Publication Statistics Locally
#'
#' @description
#' Import from a local file a specific statistical publication produced by APRA.
#'
#' Please consult the [apra_stat_pubs] dataset to see which of APRA's
#' statistical publications are available in `readapra`.
#'
#' @param file_path path to the local file from which the statistical
#' publication data will be imported.
#' @param stat_pub character vector detailing a statistical publication to be
#' imported. Must match a valid value in the `apra_stat_pubs_acronym` variable
#' of the [apra_stat_pubs] dataset.
#' @param cur_hist character vector detailing whether to import a current
#' or historic statistical publication. Must match a
#' valid value in the `cur_hist` variable of the [apra_stat_pubs]
#' dataset.
#'
#' @return A tibble containing the statistical publication data.
#' @export
#'
#' @examples
#' \donttest{
#' # Download the current MADIS data and get the file path:
#' current_madis_file_path <-
#'   download_apra(publication = "madis", cur_hist = "current")
#'
#' # Import the current MADIS data:
#' current_madis_data <-
#'   read_apra_local(
#'     publication = "madis",
#'     cur_hist = "current",
#'     file_path = current_madis_file_path
#'   )
#'
#' # Examine the current MADIS data:
#' print(current_madis_data)
#' }
#'
read_apra_local <- function(file_path,
                            stat_pub,
                            cur_hist = "current") {
  check_valid_file_path(file_path)

  matched_fn_name <-
    check_standard_user_inputs(stat_pub, cur_hist)$read_function

  if (length(matched_fn_name) != 1) {
    cli::cli_abort(
      message = "Could not read local file. Couldn't determine correct function to use.",
      class = "readapra_error_multiple_functions_in_read_apra_local"
    )
  }

  read_fn <-
    rlang::as_function(
      x = matched_fn_name,
      env = asNamespace("readapra")
    )

  read_fn(file_path, cur_hist)
}
