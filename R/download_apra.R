download_apra <- function(statistics, period) {
  urls <-
    get_apra_statistics_url(statistics = statistics, period = period)
  download_location <-
    download_file(details = urls$details, urls = urls$url)
  return(download_location)
}

download_file <- function(details, urls, path1 = tempdir()) {
  list_statistics_file_path_name <-
    purrr::map2(
      .x = details,
      .y = urls,
      .f = function(x = .x, y = .y, path = path1) {

  file_name <- basename(y)
  file_path_name <- file.path(path, file_name)

  utils::download.file(
    url = y,
    destfile = file_path_name,
    quiet = FALSE,
    mode = "wb")

  download_details <-
    tibble::tibble(
      details = x,
      file_path_name = file_path_name
    )

  return(download_details)
  })

  tibble_statistics_file_path_name <-
    dplyr::bind_rows(list_statistics_file_path_name)

  return(tibble_statistics_file_path_name)
}


