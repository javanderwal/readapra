clean_apra_url <- function(url) {
  cleaned_url <- stringr::str_replace_all(
    url,
    "\\d{4}-\\d{2}/\\d{8}",
    "\\\\d{4}-\\\\d{2}/\\\\d{8}"
  )
  cleaned_url <-
    stringr::str_replace_all(
      cleaned_url,
      "%20[A-Za-z]+%20\\d{4}",
      "%20[A-Za-z]+%20\\\\d{4}"
    )
  return(cleaned_url)
}

apra_stat_pub_details <-
  dplyr::mutate(
    .data = readr::read_csv("data-raw/apra_statistical_publication_record.csv"),
    regex_link = clean_apra_url(link),
    file_name = basename(link)
  )

usethis::use_data(
  apra_stat_pub_details,
  internal = TRUE,
  overwrite = TRUE
)
