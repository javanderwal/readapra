apra_stat_pub_details <-
  readr::read_csv("data-raw/apra_statistical_publication_record.csv") |>
  dplyr::mutate(
    link =
      stringr::str_replace_all(
        link,
        "\\d{4}-\\d{2}/\\d{8}",
        "\\\\d{4}-\\\\d{2}/\\\\d{8}"
      ),
    link =
      stringr::str_replace_all(
        link,
        "%20[A-Za-z]+%20\\d{4}",
        "%20[A-Za-z]+%20\\\\d{4}"
      )
  )

usethis::use_data(
  apra_stat_pub_details,
  internal = TRUE,
  overwrite = TRUE
)
