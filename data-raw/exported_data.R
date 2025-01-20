# URL data for download_apra ----------------------------------------------
apra_stat_pubs <-
  dplyr::select(
    .data = readr::read_csv("data-raw/apra_statistical_publication_record.csv"),
    stat_pub_name,
    stat_pub_acronym,
    cur_hist
  )

# Building system data ----------------------------------------------------
usethis::use_data(
  apra_stat_pubs,
  internal = FALSE,
  overwrite = TRUE
)
