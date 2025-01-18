# URL data for download_apra ----------------------------------------------
apra_statistical_publications <-
    dplyr::select(
      .data = readr::read_csv("data-raw/apra_statistical_publication_record.csv"),
      statistical_publication_name,
      statistical_publication_acronym,
      cur_hist
    )

# Building system data ----------------------------------------------------
usethis::use_data(
  apra_statistical_publications,
  internal = FALSE,
  overwrite = TRUE
)
