# URL data for download_apra ----------------------------------------------
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

apra_stat_pubs_details <-
  dplyr::mutate(
    .data = readr::read_csv("data-raw/apra_statistical_publication_record.csv"),
    regex_link = clean_apra_url(file_link),
    file_name = basename(file_link)
  )

# MADIS balance sheet category data ---------------------------------------
madis_current_balance_sheet <-
  readr::read_csv("data-raw/madis_current_balance_sheet.csv")

madis_historic_balance_sheet <-
  readr::read_csv("data-raw/madis_historic_balance_sheet.csv")

# QADICP risk metric category ---------------------------------------------
qadicp_risk_metric_category <-
  readr::read_csv("data-raw/qadicp_risk_metric_category.csv")

# Building system data ----------------------------------------------------
usethis::use_data(
  apra_stat_pubs_details,
  madis_current_balance_sheet,
  madis_historic_balance_sheet,
  qadicp_risk_metric_category,
  internal = TRUE,
  overwrite = TRUE
)
