search_for_abn <- function(entity_to_search) {
  abr_search_table <- get_abr_search_table(entity_to_search)
  cleaned_abr_search_table <- clean_abr_search_table(abr_search_table)
  abr_search_results <- cleaned_abr_search_table[1,c("abn", "name")]
  return(abr_search_results)
}

get_abr_search_table <- function(entity_to_search) {
  encoded_entity_to_search <-
    URLencode(entity_to_search, reserved = TRUE)
  abr_url_to_scrape <-
    stringr::str_c(abr_abn_search_url, encoded_entity_to_search)
  session <- polite::bow(abr_url_to_scrape)
  scraped_abr_table <-
    rvest::html_table(
      rvest::html_node(
        polite::scrape(session), "table"
      )
    )
  return(scraped_abr_table)
}

clean_abr_search_table <- function(abr_search_table) {
  names(abr_search_table) <-
    tolower(names(abr_search_table))
  filtered_abr_search_table <-
    dplyr::filter(abr_search_table, type == "Entity Name")
  cleaned_abr_search_table <-
    dplyr::mutate(
      filtered_abr_search_table,
      abn = as.numeric(stringr::str_remove_all(abn, "[^0-9]"))
    )
  return(cleaned_abr_search_table)
}
