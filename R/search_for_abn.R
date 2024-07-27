search_for_abn <- function(entity_to_search) {
  abr_search_table <- get_abr_search_table(entity_to_search)
  cleaned_abr_search_table <- clean_abr_search_table(abr_search_table)
  best_match_string_index <-
    stringdist::amatch(
      x = tolower(entity_to_search),
      table = tolower(cleaned_abr_search_table$name),
      maxDist = Inf
    )
  abr_search_results <-
    cleaned_abr_search_table[best_match_string_index, c("abn", "name")]
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
      rvest::html_element(
        polite::scrape(session), "table"
      )
    )
  return(scraped_abr_table)
}

clean_abr_search_table <- function(abr_search_table) {
  names(abr_search_table) <-
    tolower(names(abr_search_table))
  abr_search_table$abn <-
    stringr::str_replace_all(abr_search_table$abn, "\\s+", "")
  abr_search_table <-
    tidyr::separate_wider_regex(
      data = abr_search_table,
      cols = abn,
      patterns = c(abn = "\\d+", status = "[a-zA-Z]+")
    )
  abr_search_table$abn <- as.numeric(abr_search_table$abn)
  abr_search_table$type <-
    stringr::str_replace_all(abr_search_table$type, "\\s+", " ")
  abr_search_table$type <-
    stringr::str_trim(
      stringr::str_replace(abr_search_table$type, "\\s*\\(Historic\\)$", "")
    )
  filtered_abr_search_table <-
    dplyr::filter(
      .data = abr_search_table,
      type == "Entity Name",
      status == "Active"
    )
  return(filtered_abr_search_table)
}
