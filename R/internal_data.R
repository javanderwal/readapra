abr_abn_search_url <- "https://abr.business.gov.au/Search/ResultsAll?SearchText="

apra_scraped_download_links <-
  tibble::tribble(
    ~statistics, ~period, ~stable_url, ~url, ~css_selector,
    "madis", "current", FALSE, "https://www.apra.gov.au/monthly-authorised-deposit-taking-institution-statistics", "div[data-entity-uuid='a543134e-c02c-4109-a123-7e62cf89a9fd'] a.document-link",
    "madis", "back_series", FALSE, "https://www.apra.gov.au/monthly-authorised-deposit-taking-institution-statistics", "div[data-entity-uuid='334813a6-ec21-4aef-94b1-41f23d0d2389'] a.document-link",
    "madis", "historic", TRUE, "https://www.apra.gov.au/sites/default/files/monthly_banking_statistics_june_2019_back_series.xlsx", NA,
    "qadips", "current", FALSE, "https://www.apra.gov.au/quarterly-authorised-deposit-taking-institution-statistics", "div[data-entity-uuid='ebb4a7fc-ac2d-462e-9197-2d5164cca32b'] a.document-link",
    "qadicp", "current", FALSE, "https://www.apra.gov.au/quarterly-authorised-deposit-taking-institution-statistics", "div[data-entity-uuid='62de0e06-6e1a-42dc-9fdc-0c227253f64e'] a.document-link",
    "qadipexs", "current", FALSE, "https://www.apra.gov.au/quarterly-authorised-deposit-taking-institution-statistics", "div[data-entity-uuid='af4eac5f-d34f-49ca-a072-b622e9544855'] a.document-link",
    "qadipexs", "historic", TRUE, "https://www.apra.gov.au/sites/default/files/2020-03/Quarterly%20authorised%20deposit-taking%20institution%20property%20exposures%20-%20Historical%20data.xlsx", NA
  )
