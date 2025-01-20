#' Read Quarterly ADI Property Exposure Statistics
#'
#' @description
#' Import the Quarterly Authorised Deposit-taking Institution Property Exposure
#' Statistics (QPEXS) from a local file. Both the current and historic versions
#' of this statistical publication are available.
#'
#' @param file_path path to the local .xlsx file.
#' @param cur_hist character vector determining whether to download the current
#' publication (`"current"`) or the historic publication (`"historic"`).
#'
#' @return A tibble containing the Quarterly ADI Performance Statistics data.
#'
#' @noRd
#'
read_qadipexs <- function(file_path, cur_hist, call = rlang::caller_env()) {
  tidyxl_data <- read_tidyxl_data(file_path, call = call)
  formatting_data <- read_tidyxl_formatting_data(file_path)
  qadipexs_data(tidyxl_data, formatting_data, cur_hist, call = call)
}

#' Extracts the QADIPEXS data from the various sheets and conducts final
#' formatting tweaks
#'
#' @param tidyxl_data cell data extracted from a .xlsx file using the tidyxl
#' package
#' @param formatting_data formatting data extracted from a .xlsx file using the
#' tidyxl package
#' @param cur_hist character vector determining whether to download the current
#' publication (`"current"`) or the historic publication (`"historic"`).
#' @param call the caller environment
#'
#' @keywords internal
#' @noRd
#'
qadipexs_data <- function(tidyxl_data,
                          formatting_data,
                          cur_hist,
                          call = rlang::caller_env()) {
  qadipexs_data <-
    attempt_format_horizontal_data(
      tidyxl_data = tidyxl_data,
      formatting_data = formatting_data,
      stat_pub_name = "Quarterly Authorised Deposit-taking Institution Property Exposures Statistics",
      sheet_str_detect = "Tab",
      frequency = "Quarterly",
      call = call
    )

  if (cur_hist == "current") {
    qadipexs_data <-
      dplyr::mutate(
        .data = qadipexs_data,
        unit = dplyr::case_when(
          series %in% c(
            "Impaired assets to exposures",
            "Specific provisions to exposures",
            "Specific provisions to impaired exposures",
            "Specific provisions and security held to impaired exposures",
            "Non-performing to total exposures",
            "Specific provisions to total exposures",
            "Specific provisions to non-performing exposures",
            "Specific provisions and security held to non-performing exposures",
            "Weighted average variable rate of new loans funded",
            "Weighted average assessment rate used for serviceability"
          )
          ~ "Percent",
          .default = unit
        )
      )
  }

  qadipexs_data <-
    replace_columns_patterns(
      data = qadipexs_data,
      cols = c("series_hierarchy", "series"),
      match = qadipexs_string_replacement$match,
      replace = qadipexs_string_replacement$replacement
    )

  return(qadipexs_data)
}

#' Malformed strings in the QADIPEXS that need replacing
#'
#' @noRd
#'
qadipexs_string_replacement <-
  tibble::tribble(
    ~match, ~replacement,
    "Number of entitiesa", "Number of entities",
    "Number of entitiesb", "Number of entities",
    "Impaired Commercial property exposuresa", "Impaired Commercial property exposures",
    "Specific provisions and securitya", "Specific provisions and security",
    "Impaired assets to exposuresa", "Impaired assets to exposures",
    "Specific provisions to exposuresa", "Specific provisions to exposures",
    "Specific provisions and security held to impaired exposuresa", "Specific provisions and security held to impaired exposures",
    "All ADIsb", "All ADIs",
    "of which:a", "of which:",
    "of which:b", "of which:",
    "of which:c", "of which:",
    "All other domestic banksa", "All other domestic banks",
    "All foreign subsidiary banksa", "All foreign subsidiary banks"
  )
