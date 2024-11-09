clean_imported_data <- function(file_path, sheet,
                                stat_pub_name,
                                call = rlang::caller_env()) {
  imported_data <-
    readxl::read_excel(
      path = file_path,
      sheet = sheet,
      trim_ws = TRUE,
      col_names = FALSE,
      .name_repair = "minimal"
    )

  # .names_repair "unique" is noticeably slower than make.names
  names(imported_data) <- make.names(names(imported_data), unique = TRUE)

  # Remove NA columns & rows
  imported_data <- purrr::discard(imported_data, ~ all(is.na(.)))
  imported_data <- dplyr::filter(imported_data, !dplyr::if_all(dplyr::everything(), is.na))

  # Get the first complete row of the data
  first_complete_row <- which(complete.cases(imported_data))[1]

  if (is.na(first_complete_row)) {
    cli::cli_abort(
      message = "Cannot import {.field {stat_pub_name}}.
      Spreadsheet is not in a recognised format.",
      call = call
    )
  }

  imported_data <- dplyr::slice(imported_data, first_complete_row:nrow(imported_data))

  # Rename columns
  colnames(imported_data) <- as.character(imported_data[1, ])
  imported_data <- imported_data[-1, ]

  # Convert columns to correct types
  imported_data <- type.convert(imported_data, as.is = TRUE)

  safe_col1_date_data <- safely_first_col_to_date(imported_data)

  if (!is.null(safe_col1_date_data$error) ||
      !all(!is.na(safe_col1_date_data$result[[1]]))
  ) {
    cli::cli_abort(
      message = "Cannot import {.field {stat_pub_name}}.
      Spreadsheet is not in a recognised format.",
      call = call
    )
  } else {
    imported_data <- safe_col1_date_data$result
  }

  return(imported_data)
}

first_col_to_date <- function(data) {
  data[[1]] <-
    lubridate::as_date(x = as.numeric(data[[1]]), origin = "1899-12-30")

  return(data)
}

safely_first_col_to_date <- purrr::safely(first_col_to_date)
