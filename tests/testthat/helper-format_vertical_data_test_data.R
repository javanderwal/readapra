vertical_data_input <- function() {
  output_tibble <-
    tibble::tribble(
      ~sheet, ~address, ~row, ~col, ~is_blank, ~content, ~data_type, ~error, ~logical, ~numeric, ~date, ~character, ~formula, ~is_array, ~formula_ref, ~formula_group, ~comment, ~height, ~width, ~row_outline_level, ~col_outline_level, ~style_format, ~local_format_id,
      "Cover", "A1", 1L, 1L, FALSE, 12, "character", NA, NA, NA, NA, "This is a test cover sheet", NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Normal", 1L,
      "Notes", "A1", 1L, 1L, FALSE, 13, "character", NA, NA, NA, NA, "Notes", NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Normal", 1L,
      "Notes", "A2", 2L, 1L, FALSE, 14, "character", NA, NA, NA, NA, "This file is for testing purposes", NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Normal", 1L,
      "Table 1", "A1", 1L, 1L, FALSE, 0, "character", NA, NA, NA, NA, "($million)", NA, FALSE, NA, NA, NA, 14.4, 10.5546875, 1L, 1L, "Normal", 1L,
      "Table 1", "A3", 3L, 1L, FALSE, 1, "character", NA, NA, NA, NA, "Period", NA, FALSE, NA, NA, NA, 14.4, 10.5546875, 1L, 1L, "Normal", 1L,
      "Table 1", "B3", 3L, 2L, FALSE, 2, "character", NA, NA, NA, NA, "ABN", NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Normal", 1L,
      "Table 1", "C3", 3L, 3L, FALSE, 3, "character", NA, NA, NA, NA, "Entity", NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Normal", 1L,
      "Table 1", "D3", 3L, 4L, FALSE, 4, "character", NA, NA, NA, NA, "Field 1", NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Normal", 1L,
      "Table 1", "E3", 3L, 5L, FALSE, 5, "character", NA, NA, NA, NA, "Field 2", NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Normal", 1L,
      "Table 1", "F3", 3L, 6L, FALSE, 6, "character", NA, NA, NA, NA, "Field 3", NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Normal", 1L,
      "Table 1", "G3", 3L, 7L, FALSE, 7, "character", NA, NA, NA, NA, "Field 4", NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Normal", 1L,
      "Table 1", "H3", 3L, 8L, FALSE, 8, "character", NA, NA, NA, NA, "Field 5", NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Normal", 1L,
      "Table 1", "A4", 4L, 1L, FALSE, 42035, "date", NA, NA, NA, "2015-01-31", NA, NA, FALSE, NA, NA, NA, 14.4, 10.5546875, 1L, 1L, "Normal", 2L,
      "Table 1", "B4", 4L, 2L, FALSE, 1111111, "numeric", NA, NA, 1111111, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Normal", 1L,
      "Table 1", "C4", 4L, 3L, FALSE, 9, "character", NA, NA, NA, NA, "Entity 1", NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Normal", 1L,
      "Table 1", "D4", 4L, 4L, FALSE, 0.585, "numeric", NA, NA, 0.585, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 3L,
      "Table 1", "E4", 4L, 5L, FALSE, 173274.6, "numeric", NA, NA, 173274.6, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 4L,
      "Table 1", "F4", 4L, 6L, FALSE, 950842.4, "numeric", NA, NA, 950842.4, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 4L,
      "Table 1", "G4", 4L, 7L, FALSE, 1287, "numeric", NA, NA, 1287, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 4L,
      "Table 1", "H4", 4L, 8L, FALSE, 0.145, "numeric", NA, NA, 0.145, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 3L,
      "Table 1", "A5", 5L, 1L, FALSE, 42063, "date", NA, NA, NA, "2015-02-28", NA, NA, FALSE, NA, NA, NA, 14.4, 10.5546875, 1L, 1L, "Normal", 2L,
      "Table 1", "B5", 5L, 2L, FALSE, 1111111, "numeric", NA, NA, 1111111, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Normal", 1L,
      "Table 1", "C5", 5L, 3L, FALSE, 9, "character", NA, NA, NA, NA, "Entity 1", NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Normal", 1L,
      "Table 1", "D5", 5L, 4L, FALSE, 0.429, "numeric", NA, NA, 0.429, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 3L,
      "Table 1", "E5", 5L, 5L, FALSE, 105928.2, "numeric", NA, NA, 105928.2, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 4L,
      "Table 1", "F5", 5L, 6L, FALSE, 842715.7, "numeric", NA, NA, 842715.7, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 4L,
      "Table 1", "G5", 5L, 7L, FALSE, 8168.7, "numeric", NA, NA, 8168.7, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 4L,
      "Table 1", "H5", 5L, 8L, FALSE, 0.616, "numeric", NA, NA, 0.616, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 3L,
      "Table 1", "A6", 6L, 1L, FALSE, 42035, "date", NA, NA, NA, "2015-01-31", NA, NA, FALSE, NA, NA, NA, 14.4, 10.5546875, 1L, 1L, "Normal", 2L,
      "Table 1", "B6", 6L, 2L, FALSE, 2222222, "numeric", NA, NA, 2222222, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Normal", 1L,
      "Table 1", "C6", 6L, 3L, FALSE, 10, "character", NA, NA, NA, NA, "Entity 2", NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Normal", 1L,
      "Table 1", "D6", 6L, 4L, FALSE, 0.043, "numeric", NA, NA, 0.043, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 3L,
      "Table 1", "E6", 6L, 5L, FALSE, 474678.9, "numeric", NA, NA, 474678.9, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 4L,
      "Table 1", "F6", 6L, 6L, FALSE, 837229.9, "numeric", NA, NA, 837229.9, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 4L,
      "Table 1", "G6", 6L, 7L, FALSE, 453505.1, "numeric", NA, NA, 453505.1, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 4L,
      "Table 1", "H6", 6L, 8L, FALSE, 0.868, "numeric", NA, NA, 0.868, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 3L,
      "Table 1", "A7", 7L, 1L, FALSE, 42063, "date", NA, NA, NA, "2015-02-28", NA, NA, FALSE, NA, NA, NA, 14.4, 10.5546875, 1L, 1L, "Normal", 2L,
      "Table 1", "B7", 7L, 2L, FALSE, 2222222, "numeric", NA, NA, 2222222, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Normal", 1L,
      "Table 1", "C7", 7L, 3L, FALSE, 10, "character", NA, NA, NA, NA, "Entity 2", NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Normal", 1L,
      "Table 1", "D7", 7L, 4L, FALSE, 0.913, "numeric", NA, NA, 0.913, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 3L,
      "Table 1", "E7", 7L, 5L, FALSE, 267300.8, "numeric", NA, NA, 267300.8, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 4L,
      "Table 1", "F7", 7L, 6L, FALSE, 352735.9, "numeric", NA, NA, 352735.9, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 4L,
      "Table 1", "G7", 7L, 7L, FALSE, 275364.7, "numeric", NA, NA, 275364.7, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 4L,
      "Table 1", "H7", 7L, 8L, FALSE, 0.02, "numeric", NA, NA, 0.02, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 3L,
      "Table 1", "A8", 8L, 1L, FALSE, 42035, "date", NA, NA, NA, "2015-01-31", NA, NA, FALSE, NA, NA, NA, 14.4, 10.5546875, 1L, 1L, "Normal", 2L,
      "Table 1", "B8", 8L, 2L, FALSE, 3333333, "numeric", NA, NA, 3333333, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Normal", 1L,
      "Table 1", "C8", 8L, 3L, FALSE, 11, "character", NA, NA, NA, NA, "Entity 3", NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Normal", 1L,
      "Table 1", "D8", 8L, 4L, FALSE, 0.924, "numeric", NA, NA, 0.924, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 3L,
      "Table 1", "E8", 8L, 5L, FALSE, 575370.1, "numeric", NA, NA, 575370.1, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 4L,
      "Table 1", "F8", 8L, 6L, FALSE, 46911.1, "numeric", NA, NA, 46911.1, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 4L,
      "Table 1", "G8", 8L, 7L, FALSE, 83551.4, "numeric", NA, NA, 83551.4, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 4L,
      "Table 1", "H8", 8L, 8L, FALSE, 0.045, "numeric", NA, NA, 0.045, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 3L,
      "Table 1", "A9", 9L, 1L, FALSE, 42063, "date", NA, NA, NA, "2015-02-28", NA, NA, FALSE, NA, NA, NA, 14.4, 10.5546875, 1L, 1L, "Normal", 2L,
      "Table 1", "B9", 9L, 2L, FALSE, 3333333, "numeric", NA, NA, 3333333, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Normal", 1L,
      "Table 1", "C9", 9L, 3L, FALSE, 11, "character", NA, NA, NA, NA, "Entity 3", NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Normal", 1L,
      "Table 1", "D9", 9L, 4L, FALSE, 0.889, "numeric", NA, NA, 0.889, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 3L,
      "Table 1", "E9", 9L, 5L, FALSE, 103494.9, "numeric", NA, NA, 103494.9, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 4L,
      "Table 1", "F9", 9L, 6L, FALSE, 500367.3, "numeric", NA, NA, 500367.3, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 4L,
      "Table 1", "G9", 9L, 7L, FALSE, 742331.1, "numeric", NA, NA, 742331.1, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 4L,
      "Table 1", "H9", 9L, 8L, FALSE, 0.236, "numeric", NA, NA, 0.236, NA, NA, NA, FALSE, NA, NA, NA, 14.4, 8.38, 1L, 1L, "Percent", 3L
    )

  output_tibble$date <- lubridate::ymd(output_tibble$date)

  return(output_tibble)
}

vertical_formatting_input <- function() {
  list(local = list(numFmt = c("General", "mm-dd-yy", "0.0%", "General")))
}

vertical_data_output <- function() {
  output_tibble <-
    tibble::tribble(
      ~statistics_publication_name, ~date, ~abn, ~entity, ~series, ~unit, ~value,
      "Test", "31/01/2015", 1111111L, "Entity 1", "Field 1", "Percent", 0.585,
      "Test", "31/01/2015", 1111111L, "Entity 1", "Field 2", "$ million", 173274.6,
      "Test", "31/01/2015", 1111111L, "Entity 1", "Field 3", "$ million", 950842.4,
      "Test", "31/01/2015", 1111111L, "Entity 1", "Field 4", "$ million", 1287,
      "Test", "31/01/2015", 1111111L, "Entity 1", "Field 5", "Percent", 0.145,
      "Test", "28/02/2015", 1111111L, "Entity 1", "Field 1", "Percent", 0.429,
      "Test", "28/02/2015", 1111111L, "Entity 1", "Field 2", "$ million", 105928.2,
      "Test", "28/02/2015", 1111111L, "Entity 1", "Field 3", "$ million", 842715.7,
      "Test", "28/02/2015", 1111111L, "Entity 1", "Field 4", "$ million", 8168.7,
      "Test", "28/02/2015", 1111111L, "Entity 1", "Field 5", "Percent", 0.616,
      "Test", "31/01/2015", 2222222L, "Entity 2", "Field 1", "Percent", 0.043,
      "Test", "31/01/2015", 2222222L, "Entity 2", "Field 2", "$ million", 474678.9,
      "Test", "31/01/2015", 2222222L, "Entity 2", "Field 3", "$ million", 837229.9,
      "Test", "31/01/2015", 2222222L, "Entity 2", "Field 4", "$ million", 453505.1,
      "Test", "31/01/2015", 2222222L, "Entity 2", "Field 5", "Percent", 0.868,
      "Test", "28/02/2015", 2222222L, "Entity 2", "Field 1", "Percent", 0.913,
      "Test", "28/02/2015", 2222222L, "Entity 2", "Field 2", "$ million", 267300.8,
      "Test", "28/02/2015", 2222222L, "Entity 2", "Field 3", "$ million", 352735.9,
      "Test", "28/02/2015", 2222222L, "Entity 2", "Field 4", "$ million", 275364.7,
      "Test", "28/02/2015", 2222222L, "Entity 2", "Field 5", "Percent", 0.02,
      "Test", "31/01/2015", 3333333L, "Entity 3", "Field 1", "Percent", 0.924,
      "Test", "31/01/2015", 3333333L, "Entity 3", "Field 2", "$ million", 575370.1,
      "Test", "31/01/2015", 3333333L, "Entity 3", "Field 3", "$ million", 46911.1,
      "Test", "31/01/2015", 3333333L, "Entity 3", "Field 4", "$ million", 83551.4,
      "Test", "31/01/2015", 3333333L, "Entity 3", "Field 5", "Percent", 0.045,
      "Test", "28/02/2015", 3333333L, "Entity 3", "Field 1", "Percent", 0.889,
      "Test", "28/02/2015", 3333333L, "Entity 3", "Field 2", "$ million", 103494.9,
      "Test", "28/02/2015", 3333333L, "Entity 3", "Field 3", "$ million", 500367.3,
      "Test", "28/02/2015", 3333333L, "Entity 3", "Field 4", "$ million", 742331.1,
      "Test", "28/02/2015", 3333333L, "Entity 3", "Field 5", "Percent", 0.236
    )

  output_tibble$date <- lubridate::dmy(output_tibble$date)

  return(output_tibble)
}
