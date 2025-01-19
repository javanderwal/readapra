test_that("check_standard_user_inputs() behaves as expected", {

  # Expect success
  expect_equal(
    check_standard_user_inputs("madis", "historic"),
    dplyr::filter(
      .data = apra_stat_pubs_details,
      stat_pub_acronym == "madis",
      cur_hist == "historic"
    )
  )

  # Expect success
  expect_equal(
    check_standard_user_inputs("qadipexs", "current"),
    dplyr::filter(
      .data = apra_stat_pubs_details,
      stat_pub_acronym == "qadipexs",
      cur_hist == "current"
    )
  )

  # Error if invalid stat_pub input
  expect_error(
    check_standard_user_inputs("madisa", "historical"),
    class = "readapra_error_invalid_stat_pub_input"
    )

  # Error if invalid cur_hist input
  expect_error(
    check_standard_user_inputs("qadips", "historical"),
    class = "readapra_error_invalid_cur_hist_input"
  )
})

test_that("check_character_length_one() behaves as expected", {

  # Error if arg not vector
  expect_error(
    check_character_length_one(data.frame()),
    class = "readapra_error_input_arg_not_vector"
  )

  # Error if arg is not character vector
  expect_error(
    check_character_length_one(numeric()),
    class = "readapra_error_input_arg_not_character"
  )

  # Error if arg is not character vector of length 1.
  expect_error(
    check_character_length_one(c("test_a", "test_b", "test_c")),
    class = "readapra_error_input_arg_not_length_1"
  )
})
