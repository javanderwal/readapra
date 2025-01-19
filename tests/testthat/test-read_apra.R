test_that("read_apra() behaves as expected", {
  skip_if_offline()
  skip_on_cran()

  expect_no_error(
    qadips_data <- read_apra("qadips", "current", quiet = TRUE)
  )

  expect_s3_class(qadips_data, "tbl")
})

test_that("read_apra_local() behaves as expected", {
  skip_if_offline()
  skip_on_cran()

  expect_no_error(
    qadips_data <- read_apra_local(
      file_path = download_apra("qadips", "current", quiet = TRUE),
      stat_pub = "qadips",
      cur_hist = "current"
    )
  )

  expect_s3_class(qadips_data, "tbl")
})

test_that("read_apra() error snapshots", {
  # Misspelled stat_pub input
  testthat::expect_snapshot(
    {
      read_apra(stat_pub = "madi", cur_hist = "current")
    },
    error = TRUE
  )

  # stat_pub is wrong object type
  testthat::expect_snapshot(
    {
      read_apra(stat_pub = data.frame(), cur_hist = "current")
    },
    error = TRUE
  )

  # stat_pub is wrong vector type
  testthat::expect_snapshot(
    {
      read_apra(stat_pub = 1:5, cur_hist = "current")
    },
    error = TRUE
  )

  # stat_pub is wrong length
  testthat::expect_snapshot(
    {
      read_apra(stat_pub = c("madis", "current"), cur_hist = "current")
    },
    error = TRUE
  )

  # Misspelled cur_hist input
  testthat::expect_snapshot(
    {
      read_apra(stat_pub = "madi", cur_hist = "curren")
    },
    error = TRUE
  )

  # cur_hist is wrong object type
  testthat::expect_snapshot(
    {
      read_apra(stat_pub = "madis", cur_hist = data.frame())
    },
    error = TRUE
  )

  # cur_hist is wrong vector type
  testthat::expect_snapshot(
    {
      read_apra(stat_pub = "madis", cur_hist = 1:5)
    },
    error = TRUE
  )

  # cur_hist is wrong length
  testthat::expect_snapshot(
    {
      read_apra(stat_pub = "madis", cur_hist = c("current", "historic"))
    },
    error = TRUE
  )

  # quiet is wrong object type
  testthat::expect_snapshot(
    {
      read_apra(
        stat_pub = "madis",
        cur_hist = "current",
        quiet = data.frame()
      )
    },
    error = TRUE
  )

  # quiet is wrong vector type
  testthat::expect_snapshot(
    {
      read_apra(
        stat_pub = "madis",
        cur_hist = "current",
        quiet = "TRUE"
      )
    },
    error = TRUE
  )

  # quiet is wrong length
  testthat::expect_snapshot(
    {
      read_apra(
        stat_pub = "madis",
        cur_hist = "current",
        quiet = c(TRUE, TRUE)
      )
    },
    error = TRUE
  )

  # overwrite is wrong object type
  testthat::expect_snapshot(
    {
      read_apra(
        stat_pub = "madis",
        cur_hist = "current",
        overwrite = data.frame()
      )
    },
    error = TRUE
  )

  # overwrite is wrong vector type
  testthat::expect_snapshot(
    {
      read_apra(
        stat_pub = "madis",
        cur_hist = "current",
        overwrite = "TRUE"
      )
    },
    error = TRUE
  )

  # overwrite is wrong length
  testthat::expect_snapshot(
    {
      read_apra(
        stat_pub = "madis",
        cur_hist = "current",
        overwrite = c(TRUE, TRUE)
      )
    },
    error = TRUE
  )
})

test_that("read_apra() errors are correct class", {
  # Misspelled stat_pub input
  expect_error(
    read_apra(stat_pub = "madi", cur_hist = "current"),
    class = "readapra_error_invalid_stat_pub_input"
  )

  # stat_pub is wrong object type
  expect_error(
    read_apra(stat_pub = data.frame(), cur_hist = "current"),
    class = "readapra_error_input_arg_not_vector"
  )

  # stat_pub is wrong vector type
  expect_error(
    read_apra(stat_pub = 1:5, cur_hist = "current"),
    class = "readapra_error_input_arg_not_character"
  )

  # stat_pub is wrong length
  expect_error(
    read_apra(stat_pub = c("madis", "current"), cur_hist = "current"),
    class = "readapra_error_input_arg_not_length_1"
  )

  # Misspelled cur_hist input
  expect_error(
    read_apra(stat_pub = "madis", cur_hist = "curren"),
    class = "readapra_error_invalid_cur_hist_input"
  )

  # cur_hist is wrong object type
  expect_error(
    read_apra(stat_pub = "madis", cur_hist = data.frame()),
    class = "readapra_error_input_arg_not_vector"
  )

  # cur_hist is wrong vector type
  expect_error(
    read_apra(stat_pub = "madis", cur_hist = 1:5),
    class = "readapra_error_input_arg_not_character"
  )

  # cur_hist is wrong length
  expect_error(
    read_apra(stat_pub = "madis", cur_hist = c("current", "historic")),
    class = "readapra_error_input_arg_not_length_1"
  )

  # quiet is wrong object type
  expect_error(
    read_apra(
      stat_pub = "madis",
      cur_hist = "current",
      quiet = data.frame()
    ),
    class = "readapra_error_input_arg_not_vector"
  )

  # quiet is wrong vector type
  expect_error(
    read_apra(
      stat_pub = "madis",
      cur_hist = "current",
      quiet = "TRUE"
    ),
    class = "readapra_error_input_arg_not_logical"
  )

  # quiet is wrong length
  expect_error(
    read_apra(
      stat_pub = "madis",
      cur_hist = "current",
      quiet = c(TRUE, TRUE)
    ),
    class = "readapra_error_input_arg_not_length_1"
  )

  # overwrite is wrong object type
  expect_error(
    read_apra(
      stat_pub = "madis",
      cur_hist = "current",
      overwrite = data.frame()
    ),
    class = "readapra_error_input_arg_not_vector"
  )

  # overwrite is wrong vector type
  expect_error(
    read_apra(
      stat_pub = "madis",
      cur_hist = "current",
      overwrite = "TRUE"
    ),
    class = "readapra_error_input_arg_not_logical"
  )

  # overwrite is wrong length
  expect_error(
    read_apra(
      stat_pub = "madis",
      cur_hist = "current",
      overwrite = c(TRUE, TRUE)
    ),
    class = "readapra_error_input_arg_not_length_1"
  )
})

test_that("read_apra_local() error snapshots", {
  # Skip due to divergence across OS systems.
  skip_on_os(c("mac", "linux", "solaris"))

  # Invalid file_path
  testthat::expect_snapshot(
    {
      read_apra_local(
        file_path = "C:/Users/test_user/test_file.xlsx",
        stat_pub = "madi",
        cur_hist = "current"
      )
    },
    error = TRUE
  )
})

test_that("read_apra_local_local() errors are correct class", {
  # Invalid file_path
  expect_error(
    read_apra_local(
      file_path = "C:/Users/test_user/test_file.xlsx",
      stat_pub = "madi",
      cur_hist = "current"
    ),
    class = "read_apra_error_file_path_does_not_exist"
  )
})
