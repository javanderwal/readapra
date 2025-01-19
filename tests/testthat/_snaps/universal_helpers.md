# check_valid_file_path() behaves as expected

    Code
      wrapper_check_valid_file_path("C:/Users/test_user/test_file.xlsx")
    Condition
      Error in `wrapper_check_valid_file_path()`:
      x No such file exists at: 'C:\Users\test_user\test_file.xlsx'.
      ! Please check you have specified the correct file path.

---

    Code
      wrapper_check_valid_file_path(data.frame())
    Condition
      Error in `wrapper_check_valid_file_path()`:
      ! `file_path` must be a <character> vector, not a <data.frame> object.

---

    Code
      wrapper_check_valid_file_path(1:5)
    Condition
      Error in `wrapper_check_valid_file_path()`:
      ! `file_path` must be a <character> vector, not a <integer> vector.

---

    Code
      wrapper_check_valid_file_path(c("test_a", "test_b"))
    Condition
      Error in `wrapper_check_valid_file_path()`:
      ! `file_path` must be a character vector of length 1, not length 2.

# check_logical_length_one() behaves as expected

    Code
      wrapper_check_logical_length_one(data.frame())
    Condition
      Error in `wrapper_check_logical_length_one()`:
      ! `data.frame()` must be a <logical> vector, not a <data.frame> object.

---

    Code
      wrapper_check_logical_length_one(1:5)
    Condition
      Error in `wrapper_check_logical_length_one()`:
      ! `1:5` must be a <logical> vector, not a <integer> vector.

---

    Code
      wrapper_check_logical_length_one(c(TRUE, FALSE, TRUE))
    Condition
      Error in `wrapper_check_logical_length_one()`:
      ! `c(TRUE, FALSE, TRUE)` must be a logical vector of length 1, not length 3.

# check_character_length_one() behaves as expected

    Code
      wrapper_check_character_length_one(data.frame())
    Condition
      Error in `wrapper_check_character_length_one()`:
      ! `data.frame()` must be a <character> vector, not a <data.frame> object.

---

    Code
      wrapper_check_character_length_one(1:5)
    Condition
      Error in `wrapper_check_character_length_one()`:
      ! `1:5` must be a <character> vector, not a <integer> vector.

---

    Code
      wrapper_check_character_length_one(c("test_a", "test_b"))
    Condition
      Error in `wrapper_check_character_length_one()`:
      ! `c("test_a", "test_b")` must be a character vector of length 1, not length 2.

