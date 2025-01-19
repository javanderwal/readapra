# read_apra() error snapshots

    Code
      read_apra(stat_pub = "madi", cur_hist = "current")
    Condition
      Error in `read_apra()`:
      ! `stat_pub` must be an acronym of a statistical publication, not "madi".
      i Did you mean "madis"?
      i A list of valid acronyms is available in the `apra_stat_pubs` dataset.

---

    Code
      read_apra(stat_pub = data.frame(), cur_hist = "current")
    Condition
      Error in `read_apra()`:
      ! `stat_pub` must be a <character> vector, not a <data.frame> object.

---

    Code
      read_apra(stat_pub = 1:5, cur_hist = "current")
    Condition
      Error in `read_apra()`:
      ! `stat_pub` must be a <character> vector, not a <integer> vector.

---

    Code
      read_apra(stat_pub = c("madis", "current"), cur_hist = "current")
    Condition
      Error in `read_apra()`:
      ! `stat_pub` must be a character vector of length 1, not length 2.

---

    Code
      read_apra(stat_pub = "madi", cur_hist = "curren")
    Condition
      Error in `read_apra()`:
      ! `stat_pub` must be an acronym of a statistical publication, not "madi".
      i Did you mean "madis"?
      i A list of valid acronyms is available in the `apra_stat_pubs` dataset.

---

    Code
      read_apra(stat_pub = "madis", cur_hist = data.frame())
    Condition
      Error in `read_apra()`:
      ! `cur_hist` must be a <character> vector, not a <data.frame> object.

---

    Code
      read_apra(stat_pub = "madis", cur_hist = 1:5)
    Condition
      Error in `read_apra()`:
      ! `cur_hist` must be a <character> vector, not a <integer> vector.

---

    Code
      read_apra(stat_pub = "madis", cur_hist = c("current", "historic"))
    Condition
      Error in `read_apra()`:
      ! `cur_hist` must be a character vector of length 1, not length 2.

---

    Code
      read_apra(stat_pub = "madis", cur_hist = "current", quiet = data.frame())
    Condition
      Error in `read_apra()`:
      ! `quiet` must be a <logical> vector, not a <data.frame> object.

---

    Code
      read_apra(stat_pub = "madis", cur_hist = "current", quiet = "TRUE")
    Condition
      Error in `read_apra()`:
      ! `quiet` must be a <logical> vector, not a <character> vector.

---

    Code
      read_apra(stat_pub = "madis", cur_hist = "current", quiet = c(TRUE, TRUE))
    Condition
      Error in `read_apra()`:
      ! `quiet` must be a logical vector of length 1, not length 2.

---

    Code
      read_apra(stat_pub = "madis", cur_hist = "current", overwrite = data.frame())
    Condition
      Error in `read_apra()`:
      ! `overwrite` must be a <logical> vector, not a <data.frame> object.

---

    Code
      read_apra(stat_pub = "madis", cur_hist = "current", overwrite = "TRUE")
    Condition
      Error in `read_apra()`:
      ! `overwrite` must be a <logical> vector, not a <character> vector.

---

    Code
      read_apra(stat_pub = "madis", cur_hist = "current", overwrite = c(TRUE, TRUE))
    Condition
      Error in `read_apra()`:
      ! `overwrite` must be a logical vector of length 1, not length 2.

# read_apra_local() error snapshots

    Code
      read_apra_local(file_path = "C:/Users/test_user/test_file.xlsx", stat_pub = "madi",
        cur_hist = "current")
    Condition
      Error in `read_apra_local()`:
      x No such file exists at: 'C:\Users\test_user\test_file.xlsx'.
      ! Please check you have specified the correct file path.

