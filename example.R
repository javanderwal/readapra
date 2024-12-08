temp_file_path <- download_apra(publication = "qadip", cur_hist = "current")
tidyxl_data <- read_tidyxl_data(temp_file_path)
formatting_data <- read_tidyxl_formatting_data(temp_file_path)

# Key Stats




# All other tabs

#QPEX
temp_file_path <- download_apra(publication = "qpex", cur_hist = "historic")
tidyxl_data <- read_tidyxl_data(temp_file_path)
formatting_data <- read_tidyxl_formatting_data(temp_file_path)
tab_dependency_names <- get_dependency_names(tidyxl_data, formatting_data, sheet_str_detect = "Tab")
get_joined_pub_data(tidyxl_data, dependency_names = tab_dependency_names, "Tab") |> View()
