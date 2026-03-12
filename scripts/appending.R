# ----------------------------------------
# PM POSHAN – Append Cleaned CSV Files
# ----------------------------------------

library(dplyr)
library(readr)

# ----------------------------------------
# paths
# ----------------------------------------

data_path <- "D:/OneDrive - Azim Premji Foundation/Documents/iic_stuff/meal_not_served"
base_path <- file.path(data_path, "csv_cleaned")
out_path <- file.path(data_path, "csv_cleaned")


# ----------------------------------------
# get all csv files
# ----------------------------------------

csv_files <- list.files(
  base_path,
  pattern = "\\.csv$",
  full.names = TRUE
)


# ----------------------------------------
# read and append
# ----------------------------------------

combined_data <- csv_files %>%
  lapply(function(f){
    
    df <- read_csv(f, show_col_types = FALSE)
    
    df %>%
      mutate(
        month = as.numeric(month),
        day   = as.numeric(day),
        year  = as.numeric(year)
      )
    
  }) %>%
  bind_rows()

# ----------------------------------------
# sort by date
# ----------------------------------------

combined_data <- combined_data %>%
  arrange(year, month, day)

# ----------------------------------------
# save final dataset
# ----------------------------------------
write_csv(
  combined_data,
  file.path(base_path, "ams_mns_combined.csv")
)

