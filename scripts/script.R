rm(list = ls())
Sys.setenv(JAVA_HOME = "C:/Program Files/Eclipse Adoptium/jdk-25.0.2.10-hotspot")

library(rJava)
library(tabulapdf)
library(dplyr)
library(stringr)

# ---------------------------
#define paths (specify data path)
# ---------------------------
#data_path <- ""
base_path <- file.path(data_path, "meal_not_served")
out_path <- file.path(base_path, "clean_csv")

# ---------------------------
#extraction
# ---------------------------

process_file <- function(file_stub){
  
  file <- paste0(base_path, file_stub, ".pdf")
  
  tables <- extract_tables(file, method = "stream")
  
  df <- do.call(rbind, lapply(tables, as.data.frame))
  df <- as.data.frame(df)
  
  #cleaning
  df <- df[-(1:4), ]
  rownames(df) <- NULL
  df <- df[-nrow(df), ]
  df <- df[, -c(7, 8, 11, 14)]
  
  colnames(df) <- c(
    "sno","state","total_schools","reported_schools","meal_not_served",
    "food_grains","fund","cook_cum_helper","ngo_shg","holidays","others"
  )
  
# ---------------------------
#extracting date from file name
# ---------------------------
  parts <- strsplit(file_stub, "_")[[1]]
  
  month <- parts[3]
  day   <- parts[4]
  year  <- "2026"
  
  df <- df %>%
    mutate(
      month = month,
      day   = day,
      year  = year,
      .before = sno
    )
  
# ---------------------------
#save as csv
# ---------------------------
  write.csv(df,
            paste0(out_path, file_stub, ".csv"),
            row.names = FALSE)
}

# ---------------------------
#file names
# ---------------------------

files_1 <- sprintf("ams_mns_1_%02d", 5:31)
files_2 <- sprintf("ams_mns_2_%02d", 1:4)

all_files <- c(files_1, files_2)

# ---------------------------
#loop
# ---------------------------

for(f in all_files){
  try(process_file(f))
}

#rm(list = ls())

# ---------------------------
#loop for 09, 10, 12, 14, 17
# ---------------------------

library(tabulapdf)
library(tidyr)
library(dplyr)
library(stringr)

base_path <- file.path(data_path, "meal_not_served")
out_path <- file.path(base_path, "clean_csv")

process_file <- function(file_stub){
  
  file <- paste0(base_path, file_stub, ".pdf")
  
  tables <- extract_tables(file, method = "stream")
  
  df <- do.call(rbind, lapply(tables, as.data.frame))
  df <- as.data.frame(df)
  
  # -------- cleaning --------
  df <- df[-(1:4), ]
  rownames(df) <- NULL
  
  # safe column removal (prevents crashes)
  drop_cols <- c(7, 8, 11)
  drop_cols <- drop_cols[drop_cols <= ncol(df)]
  df <- df[, -drop_cols]
  
  df <- df[-nrow(df), ]
  
  # column check
  if(ncol(df) != 11){
    cat("Skipped:", file_stub, "- found", ncol(df), "columns\n")
    return(NULL)
  }
  
  colnames(df) <- c(
    "sno","state","total_schools","reported_schools","meal_not_served",
    "food_grains","fund","cook_cum_helper","ngo_shg","holidays","others"
  )
  
  # -------- date extraction --------
  parts <- strsplit(file_stub, "_")[[1]]
  
  df <- df %>%
    mutate(
      month = parts[3],
      day   = parts[4],
      year  = "2026",
      .before = sno
    )
  
  # -------- save --------
  write.csv(df,
            paste0(out_path, file_stub, ".csv"),
            row.names = FALSE)
  
}

# ---------------------------
# only selected files
# ---------------------------

files <- c(
  "ams_mns_1_09",
  "ams_mns_1_10",
  "ams_mns_1_12",
  "ams_mns_1_13",
  "ams_mns_1_14",
  "ams_mns_1_17"
)

for(f in files){
  tryCatch(
    process_file(f),
    error = function(e) cat("Failed:", f, "-", e$message, "\n")
  )
}


# ---------------------------
# sundays
# ---------------------------

library(tabulapdf)
library(tidyr)
library(dplyr)
library(stringr)

base_path <- file.path(data_path, "meal_not_served")
out_path <- file.path(base_path, "clean_csv")

process_file <- function(file_stub){
  
  file <- paste0(base_path, file_stub, ".pdf")
 
  tables <- extract_tables(file, method = "stream")
  
  df <- do.call(rbind, lapply(tables, as.data.frame))
  df <- as.data.frame(df)
  
  # cleaning
  df <- df[-(1:4), ]
  rownames(df) <- NULL
  
  # remove last 5 columns 
  if (ncol(df) > 5) {
    df <- df[, 1:(ncol(df) - 5)]
  }
  
  # column check
  if (ncol(df) != 11) {
    cat("Skipped:", file_stub, "- found", ncol(df), "columns\n")
    return(NULL)
  }
  
  colnames(df) <- c(
    "sno","state","total_schools","reported_schools","meal_not_served",
    "food_grains","fund","cook_cum_helper","ngo_shg","holidays","others"
  )
  
  # first row numeric fix 
  if (nrow(df) > 0) {
    numeric_cols <- 3:ncol(df)
    df[1, numeric_cols] <- "0"
  }
  
  # -------- date extraction --------
  parts <- strsplit(file_stub, "_")[[1]]
  
  df <- df %>%
    mutate(
      month = parts[3],
      day   = parts[4],
      year  = "2026",
      .before = sno
    )
  
  # clean TOTAL
  df$state[df$state == "TOTAL"] <- NA
  
  # -------- save --------
  write.csv(df,
            paste0(out_path, file_stub, ".csv"),
            row.names = FALSE)
}

# ---------------------------
# 11, 18, 25, 01
# ---------------------------

files <- c(
  "ams_mns_1_11",
  "ams_mns_1_18",
  "ams_mns_1_25",
  "ams_mns_2_01"
)

for(f in files){
  tryCatch(
    process_file(f),
    error = function(e) cat("Failed:", f, "-", e$message, "\n")
  )
}

rm(list = ls())

library(dplyr)
library(readr)

base_path <- file.path(data_path, "meal_not_served")
out_path <- file.path(base_path, "clean_csv")

files_jan <- sprintf("ams_mns_1_%02d.csv", 5:31)
files_feb <- sprintf("ams_mns_2_%02d.csv", 1:4)

all_files <- c(files_jan, files_feb)

file_paths <- file.path(base_path, all_files)

combined_data <- file_paths %>%
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

combined_data <- combined_data %>%
  arrange(month, day)

write_csv(
  combined_data,
  paste0(base_path, "ams_mns_combined_r.csv")
)




