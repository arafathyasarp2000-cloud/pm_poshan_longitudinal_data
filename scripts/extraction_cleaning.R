Sys.setenv(JAVA_HOME = "C:/Program Files/Eclipse Adoptium/jdk-25.0.2.10-hotspot")

library(rJava)
library(tabulapdf)
library(dplyr)
library(stringr)


data_path <- "D:/OneDrive - Azim Premji Foundation/Documents/iic_stuff"
base_path <- file.path(data_path, "meal_not_served")
out_path <- file.path(base_path, "csv_cleaned")

dir.create(out_path, showWarnings = FALSE)

process_file <- function(file_path){
  
  file_stub <- tools::file_path_sans_ext(basename(file_path))
  parts <- strsplit(file_stub, "_")[[1]]
  
  month <- parts[3]
  day   <- parts[4]
  
  cat("Processing:", file_stub, "\n")
  
  tables <- extract_tables(file_path, method = "stream")
  
  df <- do.call(rbind, lapply(tables, as.data.frame))
  df <- as.data.frame(df)
  
  df <- df[-(1:4), ]
  rownames(df) <- NULL
  
  # -------- Sunday files --------
  if(day %in% c("11","18","25","01")){
    
    # remove last 5 columns safely
    if(ncol(df) > 5){
      df <- df[, 1:(ncol(df) - 5)]
    }
    
    # fix first row numeric values safely
    if(nrow(df) > 0 && ncol(df) >= 3){
      numeric_cols <- 3:ncol(df)
      df[1, numeric_cols] <- "0"
    }
    
    # clean TOTAL row
    if("state" %in% colnames(df)){
      df$state[df$state == "TOTAL"] <- NA
    }
  }
  
  # -------- irregular column days --------
  else if(day %in% c("09","10","12","13","14","17")){
    
    drop_cols <- c(7,8,11)
    drop_cols <- drop_cols[drop_cols <= ncol(df)]
    df <- df[, -drop_cols]
    
    df <- df[-nrow(df), ]
    
  }
  
  # -------- normal days --------
  else{
    
    drop_cols <- c(7,8,11,14)
    drop_cols <- drop_cols[drop_cols <= ncol(df)]
    df <- df[, -drop_cols]
    
    df <- df[-nrow(df), ]
    
  }
  
  # column validation
  if(ncol(df) != 11){
    cat("Skipped:", file_stub, "- found", ncol(df), "columns\n")
    return(NULL)
  }
  
  colnames(df) <- c(
    "sno","state","total_schools","reported_schools","meal_not_served",
    "food_grains","fund","cook_cum_helper","ngo_shg","holidays","others"
  )
  
  df <- df %>%
    mutate(
      month = month,
      day   = day,
      year  = "2026",
      .before = sno
    )
  
  write.csv(
    df,
    file.path(out_path, paste0(file_stub, ".csv")),
    row.names = FALSE
  )
  
  cat("Saved:", file_stub, "\n")
}

pdf_files <- list.files(
  base_path,
  pattern = "\\.pdf$",
  full.names = TRUE
)

for(f in pdf_files){
  tryCatch(
    process_file(f),
    error = function(e) cat("Failed:", f, "-", e$message, "\n")
  )
}

cat("All files processed\n")