# ==============================================================================
# Script ID:   03_Env_Var_Merge.R
# Project:     Quantifying the Relationship Between Environmental Variables and BirdNET Classification Confidence Using Earth Observation Data and GIS
# Section:     Data Management - Environmental Variable Integration
# Purpose:     Merge monthly environmental CSV files (Jan–Nov) into a single 
#              longitudinal Parquet dataset. Standardizes data types (Date, 
#              HourUTC, DplymID) to ensure relational integrity for the 
#              final Master Join.
# Author:      Smit Rajshekhar Patel
# ==============================================================================

# [SECTION] 1. LOAD LIBRARIES --------------------------------------------------
library(tidyverse)
library(arrow)

# [SECTION] 2. PATH CONFIGURATION ----------------------------------------------
# Raw GEE export directory
input_dir <- "C:/Users/smit1/OneDrive/Desktop/ITC_MSc/Internship_NINA/NINA/InternshipWork/Final_Scripts_Datasets_March23/Datasets/Processed_Datasets/Env_Var"

# Output for aggregated parquet file
output_file <- "C:/Users/smit1/OneDrive/Desktop/ITC_MSc/Internship_NINA/NINA/InternshipWork/Final_Scripts_Datasets_March23/Datasets/Processed_Datasets/Env_Var/Integrated_Env_var/env_variables_aggregated_Jan_Nov.parquet"

dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

# [SECTION] 3. FILE SELECTION (JAN–NOV) ----------------------------------------
all_files <- list.files(input_dir, pattern = "\\.csv$", full.names = TRUE)

# Filter out December to maintain specific project scope for this aggregation
env_files <- all_files %>%
  keep(~ !str_detect(.x, "Dec|December|12_")) %>%
  sort()

# [SECTION] 4. BATCH READ & TYPE STANDARDIZATION -------------------------------
env_data_list <- lapply(env_files, function(file) {
  
  message("Reading and standardizing: ", basename(file))
  
  df <- read_csv(file, show_col_types = FALSE)
  
  # ---- STANDARDIZE DATE FORMAT ----
  # Essential for joining with BirdNET local_date
  if ("Date" %in% names(df)) {
    df <- df %>%
      mutate(
        Date = as.Date(Date, format = "%m/%d/%Y")
      )
  }
  
  # ---- STANDARDIZE TEMPORAL TYPE ----
  if ("HourUTC" %in% names(df)) {
    df <- df %>%
      mutate(HourUTC = as.numeric(HourUTC))
  }
  
  # ---- STANDARDIZE DEPLOYMENT KEYS ----
  if ("DplymID" %in% names(df)) {
    df <- df %>%
      mutate(DplymID = as.character(DplymID))
  }
  
  # ---- OPTIONAL: DATETIME SYNCHRONIZATION ----
  if ("datetime" %in% names(df)) {
    df <- df %>%
      mutate(datetime = as.POSIXct(datetime, tz = "UTC"))
  }
  
  return(df)
})

# [SECTION] 5. DATA MERGING ----------------------------------------------------
# Efficiently bind all monthly dataframes into one master environmental table
env_merged <- bind_rows(env_data_list)

# [SECTION] 6. QUALITY CONTROL CHECKS ------------------------------------------
cat("\n--- INTEGRATION SUMMARY ---\n")
cat("Total observations:", nrow(env_merged), "\n")
cat("Total variables:", ncol(env_merged), "\n")

cat("\nDate column class:", class(env_merged$Date), "\n")
cat("Date range captured:")
print(range(env_merged$Date, na.rm = TRUE))

# [SECTION] 7. DATA EXPORT -----------------------------------------------------
# Save as Parquet for high-speed I/O in modeling scripts
write_parquet(env_merged, output_file)

cat("\n✅ Saved merged dataset to:\n", output_file, "\n")
