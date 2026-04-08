# ==============================================================================
# Script ID:   06_Audit_Implementation.R
# Project:     Quantifying the Relationship Between Environmental Variables and BirdNET Classification Confidence Using Earth Observation Data and GIS
# Section:     Methodology - Data Harmonization (Final Cleaning)
# Purpose:     Execute removal of missing values, out-of-bounds records, and 
#              duplicates based on findings from the diagnostic audit.
# ==============================================================================

# [SECTION] Load Libraries -----------------------------------------------------
library(arrow)
library(dplyr)
library(here) # Used for relative path management

# [SECTION] 1. Define Paths ----------------------------------------------------
# Using here() to maintain project portability across machines
input_path  <- here("Final_Scripts_Datasets_March23", "Datasets", "Processed_Datasets", "PreAggregated_Processed", "BirdNET_JanToDec_FINAL_TEMPORAL.parquet")
output_path <- here("Final_Scripts_Datasets_March23", "Datasets", "Processed_Datasets", "PreAggregated_Processed", "BirdNET_JanToDec_CLEANED.parquet")

# [SECTION] 2. Read & Clean ----------------------------------------------------
# We use open_dataset to keep it "lazy" until the last moment
dataset <- open_dataset(input_path)

cat("Starting cleaning process for 74M+ rows...\n")

# Stage 1: Filter out NAs and out-of-bounds values
data_clean_stage1 <- dataset %>%
  filter(
    !is.na(timestamp_utc),
    !is.na(timestamp_local),
    !is.na(local_date),
    !is.na(local_hour),
    confidence >= 0,
    confidence <= 1,
    local_hour >= 0,
    local_hour <= 23
  ) %>%
  collect() # Pull into RAM only after filtering to save space

# Stage 2: Remove duplicates
data_clean_final <- data_clean_stage1 %>%
  distinct(filename, start_time, scientific_name, .keep_all = TRUE)

# [SECTION] 3. Summarize & Save ------------------------------------------------
original_rows <- nrow(dataset) # Count rows without loading full data
final_rows    <- nrow(data_clean_final)

cat("\n===== CLEANING SUMMARY =====\n")
cat("Original rows:      ", original_rows, "\n")
cat("Cleaned rows:        ", final_rows, "\n")
cat("Total rows removed: ", original_rows - final_rows, "\n")

write_parquet(data_clean_final, output_path)

cat("\nSuccess! Clean dataset saved to:\n", output_path, "\n")
