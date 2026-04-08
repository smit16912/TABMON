# ==============================================================================
# Script ID:   01_Data_Merge.R
# Project:     Quantifying the Relationship Between Environmental Variables and BirdNET Classification Confidence Using Earth Observation Data and GIS
# Section:     Methodology - Data Harmonization (Parquet Management)
# Purpose:     Merge Hive-partitioned raw BirdNET data into a unified Parquet file.
# ==============================================================================

# [SECTION] Load Libraries -----------------------------------------------------
library(arrow)
library(dplyr)
library(here) # Used for relative path management

# [SECTION] Define the Input and Output Paths ----------------------------------
# We use here() to ensure the project remains portable across different machines
raw_data_path <- here("Data", "ProcessedData", "BirdNET_JantoDec", "BirdNET_Raw")

output_dir <- here("Final_Scripts_Datasets_March23", "Datasets", "Processed_Datasets", "PreAggregated_Processed")

output_file <- file.path(output_dir, "BirdNET_JanToDec_ALL_COUNTRIES_MERGED.parquet")

# Ensure the output directory exists
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# [SECTION] Data Discovery & Processing ----------------------------------------
# Because your folders are named 'country=France', 'device_id=...', 
# arrow automatically creates these as columns in the data.
ds <- open_dataset(raw_data_path, partitioning = c("country", "device_id"))

# Process and Save
# We 'collect()' to execute the merge and then save to the new location.
message("🚀 Merging Hive-partitioned folders into a unified Parquet file...")

ds %>%
  collect() %>%
  write_parquet(output_file)

message("✅ SUCCESS: Merged file saved to:")
message(output_file)

# [SECTION] Technical Audit (Verification) -------------------------------------
merged_data <- read_parquet(output_file)
cat("\n--- Dataset Summary ---\n")
cat("Total Rows:", nrow(merged_data), "\n")
cat("Columns Discovered:", paste(colnames(merged_data), collapse = ", "), "\n")
cat("Countries included:", paste(unique(merged_data$country), collapse = ", "), "\n")
