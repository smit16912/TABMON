# ==============================================================================
# Script ID:   07_Post_Audit_Summary.R
# Project:     Quantifying Environmental Variables & BirdNET Confidence
# Section:     Methodology - Data Harmonization (Final Structural Audit)
# Purpose:     Generate a comprehensive metadata and structural summary of the 
#              processed dataset to confirm readiness for environmental analysis.
# ==============================================================================

# [SECTION] Load Libraries -----------------------------------------------------
library(arrow)
library(dplyr)
library(here) # Used for relative path management

# [SECTION] 1. Open Dataset ----------------------------------------------------
# Using here() to maintain project portability across machines
PARQUET_PATH <- here("Final_Scripts_Datasets_March23", "Datasets", "Processed_Datasets", "PreAggregated_Processed", "BirdNET_JanToDec_FINAL_TEMPORAL.parquet")

dataset <- open_dataset(PARQUET_PATH)

message("\n================ DATASET STRUCTURE ================")
print(dataset$schema)

# [SECTION] 2. Dataset Size & Cardinality --------------------------------------
message("\n================ DATASET SIZE ================")

size_stats <- dataset %>%
  summarize(
    Total_Rows = n(),
    Unique_Species = n_distinct(scientific_name),
    Unique_Deployments = n_distinct(deployment_id),
    .groups = "drop"
  ) %>%
  collect()

print(size_stats)

# [SECTION] 3. Confidence Summary ----------------------------------------------
message("\n================ CONFIDENCE SUMMARY ================")

confidence_stats <- dataset %>%
  summarize(
    Min_Confidence  = min(confidence, na.rm = TRUE),
    Max_Confidence  = max(confidence, na.rm = TRUE),
    Mean_Confidence = mean(confidence, na.rm = TRUE),
    Missing_Confidence = sum(is.na(confidence)),
    .groups = "drop"
  ) %>%
  collect()

print(confidence_stats)

# [SECTION] 5. Sample Records (Sanity Check) -----------------------------------
message("\n================ SAMPLE RECORDS (LOCAL TIME FOCUS) ================")

# Selecting specifically to see the results of our hard work
dataset %>% 
  select(country, scientific_name, timestamp_utc, timestamp_local, local_hour) %>% 
  head(5) %>% 
  collect() %>% 
  print()

message("\n✅ Final Metadata summary completed. You are ready for analysis!")
