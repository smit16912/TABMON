# ==============================================================================
# Script ID:   09_Data_Aggregation.R
# Project:     Quantifying Environmental Variables & BirdNET Confidence
# Section:     Methodology - Data Harmonization (Aggregation)
# Purpose:     Aggregate 3-second BirdNET detections into hourly summaries per 
#              species and deployment to match environmental data resolution.
# ==============================================================================

# [SECTION] Load Libraries -----------------------------------------------------
library(arrow)
library(dplyr)
library(here) # Used for relative path management

# [SECTION] 1. INPUT (FINAL CLEANED DATASET) -----------------------------------
# Using here() to maintain project portability across machines
INPUT_PARQUET <- here("Final_Scripts_Datasets_March23", "Datasets", "Processed_Datasets", "PreAggregated_Processed", "BirdNET_JanToDec_CLEANED.parquet")

# [SECTION] 2. OUTPUT DIRECTORY ------------------------------------------------
OUTPUT_DIR <- here("DataAnalyses", "Data", "ProcessedData", "BirdNET_JantoDec", "Aggregation", "Final_Aggregation")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

OUTPUT_PARQUET <- file.path(
  OUTPUT_DIR,
  "BirdNET_Hourly_Aggregated_Species_Level.parquet"
)

# [SECTION] 3. OPEN DATASET (LAZY) ---------------------------------------------
dataset <- open_dataset(INPUT_PARQUET)

cat("🚀 Starting aggregation on large dataset...\n")

# [SECTION] 4. AGGREGATION -----------------------------------------------------
aggregated_data <- dataset %>%
  group_by(
    deployment_id,
    scientific_name,
    local_date,
    local_hour
  ) %>%
  summarise(
    
    # ---------------------------
    # DETECTABILITY PROXY
    # ---------------------------
    n_detections = n(),
    
    # ---------------------------
    # CONFIDENCE (PRIMARY VARIABLE)
    # ---------------------------
    confidence_mean     = mean(confidence, na.rm = TRUE),
    confidence_variance = var(confidence, na.rm = TRUE),
    
    # ---------------------------
    # UNCERTAINTY (SECONDARY)
    # ---------------------------
    uncertainty_mean     = mean(max_uncertainty, na.rm = TRUE),
    uncertainty_variance = var(max_uncertainty, na.rm = TRUE),
    
    .groups = "drop"
  )

# [SECTION] 5. WRITE OUTPUT (FORCE SINGLE FILE) --------------------------------
aggregated_data %>%
  collect() %>%
  write_parquet(OUTPUT_PARQUET)

# [SECTION] 6. BASIC VALIDATION ------------------------------------------------
cat("\n===== AGGREGATION COMPLETE =====\n")

cat("Checking output file existence:\n")
print(file.exists(OUTPUT_PARQUET))

# Quick structure check
agg_check <- open_dataset(OUTPUT_PARQUET)

summary_stats <- agg_check %>%
  summarise(
    total_rows = n(),
    unique_species = n_distinct(scientific_name),
    unique_deployments = n_distinct(deployment_id)
  ) %>%
  collect()

cat("\n===== OUTPUT SUMMARY =====\n")
print(summary_stats)

cat("\n✅ Aggregated dataset ready for modelling.\n")
cat("📁 Saved at:\n", OUTPUT_PARQUET, "\n")
