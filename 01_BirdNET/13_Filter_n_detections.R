# ==============================================================================
# Script ID:   13_Filter_n_detections.R
# Project:     Quantifying the Relationship Between Environmental Variables and BirdNET Classification Confidence Using Earth Observation Data and GIS
# Section:     Methodology - Data Harmonization (Reliability Filtering)
# Purpose:     Filter the dataset to include only records with more than one 
#              detection (n > 1) per hour to ensure statistical stability.
# ==============================================================================

# [SECTION] Load Libraries -----------------------------------------------------
library(arrow)
library(dplyr)
library(here) # Used for relative path management

# [SECTION] 1. INPUT -----------------------------------------------------------
# Using here() to maintain project portability across machines
INPUT <- here("Final_Scripts_Datasets_March23", "Datasets", "Processed_Datasets", "Aggregated", "BirdNET_Filtered_SelectedSpecies.parquet")

# [SECTION] 2. OUTPUT (final modelling dataset) --------------------------------
OUTPUT <- here("Final_Scripts_Datasets_March23", "Datasets", "Processed_Datasets", "Aggregated", "BirdNET_FINAL_MODEL_INPUT_Ngt1.parquet")

# [SECTION] 3. PROCESS ---------------------------------------------------------
# Loading the parquet file
dataset <- read_parquet(INPUT)

# Filtering for high-performance (N > 1) records
final_data <- dataset %>%
  filter(n_detections > 1)

# [SECTION] 4. SAVE ------------------------------------------------------------
write_parquet(final_data, OUTPUT)

cat("✅ Final modelling dataset saved.\n")
cat("📁 Path:", OUTPUT, "\n")
