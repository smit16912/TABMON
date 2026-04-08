# ==============================================================================
# Script ID:   10_PostAggregation_SpeciesFilter.R
# Project:     Quantifying Environmental Variables & BirdNET Confidence
# Section:     Methodology - Data Harmonization (Subset Selection)
# Purpose:     Subset the hourly aggregated dataset to focus on specific 
#              species of interest for final modeling and analysis.
# ==============================================================================

# [SECTION] Load Libraries -----------------------------------------------------
library(arrow)
library(dplyr)
library(here) # Used for relative path management

# [SECTION] 1. INPUT (AGGREGATED DATASET) --------------------------------------
# Using here() to maintain project portability across machines
INPUT_PARQUET <- here("DataAnalyses", "Data", "ProcessedData", "BirdNET_JantoDec", "Aggregation", "Final_Aggregation", "BirdNET_Hourly_Aggregated_Species_Level.parquet")

# [SECTION] 2. OUTPUT ----------------------------------------------------------
OUTPUT_PATH <- here("DataAnalyses", "Data", "ProcessedData", "BirdNET_JantoDec", "Aggregation", "Final_Aggregation", "BirdNET_Filtered_SelectedSpecies.parquet")

# [SECTION] 3. SPECIES FILTER (SCIENTIFIC NAMES ONLY) --------------------------
species_filter <- c(
  "Hirundo rustica",
  "Poecile montanus",
  "Motacilla alba",
  "Larus canus",
  "Tringa ochropus",
  "Parus major",
  "Sylvia borin",
  "Phylloscopus trochilus",
  "Carduelis carduelis",
  "Spinus spinus",
  "Sitta europaea",
  "Pica pica",
  "Dendrocopos major",
  "Chloris chloris",
  "Emberiza citrinella",
  "Fringilla montifringilla",
  "Cyanistes caeruleus",
  "Prunella modularis",
  "Fringilla coelebs",
  "Columba palumbus",
  "Apus apus"
)

# [SECTION] 4. OPEN DATASET ----------------------------------------------------
dataset <- open_dataset(INPUT_PARQUET)

cat("Filtering selected species...\n")

# [SECTION] 5. FILTER ----------------------------------------------------------
filtered_data <- dataset %>%
  filter(scientific_name %in% species_filter)

# [SECTION] 6. SAVE ------------------------------------------------------------
filtered_data %>%
  collect() %>%
  write_parquet(OUTPUT_PATH)

# [SECTION] 7. VALIDATION ------------------------------------------------------
cat("\n===== FILTERING COMPLETE =====\n")

check <- open_dataset(OUTPUT_PATH)

summary_check <- check %>%
  summarise(
    total_rows = n(),
    unique_species = n_distinct(scientific_name),
    unique_deployments = n_distinct(deployment_id)
  ) %>%
  collect()

print(summary_check)

cat("\nSpecies retained:\n")
print(unique(species_filter))

cat("\n✅ Filtered dataset ready for modelling.\n")
cat("📁 Saved at:\n", OUTPUT_PATH, "\n")
