# ==============================================================================
# Script ID:   02_Standardize_Nomenclature.R
# Project:     Quantifying the Relationship Between Environmental Variables and BirdNET Classification Confidence Using Earth Observation Data and GIS
# Section:     Methodology - Data Harmonization (Column Cleaning)
# Purpose:     Clean and standardize column names (lowercase, no spaces) for 
#              modeling consistency.
# ==============================================================================

# [SECTION] Load Libraries -----------------------------------------------------
library(arrow)
library(dplyr)
library(stringr)
library(here) # Used for relative path management

# [SECTION] 1. SET PATHS -------------------------------------------------------
# Using here() to maintain project portability
input_path  <- here("Final_Scripts_Datasets_March23", "Datasets", "Processed_Datasets", "PreAggregated_Processed", "BirdNET_JanToDec_ALL_COUNTRIES_MERGED.parquet")
output_path <- here("Final_Scripts_Datasets_March23", "Datasets", "Processed_Datasets", "PreAggregated_Processed", "BirdNET_JanToDec_CLEAN_NOMENCLATURE.parquet")

# [SECTION] 2. OPEN DATASET ----------------------------------------------------
ds <- open_dataset(input_path)

# [SECTION] 3. CREATE RENAMING MAP ---------------------------------------------
current_names <- names(ds)

# Function to create clean names
clean_names_fun <- function(df_names) {
  df_names %>%
    str_to_lower() %>%
    str_replace_all("\\s+", "_") %>%
    str_remove_all("[^a-z0-9_]")
}

clean_names <- clean_names_fun(current_names)

# Create a named vector for renaming: c("new_name" = "old_name")
rename_map <- setNames(current_names, clean_names)

# [SECTION] 4. APPLY STANDARDIZATION -------------------------------------------
# Use rename() and then collect/write
message("Standardizing column names...")

# We use 'select' with 'all_of' to perform the rename on the Arrow Dataset
ds_clean <- ds %>%
  rename(!!rename_map)

# [SECTION] 5. INSPECTION & QUALITY CHECK --------------------------------------
# Run this to see the Confidence scores per species
confidence_audit <- inspection_data %>%
  group_by(scientific_name) %>%
  summarise(
    avg_confidence = mean(confidence, na.rm = TRUE), # Now we look at Confidence!
    min_conf = min(confidence),
    max_conf = max(confidence),
    obs_count = n()
  )

print(confidence_audit)

# 1. This shows the first 10 rows with ALL columns visible
glimpse(inspection_data)

# 2. Or, if you want a spreadsheet-style view
head(inspection_data, 10)

# [SECTION] 6. SAVE THE CLEANED DATASET ----------------------------------------
# This creates the physical file with the new headers
write_parquet(ds_clean, output_path)

message("✅ SUCCESS: Standardized file saved to: ", output_path)
