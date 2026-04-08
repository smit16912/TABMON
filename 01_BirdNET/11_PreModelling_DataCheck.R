# ==============================================================================
# Script ID:   11_PreModelling_DataCheck.R
# Project:     Quantifying Environmental Variables & BirdNET Confidence
# Section:     Methodology - Final Data Validation
# Purpose:     Verify the suitability of the filtered and aggregated dataset 
#              for the modelling pipeline (checking bounds, NAs, and cardinality).
# ==============================================================================

# [SECTION] Load Libraries -----------------------------------------------------
library(arrow)
library(dplyr)
library(here) # Used for relative path management

# [SECTION] 1. INPUT -----------------------------------------------------------
# Using here() to maintain project portability across machines
INPUT_PARQUET <- here("Final_Scripts_Datasets_March23", "Datasets", "Processed_Datasets", "Aggregated", "BirdNET_Filtered_SelectedSpecies.parquet")

# [SECTION] 2. LOAD DATA (lazy first) ------------------------------------------
dataset <- open_dataset(INPUT_PARQUET)

cat("\n==============================\n")
cat("DATASET STRUCTURE\n")
cat("==============================\n")

print(dataset)

cat("\nColumn names:\n")
print(names(dataset))

# [SECTION] 3. BASIC SUMMARY ---------------------------------------------------
cat("\n==============================\n")
cat("GLOBAL SUMMARY\n")
cat("==============================\n")

summary_stats <- dataset %>%
  summarise(
    total_rows = n(),
    unique_species = n_distinct(scientific_name),
    unique_deployments = n_distinct(deployment_id),
    min_conf = min(confidence_mean, na.rm = TRUE),
    max_conf = max(confidence_mean, na.rm = TRUE),
    mean_conf = mean(confidence_mean, na.rm = TRUE),
    sd_conf = sd(confidence_mean, na.rm = TRUE)
  ) %>%
  collect()

print(summary_stats)

# [SECTION] 4. SPECIES CHECK (CRITICAL) ----------------------------------------
cat("\n==============================\n")
cat("SPECIES LIST CHECK\n")
cat("==============================\n")

species_list <- dataset %>%
  distinct(scientific_name) %>%
  arrange(scientific_name) %>%
  collect()

print(species_list)

cat("\nTotal species detected:", nrow(species_list), "\n")

# [SECTION] 5. RESPONSE VARIABLE VALIDITY --------------------------------------
cat("\n==============================\n")
cat("CONFIDENCE VALIDITY CHECK\n")
cat("==============================\n")

conf_check <- dataset %>%
  summarise(
    missing_conf = sum(is.na(confidence_mean)),
    zero_conf = sum(confidence_mean == 0, na.rm = TRUE),
    one_conf = sum(confidence_mean == 1, na.rm = TRUE),
    below_zero = sum(confidence_mean < 0, na.rm = TRUE),
    above_one = sum(confidence_mean > 1, na.rm = TRUE)
  ) %>%
  collect()

print(conf_check)

# [SECTION] 6. SAMPLE SIZE (AGGREGATION QUALITY) -------------------------------
cat("\n==============================\n")
cat("AGGREGATION QUALITY (n_detections)\n")
cat("==============================\n")

n_det_summary <- dataset %>%
  summarise(
    min_n = min(n_detections, na.rm = TRUE),
    median_n = median(n_detections, na.rm = TRUE),
    mean_n = mean(n_detections, na.rm = TRUE),
    max_n = max(n_detections, na.rm = TRUE),
    single_obs = sum(n_detections == 1, na.rm = TRUE)
  ) %>%
  collect()

print(n_det_summary)

# [SECTION] 7. VARIANCE CHECK --------------------------------------------------
cat("\n==============================\n")
cat("CONFIDENCE VARIANCE CHECK\n")
cat("==============================\n")

var_summary <- dataset %>%
  summarise(
    min_var = min(confidence_variance, na.rm = TRUE),
    max_var = max(confidence_variance, na.rm = TRUE),
    mean_var = mean(confidence_variance, na.rm = TRUE),
    zero_variance = sum(confidence_variance == 0, na.rm = TRUE)
  ) %>%
  collect()

print(var_summary)

# [SECTION] 8. TEMPORAL STRUCTURE CHECK ----------------------------------------
cat("\n==============================\n")
cat("TEMPORAL VARIABLES CHECK\n")
cat("==============================\n")

time_check <- dataset %>%
  summarise(
    missing_hour = sum(is.na(local_hour)),
    min_hour = min(local_hour, na.rm = TRUE),
    max_hour = max(local_hour, na.rm = TRUE)
  ) %>%
  collect()

print(time_check)

# [SECTION] 9. FINAL INTERPRETATION FLAGS --------------------------------------
cat("\n==============================\n")
cat("INTERPRETATION FLAGS\n")
cat("==============================\n")

cat("✔ Confidence within (0,1): Required for logit modelling\n")
cat("✔ n_detections > 1: Required for stable aggregation\n")
cat("✔ Species count should be 21\n")
cat("✔ No NA in key variables\n")

cat("\n>>> DATASET DIAGNOSTIC COMPLETE\n")
