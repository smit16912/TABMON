# ==============================================================================
# Script ID:   04_Temporal_Refinement.R
# Project:     Quantifying Environmental Variables & BirdNET Confidence
# Section:     Methodology - Data Harmonization (Temporal Engine)
# Purpose:     Convert Zulu/UTC timestamps to Local Time based on geographic 
#              coordinates to allow for diel (hourly) analysis.
# ==============================================================================

# [SECTION] Load Libraries -----------------------------------------------------
library(arrow)
library(dplyr)
library(lubridate)
library(stringr)
library(lutz)
library(here) # Used for relative path management

# [SECTION] 1. SET PATHS -------------------------------------------------------
# Using here() to maintain project portability across machines
input_path  <- here("Final_Scripts_Datasets_March23", "Datasets", "Processed_Datasets", "PreAggregated_Processed", "BirdNET_JanToDec_WITH_COORDINATES.parquet")
output_path <- here("Final_Scripts_Datasets_March23", "Datasets", "Processed_Datasets", "PreAggregated_Processed", "BirdNET_JanToDec_FINAL_TEMPORAL.parquet")

# [SECTION] 2. OPEN DATASET ----------------------------------------------------
ds <- open_dataset(input_path)

# [SECTION] 3. CREATE TIMEZONE LOOKUP ------------------------------------------
message("🌍 Generating Timezone lookup from coordinates...")
tz_lookup <- ds %>%
  distinct(device_id, Latitude, Longitude) %>%
  collect() %>%
  filter(!is.na(Latitude)) %>%
  mutate(tz = tz_lookup_coords(Latitude, Longitude, method = "fast")) %>%
  select(device_id, tz)

# [SECTION] 4. THE TEMPORAL ENGINE ---------------------------------------------
message("🕓 Processing timestamps for 74 million rows (Zulu to Local)...")

ds_final <- ds %>%
  left_join(tz_lookup, by = "device_id") %>%
  # NECESSARY CHANGE: Robust string cleaning and multi-format parsing
  mutate(
    timestamp_str = str_remove(filename, "Z?\\.mp3$"),        # Handles Z.mp3 and .mp3
    timestamp_str = str_replace_all(timestamp_str, "[T_]", " ") # Replaces T and _ with space
  ) %>%
  collect() %>% 
  mutate(
    # Parse using orders for both Long (with seconds) and Short (hours/mins only)
    timestamp_utc = parse_date_time(timestamp_str, orders = c("ymd HMS", "ymd HM"), tz = "UTC"),
    timestamp_utc = timestamp_utc + seconds(start_time)
  )

# [SECTION] 5. FINALIZE LOCAL TIME ---------------------------------------------
message("🌐 Finalizing Local Time offsets...")
ds_final <- ds_final %>%
  group_by(tz) %>%
  mutate(timestamp_local = with_tz(timestamp_utc, unique(tz))) %>%
  ungroup() %>%
  mutate(
    local_date  = as_date(timestamp_local),
    local_hour  = hour(timestamp_local),
    local_month = month(timestamp_local)
  )

# [SECTION] 6. SAVE ------------------------------------------------------------
message("💾 Saving final dataset...")
write_parquet(ds_final, output_path)
message("✅ SUCCESS: 74 million rows are now localized!")

# [SECTION] 7. FINAL AUDIT -----------------------------------------------------
message("\n🔍 Auditing results...")
ds_audit <- open_dataset(output_path)

summary_audit <- ds_audit %>%
  group_by(country, tz) %>%
  summarise(
    total_detections = n(),
    sample_utc = min(timestamp_utc, na.rm = TRUE),
    sample_local = min(timestamp_local, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  collect() %>%
  mutate(
    # NECESSARY CHANGE: Force units to "hours" for clear report
    hour_offset = as.numeric(difftime(sample_local, sample_utc, units = "hours"))
  )

print("--- Timezone Offset Report ---")
print(summary_audit)

# Cardinality Check
size_stats <- ds_audit %>%
  summarize(
    Total_Rows = n(),
    Unique_Species = n_distinct(scientific_name),
    Unique_Deployments = n_distinct(deployment_id), 
    .groups = "drop"
  ) %>%
  collect()

print("--- Dataset Size Summary ---")
print(size_stats)

message("\n✅ Done. All countries should now show valid times and offsets.")
