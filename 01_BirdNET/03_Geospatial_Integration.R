# ==============================================================================
# Script ID:   03_Geospatial_Integration.R
# Project:     Quantifying Environmental Variables & BirdNET Confidence
# Section:     Methodology - Data Harmonization (Spatial Joins)
# Purpose:     Join BirdNET detections with Bugg deployment metadata to 
#              attach Latitude, Longitude, and Site information.
# ==============================================================================

# [SECTION] Load Libraries -----------------------------------------------------
library(arrow)
library(dplyr)
library(readr)
library(stringr)
library(here) # Used for relative path management

# [SECTION] 1. SET PATHS -------------------------------------------------------
# Using here() to maintain project portability across machines
birdnet_path       <- here("Final_Scripts_Datasets_March23", "Datasets", "Processed_Datasets", "PreAggregated_Processed", "BirdNET_JanToDec_CLEAN_NOMENCLATURE.parquet")
bugg_metadata_path <- here("Data", "OriginalData", "Bugg_deployment_form.csv")
output_path        <- here("Final_Scripts_Datasets_March23", "Datasets", "Processed_Datasets", "PreAggregated_Processed", "BirdNET_JanToDec_WITH_COORDINATES.parquet")

# [SECTION] 2. PREPARE THE METADATA (CSV) --------------------------------------
# NECESSARY CHANGE: Use DeploymentID to extract the device_id
df_bugg_clean <- read_csv(bugg_metadata_path, show_col_types = FALSE) %>%
  mutate(device_id = str_extract(DeploymentID, "[^_]+$")) %>% # Extracts ID after the last '_'
  select(device_id, Latitude, Longitude, Site, Cluster) %>%
  distinct(device_id, .keep_all = TRUE) 

# [SECTION] 3. EXECUTE THE JOIN ------------------------------------------------
ds_birdnet <- open_dataset(birdnet_path)

message("Merging coordinates for 74 million detections...")

ds_combined <- ds_birdnet %>%
  left_join(df_bugg_clean, by = "device_id")

# [SECTION] 4. SAVE TO DISK ----------------------------------------------------
message("Writing final parquet file...")
write_parquet(ds_combined, output_path)

# [SECTION] 5. JOIN INTEGRITY AUDIT --------------------------------------------
# Re-opening saved file to verify the 813k missing rows are now fixed
ds_check <- open_dataset(output_path)

message("Auditing join results...")

integrity_report <- ds_check %>%
  group_by(device_id, country) %>%
  summarise(
    total_detections = n(),
    unjoined_count = sum(is.na(Latitude)),
    .groups = "drop"
  ) %>%
  collect()

print("--- JOIN INTEGRITY REPORT ---")
print(integrity_report)

total_unjoined <- sum(integrity_report$unjoined_count)
if(total_unjoined > 0) {
  cat("\n⚠️ ALERT: Found", total_unjoined, "detections still missing coordinates.\n")
} else {
  cat("\n✅ PERFECT: All detections matched successfully!\n")
}
