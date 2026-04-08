# ==============================================================================
# Script ID:   05_Structural_Audits.R
# Project:     Quantifying the Relationship Between Environmental Variables and BirdNET Classification Confidence Using Earth Observation Data and GIS
# Section:     Methodology - Data Harmonization (Quality Control)
# Purpose:     Identify missing values, logical insanities (bounds), and 
#              duplicates to ensure dataset integrity before modeling.
# ==============================================================================

# [SECTION] Load Libraries -----------------------------------------------------
library(arrow)
library(dplyr)
library(openxlsx)
library(here) # Used for relative path management

# [SECTION] 1. Define Paths ----------------------------------------------------
# Using here() to maintain project portability across machines
input_path  <- here("Final_Scripts_Datasets_March23", "Datasets", "Processed_Datasets", "PreAggregated_Processed", "BirdNET_JanToDec_FINAL_TEMPORAL.parquet")
output_dir  <- here("Final_Scripts_Datasets_March23", "Datasets", "Processed_Datasets", "PreAggregated_Processed", "Diagnostics", "01")
output_file <- file.path(output_dir, "BirdNET_Problematic_Rows_Audit.xlsx")

# [SECTION] 2. Read dataset ----------------------------------------------------
# Using open_dataset is smart. We only pull needed columns into memory first.
dataset <- open_dataset(input_path)

data <- dataset %>% 
  collect()

# [SECTION] 3. Flag missing values and sanity violations -----------------------
data_flagged <- data %>%
  mutate(
    issue_missing_timestamp = is.na(timestamp_utc) | is.na(timestamp_local),
    issue_missing_local_time = is.na(local_date) | is.na(local_hour),
    issue_confidence_bounds = confidence < 0 | confidence > 1,
    issue_local_hour_bounds = local_hour < 0 | local_hour > 23
  )

# [SECTION] 4. Identify duplicated detections ----------------------------------
duplicate_keys <- data_flagged %>%
  count(filename, start_time, scientific_name) %>%
  filter(n > 1) %>%
  select(filename, start_time, scientific_name) %>%
  mutate(issue_duplicate = TRUE)

data_flagged <- data_flagged %>%
  left_join(duplicate_keys, by = c("filename", "start_time", "scientific_name")) %>%
  mutate(issue_duplicate = tidyr::replace_na(issue_duplicate, FALSE))

# [SECTION] 5. Extract problematic rows and summarize --------------------------
problematic_rows <- data_flagged %>%
  filter(
    issue_missing_timestamp | 
      issue_missing_local_time | 
      issue_confidence_bounds | 
      issue_local_hour_bounds | 
      issue_duplicate
  )

issue_summary <- data.frame(
  total_rows_scanned = nrow(data),
  total_problematic_rows = nrow(problematic_rows),
  missing_timestamp = sum(data_flagged$issue_missing_timestamp, na.rm = TRUE),
  missing_local_time = sum(data_flagged$issue_missing_local_time, na.rm = TRUE),
  confidence_out_of_bounds = sum(data_flagged$issue_confidence_bounds, na.rm = TRUE),
  hour_out_of_bounds = sum(data_flagged$issue_local_hour_bounds, na.rm = TRUE),
  duplicate_count = sum(data_flagged$issue_duplicate, na.rm = TRUE)
)

# [SECTION] 6. Write Excel audit file ------------------------------------------
if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

wb <- createWorkbook()
addWorksheet(wb, "Summary")
writeData(wb, "Summary", issue_summary)

addWorksheet(wb, "Problematic_Data")
writeData(wb, "Problematic_Data", problematic_rows)

saveWorkbook(wb, output_file, overwrite = TRUE)

cat("\n--- Audit Complete ---\n")
cat("File saved to:", output_file, "\n")

# [SECTION] 7. Access Audit Results --------------------------------------------
# Loading and inspecting the generated Excel audit report

# Define the path to the audit file you just created
audit_path <- here("Final_Scripts_Datasets_March23", "Datasets", "Processed_Datasets", "PreAggregated_Processed", "Diagnostics", "01", "BirdNET_Problematic_Rows_Audit.xlsx")

# Read the Summary sheet
audit_summary <- read.xlsx(audit_path, sheet = "Summary")

# Read the Problematic Data sheet
problem_data <- read.xlsx(audit_path, sheet = "Problematic_Data")

# --- Quick Inspection ---
print("--- Issue Summary ---")
print(audit_summary)

print("--- Sample of Problematic Rows ---")
head(problem_data)

# --- Deep Dive ---
species_issues <- problem_data %>%
  group_by(scientific_name) %>%
  summarise(total_issues = n()) %>%
  arrange(desc(total_issues))

print("--- Species with most issues ---")
print(head(species_issues))

file_issues <- problem_data %>%
  group_by(filename) %>%
  summarise(issue_count = n()) %>%
  arrange(desc(issue_count))

print("--- Files with most issues ---")
print(head(file_issues))
