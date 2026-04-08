# ==============================================================================
# Script ID:   08_Pre_DataAggregation_Diagnostics.R
# Project:     Quantifying the Relationship Between Environmental Variables and BirdNET Classification Confidence Using Earth Observation Data and GIS
# Section:     Results - Distribution Analysis
# Purpose:     Evaluate the distribution of confidence scores (Mean vs Median) 
#              and generate diagnostic plots for the internship report.
# ==============================================================================

# [SECTION] Load Libraries -----------------------------------------------------
library(arrow)
library(dplyr)
library(ggplot2)
library(moments)
library(here) # Used for relative path management

# [SECTION] 1. INPUT DATA ------------------------------------------------------
# Using here() to maintain project portability across machines
INPUT_PARQUET <- here("Final_Scripts_Datasets_March23", "Datasets", "Processed_Datasets", "PreAggregated_Processed", "BirdNET_JanToDec_CLEANED.parquet")

# [SECTION] 2. OUTPUT DIRECTORY (PLOTS) ----------------------------------------
# Standardized output path within the project structure
OUTPUT_DIR <- here("Final_Scripts_Datasets_March23", "Scripts", "Phase_2", "Final_Scripts_29March", "Plots_Final")

dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

# [SECTION] 3. LOAD DATA -------------------------------------------------------
# Efficient sampling for large dataset
dataset <- open_dataset(INPUT_PARQUET)

data_sample <- dataset %>%
  select(confidence) %>%
  filter(!is.na(confidence)) %>%
  head(500000) %>%   # large, sufficient sample
  collect()

# [SECTION] 4. SUMMARY STATISTICS ----------------------------------------------
# Mean vs Median diagnostics
summary_stats <- data_sample %>%
  summarise(
    mean_conf    = mean(confidence),
    median_conf = median(confidence),
    sd_conf     = sd(confidence),
    skewness_conf = skewness(confidence)
  )

quantiles <- quantile(
  data_sample$confidence,
  probs = c(0.1, 0.25, 0.5, 0.75, 0.9)
)

cat("\n===== SUMMARY STATISTICS =====\n")
print(summary_stats)

cat("\n===== QUANTILES =====\n")
print(quantiles)

# [SECTION] 5. HISTOGRAM -------------------------------------------------------
# distribution shape
hist_plot <- ggplot(data_sample, aes(x = confidence)) +
  geom_histogram(bins = 50) +
  ggtitle("BirdNET Confidence Distribution (Sampled Data)") +
  xlab("Confidence Score") +
  ylab("Frequency") +
  theme_minimal()

# Save histogram
ggsave(
  filename = file.path(OUTPUT_DIR, "Confidence_Distribution.png"),
  plot = hist_plot,
  width = 8,
  height = 5,
  dpi = 300
)

# [SECTION] 6. BOXPLOT ---------------------------------------------------------
# outliers and spread
box_plot <- ggplot(data_sample, aes(y = confidence)) +
  geom_boxplot() +
  ggtitle("BirdNET Confidence Distribution – Boxplot") +
  ylab("Confidence Score") +
  theme_minimal()

# Save boxplot
ggsave(
  filename = file.path(OUTPUT_DIR, "Confidence_Boxplot.png"),
  plot = box_plot,
  width = 6,
  height = 5,
  dpi = 300
)

# [SECTION] 7. FINAL MESSAGE ---------------------------------------------------
cat("\n✅ Diagnostics completed successfully.\n")
cat("📁 Plots saved to:\n", OUTPUT_DIR, "\n")
