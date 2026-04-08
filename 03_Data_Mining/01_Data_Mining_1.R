# ==============================================================================
# Script ID:   01_Data_Mining_1.R
# Project:     Quantifying the Relationship Between Environmental Variables and BirdNET Classification Confidence Using Earth Observation Data and GIS
# Section:     EDA - Data Mining & Preliminary Visualization
# Purpose:     Explore BirdNET performance patterns including species-specific 
#              confidence thresholds, diurnal activity profiles (radar plots), 
#              and acoustic overlap impacts on segment confidence.
# ==============================================================================

# [SECTION] 1. LOAD LIBRARIES --------------------------------------------------
library(arrow)
library(dplyr)
library(ggplot2)
library(scales)
library(forcats)
library(patchwork)
library(here) # For project portability

# [SECTION] 2. SET PATHS -------------------------------------------------------
# Note: Keeping your explicit paths as requested
input_path <- "C:/Users/smit1/OneDrive/Desktop/ITC_MSc/Internship_NINA/NINA/InternshipWork/Final_Scripts_Datasets_March23/Datasets/Processed_Datasets/PreAggregated_Processed/Processed/BirdNET_JanToDec_FINAL_TEMPORAL.parquet"

output_dir <- "C:/Users/smit1/OneDrive/Desktop/ITC_MSc/Internship_NINA/NINA/InternshipWork/Final_Scripts_Datasets_March23/Results/Plots/Data_mining"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# [SECTION] 3. DATA LOADING ----------------------------------------------------
ds <- open_dataset(input_path)

# [SECTION] 4. SPECIES SELECTION & COLOR SCHEME --------------------------------
target_colors <- c(
  "Acrocephalus schoenobaenus" = "#A6CEE3", 
  "Anser anser" = "#1F78B4", 
  "Cettia cetti" = "#B2DF8A", 
  "Cisticola juncidis" = "#33A02C", 
  "Himantopus himantopus" = "#FB9A99", 
  "Locustella luscinioides" = "#E31A1C",
  "Otus scops" = "#FDBF6F", 
  "Phylloscopus collybita" = "#FF7F00", 
  "Porzana porzana" = "#CAB2D6", 
  "Troglodytes troglodytes" = "#6A3D9A"
)

top12_names <- names(target_colors)

# [SECTION] 5. PRE-PROCESSING FOR VISUALIZATION -------------------------------
cat(">>> Prepping data for mining plots...\n")

plot1_data <- ds %>%
  filter(confidence > 0.8) %>%
  group_by(scientific_name) %>%
  summarise(total_count = n()) %>%
  collect() %>%
  slice_max(total_count, n = 60)

diurnal_scatter_data <- ds %>%
  filter(scientific_name %in% top12_names) %>%
  group_by(scientific_name, local_hour) %>%
  summarise(
    n = n(),
    mean_conf = mean(confidence, na.rm = TRUE)
  ) %>%
  collect()

heatmap_data <- ds %>%
  filter(scientific_name %in% top12_names) %>%
  group_by(deployment_id, scientific_name) %>%
  summarise(mean_conf = mean(confidence, na.rm = TRUE)) %>%
  collect()

# [SECTION] 6. SPECIES EFFECT (TOP 60 CONFIDENCE) ------------------------------
species_conf_data <- ds %>%
  group_by(scientific_name) %>%
  summarise(
    mean_conf = mean(confidence, na.rm = TRUE),
    n = n()
  ) %>%
  collect() %>%
  filter(n > 200) %>%
  arrange(desc(mean_conf)) %>%
  slice_head(n = 60)

p_species <- ggplot(
  species_conf_data,
  aes(x = mean_conf, y = forcats::fct_reorder(scientific_name, mean_conf))
) +
  geom_col(fill = "steelblue") +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 7, face = "italic"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  labs(
    title = "Top 60 Species by Mean BirdNET Confidence",
    subtitle = "Systematic variation in confidence across species",
    x = "Mean Confidence",
    y = "Scientific Name"
  )

ggsave(
  filename = file.path(output_dir, "01_species_confidence_top60.png"),
  plot = p_species, width = 10, height = 12
)

# [SECTION] 7. DIURNAL RADAR PROFILES ------------------------------------------
p2 <- ggplot(diurnal_scatter_data, aes(x = local_hour, y = mean_conf)) +
  geom_line(color = "black", size = 0.8) +
  coord_polar(start = 0) +
  facet_wrap(~scientific_name, ncol = 4) +
  scale_x_continuous(breaks = seq(0, 20, by = 4)) +
  theme_light() +
  labs(
    title = "Mean Confidence Profile (24-Hour Cycle)", 
    x = "Hour of Day (Local)", 
    y = "Mean Confidence Score"
  ) +
  theme(strip.text = element_text(face = "italic", size = 8))

ggsave(
  filename = file.path(output_dir, "02_diurnal_radar.png"),
  plot = p2, width = 12, height = 10
)

Sys.sleep(1)

# [SECTION] 8. DEVICE-LEVEL PERFORMANCE HEATMAP (A4) ---------------------------
heatmap_data_device <- ds %>%
  filter(scientific_name %in% top12_names) %>%
  group_by(device_id, scientific_name) %>%
  summarise(mean_conf = mean(confidence, na.rm = TRUE)) %>%
  collect()

p4_device <- ggplot(heatmap_data_device, aes(x = scientific_name, y = device_id, fill = mean_conf)) +
  geom_tile() +
  scale_fill_viridis_c(
    option = "viridis",
    name = "Confidence Score",
    na.value = "grey20"
  ) +
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "italic", size = 9),
    legend.position = "bottom",
    legend.box = "horizontal",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#3a3a3a", color = NA)
  ) +
  labs(
    title = "Device-Specific Performance",
    subtitle = "Intensity = Mean BirdNET Confidence",
    x = "Scientific Name",
    y = "Device ID"
  )

ggsave(
  filename = file.path(output_dir, "04_device_heatmap_A4.png"),
  plot = p4_device, width = 8.27, height = 11.69
)

# [SECTION] 9. ACOUSTIC OVERLAP ANALYSIS ---------------------------------------
overlap_data <- arrow::read_parquet(
  "C:/Users/smit1/OneDrive/Desktop/ITC_MSc/Internship_NINA/NINA/InternshipWork/Final_Scripts_Datasets_March23/Results/Plots/Data_mining/overlap_dataset.parquet"
)

overlap_data <- overlap_data %>%
  filter(mean_conf <= 1)

base_theme <- theme_bw() +
  theme(
    text = element_text(size = 12),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    plot.title = element_text(size = 13, face = "bold"),
    panel.grid.minor = element_blank()
  )

p_overlap_var <- ggplot(overlap_data, aes(x = factor(n_species), y = sd_conf)) +
  geom_boxplot(width = 0.4, outlier.size = 0.8) +
  base_theme +
  labs(
    title = "Confidence Variability",
    x = "No. of Species (3-sec segment)",
    y = "SD of Confidence"
  )

p_overlap_mean <- ggplot(overlap_data, aes(x = factor(n_species), y = mean_conf)) +
  geom_boxplot(width = 0.4, outlier.size = 0.8) +
  base_theme +
  labs(
    title = "Mean Confidence",
    x = "No. of Species (3-sec segment)",
    y = "Mean Confidence"
  )

p_combined <- p_overlap_var + p_overlap_mean +
  plot_layout(ncol = 2)

ggsave(
  filename = file.path(output_dir, "05_overlap_combined_A4.png"),
  plot = p_combined, width = 11.7, height = 6.5
)

cat("\n>>> ALL EDA PLOTS SUCCESSFULLY GENERATED AND SAVED.\n")
cat("Location:", output_dir, "\n")
