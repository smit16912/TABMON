# ==============================================================================
# Script ID:   02_Data_Mining_2_IUCN_Redlist.R
# Project:     Quantifying the Relationship Between Environmental Variables and BirdNET Classification Confidence Using Earth Observation Data and GIS
# Section:     Conservation Analytics - Threatened Species Performance
# Purpose:     Visualize BirdNET confidence across deployments specifically for 
#              threatened species (VU, EN, CR). Optimized for A4 portrait 
#              printing with italicized species names and dark background.
# ==============================================================================

# [SECTION] 1. LOAD LIBRARIES --------------------------------------------------
library(arrow)
library(dplyr)
library(ggplot2)
library(stringr)

# [SECTION] 2. SET PATHS -------------------------------------------------------
birdnet_path <- "C:/Users/smit1/OneDrive/Desktop/ITC_MSc/Internship_NINA/NINA/InternshipWork/Final_Scripts_Datasets_March23/Datasets/Processed_Datasets/PreAggregated_Processed/Processed/BirdNET_JanToDec_FINAL_TEMPORAL.parquet"

status_path <- "FINAL_species_status_COMPLETE.csv"

# [SECTION] 3. DATA LOADING & PREPARATION --------------------------------------
birdnet <- open_dataset(birdnet_path)
status_df <- read.csv(status_path)

# Extract and standardize names for joining
birdnet_full <- birdnet %>%
  select(deployment_id, scientific_name, confidence) %>%
  collect() %>%
  mutate(species = str_to_lower(scientific_name))

# Join BirdNET data with Red List status
data_joined <- birdnet_full %>%
  left_join(status_df, by = "species")

# [SECTION] 4. THREATENED SPECIES FILTERING ------------------------------------
# Filter only Vulnerable (VU), Endangered (EN), and Critically Endangered (CR)
threatened_data <- data_joined %>%
  filter(final_status %in% c("VU", "EN", "CR"))

# Aggregate mean confidence per species/deployment
heatmap_data <- threatened_data %>%
  group_by(species, deployment_id, final_status) %>%
  summarise(mean_conf = mean(confidence, na.rm = TRUE), .groups = "drop")

# [SECTION] 5. FACTOR ORDERING & STRING CLEANING -------------------------------
# Order species by threat category severity
heatmap_data <- heatmap_data %>%
  mutate(
    final_status = factor(final_status, levels = c("CR", "EN", "VU"))
  ) %>%
  arrange(final_status, species)

# Extract clean deployment IDs (Short names)
heatmap_data <- heatmap_data %>%
  mutate(
    deployment_clean = str_extract(deployment_id, "[A-Z]{2}_.*")
  )

# [SECTION] 6. HEATMAP VISUALIZATION (NO GRID) ---------------------------------
p <- ggplot(heatmap_data, aes(x = deployment_clean, y = species, fill = mean_conf)) +
  geom_tile(color = NA) +
  
  scale_fill_viridis_c(
    option = "viridis",
    name = "Confidence",
    na.value = "#3a3a3a"
  ) +
  
  facet_grid(final_status ~ ., scales = "free_y", space = "free_y") +
  
  labs(
    title = "BirdNET Detection Confidence (Threatened Species)",
    subtitle = "Mean confidence across deployments",
    x = "Deployment",
    y = "Species"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "#3a3a3a", color = NA),
    plot.background = element_blank(),
    
    axis.text.x = element_text(
      angle = 90,
      vjust = 0.5,
      hjust = 1,
      size = 4.5
    ),
    axis.ticks.x = element_line(),
    
    axis.text.y = element_text(
      size = 10,       # Optimized for legibility
      face = "italic"  # Correct botanical/zoological notation
    ),
    
    strip.text = element_text(size = 11, face = "bold"),
    plot.title = element_text(size = 14, face = "bold"),
    plot.subtitle = element_text(size = 10)
  )

# [SECTION] 7. EXPORT ----------------------------------------------------------
ggsave(
  "C:/Users/smit1/OneDrive/Desktop/ITC_MSc/Internship_NINA/NINA/InternshipWork/Final_Scripts_Datasets_March23/Datasets/Processed_Datasets/PreAggregated_Processed/Processed/Threatened_Species_Heatmap_A4_PORTRAIT.png",
  plot = p,
  width = 8.27,    # A4 Portrait Width
  height = 11.69,  # A4 Portrait Height
  dpi = 300
)

# Display to screen
print(p)
