# ==============================================================================
# Script ID:   03_Data_Mining_3_Env_Var_Conf_ScatterPlots.R
# Project:     Quantifying the Relationship Between Environmental Variables and BirdNET Classification Confidence Using Earth Observation Data and GIS
# Section:     Modeling - Data Integration & Linear Diagnostics
# Purpose:     Join BirdNET aggregated detections with environmental covariates,
#              standardize 16 variables (z-scores), and generate linear 
#              regression panels to visualize baseline relationships.
# Author:      Smit Rajshekhar Patel
# ==============================================================================

# [SECTION] 1. LOAD LIBRARIES --------------------------------------------------
library(tidyverse)
library(arrow)
library(ggplot2)
library(patchwork) 
library(scales)
library(dplyr)

# [SECTION] 2. SET PATHS -------------------------------------------------------
bird_path <- "C:/Users/smit1/OneDrive/Desktop/ITC_MSc/Internship_NINA/NINA/InternshipWork/Final_Scripts_Datasets_March23/Datasets/Processed_Datasets/Aggregated/BirdNET_FINAL_MODEL_INPUT_Ngt1.parquet"
env_path  <- "C:/Users/smit1/OneDrive/Desktop/ITC_MSc/Internship_NINA/NINA/InternshipWork/Final_Scripts_Datasets_March23/Datasets/Processed_Datasets/Env_Var/Integrated_Env_var/Env_Variables_Clean_No_Multicollinearity.parquet"
plot_dir  <- "C:/Users/smit1/OneDrive/Desktop/ITC_MSc/Internship_NINA/NINA/InternshipWork/Final_Scripts_Datasets_March23/Plots"

if(!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

# [SECTION] 3. VARIABLE NAME CLEANER (16 VARIABLES) ----------------------------
clean_names <- function(x) {
  if (is.null(x) || length(x) == 0) return(x)
  x <- gsub("CLIM_T_C", "Air Temperature (°C)", x)
  x <- gsub("CLIM_RH_pct", "Relative Humidity (%)", x)
  x <- gsub("CLIM_WindSpeed_m_s", "Wind Speed (m/s)", x)
  x <- gsub("CLIM_Precip_mm_hr", "Precipitation (mm/hr)", x)
  x <- gsub("ECO101_LAI", "Leaf Area Index", x)
  x <- gsub("VEG201_AcousticVegetationImpedance", "Acoustic Veg. Impedance", x)
  x <- gsub("DIST_WATER", "Distance to Water (m)", x)
  x <- gsub("TEI", "Topographic Exposure Index", x)
  x <- gsub("nightlights", "Night-time Lights", x)
  x <- gsub("builtup", "Built-up Fraction", x)
  x <- gsub("CLIM_SurfacePressure_hPa", "Surface Pressure (hPa)", x)
  x <- gsub("ECO202_SurfaceWetness_mm", "Surface Wetness (mm)", x)
  x <- gsub("ECO203_Snow_SAR", "Snow Cover (SAR)", x)
  x <- gsub("dem", "Elevation (DEM)", x)
  x <- gsub("slope", "Slope", x)
  x <- gsub("VEG104_SAR_GLCM_Entropy", "Veg. Entropy (SAR)", x)
  return(x)
}

# [SECTION] 4. LOAD & JOIN DATASETS --------------------------------------------
bird <- read_parquet(bird_path)
env  <- read_parquet(env_path) %>%
  dplyr::rename(
    deployment_id = DplymID,
    local_date    = Date,
    local_hour    = HourUTC,
    DIST_WATER    = GEO101_DistToWater_m,
    nightlights   = ANTH102_NightLights,
    builtup       = ANTH101_BuiltUpFrac,
    TEI           = TER301_TopographicExposureIndex
  )

bird <- bird %>% dplyr::mutate(local_hour = as.numeric(local_hour))
env  <- env  %>% dplyr::mutate(local_hour = as.numeric(local_hour))

# Inner join to ensure only concurrent observations and variables are kept
master_df <- bird %>%
  dplyr::inner_join(env, by = c("deployment_id","local_date","local_hour"))

# [SECTION] 5. STANDARDIZATION & SCALING (FULL SET) ----------------------------
env_vars <- c("CLIM_T_C", "CLIM_RH_pct", "CLIM_WindSpeed_m_s",
              "CLIM_Precip_mm_hr", "ECO101_LAI",
              "VEG201_AcousticVegetationImpedance",
              "DIST_WATER", "TEI", "builtup", 
              "nightlights", "CLIM_SurfacePressure_hPa", 
              "ECO202_SurfaceWetness_mm", "ECO203_Snow_SAR", 
              "dem", "slope", "VEG104_SAR_GLCM_Entropy")

master_df <- master_df %>%
  dplyr::mutate(
    # Prepare confidence for potential Beta regression (bounding 0-1)
    confidence_beta = pmin(pmax(confidence_mean, 0.0001), 0.9999)
  ) %>%
  # Apply z-score scaling for coefficient comparability
  dplyr::mutate(across(all_of(env_vars), ~ as.numeric(scale(.)))) %>%
  dplyr::filter(if_all(all_of(env_vars), ~ !is.na(.)))

# Save the master analysis-ready dataset
saveRDS(master_df, file = file.path(plot_dir, "MASTER_DATASET.rds"))

# [SECTION] 6. GENERATING REGRESSION PANELS ------------------------------------
cat("\n>>> GENERATING MASTER REGRESSION PANELS...\n")

# Split variables into two sets for cleaner panel visualization
vars_panel_1 <- env_vars[1:9]
vars_panel_2 <- env_vars[10:16]

create_plots <- function(var_list) {
  plots <- list()
  for(v in var_list){
    p <- ggplot(master_df, aes_string(x = v, y = "confidence_mean")) +
      geom_point(alpha = 0.05, size = 0.4, color = "gray40") + 
      geom_smooth(method = "lm", color = "red", se = TRUE, size = 0.8) +
      labs(
        title = clean_names(v),
        x = "Standardized Value",
        y = "Confidence"
      ) +
      theme_minimal(base_size = 10) +
      theme(
        plot.title = element_text(size = 11, face = "bold"),
        axis.title = element_text(size = 9)
      )
    plots[[v]] <- p
  }
  return(plots)
}

# --- PANEL 1 (Variables 1-9) ---
plots_1 <- create_plots(vars_panel_1)
combined_1 <- wrap_plots(plots_1, ncol = 3) +
  plot_annotation(title = "Environmental Drivers Panel 1", subtitle = "Variables 1-9")

ggsave(filename = file.path(plot_dir, "Regression_Panel_1.png"), 
       plot = combined_1, width = 15, height = 12, dpi = 300)

# --- PANEL 2 (Variables 10-16) ---
plots_2 <- create_plots(vars_panel_2)
combined_2 <- wrap_plots(plots_2, ncol = 3) +
  plot_annotation(title = "Environmental Drivers Panel 2", subtitle = "Variables 10-16")

ggsave(filename = file.path(plot_dir, "Regression_Panel_2.png"), 
       plot = combined_2, width = 15, height = 10, dpi = 300)

cat("✔ BOTH REGRESSION PANELS SAVED SUCCESSFULLY.\n")
