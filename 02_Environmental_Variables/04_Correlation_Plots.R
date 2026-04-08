# ==============================================================================
# Script ID:   04_Correlation_Plots.R
# Project:     Quantifying the Relationship Between Environmental Variables and BirdNET Classification Confidence Using Earth Observation Data and GIS
# Section:     Diagnostics - Multicollinearity & Variable Independence
# Purpose:     Generate a publication-ready correlation matrix (Pearson) to 
#              validate that the final environmental dataset is free from 
#              extreme multicollinearity (|r| > 0.9). Includes hierarchical 
#              clustering for variable grouping.
# ==============================================================================

# [SECTION] 1. LOAD LIBRARIES --------------------------------------------------
library(arrow)
library(dplyr)
library(corrplot)
library(RColorBrewer)
library(caret)

# [SECTION] 2. LOAD FINAL CLEAN DATASET ----------------------------------------
file_path <- "C:/Users/smit1/OneDrive/Desktop/ITC_MSc/Internship_NINA/NINA/InternshipWork/Final_Scripts_Datasets_March23/Datasets/Processed_Datasets/Env_Var/Integrated_Env_var/Env_Variables_Clean_No_Multicollinearity.parquet"

env_final_clean <- read_parquet(file_path)

# [SECTION] 3. OUTPUT DIRECTORY ------------------------------------------------
plot_dir <- "C:/Users/smit1/OneDrive/Desktop/ITC_MSc/Internship_NINA/NINA/InternshipWork/Final_Scripts_Datasets_March23/Scripts/Correlation_Plots"
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# [SECTION] 4. DATA SELECTION & STANDARDIZED NAMING ----------------------------
# Select numeric variables and exclude non-predictive temporal/ID columns
corr_data <- env_final_clean %>%
  select(where(is.numeric)) %>%
  select(-any_of(c("HourUTC", "DplymID")))

# Rename columns for publication-quality plot labels
corr_data_named <- corr_data %>%
  rename(
    "Air Temperature (°C)" = CLIM_T_C,
    "Built-up Fraction" = ANTH101_BuiltUpFrac,
    "Night-time Lights" = ANTH102_NightLights,
    "Precipitation (mm/hr)" = CLIM_Precip_mm_hr,
    "Relative Humidity (%)" = CLIM_RH_pct,
    "Surface Pressure (hPa)" = CLIM_SurfacePressure_hPa,
    "Wind Speed (m/s)" = CLIM_WindSpeed_m_s,
    "Leaf Area Index" = ECO101_LAI,
    "Surface Wetness (mm)" = ECO202_SurfaceWetness_mm,
    "Snow Cover (SAR)" = ECO203_Snow_SAR,
    "Distance to Water (m)" = GEO101_DistToWater_m,
    "Topographic Exposure Index" = TER301_TopographicExposureIndex,
    "Elevation (m)" = dem,
    "Slope (°)" = slope,
    "SAR Entropy" = VEG104_SAR_GLCM_Entropy,
    "Acoustic Vegetation Impedance" = VEG201_AcousticVegetationImpedance
  )

# [SECTION] 5. COMPUTE CORRELATION MATRIX --------------------------------------
# Compute Pearson correlation using complete observations
corr_matrix <- cor(corr_data_named, use = "complete.obs")

# Double-check for extreme correlations (Final Safeguard)
high_corr <- findCorrelation(corr_matrix, cutoff = 0.9)
if(length(high_corr) > 0){
  corr_matrix <- corr_matrix[-high_corr, -high_corr]
}

# [SECTION] 6. VISUALIZATION (BLUE-WHITE-RED STANDARD) -------------------------
col_palette <- colorRampPalette(
  rev(brewer.pal(11, "RdBu"))
)(200)

# Export as high-resolution PNG
png(file.path(plot_dir, "Correlation_Matrix_Final11.png"),
    width = 1400, height = 1200, res = 150)

corrplot(
  corr_matrix,
  method = "circle",
  type = "lower",      # Lower triangle only for cleaner publication look
  order = "hclust",    # Group variables by hierarchical similarity
  col = col_palette,
  
  tl.col = "black",    # Text label color
  tl.cex = 0.8,        # Text label size
  tl.srt = 45,         # Text rotation
  
  addCoef.col = "black", # Add correlation coefficients inside shapes
  number.cex = 0.6,
  
  diag = FALSE,        # Remove identity diagonal
  cl.pos = "b",        # Color legend at bottom
  cl.cex = 0.8
)

dev.off()

# [SECTION] 7. SUMMARY & QC ----------------------------------------------------
cat("\nCorrelation matrix successfully generated and saved.\n")

# Print NA counts per variable to ensure data integrity
colSums(is.na(corr_data_named))
