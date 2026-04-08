# ==============================================================================
# Script ID:   02_Modelling_Visualization.R
# Project:     Quantifying Environmental Variables & BirdNET Confidence
# Section:     Visualization - Model Diagnostics & Effect Plots
# Purpose:     Generate all thesis figures including AIC comparisons, GAM 
#              smooths (M5/Detections), M3 Beta Regression landscapes, and 
#              environmental lift bar charts.
# ==============================================================================

# [SECTION] Load Libraries -----------------------------------------------------
library(mgcv)
library(betareg)
library(ggplot2)
library(gratia)
library(patchwork)
library(dplyr)
library(broom)
library(scales)
library(here) # For project-relative paths

# [SECTION] 1. HARDWARE & PATHS ------------------------------------------------
# Using here() to maintain project portability
plot_dir <- here("Final_Scripts_Datasets_March23", "Plots")

# Load model list generated in Script 16
model_list <- readRDS(file.path(plot_dir, "ALL_MODELS_FULL.rds"))

# Extract models for local environment
m0                <- model_list$m0
m1                <- model_list$m1
m2                <- model_list$m2
m3                <- model_list$m3
m4                <- model_list$m4
m5                <- model_list$m5
m5_noENV          <- model_list$m5_noENV
m6                <- model_list$m6
m7                <- model_list$m7
m_detections_full <- model_list$m_detections_full

# [SECTION] 2. VARIABLE NAME CLEANER (REFINED) ---------------------------------
clean_names <- function(x) {
  if (is.null(x) || length(x) == 0) return(x)
  
  # Standardize common GAM label formats like s(VARIABLE)
  x <- gsub("s\\(([^)]+)\\)", "\\1", x) 
  
  x <- gsub("CLIM_T_C", "Air Temperature (°C)", x)
  x <- gsub("CLIM_RH_pct", "Relative Humidity (%)", x)
  x <- gsub("CLIM_WindSpeed_m_s", "Wind Speed (m/s)", x)
  x <- gsub("CLIM_Precip_mm_hr", "Precipitation (mm/hr)", x)
  x <- gsub("ECO101_LAI", "Leaf Area Index", x)
  x <- gsub("VEG201_AcousticVegetationImpedance", "Acoustic Vegetation Impedance", x)
  x <- gsub("DIST_WATER", "Distance to Water (m)", x)
  x <- gsub("TEI", "Topographic Exposure Index", x)
  x <- gsub("nightlights", "Night-time Lights", x)
  
  # New requested cleaners
  x <- gsub("deployment_ID", "Deployment ID", x)
  x <- gsub("scientific_name", "Scientific Name", x)
  x <- gsub("local_hour", "Local Hour", x)
  
  return(x)
}

# [SECTION] 3. COMBINED AIC + DEVIANCE PLOT ------------------------------------
cat("\n>>> GENERATING AND SAVING ALL PLOTS WITH REFINED NAMES...\n")

# Note: comparison_table is loaded from Script 16 output if not in memory
comparison_table <- read.csv(file.path(plot_dir, "Model_Comparison_Table.csv"))

comp_long <- comparison_table %>%
  select(Model, AIC, Deviance_Explained) %>%
  tidyr::pivot_longer(cols = c(AIC, Deviance_Explained),
                      names_to = "Metric",
                      values_to = "Value")

p_combined <- ggplot(comp_long,
                     aes(x = reorder(Model, Value), y = Value, fill = Metric)) +
  geom_col(position = "dodge", width = 0.5) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Model Comparison (AIC & Deviance Explained)",
       x = "Model",
       y = "Value")

ggsave(file.path(plot_dir, "Model_Comparison_Combined.png"),
       p_combined, width = 8, height = 5)

# [SECTION] 4. M5 SMOOTH PLOTS -------------------------------------------------
smooths_m5_raw <- draw(m5)

plots_m5_cleaned <- lapply(seq_along(smooths_m5_raw), function(i) {
  p <- smooths_m5_raw[[i]]
  p + labs(
    title = clean_names(p$labels$title),
    x = clean_names(p$labels$x),
    y = clean_names(p$labels$y)
  )
})

smooths_m5 <- wrap_plots(plots_m5_cleaned)

ggsave(file.path(plot_dir, "M5_Smooths.png"),
       smooths_m5,
       width = 12, height = 10)

# [SECTION] 5. DETECTION SMOOTHS -----------------------------------------------
smooths_det_raw <- draw(m_detections_full)

plots_det_cleaned <- lapply(seq_along(smooths_det_raw), function(i) {
  p <- smooths_det_raw[[i]]
  p + labs(
    title = clean_names(p$labels$title),
    x = clean_names(p$labels$x),
    y = clean_names(p$labels$y)
  )
})

smooths_det <- wrap_plots(plots_det_cleaned)

ggsave(file.path(plot_dir, "Detection_Smooths.png"),
       smooths_det,
       width = 12, height = 10)

# [SECTION] 6. M3 BETA COEFFICIENTS (LANDSCAPE) --------------------------------
# Prepare data
m3_combined <- tidy(m3, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    is_species = grepl("scientific_name", term),
    term_clean = sapply(term, clean_names),
    term_clean = gsub("Scientific Name", "", term_clean),
    Group = ifelse(is_species, "Species", "Environmental/Other")
  ) %>%
  filter(!grepl("Deployment ID", term_clean)) %>%
  arrange(Group, estimate) %>%
  mutate(term_clean = factor(term_clean, levels = term_clean))

# Create Landscape Plot
p_m3_final <- ggplot(m3_combined, 
                     aes(x = term_clean, y = estimate, color = Group)) +
  geom_point(size = 3) + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.4) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  
  scale_y_continuous(
    trans = "pseudo_log", 
    breaks = c(-0.5, 0, 0.5, 1.0, 2.0, 4.0, 8.0)
  ) +
  
  scale_color_manual(values = c("Species" = "#2c7bb6", "Environmental/Other" = "#d7191c")) +
  coord_cartesian(clip = "off") + 
  theme_minimal() +
  labs(title = "M3 Beta Regression Coefficients (Cleaned Scale)",
       x = "Variables / Species",
       y = "Effect Size (β)",
       color = "Category") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 14), 
    axis.text.y = element_text(size = 20, face = "bold"), 
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 13),
    panel.grid.major.y = element_line(color = "gray95"),
    panel.grid.minor.y = element_blank(),
    legend.position = "top",
    plot.margin = margin(t = 20, r = 30, b = 100, l = 100, unit = "pt")
  )

ggsave(file.path(plot_dir, "M3_Landscape_Clean_LargeY.png"),
       p_m3_final, width = 20, height = 10, dpi = 300)

# [SECTION] 7. SIGNAL COMPARISONS & LIFT ---------------------------------------
# Detection vs Confidence
comparison_df <- data.frame(
  Model = c("Confidence (M5)", "Detection"),
  Deviance = c(summary(m5)$dev.expl * 100,
               summary(m_detections_full)$dev.expl * 100)
)

p_compare <- ggplot(comparison_df, aes(x = Model, y = Deviance)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Detection vs Classification Signal",
       y = "% Deviance Explained",
       x = "Model Type")

ggsave(file.path(plot_dir, "Detection_vs_Confidence.png"),
       p_compare, width = 6, height = 5)

# Environmental lift
lift_df <- data.frame(
  Model = c("Baseline (No ENV)", "Full Model"),
  Deviance = c(summary(m5_noENV)$dev.expl * 100,
               summary(m5)$dev.expl * 100)
)

p_lift <- ggplot(lift_df, aes(x = Model, y = Deviance)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Environmental Contribution to Confidence",
       y = "% Deviance Explained",
       x = "Model Configuration")

ggsave(file.path(plot_dir, "Environmental_Lift_Confidence.png"),
       p_lift, width = 6, height = 5)

# [SECTION] 8. RESIDUAL DIAGNOSTICS --------------------------------------------
png(file.path(plot_dir, "M5_Residuals.png"), width = 800, height = 600)
plot(m5, residuals = TRUE, pch = 16)
dev.off()

png(file.path(plot_dir, "Detection_Residuals.png"), width = 800, height = 600)
plot(m_detections_full, residuals = TRUE, pch = 16)
dev.off()

# [SECTION] 9. INTERACTION PLOTS (M6) ------------------------------------------
if(exists("m6")) {
  p_int_raw <- draw(m6)
  plots_m6_cleaned <- lapply(seq_along(p_int_raw), function(i) {
    p <- p_int_raw[[i]]
    p + labs(
      title = clean_names(p$labels$title),
      x = clean_names(p$labels$x),
      y = clean_names(p$labels$y)
    )
  })
  p_int <- wrap_plots(plots_m6_cleaned)
  ggsave(file.path(plot_dir, "M6_Interactions.png"),
         p_int, width = 12, height = 10)
}

# [SECTION] 10. M3 MULTI-PANEL EFFECT PLOTS ------------------------------------
# Define effect plotting function
plot_m3_effect <- function(model, data, var, label) {
  x_seq <- seq(min(data[[var]], na.rm = TRUE),
               max(data[[var]], na.rm = TRUE),
               length.out = 100)
  newdata <- data[rep(1, 100), ]
  for(col in names(newdata)) {
    if(is.numeric(data[[col]])) {
      newdata[[col]] <- mean(data[[col]], na.rm = TRUE)
    }
  }
  newdata[[var]] <- x_seq
  pred <- predict(model, newdata = newdata, type = "response")
  plot_df <- data.frame(x = x_seq, fit = pred)
  
  ggplot(plot_df, aes(x = x, y = fit)) +
    geom_line(size = 1) +
    theme_minimal() +
    labs(title = label, x = label, y = "Predicted Confidence")
}

# Note: master_df should be available from Script 16 environment
p1 <- plot_m3_effect(m3, master_df, "CLIM_T_C", "Temperature")
p2 <- plot_m3_effect(m3, master_df, "CLIM_RH_pct", "Humidity")
p3 <- plot_m3_effect(m3, master_df, "CLIM_WindSpeed_m_s", "Wind Speed")
p4 <- plot_m3_effect(m3, master_df, "CLIM_Precip_mm_hr", "Precipitation")
p5 <- plot_m3_effect(m3, master_df, "ECO101_LAI", "LAI")
p6 <- plot_m3_effect(m3, master_df, "VEG201_AcousticVegetationImpedance", "Vegetation Impedance")
p7 <- plot_m3_effect(m3, master_df, "DIST_WATER", "Distance to Water")
p8 <- plot_m3_effect(m3, master_df, "TEI", "Topographic Exposure")

p_panel <- (p1 | p2 | p3 | p4) / (p5 | p6 | p7 | p8)

ggsave(file.path(plot_dir, "M3_Effects_Panel.png"),
       p_panel, width = 12, height = 8)

cat("\n>>> ALL PLOTS GENERATED AND SAVED SUCCESSFULLY.\n")
