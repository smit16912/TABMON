# ==============================================================================
# Script ID:   14_Statistical_Diagnostics.R
# Project:     Quantifying the Relationship Between Environmental Variables and BirdNET Classification Confidence Using Earth Observation Data and GIS
# Section:     Results - Statistical Diagnostics (Model Fitness)
# Purpose:     Comprehensive diagnostic suite for the modeling dataset, covering
#              distribution, residuals, non-linearity, and spatial autocorrelation.
# ==============================================================================

# [SECTION] Load Libraries -----------------------------------------------------
library(tidyverse)
library(car)
library(lmtest)
library(sandwich)
library(broom)
library(arrow)
library(mgcv)
library(spdep)
library(here) # Used for relative path management

# [SECTION] 1. DATA LOADING ----------------------------------------------------
# Using here() to maintain project portability across machines
birdnet_path     <- here("Final_Scripts_Datasets_March23", "Datasets", "Processed_Datasets", "Aggregated", "BirdNET_FINAL_MODEL_INPUT_Ngt1.parquet")
deployments_path <- here("DataAnalyses", "Data", "ProcessedData", "Bugg_deployment_form.csv")

birdnet     <- read_parquet(birdnet_path)
deployments <- read.csv(deployments_path)

# [SECTION] 2. OUTPUT DIRECTORY ------------------------------------------------
plot_dir <- here("Final_Scripts_Datasets_March23", "Scripts", "Phase_2", "Final_Scripts_29March", "Plots_Final", "Final_BirdNET_Stats_Diag")
dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

# [SECTION] 3. FINAL FILTER + TRANSFORM ----------------------------------------
birdnet_valid <- birdnet %>%
  filter(!is.na(confidence_mean)) %>%
  filter(confidence_mean > 0 & confidence_mean < 1) %>%
  mutate(
    logit_conf_mean = car::logit(confidence_mean, adjust = 0.01),
    log_det = log1p(n_detections)
  )

# [SECTION] 4. RESPONSE DISTRIBUTION -------------------------------------------
cat("\n=========== RESPONSE DISTRIBUTION ===========\n")

p1 <- ggplot(birdnet_valid, aes(x = confidence_mean)) +
  geom_histogram(bins = 100) +
  theme_bw() +
  ggtitle("Raw Mean Confidence Distribution")

ggsave(file.path(plot_dir, "01_raw_mean_distribution.png"), p1, width = 8, height = 6)

p2 <- ggplot(birdnet_valid, aes(x = logit_conf_mean)) +
  geom_histogram(bins = 100) +
  theme_bw() +
  ggtitle("Logit Transformed Mean Distribution")

ggsave(file.path(plot_dir, "02_logit_mean_distribution.png"), p2, width = 8, height = 6)

# [SECTION] 5. SAMPLE FOR VISUAL RELATIONSHIPS ---------------------------------
set.seed(42)
sample_df <- birdnet_valid %>% slice_sample(n = min(nrow(.), 100000))

p3 <- ggplot(sample_df, aes(x = n_detections, y = confidence_mean)) +
  geom_point(alpha = 0.2) +
  scale_x_log10() +
  theme_bw() +
  ggtitle("Mean Confidence vs Number of Detections")

ggsave(file.path(plot_dir, "03_mean_vs_detections.png"), p3, width = 8, height = 6)

p4 <- ggplot(birdnet_valid, aes(x = reorder(scientific_name, confidence_mean, FUN = median),
                                y = confidence_mean)) +
  geom_boxplot(outlier.alpha = 0.1) +
  coord_flip() +
  theme_bw() +
  ggtitle("Confidence Variation by Species")

ggsave(file.path(plot_dir, "04_species_variation.png"), p4, width = 10, height = 8)

# [SECTION] 6. BASE MODEL ------------------------------------------------------
cat("\n=========== BASE MODEL ===========\n")

model_base <- lm(logit_conf_mean ~ log_det, data = birdnet_valid)
print(summary(model_base))

# Non-linearity check
gam_check <- gam(logit_conf_mean ~ s(log_det, k = 5), data = birdnet_valid)

cat("\n--- GAM CHECK ---\n")
print(summary(gam_check))

png(file.path(plot_dir, "05_gam_smooth.png"), width = 800, height = 600)
plot(gam_check, shade = TRUE, main = "Smooth effect of Log(Detections)")
dev.off()

# [SECTION] 7. RESIDUAL DIAGNOSTICS --------------------------------------------
cat("\n=========== RESIDUAL DIAGNOSTICS ===========\n")

png(file.path(plot_dir, "06_residuals_vs_fitted.png"), width = 800, height = 600)
plot(model_base, which = 1)
dev.off()

png(file.path(plot_dir, "07_qq_plot.png"), width = 800, height = 600)
plot(model_base, which = 2)
dev.off()

# [SECTION] 8. HETEROSKEDASTICITY + ROBUST SE ----------------------------------
cat("\n=========== STATISTICAL TESTS ===========\n")

cat("\n--- Breusch-Pagan Test ---\n")
print(bptest(model_base))

cat("\n--- Robust SE ---\n")
print(coeftest(model_base, vcov = vcovHC(model_base, type = "HC1")))

# [SECTION] 9. INFLUENCE DIAGNOSTICS -------------------------------------------
cat("\n=========== INFLUENCE DIAGNOSTICS ===========\n")

# Improved axis formatting
png(file.path(plot_dir, "08_cooks_distance.png"), width = 900, height = 700)
par(cex.lab = 1.6, cex.axis = 1.4)

cd <- cooks.distance(model_base)
plot(cd,
     type = "h",
     xaxt = "n",   # remove default x-axis
     xlab = "Observation Index",
     ylab = "Cook's Distance",
     main = "")

# Add clean scientific-style axis
axis(1,
     at = pretty(seq_along(cd)),
     labels = scales::comma(pretty(seq_along(cd))))
dev.off()

# [SECTION] 10. TEMPORAL STRUCTURE ---------------------------------------------
cat("\n=========== TEMPORAL STRUCTURE ===========\n")

p5 <- birdnet_valid %>%
  group_by(local_hour) %>%
  summarise(m_conf = mean(confidence_mean, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(local_hour, m_conf)) +
  geom_line(linewidth = 1) +
  theme_bw() +
  ggtitle("Confidence by Hour of Day")

ggsave(file.path(plot_dir, "09_time_pattern.png"), p5, width = 8, height = 6)

# [SECTION] 11. SPATIAL AUTOCORRELATION ----------------------------------------
cat("\n=========== SPATIAL AUTOCORRELATION ===========\n")

coords <- deployments %>%
  dplyr::select(DeploymentID, Latitude, Longitude) %>%
  distinct() %>%
  rename(
    deployment_id = DeploymentID,
    latitude = Latitude,
    longitude = Longitude
  )

res_df <- birdnet_valid %>%
  mutate(
    deployment_id = as.character(deployment_id),
    res = residuals(model_base)
  ) %>%
  left_join(mutate(coords, deployment_id = as.character(deployment_id)),
            by = "deployment_id") %>%
  filter(!is.na(longitude) & !is.na(latitude)) %>%
  group_by(deployment_id) %>%
  summarise(
    res = mean(res),
    longitude = first(longitude),
    latitude = first(latitude),
    .groups = "drop"
  )

if(nrow(res_df) > 0) {
  coords_mat <- cbind(res_df$longitude, res_df$latitude)
  nb <- knn2nb(knearneigh(coords_mat, k = 5))
  lw <- nb2listw(nb, style = "W")
  
  cat("\n--- Moran's I Test ---\n")
  print(moran.test(res_df$res, lw))
} else {
  cat("\nWARNING: No spatial match found.\n")
}

cat("\n>>> FINAL STATISTICAL DIAGNOSTICS COMPLETE\n")
