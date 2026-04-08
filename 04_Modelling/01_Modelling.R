# ==============================================================================
# Script ID:   01_Modelling.R
# Project:     Quantifying the Relationship Between Environmental Variables and BirdNET Classification Confidence Using Earth Observation Data and GIS
# Section:     Analysis - Confidence Ladder & Detection Abundance
# Purpose:     Execute the full statistical pipeline: from baseline Beta 
#              regressions (M0-M3) to advanced GAMMs (M4-M7) and Negative 
#              Binomial count models for ecological activity.
# ==============================================================================

# [SECTION] Load Libraries -----------------------------------------------------
library(tidyverse)
library(arrow)
library(mgcv)
library(betareg)
library(parallel)
library(here) # Used for relative path management

# [SECTION] 1. HARDWARE & PATHS ------------------------------------------------
nc_cores <- max(1, detectCores()-1)

# Using here() to maintain project portability
bird_path <- here("Final_Scripts_Datasets_March23", "Datasets", "Processed_Datasets", "Aggregated", "BirdNET_FINAL_MODEL_INPUT_Ngt1.parquet")
env_path  <- here("Final_Scripts_Datasets_March23", "Datasets", "Processed_Datasets", "Env_Var", "Integrated_Env_var", "Env_Variables_Clean_No_Multicollinearity.parquet")

# Output Plot Directory
plot_dir <- here("Final_Scripts_Datasets_March23", "Plots")
if(!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

# [SECTION] 2. DATA LOADING & JOINING ------------------------------------------
cat(">>> Loading and Joining Datasets...\n")

bird <- read_parquet(bird_path)

env  <- read_parquet(env_path) %>%
  rename(
    deployment_id = DplymID,
    local_date    = Date,
    local_hour    = HourUTC,
    DIST_WATER    = GEO101_DistToWater_m,
    nightlights   = ANTH102_NightLights,
    builtup       = ANTH101_BuiltUpFrac,
    TEI           = TER301_TopographicExposureIndex
  )

# Standardize join keys
bird <- bird %>% mutate(local_hour = as.numeric(local_hour))
env  <- env  %>% mutate(local_hour = as.numeric(local_hour))

master_df <- bird %>%
  inner_join(env, by = c("deployment_id","local_date","local_hour"))

# [SECTION] 3. DATA REFINEMENT (Scaling & Squishing) ---------------------------
# Essential for "Rigorous" variable importance
env_vars <- c("CLIM_T_C", "CLIM_RH_pct", "CLIM_WindSpeed_m_s", "CLIM_Precip_mm_hr", 
              "ECO101_LAI", "VEG201_AcousticVegetationImpedance", "DIST_WATER", "TEI")

master_df <- master_df %>%
  mutate(
    # 1. Prep Response for Beta Regression (must be 0 < y < 1)
    confidence_beta = pmin(pmax(confidence_mean, 0.0001), 0.9999),
    # 2. Factorize for Random Effects
    scientific_name = as.factor(scientific_name),
    deployment_id = as.factor(deployment_id)
  ) %>%
  # 3. SCALE PREDICTORS (Z-scores)
  mutate(across(all_of(env_vars), ~ as.numeric(scale(.)))) %>%
  drop_na(all_of(env_vars))

cat("Final Rows for Analysis:", nrow(master_df), "\n")

# [SECTION] PART A: THE CONFIDENCE LADDER (CLASSIFICATION PERFORMANCE) ---------
cat("\n>>> STARTING CONFIDENCE MODELLING LADDER (BETA REGRESSION)...\n")

# M0 — NULL
m0 <- betareg(confidence_beta ~ 1, data = master_df)

# M1 — STRUCTURE (Species Only)
m1 <- betareg(confidence_beta ~ scientific_name, data = master_df)

# M2 — TIME + ANTHRO
m2 <- betareg(
  confidence_beta ~ scientific_name + sin(2*pi*local_hour/24) + cos(2*pi*local_hour/24) + nightlights,
  data = master_df
)

# M3 — ENVIRONMENT (LINEAR)
m3 <- betareg(
  confidence_beta ~ CLIM_T_C + CLIM_RH_pct + CLIM_WindSpeed_m_s + CLIM_Precip_mm_hr + 
    ECO101_LAI + VEG201_AcousticVegetationImpedance + DIST_WATER + TEI + 
    scientific_name + sin(2*pi*local_hour/24) + cos(2*pi*local_hour/24) + nightlights,
  data = master_df
)

# M4 — NONLINEAR (Switch to BAM)
m4 <- bam(
  confidence_beta ~ s(CLIM_T_C, k=5) + s(CLIM_RH_pct, k=5) + s(CLIM_WindSpeed_m_s, k=5) + 
    s(CLIM_Precip_mm_hr, k=5) + s(ECO101_LAI, k=5) + s(VEG201_AcousticVegetationImpedance, k=5) + 
    s(DIST_WATER, k=5) + s(TEI, k=5) + scientific_name + 
    sin(2*pi*local_hour/24) + cos(2*pi*local_hour/24) + nightlights,
  family = betar(link="logit"), data = master_df, discrete = TRUE, nthreads = nc_cores
)

# M5 — FULL GAMM (Adding Deployment Random Effect)
m5 <- bam(
  confidence_beta ~ s(CLIM_T_C, k=5) + s(CLIM_RH_pct, k=5) + s(CLIM_WindSpeed_m_s, k=5) + 
    s(CLIM_Precip_mm_hr, k=5) + s(ECO101_LAI, k=5) + s(VEG201_AcousticVegetationImpedance, k=5) + 
    s(DIST_WATER, k=5) + s(TEI, k=5) + s(local_hour, bs="cc", k=10) + 
    s(nightlights, k=5) + s(scientific_name, bs="re") + s(deployment_id, bs="re"),
  knots = list(local_hour = c(0,24)), family = betar(link="logit"),
  data = master_df, discrete = TRUE, nthreads = nc_cores
)

# M5_BASELINE (Time/Site only - to calculate "Environmental Lift")
m5_noENV <- bam(
  confidence_beta ~ s(local_hour, bs="cc", k=10) + s(nightlights, k=5) + 
    s(scientific_name, bs="re") + s(deployment_id, bs="re"),
  knots = list(local_hour = c(0,24)), family = betar(link="logit"),
  data = master_df, discrete = TRUE, nthreads = nc_cores
)

# M6 — INTERACTIONS
m6 <- bam(
  confidence_beta ~ te(CLIM_T_C, CLIM_RH_pct, k=5) + te(CLIM_WindSpeed_m_s, ECO101_LAI, k=5) + 
    s(CLIM_Precip_mm_hr, k=5) + s(VEG201_AcousticVegetationImpedance, k=5) + 
    s(DIST_WATER, k=5) + s(TEI, k=5) + s(local_hour, bs="cc", k=10) + 
    s(nightlights, k=5) + s(scientific_name, bs="re") + s(deployment_id, bs="re"),
  knots = list(local_hour = c(0,24)), family = betar(link="logit"),
  data = master_df, discrete = TRUE, nthreads = nc_cores
)

# M7 — SPECIES-SPECIFIC (Factor-Smooth Interactions)
m7 <- bam(
  confidence_beta ~ s(CLIM_RH_pct, k=5) + s(CLIM_WindSpeed_m_s, k=5) + s(CLIM_Precip_mm_hr, k=5) + 
    s(VEG201_AcousticVegetationImpedance, k=5) + s(DIST_WATER, k=5) + s(TEI, k=5) + 
    s(CLIM_T_C, scientific_name, bs="fs", k=5) + s(ECO101_LAI, scientific_name, bs="fs", k=5) + 
    s(local_hour, bs="cc", k=10) + s(nightlights, k=5) + s(deployment_id, bs="re"),
  knots = list(local_hour = c(0,24)), family = betar(link="logit"),
  data = master_df, discrete = TRUE, nthreads = nc_cores
)

# [SECTION] PART B: DETECTION EXTENSION (ECOLOGICAL ACTIVITY) ------------------
cat("\n>>> STARTING DETECTION ABUNDANCE MODEL (NEGATIVE BINOMIAL)...\n")

m_detections_full <- bam(
  n_detections ~ 
    s(CLIM_T_C, k=5) + 
    s(CLIM_WindSpeed_m_s, k=5) + 
    s(CLIM_Precip_mm_hr, k=5) + 
    s(CLIM_RH_pct, k=5) + 
    s(ECO101_LAI, k=5) + 
    s(VEG201_AcousticVegetationImpedance, k=5) + 
    s(local_hour, bs="cc", k=10) + 
    s(scientific_name, bs="re") + 
    s(deployment_id, bs="re"),
  data = master_df,
  family = nb(), 
  discrete = TRUE,
  nthreads = nc_cores,
  knots = list(local_hour = c(0, 24))
)

# [SECTION] ENVIRONMENT-ONLY VARIANCE (DETECTION MODEL) ------------------------
cat("\n>>> FITTING DETECTION MODEL WITHOUT ENVIRONMENT...\n")

m_detections_noENV <- bam(
  n_detections ~ 
    s(local_hour, bs="cc", k=10) + 
    s(scientific_name, bs="re") + 
    s(deployment_id, bs="re"),
  data = master_df,
  family = nb(),
  discrete = TRUE,
  nthreads = nc_cores,
  knots = list(local_hour = c(0, 24))
)

# [SECTION] MODEL COMPARISON & SAVING ------------------------------------------
dev_full  <- deviance(m_detections_full)
dev_noENV <- deviance(m_detections_noENV)
env_dev_explained <- (dev_noENV - dev_full) / dev_noENV

cat("\nEnvironmental deviance explained (proportion):", round(env_dev_explained, 4), "\n")

cat("\n--- 1. AIC COMPARISON (CONFIDENCE LADDER) ---\n")
print(AIC(m0, m1, m2, m3, m4, m5, m6, m7))

cat("\n--- 2. DEVIANCE EXPLAINED (CONFIDENCE VS DETECTION) ---\n")
cat("Confidence Model (M5):", summary(m5)$dev.expl * 100, "%\n")
cat("Confidence Baseline (M5_noEnv):", summary(m5_noENV)$dev.expl * 100, "%\n")
cat("Detection/Abundance Model:", summary(m_detections_full)$dev.expl * 100, "%\n")

# [SECTION] 7. FULL EXPORT SYSTEM ----------------------------------------------
cat("\n>>> SAVING FULL MODEL OBJECTS AND RESULTS...\n")

model_list <- list(
  m0 = m0, m1 = m1, m2 = m2, m3 = m3, m4 = m4, m5 = m5, 
  m5_noENV = m5_noENV, m6 = m6, m7 = m7, m_detections_full = m_detections_full
)

saveRDS(model_list, file = file.path(plot_dir, "ALL_MODELS_FULL.rds"))

comparison_table <- data.frame(
  Model = names(model_list),
  AIC = sapply(model_list, AIC),
  Deviance_Explained = sapply(model_list, function(x) {
    if(inherits(x, "betareg")) return(x$pseudo.r.squared * 100)
    return(summary(x)$dev.expl * 100)
  })
)

write.csv(comparison_table, file = file.path(plot_dir, "Model_Comparison_Table.csv"), row.names = FALSE)

# Export summaries to TXT
summary_file <- file.path(plot_dir, "ALL_MODEL_SUMMARIES.txt")
sink(summary_file)
for(name in names(model_list)) {
  cat("\n--------------------------------------------------\nMODEL:", name, "\n")
  print(summary(model_list[[name]]))
}
sink()

# Export Coefficients & Smooth Terms
extract_betareg_coefs <- function(model, name) {
  s <- summary(model)
  coefs <- as.data.frame(s$coefficients$mean)
  coefs$term <- rownames(coefs); coefs$model <- name
  return(coefs)
}

betareg_coefs <- do.call(rbind, lapply(c("m0", "m1", "m2", "m3"), function(n) extract_betareg_coefs(model_list[[n]], n)))
write.csv(betareg_coefs, file = file.path(plot_dir, "Betareg_Coefficients.csv"), row.names = FALSE)

extract_gam_terms <- function(model, name) {
  s <- summary(model)
  smooth <- as.data.frame(s$s.table)
  smooth$term <- rownames(smooth); smooth$model <- name
  return(smooth)
}

gam_terms <- do.call(rbind, lapply(c("m4", "m5", "m6", "m7", "m_detections_full"), function(n) extract_gam_terms(model_list[[n]], n)))
write.csv(gam_terms, file = file.path(plot_dir, "GAM_Smooth_Terms.csv"), row.names = FALSE)

cat("\n>>> ALL RESULTS SUCCESSFULLY SAVED.\nLocation:", plot_dir, "\n")
