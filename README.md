1. Internship Research: Quantifying the Relationship Between Environmental Variables and BirdNET Classification Confidence Using Earth Observation Data and GIS

2. Host Institute: Norwegian Institute of Nature Research, Trondheim

3. Author: Smit Rajshekhar Patel, MSc Geoinformation Science and Earth Observation, Faculty ITC, University of Twente 

4. Project Overview
    This repository contains the complete analytical pipeline for my internship work at the Norwegian Institute for Nature Research (NINA). The project integrates      acoustic monitoring data (TABMON) with multi-source environmental variables to analyze the factors influencing BirdNET detection confidence.

5. Detailed Repository Structure

01_BirdNET
    01_Data_Merge.R
    02_Standardize_Nomenclature.R
    03_Geospatial_Integration.R
    04_Temporal_Refinement.R
    05_Structural_Audits.R
    06_Audit_Implementation.R
    07_Post_Audit_Summary.R
    08_Pre_DataAggregation_Diagnostics.R
    09_Data_Aggregation.R
    10_PostAggregation_SpeciesFilter.R
    11_PreModelling_DataCheck.R
    12_PreModelling_Data_Summary.R
    13_Filter_n_detections.R
    14_Statistical_Diagnostics.R
02_Environmental_Variables
    01_Env_Var_Extraction_GEE
    02_Env_Var_Visualisation_GEE
    03_Env_Var_Merge.R
    04_Correlation_Plots.R
03_Data_Mining
    01_Data_Mining_1.R
    02_Data_Mining_2_IUCN_Redlist.R
    03_Data_Mining_3_Env_Var_Conf_ScatterPlots.R
04_Modelling
    01_Modelling.R
    02_Modelling_Visualization.R
    

4. Technical Implementation
    Languages: R (Tidyverse, Arrow, Patchwork) & JavaScript (Google Earth Engine).
    Data Handling: Optimized for .parquet format to manage high-volume acoustic datasets.
    Spatial Resolution: All environmental variables are extracted using a 250m buffer around TABMON recording stations.

5. Note on Data: Raw acoustic recordings and processed datasets are proprietary to NINA. This repository serves as a code record for reproducibility.
