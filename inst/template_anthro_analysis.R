# TEMPLATE HEALHTYR ANALYSIS DOCUMENT for ANTHROPOMETRIC Data
#
# For use by REACH Initiative HQ and Country Teams
# Drafted 18 April 2022 by Cluster Support Unit (CSU)
# If any issues with the scripts or troubleshooting needed,
# please contact saeed.rahman@reach-initiative.org

# Setup ####

rm(list = ls())

# remotes::install_github("SaeedR1987/healthyr")

# library(tidyverse)
# library(healthyr)

devtools::load_all()

# Step 1: Load your Dataset ####

df <- raw_anthro1

# df <- readxl::read_xlsx("kalonge 17082022.xlsx")

# Step 2: Format Your Dataset ####

df2 <- format_nut_health_indicators(df = df,

                                    hhid = "KEY",
                                    date_of_dc = "today",
                                    cluster = "cluster_id",
                                    enum = "enum",
                                    sex_var = "child_sex",
                                    age_months_var = "age",
                                    date_of_birth = "birthdate",
                                    weight_var = "weight",
                                    height_var = "height",
                                    muac_var = "muac",
                                    oedema_var = "edema_confirm",
                                    )

df3 <- flag_anthro_issues(df2)

# df3 <- flag_anthro_issues(df2, grouping = "")

# Step 3: Create a Quality Summary Report ####

(create_anthro_quality_report(df = df3, short_report = TRUE))

(a <- create_anthro_quality_report(df = df3, grouping = "enum", short_report = FALSE))

# Step 4: Evaluate Data with Visualizations ####
# Use 'wfhz' for index for Weight for Height z-score
# Use 'hfaz' for index for Height for Age z-score
# Use 'wfaz' for index for Weight for Age z-score
# Use 'mfaz' for index for MUAC for Age z-score

(plot_age_months_distribution(df3))

(plot_age_months_distribution(df3, by_group = "enum"))

(plot_age_proxy_distribution(df = df3, by_group = "enum"))

(plot_zscore_distribution(df = df3, index = "wfhz", flags = "yes"))

(plot_zscore_distribution(df = df3, index = "wfhz", flags = "no", grouping = "enum"))

(plot_anthro_age_distribution(df = df3, index = "mfaz"))

(plot_cumulative_distribution(df = df3, index = "muac", flags = "no"))

(plot_cumulative_distribution(df = df3, index = "muac", flags = "yes", grouping = "enum"))

(plot_cumulative_distribution(df = df3, index = "wfhz", flags = "no"))

(plot_cumulative_distribution(df = df3, index = "wfhz", flags = "no", grouping = "enum"))

# Step 5: Export Flagged Records to Cleaning Log + Cleaning ####

(flag_summary <- flag_summary_table(df = df3))

cl <- create_cleaning_log_flags(df = df3, uuid_col = "hh_id")
View(cl)

# Step 6: Analyse Survey Results ####

(res <- analyse_survey_results(df = df3,

                                  sample_design = "two_stage_cluster",
                                  cluster = "cluster",

                                  proportions = c("gam_wfhz_noflag", "mam_wfhz_noflag", "sam_wfhz_noflag",
                                                  "global_stunting_noflag", "moderate_stunting_noflag", "severe_stunting_noflag",
                                                  "global_underweight_noflag", "moderate_underweight_noflag", "severe_underweight_noflag",
                                                  "gam_muac_noflag", "mam_muac_noflag", "sam_muac_noflag"),

                                  means = c("wfhz_noflag", "hfaz_noflag", "wfaz_noflag", "muac_noflag")))

# Step 7: Exporting Results to ENA ####

df3 %>% healthyr::format_anthro_to_ena() %>% writexl::write_xlsx("my_ena_anthro_data.xlsx")








