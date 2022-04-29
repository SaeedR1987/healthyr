# TEMPLATE HEALHTYR ANALYSIS DOCUMENT for WG-SS SURVEY Data
#
# For use by REACH Initiative HQ and Country Teams
# Drafted 18 April 2022 by Cluster Support Unit (CSU)
# If any issues with the scripts or troubleshooting needed,
# please contact saeed.rahman@reach-initiative.org

# Setup ####

rm(list = ls())

# remotes::install_github("SaeedR1987/healthyr")

library(tidyverse)
library(healthyr)

# Step 1: Load your Dataset ####

wgss_data <- raw_wgss3

# Step 2: Format Your Dataset ####

df2 <- format_nut_health_indicators(df = wgss_data,
                                    hhid = "X_uuid",
                                    cluster = "cadaster",
                                    sex_var = "gender_ind",
                                    age_years_var = "age_yrs_ind",
                                    wgss_seeing = "wgss_seeing",
                                    wgss_hearing = "wgss_hearing",
                                    wgss_walking = "wgss_walking",
                                    wgss_remembering = "wgss_remembering",
                                    wgss_selfcare = "wgss_selfcare",
                                    wgss_communicating = "wgss_communicating"
                                    )

# Step 3: Create a Quality Summary Report ####

# PLAUSIBILITY REPORT TO BE DEVELOPED for WGSS #

# Ideas for Plausibility Report
# Illogical reporting of domains - mobility issues but no self-care? etc?
# Demographic checks for sex and age distributions
# % of disability 1,2,3,4 for different age groups, with the expectation older people should have a higher % than younger people
# p-value association between domains expected to be reported together?
# % of interviews directly asked to the person
# Multiple individuals with disability in the same household?
#

# Step 4: Evaluate Data with Visualizations ####

(plot_agepyramid(df2))

(plot_agepyramid(df2, filtering = "disability1"))

(plot_agepyramid(df2, filtering = "disability2"))

(plot_agepyramid(df2, filtering = "disability3"))

(plot_agepyramid(df2, filtering = "disability4"))

(plot_age_years_distribution(df2, min_age = 15, max_age = 80, breaks = 1))

(plot_age_years_distribution(df2, min_age = 0, max_age = 18, breaks = 1))

# Step 5: Export Flagged Records to Cleaning Log + Cleaning ####

(flag_summary <- flag_summary_table(df = df2))

(flag_summary <- flag_summary_table(df = df2, grouping = "region"))

cl <- create_cleaning_log_flags(df = df2, uuid_col = "hh_id")

View(cl)

# Step 6: Analyse Survey Results ####

(res <- analyse_survey_results(df = df2,
                               aggregation = "region",
                               sample_design = "two_stage_stratified", cluster = "cluster",
                               strata = "strata", svy_weights = "svy_weight",
                               proportions = c("disability3", "disability4"),
                               means = c("wgss_sco_score", "wgss_hd_score")))

(res <- analyse_survey_results(df = df2,
                               sample_design = "two_stage_stratified",
                               strata = "strata", svy_weights = "svy_weight", cluster = "cluster",
                               aggregation = "sex",
                               proportions = c("disability3", "disability4"),
                               means = c("wgss_sco_score", "wgss_hd_score")))

(res <- analyse_survey_results(df = df2,
                               sample_design = "two_stage_stratified",
                               cluster = "cluster",
                               strata = "strata", svy_weights = "svy_weight",
                               aggregation = "age_group",
                               proportions = c("disability3", "disability4"),
                               means = c("wgss_sco_score", "wgss_hd_score")))








