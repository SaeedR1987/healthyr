# TEMPLATE HEALHTYR ANALYSIS DOCUMENT for Food Security Outcomes Data
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

df <- raw_fsl1

# Step 2: Format Your Dataset ####

df2 <- format_nut_health_indicators(df = df,

                                    cluster = "cluster_id", enum = "enum",

                                    fcs_cereal = "F01A", fcs_legumes = "F02A", fcs_dairy = "F03A", fcs_meat = "F04A", fcs_veg = "F05A", fcs_fruit = "F06A", fcs_oil = "F07A", fcs_sugar = "F08A",

                                    hdds_cereals = "F011B", hdds_tubers = "F012B", hdds_dairy = "F03B", hdds_veg = "F05B", hdds_fish = "F043B", hdds_meat = "hdds_meats_any", hdds_eggs = "F044B", hdds_fruit = "F06B", hdds_legumes = "F02B", hdds_condiments = "F09B", hdds_sugars = "F08B", hdds_oils = "F07B",

                                    hhs_nofoodhh_1 = "hhs_1", hhs_nofoodhh_1a = "hhs_2", hhs_sleephungry_2 = "hhs_3", hhs_sleephungry_2a = "hhs_4", hhs_alldaynight_3 = "hhs_5", hhs_alldaynight_3a = "hhs_6",

                                    rcsi_lesspreferred_1 = "rcsi1", rcsi_borrowfood_2 = "rcsi2", rcsi_limitportion_3 = "rcsi3", rcsi_restrict_4 = "rcsi4", rcsi_reducemeals5 = "rcsi5",

                                    lcs_variables = c("lcs1", "lcs2", "lcs3", "lcs4", "lcs5", "lcs6", "lcs7", "lcs8", "lcs9", "lcs10")
)

# Step 3: Review a Quality Summary Report ####

(create_fsl_quality_report(df = df2, short_report = TRUE))

(create_fsl_quality_report(df = df2, short_report = FALSE))

(create_fsl_quality_report(df = df2, grouping = "enum", short_report = TRUE))

(create_fsl_quality_report(df = df2, grouping = "enum", short_report = FALSE))

# Step 4: Evaluate Data with Visualizations ####

(plot_ridge_distribution(df2, numeric_cols = c("fcs_cereal", "fcs_dairy", "fcs_veg", "fcs_fruit", "fcs_legumes", "fcs_sugar", "fcs_oil"),
                         name_groups = "Food Groups", name_units = "Days"))

(plot_ridge_distribution(df2, numeric_cols = c("fcs_cereal", "fcs_dairy", "fcs_veg", "fcs_fruit", "fcs_legumes", "fcs_sugar", "fcs_oil"),
                         name_groups = "Food Groups", name_units = "Days", grouping = "enum"))

(plot_ridge_distribution(df2, numeric_cols = c("rcsi_lesspreferred_1", "rcsi_borrowfood_2", "rcsi_limitportion_3","rcsi_restrict_4", "rcsi_reducemeals5"),
                         name_groups = "Food Coping Strategy", name_units = "Days"))

(plot_ridge_distribution(df2, numeric_cols = c("rcsi_lesspreferred_1", "rcsi_borrowfood_2", "rcsi_limitportion_3","rcsi_restrict_4", "rcsi_reducemeals5"),
                         name_groups = "Food Coping Strategy", name_units = "Days", grouping = "enum"))

(plot_correlogram(df2, numeric_cols = c("fcs_score", "hdds_score", "rcsi_score",  "hhs_score")))

# Step 5: Export Flagged Records to Cleaning Log + Cleaning ####

(flag_summary <- flag_summary_table(df = df2, grouping = "enum"))

cl <- create_cleaning_log_flags(df = df2, uuid_col = "KEY")
View(cl)

# Step 6: Analyse Survey Results ####

(res <- analyse_survey_results(df = df2,

                               aggregation = "enum",

                                  sample_design = "two_stage_cluster",
                                  cluster = "cluster",

                                  proportions = c("fcs_cat", "hhs_cat", "hdds_cat", "rcsi_cat", "lcs_cat",
                                                  "fc_phase", "fclc_phase"),

                                  means = c("fcs_score", "hhs_score", "rcsi_score")))









