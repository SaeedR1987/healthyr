# TEMPLATE HEALHTYR ANALYSIS DOCUMENT for Food Security Outcomes Data
#
# For use by REACH Initiative HQ and Country Teams
# Drafted 01 May 2023 by Public Health Unit at IMPACT HQ
# If any issues with the scripts or troubleshooting needed,
# please contact saeed.rahman@impact-initiative.org
# or Olivia Falkowitz, IMPACT FSL Focal Point (olivia.falkowitz@impact-initiatives.org)

# Setup ####

rm(list = ls())

# remotes::install_github("SaeedR1987/healthyr")

library(tidyverse)
library(healthyr)

# Step 1: Load your Dataset ####

df <- raw_fsl1

# Step 2: Format Your Dataset ####

df3 <- format_nut_health_indicators(df = raw_fsl1,
                                    cluster = "cluster_id",
                                    enum = "enum",
                                    date_of_dc = "today",

                                    # FSL Outcome Indicators
                                    fcs_cereal = "F01A", fcs_legumes = "F02A", fcs_dairy = "F03A", fcs_meat = "F04A", fcs_veg = "F05A", fcs_fruit = "F06A", fcs_oil = "F07A", fcs_sugar = "F08A",
                                    hdds_cereals = "F011B", hdds_tubers = "F012B", hdds_dairy = "F03B", hdds_veg = "F05B", hdds_fish = "F043B", hdds_meat = "hdds_meats_any", hdds_eggs = "F044B", hdds_fruit = "F06B", hdds_legumes = "F02B", hdds_condiments = "F09B", hdds_sugars = "F08B", hdds_oils = "F07B",
                                    hhs_nofoodhh_1 = "hhs_1", hhs_nofoodhh_1a = "hhs_2", hhs_sleephungry_2 = "hhs_3", hhs_sleephungry_2a = "hhs_4", hhs_alldaynight_3 = "hhs_5", hhs_alldaynight_3a = "hhs_6",
                                    rcsi_lesspreferred_1 = "rcsi1", rcsi_borrowfood_2 = "rcsi2", rcsi_limitportion_3 = "rcsi3", rcsi_restrict_4 = "rcsi4", rcsi_reducemeals5 = "rcsi5",

                                    # Livelihood Coping Strategy Indicators
                                    lcs_variables = c("lcs1", "lcs2", "lcs3", "lcs4", "lcs5", "lcs6", "lcs7", "lcs8", "lcs9", "lcs10"),

                                    # Income variables as in MSNA Indicator Bank
                                    livelihood_variables = c("income_salaried", "income_casual", "income_trade", "income_own_production", "income_social_benefits",
                                                             "income_rent", "income_remittances", "income_loans_family", "income_loans_community",
                                                             "income_humanitarian_assistance", "income_other"),

                                    # Expenditure Indicators as in MSNA Indicator Bank
                                    food_exp_col = "exp_food",
                                    health_exp_col = "exp_health",
                                    monthly_expenditures = c("exp_food", "exp_rent", "exp_nfi_monthly", "exp_utilities", "exp_fuel", "exp_transport", "exp_comms", "exp_other_monthly"),
                                    period_expenditures = c("exp_shelter", "exp_nfi_infrequent", "exp_health", "exp_education", "exp_debt", "exp_other_infrequent"),
                                    num_period_months = 6

)


# Step 3: Review a Quality Summary Report ####

(create_fsl_quality_report(df = df2, short_report = TRUE))

(create_fsl_quality_report(df = df2, short_report = FALSE))

(create_fsl_quality_report(df = df2, grouping = "enum", short_report = TRUE))

(create_fsl_quality_report(df = df2, grouping = "enum", short_report = FALSE))

# Step 4a: Evaluate Data with Visualizations ####

(plot_ridge_distribution(df2, numeric_cols = c("fcs_cereal", "fcs_dairy", "fcs_veg", "fcs_fruit", "fcs_legumes", "fcs_sugar", "fcs_oil"),
                         name_groups = "Food Groups", name_units = "Days"))

(plot_ridge_distribution(df2, numeric_cols = c("fcs_cereal", "fcs_dairy", "fcs_veg", "fcs_fruit", "fcs_legumes", "fcs_sugar", "fcs_oil"),
                         name_groups = "Food Groups", name_units = "Days", grouping = "enum"))

(plot_ridge_distribution(df2, numeric_cols = c("rcsi_lesspreferred_1", "rcsi_borrowfood_2", "rcsi_limitportion_3","rcsi_restrict_4", "rcsi_reducemeals5"),
                         name_groups = "Food Coping Strategy", name_units = "Days"))

(plot_ridge_distribution(df2, numeric_cols = c("rcsi_lesspreferred_1", "rcsi_borrowfood_2", "rcsi_limitportion_3","rcsi_restrict_4", "rcsi_reducemeals5"),
                         name_groups = "Food Coping Strategy", name_units = "Days", grouping = "enum"))

(plot_correlogram(df2, numeric_cols = c("fcs_score", "hdds_score", "rcsi_score",  "hhs_score")))

# Step 4b: Data Quality Checking Dashboard
# install and load these libraries below in order to run the dashboard
# for the paramater df, use the formatted dataset from format_nut_health_indicators
# for the paramater grouping_var,

library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(DT)

healthyr::run_fsl_monitoring_dashboard(df = df2, grouping_var = "enum", filter_var1 = "cluster")

# Step 5: Export Flagged Records to Cleaning Log + Cleaning ####

(flag_summary <- flag_summary_table(df = df2, grouping = "enum"))

cl <- create_cleaning_log_flags(df = df2, uuid_col = "KEY")
View(cl)

# Step 6: Analyse Survey Results ####
# if cluster survey, use sample_design = "two_stage_cluster"
# if not cluster and multiple areas, use sample_design = "two_stage_stratified"

(res <- analyse_survey_results(df = df2 %>% dplyr::mutate(food_exp_share = as.numeric(food_exp_share)),

                                  # aggregation = "enum",

                                  # sample_design = "two_stage_cluster",
                                  sample_design = "two_stage_stratified",
                                  cluster = "cluster",

                                  proportions = c("fcs_cat", "hhs_cat", "hdds_cat", "rcsi_cat", "lcs_cat",
                                                  "fc_phase", "fclc_phase"),

                                  means = c("fcs_score", "hhs_score", "rcsi_score", "food_exp_share")))









