# TEMPLATE HEALHTYR ANALYSIS DOCUMENT for MORTALITY SURVEY Data
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

roster_data <- raw_mortality_roster1
leavers_data <- raw_mortality_left1
deaths_data <- raw_mortality_died1

# Step 2: Format Your Dataset ####

df2 <- format_mortality_current_census(date_recall_event = "21/04/2019",
                                       #Current roster data and columns
                                       df_roster = roster_data,
                                       date_dc_roster = "today", enum_roster = "enum", cluster_roster = "cluster_id", admin2_roster = "county",
                                       hh_id_roster = "KEY", sex_roster = "sex_roster", age_roster = "age_years", joined_roster = "joined", birth_roster = "birth",
                                       #Left people data and columns
                                       df_left = leavers_data,
                                       date_dc_left = "today", enum_left = "enum", cluster_left = "cluster_id", admin2_left = "county",
                                       hh_id_left = "KEY", sex_left = "sex_left", age_left = "age_left", birth_left = "birth_left", joined_left = "join_left",

                                       #Died people data and columns
                                       df_died = deaths_data,
                                       date_dc_died = "today", enum_died = "enum", cluster_died = "cluster_id",  admin2_died = "county",
                                       hh_id_died = "KEY", sex_died = "sex_died", age_died = "age_died", birth_died = "birth_died", joined_died = "join_died",
                                       death_cause = "death_cause", death_location = "death_location" )



# Step 3: Create a Quality Summary Report ####
# Ratio is (prevalence / (1 - prevalence))

(create_mortality_quality_report(df2,
                                 grouping = "enum",
                                 short_report = TRUE,
                                 exp_sex_ratio = 1, exp_ratio_0_4 = (0.19 / 1 - 0.19),
                                 exp_ratio_2_5 = (0.4 / (1 - 0.4)), exp_ratio_5_10 = 1.1,
                                 exp_hh_size = 7.5))

(create_mortality_quality_report(df2,
                                 grouping = "enum",
                                 short_report = FALSE,
                                 exp_sex_ratio = 1, exp_ratio_0_4 = (0.19 / 1 - 0.19),
                                 exp_ratio_2_5 = (0.4 / (1 - 0.4)), exp_ratio_5_10 = 1.1,
                                 exp_hh_size = 7.5))

# Step 4: Evaluate Data with Visualizations ####

(plot_agepyramid(df2))

(plot_agepyramid(df2, filtering = "death"))

# Step 5: Export Flagged Records to Cleaning Log + Cleaning ####

(flag_summary <- flag_summary_table(df = df2))

(flag_summary <- flag_summary_table(df = df2, grouping = "enum"))

cl <- create_cleaning_log_flags(df = df2, uuid_col = "hh_id")

View(cl)

# Step 6: Analyse Survey Results ####

(res <- analyse_survey_results(df = df2,
                                  sample_design = "two_stage_cluster",
                                  cluster = "cluster",
                                  ratios_rates.numerators = c("death", "death_under5"),
                                  ratios_rates.denominators = c("person_time", "under_5_pt"),
                                  ratios_rates.multiplier = 10000))

# Step 7: Exporting Results to ENA ####

df2 %>% healthyr::format_mortality_to_ena() %>% writexl::write_xlsx("my_ena_mortality_data.xlsx")








