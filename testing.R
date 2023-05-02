# Testing File

# clear environment
rm(list = ls())

# load healthyr
devtools::load_all()

# Load sample data

df2 <- format_nut_health_indicators(df = raw_fsl1,
                                     cluster = "cluster_id", enum = "enum", date_of_dc = "today",

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

# df3 <- df2[[1]]
# (map <- df2[[2]])

(plaus <- create_fsl_quality_report(df2, grouping = "enum", short_report = FALSE))
View(plaus)

writexl::write_xlsx(list(df2, plaus), "checking_fsl_calcs.xlsx")

library(healthyr)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(DT)

run_fsl_monitoring_dashboard(df = df2, grouping_var = "enum")

# bad_entry <- c(5,4,5,4,5,4,5,4)
# ok_entry1 <- c(7,5,4,0,0,3,6,7)
# ok_entry2 <- c()
#
# mean(bad_entry)
# mean(ok_entry1)
# sd(bad_entry)
# sd(ok_entry1)
# median(bad_entry)
# median(ok_entry1)
#
# (max(bad_entry) - min(bad_entry))
# (max(ok_entry1) - min(ok_entry1))

#
# test <- df1 %>%
#   dplyr::rowwise() %>%
#   dplyr::mutate(sd_foods = sd(c(F02A, F02B, F03A), na.rm = TRUE))
#
#
#
#
#
#


