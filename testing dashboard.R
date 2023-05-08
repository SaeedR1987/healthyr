

rm(list = ls())

# devtools::load_all()
library(healthyr)

df <- healthyr::raw_fsl1

df2 <- format_nut_health_indicators2(df = df, date_of_dc = "today",
                                     cluster = "cluster_id", enum = "enum",
                                     fcs_cereal = "F01A", fcs_legumes = "F02A", fcs_dairy = "F03A", fcs_meat = "F04A", fcs_veg = "F05A", fcs_fruit = "F06A", fcs_oil = "F07A", fcs_sugar = "F08A",
                                     hdds_cereals = "F011B", hdds_tubers = "F012B", hdds_dairy = "F03B", hdds_veg = "F05B", hdds_fish = "F043B", hdds_meat = "hdds_meats_any", hdds_eggs = "F044B", hdds_fruit = "F06B", hdds_legumes = "F02B", hdds_condiments = "F09B", hdds_sugars = "F08B", hdds_oils = "F07B",
                                     hhs_nofoodhh_1 = "hhs_1", hhs_nofoodhh_1a = "hhs_2", hhs_sleephungry_2 = "hhs_3", hhs_sleephungry_2a = "hhs_4", hhs_alldaynight_3 = "hhs_5", hhs_alldaynight_3a = "hhs_6",
                                     rcsi_lesspreferred_1 = "rcsi1", rcsi_borrowfood_2 = "rcsi2", rcsi_limitportion_3 = "rcsi3", rcsi_restrict_4 = "rcsi4", rcsi_reducemeals5 = "rcsi5",
                                     lcs_variables = c("lcs1", "lcs2", "lcs3", "lcs4", "lcs5", "lcs6", "lcs7", "lcs8", "lcs9", "lcs10"))

df3 <- df2[[1]]
head(df3)

cols <- intersect(c("fcs_cereal", "fcs_legumes", "fcs_dairy", "fcs_meat", "fcs_veg", "fcs_fruit", "fcs_oil", "fcs_sugar"), colnames(df3))

plot_ridge_distribution(df = df3, numeric_cols = cols)

run_fsl_monitoring_dashboard(df = df2, grouping_var = "enum")



df3 <- proc_anthro_iycf2

library(healthyr)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(shinyWidgets)
library(plotly)
library(DT)

run_iycf_monitoring_dashboard(df = df3, grouping_var = "enum")

df4 <- proc_mortality1

run_mortality_monitoring_dashboard(df = df4, grouping_var = "enum")

