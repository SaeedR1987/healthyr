
rm(list = ls())

# remotes::install_github("SaeedR1987/healthyr")

library(tidyverse)
# library(healthyr)

# Step 1: Load your Dataset ####

roster_data <- read.csv("MultiSectoral_HHS-A_current_member.csv")
leavers_data <- read.csv("MultiSectoral_HHS-D_left_member.csv")
deaths_data <- read.csv("MultiSectoral_HHS-E_died_member.csv")

# Step 2: Format Your Dataset ####

# include situation where dob_nown = "no" and age estimate is included. create an estimate birthdate

# some basic date cleaning first

roster_data2 <- roster_data %>%
  mutate(birthdate = format(lubridate::parse_date_time(birthdate, orders = "dmy"), "%Y-%m-%d" ),
         curr_joined_date = format(lubridate::parse_date_time(curr_joined_date, orders = "dmy"), "%Y-%m-%d" ),
         calc_roster_date_dc = stringr::str_sub(calc_roster_date_dc, start = 1, end = 10),
         calc_roster_date_dc = format(lubridate::parse_date_time(calc_roster_date_dc, orders = "ymd"), "%Y-%m-%d" ),
         calc_roster_year_dc = lubridate::year(calc_roster_date_dc),
         calc_roster_month_dc = lubridate::month(calc_roster_date_dc),
         calc_roster_day_dc = lubridate::day(calc_roster_date_dc),
         days_since_born = ifelse(is.na(months), NA, round(months*(365.25/12))),
         #
         birthdate = ifelse(is.na(dob_known), NA, ifelse(dob_known == "no" & !is.na(months), as.Date(paste(calc_roster_year_dc, calc_roster_month_dc, calc_roster_date_dc, sep = "/")) - days_since_born, as.Date(birthdate) )),
         birthdate = lubridate::as_date(birthdate),

         born = ifelse(is.na(birthdate), NA, ifelse(birthdate > format(lubridate::parse_date_time("2020-11-04", orders = "ymd"), "%Y-%m-%d"), "y", "n" ))
         )

leavers_data2 <- leavers_data %>%
  mutate(left_leave_date = lubridate::parse_date_time(left_leave_date, orders = "mdy", tz = ""),
         left_join_date = lubridate::parse_date_time(left_join_date, orders = "mdy", tz = ""),
         calc_left_date_dc= stringr::str_sub(calc_left_date_dc, start = 1, end = 10),
         calc_left_date_dc = lubridate::parse_date_time(calc_left_date_dc, orders = "ymd"),
         birth_left = "")

deaths_data2 <- deaths_data %>%
  mutate(died_join_date = lubridate::parse_date_time(died_join_date, orders = "mdy"),
         died_birthdate = lubridate::parse_date_time(died_birthdate, orders = "mdy"),

         calc_death_today= stringr::str_sub(calc_death_today, start = 1, end = 10),
         calc_death_today = lubridate::parse_date_time(calc_death_today, orders = "ymd"),

         calc_death_year_dc = lubridate::year(calc_death_today),
         calc_death_month_dc = lubridate::month(calc_death_today),
         calc_death_day_dc = lubridate::day(calc_death_today),

         calc_death_date_final= stringr::str_sub(calc_death_date_final, start = 1, end = 10),
         calc_death_date_final = lubridate::parse_date_time(calc_death_date_final, orders = "ymd"),

         days_since_born = ifelse(is.na(died_months), NA, round(died_months*(365.25/12))),
         died_birthdate = ifelse(is.na(died_dob_known), NA, ifelse(died_dob_known == "no" & !is.na(died_months), as.Date(paste(calc_death_year_dc, calc_death_month_dc, calc_death_day_dc, sep = "/")) - days_since_born, as.Date(died_birthdate) )),
         died_birthdate = lubridate::as_date(died_birthdate),
         died_born = ifelse(is.na(died_birthdate), NA, ifelse(died_birthdate > format(lubridate::parse_date_time("2020-11-04", orders = "ymd"), "%Y-%m-%d"), "y", "n" ))

         )

# formatting and merging the datasets

df2 <- format_mortality_current_census(date_recall_event = "03/11/2020",
                                       #Current roster data and columns
                                       df_roster = roster_data2, # your current roster dataset
                                       date_dc_roster = "calc_roster_date_dc", # date of data collection
                                       enum_roster = "calc_roster_enum", # enumerator or team id
                                       cluster_roster = "calc_roster_site", # cluster id
                                       admin2_roster = "calc_roster_admin2", # optional but recommended admin level
                                       hh_id_roster = "PARENT_KEY", # unique household id, like the UUID. Must match between datasets
                                       sex_roster = "sex_roster",
                                       age_roster = "age_years",
                                       joined_roster = "curr_joined",
                                       birth_roster = "born",
                                       joined_date_roster = "curr_joined_date",
                                       birthdate_roster = "birthdate",

                                       #Left people data and columns
                                       df_left = leavers_data2, # Left household member dataset
                                       date_dc_left = "calc_left_date_dc", # date of data collection
                                       enum_left = "calc_left_enum", # enumerator or team number
                                       cluster_left = "calc_left_site", # cluster id
                                       admin2_left = "calc_left_admin2", #optional but recommended admin level
                                       hh_id_left = "PARENT_KEY", # unique household id, like the UUID. Must match between datasets
                                       sex_left = "sex_left", # sex of left household member
                                       age_left = "age_left", # age in years of left household member
                                       birth_left = "birth_left", # birth of left household member
                                       joined_left = "join_left",  # joined the household before leaving the household
                                       joined_date_left = "left_join_date",
                                       left_date_left = "left_leave_date",

                                       #Died people data and columns
                                       df_died = deaths_data2, # your deaths dataset
                                       date_dc_died = "calc_death_today", # date of data collection
                                       enum_died = "calc_death_enum",# enumerator or team number
                                       cluster_died = "calc_death_site", # cluster id
                                       admin2_died = "calc_death_admin2", #optional but recommended admin level
                                       hh_id_died = "PARENT_KEY", # unique household id, like the UUID. Must match between datasets
                                       sex_died = "sex_died", # sex of deceased
                                       age_died = "age_died", # age in years of the deceased at time of death
                                       birth_died = "died_born", # born during recall period
                                       joined_died = "died_join", # joined the HH during recall period
                                       death_cause = "death_cause_inj_trauma", # cause of death
                                       death_location = "death_location",  # location of death
                                       date_death = "calc_death_date_final",
                                       joined_date_died = "died_join_date",
                                       birthdate_died = "died_birthdate",
)

writexl::write_xlsx(df2, "testmortscript.xlsx")



test_fsl <- raw_fsl1

test_fsl2 <- format_nut_health_indicators(df = test_fsl, date_of_dc = "today",
  cluster = "cluster_id", enum = "enum",

  fcs_cereal = "F01A", fcs_legumes = "F02A", fcs_dairy = "F03A", fcs_meat = "F04A", fcs_veg = "F05A", fcs_fruit = "F06A", fcs_oil = "F07A", fcs_sugar = "F08A",

  hdds_cereals = "F011B", hdds_tubers = "F012B", hdds_dairy = "F03B", hdds_veg = "F05B", hdds_fish = "F043B", hdds_meat = "hdds_meats_any", hdds_eggs = "F044B", hdds_fruit = "F06B", hdds_legumes = "F02B", hdds_condiments = "F09B", hdds_sugars = "F08B", hdds_oils = "F07B",

  hhs_nofoodhh_1 = "hhs_1", hhs_nofoodhh_1a = "hhs_2", hhs_sleephungry_2 = "hhs_3", hhs_sleephungry_2a = "hhs_4", hhs_alldaynight_3 = "hhs_5", hhs_alldaynight_3a = "hhs_6",

  rcsi_lesspreferred_1 = "rcsi1", rcsi_borrowfood_2 = "rcsi2", rcsi_limitportion_3 = "rcsi3", rcsi_restrict_4 = "rcsi4", rcsi_reducemeals5 = "rcsi5",

  lcs_variables = c("lcs1", "lcs2", "lcs3", "lcs4", "lcs5", "lcs6", "lcs7", "lcs8", "lcs9", "lcs10"))

run_monitoring_dashboard(df = test_fsl2,
                         grouping_var = "enum",
                         filter_var1 = "cluster",
                         filter_var2 = "fcs_cereal")



(plot_ridge_distribution(df = proc_anthro1,
                         # numeric_cols = c("rcsi_lesspreferred_1","rcsi_borrowfood_2","rcsi_limitportion_3","rcsi_restrict_4","rcsi_reducemeals5")
  ))





