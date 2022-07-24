# Cleaning and Analysis Script for

# Step 1: Clear environment #####
rm(list = ls())

# Step 2: Load packages and libraries #####

library(tidyverse)
# library(healthyr)
library(surveyweights)

devtools::load_all()

# Step 3: Load the Data #####



household_data <- readxl::read_xlsx("4. eth_hhs_data_clean_to_share.xlsx", sheet = 1) %>%
  mutate(service_referral = NULL, service_referral2 = NULL, service_mapping_info = NULL, enu_notes = NULL)

sites <- household_data %>% dplyr::select(zone, Woreda, enumerator_id, team_id, site, KEY) %>% rename(PARENT_KEY = KEY)

# woman_data <- read.csv("data/recent_06302022/MultiSectoral_HHS-C_woman_health.csv") %>% mutate(calc_woman_name = NULL)
# child_data <- read.csv("data/recent_06302022/MultiSectoral_HHS-B_U5_health.csv") %>% mutate(calc_child_health_name = NULL)
roster_data <- readxl::read_xlsx("4. eth_hhs_data_clean_to_share.xlsx", sheet = 2) %>% mutate(name = NULL) %>% dplyr::left_join(sites, by = "PARENT_KEY")
leavers_data <- readxl::read_xlsx("4. eth_hhs_data_clean_to_share.xlsx", sheet = 3) %>% mutate(name_left = NULL) %>% dplyr::left_join(sites, by = "PARENT_KEY")
deaths_data <- readxl::read_xlsx("4. eth_hhs_data_clean_to_share.xlsx", sheet = 4) %>% mutate(name_died = NULL) %>% dplyr::left_join(sites, by = "PARENT_KEY")


# Step 4a: Data Preparation and Wrangling #####

# some basic date cleaning first

roster_data2 <- roster_data %>%
  mutate(birthdate = format(lubridate::parse_date_time(birthdate, orders = "ymd"), "%Y-%m-%d" ),
         curr_joined_date = format(lubridate::parse_date_time(curr_joined_date, orders = "ymd"), "%Y-%m-%d" ),
         calc_roster_date_dc = stringr::str_sub(calc_roster_date_dc, start = 1, end = 10),
         calc_roster_date_dc = format(lubridate::parse_date_time(calc_roster_date_dc, orders = "ymd"), "%Y-%m-%d" ),
         calc_roster_year_dc = lubridate::year(calc_roster_date_dc),
         calc_roster_month_dc = lubridate::month(calc_roster_date_dc),
         calc_roster_day_dc = lubridate::day(calc_roster_date_dc),
         days_since_born = ifelse(is.na(months), NA, round(months*(365.25/12))),
         birthdate = ifelse(is.na(dob_known), NA, ifelse(dob_known == "no" & !is.na(months),
                                                         as.Date(paste(calc_roster_year_dc, calc_roster_month_dc, calc_roster_date_dc, sep = "/")) - days_since_born, as.Date(birthdate) )),
         birthdate = lubridate::as_date(birthdate),
         born = ifelse(is.na(birthdate), NA, ifelse(birthdate > format(lubridate::parse_date_time("2020-11-04", orders = "ymd"), "%Y-%m-%d"), "y", "n" ))
  )

leavers_data2 <- leavers_data %>%
  mutate(left_leave_date = lubridate::parse_date_time(left_leave_date, orders = "ymd", tz = ""),
         left_join_date = lubridate::parse_date_time(left_join_date, orders = "ymd", tz = ""),
         calc_left_date_dc= stringr::str_sub(calc_left_date_dc, start = 1, end = 10),
         calc_left_date_dc = lubridate::parse_date_time(calc_left_date_dc, orders = "ymd"),
         birth_left = "")

deaths_data2 <- deaths_data %>%
  dplyr::mutate(calc_death_date_final = ifelse(calc_death_date_final == "44354", "2021-06-07",
                                               ifelse(calc_death_date_final == "44173", "2020-12-08",
                                                      ifelse(calc_death_date_final == "44347", "2021-05-31", calc_death_date_final)))) %>%
  mutate(died_join_date = lubridate::parse_date_time(died_join_date, orders = "ymd"),
         died_birthdate = lubridate::parse_date_time(died_birthdate, orders = "ymd"),
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

  ) %>%
  mutate(cause_of_death = case_when(death_cause_inj_trauma == "yes" ~ "Conflict",
                                    death_cause_animal == "yes" ~ "Animal/Insect",
                                    death_cause_illness_confirm == "yes" & death_cause_woman == "yes" ~ "Maternal",
                                    death_cause_illness_confirm == "yes" ~ "Illness",
                                    TRUE ~ NA_character_
  ))

# creating a few ETH team specific flags

household_data <- household_data %>%
  mutate(flag_livelihood_beg = ifelse(is.na(`livelihood.beg`), NA,
                                      ifelse(is.na(lcs11_e), NA,
                                             ifelse(`livelihood.beg`== 1  & lcs11_e != "yes", 1,
                                                    ifelse(`livelihood.beg` == 0  & lcs11_e == "yes", 1, 0)))),
         flag_livelihood_loans = ifelse(is.na(`livelihood.loan`), NA,
                                        ifelse(is.na(lcs2_s), NA,
                                               ifelse(`livelihood.loan`== 1  & lcs2_s != "yes", 1,
                                                      ifelse(`livelihood.loan` == 0  & lcs2_s == "yes", 1, 0)))))

# Step 4b: Standardizing the Technical Indicators #####

# Standardizing and Merging the mortality datasets

df_mortality <- format_mortality_current_census(date_recall_event = "2020-11-03",
                                       #Current roster data and columns
                                       df_roster = roster_data2, # your current roster dataset
                                       date_dc_roster = "calc_roster_date_dc", # date of data collection
                                       enum_roster = "calc_roster_enum", # enumerator or team id
                                       cluster_roster = "site", # cluster id
                                       admin2_roster = "Woreda", # optional but recommended admin level
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
                                       cluster_left = "site", # cluster id
                                       admin2_left = "Woreda", #optional but recommended admin level
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
                                       cluster_died = "site", # cluster id
                                       admin2_died = "Woreda", #optional but recommended admin level
                                       hh_id_died = "PARENT_KEY", # unique household id, like the UUID. Must match between datasets
                                       sex_died = "sex_died", # sex of deceased
                                       age_died = "age_died", # age in years of the deceased at time of death
                                       birth_died = "died_born", # born during recall period
                                       joined_died = "died_join", # joined the HH during recall period
                                       death_cause = "cause_of_death", # cause of death
                                       death_location = "death_location",  # location of death
                                       date_death = "calc_death_date_final",
                                       joined_date_died = "died_join_date",
                                       birthdate_died = "died_birthdate")

df_mortality2 <- df_mortality

writexl::write_xlsx(df_mortality, "test_this_3.xlsx")

# create ETH team custom flag_woman_birth - that is a child 0-1 year old in the household, but no woman

(child_nowoman <- df_mortality %>%
  mutate(is_0years = ifelse(is.na(age_years), NA, ifelse(age_years == 0, 1, 0)),
         is_woman = ifelse(is.na(age_years), NA, ifelse(age_years >=15 & age_years <50 & sex == 2, 1 , 0))) %>%
  group_by(hh_id) %>%
  summarize(sum_0years = sum(is_0years, na.rm = TRUE),
            sum_women = sum(is_woman, na.rm = TRUE)) %>%
  mutate(flag_child_nowoman = ifelse(sum_0years == 1 & sum_women == 0, 1, 0),
         sum_0years = NULL, sum_women = NULL))

df_mortality <- merge(df_mortality, child_nowoman, by = "hh_id", all.x = TRUE)

# writexl::write_xlsx(df_mortality, "test_mortality.xlsx")

# Standardizing the Household Food Security Indicators

df_hh <- format_nut_health_indicators(df = household_data,
                                      date_of_dc = "interviewdate",
                                      hhid = "KEY",
                                      file_path = paste0( "data/processed/household_processed_", Sys.Date()),
                                    cluster = "site", enum = "enumerator_id",
                                    hhs_nofoodhh_1 = "hhs_1", hhs_nofoodhh_1a = "hhs_2", hhs_sleephungry_2 = "hhs_3",
                                    hhs_sleephungry_2a = "hhs_4", hhs_alldaynight_3 = "hhs_5", hhs_alldaynight_3a = "hhs_6",
                                    lcs_variables = c("lcs1_s", "lcs2_s", "lcs3_s", "lcs4_s", "lcs5_c", "lcs6_c", "lcs7_c", "lcs8_e", "lcs9_e", "lcs10_e"))

# Standardizing the Anthropometric Child Data

df_child2 <- format_nut_health_indicators(df = child_data %>%
                                            mutate(oedema = NULL,
                                                   muac = ifelse(muac == 99, NA, muac)) %>%
                                            dplyr::select(1:53),
                                    hhid = "hh_id",
                                    id = "individual_id",
                                    date_of_dc = "date_dc",
                                    cluster = "cluster",
                                    enum = "enum",
                                    sex_var = "sex",
                                    age_months_var = "age_months",
                                    muac_var = "muac",
                                    oedema_var = "edema")

df_child2 <- flag_anthro_issues(df = df_child2, file_path = paste0( "data/processed/child_processed_", Sys.Date()))

# Standardizing the WG-SS in the Woman's Data

df_woman2 <- format_nut_health_indicators(df = woman_data %>% mutate(sex = "female"),
                                          file_path = paste0( "data/processed/woman_processed_", Sys.Date()),
                                          hhid = "PARENT_KEY",
                                          id = "KEY",
                                          cluster = "calc_woman_site",
                                          type_interview = "woman_name_note",
                                          sex_var = "sex",
                                          age_years_var = "calc_woman_age_years",
                                          wgss_seeing = "wgq_vision",
                                          wgss_hearing = "wgq_hearing",
                                          wgss_walking = "wgq_mobility",
                                          wgss_remembering = "wgq_conc",
                                          wgss_selfcare = "wgq_selfcare",
                                          wgss_communicating = "wgq_comm") %>%
  mutate(gam_muac = ifelse(is.na(muac_woman_cm), NA, ifelse(as.numeric(muac_woman_cm) < 23, 1, 0)),
         mam_muac = ifelse(is.na(muac_woman_cm), NA, ifelse(as.numeric(muac_woman_cm) >=21 & as.numeric(muac_woman_cm) < 23, 1, 0)),
         sam_muac = ifelse(is.na(muac_woman_cm), NA, ifelse(as.numeric(muac_woman_cm) < 21, 1, 0)))

# Create ETH team Custom flag on if born_alive reported for woman pregancny, but no births reported in the mortality data.

sum_births <- df_mortality %>%
  group_by(hh_id) %>%
  summarise(count_births = sum(!is.na(birth)))

df_woman <- merge(df_woman2, sum_births, by = "hh_id", all.x = TRUE) %>%
  mutate(flag_woman_birth = ifelse(is.na(woman_preg_outcome), NA, ifelse(woman_preg_outcome == "born_alive" & count_births == 0, 1, 0)))

# Step 5: Run Quality Reports for Technical Indicators #####

# Flag Summary Tables

(mortality_flag_summary <- flag_summary_table(df_mortality, grouping = "enum"))
(anthro_flag_summary <- flag_summary_table(df_child, grouping = "enum"))
(hh_flag_summary <- flag_summary_table(df_hh, grouping = "enum"))
(woman_flag_summary <- flag_summary_table(df_woman, grouping = "enum"))

# Checking for missing or other values by group and column

(NAs_table <- summarize_NA_by_group(df_hh, grouping = "enum"))
(dontknows_table <- summarize_value_by_group(household_data, grouping = "enum", val = "dnk" ))

(NAs_table <- summarize_NA_by_group(df_hh, grouping = "date_dc"))
(dontknows_table <- summarize_value_by_group(household_data, grouping = "date_dc", val = "dnk" ))

# summarize number of interviews by date of data collection and enumerator
(df_hh %>% group_by(date_dc, enum)) %>% summarize(n = n())

# Mortality Quality Summary Reports

#13.2% 5-10 years; 14.6% 0-5 years from populationpyramid.net. % children <5 was 15% from our last survey
(mortality_quality_report1 <- create_mortality_quality_report(df_mortality,
                                                             # file_path = paste0("data/quality_reports/mortality_short_", Sys.Date()),
                                                             short_report = FALSE,
                                                             exp_sex_ratio = 1,
                                                             exp_ratio_0_4 = (0.15 / (1 - 0.15)),
                                                             exp_ratio_2_5 = NULL ,
                                                             exp_ratio_5_10 = ((.15) / (.282 - .15)),
                                                             exp_hh_size = 5.4))

#13.2% 5-10 years; 14.6% 0-5 years from populationpyramid.net, % children <5 was 15% from our last survey
(mortality_quality_report2 <- create_mortality_quality_report(df_mortality,
                                                             # file_path = paste0("data/quality_reports/mortality_full_", Sys.Date()),
                                                             short_report = FALSE,
                                                             exp_sex_ratio = 1,
                                                             exp_ratio_0_4 = (0.15 / (1 - 0.15)),
                                                             exp_ratio_2_5 = NULL ,
                                                             exp_ratio_5_10 = ((.15) / (.282 - .15)),
                                                             exp_hh_size = 5.4))

#13.2% 5-10 years; 14.6% 0-5 years from populationpyramid.net. % children <5 was 15% from our last survey
(mortality_quality_report3 <- create_mortality_quality_report(df_mortality,
                                                             # file_path = paste0("data/quality_reports/mortality_enum_short_", Sys.Date()),
                                                             grouping = "enum",
                                                             short_report = TRUE,
                                                             exp_sex_ratio = 1, exp_ratio_0_4 = (0.15 / (1 - 0.15)),
                                                             exp_ratio_2_5 = NULL , exp_ratio_5_10 = ((.15) / (.282 - .15)),
                                                             exp_hh_size = 5.4))

(mortality_quality_report4 <- create_mortality_quality_report(df_mortality %>% filter(enum != 5),
                                                             grouping = "enum",
                                                             short_report = FALSE,
                                                             exp_sex_ratio = 1, exp_ratio_0_4 = (0.15 / (1 - 0.15)),
                                                             exp_ratio_2_5 = NULL , exp_ratio_5_10 = ((.15) / (.282 - .15)), #13.2% 5-10 years; 14.6% 0-5 years from populationpyramid.net. % children <5 was 15% from our last survey
                                                             exp_hh_size = 5.4))

(mortality_quality_report5 <- create_mortality_quality_report(df_mortality,
                                                             file_path = paste0("data/quality_reports/mortality_admin2_short_", Sys.Date()),
                                                             grouping = "admin2",
                                                             short_report = TRUE,
                                                             exp_sex_ratio = 1, exp_ratio_0_4 = (0.15 / (1 - 0.15)),
                                                             exp_ratio_2_5 = NULL , exp_ratio_5_10 = ( (.15) / (.282 - .15)), #13.2% 5-10 years; 14.6% 0-5 years from populationpyramid.net. % children <5 was 15% from our last survey
                                                             exp_hh_size = 5.4))

(mortality_quality_report6 <- create_mortality_quality_report(df_mortality,
                                                             file_path = paste0("data/quality_reports/mortality_admin2_full_", Sys.Date()),
                                                             grouping = "admin2",
                                                             short_report = FALSE,
                                                             exp_sex_ratio = 1, exp_ratio_0_4 = (0.15 / (1 - 0.15)),
                                                             exp_ratio_2_5 = NULL , exp_ratio_5_10 = ( (.15) / (.282 - .15)), #13.2% 5-10 years; 14.6% 0-5 years from populationpyramid.net. % children <5 was 15% from our last survey
                                                             exp_hh_size = 5.4))

(hh_quality_report1 <- create_fsl_quality_report(d, short_report = TRUE, file_path = paste0("data/quality_reports/fsl_short_", Sys.Date())))
(hh_quality_report2 <- create_fsl_quality_report(df_hh, short_report = FALSE, file_path = paste0("data/quality_reports/fsl_full_", Sys.Date())))
(hh_quality_report3 <- create_fsl_quality_report(df_hh, short_report = TRUE, grouping = "enum", file_path = paste0("data/quality_reports/fsl_enum_short_", Sys.Date())))
(hh_quality_report4 <- create_fsl_quality_report(df_hh, short_report = FALSE, grouping = "enum", file_path = paste0("data/quality_reports/fsl_enum_full_", Sys.Date())))
(hh_quality_report5 <- create_fsl_quality_report(df_hh, short_report = TRUE, grouping = "admin2", file_path = paste0("data/quality_reports/fsl_admin2_short_", Sys.Date())))
(hh_quality_report6 <- create_fsl_quality_report(df_hh, short_report = FALSE, grouping = "admin2", file_path = paste0("data/quality_reports/fsl_admin2_full_", Sys.Date())))

(anthro_quality_report <- create_anthro_quality_report(df_child2, short_report = FALSE, file_path = paste0("data/quality_reports/muac_short_", Sys.Date())))
(anthro_quality_report <- create_anthro_quality_report(df_child2, short_report = FALSE, file_path = paste0("data/quality_reports/muac_full_", Sys.Date())))
(anthro_quality_report <- create_anthro_quality_report(df_child2, short_report = TRUE, grouping = "enum", file_path = paste0("data/quality_reports/muac_enum_short_", Sys.Date())))
(anthro_quality_report <- create_anthro_quality_report(df_child2, short_report = FALSE, grouping = "calc_u5_admin2", file_path = paste0("data/quality_reports/muac_enum_full_", Sys.Date())))
(anthro_quality_report <- create_anthro_quality_report(df_child2, short_report = TRUE, grouping = "admin2", file_path = paste0("data/quality_reports/muac_admin2_short_", Sys.Date())))
(anthro_quality_report <- create_anthro_quality_report(df_child2, short_report = FALSE, grouping = "admin2", file_path = paste0("data/quality_reports/muac_admin2_full_", Sys.Date())))

# Step 6: Visualization Checks #####

# Mortality Visualizations

(plot_agepyramid(df_mortality %>% mutate(age_group = as_factor(age_group),
                                         age_group = )))

(plot_agepyramid(df_mortality %>% filter(enum == "5")))
(plot_agepyramid(df_mortality, filtering = "death"))

(plot_age_years_distribution(df_mortality, min_age = 0, max_age = 100))
(plot_age_years_distribution(df_mortality, min_age = 0, max_age = 10))

# Anthropometry Visualizations

(plot_age_months_distribution(df_child2))
(plot_age_months_distribution(df_child, by_group = "enum"))
(plot_age_proxy_distribution(df = df_child, by_group = "enum"))

(plot_zscore_distribution(df = df_child2, index = "mfaz", flags = "yes"))
(plot_zscore_distribution(df = df_child, index = "mfaz", flags = "no", grouping = "enum"))

(plot_anthro_age_distribution(df = df_child2, index = "muac"))
(plot_anthro_age_distribution(df = df_child2, index = "mfaz"))

(plot_cumulative_distribution(df = df_child2, index = "mfaz", flags = "no"))

(plot_cumulative_distribution(df = df_child2 %>%
                                filter(enum != 25 & enum != 31 & enum != 32 & enum != 33 & enum != 34 & enum != 35 & enum != 36 & enum != 5),
                              index = "muac", flags = "yes", grouping = "enum"))

(plot_cumulative_distribution(df = df_child2, index = "mfaz", flags = "no"))
(plot_cumulative_distribution(df = df_child, index = "mfaz", flags = "no", grouping = "enum"))

# WG-SS Visualizations

(plot_agepyramid(df_woman))
(plot_agepyramid(df_woman, filtering = "disability1"))
(plot_agepyramid(df_woman, filtering = "disability2"))
(plot_agepyramid(df_woman, filtering = "disability3"))
(plot_agepyramid(df_woman, filtering = "disability4"))

(plot_age_years_distribution(df_woman, min_age = 15, max_age = 80, breaks = 1))
(plot_age_years_distribution(df_woman, min_age = 0, max_age = 18, breaks = 1))


(g1 <- plot_domain_radar(df = df_woman,
                        domain_cols = c("wg_sum_seeing_34","wg_sum_hearing_34","wg_sum_communication_34","wg_sum_walking_34","wg_sum_selfcare_34","wg_sum_remembering_34"),
                        domain_labels = c("Seeing", "Hearing", "Communication", "Walking", "Self-care", "Remembering")))

(g2 <- plot_domain_radar(df = df_woman,
                        domain_cols = c("wg_sum_seeing_34","wg_sum_hearing_34","wg_sum_communication_34","wg_sum_walking_34","wg_sum_selfcare_34","wg_sum_remembering_34"),
                        domain_labels = c("Seeing", "Hearing", "Communication", "Walking", "Self-care", "Remembering"),
                        grouping = "enum"))

(g3 <- plot_domain_radar(df = df_woman,
                        domain_cols = c("wg_sum_seeing_34","wg_sum_hearing_34","wg_sum_communication_34","wg_sum_walking_34","wg_sum_selfcare_34","wg_sum_remembering_34"),
                        domain_labels = c("Seeing", "Hearing", "Communication", "Walking", "Self-care", "Remembering"),
                        grouping = "admin2"))

(g4 <- plot_domain_radar(df = df_woman,
                        domain_cols = c("wg_sum_seeing_34","wg_sum_hearing_34","wg_sum_communication_34","wg_sum_walking_34","wg_sum_selfcare_34","wg_sum_remembering_34"),
                        domain_labels = c("Seeing", "Hearing", "Communication", "Walking", "Self-care", "Remembering"),
                        grouping = "age_group"))


# Step 7: Cleaning Log Exports #####

# Load working Cleaning and Deletion Log files

household_cl <- readxl::read_xlsx("cleaning_log/ETH_HHS_cleaning_log.xlsx", sheet = "household")
child_cl <- readxl::read_xlsx("cleaning_log/ETH_HHS_cleaning_log.xlsx", sheet = "child")
woman_cl <- readxl::read_xlsx("cleaning_log/ETH_HHS_cleaning_log.xlsx", sheet = "woman")
mortality_cl <- readxl::read_xlsx("cleaning_log/ETH_HHS_cleaning_log.xlsx", sheet = "mortality")

household_dl <- readxl::read_xlsx("cleaning_log/ETH_HHS_cleaning_log.xlsx", sheet = "deletion_household")
child_dl <- readxl::read_xlsx("cleaning_log/ETH_HHS_cleaning_log.xlsx", sheet = "deletion_child")
woman_dl <- readxl::read_xlsx("cleaning_log/ETH_HHS_cleaning_log.xlsx", sheet = "deletion_woman")
mortality_dl <- readxl::read_xlsx("cleaning_log/ETH_HHS_cleaning_log.xlsx", sheet = "deletion_mortality")

# Create Cleaning Log for Outstanding Issues

new_cl_mortality <- create_cleaning_log_flags(df = df_mortality, uuid_col = "id" )
new_cl_child <- create_cleaning_log_flags(df = df_child_clean, uuid_col = "hh_id")
new_cl_hh <- create_cleaning_log_flags(df = df_household_clean, uuid_col = "hh_id")
new_cl_woman <- create_cleaning_log_flags(df = df_woman_clean, uuid_col = "hh_id")

# Implement Existing Deletion Logs

df_hh2 <- implement_deletion_log(df = df_mortality, deletion_log = household_dl, uuid_col = "hh_id")
df_child2 <- implement_deletion_log(df = df_child, deletion_log = child_dl, uuid_col = "KEY")
df_woman2 <- implement_deletion_log(df = df_woman, deletion_log = woman_dl, uuid_col = "KEY")
df_mortality2 <- implement_deletion_log(df = df_mortality, deletion_log = mortality_dl, uuid_col = "id")

df_child2 <- implement_deletion_log(df = df_child2, deletion_log = household_dl, uuid_col = "PARENT_KEY")
df_woman2 <- implement_deletion_log(df = df_woman2, deletion_log = household_dl, uuid_col = "PARENT_KEY")
df_mortality2 <- implement_deletion_log(df = df_mortality2, deletion_log = household_dl, uuid_col = "hh_id")

# Implement Existing Cleaning Log

df_mortality_clean <- implement_cleaning_log(df = df_mortality2, cleaning_log = mortality_cl, uuid_col = "hh_id")
df_child_clean <- implement_cleaning_log(df = df_child2, cleaning_log = child_cl, uuid_col = "KEY")
df_woman_clean <- implement_cleaning_log(df = df_woman2, cleaning_log = woman_cl, uuid_col = "KEY")
df_household_clean <- implement_cleaning_log(df = df_hh2, cleaning_log = household_cl, uuid_col = "id")



# # Harmonize Existing and New Cleaning Log Issues
#
# new_cl_mortality <- append_cleaning_log(mortality_cl, new_cl_mortality)
# new_cl_child <- append_cleaning_log(child_cl, new_cl_child)
# new_cl_hh <- append_cleaning_log(household_cl, new_cl_hh)
# new_cl_woman <- append_cleaning_log(woman_cl, new_cl_woman)

# Export Updated Cleaning Logs

dfs_cl_dl <- list(household = new_cl_hh,
                  child = new_cl_child,
                  woman = new_cl_woman,
                  mortality = new_cl_mortality,
                  deletion_household = household_dl,
                  deletion_child = child_dl,
                  deletion_woman = woman_dl,
                  deletion_mortality = mortality_dl)

writexl::write_xlsx(dfs_cl_dl, paste0("cleaning_log/ETH_HHS_cleaning_log_", Sys.Date()))

# Step 8: Export the Cleaned Data (Excel, and Ena formatted datasets) #####

df_mortality_clean %>% format_mortality_to_ena() %>% write.csv(paste0("data/clean/mortality_ena_", Sys.Date()))
df_child_clean %>% format_anthro_to_ena() %>% write.csv(paste0("data/clean/anthro_ena_", Sys.Date()))

dfs_clean <- list(household = df_household_clean,
                  child = df_child_clean,
                  woman = df_woman_clean,
                  mortality = df_mortality_clean)

writexl::write_xlsx(dfs_clean, paste0("data/clean/cleaned_data_", Sys.Date()))


# Step 9a: Analyze the Data #####
#
# # Create survey weights
#
# sampling_frame <- read.csv("sampling/sampling_Accessible_Areas_24022022 (1).csv")
#
# hh_weights <- weighting_fun_from_samplingframe(data = df_household_clean,
#                                      data.stratum.column = "zone",
#                                      sampling.frame = sampling_frame,
#                                      sampling.frame.population.column = "pop2021",
#                                      sampling.frame.stratum.column = "adm2_cd")
#
# child_weights <- weighting_fun_from_samplingframe(data = df_child_clean,
#                                                   data.stratum.column = "zone",
#                                                   sampling.frame = sampling_frame,
#                                                   sampling.frame.population.column = "pop2021",
#                                                   sampling.frame.stratum.column = "adm2_cd")
#
# woman_weights <- weighting_fun_from_samplingframe(data = df_woman_clean,
#                                                   data.stratum.column = "zone",
#                                                   sampling.frame = sampling_frame,
#                                                   sampling.frame.population.column = "pop2021",
#                                                   sampling.frame.stratum.column = "adm2_cd")
#
# df_household_clean$svy_weights <- hh_weights(df_household_clean)
# df_mortality_clean$svy_weights <- hh_weights(df_mortality_clean)
# df_child_clean$svy_weights <- child_weights(df_child_clean)
# df_woman_clean$svy_weights <- woman_weights(df_woman_clean)

# Step 9b: Export the Aggregated Results #####

#mortality results
(mort1 <- healthyr::analyse_survey_results(df = df_mortality %>% filter(person_time >= 0),
                                 # svy_weights = "svy_weights",
                                 # strata = "zone",
                               sample_design = "two_stage_cluster",
                               cluster = "cluster",
                               ratios_rates.numerators = c("death", "death_under5"),
                               ratios_rates.denominators = c("person_time", "under_5_pt"),
                               ratios_rates.multiplier = 10000))

# (mort2 <- analyse_survey_results(df = df_mortality,
#                                  aggregation = "death_cause",
#                                  sample_design = "two_stage_stratified_cluster",
#                                  cluster = "cluster",
#                                  ratios_rates.numerators = c("death", "death_under5"),
#                                  ratios_rates.denominators = c("person_time", "under_5_pt"),
#                                  ratios_rates.multiplier = 10000))

(mort3 <- analyse_survey_results(df = df_mortality,
                                 aggregation = "sex",
                                 sample_design = "two_stage_stratified_cluster",
                                 cluster = "cluster",
                                 ratios_rates.numerators = c("death", "death_under5"),
                                 ratios_rates.denominators = c("person_time", "under_5_pt"),
                                 ratios_rates.multiplier = 10000))

(mort4 <- analyse_survey_results(df = df_mortality,
                                 aggregation = "age_group",
                                 sample_design = "two_stage_stratified_cluster",
                                 cluster = "cluster",
                                 ratios_rates.numerators = c("death", "death_under5"),
                                 ratios_rates.denominators = c("person_time", "under_5_pt"),
                                 ratios_rates.multiplier = 10000))

(mort5 <- analyse_survey_results(df = df_mortality,
                                 aggregation = "death_location",
                                 sample_design = "two_stage_stratified_cluster",
                                 cluster = "cluster",
                                 ratios_rates.numerators = c("death", "death_under5"),
                                 ratios_rates.denominators = c("person_time", "under_5_pt"),
                                 ratios_rates.multiplier = 10000))

# set the dates for the blockage period here - need to copy and paste this code, and change dates,
# and update list of outputs, for other periods of time.



(mort6 <- analyse_survey_results(df = df_mortality %>%
                                   filter(person_time >= 0) %>%
                                   filter(!is.na(age_group)) %>%
                                   filter(death_cause_smart != "Conflict" & death_cause_smart != "2") %>%
                                   subset_mortality_period(start_date = "2020-11-03", end_date = "2021-01-31"),
                                 sample_design = "two_stage_cluster",
                                 cluster = "cluster",
                                 ratios_rates.numerators = c("death", "death_under5"),
                                 ratios_rates.denominators = c("person_time", "under_5_pt"),
                                 ratios_rates.multiplier = 10000))

(mort6conflict <- analyse_survey_results(df = df_mortality %>%
                                   filter(person_time >= 0) %>%
                                   filter(!is.na(age_group)) %>%
                                   filter(!is.na(sex)) %>%
                                   filter(death_cause != "Illness" & death_cause != "Maternal") %>%
                                   subset_mortality_period(start_date = "2020-11-03", end_date = "2021-01-31"),
                                 # svy_weights = "svy_weights",
                                 # strata = "zone",
                                 sample_design = "two_stage_cluster",
                                 cluster = "cluster",
                                 ratios_rates.numerators = c("death", "death_under5"),
                                 ratios_rates.denominators = c("person_time", "under_5_pt"),
                                 ratios_rates.multiplier = 10000))


(mort7 <- analyse_survey_results(df = df_mortality_clean,
                                 svy_weights = "svy_weights",
                                 strata = "zone",
                                 aggregation = "woreda",
                                 sample_design = "two_stage_stratified_cluster",
                                 cluster = "cluster",
                                 ratios_rates.numerators = c("death", "death_under5"),
                                 ratios_rates.denominators = c("person_time", "under_5_pt"),
                                 ratios_rates.multiplier = 10000))

dfs_mort <- list(overall = mort1,
                 by_woreda = mort7,
                 by_cause = mort2,
                 by_sex = mort3,
                 by_age = mort4,
                 by_location = mort5,
                 by_period = mort6)

writexl::write_xlsx(dfs_mort, paste0("results/mortality_results_", Sys.Date() ))

# Household data

(hh1 <- analyse_survey_results(df = df_household_clean,
                                 svy_weights = "svy_weights",
                                 strata = "zone",
                                 sample_design = "two_stage_stratified_cluster",
                                 cluster = "cluster",
                                 proportions = c("Woreda", "sex_respondent", "sex_hoh", "residency_status",
                                                 # livelihood indicators
                                                 "livelihood.salary", "livelihood.sell_agri_prod",
                                                 "livelihood.sell_anim_prod","livelihood.sell_collected", "livelihood.trader","livelihood.daily_labour_ag",
                                                 "livelihood.daily_labour_skilled", "livelihood.daily_labour_non","livelihood.saving","livelihood.pension",
                                                 "livelihood.remitance","livelihood.gifts_donations","livelihood.loan",
                                                 "livelihood.hum_assistance","livelihood.beg","livelihood.other", "livelihood.dnk",
                                                 # health impairment
                                                 "hh_physical_impair", "hh_mental_impair", "hh_mental_impair_support.spiritual", "hh_mental_impair_support.health_post",
                                                 "hh_mental_impair_support.mobile_health", "hh_mental_impair_support.specialized","hh_mental_impair_support.none",
                                                 "hh_mental_impair_support.dnk",
                                                 # hhs
                                                 "hhs_nofoodhh_1","hhs_sleephungry_2","hhs_alldaynight_3", "hhs_cat",
                                                 # LCS
                                                 "lcs1_s","lcs2_s","lcs3_s","lcs4_s","lcs5_c","lcs6_c","lcs7_c","lcs8_e","lcs9_e","lcs10_e","lcs11_e","lcs12_e", "lcs_cat",
                                                 # WASH
                                                 "water_source", "water_source_rainy", "water_source_change", "water_sufficient.drinking","water_sufficient.cooking",
                                                 "water_sufficient.hygiene","water_sufficient.domestic","water_sufficient.none", "water_sufficient.dnk","water_collect_time",
                                                 "water_treatment.none","water_treatment.boil","water_treatment.chlorine", "water_treatment.filter_cloth","water_treatment.other",
                                                 "water_treatment.dnk", "soap_access", "latrine_access",
                                                 # mortality
                                                 "death_any", "death_child"),
                                 means = c("age_respondent", "age_hoh", "calc_numfamily", "calc_num_children_under5", "calc_numwomen", "calc_numdependents", "calc_numelderly",
                                           "hhs_nofoodhh_1a","hhs_sleephungry_2a", "hhs_alldaynight_3a", "hhs_score", "num_left", "num_died")) )

(hh2 <- analyse_survey_results(df = df_household_clean,
                               aggregation = "Woreda",
                               svy_weights = "svy_weights",
                               strata = "zone",
                               sample_design = "two_stage_stratified_cluster",
                               cluster = "cluster",
                               proportions = c("Woreda", "sex_respondent", "sex_hoh", "residency_status",
                                               # livelihood indicators
                                               "livelihood.salary", "livelihood.sell_agri_prod",
                                               "livelihood.sell_anim_prod","livelihood.sell_collected", "livelihood.trader","livelihood.daily_labour_ag",
                                               "livelihood.daily_labour_skilled", "livelihood.daily_labour_non","livelihood.saving","livelihood.pension",
                                               "livelihood.remitance","livelihood.gifts_donations","livelihood.loan",
                                               "livelihood.hum_assistance","livelihood.beg","livelihood.other", "livelihood.dnk",
                                               # health impairment
                                               "hh_physical_impair", "hh_mental_impair", "hh_mental_impair_support.spiritual", "hh_mental_impair_support.health_post",
                                               "hh_mental_impair_support.mobile_health", "hh_mental_impair_support.specialized","hh_mental_impair_support.none",
                                               "hh_mental_impair_support.dnk",
                                               # hhs
                                               "hhs_nofoodhh_1","hhs_sleephungry_2","hhs_alldaynight_3", "hhs_cat",
                                               # LCS
                                               "lcs1_s","lcs2_s","lcs3_s","lcs4_s","lcs5_c","lcs6_c","lcs7_c","lcs8_e","lcs9_e","lcs10_e","lcs11_e","lcs12_e", "lcs_cat",
                                               # WASH
                                               "water_source", "water_source_rainy", "water_source_change", "water_sufficient.drinking","water_sufficient.cooking",
                                               "water_sufficient.hygiene","water_sufficient.domestic","water_sufficient.none", "water_sufficient.dnk","water_collect_time",
                                               "water_treatment.none","water_treatment.boil","water_treatment.chlorine", "water_treatment.filter_cloth","water_treatment.other",
                                               "water_treatment.dnk", "soap_access", "latrine_access",
                                               # mortality
                                               "death_any", "death_child"),
                               means = c("age_respondent", "age_hoh", "calc_numfamily", "calc_num_children_under5", "calc_numwomen", "calc_numdependents", "calc_numelderly",
                                         "hhs_nofoodhh_1a","hhs_sleephungry_2a", "hhs_alldaynight_3a", "hhs_score", "num_left", "num_died")) )

dfs_hh <- list(overall = hh1, by_woreda = hh2)

writexl::write_xlsx(dfs_hh, paste0("results/household_results_", Sys.Date()))

# Child data

(child1 <- analyse_survey_results(df = df_child2,
                               # svy_weights = "svy_weights",
                               # strata = "zone",
                               sample_design = "two_stage_cluster",
                               cluster = "cluster",
                               proportions = c(
                                               # anthropometric results
                                               "gam_muac_noflag", "mam_muac_noflag", "sam_muac_noflag", "global_mfaz_noflag",
                                               "moderate_mfaz_noflag", "severe_mfaz_noflag", "oedema",
                                               # health results
                                               "child_nut_supp", "illness2weeks", "ill_symptom.fever", "ill_symptom.diarrhoea","ill_symptom.cough",
                                               "ill_symptom.conjunctivitis","ill_symptom.skin_infection","ill_symptom.ear_infection","ill_symptom.fatigue","ill_symptom.other",
                                               "ill_symptom.dnk", "diarrhoea_loosestools","diarrhoea_dehydration","ari_fastdifficult","ari_chestproblem","treatmentsought",
                                               "notreatment_reason", "calc_ill_fever","calc_ill_diarrhoea","calc_ill_cough","calc_ill_pneumonia","measles"
                                               ),
                               means = c("muac_noflag", "mfaz_noflag")) )

(child2 <- analyse_survey_results(df = df_child_clean,
                                  aggregation = "Woreda",
                                  svy_weights = "svy_weights",
                                  # strata = "zone",
                                  sample_design = "two_stage_cluster",
                                  cluster = "cluster",
                                  proportions = c(
                                    # anthropometric results
                                    "gam_muac_noflag", "mam_muac_noflag", "sam_muac_noflag", "gam_mfaz_noflag",
                                    "mam_mfaz_noflag", "sam_mfaz_noflag", "edema_confirm",
                                    # health results
                                    "child_nut_supp", "illness2weeks", "ill_symptom.fever", "ill_symptom.diarrhoea","ill_symptom.cough",
                                    "ill_symptom.conjunctivitis","ill_symptom.skin_infection","ill_symptom.ear_infection","ill_symptom.fatigue","ill_symptom.other",
                                    "ill_symptom.dnk", "diarrhoea_loosestools","diarrhoea_dehydration","ari_fastdifficult","ari_chestproblem","treatmentsought",
                                    "notreatment_reason", "calc_ill_fever","calc_ill_diarrhoea","calc_ill_cough","calc_ill_pneumonia","measles"
                                  ),
                                  means = c("muac_noflag", "mfaz_noflag" )) )

dfs_child <- list(overall = child1,
                  by_woreda = child2)

writexl::write_xlsx(dfs_child, paste0("results/child_results_", Sys.Date()))

# Woman data

(woman1 <- analyse_survey_results(df = df_woman2,
                                  # svy_weights = "svy_weights",
                                  # strata = "zone",
                                  sample_design = "two_stage_cluster",
                                  cluster = "cluster",
                                  proportions = c("plw_status",
                                                  # Washington Group results
                                                  "disability1", "disability2", "disability3", "disability4", "wg_sum_seeing_34", "wg_sum_hearing_34",
                                                  "wg_sum_communication_34","wg_sum_walking_34","wg_sum_selfcare_34","wg_sum_remembering_34",
                                                  "wgss_type_interview", "wgss1_seeing","wgss2_hearing","wgss3_walking", "wgss4_remembering","wgss5_selfcare","wgss6_communicating",
                                                  # malnutrition results
                                                  "gam_muac", "mam_muac", "sam_muac",
                                                  # reproductive health indicators
                                                  "woman_preg_recall","woman_preg_outcome","plw_birthplace","plw_assisted", "plw_status_childage",
                                                  "plw_tsfp_prev","plw_tsfp_sharing", "reprod_unmet.family_planning","reprod_unmet.anc","reprod_unmet.pnc",
                                                  "reprod_unmet.post_abort","reprod_unmet.post_gbv","reprod_unmet.infect", "reprod_unmet.supplements","reprod_unmet.general",
                                                  "reprod_unmet.other","reprod_unmet.none","reprod_unmet.dnk"),
                                  means = c("age_years", "muac_woman_cm", "wgss_sco_score", "wgss_hd_score")))


(dim(df_woman2))

b <- as.vector(df_woman %>% dplyr::select(age_years) %>% t %>% c %>% unique)

all(varhandle::check.numeric(b))


(woman2 <- analyse_survey_results(df = df_woman_clean,
                                  aggregation = "age_group",
                                  svy_weights = "svy_weights",
                                  strata = "zone",
                                  sample_design = "two_stage_stratified_cluster",
                                  cluster = "cluster",
                                  proportions = c("plw_status",
                                                  # Washington Group results
                                                  "disability1", "disability2", "disability3", "disability4", "wg_sum_seeing_34", "wg_sum_hearing_34",
                                                  "wg_sum_communication_34","wg_sum_walking_34","wg_sum_selfcare_34","wg_sum_remembering_34",
                                                  "wgss_type_interview", "wgss1_seeing","wgss2_hearing","wgss3_walking", "wgss4_remembering","wgss5_selfcare","wgss6_communicating",
                                                  # malnutrition results
                                                  "gam_muac", "mam_muac", "sam_muac",
                                                  # reproductive health indicators
                                                  "woman_preg_recall","woman_preg_outcome","plw_birthplace","plw_assisted", "plw_status_childage.0_6months",
                                                  "plw_status_childage.6_23months", "plw_status_childage.24_plus", "plw_status_childage.dnk",
                                                  "plw_tsfp_prev","plw_tsfp_sharing", "reprod_unmet.family_planning","reprod_unmet.anc","reprod_unmet.pnc",
                                                  "reprod_unmet.post_abort","reprod_unmet.post_gbv","reprod_unmet.infect", "reprod_unmet.supplements","reprod_unmet.general",
                                                  "reprod_unmet.other","reprod_unmet.none","reprod_unmet.dnk"),
                                  means = c("age_years", "muac_woman_cm", "wgss_sco_score", "wgss_hd_score")) )

(woman3 <- analyse_survey_results(df = df_woman_clean,
                                  aggregation = "plw_status",
                                  svy_weights = "svy_weights",
                                  strata = "zone",
                                  sample_design = "two_stage_stratified_cluster",
                                  cluster = "cluster",
                                  proportions = c("plw_status",
                                                  # Washington Group results
                                                  "disability1", "disability2", "disability3", "disability4", "wg_sum_seeing_34", "wg_sum_hearing_34",
                                                  "wg_sum_communication_34","wg_sum_walking_34","wg_sum_selfcare_34","wg_sum_remembering_34",
                                                  "wgss_type_interview", "wgss1_seeing","wgss2_hearing","wgss3_walking", "wgss4_remembering","wgss5_selfcare","wgss6_communicating",
                                                  # malnutrition results
                                                  "gam_muac", "mam_muac", "sam_muac",
                                                  # reproductive health indicators
                                                  "woman_preg_recall","woman_preg_outcome","plw_birthplace","plw_assisted", "plw_status_childage.0_6months",
                                                  "plw_status_childage.6_23months", "plw_status_childage.24_plus", "plw_status_childage.dnk",
                                                  "plw_tsfp_prev","plw_tsfp_sharing", "reprod_unmet.family_planning","reprod_unmet.anc","reprod_unmet.pnc",
                                                  "reprod_unmet.post_abort","reprod_unmet.post_gbv","reprod_unmet.infect", "reprod_unmet.supplements","reprod_unmet.general",
                                                  "reprod_unmet.other","reprod_unmet.none","reprod_unmet.dnk"),
                                  means = c("age_years", "muac_woman_cm", "wgss_sco_score", "wgss_hd_score")) )

(woman4 <- analyse_survey_results(df = df_woman_clean,
                                  aggregation = "disability3",
                                  svy_weights = "svy_weights",
                                  strata = "zone",
                                  sample_design = "two_stage_stratified_cluster",
                                  cluster = "cluster",
                                  proportions = c("plw_status",
                                                  # malnutrition results
                                                  "gam_muac", "mam_muac", "sam_muac",
                                                  # reproductive health indicators
                                                  "woman_preg_recall","woman_preg_outcome","plw_birthplace","plw_assisted", "plw_status_childage.0_6months",
                                                  "plw_status_childage.6_23months", "plw_status_childage.24_plus", "plw_status_childage.dnk",
                                                  "plw_tsfp_prev","plw_tsfp_sharing", "reprod_unmet.family_planning","reprod_unmet.anc","reprod_unmet.pnc",
                                                  "reprod_unmet.post_abort","reprod_unmet.post_gbv","reprod_unmet.infect", "reprod_unmet.supplements","reprod_unmet.general",
                                                  "reprod_unmet.other","reprod_unmet.none","reprod_unmet.dnk"),
                                  means = c("age_years", "muac_woman_cm", "wgss_sco_score", "wgss_hd_score")) )

(woman5 <- analyse_survey_results(df = df_woman_clean,
                                  aggregation = "calc_woman_admin2",
                                  svy_weights = "svy_weights",
                                  strata = "zone",
                                  sample_design = "two_stage_stratified_cluster",
                                  cluster = "cluster",
                                  proportions = c("plw_status",
                                                  # Washington Group results
                                                  "disability1", "disability2", "disability3", "disability4", "wg_sum_seeing_34", "wg_sum_hearing_34",
                                                  "wg_sum_communication_34","wg_sum_walking_34","wg_sum_selfcare_34","wg_sum_remembering_34",
                                                  "wgss_type_interview", "wgss1_seeing","wgss2_hearing","wgss3_walking", "wgss4_remembering","wgss5_selfcare","wgss6_communicating",
                                                  # malnutrition results
                                                  "gam_muac", "mam_muac", "sam_muac",
                                                  # reproductive health indicators
                                                  "woman_preg_recall","woman_preg_outcome","plw_birthplace","plw_assisted", "plw_status_childage.0_6months",
                                                  "plw_status_childage.6_23months", "plw_status_childage.24_plus", "plw_status_childage.dnk",
                                                  "plw_tsfp_prev","plw_tsfp_sharing", "reprod_unmet.family_planning","reprod_unmet.anc","reprod_unmet.pnc",
                                                  "reprod_unmet.post_abort","reprod_unmet.post_gbv","reprod_unmet.infect", "reprod_unmet.supplements","reprod_unmet.general",
                                                  "reprod_unmet.other","reprod_unmet.none","reprod_unmet.dnk"),
                                  means = c("age_years", "muac_woman_cm", "wgss_sco_score", "wgss_hd_score")) )

# disaggregate by plw status, by disability status, by age

dfs_woman <- list(overall = woman1,
                  by_woreda = woman5,
                  by_age = woman2,
                  by_plw = woman3,
                  by_disability = woman4)

writexl::write_xlsx(dfs_woman, paste0("results/woman_results_", Sys.Date()))

# Step 10: Statistical Checks and Exporting #####

(hh_tests1 <- run_statistical_tests(df = df_household_clean,
                                    aggregation = "sex_hoh",
                                    svy_weights = "svy_weights",
                                    strata = "zone",
                                    sample_design = "two_stage_stratified_cluster",
                                    cluster = "cluster",
                                    categorical_vars = c("residency_status",
                                                         "livelihood.salary","livelihood.sell_agri_prod",
                                                         "livelihood.sell_anim_prod","livelihood.sell_collected",
                                                         "livelihood.trader","livelihood.daily_labour_ag",
                                                         "livelihood.daily_labour_skilled","livelihood.daily_labour_non",
                                                         "livelihood.saving","livelihood.pension","livelihood.remitance",
                                                         "livelihood.gifts_donations","livelihood.loan","livelihood.hum_assistance",
                                                         "livelihood.beg","livelihood.other","livelihood.dnk",
                                                         "hhs_cat"),
                                    numerical_vars = c("hhs_score")))

# Houseohld results hypothesis tests

# Child results hypothesis tests

# Women results hypothesis tests
(women_tests1 <- run_statistical_tests(df = df_woman_clean,
                                      aggregation = "plw_status",
                                      svy_weights = "svy_weights",
                                      strata = "zone",
                                      sample_design = "two_stage_stratified_cluster",
                                      cluster = "cluster",
                                      chisquared_tests = c("disability3",
                                                           "gam_muac", "mam_muac", "sam_muac",
                                                           "woman_preg_recall","woman_preg_outcome","plw_birthplace","plw_assisted",
                                                           "plw_tsfp_prev","plw_tsfp_sharing"),
                                      t_tests = c("muac_plw_cm")))

(women_tests2 <- run_statistical_tests(df = testwomen,
                                       aggregation = "disability3",
                                       svy_weights = "svy_weights",
                                       # strata = "zone",
                                       sample_design = "two_stage_cluster",
                                       cluster = "cluster",
                                       chisquared_tests = c("plw_status",
                                                            "gam_muac", "mam_muac", "sam_muac",
                                                            "woman_preg_recall","woman_preg_outcome","plw_birthplace","plw_assisted",
                                                            "plw_tsfp_prev","plw_tsfp_sharing")))








test_data <- proc_anthro1

test_survey <- srvyr::as_survey_design(.data = test_data, ids = cluster)

frml <- as.formula(paste0("sex", "~", "wfhz_noflag"))

(survey::svyttest(formula = wfhz_noflag ~ sex, design = test_survey)[1])

(testing <- run_statistical_tests(df = test_data, aggregation = "sex",
                                 cluster = "cluster", sample_design = "two_stage_cluster",
                                 chisquared_tests = c("gam_wfhz_noflag", "sam_wfhz_noflag",
                                                      "c_gam", "c_sam", "gam_muac_noflag"),
                                 t_tests = c("wfhz_noflag", "mfaz_noflag", "muac_noflag", "hfaz_noflag")))



# survey::svyttest(formula = frml, design = test_data)




