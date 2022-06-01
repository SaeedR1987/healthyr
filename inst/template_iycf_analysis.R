# TEMPLATE HEALHTYR ANALYSIS DOCUMENT for Infant and Young Child Feeding (IYCF v2021) Data
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

df <- raw_anthro_iycf2

# Step 2: Format Your Dataset ####

df2 <- format_nut_health_indicators(df = df, use_flags_yn = "yes",

                                    hhid = "household_id",
                                    date_of_dc = "date_of_interview",
                                    enum = "team_number",
                                    cluster = "cluster_id",

                                    date_of_birth = "date_of_birth_of_name",
                                    age_months_var = "interview_section_n1_j_childage",
                                    sex_var = "sex_of_name",

                                    iycf_1 = "ever_breastfed", # ever breastfed (y/n)
                                    iycf_2 = "first_put_to_breast", # how long after birth breastfed
                                    iycf_3 = "breastmilk_first_two_days", # given anything to eat/drink in first 2 days after delivery
                                    iycf_4 = "breastfed_yesterday", # breastfed yesterday during the day or night (y/n)
                                    iycf_5 = "bottle_with_nipple", # drink anything from bottle with a nipple (y/n)
                                    iycf_6a = "plain_water", # plain water
                                    iycf_6b = "infant_formula", # infant formula (y/n)
                                    iycf_6b_num = "number_infant_formula", # infant formula (number)
                                    iycf_6c = "animal_tinned_or_powdered_milk", # milk from animals, fresh tinned powder (y/n)
                                    iycf_6c_num = "number_drink_milk", # milk form animals, fresh, tinned, pwder (number)
                                    #iycf_6c_swt = "sweet_or_flavoured_type_of_milk", # milk was sweetened (y/n)
                                    iycf_6d = "yogurt_drinks", # yoghurt drinks (y/n)
                                    iycf_6d_num = "number_yoghurt_drinks", # yoghurt drinks (number)
                                    iycf_6d_swt = "sweet_or_flavoured_type_of_yogurt_drink", # yoghurt drink was sweetened (y/n)
                                    iycf_6e = "chocolate_flavoured_drinks", # chocolate flavoured drinks, including from syrup / powders (y/n)
                                    iycf_6f = "fruit_juice", # Fruit juice or fruit-flavoured drinks including those made from syrups or powders? (y/n)
                                    iycf_6g = "sodas_malt_drinks", # sodas, malt drinks, sports and energy drinks (y/n)
                                    iycf_6h = "tea_coffee", # tea, coffee, herbal drinks (y/n)
                                    #iycf_6h_swt = "any_of_these_otherliquids_sweetened", # tea coffee herbal drinks were sweetened (y/n)
                                    iycf_6i = "clear_broth", # clear broth / soup (y/n)
                                    iycf_6j = "any_other_liquids", # other liquids (y/n)
                                    #iycf_6j_swt = "any_of_these_otherliquids_sweetened", # other drinks were sweetened (y/n)
                                    iycf_7a = "yoghurt_other_than_yogurt_drinks", # yoghurt (NOT yoghurt drinks) (y/n)
                                    iycf_7a_num = "number_food_yoghurt", # yoghurt (NOT yoghurt drinks) (number)
                                    iycf_7b = "porridge_bread_rice_noodles_pasta_asida_and_kisra", # porridge, bread, rice, nooodles (y/n)
                                    iycf_7c = "pumpkin_carrots_sweet_red_peppers_squash_sweet_potatoes", # vitamin a rich vegetables (pumpkin, carrots, sweet red peppers, squash or yellow/orange sweet potatoes) (y/n)
                                    iycf_7d = "plantains_white_potatoes_white_yams_manioc_cassava", # white starches (plaintains, white potatoes, white yams, manioc, cassava) (y/n)
                                    iycf_7e = "dark_green_leafy_vegetables", # dark green leafy vegetables (y/n)
                                    iycf_7f = "any_other_vegetables", # other vegetables (y/n)
                                    iycf_7g = "ripe_mangoes_or_ripe_papayas", # vitamin a rich fruits (ripe mangoes, ripe papayas) (y/n)
                                    iycf_7h = "any_other_fruits", # other fruits (y/n)
                                    iycf_7i = "liver_kidney_heart", # organ meats (liver ,kidney, heart) (y/n)
                                    iycf_7j = "sausages_hot_dogs_frankfurters_ham_bacon_salami_canned_meat", # processed meats (sausages, hot dogs, ham, bacon, salami, canned meat) (y/n)
                                    iycf_7k = "any_other_meat_beef_pork_lamb_goat_chicken_duck", # any other meats (beef, chicken, pork, goat, chicken, duck) (y/n)
                                    iycf_7l = "eggs", # eggs (y/n)
                                    iycf_7m = "fish_or_shellfish", # fish (fresh or dried fish or shellfish) (y/n)
                                    iycf_7n = "beans_peas_lentils_nuts_seeds", # legumes (beans, peas, lentils, seeds, chickpeas) (y/n)
                                    iycf_7o = "hard_or_soft_cheese", # cheeses (hard or soft cheeses) (y/n)
                                    iycf_7p = "sweet_foods_chocolates_candies_pastries_cakes_biscuit_ice_cream", # sweets (chocolates, candies, pastries, cakes) (y.n)
                                    iycf_7q = "chips_crisps_puffs_french_fries_fried_dough_instant_noodles", # fried or empty carbs (chips, crisps, french fries, fried dough, instant noodles) (y/n)
                                    iycf_7r = "any_other_food", # Any other solid, semi-solid, or soft foods
                                    #iycf_7s = "any_other_solid_semi_solid_or_soft_food", # did child eat solid/semi-solid foods (y/n) for list based questionnaires
                                    iycf_8 = "number_solid_semi_solid_soft_foods_yesterday", # times child ate solid/semi-solid foods (number)
)

# Step 3: Review a Quality Summary Report ####
# Ratio is (prevalence / (1 - prevalence))

(create_iycf_quality_report(df = df2,
                            short_report = FALSE,
                            exp_prevalence_mad = 0.05,
                            exp_sex_ratio = 1,
                            exp_ratio_under6m_6to23m = 0.3))

(create_iycf_quality_report(df = df2, grouping = "enum", short_report = TRUE, exp_prevalence_mad = 0.05, exp_sex_ratio = 1, exp_ratio_under6m_6to23m = 0.3))

(create_iycf_quality_report(df = df2, grouping = "enum", short_report = FALSE, exp_prevalence_mad = 0.05, exp_sex_ratio = 1, exp_ratio_under6m_6to23m = 0.3))

(create_iycf_quality_report(df = df2, grouping = "county_admin2", short_report = FALSE, exp_prevalence_mad = 0.05, exp_sex_ratio = 1, exp_ratio_under6m_6to23m = 0.3))

# Step 4: Evaluate Data with Visualizations ####

(plot_age_months_distribution(df2))

(plot_age_months_distribution(df2, by_group = "enum"))

(plot_age_months_distribution(df2, by_group = "county_admin2"))

(g <- plot_iycf_areagraph(df2))

(g <- plot_iycf_areagraph(df2 %>% filter(enum == 1)))

# Step 5: Export Flagged Records to Cleaning Log + Cleaning ####

(flag_summary <- flag_summary_table(df = df2, grouping = "enum"))

cl <- create_cleaning_log_flags(df = df2, uuid_col = "hh_id")
View(cl)

# Step 6: Analyse Survey Results ####

(res <- analyse_survey_results(df = df2,

                               aggregation = "county_admin2",

                                  sample_design = "two_stage_stratified_cluster",
                               strata = "county_admin2",
                                  cluster = "cluster",

                                  proportions = c("iycf_evbf","iycf_eibf", "iycf_ebf2d", "iycf_ebf", "iycf_mixmf",
                                                  "iycf_cbf", "iycf_isssf", "mmf_bf_6to8months", "mmf_bf_9to23months",
                                                  "iycf_mdd_cat", "mmf_nonbf_6to23months", "iycf_mmf", "iycf_mmff",
                                                  "iycf_mad", "iycf_eff", "iycf_ufc","iycf_zvf", "iycf_bof"),

                                  means = c("iycf_mdd_score")))









