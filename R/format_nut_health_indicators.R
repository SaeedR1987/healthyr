
#' Format Nutrition and Health Indicators
#'
#' This function allows for standardization of variable names and values for technical nutrition and health indicators. The function additionally
#' calls helper functions to reformat, calculate indicators, and create data quality flags for each of the sets of indicators.
#'
#' @param df Inputs a dataframe with nutrition and health data of interest.
#' @param file_path Optional input of a character file path. If included will save an excel sheet of the new dataframe.
#' @param use_flags_yn Optional input if you want to exclude flagged values from the final indicator calculations. If "yes"
#' then records with data quality flags will be treated differently in the calculations.
#' @param hhid Input of a character value specifying the column name for household id. Can use the uuid here as well.
#' @param date_of_dc Input of a character value specifying the column name for date of data collection.
#' @param enum Input of a character value specifying the column name for enumerator or team id.
#' @param cluster Input of a character value specifying the column name for cluster number or id.
#' @param id Input of a character value specifying the column name for an individual id within the household.
#' @param date_of_birth Input of a character value specifying the column name for date of birth
#' @param age_years_var Input of a character value specifying the column name for age in years of the individual
#' @param age_months_var Input of a character value specifying the column name for age in months of the individual
#' @param age_proxy_var Input of a character value specifying the column name for proxy age of children. That is, whether
#' the child is 6-23 months of age, or 24-59 months of age. For use with some MUAC only related data collection.
#' @param age_days Input of a character value specifying the column name for age in days of children under-5 years of age
#' @param sex_var Input of a character value specifying the column name for sex of the individual.
#' @param oedema_var Input of a character value specifying the column name for nutritional oedema
#' @param plw_status_var Input of a character value specifying the column name for pregnant and/or breastfeeding status for women of reproductive age
#' @param muac_var Input of a character value specifying the column name for MUAC measurements in mm or cm
#' @param weight_var Input of a character value specifying the column name for weight measurements in kilograms
#' @param height_var Input of a character value specifying the column name for length and/or height measurements in cm
#' @param type_interview Input of a character value specifying the column name for direct or proxy interviews for Washington Group questions
#' @param wgss_seeing Input of a character value specifying the column name for seeing domain of Washington Group Short Set of questions
#' @param wgss_hearing Input of a character value specifying the column name for hearing domain of Washington Group Short Set of questions
#' @param wgss_walking Input of a character value specifying the column name for walking domain of Washington Group Short Set of questions
#' @param wgss_remembering Input of a character value specifying the column name for remembering domain of Washington Group Short Set of questions
#' @param wgss_selfcare Input of a character value specifying the column name for self-care domain of Washington Group Short Set of questions
#' @param wgss_communicating Input of a character value specifying the column name for communicating domain of Washington Group Short Set of questions
#' @param delivery_location Input of a character value specifying the column name for child delivery location
#' @param birth_assistant Input of a character value specifying the column name for who attended the woman's birth
#' @param iycf_caregiver Input of a character value specifying the column name for if the primary caregiver or mother of the child was interviewed for the IYCF questions (y/n)
#' @param iycf_1 Input of a character value specifying the column name for ever breastfed (y.n)
#' @param iycf_2 Input of a character value specifying the column name for how long after birth breastfed
#' @param iycf_3 Input of a character value specifying the column name for given anything to eat/drink in first 2 days after delivery
#' @param iycf_4 Input of a character value specifying the column name for breastfed yesterday during the day or night (y/n)
#' @param iycf_5 Input of a character value specifying the column name for drink anything from bottle with a nipple (y/n)
#' @param iycf_6a Input of a character value specifying the column name for plain water
#' @param iycf_6b Input of a character value specifying the column name for infant formula (y/n)
#' @param iycf_6b_num Input of a character value specifying the column name for infant formula (number)
#' @param iycf_6c Input of a character value specifying the column name for milk from animals, fresh tinned powder (y/n)
#' @param iycf_6c_num Input of a character value specifying the column name for milk form animals, fresh, tinned, pwder (number)
#' @param iycf_6c_swt Input of a character value specifying the column name for milk was sweetened (y/n)
#' @param iycf_6d Input of a character value specifying the column name for yoghurt drinks (y/n)
#' @param iycf_6d_num Input of a character value specifying the column name for yoghurt drinks (number)
#' @param iycf_6d_swt Input of a character value specifying the column name for yoghurt drink was sweetened (y/n)
#' @param iycf_6e Input of a character value specifying the column name for chocolate flavoured drinks, including from syrup / powders (y/n)
#' @param iycf_6f Input of a character value specifying the column name for Fruit juice or fruit-flavoured drinks including those made from syrups or powders? (y/n)
#' @param iycf_6g Input of a character value specifying the column name for sodas, malt drinks, sports and energy drinks (y/n)
#' @param iycf_6h Input of a character value specifying the column name for tea, coffee, herbal drinks (y/n)
#' @param iycf_6h_swt Input of a character value specifying the column name for tea coffee herbal drinks were sweetened (y/n)
#' @param iycf_6i Input of a character value specifying the column name for clear broth / soup (y/n)
#' @param iycf_6j Input of a character value specifying the column name for other liquids (y/n)
#' @param iycf_6j_swt Input of a character value specifying the column name for other drinks were sweetened (y/n)
#' @param iycf_7a Input of a character value specifying the column name for yoghurt (NOT yoghurt drinks) (y/n)
#' @param iycf_7a_num Input of a character value specifying the column name for yoghurt (NOT yoghurt drinks) (number)
#' @param iycf_7b Input of a character value specifying the column name for porridge, bread, rice, nooodles (y/n)
#' @param iycf_7c Input of a character value specifying the column name for vitamin a rich vegetables (pumpkin, carrots, sweet red peppers, squash or yellow/orange sweet potatoes) (y/n)
#' @param iycf_7d Input of a character value specifying the column name for white starches (plaintains, white potatoes, white yams, manioc, cassava) (y/n)
#' @param iycf_7e Input of a character value specifying the column name for dark green leafy vegetables (y/n)
#' @param iycf_7f Input of a character value specifying the column name for other vegetables (y/n)
#' @param iycf_7g Input of a character value specifying the column name for vitamin a rich fruits (ripe mangoes, ripe papayas) (y/n)
#' @param iycf_7h Input of a character value specifying the column name for other fruits (y/n)
#' @param iycf_7i Input of a character value specifying the column name for organ meats (liver ,kidney, heart) (y/n)
#' @param iycf_7j Input of a character value specifying the column name for processed meats (sausages, hot dogs, ham, bacon, salami, canned meat) (y/n)
#' @param iycf_7k Input of a character value specifying the column name for any other meats (beef, chicken, pork, goat, chicken, duck) (y/n)
#' @param iycf_7l Input of a character value specifying the column name for eggs (y/n)
#' @param iycf_7m Input of a character value specifying the column name for fish (fresh or dried fish or shellfish) (y/n)
#' @param iycf_7n Input of a character value specifying the column name for legumes (beans, peas, lentils, seeds, chickpeas) (y/n)
#' @param iycf_7o Input of a character value specifying the column name for cheeses (hard or soft cheeses) (y/n)
#' @param iycf_7p Input of a character value specifying the column name for sweets (chocolates, candies, pastries, cakes) (y.n)
#' @param iycf_7q Input of a character value specifying the column name for fried or empty carbs (chips, crisps, french fries, fried dough, instant noodles) (y/n)
#' @param iycf_7r Input of a character value specifying the column name for Any other solid, semi-solid, or soft foods
#' @param iycf_7s Input of a character value specifying the column name for did child eat solid/semi-solid foods (y/n) for list based questionnaires
#' @param iycf_8 Input of a character value specifying the column name for times child ate solid/semi-solid foods (number)
#' @param fcs_cereal Input of a character value specifying the column name for number of days of cereals food consumption in the household
#' @param fcs_legumes Input of a character value specifying the column name for number of days of legumes food consumption in the household
#' @param fcs_dairy Input of a character value specifying the column name for number of days of dairy food consumption in the household
#' @param fcs_meat Input of a character value specifying the column name for number of days of meats food consumption in the household
#' @param fcs_veg Input of a character value specifying the column name for number of days of vegetables food consumption in the household
#' @param fcs_fruit Input of a character value specifying the column name for number of days of fruits food consumption in the household
#' @param fcs_oil Input of a character value specifying the column name for number of days of oils food consumption in the household
#' @param fcs_sugar Input of a character value specifying the column name for number of days of sugars food consumption in the household
#' @param hdds_cereals Input of a character value specifying the column name for yes/no if cereals consumed yesterday in the household
#' @param hdds_tubers Input of a character value specifying the column name for yes/no if tubers consumed yesterday in the household
#' @param hdds_veg Input of a character value specifying the column name for yes/no if vegetables consumed yesterday in the household
#' @param hdds_fruit Input of a character value specifying the column name for yes/no if fruits consumed yesterday in the household
#' @param hdds_meat Input of a character value specifying the column name for yes/no if meats consumed yesterday in the household
#' @param hdds_eggs Input of a character value specifying the column name for yes/no if eggs consumed yesterday in the household
#' @param hdds_fish Input of a character value specifying the column name for yes/no if fish consumed yesterday in the household
#' @param hdds_legumes Input of a character value specifying the column name for yes/no if legumes consumed yesterday in the household
#' @param hdds_dairy Input of a character value specifying the column name for yes/no if dairy consumed yesterday in the household
#' @param hdds_oils Input of a character value specifying the column name for yes/no if oils consumed yesterday in the household
#' @param hdds_sugars Input of a character value specifying the column name for yes/no if sugars consumed yesterday in the household
#' @param hdds_condiments Input of a character value specifying the column name for yes/no if condiments consumed yesterday in the household
#' @param hhs_nofoodhh_1 Input of a character value specifying the column name for household hunger scale question 1
#' @param hhs_nofoodhh_1a Input of a character value specifying the column name for household hunger scale question 2
#' @param hhs_sleephungry_2 Input of a character value specifying the column name for household hunger scale question 3
#' @param hhs_sleephungry_2a Input of a character value specifying the column name for household hunger scale question 4
#' @param hhs_alldaynight_3 Input of a character value specifying the column name for household hunger scale question 5
#' @param hhs_alldaynight_3a Input of a character value specifying the column name for household hunger scale question 6
#' @param rcsi_lesspreferred_1 Input of a character value specifying the column name for rcsi less preffered foods
#' @param rcsi_borrowfood_2 Input of a character value specifying the column name for rcsi borrow foods
#' @param rcsi_limitportion_3 Input of a character value specifying the column name for rcsi limit portion size
#' @param rcsi_restrict_4 Input of a character value specifying the column name for rcsi restrict consumption for adults
#' @param rcsi_reducemeals5 Input of a character value specifying the column name for rcsi reduce meals per day
#' @param health_barriers Inputs a character vector of column names for self-reported health barriers.
#' @param monthly_expenditures Inputs a character vector of column names for monthly expenditures over 30 days
#' @param period_expenditures Inputs a character vector of column names for period expenditures of a period of time
#' @param num_period_months Inputs a number for the number of months of the period expenditures for health expediture calculations
#' @param food_exp_col Inputs a character value specifying the column name for food expenditures
#' @param health_exp_col Inputs a character value specifying the column name for health expenditures
#' @param lcs_variables Inputs a character vector of column names specifying all the livelihood coping strategy columns.
#' @param vaccine_card_yn Inputs a character value specifying the column name for vaccine card yes no question
#' @param penta Inputs a character value specifying the column name for pentavalent vaccine
#' @param penta_date1 Inputs a character value specifying the column name for date of penta dose 1
#' @param penta_date2 Inputs a character value specifying the column name for date of penta dose 2
#' @param penta_date3 Inputs a character value specifying the column name for date of penta dose 3
#' @param penta_count Inputs a character value specifying the column name for number of penta doses reported
#' @param measles Inputs a character value specifying the column name for measles vaccine
#' @param measles_date1 Inputs a character value specifying the column name for date of measles dose 1
#' @param measles_date2 Inputs a character value specifying the column name for date of measles dose 2
#' @param livelihood_variables Inputs a character vector of column names specifying all the livelihood activities
#' for which income is received. These should be numerical columns with estimated income per livelihood activity.
#' As per MSNA core indicator bank 2023.
#'
#' @return Returns a data frame with standardized variable names and values, additional columns for calculated indicators, and additional columns
#' for data quality flags.
#' @export
#'
#' @examples
#' \dontrun{format_nut_health_indicators(df = dfanthro, age_months = "age_in_months",
#' muac_var = "muac", ...)}
#'
#' @importFrom rlang .data
format_nut_health_indicators <- function(df, #dataframe
                                         file_path = NULL, use_flags_yn = NULL,
                                         hhid = NULL, date_of_dc = NULL, enum = NULL,
                                         cluster = NULL, #survey info variables
                                         id, date_of_birth = NULL, age_years_var = NULL, age_months_var = NULL, age_proxy_var = NULL, age_days = NULL, sex_var, oedema_var, plw_status_var = NULL, # demographic variables
                                         muac_var = NULL, weight_var = NULL, height_var = NULL, # anthropometric variables
                                         type_interview = NULL, wgss_seeing = NULL, wgss_hearing = NULL, wgss_walking = NULL, wgss_remembering = NULL, wgss_selfcare = NULL, wgss_communicating = NULL, # Washington Group Short Set

                                         # select MSNA indicators
                                         health_barriers = NULL,
                                         delivery_location = NULL,
                                         birth_assistant = NULL,

                                         # IYCF 2021
                                         iycf_caregiver = NULL, # Was the mother or primary caregiver of the child interviewed (y/n)
                                         iycf_1 = NULL, # ever breastfed (y/n)
                                         iycf_2 = NULL, # how long after birth breastfed
                                         iycf_3 = NULL, # given anything to eat/drink in first 2 days after delivery
                                         iycf_4 = NULL, # breastfed yesterday during the day or night (y/n)
                                         iycf_5 = NULL, # drink anything from bottle with a nipple (y/n)
                                         iycf_6a = NULL, # plain water
                                         iycf_6b = NULL, # infant formula (y/n)
                                         iycf_6b_num = NULL, # infant formula (number)
                                         iycf_6c = NULL, # milk from animals, fresh tinned powder (y/n)
                                         iycf_6c_num = NULL, # milk form animals, fresh, tinned, pwder (number)
                                         iycf_6c_swt = NULL, # milk was sweetened (y/n)
                                         iycf_6d = NULL, # yoghurt drinks (y/n)
                                         iycf_6d_num = NULL, # yoghurt drinks (number)
                                         iycf_6d_swt = NULL, # yoghurt drink was sweetened (y/n)
                                         iycf_6e = NULL, # chocolate flavoured drinks, including from syrup / powders (y/n)
                                         iycf_6f = NULL, # Fruit juice or fruit-flavoured drinks including those made from syrups or powders? (y/n)
                                         iycf_6g = NULL, # sodas, malt drinks, sports and energy drinks (y/n)
                                         iycf_6h = NULL, # tea, coffee, herbal drinks (y/n)
                                         iycf_6h_swt = NULL, # tea coffee herbal drinks were sweetened (y/n)
                                         iycf_6i = NULL, # clear broth / soup (y/n)
                                         iycf_6j = NULL, # other liquids (y/n)
                                         iycf_6j_swt = NULL, # other drinks were sweetened (y/n)
                                         iycf_7a = NULL, # yoghurt (NOT yoghurt drinks) (y/n)
                                         iycf_7a_num = NULL, # yoghurt (NOT yoghurt drinks) (number)
                                         iycf_7b = NULL, # porridge, bread, rice, nooodles (y/n)
                                         iycf_7c = NULL, # vitamin a rich vegetables (pumpkin, carrots, sweet red peppers, squash or yellow/orange sweet potatoes) (y/n)
                                         iycf_7d = NULL, # white starches (plaintains, white potatoes, white yams, manioc, cassava) (y/n)
                                         iycf_7e = NULL, # dark green leafy vegetables (y/n)
                                         iycf_7f = NULL, # other vegetables (y/n)
                                         iycf_7g = NULL, # vitamin a rich fruits (ripe mangoes, ripe papayas) (y/n)
                                         iycf_7h = NULL, # other fruits (y/n)
                                         iycf_7i = NULL, # organ meats (liver ,kidney, heart) (y/n)
                                         iycf_7j = NULL, # processed meats (sausages, hot dogs, ham, bacon, salami, canned meat) (y/n)
                                         iycf_7k = NULL, # any other meats (beef, chicken, pork, goat, chicken, duck) (y/n)
                                         iycf_7l = NULL, # eggs (y/n)
                                         iycf_7m = NULL, # fish (fresh or dried fish or shellfish) (y/n)
                                         iycf_7n = NULL, # legumes (beans, peas, lentils, seeds, chickpeas) (y/n)
                                         iycf_7o = NULL, # cheeses (hard or soft cheeses) (y/n)
                                         iycf_7p = NULL, # sweets (chocolates, candies, pastries, cakes) (y.n)
                                         iycf_7q = NULL, # fried or empty carbs (chips, crisps, french fries, fried dough, instant noodles) (y/n)
                                         iycf_7r = NULL, # Any other solid, semi-solid, or soft foods
                                         iycf_7s = NULL, # did child eat solid/semi-solid foods (y/n) for list based questionnaires
                                         iycf_8 = NULL, # times child ate solid/semi-solid foods (number)

                                         # Expenditures
                                         monthly_expenditures = NULL, # character vector of columns names for monthly expenses (over 30 days)
                                         period_expenditures = NULL, # character vector of column names for period expenses (over num_period_months)
                                         num_period_months = NULL,  # number of months of the period for period expenses
                                         health_exp_col = NULL, # character value of name of the health expenses column
                                         food_exp_col = NULL, # character value of name of the food expenses column

                                         # Child Vaccination Indicators
                                         vaccine_card_yn = NULL,
                                         penta = NULL,
                                         penta_date1 = NULL,
                                         penta_date2 = NULL,
                                         penta_date3 = NULL,
                                         penta_count = NULL,
                                         measles = NULL,
                                         measles_date1 = NULL,
                                         measles_date2 = NULL,

                                         # Food security outcome indicators
                                         fcs_cereal = NULL, fcs_legumes = NULL, fcs_dairy = NULL, fcs_meat = NULL, fcs_veg = NULL, fcs_fruit = NULL, fcs_oil = NULL, fcs_sugar = NULL,
                                         hdds_cereals = NULL, hdds_tubers = NULL, hdds_veg = NULL, hdds_fruit = NULL, hdds_meat = NULL, hdds_eggs = NULL, hdds_fish = NULL,
                                         hdds_legumes = NULL, hdds_dairy = NULL,  hdds_oils = NULL, hdds_sugars = NULL, hdds_condiments = NULL,
                                         hhs_nofoodhh_1 = NULL, hhs_nofoodhh_1a = NULL, hhs_sleephungry_2 = NULL, hhs_sleephungry_2a = NULL, hhs_alldaynight_3 = NULL, hhs_alldaynight_3a = NULL,
                                         rcsi_lesspreferred_1 = NULL, rcsi_borrowfood_2 = NULL, rcsi_limitportion_3 = NULL, rcsi_restrict_4 = NULL, rcsi_reducemeals5 = NULL,
                                         lcs_variables = NULL,
                                         livelihood_variables = NULL
) {

  options(warn=-1)

  # Create initial map

  # Step 1: Create initial map of categorical and character columns.

  df_map <- create_value_map(df)

  # Step 4: Carry throughout the current formatting functions and add mapped values
  # use an "appending" function to add on variable names and values

  # Step 5: Create a "checking" function on whether new mapping is needed.

  # Step 6: Exporting and importing the map


  # Rename Columns

  variable <- names(df)

  df <- df %>%
    dplyr::rename(date_dc = {{date_of_dc}},
           enum = {{enum}},
           cluster = {{cluster}},
           hh_id = {{hhid}},
           individual_id = {{id}},
           sex = {{sex_var}},
           dob = {{date_of_birth}},
           age_years = {{age_years_var}},
           age_months = {{age_months_var}},
           age_proxy = {{age_proxy_var}},
           age_days = {{age_days}},
           muac = {{muac_var}},
           weight = {{weight_var}},
           height = {{height_var}},
           oedema = {{oedema_var}},
           # Washington Group Short Set vars
           wgss_type_interview = {{type_interview}},
           wgss1_seeing = {{wgss_seeing}},
           wgss2_hearing = {{wgss_hearing}},
           wgss3_walking = {{wgss_walking}},
           wgss4_remembering = {{wgss_remembering}},
           wgss5_selfcare = {{wgss_selfcare}},
           wgss6_communicating = {{wgss_communicating}},
           # IYCF 2021 vars
           iycf_caregiver = {{iycf_caregiver}},
           iycf_1 = {{iycf_1}}, # ever breastfed (y.n)
           iycf_2 = {{iycf_2}}, # how long after birth breastfed
           iycf_3 = {{iycf_3}}, # given anything to eat/drink in first 2 days after delivery
           iycf_4 = {{iycf_4}}, # breastfed yesterday during the day or night (y/n)
           iycf_5 = {{iycf_5}}, # drink anything from bottle with a nipple (y/n)
           iycf_6a = {{iycf_6a}}, # plain water
           iycf_6b = {{iycf_6b}}, # infant formula (y/n)
           iycf_6b_num = {{iycf_6b_num}}, # infant formula (number)
           iycf_6c = {{iycf_6c}}, # milk from animals, fresh tinned powder (y/n)
           iycf_6c_num = {{iycf_6c_num}}, # milk form animals, fresh, tinned, pwder (number)
           iycf_6c_swt = {{iycf_6c_swt}}, # milk was sweetened (y/n)
           iycf_6d = {{iycf_6d}}, # yoghurt drinks (y/n)
           iycf_6d_num = {{iycf_6d_num}}, # yoghurt drinks (number)
           iycf_6d_swt = {{iycf_6d_swt}}, # yoghurt drink was sweetened (y/n)
           iycf_6e = {{iycf_6e}}, # chocolate flavoured drinks, including from syrup / powders (y/n)
           iycf_6f = {{iycf_6f}}, # Fruit juice or fruit-flavoured drinks including those made from syrups or powders? (y/n)
           iycf_6g = {{iycf_6g}}, # sodas, malt drinks, sports and energy drinks (y/n)
           iycf_6h = {{iycf_6h}}, # tea, coffee, herbal drinks (y/n)
           iycf_6h_swt = {{iycf_6h_swt}}, # tea coffee herbal drinks were sweetened (y/n)
           iycf_6i = {{iycf_6i}}, # clear broth / soup (y/n)
           iycf_6j = {{iycf_6j}}, # other liquids (y/n)
           iyf_6j_swt = {{iycf_6j_swt}}, # other drinks were sweetened (y/n)
           iycf_7a = {{iycf_7a}}, # yoghurt (NOT yoghurt drinks) (y/n)
           iycf_7a_num = {{iycf_7a_num}}, # yoghurt (NOT yoghurt drinks) (number)
           iycf_7b = {{iycf_7b}}, # porridge, bread, rice, nooodles (y/n)
           iycf_7c = {{iycf_7c}}, # vitamin a rich vegetables (pumpkin, carrots, sweet red peppers, squash or yellow/orange sweet potatoes) (y/n)
           iycf_7d = {{iycf_7d}}, # white starches (plaintains, white potatoes, white yams, manioc, cassava) (y/n)
           iycf_7e = {{iycf_7e}}, # dark green leafy vegetables (y/n)
           iycf_7f = {{iycf_7f}}, # other vegetables (y/n)
           iycf_7g = {{iycf_7g}}, # vitamin a rich fruits (ripe mangoes, ripe papayas) (y/n)
           iycf_7h = {{iycf_7h}}, # other fruits (y/n)
           iycf_7i = {{iycf_7i}}, # organ meats (liver ,kidney, heart) (y/n)
           iycf_7j = {{iycf_7j}}, # processed meats (sausages, hot dogs, ham, bacon, salami, canned meat) (y/n)
           iycf_7k = {{iycf_7k}}, # any other meats (beef, chicken, pork, goat, chicken, duck) (y/n)
           iycf_7l = {{iycf_7l}}, # eggs (y/n)
           iycf_7m = {{iycf_7m}}, # fish (fresh or dried fish or shellfish) (y/n)
           iycf_7n = {{iycf_7n}}, # legumes (beans, peas, lentils, seeds, chickpeas) (y/n)
           iycf_7o = {{iycf_7o}}, # cheeses (hard or soft cheeses) (y/n)
           iycf_7p = {{iycf_7p}}, # sweets (chocolates, candies, pastries, cakes) (y.n)
           iycf_7q = {{iycf_7q}}, # fried or empty carbs (chips, crisps, french fries, fried dough, instant noodles) (y/n)
           iycf_7r = {{iycf_7r}}, # Any other solid, semi-solid, or soft foods
           iycf_7s = {{iycf_7s}}, # did child eat solid/semi-solid foods (y/n) for list based questionnaires
           iycf_8 = {{iycf_8}}, # times child ate solid/semi-solid foods (number))
           # FSL Variables
           fcs_cereal = {{fcs_cereal}},
           fcs_legumes = {{fcs_legumes}},
           fcs_dairy = {{fcs_dairy}},
           fcs_meat = {{fcs_meat}},
           fcs_veg = {{fcs_veg}},
           fcs_fruit = {{fcs_fruit}},
           fcs_oil = {{fcs_oil}},
           fcs_sugar = {{fcs_sugar}},
           hdds_cereals = {{hdds_cereals}},
           hdds_tubers = {{hdds_tubers}},
           hdds_veg = {{hdds_veg}},
           hdds_fruit = {{hdds_fruit}},
           hdds_meat = {{hdds_meat}},
           hdds_eggs = {{hdds_eggs}},
           hdds_fish = {{hdds_fish}},
           hdds_legumes = {{hdds_legumes}},
           hdds_dairy = {{hdds_dairy}},
           hdds_oils = {{hdds_oils}},
           hdds_sugars = {{hdds_sugars}},
           hdds_condiments = {{hdds_condiments}},
           hhs_nofoodhh_1 = {{hhs_nofoodhh_1}},
           hhs_nofoodhh_1a = {{hhs_nofoodhh_1a}},
           hhs_sleephungry_2 = {{hhs_sleephungry_2}},
           hhs_sleephungry_2a = {{hhs_sleephungry_2a}},
           hhs_alldaynight_3 = {{hhs_alldaynight_3}},
           hhs_alldaynight_3a = {{hhs_alldaynight_3a}},
           rcsi_lesspreferred_1 = {{rcsi_lesspreferred_1}},
           rcsi_borrowfood_2 = {{rcsi_borrowfood_2}},
           rcsi_limitportion_3 = {{rcsi_limitportion_3}},
           rcsi_restrict_4 = {{rcsi_restrict_4}},
           rcsi_reducemeals5 = {{rcsi_reducemeals5}},
           # Child vaccination indicators
           vaccine_card_yn = {{vaccine_card_yn}},
           penta = {{penta}},
           penta_date1 = {{penta_date1}},
           penta_date2 = {{penta_date2}},
           penta_date3 = {{penta_date3}},
           penta_count = {{penta_count}},
           measles = {{measles}},
           measles_date1 = {{measles_date1}},
           measles_date2 = {{measles_date2}},
           # Select MSNA Indicators
           delivery_location = {{delivery_location}},
           birth_assistant = {{birth_assistant}},
           # Health and Food expenditures
           main_income_source = {{main_income_source}},
           monthly_income = {{monthly_income}},
           food_exp = {{food_exp_col}},
           health_exp = {{health_exp_col}}


    )

  new_variable <- names(df)

  changed_names <- data.frame(
    variable,
    new_variable
  ) %>%
    dplyr::filter(variable != new_variable)

  df_map <- df_map %>%
    dplyr::filter(variable %in% changed_names$variable) %>%
    dplyr::left_join(changed_names, by = "variable")

  map_object <- list(changed_names, df_map)


  # input checks for anthropometry
  if(c("age_months") %in% names(df)) {

    if(!all(varhandle::check.numeric(df$age_months))) {stop("There are non-numeric values in the age_months variable. Please check your input.")} else {df <- df %>% dplyr::mutate(age_months = ifelse(is.na(.data$age_months), NA,  as.numeric(.data$age_months)))}


  }

  if(c("weight") %in% names(df)) {
    if(!all(varhandle::check.numeric(df$weight))) {
      stop("There are non-numeric values in the weight variable. Please check your input.")
    } else {df <- df %>% dplyr::mutate(weight = ifelse(is.na(.data$weight), NA,  as.numeric(.data$weight)))}

  } else if(length(setdiff(c("height", "age_months", "age_days", "dob"), names(df)))==4 & (c("weight") %in% colnames(df))) {stop("You have weight, but neither height or any age variables included in the input. Weight must be analyzed with either height or age variables. Please revise your input or remove the weight variable.")}

  if(c("height") %in% names(df)) {
    if(!all(varhandle::check.numeric(df$height))) {stop("There are non-numeric values in the height variable. Please check your input.")} else {df <- df %>% dplyr::mutate(height = ifelse(is.na(.data$height), NA,  as.numeric(.data$height)))}




  } else if(length(setdiff(c("weight", "age_months", "age_days", "dob"), names(df)))==4 & (c("height") %in% colnames(df))) {stop("You have height, but neither weight or any age variables included in the input. Height must be analyzed with either weight or age variables. Please revise your input or remove the height variable.")}
  if(c("muac") %in% names(df)) {
    if(!all(varhandle::check.numeric(df$muac))) {stop("There are non-numeric values in the muac variable. Please check your input.")} else {df <- df %>% dplyr::mutate(muac = ifelse(is.na(.data$muac), NA,  as.numeric(.data$muac)))}
  }

  # input checks Washington Group Short Set
  wgss_vars <- c("wgss1_seeing", "wgss2_hearing", "wgss3_walking", "wgss4_remembering", "wgss5_selfcare", "wgss6_communicating")

  if(length(setdiff(wgss_vars, colnames(df)))!=6) {
    if(length(setdiff(wgss_vars, colnames(df)))!=0) {stop(cat("You have not entered in the minimum variables needed for Washington Group Short Set of questions. \n You must include  all six WGSS questions (seeing, hearing, walking, remembering, selfcare, and communicating)."))}
  }


  # input checks for health_barriers
  if(!is.null(health_barriers)) {
    if(!is.vector(health_barriers)) {stop("Health barriers must be a character vector of column names for mulitple choice health barriers. Something like c('', '', '', ...). Please check your input.")}
    if(!is.character(health_barriers)) {stop("The 'health barriers' argument should contain a character vector of the names of the health barrier columns in your dataframe. Your input was not a character vector. Please check your input.")}
    if(length(setdiff(health_barriers, colnames(df)))!=0) {stop("Not all column names in health_barriers exist in the dataframe. Please check your input.")}
  }

  # input checks for lcs_variables
  if(!is.null(lcs_variables)) {
    if(!is.vector(lcs_variables)) {stop("lcs_variables must be a character vector of column names for livelihood coping strategy questions. Something like c('', '', '', ...). Please check your input.")}
    if(!is.character(lcs_variables)) {stop("The 'lcs_variables' argument should contain a character vector of the names of the livelihood coping strategy columns in your dataframe. Your input was not a character vector. Please check your input.")}
    if(length(setdiff(lcs_variables, colnames(df)))!=0) {stop("Not all column names in lcs_variables exist in the dataframe. Please check your input.")}
  }

  # input checks for livelihood_variables
  if(!is.null(livelihood_variables)) {
    if(!is.vector(livelihood_variables)) {stop("livelihood_variables must be a character vector of column names for main sources of income, as per MSNA 2023 guidelines. Something like c('', '', '', ...). Please check your input.")}
    if(!is.character(livelihood_variables)) {stop("The 'livelihood_variables' argument should contain a character vector of the names of the livelihood coping strategy columns in your dataframe. Your input was not a character vector. Please check your input.")}
    if(length(setdiff(livelihood_variables, colnames(df)))!=0) {stop("Not all column names in livelihood_variables exist in the dataframe. Please check your input.")}
  }

  # input checks for IYCF 2021
  # calculating any tertiary yn/number vars that may not have been included

  if(is.null(iycf_6b) & !is.null(iycf_6b_num)) {df <- df %>% dplyr::mutate(iycf_6b = ifelse(is.na(.data$iycf_6b_num), NA, ifelse(.data$iycf_6b_num == 0,"n", "y")))}
  if(is.null(iycf_6c) & !is.null(iycf_6c_num)) {df <- df %>% dplyr::mutate(iycf_6c = ifelse(is.na(.data$iycf_6c_num), NA, ifelse(.data$iycf_6c_num == 0,"n", "y")))}
  if(is.null(iycf_6d) & !is.null(iycf_6d_num)) {df <- df %>% dplyr::mutate(iycf_6d = ifelse(is.na(.data$iycf_6d_num), NA, ifelse(.data$iycf_6d_num == 0,"n", "y")))}
  if(is.null(iycf_7a) & !is.null(iycf_7a_num)) {df <- df %>% dplyr::mutate(iycf_7a = ifelse(is.na(.data$iycf_7a_num), NA, ifelse(.data$iycf_7a_num == 0,"n", "y")))}

  # checks for which indicators can be calculated

  print("The following standard IYCF indicators can, or cannot, be calculated with the given inputs. Only checking based on columns provided, has not checked column values yet.")

  ebf_vars <- c("iycf_4", "iycf_6a", "iycf_6b", "iycf_6c", "iycf_6d", "iycf_6e", "iycf_6f", "iycf_6g", "iycf_6h", "iycf_6i", "iycf_6j",
                "iycf_7a", "iycf_7b", "iycf_7c", "iycf_7d", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "iycf_7n", "iycf_7o", "iycf_7p", "iycf_7q", "iycf_7r")
  mdd_vars <- c("iycf_4", "iycf_6b", "iycf_6c", "iycf_6d", "iycf_7a", "iycf_7b", "iycf_7c", "iycf_7d", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "iycf_7n", "iycf_7o")
  mmf_vars <- c("iycf_4", "iycf_6b_num", "iycf_6c_num", "iycf_6d_num", "iycf_8")
  mmff_vars <- c("iycf_4", "iycf_6b_num", "iycf_6c_num", "iycf_6d_num", "iycf_7a_num")
  swt_bv_vars <- c("iycf_6c_swt", "iycf_6d_swt", "iycf_6e", "iycf_6f", "iycf_6g", "iycf_6h_swt", "iycf_6j_swt")
  food_vars <- c("iycf_7a", "iycf_7b", "iycf_7c", "iycf_7d", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h", "iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m", "iycf_7n", "iycf_7o", "iycf_7p", "iycf_7q", "iycf_7r")
  flesh_foods_vars <- c("iycf_7i", "iycf_7j", "iycf_7k", "iycf_7l", "iycf_7m")
  unhealthy_food_vars <- c("iycf_7p", "iycf_7q")
  zero_veg_fruit_vars <- c("iycf_7c", "iycf_7e", "iycf_7f", "iycf_7g", "iycf_7h")

  if(c("iycf_4") %in% names(df)) {

    if(!is.null(iycf_1) & !is.null(age_months_var)) {print("IYCF Indicator 1: Ever Breastfed; YES")} else {print("IYCF Indicator 1: Ever Breastfed; NO")}
    if(!is.null(iycf_2) & !is.null(age_months_var)) {print("IYCF Indicator 2: Early Initiation of Breastfeeding; YES")} else {print("IYCF Indicator 2: Early Initiation of Breastfeeding; NO")}
    if(!is.null(iycf_3) & !is.null(age_months_var)) {print("IYCF Indicator 3: Exclusive Breastfeeding First 2 Days After Birth; YES")} else {print("IYCF Indicator 3: Exclusive Breastfeeding First 2 Days After Birth; NO")}
    if(!is.null(age_months_var) & length(setdiff(ebf_vars, colnames(df)))== 0) {print("IYCF Indicator 4: Exclusive Breastfeeding; YES")} else {print("IYCF Indicator 2: Exclusive Breastfeeding; NO")}
    if(!is.null(age_months_var) & !is.null(iycf_4) & !is.null(iycf_6b) & !is.null(iycf_6c)) {print("IYCF Indicator 5: Mixed Milk Feeding (MIxMF); YES")} else {print("IYCF Indicator 5: Mixed Milk Feeding (MIxMF); NO")}
    if(!is.null(age_months_var) & !is.null(iycf_4)) {print("IYCF Indicator 6: Continued Breastfeeding 12-23 months; YES")} else {print("IYCF Indicator 6: Continued Breastfeeding 12-23 months; NO")}
    if(!is.null(age_months_var) & length(setdiff(food_vars, colnames(df))) == 0) {print("IYCF Indicator 7: Introduction of Solid, Semi-Solid, or Soft Foods; YES")} else {print("IYCF Indicator 7: Introduction of Solid, Semi-Solid, or Soft Foods; NO")}
    if(!is.null(age_months_var) & length(setdiff(mdd_vars, colnames(df)))==0) {print("IYCF Indicator 8: Minimum Dietary Diversity 6-23 months (MDD); YES")} else {print("IYCF Indicator 8: Minimum Dietary Diversity 6-23 months (MDD); NO")}
    if(!is.null(age_months_var) & length(setdiff(mmf_vars, colnames(df)))==0) {print("IYCF Indicator 9: Minimum Meal Frequency 6-23 months (MMF); YES")} else {print("IYCF Indicator 9: Minimum Meal Frequency 6-23 months (MMF); NO")}
    if(!is.null(age_months_var) & length(setdiff(mmff_vars, colnames(df)))==0) {print("IYCF Indicator 10: Minimum Milk Feeding Frequency For Non-Breastfed Children 6-23 months (MMFF); YES")} else {print("IYCF Indicator 10: Minimum Milk Feeding Frequency For Non-Breastfed Children 6-23 months (MMFF); NO")}
    if(!is.null(age_months_var) & length(setdiff(mdd_vars, colnames(df)))==0 & length(setdiff(mmf_vars, colnames(df)))==0) {print("IYCF Indicator 11: Minimum Acceptable Diet 6-23 months (MAD); YES")} else {print("IYCF Indicator 11: Minimum Acceptable Diet 6-23 months (MAD); NO")}
    if(!is.null(age_months_var) & length(setdiff(flesh_foods_vars, colnames(df)))==0) {print("IYCF Indicator 12: Eggs & Flesh Foods Consumption 6-23 months (EFF); YES")} else {print("IYCF Indicator 12: Eggs & Flesh Foods Consumption 6-23 months (EFF); NO")}
    if(!is.null(age_months_var) & length(setdiff(swt_bv_vars, colnames(df)))==0) {print("IYCF Indicator 13: Sweet Beverage Consumption 6-23 months; YES")} else {print("IYCF Indicator 13: Sweet Beverage Consumption 6-23 months; NO")}
    if(!is.null(age_months_var) & length(setdiff(unhealthy_food_vars, colnames(df)))==0) {print("IYCF Indicator 14: Unehalthy Food Consumption 6-23 months (UFC); YES")} else {print("IYCF Indicator 14: Unhealthy Food Consumption; NO")}
    if(!is.null(age_months_var) & length(setdiff(zero_veg_fruit_vars, colnames(df)))==0) {print("IYCF Indicator 15: Zero Vegetable or Fruit Consumption 6-23 months (ZVF); YES")} else {print("IYCF Indicator 15: Zero Vegetable or Fruit Consumption 6-23 months (ZVF);; NO")}
    if(!is.null(age_months_var) & !is.null(iycf_5)) {print("IYCF Indicator 16: Bottle Feeding 0-23 months; YES")} else {print("IYCF Indicator 16: Bottle Feeding 0-23 months;; NO")}
    if(!is.null(age_months_var) & length(setdiff(ebf_vars, colnames(df)))==0) {print("Infant Feeding Area Graphs; YES")} else {print("Infant Feeding Area Graphs; NO")}

  }
  # input checks for FSL

  if(c("fcs_cereal") %in% names(df)) {
    if(!all(varhandle::check.numeric(df$fcs_cereal))) {stop("There are non-numeric values in the fcs_cereal variable. Please check your input.")} else {df <- df %>% dplyr::mutate(fcs_cereal = ifelse(is.na(.data$fcs_cereal), NA,  as.numeric(.data$fcs_cereal)))}
    a <- 1
  } else {a <- 0}
  if(c("fcs_legumes") %in% names(df)) {
    if(!all(varhandle::check.numeric(df$fcs_legumes))) {stop("There are non-numeric values in the fcs_legumes variable. Please check your input.")} else {df <- df %>% dplyr::mutate(fcs_legumes = ifelse(is.na(.data$fcs_legumes), NA,  as.numeric(.data$fcs_legumes)))}
    b <- 1
  } else {b <- 0}
  if(c("fcs_dairy") %in% names(df)) {
    if(!all(varhandle::check.numeric(df$fcs_dairy))) {stop("There are non-numeric values in the fcs_dairy variable. Please check your input.")} else {df <- df %>% dplyr::mutate(fcs_dairy = ifelse(is.na(.data$fcs_dairy), NA,  as.numeric(.data$fcs_dairy)))}
    c <- 1
  } else {c <- 0}
  if(c("fcs_meat") %in% names(df)) {
    if(!all(varhandle::check.numeric(df$fcs_meat))) {stop("There are non-numeric values in the fcs_meat variable. Please check your input.")} else {df <- df %>% dplyr::mutate(fcs_meat = ifelse(is.na(.data$fcs_meat), NA,  as.numeric(.data$fcs_meat)))}
    d <- 1
  } else {d <- 0}
  if(c("fcs_veg") %in% names(df)) {
    if(!all(varhandle::check.numeric(df$fcs_veg))) {stop("There are non-numeric values in the fcs_veg variable. Please check your input.")} else {df <- df %>% dplyr::mutate(fcs_veg = ifelse(is.na(.data$fcs_veg), NA,  as.numeric(.data$fcs_veg)))}
    e <- 1
  } else {e <- 0}
  if(c("fcs_fruit") %in% names(df)) {
    if(!all(varhandle::check.numeric(df$fcs_fruit))) {stop("There are non-numeric values in the fcs_fruit variable. Please check your input.")} else {df <- df %>% dplyr::mutate(fcs_fruit = ifelse(is.na(.data$fcs_fruit), NA,  as.numeric(.data$fcs_fruit)))}
    f <- 1
  } else {f <- 0}
  if(c("fcs_oil") %in% names(df)) {
    if(!all(varhandle::check.numeric(df$fcs_oil))) {stop("There are non-numeric values in the fcs_oil variable. Please check your input.")} else {df <- df %>% dplyr::mutate(fcs_oil = ifelse(is.na(.data$fcs_oil), NA,  as.numeric(.data$fcs_oil)))}
    g <- 1
  } else {g <- 0}
  if(c("fcs_sugar") %in% names(df)) {
    if(!all(varhandle::check.numeric(df$fcs_sugar))) {stop("There are non-numeric values in the fcs_sugar variable. Please check your input.")} else {df <- df %>% dplyr::mutate(fcs_sugar = ifelse(is.na(.data$fcs_sugar), NA,  as.numeric(.data$fcs_sugar)))}
    h <- 1
  } else {h <- 0}

  fcs_vars <- c("fcs_cereal", "fcs_legumes", "fcs_dairy", "fcs_meat", "fcs_veg", "fcs_fruit", "fcs_oil", "fcs_sugar")
  if(length(setdiff(fcs_vars, colnames(df)))> 0 & length(setdiff(fcs_vars, colnames(df))) < 8) {stop("You've included some of the columns necesssary for Food Consumption Scores (FCS), but not all of them. You minimally require fcs_cereal, fcs_legumes, fcs_dairy, fcs_meat, fcs_veg, fcs_fruit, fcs_oil, fcs_sugar. Please check your input. ")}

  if(c("rcsi_lesspreferred_1") %in% names(df)) {
    if(!all(varhandle::check.numeric(df$rcsi_lesspreferred_1))) {stop("There are non-numeric values in the rcsi_lesspreferred_1 variable. Please check your input.")} else {df <- df %>% dplyr::mutate(rcsi_lesspreferred_1 = ifelse(is.na(.data$rcsi_lesspreferred_1), NA,  as.numeric(.data$rcsi_lesspreferred_1)))}
  }
  if(c("rcsi_borrowfood_2") %in% names(df)) {
    if(!all(varhandle::check.numeric(df$rcsi_borrowfood_2))) {stop("There are non-numeric values in the rcsi_borrowfood_2 variable. Please check your input.")} else {df <- df %>% dplyr::mutate(rcsi_borrowfood_2 = ifelse(is.na(.data$rcsi_borrowfood_2), NA,  as.numeric(.data$rcsi_borrowfood_2)))}
  }
  if(c("rcsi_limitportion_3") %in% names(df)) {
    if(!all(varhandle::check.numeric(df$rcsi_limitportion_3))) {stop("There are non-numeric values in the rcsi_limitportion_3 variable. Please check your input.")} else {df <- df %>% dplyr::mutate(rcsi_limitportion_3 = ifelse(is.na(.data$rcsi_limitportion_3), NA,  as.numeric(.data$rcsi_limitportion_3)))}
  }
  if(c("rcsi_restrict_4") %in% names(df)) {
    if(!all(varhandle::check.numeric(df$rcsi_restrict_4))) {stop("There are non-numeric values in the rcsi_restrict_4 variable. Please check your input.")} else {df <- df %>% dplyr::mutate(rcsi_restrict_4 = ifelse(is.na(.data$rcsi_restrict_4), NA,  as.numeric(.data$rcsi_restrict_4)))}
  }
  if(c("rcsi_reducemeals5") %in% names(df)) {
    if(!all(varhandle::check.numeric(df$rcsi_reducemeals5))) {stop("There are non-numeric values in the rcsi_reducemeals5 variable. Please check your input.")} else {df <- df %>% dplyr::mutate(rcsi_reducemeals5 = ifelse(is.na(.data$rcsi_reducemeals5), NA,  as.numeric(.data$rcsi_reducemeals5)))}
  }

  rcsi_vars <- c("rcsi_lesspreferred_1", "rcsi_borrowfood_2", "rcsi_limitportion_3", "rcsi_restrict_4", "rcsi_reducemeals5")
  if(length(setdiff(rcsi_vars, colnames(df)))> 0 & length(setdiff(rcsi_vars, colnames(df))) < 5 ) {stop("You've included some of the columns necesssary for reduced Coping Strategies Index (rCSI), but not all of them. You minimally require rcsi_lesspreferred_1, rcsi_borrowfood_2, rcsi_limitportion_3, rcsi_restrict_4, rcsi_reducemeals5. ")}

  # input checks for health expenditure indicators

  if(!is.null(monthly_expenditures)) {
    if(!is.vector(monthly_expenditures)) {stop("monthly_expenditures must be a character vector of column names. Something like c('', '', '', ...). Please check your input.")}
    if(!is.character(monthly_expenditures)) {stop("The 'monthly_expenditures' parameter should contain a character vector of the names of the monthly_expenditures columns in your dataframe. Your input was not a character vector. Please check your input.")}

    if(!is.null(food_exp_col)) {monthly_expenditures <- replace(monthly_expenditures, monthly_expenditures == food_exp_col, "food_exp")}

    if(length(setdiff(monthly_expenditures, colnames(df)))!=0) {stop("Not all column names in monthly_expenditures exist in the dataframe. Please check your input.")}
  }
  if(!is.null(period_expenditures)) {
    if(!is.vector(period_expenditures)) {stop("period_expenditures must be a character vector of column names. Something like c('', '', '', ...). Please check your input.")}
    if(!is.character(period_expenditures)) {stop("The 'period_expenditures' parameter should contain a character vector of the names of the period_expenditures columns in your dataframe. Your input was not a character vector. Please check your input.")}

    if(!is.null(health_exp_col)) {period_expenditures <- replace(period_expenditures, period_expenditures == health_exp_col, "health_exp")}

    if(length(setdiff(period_expenditures, colnames(df)))!=0) {stop("Not all column names in period_expenditures exist in the dataframe. Please check your input.")}
  }

  if(!is.null(monthly_expenditures)) {a <- 1} else {a <- 0 }
  if(!is.null(period_expenditures)) {b <- 1} else {b <- 0 }
  if(!is.null(food_exp_col)) {c2 <- 1} else {c2 <- 0 }
  if(!is.null(health_exp_col)) {c <- 1} else {c <- 0 }
  if(!is.null(num_period_months)) {d <- 1} else {d <- 0 }


  e <- a + c + c2
  f <- a + b + c + d

  if(e == 1) {stop("To calculate health or food expenditure indicators over 30 day period, you minimally require the monthly_expenditures and health_exp_col and/or  parameters. You only have one, but not both. Please check your input.")}
  if(f > 0 & f < 4 & b + d > 0 & c == 1) {stop("To calculate health expenditures with varying recall periods, you minimially require the monthly_expenditures, period_expenditures, health_exp_col, and num_period_months parameters. You have included some period parameters, but not all the required parameters. Please check your input.")}

  if(!is.null(monthly_expenditures) & !is.null(period_expenditures)) {
    g <- length(intersect(monthly_expenditures, period_expenditures))
    if(g > 0) {stop("You cannot include the same column in both monthly and period expenditures. Please make sure these two sets of expenditures are mutually exclusive. Please check your input.")}

  }

  if(!is.null(monthly_expenditures)) {

    for (i in 1:length(monthly_expenditures)) {

      a <- monthly_expenditures[[i]]

      if(all(varhandle::check.numeric(df %>% dplyr::pull(a)))) {} else {stop(paste0("There are non-numeric values in ", monthly_expenditures[[i]], ". Please check your input."))}

    }

  }
  if(!is.null(period_expenditures)) {

    for (i in 1:length(period_expenditures)) {

      a <- period_expenditures[[i]]

      if(all(varhandle::check.numeric(df %>% dplyr::pull(a)))) {} else {stop(paste0("There are non-numeric values in ", period_expenditures[[i]], ". Please check your input."))}

    }

  }
  if(!is.null(health_exp_col)) {
    if(all(varhandle::check.numeric(df %>% dplyr::pull("health_exp")))) {} else {stop(paste0("There are non-numeric values in ", health_exp_col, ". Please check your input."))}
  }
  if(!is.null(food_exp_col)) {
    if(all(varhandle::check.numeric(df %>% dplyr::pull("food_exp")))) {} else {stop(paste0("There are non-numeric values in ", food_exp_col, ". Please check your input."))}
  }
  if(!is.null(num_period_months)) {
    if(all(varhandle::check.numeric(num_period_months))) {} else {stop("There is a non-numeric value for num_period_months. The input must be a number (in months). Please check your input")}
  }

  # input checks for date variables

  if(!is.null(date_of_birth)) {

    dob_recodes <- c("mdy", "dmy", "ymd", "ydm")
    list_to_check <- df %>% dplyr::select(.data$dob) %>% t %>% c %>% unique
    b = 0
    for (i in 1:length(dob_recodes)) {
      a <- all(!is.na(lubridate::parse_date_time(list_to_check,orders=dob_recodes[[i]])))
      if(a == TRUE) {b <- b + 1}
    }

    if(b > 0) {stop("Date of Birth (dob) does not appear to be in any valid date format. Please check if your formatting matches any of the following: 'mdy', 'dmy', 'ymd', or 'ydm'.")}

  }
  if(!is.null(date_of_dc)) {

    dob_recodes <- c("mdy", "dmy", "ymd", "ydm")
    list_to_check <- df %>% dplyr::select(.data$date_dc) %>% t %>% c %>% unique
    b = 0
    for (i in 1:length(dob_recodes)) {
      a <- all(!is.na(lubridate::parse_date_time(list_to_check,orders=dob_recodes[[i]])))
      if(a == TRUE) {b <- b + 1}
    }

    if(b > 3) {stop("Date of Data Collection (date_dc) does not appear to be in any valid date format. Please check if your formatting matches any of the following: 'mdy', 'dmy', 'ymd', or 'ydm'.")}

  }

  if(!is.null(penta_date1)) {

    dob_recodes <- c("mdy", "dmy", "ymd", "ydm")
    list_to_check <- df %>% dplyr::select(.data$penta_date1) %>% t %>% c %>% unique
    b = 0
    for (i in 1:length(dob_recodes)) {
      a <- all(!is.na(lubridate::parse_date_time(list_to_check,orders=dob_recodes[[i]])))
      if(a == TRUE) {b <- b + 1}
    }

    if(b > 0) {stop("penta_date1 does not appear to be in any valid date format. Please check if your formatting matches any of the following: 'mdy', 'dmy', 'ymd', or 'ydm'.")}

  }
  if(!is.null(penta_date2)) {

    dob_recodes <- c("mdy", "dmy", "ymd", "ydm")
    list_to_check <- df %>% dplyr::select(.data$penta_date2) %>% t %>% c %>% unique
    b = 0
    for (i in 1:length(dob_recodes)) {
      a <- all(!is.na(lubridate::parse_date_time(list_to_check,orders=dob_recodes[[i]])))
      if(a == TRUE) {b <- b + 1}
    }

    if(b > 0) {stop("penta_date2 does not appear to be in any valid date format. Please check if your formatting matches any of the following: 'mdy', 'dmy', 'ymd', or 'ydm'.")}

  }
  if(!is.null(penta_date3)) {

    dob_recodes <- c("mdy", "dmy", "ymd", "ydm")
    list_to_check <- df %>% dplyr::select(.data$penta_date3) %>% t %>% c %>% unique
    b = 0
    for (i in 1:length(dob_recodes)) {
      a <- all(!is.na(lubridate::parse_date_time(list_to_check,orders=dob_recodes[[i]])))
      if(a == TRUE) {b <- b + 1}
    }

    if(b > 0) {stop("penta_date3 does not appear to be in any valid date format. Please check if your formatting matches any of the following: 'mdy', 'dmy', 'ymd', or 'ydm'.")}

  }
  if(!is.null(measles_date1)) {

    dob_recodes <- c("mdy", "dmy", "ymd", "ydm")
    list_to_check <- df %>% dplyr::select(.data$measles_date1) %>% t %>% c %>% unique
    b = 0
    for (i in 1:length(dob_recodes)) {
      a <- all(!is.na(lubridate::parse_date_time(list_to_check,orders=dob_recodes[[i]])))
      if(a == TRUE) {b <- b + 1}
    }

    if(b > 0) {stop("measles_date1 does not appear to be in any valid date format. Please check if your formatting matches any of the following: 'mdy', 'dmy', 'ymd', or 'ydm'.")}

  }
  if(!is.null(measles_date2)) {

    dob_recodes <- c("mdy", "dmy", "ymd", "ydm")
    list_to_check <- df %>% dplyr::select(.data$measles_date2) %>% t %>% c %>% unique
    b = 0
    for (i in 1:length(dob_recodes)) {
      a <- all(!is.na(lubridate::parse_date_time(list_to_check,orders=dob_recodes[[i]])))
      if(a == TRUE) {b <- b + 1}
    }

    if(b > 0) {stop("measles_date2 does not appear to be in any valid date format. Please check if your formatting matches any of the following: 'mdy', 'dmy', 'ymd', or 'ydm'.")}

  }

  # input check for childhood vaccination indicators

  if(c("penta_count") %in% names(df)) {
    if(all(varhandle::check.numeric(df$penta_count))) {} else {stop("There is a non-numeric value for penta_count The input must be a number. Please check your input.")}


  }

  # Passing to the reformatting and calculation functions

  return_reformat <- reformat_nut_health_indicators(df, health_barriers = health_barriers, lcs_variables = lcs_variables, livelihood_variables = livelihood_variables, value_map = df_map)
  df <- return_reformat[[1]]
  df_map <- return_reformat[[2]]

  if(is.null(use_flags_yn)) {use_flags_yn <- "no"} else {use_flags_yn <- use_flags_yn}

  df <- healthyr::calculate_nut_health_indicators(df, monthly_expenditures = period_expenditures, period_expenditures = period_expenditures, num_period_months = num_period_months)
  df <- healthyr::flag_nut_health_issues(df, use_flags = use_flags_yn)

  new_names <- setdiff(names(df), variable)
  print(paste0("The following columns have been added to the dataset:"))
  print(new_names)

  # Saving the new dataframe to a xlsx, if specified
  if(!is.null(file_path)) {writexl::write_xlsx(df, file_path)}

  options(warn=0)

  return(df)

}
