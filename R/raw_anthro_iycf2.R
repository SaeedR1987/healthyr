
#' Food Security and Nutrition Household Survey Data - Unprocessed
#'
#' A dataset containing household anthropometric and infant young child feeding practices
#' data for children under-5 years of age. Not processed by the healthyr functions.
#'
#' @format A data frame with 1284 rows and 63 variables:
#' \describe{
#'   \item{household_id}{Unique household survey identifier.}
#'   \item{interview_section_n1_n_ref_a_date}{The date of data collection.}
#'   \item{sex_of_name}{Sex of the child}
#'   \item{date_of_birth_of_name}{Date of birth of the child, if known}
#'   \item{interview_section_n1_j_childage}{Age in months of the child, from dat of birth or estimate from a
#'   local events calendar.}
#'   \item{ever_breastfed}{(Yes)/(No) if the child was ever breastfed}
#'   \item{first_put_to_breast}{If the child was put to the breast after birth in (Less than 1 hour),
#'   (Between 1 and 23 hours), (More than 24 hours), (Never breastfed), or (Don't know) }
#'   \item{breastmilk_first_two_days}{(Yes)/(No) if the child was given anything OTHER than breastmilk in the
#'   first two days.}
#'   \item{breastfed_yesterday}{(Yes)/(No) if the child was breastfed yesterday during the day or night.}
#'   \item{bottle_with_nipple}{(Yes)/(No) if the child drank anything from a bottle with a nipple
#'   yesterday during the day or night}
#'   \item{plain_water}{(Yes)/(No) if the child consumed plain water yesterday during the day or night}
#'   \item{infant_formula}{(Yes)/(No) if the child consumed any infant formula yesterday during the day or night}
#'   \item{number_infant_formula}{Number of times consumed infant formula yesterday during the day or night.}
#'   \item{animal_tinned_or_powdered_milk}{(Yes)/(No) if the child consumed any animal milk, tinned milk, or canned
#'   milk yesterday during the day or night}
#'   \item{number_drink_milk}{Number of times consumed animal, tinned or canned milk yesterday during the day or night.}
#'   \item{sweet_or_flavoured_type_of_milk}{(Yes)/(No) if the milk was sweetened or flavoured.}
#'   \item{yogurt_drinks}{(Yes)/(No) if the child consumed any yoghurt drinks yesterday during the day or night}
#'   \item{number_yoghurt_drinks}{Number of times consumed yoghurt drinks yesterday during the day or night}
#'   \item{sweet_or_flavoured_type_of_yogurt_drink}{(Yes)/(No) if the yoghurt drink was sweet or flavoured.}
#'   \item{chocolate_flavoured_drinks}{(Yes)/(No) if the child consumed any chocolate flavoured drinks yesterday
#'   during the day or night}
#'   \item{fruit_juice}{(Yes)/(No) if the child consumed any fruit juice or fruit flavoured drinks yesterday
#'   during the day or night.}
#'   \item{sodas_malt_drinks}{(Yes)/(No) if the child consumed any sodas, malt drinks, sports drinks or energy
#'   drinks yesterday during the day or night.}
#'   \item{tea_coffee}{(Yes)/(No) if the child consumed any tea, coffee, or herbal drinks yesterday
#'   during the day or night.}
#'   \item{any_of_these_drinks_sweetened}{(Yes)/(No) if the tea, coffee or herbal drinks were sweetened or not.}
#'   \item{clear_broth}{(Yes)/(No) if the child consumed any clear broth or soup yesterday during the day or night.}
#'   \item{any_other_liquids}{(Yes)/(No) if the child consumed any other liquids yesterday during the day or night.}
#'   \item{other_liquid}{Text field on what other food consumed, if any.}
#'   \item{any_of_these_otherliquids_sweetened}{(Yes)/(No) if any of these other liquids were sweetened.}
#'   \item{yoghurt_other_than_yogurt_drinks}{(Yes)/(No) if child consumed any yoghurt that was not a drink, yesterday
#'   during the day or night.}
#'   \item{number_food_yoghurt}{Number of times consumed non-drink yoghurt yesterday during the day or night.}
#'   \item{porridge_bread_rice_noodles_pasta_asida_and_kisra}{(Yes)/(No) if the child consumed any porridge, bread,
#'   rice, noodles, pasta, asida, kisra and such staple cereal foods yesterday during the day or night.}
#'   \item{pumpkin_carrots_sweet_red_peppers_squash_sweet_potatoes}{(Yes)/(No) if the child consumed any pumpkin,
#'   carrots, sweet red peppers, squash, red/orange sweet potatoes or such vitamin A rich vegetables yesterday during the day or night.}
#'   \item{plantains_white_potatoes_white_yams_manioc_cassava}{(Yes)/(No) if the child consumed plantains, white potatoes,
#'   white yams, manioc, cassava or other white starcy tubers yesterday during the day or night.}
#'   \item{dark_green_leafy_vegetables}{(Yes)/(No) if the child consumed dark green leafy vegetables yesterday during
#'   the or night.}
#'   \item{any_other_vegetables}{(Yes)/(No) if the child consumed any other vegetables yesterday during the day or night.}
#'   \item{ripe_mangoes_or_ripe_papayas}{(Yes)/(No) if the child consumed ripe mango, papaya or other vitamin A rich
#'   fruits yesterday during the day or night.}
#'   \item{any_other_fruits}{(Yes)/(No) if the child consumed any other fruits yesterday during hte day or night.}
#'   \item{liver_kidney_heart}{(Yes)/(No) if the child consumed any liver, kidney, heart or other animal organs
#'   yesterday during the day or night.}
#'   \item{sausages_hot_dogs_frankfurters_ham_bacon_salami_canned_meat}{(Yes)/(No) if the child consumed any sausages,
#'   hot dogs, ham, bacon, salami, canned meats, or other processed meats yesterday during the day or night.}
#'   \item{any_other_meat_beef_pork_lamb_goat_chicken_duck}{(Yes)/(No) if the child consumed any beef, pork, lamb, goat,
#'   chicken, duck or other meats yesterday during the day or night.}
#'   \item{eggs}{(Yes)/(No) if the child consumed any eggs yesterday during the day or night.}
#'   \item{fish_or_shellfish}{(Yes)/(No) if the child consumed any fish yesterday during the day or night.}
#'   \item{beans_peas_lentils_nuts_seeds}{(Yes)/(No) if the child consumed beans, peas, lentils, nuts, seeds or
#'   other legumes yesterday during the day or night.}
#'   \item{hard_or_soft_cheese}{(Yes)/(No) if the child consumed any cheeses yesterday during the day or night.}
#'   \item{sweet_foods_chocolates_candies_pastries_cakes_biscuit_ice_cream}{(Yes)/(No) if the child consumed any
#'   chocolates, candy, pastries, cakes, biscuits, ice cream or other sweets yesterday during the day or night.}
#'   \item{chips_crisps_puffs_french_fries_fried_dough_instant_noodles}{(Yes)/(No) if the child consumed any chips,
#'   crisps, puffs, french fries, fried dough, instant noodles, or other fried carbohydrayes yesterday during the
#'   day or night.}
#'   \item{any_other_food}{(Yes)/(No) if the child consumed any other food yesterday during the day or night.}
#'   \item{other_food}{Text field what the other food consumed was.}
#'   \item{any_solid_semi_solid_soft_food_yesterday}{(Yes)/(No) if the child consumed any solid, semi-solid, or soft
#'   foods consumed yesterday during the day or night.}
#'   \item{what_solid_semi_solid_soft_foods}{Text field what solid, semi-solid or soft food consumed.}
#'   \item{number_solid_semi_solid_soft_foods_yesterday}{Number of times the child ate solid, semi-solid, or soft foods
#'   yesterday during the day or night.}
#'   \item{weight_in_kg}{Weight of the child in kilograms.}
#'   \item{muac_mid_upper_arm_circumference}{Mid-upper arm circumference (MUAC) of the child in cm.}
#'   \item{bilateral_pitting_oedema}{(Yes)/(No) if the child has bilateral pitting oedema.}
#'   \item{is_the_child_enrolled_in_nutrition_programme_otp_tsfp}{(Yes)/(No) if the child is enrolled in any nutrition
#'   program such as OTP or TSFP at the time of interview.}
#'   \item{the_childs_height_length_to_use}{Length or Height of the childin cm.}
#'   \item{date_of_interview}{Date of data collection.}
#'   \item{team_number}{The survey team or enumerator ID.}
#'   \item{state_admin1}{The administrative level 1 name of the county where the survey took place.}
#'   \item{county_admin2}{The administrative level 2 name of the county where the survey took place.}
#'   \item{residency_status}{Residency status of the household as Resident, IDP, IDP Returnee, or Refugee Returnee.}
#'   \item{cluster_id}{The cluster id.}
#'
#' }
#'
#' @source A food security and nutrition monitoring survey.
#'
#' @examples
#' data(raw_anthro_iycf2)
"raw_anthro_iycf2"
