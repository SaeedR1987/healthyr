
#' SMART Survey Food Security Data - Unprocessed
#'
#' A dataset containing household food security and livelihood coping strategy data from a SMART Survey.
#' Data has not been processed by the healthyr functions.
#'
#' @format A data frame with 507 rows and 47 variables:
#' \describe{
#'   \item{today}{Date of data collection}
#'   \item{enum}{The survey team or enumerator ID.}
#'   \item{cluster_id}{The cluster id.}
#'   \item{F01A}{Input for fcs_cereal. Number of days cereals, grains, roots and tubers consumed in the past 7 days. }
#'   \item{F011B}{Input for hdds_cereal. Yes(1)/No(0) if the household consumed cereals or grains in the previous day.}
#'   \item{F012B}{Input for hdds_tubers. Yes(1)/No(0) if the household consumed roots or tubers in the previous day.}
#'   \item{F02A}{Input for fcs_legumes. Number of days pulses, legumes, nuts consumed in the past 7 days.}
#'   \item{F02B}{Input for hdds_legumes. Yes(1)/No(0) if the household consumed pulses, legumes or nuts in the previous day.}
#'   \item{F03A}{Input for fcs_dairy. Number of days milk or dairy products consumed in the past 7 days.}
#'   \item{F03B}{Input for hdds_dairy. Yes(1)/No(0) if the household consumed milk or dairy products in the previous day.}
#'   \item{F04A}{Input for fcs_meat. Number of days meats, fish or eggs consumed in the past 7 days.}
#'   \item{F041B}{Contributes to input for hdds_meats, Yes(1)/No(0) but reports only if consumed flesh meats in the previous day.}
#'   \item{F042B}{Contributes to input for hdds_meats, Yes(1)/No(0) but reports only if consumed organ meats in the previous day.}
#'   \item{hdds_meats_any}{Input for hdds_meats. Yes(1)/No(0) combines F041B and F042B if either consumed in the previous day.}
#'   \item{F043B}{Input for hdds_fish. Yes(1)/No(0) if the household consumed fish in the previous day.}
#'   \item{F044B}{Input for hdds_eggs. Yes(1)/No(0) if the household consumed eggs in the previous day.}
#'   \item{F05A}{Input for fcs_veg. Number of days vegetables and leaves consumed in the past 7 days.}
#'   \item{F05B}{Input for hdds_veg. Yes(1)/No(0) if the household consumed vegetables or leaves the previous day.}
#'   \item{F06A}{Input for fcs_fruit. Number of days fruits consumed in the past 7 days.}
#'   \item{F06B}{Input for hdds_fruit. Yes(1)/No(0) if the household consumed fruits the previous day.}
#'   \item{F07A}{Input for fcs_oil. Number of days oil, fat or butter consumed in the past 7 days.}
#'   \item{F07B}{Input for hdds_oils. Yes(1)/No(0) if the household consumed oil, fat or butter in the previous day.}
#'   \item{F08A}{Input for fcs_sugar. Number of days sugar or sweets consumed in the past 7 days.}
#'   \item{F08B}{Input for hdds_sugar. Yes(1)/No(0) if the household consumed sugar or sweets in the previous day.}
#'   \item{F09B}{Input for hdds_condiments. Yes(1)/No(0) if the household consumed condiments such as salt, spices, tea, coffee etc. in the previous day.}
#'   \item{hhs_1}{Input for hhs_nofoodhh_1. Yes(1)/No(0) if the household experienced having no food at all in the household in the past 30 days.}
#'   \item{hhs_2}{Input for hhs_nofoodhh_1a. Rarely 1-2 times(1), Sometimes 3-10 times(2), or Often 10+ times (3) that the household experienced
#'   having no food at all in the household in the past 30 days.}
#'   \item{hhs_3}{Input for hhs_sleephungry_2. Yes(1)/No(0) if anyone in the household experienced going to bed hungry because of not having enough food
#'   in the past 30 days.}
#'   \item{hhs_4}{Input for hhs_sleephungry_2a. Rarely 1-2 times(1), Sometimes 3-10 times(2), or Often 10+ times (3) that anyone one in the household
#'   experienced going to bed hungry because of not having enough food in the past 30 days.}
#'   \item{hhs_5}{Input for hhs_alldaynight_3. Yes(1)/No(0) if anyone in the household experienced not eating anything all the previous
#'   day and night in the past 30 days.}
#'   \item{hhs_6}{Input for hhs_alldaynight_3a. Rarely 1-2 times(1), Sometimes 3-10 times(2), or Often 10+ times (3) that anyone one in the household
#'   experienced not eating anything all day and night in the past 30 days}
#'   \item{rcsi1}{Input for rcsi_lesspreferred_1. Number of days household reported consuming less preferred foods in the past 7 days.}
#'   \item{rcsi2}{Input for rcsi_borrowfood_2. Number of days household reported borrowing foods in the past 7 days.}
#'   \item{rcsi3}{Input for rcsi_limitportion_3. Number of days household reported limiting portion size in the past 7 days.}
#'   \item{rcsi4}{Input for rcsi_restrict_4. Number of days household reported restricting meals for adults so children could eat in the past 7 days.}
#'   \item{rcsi5}{Input for rcsi_reducemeals5. Number of days household reported reducing the number of meals consumed in the past 7 days.}
#'   \item{lcs1}{Input for lcs_variables, a STRESS Level Livelihood Coping. Yes(1), No, didn't do (2), No, didn't do because strategy exhausted (3), or Not applicable (4)
#'   if the household used Livelihood Coping Strategy #1.}
#'   \item{lcs2}{Input for lcs_variables, a STRESS Level Livelihood Coping. Yes(1), No, didn't do (2), No, didn't do because strategy exhausted (3), or Not applicable (4)
#'   if the household used Livelihood Coping Strategy #2.}
#'   \item{lcs3}{Input for lcs_variables, a STRESS Level Livelihood Coping. Yes(1), No, didn't do (2), No, didn't do because strategy exhausted (3), or Not applicable (4)
#'   if the household used Livelihood Coping Strategy #3.}
#'   \item{lcs4}{Input for lcs_variables, a CRISIS Level Livelihood Coping. Yes(1), No, didn't do (2), No, didn't do because strategy exhausted (3), or Not applicable (4)
#'   if the household used Livelihood Coping Strategy #4.}
#'   \item{lcs5}{Input for lcs_variables, a STRESS Level Livelihood Coping. Yes(1), No, didn't do (2), No, didn't do because strategy exhausted (3), or Not applicable (4)
#'   if the household used Livelihood Coping Strategy #5.}
#'   \item{lcs6}{Input for lcs_variables, a CRISIS Level Livelihood Coping. Yes(1), No, didn't do (2), No, didn't do because strategy exhausted (3), or Not applicable (4)
#'   if the household used Livelihood Coping Strategy #6.}
#'   \item{lcs7}{Input for lcs_variables, a CRISIS Level Livelihood Coping. Yes(1), No, didn't do (2), No, didn't do because strategy exhausted (3), or Not applicable (4)
#'   if the household used Livelihood Coping Strategy #7.}
#'   \item{lcs8}{Input for lcs_variables, a EMERGENCY Level Livelihood Coping. Yes(1), No, didn't do (2), No, didn't do because strategy exhausted (3), or Not applicable (4)
#'   if the household used Livelihood Coping Strategy #8.}
#'   \item{lcs9}{Input for lcs_variables, a EMERGENCY Level Livelihood Coping. Yes(1), No, didn't do (2), No, didn't do because strategy exhausted (3), or Not applicable (4)
#'   if the household used Livelihood Coping Strategy #9.}
#'   \item{lcs10}{Input for lcs_variables, a EMERGENCY Level Livelihood Coping. Yes(1), No, didn't do (2), No, didn't do because strategy exhausted (3), or Not applicable (4)
#'   if the household used Livelihood Coping Strategy #10.}
#'   \item{KEY}{Unique household identifier.}
#'   \item{exp_comms}{Input for an estimated household expenditure on communications in the last 30 days.}
#'   \item{exp_food}{Input for an estimated household expenditure on food in the last 30 days.}
#'   \item{exp_water}{Input for an estimated household expenditure on water in the last 30 days.}
#'   \item{exp_fuel}{Input for an estimated household expenditure on fuel in the last 30 days.}
#'   \item{exp_nfi_monthly}{Input for an estimated household expenditure on montly non-food item costs in the last 30 days.}
#'   \item{exp_rent}{Input for an estimated household expenditure on rent in the last 30 days.}
#'   \item{exp_transport}{Input for an estimated household expenditure on transportation in the last 30 days.}
#'   \item{exp_utilities}{Input for an estimated household expenditure on utilities in the last 30 days.}
#'   \item{exp_other_monthly}{Input for an estimated household expenditure on other expenses in the last 30 days.}
#'   \item{exp_shelter}{Input for an estimated household expenditure on shelter in the last 6 months.}
#'   \item{exp_health}{Input for an estimated household expenditure on health in the last 6 months.}
#'   \item{exp_debt}{Input for an estimated household expenditure on debt in the last 6 months.}
#'   \item{exp_education}{Input for an estimated household expenditure on education costs in the last 6 months.}
#'   \item{exp_nfi_infrequent}{Input for an estimated household expenditure on infrequent non-food items in the last 6 months.}
#'   \item{exp_other_infrequent}{Input for an estimated household expenditure on other infrequent costs in the last 6 months.}
#'   \item{income_salaried}{Input for estimated income in last 30 days from SALARIED work}
#'   \item{income_casual}{Input for estimated income in last 30 days from CASUAL labor work}
#'   \item{income_trade}{Input for estimated income in last 30 days from TRADE}
#'   \item{income_livestock}{Input for estimated income in last 30 days from sale of LIVESTOCK or livestock products }
#'   \item{income_loans_family}{Input for estimated income in last 30 days from LOANS from friends and family}
#'   \item{income_loans_community}{Input for estimated income in last 30 days from LOANS from community members}
#'   \item{income_own_production}{Input for estimated income in last 30 days from OWN PRODUCTION or sale of crops}
#'   \item{income_remittances}{Input for estimated income in last 30 days from REMITTANCES}
#'   \item{income_rent}{Input for estimated income in last 30 days from RENT PAYMENTS}
#'   \item{income_social_benefits}{Input for estimated income in last 30 days from GOVT or SOCIAL BENEFIT PROGRAMS}
#'   \item{income_humanitarian_assistance}{Input for estimated income in last 30 days from HUMANITARIAN ASSISTANCE}
#'   \item{income_other}{Input for estimated income in last 30 days from OTHER SOURCES}
#'
#' }
#'
#' @source SMART Survey August 2019 + generated expense and income data.
#'
#' @examples
#' data(raw_fsl1)
"raw_fsl1"
