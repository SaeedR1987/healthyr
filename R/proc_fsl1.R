
#' SMART Survey Food Security Data - Processed
#'
#' A dataset containing household food security and livelihood coping strategy data from a SMART Survey.
#' Data has been processed by the healthyr functions.
#'
#' @docType data
#'
#' @usage data(proc_fsl1)
#'
#' @format A data frame with 509 rows and 47 variables:
#' \describe{
#'   \item{today}{Date of data collection}
#'   \item{enum}{The survey team or enumerator ID.}
#'   \item{cluster}{The cluster id.}
#'   \item{fcs_cereal}{Number of days cereals, grains, roots and tubers consumed in the past 7 days. }
#'   \item{hdds_cereals}{Yes(1)/No(0) if the household consumed cereals or grains in the previous day.}
#'   \item{hdds_tubers}{Yes(1)/No(0) if the household consumed roots or tubers in the previous day.}
#'   \item{fcs_legumes}{Number of days pulses, legumes, nuts consumed in the past 7 days.}
#'   \item{hdds_legumes}{Yes(1)/No(0) if the household consumed pulses, legumes or nuts in the previous day.}
#'   \item{fcs_dairy}{Number of days milk or dairy products consumed in the past 7 days.}
#'   \item{hdds_dairy}{Yes(1)/No(0) if the household consumed milk or dairy products in the previous day.}
#'   \item{fcs_meat}{Number of days meats, fish or eggs consumed in the past 7 days.}
#'   \item{F041B}{Contributes to input for hdds_meats, Yes(1)/No(0) but reports only if consumed flesh meats in the previous day.}
#'   \item{F042B}{Contributes to input for hdds_meats, Yes(1)/No(0) but reports only if consumed organ meats in the previous day.}
#'   \item{hdds_fish}{Yes(1)/No(0) if the household consumed fish in the previous day.}
#'   \item{hdds_eggs}{Yes(1)/No(0) if the household consumed eggs in the previous day.}
#'   \item{fcs_veg}{Number of days vegetables and leaves consumed in the past 7 days.}
#'   \item{hdds_veg}{Yes(1)/No(0) if the household consumed vegetables or leaves the previous day.}
#'   \item{fcs_fruit}{Number of days fruits consumed in the past 7 days.}
#'   \item{hdds_fruit}{Yes(1)/No(0) if the household consumed fruits the previous day.}
#'   \item{fcs_oil}{Number of days oil, fat or butter consumed in the past 7 days.}
#'   \item{hdds_oils}{Yes(1)/No(0) if the household consumed oil, fat or butter in the previous day.}
#'   \item{fcs_sugar}{Number of days sugar or sweets consumed in the past 7 days.}
#'   \item{hdds_sugars}{Yes(1)/No(0) if the household consumed sugar or sweets in the previous day.}
#'   \item{hdds_condiments}{Yes(1)/No(0) if the household consumed condiments such as salt, spices, tea, coffee etc. in the previous day.}
#'   \item{hhs_nofoodhh_1}{Yes(1)/No(0) if the household experienced having no food at all in the household in the past 30 days.}
#'   \item{hhs_nofoodhh_1a}{Rarely 1-2 times(1), Sometimes 3-10 times(2), or Often 10+ times (3) that the household experienced
#'   having no food at all in the household in the past 30 days.}
#'   \item{hhs_sleephungry_2}{Yes(1)/No(0) if anyone in the household experienced going to bed hungry because of not having enough food
#'   in the past 30 days.}
#'   \item{hhs_sleephungry_2a}{Rarely 1-2 times(1), Sometimes 3-10 times(2), or Often 10+ times (3) that anyone one in the household
#'   experienced going to bed hungry because of not having enough food in the past 30 days.}
#'   \item{hhs_alldaynight_3}{Yes(1)/No(0) if anyone in the household experienced not eating anything all the previous
#'   day and night in the past 30 days.}
#'   \item{hhs_alldaynight_3a}{Rarely 1-2 times(1), Sometimes 3-10 times(2), or Often 10+ times (3) that anyone one in the household
#'   experienced not eating anything all day and night in the past 30 days}
#'   \item{rcsi_lesspreferred_1}{Number of days household reported consuming less preferred foods in the past 7 days.}
#'   \item{rcsi_borrowfood_2}{Number of days household reported borrowing foods in the past 7 days.}
#'   \item{rcsi_limitportion_3}{Number of days household reported limiting portion size in the past 7 days.}
#'   \item{rcsi_restrict_4}{Number of days household reported restricting meals for adults so children could eat in the past 7 days.}
#'   \item{rcsi_reducemeals5}{Number of days household reported reducing the number of meals consumed in the past 7 days.}
#'   \item{lcs1}{A STRESS Level Livelihood Coping. Yes(1), No, didn't do (2), No, didn't do because strategy exhausted (3), or Not applicable (4)
#'   if the household used Livelihood Coping Strategy #1.}
#'   \item{lcs2}{A STRESS Level Livelihood Coping. Yes(1), No, didn't do (2), No, didn't do because strategy exhausted (3), or Not applicable (4)
#'   if the household used Livelihood Coping Strategy #2.}
#'   \item{lcs3}{A STRESS Level Livelihood Coping. Yes(1), No, didn't do (2), No, didn't do because strategy exhausted (3), or Not applicable (4)
#'   if the household used Livelihood Coping Strategy #3.}
#'   \item{lcs4}{A CRISIS Level Livelihood Coping. Yes(1), No, didn't do (2), No, didn't do because strategy exhausted (3), or Not applicable (4)
#'   if the household used Livelihood Coping Strategy #4.}
#'   \item{lcs5}{A STRESS Level Livelihood Coping. Yes(1), No, didn't do (2), No, didn't do because strategy exhausted (3), or Not applicable (4)
#'   if the household used Livelihood Coping Strategy #5.}
#'   \item{lcs6}{A CRISIS Level Livelihood Coping. Yes(1), No, didn't do (2), No, didn't do because strategy exhausted (3), or Not applicable (4)
#'   if the household used Livelihood Coping Strategy #6.}
#'   \item{lcs7}{A CRISIS Level Livelihood Coping. Yes(1), No, didn't do (2), No, didn't do because strategy exhausted (3), or Not applicable (4)
#'   if the household used Livelihood Coping Strategy #7.}
#'   \item{lcs8}{An EMERGENCY Level Livelihood Coping. Yes(1), No, didn't do (2), No, didn't do because strategy exhausted (3), or Not applicable (4)
#'   if the household used Livelihood Coping Strategy #8.}
#'   \item{lcs9}{An EMERGENCY Level Livelihood Coping. Yes(1), No, didn't do (2), No, didn't do because strategy exhausted (3), or Not applicable (4)
#'   if the household used Livelihood Coping Strategy #9.}
#'   \item{lcs10}{An EMERGENCY Level Livelihood Coping. Yes(1), No, didn't do (2), No, didn't do because strategy exhausted (3), or Not applicable (4)
#'   if the household used Livelihood Coping Strategy #10.}
#'   \item{KEY}{Unique household identifier.}
#'   \item{hdds_meat}{Input for hdds_meats. Yes(1)/No(0) combines F041B and F042B if either consumed in the previous day.}
#'   \item{lcs_stress}{Yes(1)/No(0) if the household used or exhausted at least one STRESS level livelihood coping strategy.}
#'   \item{lcs_crisis}{Yes(1)/No(0) if the household used or exhausted at least one CRISIS level livelihood coping strategy.}
#'   \item{lcs_emergency}{Yes(1)/No(0) if the household used or exhausted at least one EMERGENCY level livelihood coping strategy.}
#'   \item{lcs_other}{Yes(1)/No(0) if the household used or exhausted any other livelihood coping strategy that is not categorized
#'   as Stress, Crisis or Emergency severity.}
#'   \item{fcs_weight_cereal1}{Weighted value for Food Consumption Score. fcs_cereals x 2}
#'   \item{fcs_weight_legume2}{Weighted value for Food Consumption Score. fcs_legumes x 3}
#'   \item{fcs_weight_dairy3}{Weighted value for Food Consumption Score. fcs_dairy x 4}
#'   \item{fcs_weight_meat4}{Weighted value for Food Consumption Score. fcs_meat x 4}
#'   \item{fcs_weight_veg5}{Weighted value for Food Consumption Score. fcs_veg x 1}
#'   \item{fcs_weight_fruit6}{Weighted value for Food Consumption Score. fcs_fruit x 1}
#'   \item{fcs_weight_oil7}{Weighted value for Food Consumption Score. fcs_oil x 0.5}
#'   \item{fcs_weight_sugar8}{Weighted value for Food Consumption Score. fcs_sugar x 0.5}
#'   \item{fcs_score}{Food Consumption Score value. Sum of fcs_weight variables.}
#'   \item{fcs_cat}{Food Consumption Score categorization. May be reported with normal or alternative thresholds.
#'   Normal thresholds are Poor (<21), Borderline (>=21 & <35), and Acceptable (>=35). Alternative thresholds may
#'   be used if high oil and sugar consumption is observed, and are Poor (<28), Borderline (>=28 & <42), and
#'   Acceptable (>=42).}
#'   \item{hdds1}{Yes(1)/No(0) if the household consumed cereals or grains in the previous day.}
#'   \item{hdds2}{Yes(1)/No(0) if the household consumed roots or tubers in the previous day.}
#'   \item{hdds3}{Yes(1)/No(0) if the household consumed vegetables or leaves the previous day.}
#'   \item{hdds4}{Yes(1)/No(0) if the household consumed fruits the previous day.}
#'   \item{hdds5}{Yes(1)/No(0) if the household consumed meats the previous day.}
#'   \item{hdds6}{Yes(1)/No(0) if the household consumed eggs in the previous day.}
#'   \item{hdds7}{Yes(1)/No(0) if the household consumed fish in the previous day.}
#'   \item{hdds8}{Yes(1)/No(0) if the household consumed pulses, legumes or nuts in the previous day.}
#'   \item{hdds9}{Yes(1)/No(0) if the household consumed milk or dairy products in the previous day.}
#'   \item{hdds10}{Yes(1)/No(0) if the household consumed oil, fat or butter in the previous day.}
#'   \item{hdds11}{Yes(1)/No(0) if the household consumed sugar or sweets in the previous day.}
#'   \item{hdds12}{Yes(1)/No(0) if the household consumed condiments such as salt, spices, tea, coffee etc.
#'   in the previous day.}
#'   \item{hdds_score}{Household Dietary Diversity Score (HDDS), Count of all HDDS food groups reported consumed the previous day (0-12).}
#'   \item{hdds_cat}{Household Dietary Diversity Score (HDDS) categorizatoin as Low (<3), Medium (>=3 & <4), or High (>=4) }
#'   \item{hhs_comp1}{Weighted Household Hunger Scale (HHS) composite score (0-2) for HHS experience of no food in the house at all.}
#'   \item{hhs_comp2}{Weighted Household Hunger Scale (HHS) composite score (0-2) for HHS experience of going to sleep hungry.}
#'   \item{hhs_comp3}{Weighted Household Hunger Scale (HHS) composite score (0-2) for HHS experience of eathing nothing a whole day and night.}
#'   \item{hhs_score}{Household Hunger Scale (HHS) score (0-6).}
#'   \item{hhs_cat}{Household Hunger Scale (HHS) categorization as None (0), Little (1), Moderate (2-3), Severe (4), or Very Severe (5-6).}
#'   \item{rcsi_weight1}{Weighted reduced Coping Strategy Index (rCSI) score for less preferred foods. rcsi_lesspreferred_1 x 1}
#'   \item{rcsi_weight2}{Weighted reduced Coping Strategy Index (rCSI) score for borrowing foods. rcsi_borrowfood_2 x 2}
#'   \item{rcsi_weight3}{Weighted reduced Coping Strategy Index (rCSI) score for limiting portion size. rcsi_limitportion_3 x 1}
#'   \item{rcsi_weight4}{Weighted reduced Coping Strategy Index (rCSI) score for restrict consumption of adults. rcsi_restrict_4 x 3}
#'   \item{rcsi_weight5}{Weighted reduced Coping Strategy Index (rCSI) score for borrowing foods. rcsi_reducemeals5 x 1}
#'   \item{rcsi_score}{reduced Coping Strategy Index (rCSI) score ()}
#'   \item{rcsi_cat}{reduced Coping Strategy Index (rCSI) categorization, as None to Low (<=3), Medium (>3 & <=18), or Severe (>18).}
#'   \item{lcs_cat}{Livelihood Coping Strategies categorization as None, Stress, Crisis, or Emergency.}
#'   \item{fc_cell}{FEWSNET Matrix cell value, calculated from fcs_cat, hhs_cat, and rcsi_cat. Values range from 1-45}
#'   \item{fc_phase}{FEWSNET Matrix food consumption severity phase classification, based on fc_cell value. Can range from phase 1 to 5.}
#'   \item{fclc_phase}{FEWSNET Matrix food consumption + livelihood coping severity phase classification, based on fc_cell and lcs_cat values. Can range from phase 1 to 5.}
#'   \item{flag_above7_fcs}{Data quality flag if any FCS questions that report greater than 7 days food consumption in the past 7 days.}
#'   \item{flag_meat_cereal_ratio}{Data quality flag if more days of meat consumption are reported than days of cereal consumption in the past 7 days.}
#'   \item{flag_zero_fcs}{Data quality flag if FCS score of 0.}
#'   \item{flag_all_fcs}{Data quality flag if all FCS values are 0 or 7.}
#'   \item{flag_low_fcs}{Data quality flag if FCS score < 5 is reported.}
#'   \item{flag_high_fcs}{Data quality flag if FCS score > 60 is reported.}
#'   \item{flag_fcs_extreme}{Data quality flag if an extreme FCS score of <3 or >60 is reported.}
#'   \item{flag_hhs_nofoodhh_noeat}{Data quality flag if the most severe HHS experience is reported, but not the least severe HHS experience.}
#'   \item{flag_severe_hhs}{Data quality flag if Very Severe HHS score is reported. This is not necessarily an error, but if a large proportion
#'   of households are reported as Very Severe, it may indicate a mistake by the enumerators.}
#'   \item{flag_low_hdds}{Data quality flag if low HDDS score <2 is reported.}
#'   \item{flag_high_hdds}{Data quality flag if high HDDS score >9 is reported.}
#'   \item{flag_low_sugar_oil_hdds}{Data quality flag if low HDDS is reported (<=2) but sugars and condiments are reported.}
#'   \item{flag_high_rcsi}{Data quality flag if rCSI score is >= 50}
#'   \item{flag_lcs_severity}{Data quality flag if a household reported to use or have exhausted emergency coping strategies,
#'   but not any stress or crisis coping strategies.}
#'   \item{flag_fc_cell}{Data quality flag if FEWSNET Matrix cells 3,4,5,8,9 or 10 are reported. These cells reflect implausible combinations
#'   reflective of low rCSI scores, high FCS scores, and high HHS scores.}
#'
#' }
#'
#' @source SMART Survey August 2019.
#'
#' @examples
#' data(proc_fsl1)
"proc_fsl1"


