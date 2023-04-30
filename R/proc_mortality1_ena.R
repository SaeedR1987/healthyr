#' SMART Survey Processed Mortality Dataset in ENA format.
#'
#' A dataset containing demographic and mortality data from a SMART Survey that has been
#' transformed into an ENA compatible format.
#'
#' @format A data frame with 3780 rows and 47 variables:
#' \describe{
#'   \item{date_dc_char}{Unique record level identifier}
#'   \item{cluster}{Unique household survey identifier}
#'   \item{date_dc}{Date of data collection, the original input into healthyr function.}
#'   \item{enum}{Date of recall event associated with mortality survey, the original input into healthyr function}
#'   \item{hh_id}{The survey team or enumerator ID}
#'   \item{_sex}{Sex of the individual}
#'   \item{_age_years}{Age of the individual in years}
#'   \item{_join}{Yes (1)/No(NA) if the person joined the household after the recall event}
#'   \item{_left}{Yes (1)/No(NA) if the individual left the household after the recall event}
#'   \item{_birth}{Yes (1)/No(NA) if the child was born into the household after the recall event}
#'   \item{_death}{Yes (1)/No(NA) if the individual died since the recall event}
#'   \item{_death_cause_smart}{Categorical response for cause of death coded in line with ENA.}
#'   \item{_death_location_smart}{Categorical response for location of death coded in line with ENA.}
#'   \item{1_sex}{Sex of the individual}
#'   \item{1_age_years}{Age of the individual in years}
#'   \item{1_join}{Yes (1)/No(NA) if the person joined the household after the recall event}
#'   \item{1_left}{Yes (1)/No(NA) if the individual left the household after the recall event}
#'   \item{1_birth}{Yes (1)/No(NA) if the child was born into the household after the recall event}
#'   \item{1_death}{Yes (1)/No(NA) if the individual died since the recall event}
#'   \item{1_death_cause_smart}{Categorical response for cause of death coded in line with ENA.}
#'   \item{1_death_location_smart}{Categorical response for location of death coded in line with ENA.}
#'   \item{2_sex}{Sex of the individual}
#'   \item{2_age_years}{Age of the individual in years}
#'   \item{2_join}{Yes (1)/No(NA) if the person joined the household after the recall event}
#'   \item{2_left}{Yes (1)/No(NA) if the individual left the household after the recall event}
#'   \item{2_birth}{Yes (1)/No(NA) if the child was born into the household after the recall event}
#'   \item{2_death}{Yes (1)/No(NA) if the individual died since the recall event}
#'   \item{2_death_cause_smart}{Categorical response for cause of death coded in line with ENA.}
#'   \item{2_death_location_smart}{Categorical response for location of death coded in line with ENA.}
#'   \item{3_sex}{Sex of the individual}
#'   \item{3_age_years}{Age of the individual in years}
#'   \item{3_join}{Yes (1)/No(NA) if the person joined the household after the recall event}
#'   \item{3_left}{Yes (1)/No(NA) if the individual left the household after the recall event}
#'   \item{3_birth}{Yes (1)/No(NA) if the child was born into the household after the recall event}
#'   \item{3_death}{Yes (1)/No(NA) if the individual died since the recall event}
#'   \item{3_death_cause_smart}{Categorical response for cause of death coded in line with ENA.}
#'   \item{3_death_location_smart}{Categorical response for location of death coded in line with ENA.}
#'   \item{4_sex}{Sex of the individual}
#'   \item{4_age_years}{Age of the individual in years}
#'   \item{4_join}{Yes (1)/No(NA) if the person joined the household after the recall event}
#'   \item{4_left}{Yes (1)/No(NA) if the individual left the household after the recall event}
#'   \item{4_birth}{Yes (1)/No(NA) if the child was born into the household after the recall event}
#'   \item{4_death}{Yes (1)/No(NA) if the individual died since the recall event}
#'   \item{4_death_cause_smart}{Categorical response for cause of death coded in line with ENA.}
#'   \item{4_death_location_smart}{Categorical response for location of death coded in line with ENA.}
#'   \item{5_sex}{Sex of the individual}
#'   \item{5_age_years}{Age of the individual in years}
#'   \item{5_join}{Yes (1)/No(NA) if the person joined the household after the recall event}
#'   \item{5_left}{Yes (1)/No(NA) if the individual left the household after the recall event}
#'   \item{5_birth}{Yes (1)/No(NA) if the child was born into the household after the recall event}
#'   \item{5_death}{Yes (1)/No(NA) if the individual died since the recall event}
#'   \item{5_death_cause_smart}{Categorical response for cause of death coded in line with ENA.}
#'   \item{5_death_location_smart}{Categorical response for location of death coded in line with ENA.}
#'   \item{6_sex}{Sex of the individual}
#'   \item{6_age_years}{Age of the individual in years}
#'   \item{6_join}{Yes (1)/No(NA) if the person joined the household after the recall event}
#'   \item{6_left}{Yes (1)/No(NA) if the individual left the household after the recall event}
#'   \item{6_birth}{Yes (1)/No(NA) if the child was born into the household after the recall event}
#'   \item{6_death}{Yes (1)/No(NA) if the individual died since the recall event}
#'   \item{6_death_cause_smart}{Categorical response for cause of death coded in line with ENA.}
#'   \item{6_death_location_smart}{Categorical response for location of death coded in line with ENA.}
#'   \item{7_sex}{Sex of the individual}
#'   \item{7_age_years}{Age of the individual in years}
#'   \item{7_join}{Yes (1)/No(NA) if the person joined the household after the recall event}
#'   \item{7_left}{Yes (1)/No(NA) if the individual left the household after the recall event}
#'   \item{7_birth}{Yes (1)/No(NA) if the child was born into the household after the recall event}
#'   \item{7_death}{Yes (1)/No(NA) if the individual died since the recall event}
#'   \item{7_death_cause_smart}{Categorical response for cause of death coded in line with ENA.}
#'   \item{7_death_location_smart}{Categorical response for location of death coded in line with ENA.}
#'   \item{8_sex}{Sex of the individual}
#'   \item{8_age_years}{Age of the individual in years}
#'   \item{8_join}{Yes (1)/No(NA) if the person joined the household after the recall event}
#'   \item{8_left}{Yes (1)/No(NA) if the individual left the household after the recall event}
#'   \item{8_birth}{Yes (1)/No(NA) if the child was born into the household after the recall event}
#'   \item{8_death}{Yes (1)/No(NA) if the individual died since the recall event}
#'   \item{8_death_cause_smart}{Categorical response for cause of death coded in line with ENA.}
#'   \item{8_death_location_smart}{Categorical response for location of death coded in line with ENA.}
#'   \item{9_sex}{Sex of the individual}
#'   \item{9_age_years}{Age of the individual in years}
#'   \item{9_join}{Yes (1)/No(NA) if the person joined the household after the recall event}
#'   \item{9_left}{Yes (1)/No(NA) if the individual left the household after the recall event}
#'   \item{9_birth}{Yes (1)/No(NA) if the child was born into the household after the recall event}
#'   \item{9_death}{Yes (1)/No(NA) if the individual died since the recall event}
#'   \item{9_death_cause_smart}{Categorical response for cause of death coded in line with ENA.}
#'   \item{9_death_location_smart}{Categorical response for location of death coded in line with ENA.}
#'   \item{10_sex}{Sex of the individual}
#'   \item{10_age_years}{Age of the individual in years}
#'   \item{10_join}{Yes (1)/No(NA) if the person joined the household after the recall event}
#'   \item{10_left}{Yes (1)/No(NA) if the individual left the household after the recall event}
#'   \item{10_birth}{Yes (1)/No(NA) if the child was born into the household after the recall event}
#'   \item{10_death}{Yes (1)/No(NA) if the individual died since the recall event}
#'   \item{10_death_cause_smart}{Categorical response for cause of death coded in line with ENA.}
#'   \item{10_death_location_smart}{Categorical response for location of death coded in line with ENA.}
#'   \item{11_sex}{Sex of the individual}
#'   \item{11_age_years}{Age of the individual in years}
#'   \item{11_join}{Yes (1)/No(NA) if the person joined the household after the recall event}
#'   \item{11_left}{Yes (1)/No(NA) if the individual left the household after the recall event}
#'   \item{11_birth}{Yes (1)/No(NA) if the child was born into the household after the recall event}
#'   \item{11_death}{Yes (1)/No(NA) if the individual died since the recall event}
#'   \item{11_death_cause_smart}{Categorical response for cause of death coded in line with ENA.}
#'   \item{11_death_location_smart}{Categorical response for location of death coded in line with ENA.}
#'   \item{12_sex}{Sex of the individual}
#'   \item{12_age_years}{Age of the individual in years}
#'   \item{12_join}{Yes (1)/No(NA) if the person joined the household after the recall event}
#'   \item{12_left}{Yes (1)/No(NA) if the individual left the household after the recall event}
#'   \item{12_birth}{Yes (1)/No(NA) if the child was born into the household after the recall event}
#'   \item{12_death}{Yes (1)/No(NA) if the individual died since the recall event}
#'   \item{12_death_cause_smart}{Categorical response for cause of death coded in line with ENA.}
#'   \item{12_death_location_smart}{Categorical response for location of death coded in line with ENA.}
#'   \item{13_sex}{Sex of the individual}
#'   \item{13_age_years}{Age of the individual in years}
#'   \item{13_join}{Yes (1)/No(NA) if the person joined the household after the recall event}
#'   \item{13_left}{Yes (1)/No(NA) if the individual left the household after the recall event}
#'   \item{13_birth}{Yes (1)/No(NA) if the child was born into the household after the recall event}
#'   \item{13_death}{Yes (1)/No(NA) if the individual died since the recall event}
#'   \item{13_death_cause_smart}{Categorical response for cause of death coded in line with ENA.}
#'   \item{13_death_location_smart}{Categorical response for location of death coded in line with ENA.}
#'   \item{14_sex}{Sex of the individual}
#'   \item{14_age_years}{Age of the individual in years}
#'   \item{14_join}{Yes (1)/No(NA) if the person joined the household after the recall event}
#'   \item{14_left}{Yes (1)/No(NA) if the individual left the household after the recall event}
#'   \item{14_birth}{Yes (1)/No(NA) if the child was born into the household after the recall event}
#'   \item{14_death}{Yes (1)/No(NA) if the individual died since the recall event}
#'   \item{14_death_cause_smart}{Categorical response for cause of death coded in line with ENA.}
#'   \item{14_death_location_smart}{Categorical response for location of death coded in line with ENA.}
#'   \item{15_sex}{Sex of the individual}
#'   \item{15_age_years}{Age of the individual in years}
#'   \item{15_join}{Yes (1)/No(NA) if the person joined the household after the recall event}
#'   \item{15_left}{Yes (1)/No(NA) if the individual left the household after the recall event}
#'   \item{15_birth}{Yes (1)/No(NA) if the child was born into the household after the recall event}
#'   \item{15_death}{Yes (1)/No(NA) if the individual died since the recall event}
#'   \item{15_death_cause_smart}{Categorical response for cause of death coded in line with ENA.}
#'   \item{15_death_location_smart}{Categorical response for location of death coded in line with ENA.}
#'   \item{16_sex}{Sex of the individual}
#'   \item{16_age_years}{Age of the individual in years}
#'   \item{16_join}{Yes (1)/No(NA) if the person joined the household after the recall event}
#'   \item{16_left}{Yes (1)/No(NA) if the individual left the household after the recall event}
#'   \item{16_birth}{Yes (1)/No(NA) if the child was born into the household after the recall event}
#'   \item{16_death}{Yes (1)/No(NA) if the individual died since the recall event}
#'   \item{16_death_cause_smart}{Categorical response for cause of death coded in line with ENA.}
#'   \item{16_death_location_smart}{Categorical response for location of death coded in line with ENA.}
#'   \item{17_sex}{Sex of the individual}
#'   \item{17_age_years}{Age of the individual in years}
#'   \item{17_join}{Yes (1)/No(NA) if the person joined the household after the recall event}
#'   \item{17_left}{Yes (1)/No(NA) if the individual left the household after the recall event}
#'   \item{17_birth}{Yes (1)/No(NA) if the child was born into the household after the recall event}
#'   \item{17_death}{Yes (1)/No(NA) if the individual died since the recall event}
#'   \item{17_death_cause_smart}{Categorical response for cause of death coded in line with ENA.}
#'   \item{17_death_location_smart}{Categorical response for location of death coded in line with ENA.}
#'   \item{18_sex}{Sex of the individual}
#'   \item{18_age_years}{Age of the individual in years}
#'   \item{18_join}{Yes (1)/No(NA) if the person joined the household after the recall event}
#'   \item{18_left}{Yes (1)/No(NA) if the individual left the household after the recall event}
#'   \item{18_birth}{Yes (1)/No(NA) if the child was born into the household after the recall event}
#'   \item{18_death}{Yes (1)/No(NA) if the individual died since the recall event}
#'   \item{18_death_cause_smart}{Categorical response for cause of death coded in line with ENA.}
#'   \item{18_death_location_smart}{Categorical response for location of death coded in line with ENA.}
#'   \item{19_sex}{Sex of the individual}
#'   \item{19_age_years}{Age of the individual in years}
#'   \item{19_join}{Yes (1)/No(NA) if the person joined the household after the recall event}
#'   \item{19_left}{Yes (1)/No(NA) if the individual left the household after the recall event}
#'   \item{19_birth}{Yes (1)/No(NA) if the child was born into the household after the recall event}
#'   \item{19_death}{Yes (1)/No(NA) if the individual died since the recall event}
#'   \item{19_death_cause_smart}{Categorical response for cause of death coded in line with ENA.}
#'   \item{19_death_location_smart}{Categorical response for location of death coded in line with ENA.}
#'   \item{20_sex}{Sex of the individual}
#'   \item{20_age_years}{Age of the individual in years}
#'   \item{20_join}{Yes (1)/No(NA) if the person joined the household after the recall event}
#'   \item{20_left}{Yes (1)/No(NA) if the individual left the household after the recall event}
#'   \item{20_birth}{Yes (1)/No(NA) if the child was born into the household after the recall event}
#'   \item{20_death}{Yes (1)/No(NA) if the individual died since the recall event}
#'   \item{20_death_cause_smart}{Categorical response for cause of death coded in line with ENA.}
#'   \item{20_death_location_smart}{Categorical response for location of death coded in line with ENA.}
#'
#'   }
#'
#' @source SMART Survey August 2019
#'
#' @examples
#' data(proc_mortality1)
"proc_mortality1_ena"

