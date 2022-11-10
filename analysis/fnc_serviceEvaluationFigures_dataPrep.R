# fnc_serviceEvaluationFigures_dataPrep.R
#
# This script defines a function that prepares the submitted dataset for
# plotting. The function is called in the script entitled
# "serviceEvaluationFigures.R". The proportions being plotted are calculated
# using counts redacted if <=7 and then rounded to the nearest 5. This
# obfuscation process was required by OpenSAFELY.
# using counts rounded to the nearest five.
#

fnc_serviceEvaluationFigures_dataPrep <- function(data, start, end)
{
  # Requisites.
  relevant_preOperative_infection_status <-
  c("0-2 weeks record of pre-operative SARS-CoV-2 infection",
    "3-4 weeks record of pre-operative SARS-CoV-2 infection",
    "5-6 weeks record of pre-operative SARS-CoV-2 infection")
  
  # Make timeline backbone. 
  backbone <- 
    seq(lubridate::dmy(start), lubridate::dmy(end), by = 'month') %>%
    data.frame() %>%
    mutate(Year_surgery = lubridate::year(.),
           Month_surgery = lubridate::month(., label = T)
           ) %>%
    dplyr::select(-.)
  
  # Get monthly counts for the range of interest.
  monthly_windowed_proportion_within_7wkPreOpInfection <-
    data %>%
    dplyr::filter(Year_surgery >= 2020) %>%
    group_by(Year_surgery, Month_surgery) %>%
    dplyr::summarise(
      monthly_n = n(),
      monthly_within_7wk = sum(preOperative_infection_status %in%
                                 relevant_preOperative_infection_status)
    ) %>%
    dplyr::right_join(backbone, by = c("Year_surgery", "Month_surgery"))
  
  # Get rolling 6-monthly proportions of surgeries conducted in less than 7 weeks 
  # from a positive PCR test for SARS-CoV-2.
  # The 6-month window runs from the month in question backward by 6 months. For
  # example, the proportion shown for, say, August 2020 refers to the proportion
  # of surgeries since March 2020 that were conducted in less than 7 weeks from
  # a positive PCR test for SARS-CoV-2.
  windowed_proportion_within_7wkPreOpInfection <-
    monthly_windowed_proportion_within_7wkPreOpInfection %>%
    dplyr::group_by(Year_surgery, Month_surgery) %>%
    add_column(
      sixMonthly_n =
        RcppRoll::roll_sum(.$monthly_n, 6, fill=NA, align="right", na.rm = TRUE) %>%
        replace(., (. <= 7 & . > 0), NA) %>% `/`(10) %>% round()*10
    ) %>%
    add_column(
      sixMonthly_n_within_7wk = 
        RcppRoll::roll_sum(.$monthly_within_7wk,
                           6, fill=NA, align="right", na.rm = TRUE) %>%
        replace(., (. <= 7 & . > 0), NA) %>% `/`(10) %>% round()*10
    ) %>%
    dplyr::mutate(
      sixMonthly_prop_within_7wk = (sixMonthly_n_within_7wk / sixMonthly_n)*100
    ) %>%
    dplyr::select(-c("monthly_n", "monthly_within_7wk"))
  
  return(windowed_proportion_within_7wkPreOpInfection)
  # NOTE: 
  # LockeData's webpage was very useful for understanding RcppRoll's function
  # defaults - https://itsalocke.com/blog/understanding-rolling-calculations-in-r/
}