# fnc_serviceEvaluationFigures_dataPrep.R
#
# This script defines a function that prepares the submitted dataset for
# plotting. The function is called in the script entitled
# "serviceEvaluationFigures.R".
#

fnc_serviceEvaluationFigures_dataPrep <- function(data, start, end)
{
  # Make timeline backbone. 
  backbone <- 
    seq(lubridate::dmy(start), lubridate::dmy(end), by = 'week') %>%
    data.frame() %>%
    mutate(Year_surgery = lubridate::year(.),
           Month_surgery = lubridate::month(., label = T),
           Week_surgery = lubridate::week(.))
  colnames(backbone)[1] <- "Date"
  
  # Weekly proportion of surgeries conducted more than 7 weeks from a positive
  # PCR test for SARS-CoV-2.
  relevant_preOperative_infection_status <-
    c("0-2 weeks record of pre-operative SARS-CoV-2 infection",
      "3-4 weeks record of pre-operative SARS-CoV-2 infection",
      "5-6 weeks record of pre-operative SARS-CoV-2 infection")
  
  weekly_windowed_proportion_within_7wkPreOpInfection <-
    data %>%
    group_by(Year_surgery, Month_surgery, Week_surgery) %>%
    summarise(
      weekly_n = n(),
      weekly_n_within_7wk = sum(preOperative_infection_status %in%
                                  relevant_preOperative_infection_status),
      weekly_prop_within_7wk = weekly_n_within_7wk / weekly_n
    )
  
  # Join to a left backbone of all weeks.
  weekly_windowed_proportion_within_7wkPreOpInfection <-
    backbone %>% dplyr::left_join(weekly_windowed_proportion_within_7wkPreOpInfection,
                                  by = c("Year_surgery", "Month_surgery",
                                         "Week_surgery")) %>%
    tidyr::replace_na(list("weekly_prop_within_7wk" = 0, "weekly_n_within_7wk" = 0,
                           "weekly_n" = 0))
  
  # Group weekly count by month.
  monthly_windowed_proportion_within_7wkPreOpInfection <-
    weekly_windowed_proportion_within_7wkPreOpInfection %>%
    group_by(Year_surgery, Month_surgery) %>%
    summarise(
      monthly_n = sum(weekly_n),
      monthly_within_7wk = sum(weekly_n_within_7wk),
      monthly_prop_within_7wk = monthly_within_7wk / monthly_n
    ) %>% 
    tidyr::replace_na(list("monthly_prop_within_7wk" = 0))
  
  # Monthly, 2-monthly and 3-monthly counts of 30-day post-operative mortality.
  monthly_windowed_proportion_within_7wkPreOpInfection <-
    monthly_windowed_proportion_within_7wkPreOpInfection %>%
    add_column(
      twoMonthly_n =
        RcppRoll::roll_sum(.$monthly_n,
                           2, fill=NA, align="right")
    ) %>%
    add_column(
      twoMonthly_within_7wk =
        RcppRoll::roll_sum(.$monthly_within_7wk,
                           2, fill=NA, align="right")
    ) %>%
    mutate(
        twoMonthly_prop_within_7wk = twoMonthly_within_7wk / twoMonthly_n
    ) %>%
    add_column(
      threeMonthly_n =
        RcppRoll::roll_sum(.$monthly_n,
                           3, fill=NA, align="right")
    ) %>%    
    add_column(
      threeMonthly_within_7wk =
        RcppRoll::roll_sum(.$monthly_within_7wk,
                           3, fill=NA, align="right")
    ) %>% 
    mutate(
      threeMonthly_prop_within_7wk = threeMonthly_within_7wk / threeMonthly_n
    )
  
  # Join monthly to the weekly dataframe.
  windowed_proportion_7wkPreOpInfection <-
    weekly_windowed_proportion_within_7wkPreOpInfection %>%
    select("Year_surgery", "Month_surgery", "Week_surgery",
           "weekly_n", "weekly_n_within_7wk", "weekly_prop_within_7wk"
    ) %>%
    dplyr::left_join(monthly_windowed_proportion_within_7wkPreOpInfection,
                      by = c("Year_surgery", "Month_surgery"))
  
  # Convert to percentages.
  proportion_columns <- c("weekly_prop_within_7wk",
                          "monthly_prop_within_7wk",
                          "twoMonthly_prop_within_7wk",
                          "threeMonthly_prop_within_7wk")
  windowed_proportion_7wkPreOpInfection[,proportion_columns] <- 
    windowed_proportion_7wkPreOpInfection[,proportion_columns] * 100
  
  return(windowed_proportion_7wkPreOpInfection)
  # NOTE: 
  # LockeData's webpage was very useful for understanding RcppRoll's function
  # defaults - https://itsalocke.com/blog/understanding-rolling-calculations-in-r/
}