# fnc_preprintFigure_dataPrep.R
#
# This script defines a function that prepares the submitted dataset for
# plotting. The function is called in the script entitled "preprintFigure.R".
#

fnc_preprintFigure_dataPrep <- function(data, start, end)
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
  
  weekly_windowed_proportion_7wkPreOpInfection <-
    data %>%
    group_by(Year_surgery, Month_surgery, Week_surgery) %>%
    summarise(
      weekly_7wk = sum(preOperative_infection_status %in%
                         relevant_preOperative_infection_status),
      weekly_n = n(),
      weekly_7wkPreOpInfection = weekly_7wk / weekly_n
    )
  
  # Join to a left backbone of all weeks.
  weekly_windowed_proportion_7wkPreOpInfection <-
    backbone %>% dplyr::left_join(weekly_windowed_proportion_7wkPreOpInfection,
                                  by = c("Year_surgery", "Month_surgery",
                                         "Week_surgery")) %>%
    tidyr::replace_na(list("weekly_7wkPreOpInfection" = 0, "weekly_7wk" = 0,
                           "weekly_n" = 0))
  
  # Group weekly count by month.
  monthly_windowed_proportion_7wkPreOpInfection <-
    weekly_windowed_proportion_7wkPreOpInfection %>%
    group_by(Year_surgery, Month_surgery) %>%
    summarise(
      monthly_7wk = sum(weekly_7wk),
      monthly_n = sum(weekly_n),
      monthly_7wkPreOpInfection = monthly_7wk / monthly_n
    ) %>% 
    tidyr::replace_na(list("monthly_7wkPreOpInfection" = 0))
  
  # Monthly, 2-monthly and 3-monthly counts of 30-day post-operative mortality.
  monthly_windowed_proportion_7wkPreOpInfection <-
    monthly_windowed_proportion_7wkPreOpInfection %>%
    add_column(
      twoMonthly_7wk =
        RcppRoll::roll_sum(.$monthly_7wk,
                           2, fill=NA, align="right")
    ) %>%
    add_column(
      twoMonthly_n =
        RcppRoll::roll_sum(.$monthly_n,
                           2, fill=NA, align="right")
    ) %>%
    add_column(
      threeMonthly_7wk =
        RcppRoll::roll_sum(.$monthly_7wk,
                           3, fill=NA, align="right")
    ) %>%
    add_column(
      threeMonthly_n =
        RcppRoll::roll_sum(.$monthly_n,
                           3, fill=NA, align="right")
    ) %>%
    mutate(
      twoMonthly_7wkPreOpInfection = twoMonthly_7wk / twoMonthly_n
    ) %>% 
    mutate(
      threeMonthly_7wkPreOpInfection = threeMonthly_7wk / threeMonthly_n
    )
  
  # Join monthly to the weekly dataframe.
  windowed_proportion_7wkPreOpInfection <-
    monthly_windowed_proportion_7wkPreOpInfection %>%
    select(c("Year_surgery", "Month_surgery",
             "monthly_7wkPreOpInfection",
             "twoMonthly_7wkPreOpInfection",
             "threeMonthly_7wkPreOpInfection")) %>%
    dplyr::right_join(weekly_windowed_proportion_7wkPreOpInfection,
                      by = c("Year_surgery", "Month_surgery")) %>%
    select(c("Year_surgery", "Month_surgery", "Week_surgery",
             "weekly_7wkPreOpInfection",
             "monthly_7wkPreOpInfection",
             "twoMonthly_7wkPreOpInfection",
             "threeMonthly_7wkPreOpInfection"))
  # NOTE: 
  # LockeData's webpage was very useful for understanding RcppRoll's function
  # defaults - https://itsalocke.com/blog/understanding-rolling-calculations-in-r/
}