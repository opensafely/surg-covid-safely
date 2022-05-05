# preprintFigure.R
#
# This script produces plots of weekly, monthly, 2-monthly and 3-monthly
# summaries of the proportion of surgeries conducted more than 7 weeks from a 
# positive PCR test for SARS-CoV-2 (as per the data available in OpenSAFELY).
#
# Data for two cohorts are plotted: surgery patients without a cancer diagnosis
# within +-6 months of surgery, and for patient with a cancer diagnosis within
# +-6 months of surgery.
#
# The plot area is annotated with key dates:
#   1. March 2020 - The first publication by the COVIDSurg Collaborative that
#      raised concerns about the mortality of surgery patients.
#   2. 17th March 2020 - Publication of NICE guidelines suggesting a 3-month
#      delay to all planned surgeries.
#   3. Mid-January 2021 - Specifically, 8th Dec 2020 + 5 weeks. The 8th December
#      2020 was the first date of SARS-CoV-2 vaccination. We estimated at least
#      three weeks for sufficient vaccine rollout. We added two weeks for the 
#      vaccine to take effect, according to efficacy studies
#      (https://www.nejm.org/doi/full/10.1056/nejmoa2034577)
#   4. March 2021 - Another publication by the COVIDSurg Collaborative that
#      detailed the relationship between the 30-post opertative mortality and the
#      interval between a positive PCR test for SARS-CoV-2 and a patient's
#      surgery.
#
library(ggplot2)
library(RcppRoll)
library(tidyverse)
library(lubridate)


#####################
## Filter datasets ##
#####################
# ----
# Select only the variables needed for plotting.
myPlotData <- myData %>% select(c(
  date_surgery,
  category_cancer_within_6mths_surgery,
  postOp_mortality_30day,
  Week_surgery,
  Month_surgery,
  Year_surgery
))

# Filter the dataset for patients with a record of a cancer diagnosis within
# 6 months before or after their surgery.
# Naming convention is:
#   - OS = OpenSAFELY data
#   - C  = Cancer patient
#   - NC = Non-cancer patient
#
# OpenSAFELY data, cancer patients, within 6 months of surgery.
myData_OS_C6m <- myPlotData %>% 
  dplyr::filter(category_cancer_within_6mths_surgery == 
                  "Cancer diagnosis within 6mths before surgery" |
                  category_cancer_within_6mths_surgery == 
                  "Cancer diagnosis within 6mths after surgery")

# OpenSAFELY data, cancer patients, NOT within 6 months of surgery.
myData_OS_NC6m <- myPlotData %>%
  dplyr::filter(category_cancer_within_6mths_surgery != 
                  "Cancer diagnosis within 6mths before surgery" |
                  category_cancer_within_6mths_surgery != 
                  "Cancer diagnosis within 6mths after surgery")
# ----

#################################
## Calculate the data to plot. ##
#################################
# ----
# Make timeline backbone.
startDate = "01-03-2019"
endDate = "31-03-2022"
backbone <- 
  seq(lubridate::dmy(startDate), lubridate::dmy(endDate), by = 'week') %>%
  data.frame() %>%
  mutate(Year_surgery = lubridate::year(.),
         Month_surgery = lubridate::month(., label = T),
         Week_surgery = lubridate::week(.))# %>%
  #select(c(Year_surgery, Month_surgery, Week_surgery))

# Weekly counts of 30-day post-operative mortality.
weekly_windowed_postOp_mortality <-
  myData_OS_C6m %>%
    group_by(Year_surgery, Month_surgery, Week_surgery) %>%
    summarise(weekly_mortality = sum(postOp_mortality_30day=="Dead within 30-day post-operation"))

# Join to a left backbone of all weeks.
weekly_windowed_postOp_mortality <-
  backbone %>% dplyr::left_join(weekly_windowed_postOp_mortality,
                                by = c("Year_surgery", "Month_surgery", "Week_surgery")) %>%
  tidyr::replace_na(list("weekly_mortality" = 0))

# Group weekly count by month.
monthly_windowed_postOp_mortality <-
  weekly_windowed_postOp_mortality %>%
  select(-c(".","Week_surgery")) %>%
  group_by(Year_surgery, Month_surgery) %>%
  summarise(monthly_mortality = sum(weekly_mortality))

# Monthly, 2-monthly and 3-monthly counts of 30-day post-operative mortality.
monthly_windowed_postOp_mortality <-
  monthly_windowed_postOp_mortality %>%
  add_column(
            twoMonthly_mortality = RcppRoll::roll_sum(.$monthly_mortality,
                                                      2, fill=NA, align="right")
  ) %>%
  add_column(
            threeMonthly_mortality = RcppRoll::roll_sum(.$monthly_mortality,
                                                        3, fill=NA, align="right")
  )

# Join monthly counts to the weekly dataframe.
windowed_postOp_mortality <-
  weekly_windowed_postOp_mortality %>%
  dplyr::left_join(monthly_windowed_postOp_mortality,
                   by = c("Year_surgery", "Month_surgery"))
# NOTE: 
# LockeData's webpage was very useful for understanding RcppRoll's function
# defaults - https://itsalocke.com/blog/understanding-rolling-calculations-in-r/
# ----

###################
## Plot the data ##
###################
# ----


ggplot(windowed_postOp_mortality,
       aes(x = ., y = monthly_mortality)) +
  geom_line()
# ----

