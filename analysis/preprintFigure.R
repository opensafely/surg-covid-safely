# preprintFigure.R
#
# This script produces plots of weekly, monthly, 2-monthly and 3-monthly
# summaries of the proportion of surgeries conducted more than 7 weeks from a 
# positive PCR test for SARS-CoV-2 (as per the data available in OpenSAFELY).
#
# Data for two cohorts are plotted: surgery patients with a cancer diagnosis
# within +-6 months of surgery, and for patient without a cancer diagnosis
# since the study start date (2018-03-17).
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
library(ggh4x)
library(RcppRoll)
library(tidyverse)
library(lubridate)


#####################
## Filter datasets ##
#####################
# ----
# OpenSAFELY data, cancer patients, within 6 months of surgery.
plotData_OS_C <- data_to_use_C %>%
  select(c(
    date_surgery,
    category_cancer_within_6mths_surgery,
    preOperative_infection_status,
    Week_surgery,
    Month_surgery,
    Year_surgery
  ))

# OpenSAFELY data, cancer patients, NOT within 6 months of surgery.
plotData_OS_NC <- data_to_use_NC %>%
  select(c(
    date_surgery,
    category_cancer_within_6mths_surgery,
    preOperative_infection_status,
    Week_surgery,
    Month_surgery,
    Year_surgery
  ))
# ----

#################################
## Calculate the data to plot. ##
#################################
# ----
# Define timeline.
startDate = "01-03-2020"
endDate = "31-03-2022"

# Load and run the function that does the work.
source(here::here("analysis","fnc_preprintFigure_dataPrep.R"))
OS_C6m_windowed_proportion_7wkPreOpInfection <- 
  fnc_preprintFigure_dataPrep(data = plotData_OS_C,
                              start = startDate,
                              end = endDate)
OS_NC6m_windowed_proportion_7wkPreOpInfection <-
  fnc_preprintFigure_dataPrep(data = plotData_OS_NC,
                              start = startDate,
                              end = endDate)

# Save the plot data.
write.csv(
  x = OS_C6m_windowed_proportion_7wkPreOpInfection,
  file = here::here("output",
                    "plotData_OS_C6m.csv")
)
write.csv(
  x = OS_NC6m_windowed_proportion_7wkPreOpInfection,
  file = here::here("output",
                    "plotData_OS_NC6m.csv")
)
# ----

###################
## Plot the data ##
###################
# ----
# NOTE: Nest horizontal axes require the ggh4x package to be installed.
#
# Load and run the function that does the work.
source(here::here("analysis","fnc_preprintFigure_dataPlot.R"))
fnc_preprintFigure_dataPlot(data = OS_C6m_windowed_proportion_7wkPreOpInfection,
                            cohort = "with", figureCaption = F)
fnc_preprintFigure_dataPlot(data = OS_NC6m_windowed_proportion_7wkPreOpInfection,
                            cohort = "without", figureCaption = F)

# ----


