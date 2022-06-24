# serviceEvaluationFigures.R
#
# This script produces plots of weekly, monthly, 2-monthly and 3-monthly
# summaries of the proportion of surgeries conducted more than 7 weeks from a 
# positive PCR test for SARS-CoV-2 (as per the data available in OpenSAFELY).
#
# Data for two cohorts are plotted: surgery patients with a cancer diagnosis
# within +-6 months of surgery, and for patient without a cancer diagnosis
# since the study start date (2018-03-17). Note, the plot starts at 2020-03-01.
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
#      detailed the relationship between the 30-post operative mortality and the
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
# OpenSAFELY data, cancer patients, within 3 months of surgery.
plotData_C_within3m <- data_to_use_C_within3m %>%
  select(c(
    date_surgery,
    category_admission_method,
    preOperative_infection_status,
    Week_surgery,
    Month_surgery,
    Year_surgery
  ))

# OpenSAFELY data, cancer patients, within 6 months of surgery.
plotData_C_within6m <- myData %>% 
  dplyr::filter(category_cancer_within_6mths_surgery == 
                  "Cancer diagnosis within 6mths before surgery" |
                  category_cancer_within_6mths_surgery == 
                  "Cancer diagnosis within 6mths after surgery") %>%
  select(c(
    date_surgery,
    category_admission_method,
    preOperative_infection_status,
    Week_surgery,
    Month_surgery,
    Year_surgery
  ))

# OpenSAFELY data, no-cancer patients.
plotData_NC <- data_to_use_NC %>%
  select(c(
    date_surgery,
    category_admission_method,
    preOperative_infection_status,
    Week_surgery,
    Month_surgery,
    Year_surgery
  ))

# OpenSAFELY data, all patients (identified by our surgery codelist).
plotData_AdmMethod <- myData %>%
  select(c(
    date_surgery,
    category_admission_method,
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
source(here::here("analysis","fnc_serviceEvaluationFigures_dataPrep.R"))
C3m_windowed_proportion_7wkPreOpInfection <- 
  fnc_serviceEvaluationFigures_dataPrep(data = plotData_C_within3m,
                              start = startDate,
                              end = endDate)
C6m_windowed_proportion_7wkPreOpInfection <-
  fnc_serviceEvaluationFigures_dataPrep(data = plotData_C_within6m,
                              start = startDate,
                              end = endDate)
NC_windowed_proportion_7wkPreOpInfection <- 
  fnc_serviceEvaluationFigures_dataPrep(data = plotData_NC,
                              start = startDate,
                              end = endDate)
AdmMethod_windowed_proportion_7wkPreOpInfection <-
  dplyr::bind_rows(
    fnc_serviceEvaluationFigures_dataPrep(data = plotData_AdmMethod %>%
                                            dplyr::filter(category_admission_method == "Emergency"),
                                          start = startDate,
                                          end = endDate) %>%
      tibble::add_column(Admission_method = rep("Emergency",nrow(.)), .before = "Year_surgery"),
    fnc_serviceEvaluationFigures_dataPrep(data = plotData_AdmMethod %>%
                                            dplyr::filter(category_admission_method == "Elective"),
                                          start = startDate,
                                          end = endDate) %>%
      tibble::add_column(Admission_method = rep("Elective",nrow(.)), .before = "Year_surgery")
  ) %>% dplyr::arrange(Year_surgery, Month_surgery, Week_surgery)

# Save the plot data.
write.csv(
  x = C3m_windowed_proportion_7wkPreOpInfection,
  file = here::here("output",
                    "plotData_C3m.csv")
)
write.csv(
  x = C6m_windowed_proportion_7wkPreOpInfection,
  file = here::here("output",
                    "plotData_C6m.csv")
)
write.csv(
  x = NC_windowed_proportion_7wkPreOpInfection,
  file = here::here("output",
                    "plotData_NC.csv")
)
write.csv(
  x = AdmMethod_windowed_proportion_7wkPreOpInfection,
  file = here::here("output",
                    "plotData_AdmMethod.csv")
)
# ----

###################
## Plot the data ##
###################
# ----
# NOTE: Nested horizontal axes require the ggh4x package to be installed.
#
# Load and run the function that does the work.
source(here::here("analysis","fnc_serviceEvaluationFigures_dataPlot.R"))
fnc_serviceEvaluationFigures_dataPlot(data = C3m_windowed_proportion_7wkPreOpInfection,
                            cancer = "with", window = 3, figureCaption = F)
fnc_serviceEvaluationFigures_dataPlot(data = C6m_windowed_proportion_7wkPreOpInfection,
                            cancer = "with", window = 6, figureCaption = F)
fnc_serviceEvaluationFigures_dataPlot(data = NC_windowed_proportion_7wkPreOpInfection,
                            cancer = "without", window = "", figureCaption = F)
fnc_serviceEvaluationFigures_dataPlot(data = AdmMethod_windowed_proportion_7wkPreOpInfection,
                                      cancer = "without", window = "", figureCaption = F,
                                      strata = "Admission_method")

# ----


