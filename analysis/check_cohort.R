# check_cohort.R
#
# This script is part of an adjacent mini-study to check the size of the cohort
# returned from OpenSAFELY, given our selection criteria.
#
library("tidyverse")
# Read data.
df_input <- read_csv(
  here::here("output", "input_flow_chart.csv")
)
myData <- df_input

check_cohort <- data.frame(
                          all_patients = nrow(myData),
                          has_cancer = length(which(myData$has_cancer == "1")),
                          has_surgery = length(which(myData$has_surgery == "1")),
                          has_both = length(which(myData$has_cancer == "1" &
                                         myData$has_surgery == "1")),
                          has_pos_test = length(which(myData$has_test_preOp_SARS_CoV_2_outcome_positive == "1")),
                          has_all = length(which(myData$has_cancer == "1" &
                                                   myData$has_surgery == "1" &
                                                   myData$has_test_preOp_SARS_CoV_2_outcome_positive == "1"))
                          )


write.csv(
  x = check_cohort,
  file = here::here("output","check_cohort.csv")
)