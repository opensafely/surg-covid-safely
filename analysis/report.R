
## If running on OpenSAFELY.
library('tidyverse')
library('lubridate')
library("kableExtra")
library("here")
## If ever running locally.
# list_of_packages <- c("tidyverse", "lubridate", kableExtra","here")
# new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
# if(length(new_packages)) install.packages(new_packages)
# for (i in 1:length(list_of_packages))
# {
#   library(list_of_packages[i],character.only = T)
# }
#webshot::install_phantomjs()

# Read data.
df_input <- read_csv(
  here::here("output", "input.csv"),
  col_types = cols(SARS_CoV_2_test_type = col_factor(),
                   SARS_CoV_2_symptomatic = col_factor(),
                   Sex = col_factor(),
                   patient_id = col_integer())
  )
myData <- df_input

# Define required variables.
myData <- myData %>%
            ## Distinction pre and post COVID.
            ## # NB: if the list of possible categories changes, the list will
            ## #     need to be updated in Make_Table1.R, too.
            mutate(
              surgery_pre_or_post_COVID_UK = case_when(
                .$date_surgery <= "2020-03-17" ~ "preCOVID surgery",
                .$date_surgery > "2020-03-17" ~ "postCOVID surgery",
                is.na(.$date_surgery) ~ "No surgery"
                                              )
                  ) %>%
            ## Date of death.
            mutate(
              date_death = case_when(
                is.na(.$date_death_ons) & !is.na(.$date_death_cpns) ~ .$date_death_cpns,
                is.na(.$date_death_cpns) & !is.na(.$date_death_ons) ~ .$date_death_ons,
                is.na(.$date_death_ons) & is.na(.$date_death_cpns) ~ NA_Date_
              )
            )
myData <- myData %>%
            ## Indicator for 30-day post-operative mortality.
            ## # NB: if the list of possible categories changes, the list will
            ## #     need to be updated in Make_Table1.R, too.
            mutate(
             postOp_mortality_30day = case_when(
              (.$date_death < .$date_surgery) ~ "Error: Surgery after death",
                (.$date_death - .$date_surgery) <= 30 ~ "Dead within 30-day post-operation",
                (.$date_death - .$date_surgery) > 30 ~ "Alive within 30-day post-operation",
              is.na(.$date_death) ~ "No death recorded",
              is.na(.$date_surgery) ~ "No surgery recorded"
              )
            ) %>%
            ## Month of surgery.
            mutate(Month_surgery = lubridate::month(lubridate::ymd(myData$date_surgery), label = T)) %>%
            ## Year of surgery.
            mutate(Year_surgery = lubridate::year(myData$date_surgery)) %>%
            ## Indicator for pre-surgery COVID vaccination. 
            ## # NB: if the list of possible categories changes, the list will
            ## #     need to be updated in Make_Table1.R, too.
            mutate(
              preSurgery_vaccinated = case_when(
                (.$date_COVID_first_vaccination < .$date_surgery) ~ "PreSurgery vaccination",
                (.$date_COVID_first_vaccination > .$date_surgery) ~ "PostSurgery vaccination",
                is.na(.$date_COVID_first_vaccination) ~ "No vaccination recorded"
               )
            ) %>%
            ## No record of indication of pre-operative SARS-CoV-2 infection.
            ## # NB: if the list of possible categories changes, the list will
            ## #     need to be updated in Make_Table1.R, too.
            mutate(
              preOperative_infection_status = case_when(
                (.$date_surgery - .$date_latest_test_preOp_SARS_CoV_2_outcome_positive) <0 ~
                    "Error: Test result after surgery. Check study_definition.",
                !is.na(.$date_latest_test_preOp_SARS_CoV_2_outcome_positive) & 
                  abs(.$date_surgery - .$date_latest_test_preOp_SARS_CoV_2_outcome_positive) < 14 ~ 
                    "0-2 weeks record of pre-operative SARS-CoV-2 infection",
                !is.na(.$date_latest_test_preOp_SARS_CoV_2_outcome_positive) & 
                  #dplyr::between(abs(.$date_surgery - .$date_latest_test_preOp_SARS_CoV_2_outcome_positive), 15, 28) ~ 
                  abs(.$date_surgery - .$date_latest_test_preOp_SARS_CoV_2_outcome_positive) > 15 &
                  abs(.$date_surgery - .$date_latest_test_preOp_SARS_CoV_2_outcome_positive) < 28 ~
                    "3-4 weeks record of pre-operative SARS-CoV-2 infection",
                !is.na(.$date_latest_test_preOp_SARS_CoV_2_outcome_positive) & 
                  #dplyr::between(abs(.$date_surgery - .$date_latest_test_preOp_SARS_CoV_2_outcome_positive), 29, 42) ~ 
                  abs(.$date_surgery - .$date_latest_test_preOp_SARS_CoV_2_outcome_positive) > 29 &
                  abs(.$date_surgery - .$date_latest_test_preOp_SARS_CoV_2_outcome_positive) < 42 ~
                    "5-6 weeks record of pre-operative SARS-CoV-2 infection",
                !is.na(.$date_latest_test_preOp_SARS_CoV_2_outcome_positive) & 
                  abs(.$date_surgery - .$date_latest_test_preOp_SARS_CoV_2_outcome_positive) >= 49 ~ 
                    ">=7 weeks record of pre-operative SARS-CoV-2 infection",
                TRUE ~ "No record of pre-operative SARS-CoV-2 infection"
              )
            )

# 
# # Collect plot data.
# myPlotData_plot1 <- myData %>%
#     group_by(Year_surgery, Month_surgery) %>%
#       summarise(n_tdpo = n(),
#                 count_tdpo = sum(ifelse(postOp_mortality_30day=="Dead within 30-day post-operation",1,0)),
#                 prop_tdpo = (count_tdpo / n_tdpo))
# # Save plot data.
# write.csv(
#   x = myPlotData_plot1,
#   file = paste0(here::here("output"),"/myPlotData_plot1.csv")
# )
# # Make basic plot.
# plot_postOp_mortality_30day <-
# ggplot(myPlotData_plot1, aes(x=Month_surgery, y=n_tdpo, group=Year_surgery, colour=Year_surgery)) +
# geom_line() +
# ylim(0, 25) +
# labs(x = "Month of surgery", y = "Count of patients")
#   
# 
# # Save plot.
# ggsave(
#   plot = plot_postOp_mortality_30day,
#   filename="plot_postOp_mortality_30day.png",
#   path=here::here("output"),
# )

# Make Table 1.
source(paste0(here::here("analysis"),"/Make_Table1.R"))