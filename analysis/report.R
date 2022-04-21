## If running on OpenSAFELY.
library('tidyverse')
library('lubridate')
library("kableExtra")
library("here")
library("magick")
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
df_input <- readr::read_csv(
  here::here("output", "input.csv"),
  col_types = cols(date_surgery = col_date(),
                   date_latest_test_preOp_SARS_CoV_2_outcome_any = col_date(),
                   date_latest_test_preOp_SARS_CoV_2_outcome_positive = col_date(),
                   date_latest_test_preOp_SARS_CoV_2_outcome_negative = col_date(),
                   date_death_ons = col_date(),
                   date_death_cpns = col_date(),
                   SARS_CoV_2_test_type = col_factor(),
                   SARS_CoV_2_symptomatic = col_factor(),
                   age_at_surgery = col_integer(),
                   age_group_surgery = col_factor(),
                   Sex = col_factor(),
                   COVID_first_vaccination = col_logical(),
                   COVID_second_vaccination = col_logical(),
                   COVID_first_vaccination_declined = col_logical(),
                   COVID_second_vaccination_declined = col_logical(),
                   patient_id = col_integer())
)
# Some fudges to handle unusual exceptions for the Sex variable.
df_input$Sex <- plyr::mapvalues(df_input$Sex, from = c("F", "M"), to = c("Female", "Male"))
df_input <- df_input[!(df_input$Sex == "I" | df_input$Sex == "U"),]

myData <- df_input

# Save the count of patients returned, for reference.
write.csv(
  x = nrow(myData),
  file = here::here("output","count_patients.csv")
)



# Define required variables.
myData <- myData %>%
  ## Distinction pre and post COVID.
  ## # NB: if the list of possible categories changes, the list will
  ## #     need to be updated in Make_Table1.R, too.
  dplyr::mutate(
    surgery_pre_or_post_COVID_UK = dplyr::case_when(
      .$date_surgery <= "2020-03-17" ~ "preCOVID surgery",
      .$date_surgery > "2020-03-17" ~ "postCOVID surgery",
      is.na(.$date_surgery) ~ "No surgery"
    )
  ) %>%
  ## Date of death.
  dplyr::mutate(
    date_death = dplyr::case_when(
      is.na(.$date_death_ons) & !is.na(.$date_death_cpns) ~ .$date_death_cpns,
      is.na(.$date_death_cpns) & !is.na(.$date_death_ons) ~ .$date_death_ons,
      is.na(.$date_death_ons) & is.na(.$date_death_cpns) ~ NA_Date_
    )
  ) %>%
  ## Identifying patients with a cancer diagnosis within 3 months
  ## before or after surgery.
  dplyr::mutate(
    category_cancer_within_3mths_surgery = dplyr::case_when(
      (.$date_surgery - .$date_cancer > 0) & (.$date_surgery - .$date_cancer < 90) ~ "Cancer diagnosis within 3mths before surgery",
      (.$date_cancer - .$date_surgery > 0) & (.$date_cancer - .$date_surgery < 90) ~ "Cancer diagnosis within 3mths after surgery",
      abs(.$date_cancer - .$date_surgery) > 90 ~ "No cancer diagnosis within 3mths before or after surgery",
      is.na(.$date_cancer) ~ "No cancer diagnosis recorded",
      is.na(.$date_surgery) ~ "No surgery recorded"
    )
  ) %>%
  ## Identifying patients with a cancer diagnosis within 6 months
  ## before or after surgery.
  dplyr::mutate(
    category_cancer_within_6mths_surgery = dplyr::case_when(
      (.$date_surgery - .$date_cancer > 0) & (.$date_surgery - .$date_cancer < 180) ~ "Cancer diagnosis within 6mths before surgery",
      (.$date_cancer - .$date_surgery > 0) & (.$date_cancer - .$date_surgery < 180) ~ "Cancer diagnosis within 6mths after surgery",
      abs(.$date_cancer - .$date_surgery) > 180 ~ "No cancer diagnosis within 6mths before or after surgery",
      is.na(.$date_cancer) ~ "No cancer diagnosis recorded",
      is.na(.$date_surgery) ~ "No surgery recorded"
    )
  ) %>%
  ## Distinction pre and post vaccines in the UK
  ## # NB: if the list of possible categories changes, the list will
  ## #     need to be updated in Make_Table1.R, too.
  dplyr::mutate(
    surgery_pre_or_post_vaccine_UK = dplyr::case_when(
      .$date_surgery <= "2020-12-08" ~ "preVaccine surgery",
      .$date_surgery > "2020-12-08" ~ "postVaccine surgery",
      is.na(.$date_surgery) ~ "No surgery"
    )
  ) %>%
  ## Categorising patients based on their vaccination status prior to the 
  ## test for an indication of SARS-CoV-2.
  dplyr::mutate(
    category_vaccination_status_before_test = dplyr::case_when(
      (is.na(.$COVID_first_vaccination) & is.na(.$COVID_second_vaccination)) ~
        "Error: No data on vaccine administration",
        # Irrespective of the *_declined variables.
      .$COVID_second_vaccination ~
        "Confirmed fully vaccinated before test",
        # Irrespective of the *_declined variables, and we assume the missing
        # confirmation or FALSE value of the first dose is an error.
      (.$COVID_first_vaccination & is.na(.$COVID_second_vaccination)) ~
        "At least partially vaccinated before test",
        # Even if *_declined variables are TRUE, we can't be sure if they
        # changed their mind.
      .$COVID_first_vaccination ~
        "Confirmed partially vaccinated before test",
        # Previous criteria imply the 2nd dose is F or NA.
      (.$COVID_first_vaccination_declined & .$COVID_second_vaccination_declined) ~
        "Confirmed not vaccinated before test",
        # Previous criteria would have captured a patient with any combination
        # of first or second dose.
      is.na(.$COVID_first_vaccination) ~
        "Unknown: No data for 1st dose and 2nd dose is FALSE or also missing",
        # Previous criteria imply the 2nd dose is F or NA.
      (.$COVID_first_vaccination != TRUE & .$COVID_first_vaccination_declined) ~
        "Confirmed not vaccinated before test",
        # Previous criteria imply the 2nd dose is F or NA.
      TRUE ~ "Unknown vaccination status before test"
    )
  )
myData <- myData %>%
            ## Indicator for 30-day post-operative mortality.
            ## # NB: if the list of possible categories changes, the list will
            ## #     need to be updated in Make_Table1.R, too.
            dplyr::mutate(
             postOp_mortality_30day = dplyr::case_when(
              (.$date_death < .$date_surgery) ~ "Error: Surgery after death",
                (.$date_death - .$date_surgery) <= 30 ~ "Dead within 30-day post-operation",
                (.$date_death - .$date_surgery) > 30 ~ "Alive within 30-day post-operation",
              is.na(.$date_death) ~ "No death recorded",
              is.na(.$date_surgery) ~ "No surgery recorded"
              )
            ) %>%
            ## Month of surgery.
            dplyr::mutate(Month_surgery = lubridate::month(lubridate::ymd(.$date_surgery), label = T)) %>%
            ## Year of surgery.
            dplyr::mutate(Year_surgery = lubridate::year(.$date_surgery)) %>%
            ## No record of indication of pre-operative SARS-CoV-2 infection.
            ## # NB: if the list of possible categories changes, the list will
            ## #     need to be updated in Make_Table1.R, too.
            dplyr::mutate(
              preOperative_infection_status = dplyr::case_when(
                (.$date_surgery - .$date_latest_test_preOp_SARS_CoV_2_outcome_positive) <0 ~
                    "Error: Test result after surgery. Check study_definition.",
                (.$date_surgery - .$date_latest_test_preOp_SARS_CoV_2_outcome_positive) == 0 ~
                    "Positive test and surgery on the same day. Surgery event excluded",
                !is.na(.$date_latest_test_preOp_SARS_CoV_2_outcome_positive) & 
                  abs(.$date_surgery - .$date_latest_test_preOp_SARS_CoV_2_outcome_positive) > 0 & 
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
            ) %>% dplyr::mutate_if(is.character,as.factor)

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
# labs(x = "Month of surgery", y = "Count of patients"))
#
#
#   
# 
# # Save plot.
# ggsave(
#   plot = plot_postOp_mortality_30day,
#   filename="plot_postOp_mortality_30day.png",
#   path=here::here("output"),
# )

# Make Table 1, for the data relating to the 4 week on-boarding.
source(here::here("analysis","Make_Table_Vacc.R"))
source(here::here("analysis","Make_Table_Vacc_3mths.R"))
source(here::here("analysis","Make_Table_Vacc_6mths.R"))
source(here::here("analysis","Make_Table1_4wk_onboarding.R"))
source(here::here("analysis","Make_Table1_4wk_onboarding_3mths.R"))
source(here::here("analysis","Make_Table1_4wk_onboarding_6mths.R"))
# Make Table 1, complete with all relevant variables.
#source(here::here("analysis","Make_Table1_complete.R"))

