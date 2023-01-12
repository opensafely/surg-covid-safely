## If running on OpenSAFELY.
library("plyr")
library('tidyverse')
library('lubridate')
library("kableExtra")
library("here")
## If ever running locally.
# list_of_packages <- c("tidyverse", "lubridate", "kableExtra","here")
# new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
# if(length(new_packages)) install.packages(new_packages)
# for (i in 1:length(list_of_packages))
# {
#   library(list_of_packages[i],character.only = T)
# }

# Read data.
df_input <- readr::read_csv(
  here::here("output", "input_2.csv"),
  col_types = cols(has_cancer = col_logical(),
                   has_surgery = col_logical(),
                   date_surgery = col_date(),
                   date_latest_test_preOp_SARS_CoV_2_outcome_any = col_date(),
                   date_latest_test_preOp_SARS_CoV_2_outcome_positive = col_date(),
                   date_latest_test_preOp_SARS_CoV_2_outcome_negative = col_date(),
                   date_death_ons = col_date(),
                   date_death_cpns = col_date(),
                   date_postOp_cerebrovascular_complication = col_date(),
                   SARS_CoV_2_test_type = col_factor(),
                   SARS_CoV_2_symptomatic = col_factor(),
                   age_at_surgery = col_integer(),
                   age_group_surgery = col_factor(),
                   Sex = col_factor(),
                   COVID_first_vaccination_SNOMED = col_logical(),
                   COVID_second_vaccination_SNOMED = col_logical(),
                   COVID_first_vaccination_declined_SNOMED = col_logical(),
                   COVID_second_vaccination_declined_SNOMED = col_logical(),
                   chronic_cardiac_disease = col_logical(),
                   diabetes = col_logical(),
                   chronic_respiratory_disease = col_logical(),
                   cerebrovascular_disease = col_logical(),
                   admission_method = col_factor(),
                   admission_method_patient_classification = col_factor(),
                   category_admission_method = col_factor(),
                   patient_id = col_integer())
)
# Some fudges to handle unusual exceptions for the Sex variable.
df_input$Sex <- plyr::mapvalues(df_input$Sex, from = c("F", "M"), to = c("Female", "Male"))
df_input <- df_input[!(df_input$Sex == "I" | df_input$Sex == "U"),]
# Change the logical values for the disease conditions into character types of {Yes, No}.
df_input$chronic_cardiac_disease <- plyr::mapvalues(df_input$chronic_cardiac_disease, from = c(TRUE, FALSE), to = c("Yes", "No"))
df_input$diabetes <- plyr::mapvalues(df_input$diabetes, from = c(TRUE, FALSE), to = c("Yes", "No"))
df_input$chronic_respiratory_disease <- plyr::mapvalues(df_input$chronic_respiratory_disease, from = c(TRUE, FALSE), to = c("Yes", "No"))
df_input$cerebrovascular_disease <- plyr::mapvalues(df_input$cerebrovascular_disease, from = c(TRUE, FALSE), to = c("Yes", "No"))
# Trim nonsensical values for age_at_surgery.
df_input$age_at_surgery <-
  df_input %>%
  dplyr::select(age_at_surgery) %>% as.matrix() %>%
  dplyr::if_else((. < 0 | . > 105), NA_integer_, .)

# Assign input to secondary variable. 
myData <- df_input

################################
## Define required variables. ##
################################
# ----
myData <- myData %>%
  ## Distinguish era of the COVID pandemic.
  ## # NB: if the list of possible categories changes, the list will
  ## #     need to be updated in Make_Table1.R, too.
  dplyr::mutate(
    era = dplyr::case_when(
      .$has_surgery == FALSE ~ "No surgery recorded",
      is.na(.$date_surgery) ~ "No surgery date recorded",
      .$date_surgery <= "2020-03-17" ~ "Pre-pandemic",
      .$date_surgery <= lubridate::ymd("2020-12-08") + lubridate::weeks(5) ~
                                       "Pandemic no vaccine",
      .$date_surgery > lubridate::ymd("2020-12-08") + lubridate::weeks(5) ~ 
                                       "Pandemic with vaccine"
    )
  ) %>%
  ## Indicator of surgeries that took place within the data collection window
  ## of the COVIDSurg paper that we are emulating.
  dplyr::mutate(
    COVIDSurg_data_collection_period = dplyr::case_when(
      .$has_surgery == FALSE ~ "No surgery recorded",
      is.na(.$date_surgery) ~ "No surgery date recorded",
      (.$date_surgery >= "2020-10-05" & .$date_surgery <= "2020-11-01") ~ "COVIDSurg data collection period",
      TRUE ~ "Not COVIDSurg data collection period"
    )
  ) %>%
  ## Date of death.
  dplyr::mutate(
    date_death = .$date_death_ons
    ## Simplifying the logic for date_death. Just go with ONS.
    #
    # date_death = dplyr::case_when(
    #   is.na(.$date_death_ons) & !is.na(.$date_death_cpns) ~ .$date_death_cpns,
    #   is.na(.$date_death_cpns) & !is.na(.$date_death_ons) ~ .$date_death_ons,
    #   is.na(.$date_death_ons) & is.na(.$date_death_cpns) ~ NA_Date_
    #)
  ) %>%
  ## Identifying patients with a cancer diagnosis within 3 months
  ## before or after surgery.
  dplyr::mutate(
    category_cancer_within_3mths_surgery = dplyr::case_when(
      .$has_cancer == FALSE ~ "No cancer diagnosis recorded",
      is.na(.$date_cancer) ~ "No cancer diagnosis date recorded, despite having cancer diagnosis",
      .$has_surgery == FALSE ~ "No surgery recorded",
      is.na(.$date_surgery) ~ "No surgery date recorded, despite having surgery code",
      (.$date_surgery - .$date_cancer > 0) & (.$date_surgery - .$date_cancer < 90) ~ "Cancer diagnosis within 3mths before surgery",
      (.$date_cancer - .$date_surgery > 0) & (.$date_cancer - .$date_surgery < 90) ~ "Cancer diagnosis within 3mths after surgery",
      abs(.$date_cancer - .$date_surgery) > 90 ~ "Cancer diagnosis outwith 3mths before or after surgery"
    )
  ) %>%
  ## Identifying patients with a cancer diagnosis within 6 months
  ## before or after surgery.
  dplyr::mutate(
    category_cancer_within_6mths_surgery = dplyr::case_when(
      .$has_cancer == FALSE ~ "No cancer diagnosis recorded",
      is.na(.$date_cancer) ~ "No cancer diagnosis date recorded, despite having cancer diagnosis",
      .$has_surgery == FALSE ~ "No surgery recorded",
      is.na(.$date_surgery) ~ "No surgery date recorded, despite having surgery code",
      (.$date_surgery - .$date_cancer > 0) & (.$date_surgery - .$date_cancer < 180) ~ "Cancer diagnosis within 6mths before surgery",
      (.$date_cancer - .$date_surgery > 0) & (.$date_cancer - .$date_surgery < 180) ~ "Cancer diagnosis within 6mths after surgery",
      abs(.$date_cancer - .$date_surgery) > 180 ~ "Cancer diagnosis outwith 6mths before or after surgery"
    )
  ) %>%
  ## Distinction pre and post vaccines in the UK
  ## # NB: if the list of possible categories changes, the list will
  ## #     need to be updated in Make_Table1.R, too.
  dplyr::mutate(
    surgery_pre_or_post_vaccine_UK = dplyr::case_when(
      .$has_surgery == FALSE ~ "No surgery recorded",
      .$date_surgery <= "2020-12-08" ~ "preVaccine surgery",
      .$date_surgery > "2020-12-08" ~ "postVaccine surgery",
    )
  ) %>%
  ## Categorising patients based on their vaccination status prior to the 
  ## test for an indication of SARS-CoV-2.
  dplyr::mutate(
    category_vaccination_status_before_test = dplyr::case_when(
      (is.na(.$COVID_first_vaccination_SNOMED) & is.na(.$COVID_second_vaccination_SNOMED)) ~
        "Error: No data on vaccine administration",
        # Irrespective of the *_declined variables.
      .$COVID_second_vaccination_SNOMED ~
        "Confirmed fully vaccinated before test",
        # Irrespective of the *_declined variables, and we assume the missing
        # confirmation or FALSE value of the first dose is an error.
      (.$COVID_first_vaccination_SNOMED & is.na(.$COVID_second_vaccination_SNOMED)) ~
        "At least partially vaccinated before test",
        # Even if *_declined variables are TRUE, we can't be sure if they
        # changed their mind.
      .$COVID_first_vaccination_SNOMED ~
        "Confirmed partially vaccinated before test",
        # Previous criteria imply the 2nd dose is F or NA.
      (.$COVID_first_vaccination_declined_SNOMED & .$COVID_second_vaccination_declined_SNOMED) ~
        "Confirmed not vaccinated before test",
        # Previous criteria would have captured a patient with any combination
        # of first or second dose.
      is.na(.$COVID_first_vaccination_SNOMED) ~
        "Unknown: No data for 1st dose and 2nd dose is FALSE or also missing",
        # Previous criteria imply the 2nd dose is F or NA.
      (.$COVID_first_vaccination_SNOMED != TRUE & .$COVID_first_vaccination_declined_SNOMED) ~
        "Confirmed not vaccinated before test",
        # Previous criteria imply the 2nd dose is F or NA.
      TRUE ~ "Unknown vaccination status before test"
    )
  )
# This second pipeline is separate from the first because it uses variables
# created in the first pipeline.
myData <- myData %>%
            ## Indicator for 30-day post-operative mortality.
            ## # NB: if the list of possible categories changes, the list will
            ## #     need to be updated in Make_Table1.R, too.
            dplyr::mutate(
             postOp_mortality_30day = dplyr::case_when(
               .$has_surgery == FALSE ~ "No surgery recorded",
              (.$date_death < .$date_surgery) ~ "Error: Surgery after death",
                (.$date_death - .$date_surgery) <= 30 ~ "Dead within 30 days post-operation",
                ((.$date_death - .$date_surgery) > 30 | is.na(.$date_death)) ~ "Alive within 30 days post-operation"
              )
            ) %>%
            ## Indicator for 90-day post-operative mortality.
            ## # NB: if the list of possible categories changes, the list will
            ## #     need to be updated in Make_Table1.R, too.
            dplyr::mutate(
              postOp_mortality_90day = dplyr::case_when(
                .$has_surgery == FALSE ~ "No surgery recorded",
                (.$date_death < .$date_surgery) ~ "Error: Surgery after death",
                (.$date_death - .$date_surgery) <= 90 ~ "Dead within 90 days post-operation",
                ((.$date_death - .$date_surgery) > 90 | is.na(.$date_death)) ~ "Alive within 90 days post-operation"
              )
            ) %>%
            ## Indicator for 6-month post-operative mortality.
            ## # NB: if the list of possible categories changes, the list will
            ## #     need to be updated in Make_Table1.R, too.
            dplyr::mutate(
              postOp_mortality_6mth = dplyr::case_when(
                .$has_surgery == FALSE ~ "No surgery recorded",
                (.$date_death < .$date_surgery) ~ "Error: Surgery after death",
                (.$date_death - .$date_surgery) <= 180 ~ "Dead within 6 months post-operation",
                ((.$date_death - .$date_surgery) > 180| is.na(.$date_death)) ~ "Alive within 6 months post-operation"
              )
            ) %>%
            ## Indicator for 12-month post-operative mortality.
            ## # NB: if the list of possible categories changes, the list will
            ## #     need to be updated in Make_Table1.R, too.
            dplyr::mutate(
              postOp_mortality_12mth = dplyr::case_when(
                .$has_surgery == FALSE ~ "No surgery recorded",
                (.$date_death < .$date_surgery) ~ "Error: Surgery after death",
                (.$date_death - .$date_surgery) <= 365 ~ "Dead within 12 months post-operation",
                ((.$date_death - .$date_surgery) > 365 | is.na(.$date_death)) ~ "Alive within 12 months post-operation"
              )
            ) %>%
            ## Indicator for 30-day post-operative cerebrovascular complication.
            ## # NB: if the list of possible categories changes, the list will
            ## #     need to be updated in Make_Table1.R, too.
            dplyr::mutate(
              postOp_cerebrovascular_complication_30day = dplyr::case_when(
                .$has_surgery == FALSE ~ "No surgery recorded",
                (.$date_postOp_cerebrovascular_complication < .$date_surgery) ~ "Ignore: Pre-operative complication",
                (.$date_postOp_cerebrovascular_complication - .$date_surgery) <= 30 ~ "Complications",
                ((.$date_postOp_cerebrovascular_complication - .$date_surgery) > 30 | is.na(.$date_postOp_cerebrovascular_complication)) ~ "No complications"
              )
            ) %>%
            ## Indicator for 30-day post-operative pulmonary complication
            ## # NB: if the list of possible categories changes, the list will
            ## #     need to be updated in Make_Table1.R, too.
            dplyr::mutate(
              postOp_pulmonary_complication_30day = dplyr::case_when(
                .$has_surgery == FALSE ~ "No surgery recorded",
                (.$date_postOp_pulmonary_complication < .$date_surgery) ~ "Ignore: Pre-operative complication",
                (.$date_postOp_pulmonary_complication - .$date_surgery) <= 30 ~ "Complications",
                ((.$date_postOp_pulmonary_complication - .$date_surgery) > 30 | is.na(.$date_postOp_pulmonary_complication)) ~ "No complications"
              )
            ) %>%
            ## Indicator for 30-day post-operative cardiac complication
            ## # NB: if the list of possible categories changes, the list will
            ## #     need to be updated in Make_Table1.R, too.
            dplyr::mutate(
              postOp_cardiac_complication_30day = dplyr::case_when(
                .$has_surgery == FALSE ~ "No surgery recorded",
                (.$date_postOp_cardiac_complication < .$date_surgery) ~ "Ignore: Pre-operative complication",
                (.$date_postOp_cardiac_complication - .$date_surgery) <= 30 ~ "Complications",
                ((.$date_postOp_cardiac_complication - .$date_surgery) > 30 | is.na(.$date_postOp_cardiac_complication)) ~ "No complications"
              )
            ) %>%
            ## Week of surgery.
            dplyr::mutate(Week_surgery = lubridate::week(lubridate::ymd(.$date_surgery))) %>%          
            ## Month of surgery.
            dplyr::mutate(Month_surgery = lubridate::month(lubridate::ymd(.$date_surgery), label = T)) %>%
            ## Year of surgery.
            dplyr::mutate(Year_surgery = lubridate::year(.$date_surgery)) %>%
            ## No record of indication of pre-operative SARS-CoV-2 infection.
            ## # NB: if the list of possible categories changes, the list will
            ## #     need to be updated in Make_Table1.R, too.
            dplyr::mutate(
              preOperative_infection_status = dplyr::case_when(
                (.$date_surgery - .$date_latest_test_preOp_SARS_CoV_2_outcome_positive) < 0 ~
                    "Error: Test result after surgery. Check study_definition.",
                (.$date_surgery - .$date_latest_test_preOp_SARS_CoV_2_outcome_positive) == 0 ~
                    "Positive test and surgery on the same day. Surgery event excluded",
                !is.na(.$date_latest_test_preOp_SARS_CoV_2_outcome_positive) & 
                  abs(.$date_surgery - .$date_latest_test_preOp_SARS_CoV_2_outcome_positive) >= 0 & 
                  abs(.$date_surgery - .$date_latest_test_preOp_SARS_CoV_2_outcome_positive) <= 14 ~ 
                    "0-2 weeks record of pre-operative SARS-CoV-2 infection",
                !is.na(.$date_latest_test_preOp_SARS_CoV_2_outcome_positive) & 
                  abs(.$date_surgery - .$date_latest_test_preOp_SARS_CoV_2_outcome_positive) >= 15 &
                  abs(.$date_surgery - .$date_latest_test_preOp_SARS_CoV_2_outcome_positive) <= 28 ~
                    "3-4 weeks record of pre-operative SARS-CoV-2 infection",
                !is.na(.$date_latest_test_preOp_SARS_CoV_2_outcome_positive) & 
                  abs(.$date_surgery - .$date_latest_test_preOp_SARS_CoV_2_outcome_positive) >= 29 &
                  abs(.$date_surgery - .$date_latest_test_preOp_SARS_CoV_2_outcome_positive) <= 42 ~
                    "5-6 weeks record of pre-operative SARS-CoV-2 infection",
                !is.na(.$date_latest_test_preOp_SARS_CoV_2_outcome_positive) & 
                  abs(.$date_surgery - .$date_latest_test_preOp_SARS_CoV_2_outcome_positive) >= 43 ~ 
                    ">=7 weeks record of pre-operative SARS-CoV-2 infection",
                TRUE ~ "No record of pre-operative SARS-CoV-2 infection"
              )
            ) %>% dplyr::mutate_if(is.character,as.factor)
# ----
