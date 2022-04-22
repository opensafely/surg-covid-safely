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

#########################
## Get and append data ##
#########################
# ----
# Read data.
df_input <- readr::read_csv(
  here::here("output", "input_vacc_check_posTest.csv"),
  col_types = readr::cols(date_surgery = col_date(),
                          date_cancer = col_date(),
                          date_latest_test_preOp_SARS_CoV_2_outcome_any = col_date(),
                          date_latest_test_preOp_SARS_CoV_2_outcome_positive = col_date(),
                          date_latest_test_preOp_SARS_CoV_2_outcome_negative = col_date(),
                          date_death_ons = col_date(),
                          date_death_cpns = col_date(),
                          COVID_first_vaccination_TPP_date = col_date(),
                          SARS_CoV_2_test_type = col_factor(),
                          SARS_CoV_2_symptomatic = col_factor(),
                          age_at_surgery = col_integer(),
                          age_group_surgery = col_factor(),
                          Sex = col_factor(),
                          COVID_first_vaccination_SNOMED = col_logical(),
                          COVID_second_vaccination_SNOMED = col_logical(),
                          COVID_first_vaccination_declined_SNOMED = col_logical(),
                          COVID_second_vaccination_declined_SNOMED = col_logical(),
                          COVID_additional_vaccination_TPP = col_logical(),
                          patient_id = col_integer())
)
# Some fudges to handle unusual exceptions for the Sex variable.
df_input$Sex <- plyr::mapvalues(df_input$Sex, from = c("F", "M"), to = c("Female", "Male"))
df_input <- df_input[!(df_input$Sex == "I" | df_input$Sex == "U"),]

myData <- df_input



# Define required variables.
myData <- myData %>%
  ## Make indicator for the first COVID vaccination based on TPP data.
  dplyr::mutate(
    COVID_first_vaccination_TPP = dplyr::case_when(
      !is.na(.$COVID_first_vaccination_TPP_date) ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
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
  )

myData <- myData %>%
  ## Categorising patients based on their vaccination status prior to the 
  ## test for an indication of SARS-CoV-2. Based on SNOMED data.
  dplyr::mutate(
    SNOMED_category_vaccination_status_before_test = dplyr::case_when(
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
  ) %>%
  ## Categorising patients based on their vaccination status prior to the 
  ## test for an indication of SARS-CoV-2. Based on TPP data.
  dplyr::mutate(
    TPP_category_vaccination_status_before_test = dplyr::case_when(
      (is.na(.$COVID_first_vaccination_TPP) & is.na(.$COVID_additional_vaccination_TPP)) ~
        "Error: No data on vaccine administration",
      # Irrespective of the *_declined variables.
      .$COVID_additional_vaccination_TPP ~
        "Confirmed fully vaccinated before test",
      # Irrespective of the *_declined variables, and we assume the missing
      # confirmation or FALSE value of the first dose is an error.
      (.$COVID_first_vaccination_TPP & is.na(.$COVID_additional_vaccination_TPP)) ~
        "At least partially vaccinated before test",
      # Even if *_declined variables are TRUE, we can't be sure if they
      # changed their mind.
      .$COVID_first_vaccination_TPP ~
        "Confirmed partially vaccinated before test",
      # Previous criteria imply the 2nd dose is F or NA.
      is.na(.$COVID_first_vaccination_TPP) ~
        "Unknown: No data for 1st dose and 2nd dose is FALSE or also missing",
      # Previous criteria imply the 2nd dose is F or NA.
      TRUE ~ "Unknown vaccination status before test"
    )
  ) %>%
  ## Basic rules for categorising patients based on their vaccination status 
  ## prior to the test for an indication of SARS-CoV-2. Based on SNOMED data.
  dplyr::mutate(
    AlwynSNOMED_category_vaccination_status_before_test = dplyr::case_when(
      .$COVID_second_vaccination_SNOMED ~
        "Confirmed fully vaccinated before test",
      TRUE ~ "Double vaccination before test not confirmed"
    )
  ) %>%
  ## Basic rules for categorising patients based on their vaccination status 
  ## prior to the test for an indication of SARS-CoV-2. Based on TPP data.
  dplyr::mutate(
    AlwynTPP_category_vaccination_status_before_test = dplyr::case_when(
      .$COVID_additional_vaccination_TPP ~
        "Confirmed fully vaccinated before test",
      TRUE ~ "Double vaccination before test not confirmed"
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
# ----

#################################################
# Make tibbles that will inform the final table #
#################################################
# ----
myData_3mths_vacc <- myData

myData_3mths_vacc <- myData_3mths_vacc %>% 
  dplyr::filter(category_cancer_within_3mths_surgery == 
                  "Cancer diagnosis within 3mths before surgery" |
                  category_cancer_within_3mths_surgery == 
                  "Cancer diagnosis within 3mths after surgery")

# ## Count of patients in each of the categories for pre-operative infection
# ## status:
# ##    1. "No record of pre-operative SARS-CoV-2 infection"
# ##    2. "0-2 weeks record of pre-operative SARS-CoV-2 infection"
# ##    3. "3-4 weeks record of pre-operative SARS-CoV-2 infection"
# ##    4. ">=7 weeks record of pre-operative SARS-CoV-2 infection"
# ##    5. "Error: Test result after surgery. Check study_definition."
# ## ...stratified by...
# ## - surgery era:
# ##    1. "preCOVID sugery"
# ##    2. "postVaccine surgery" (although labelled "post", this means during, too)
# ##    3. "No surgery"
# ## - and whether or not the patient died within 30 days of their surgery:
# ##    1. "Alive within 30-day post-operation"
# ##    2. "Dead within 30-day post-operation" 
# ##    3. "Error: Surgery after death"
# ##    4. "No surgery recorded"
# ##    5. "No death recorded"
SNOMED_tableVacc_postOp_mortality_30day <- 
  myData_3mths_vacc %>% dplyr::group_by(surgery_pre_or_post_vaccine_UK,
                                        SNOMED_category_vaccination_status_before_test,
                                        postOp_mortality_30day) %>%
  dplyr::summarise(n_per_group = sum(ifelse(preOperative_infection_status!=
                                              "Error: Test result after surgery. Check study_definition.",1,0)),
                   n_infection_none = sum(ifelse(preOperative_infection_status==
                                                   "No record of pre-operative SARS-CoV-2 infection",1,0)),
                   n_infection_0to2wk = sum(ifelse(preOperative_infection_status==
                                                     "0-2 weeks record of pre-operative SARS-CoV-2 infection",1,0)),
                   n_infection_3to4wk = sum(ifelse(preOperative_infection_status==
                                                     "3-4 weeks record of pre-operative SARS-CoV-2 infection",1,0)),
                   n_infection_5to6wk = sum(ifelse(preOperative_infection_status==
                                                     "5-6 weeks record of pre-operative SARS-CoV-2 infection",1,0)),
                   n_infection_7wk = sum(ifelse(preOperative_infection_status==
                                                  ">=7 weeks record of pre-operative SARS-CoV-2 infection",1,0))
  )
names(SNOMED_tableVacc_postOp_mortality_30day)[names(SNOMED_tableVacc_postOp_mortality_30day) ==
                                                 'SNOMED_category_vaccination_status_before_test'] <-
                                                    'category_vaccination_status_before_test'
TPP_tableVacc_postOp_mortality_30day <- 
  myData_3mths_vacc %>% dplyr::group_by(surgery_pre_or_post_vaccine_UK,
                                        TPP_category_vaccination_status_before_test,
                                        postOp_mortality_30day) %>%
  dplyr::summarise(n_per_group = sum(ifelse(preOperative_infection_status!=
                                              "Error: Test result after surgery. Check study_definition.",1,0)),
                   n_infection_none = sum(ifelse(preOperative_infection_status==
                                                   "No record of pre-operative SARS-CoV-2 infection",1,0)),
                   n_infection_0to2wk = sum(ifelse(preOperative_infection_status==
                                                     "0-2 weeks record of pre-operative SARS-CoV-2 infection",1,0)),
                   n_infection_3to4wk = sum(ifelse(preOperative_infection_status==
                                                     "3-4 weeks record of pre-operative SARS-CoV-2 infection",1,0)),
                   n_infection_5to6wk = sum(ifelse(preOperative_infection_status==
                                                     "5-6 weeks record of pre-operative SARS-CoV-2 infection",1,0)),
                   n_infection_7wk = sum(ifelse(preOperative_infection_status==
                                                  ">=7 weeks record of pre-operative SARS-CoV-2 infection",1,0))
  )
names(TPP_tableVacc_postOp_mortality_30day)[names(TPP_tableVacc_postOp_mortality_30day) ==
                                               'TPP_category_vaccination_status_before_test'] <-
                                                    'category_vaccination_status_before_test'
AlwynSNOMED_tableVacc_postOp_mortality_30day <- 
  myData_3mths_vacc %>% dplyr::group_by(surgery_pre_or_post_vaccine_UK,
                                        AlwynSNOMED_category_vaccination_status_before_test,
                                        postOp_mortality_30day) %>%
  dplyr::summarise(n_per_group = sum(ifelse(preOperative_infection_status!=
                                              "Error: Test result after surgery. Check study_definition.",1,0)),
                   n_infection_none = sum(ifelse(preOperative_infection_status==
                                                   "No record of pre-operative SARS-CoV-2 infection",1,0)),
                   n_infection_0to2wk = sum(ifelse(preOperative_infection_status==
                                                     "0-2 weeks record of pre-operative SARS-CoV-2 infection",1,0)),
                   n_infection_3to4wk = sum(ifelse(preOperative_infection_status==
                                                     "3-4 weeks record of pre-operative SARS-CoV-2 infection",1,0)),
                   n_infection_5to6wk = sum(ifelse(preOperative_infection_status==
                                                     "5-6 weeks record of pre-operative SARS-CoV-2 infection",1,0)),
                   n_infection_7wk = sum(ifelse(preOperative_infection_status==
                                                  ">=7 weeks record of pre-operative SARS-CoV-2 infection",1,0))
  )
names(AlwynSNOMED_tableVacc_postOp_mortality_30day)[names(AlwynSNOMED_tableVacc_postOp_mortality_30day) ==
                                                 'AlwynSNOMED_category_vaccination_status_before_test'] <-
  'category_vaccination_status_before_test'
AlwynTPP_tableVacc_postOp_mortality_30day <- 
  myData_3mths_vacc %>% dplyr::group_by(surgery_pre_or_post_vaccine_UK,
                                        AlwynTPP_category_vaccination_status_before_test,
                                        postOp_mortality_30day) %>%
  dplyr::summarise(n_per_group = sum(ifelse(preOperative_infection_status!=
                                              "Error: Test result after surgery. Check study_definition.",1,0)),
                   n_infection_none = sum(ifelse(preOperative_infection_status==
                                                   "No record of pre-operative SARS-CoV-2 infection",1,0)),
                   n_infection_0to2wk = sum(ifelse(preOperative_infection_status==
                                                     "0-2 weeks record of pre-operative SARS-CoV-2 infection",1,0)),
                   n_infection_3to4wk = sum(ifelse(preOperative_infection_status==
                                                     "3-4 weeks record of pre-operative SARS-CoV-2 infection",1,0)),
                   n_infection_5to6wk = sum(ifelse(preOperative_infection_status==
                                                     "5-6 weeks record of pre-operative SARS-CoV-2 infection",1,0)),
                   n_infection_7wk = sum(ifelse(preOperative_infection_status==
                                                  ">=7 weeks record of pre-operative SARS-CoV-2 infection",1,0))
  )
names(AlwynTPP_tableVacc_postOp_mortality_30day)[names(AlwynTPP_tableVacc_postOp_mortality_30day) ==
                                              'AlwynTPP_category_vaccination_status_before_test'] <-
  'category_vaccination_status_before_test'
# ----

#######################################################################
# Ensure tibbles show zero values when categories are not in the data #
#######################################################################
# ----
SNOMED_tableVacc_postOp_mortality_30day <- 
  expand.grid(
    surgery_pre_or_post_vaccine_UK = 
      c("No surgery", "preVaccine surgery", "postVaccine surgery"),
    category_vaccination_status_before_test = 
      c("Confirmed fully vaccinated before test",
        "Confirmed partially vaccinated before test",
        "At least partially vaccinated before test",
        "Confirmed not vaccinated before test",
        "Unknown: No data for 1st dose and 2nd dose is FALSE or also missing",
        "Error: No data on vaccine administration",
        "Unknown vaccination status before test"),
    postOp_mortality_30day = 
      c("Alive within 30-day post-operation",
        "Dead within 30-day post-operation",
        "Error: Surgery after death",
        "No death recorded",
        "No surgery recorded",
        "Missing")) %>%
  dplyr::full_join(SNOMED_tableVacc_postOp_mortality_30day) %>%
  dplyr::arrange(surgery_pre_or_post_vaccine_UK) %>%
  tidyr::replace_na(list("n_per_group" = 0,
                         "n_infection_none" = 0,
                         "n_infection_0to2wk" = 0,
                         "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0,
                         "n_infection_7wk" = 0))
TPP_tableVacc_postOp_mortality_30day <- 
  expand.grid(
    surgery_pre_or_post_vaccine_UK = 
      c("No surgery", "preVaccine surgery", "postVaccine surgery"),
    category_vaccination_status_before_test = 
      c("Confirmed fully vaccinated before test",
        "Confirmed partially vaccinated before test",
        "At least partially vaccinated before test",
        "Unknown: No data for 1st dose and 2nd dose is FALSE or also missing",
        "Error: No data on vaccine administration",
        "Unknown vaccination status before test"),
    postOp_mortality_30day = 
      c("Alive within 30-day post-operation",
        "Dead within 30-day post-operation",
        "Error: Surgery after death",
        "No death recorded",
        "No surgery recorded",
        "Missing")) %>%
  dplyr::full_join(TPP_tableVacc_postOp_mortality_30day) %>%
  dplyr::arrange(surgery_pre_or_post_vaccine_UK) %>%
  tidyr::replace_na(list("n_per_group" = 0,
                         "n_infection_none" = 0,
                         "n_infection_0to2wk" = 0,
                         "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0,
                         "n_infection_7wk" = 0))
AlwynSNOMED_tableVacc_postOp_mortality_30day <- 
  expand.grid(
    surgery_pre_or_post_vaccine_UK = 
      c("No surgery", "preVaccine surgery", "postVaccine surgery"),
    category_vaccination_status_before_test = 
      c("Confirmed fully vaccinated before test",
        "Double vaccination before test not confirmed"),
    postOp_mortality_30day = 
      c("Alive within 30-day post-operation",
        "Dead within 30-day post-operation",
        "Error: Surgery after death",
        "No death recorded",
        "No surgery recorded",
        "Missing")) %>%
  dplyr::full_join(AlwynSNOMED_tableVacc_postOp_mortality_30day) %>%
  dplyr::arrange(surgery_pre_or_post_vaccine_UK) %>%
  tidyr::replace_na(list("n_per_group" = 0,
                         "n_infection_none" = 0,
                         "n_infection_0to2wk" = 0,
                         "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0,
                         "n_infection_7wk" = 0))
AlwynTPP_tableVacc_postOp_mortality_30day <- 
  expand.grid(
    surgery_pre_or_post_vaccine_UK = 
      c("No surgery", "preVaccine surgery", "postVaccine surgery"),
    category_vaccination_status_before_test = 
      c("Confirmed fully vaccinated before test",
        "Double vaccination before test not confirmed"),
    postOp_mortality_30day = 
      c("Alive within 30-day post-operation",
        "Dead within 30-day post-operation",
        "Error: Surgery after death",
        "No death recorded",
        "No surgery recorded",
        "Missing")) %>%
  dplyr::full_join(AlwynTPP_tableVacc_postOp_mortality_30day) %>%
  dplyr::arrange(surgery_pre_or_post_vaccine_UK) %>%
  tidyr::replace_na(list("n_per_group" = 0,
                         "n_infection_none" = 0,
                         "n_infection_0to2wk" = 0,
                         "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0,
                         "n_infection_7wk" = 0))

# ----

#########################################################
# Save tibbles that will inform vectors for the tables. #
#########################################################
# ----
SNOMED_tableVacc_postOp_mortality_30day_3mths <- SNOMED_tableVacc_postOp_mortality_30day
write.csv(
  x = SNOMED_tableVacc_postOp_mortality_30day,
  file = here::here("output","SNOMED_tableVacc_postOp_mortality_30day_3mths.csv")
)
TPP_tableVacc_postOp_mortality_30day_3mths <- TPP_tableVacc_postOp_mortality_30day
write.csv(
  x = TPP_tableVacc_postOp_mortality_30day,
  file = here::here("output","TPP_tableVacc_postOp_mortality_30day_3mths.csv")
)
AlwynSNOMED_tableVacc_postOp_mortality_30day_3mths <- AlwynSNOMED_tableVacc_postOp_mortality_30day
write.csv(
  x = AlwynSNOMED_tableVacc_postOp_mortality_30day,
  file = here::here("output","AlwynSNOMED_tableVacc_postOp_mortality_30day_3mths.csv")
)
AlwynTPP_tableVacc_postOp_mortality_30day_3mths <- AlwynTPP_tableVacc_postOp_mortality_30day
write.csv(
  x = AlwynTPP_tableVacc_postOp_mortality_30day,
  file = here::here("output","AlwynTPP_tableVacc_postOp_mortality_30day_3mths.csv")
)
# ----


################
# Make tables. #
################
# ----
source(here::here("analysis","fnc_makeVaccTable.R"))
# Make SNOMED table.
makeVaccTable(SNOMED_tableVacc_postOp_mortality_30day, "SNOMED")
# Make TPP table.
makeVaccTable(TPP_tableVacc_postOp_mortality_30day, "TPP")
# Make AlwynSNOMED table.
makeVaccTable(AlwynSNOMED_tableVacc_postOp_mortality_30day, "AlwynSNOMED")
# Make AlwynTPP table.
makeVaccTable(AlwynTPP_tableVacc_postOp_mortality_30day, "AlwynTPP")

# ----
