# Make_Table1_4wk_onboarding.R
#
# This script processes data from the myData dataframe to create a table
# in the style of Table 1 from the primary publication that this project is
# emulating (doi: 10.1111/anae.15458).
# The limited variables displayed in the table are those chosen for the 
# 4-week on-boarding period in which the research team were inducted into the
# proper use of the OpenSAFELY platform.
# See Make_Table1_complete.R for similarly-structured R syntax to make a table
# that displays all relevant variables.
#
# 
# # If ever running locally.
# list_of_packages <- c("tidyverse", "lubridate", "kableExtra",
#                       "webshot", "magick", "here")
# new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
# if(length(new_packages)) install.packages(new_packages)
# for (i in 1:length(list_of_packages))
# {
#   library(list_of_packages[i],character.only = T)
# }


################################################
# Make tibbles that will inform Table 1, for   #
# the data relating to the 4 week on-boarding. #
################################################
# ----
# ## Count  of patients in each of the categories for 
# ## pre-operative infection status:
# ##    1. "No record of pre-operative SARS-CoV-2 infection"
# ##    2. "0-2 weeks record of pre-operative SARS-CoV-2 infection"
# ##    3. "3-4 weeks record of pre-operative SARS-CoV-2 infection"
# ##    4. "5-6 weeks record of pre-operative SARS-CoV-2 infection"
# ##    5. ">=7 weeks record of pre-operative SARS-CoV-2 infection"
# ##    6. "Error: Test result after surgery. Check study_definition."
# ##
# ## The counts are also stratified by surgery era:
# ##    1. Pre-pandemic
# ##    2. Pandemic no vaccine
# ##    3. Pandemic wiht vaccine
table1_totals_preOp_infection_status <- 
  data_to_use %>% dplyr::group_by(era,
                      preOperative_infection_status) %>% dplyr::summarise(n = n())
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by age band:
# ##    1. 0-29
# ##    2. 30-49
# ##    3. 50-69
# ##    4. 70-79
# ##    5. 80+
table1_ageGroup <- 
  data_to_use %>% dplyr::group_by(era, age_group_surgery) %>%
  dplyr::summarise(n_all_intervals = sum(ifelse(preOperative_infection_status!=
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
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by sex:
# ##    1. Female
# ##    2. Male
table1_Sex <- 
  data_to_use %>% dplyr::group_by(era, Sex) %>%
  dplyr::summarise(n_all_intervals = sum(ifelse(preOperative_infection_status!=
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
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by whether
# ## or not the patient died within 30 days of their surgery:
# ##    1. "Alive within 30 days post-operation"
# ##    2. "Dead within 30 days post-operation" 
# ##    3. "Error: Surgery after death"
# ##    4. "No surgery recorded"
# ##    5. "No death recorded"
table1_postOp_mortality_30day <- 
  data_to_use %>% dplyr::group_by(era, postOp_mortality_30day) %>%
  dplyr::summarise(n_all_intervals = sum(ifelse(preOperative_infection_status!=
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
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by whether
# ## or not the patient died within 90 days of their surgery:
# ##    1. "Alive within 90 days post-operation"
# ##    2. "Dead within 90 days post-operation" 
# ##    3. "Error: Surgery after death"
# ##    4. "No surgery recorded"
# ##    5. "No death recorded"
table1_postOp_mortality_90day <- 
  data_to_use %>% dplyr::group_by(era, postOp_mortality_90day) %>%
  dplyr::summarise(n_all_intervals = sum(ifelse(preOperative_infection_status!=
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
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by whether
# ## or not the patient died within 6 months of their surgery:
# ##    1. "Alive within 6 months post-operation"
# ##    2. "Dead within 6 months post-operation" 
# ##    3. "Error: Surgery after death"
# ##    4. "No surgery recorded"
# ##    5. "No death recorded"
table1_postOp_mortality_6mth <- 
  data_to_use %>% dplyr::group_by(era, postOp_mortality_6mth) %>%
  dplyr::summarise(n_all_intervals = sum(ifelse(preOperative_infection_status!=
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
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by whether
# ## or not the patient died within 12 months of their surgery:
# ##    1. "Alive within 12 months post-operation"
# ##    2. "Dead within 12 months post-operation" 
# ##    3. "Error: Surgery after death"
# ##    4. "No surgery recorded"
# ##    5. "No death recorded"
table1_postOp_mortality_12mth <- 
  data_to_use %>% dplyr::group_by(era, postOp_mortality_12mth) %>%
  dplyr::summarise(n_all_intervals = sum(ifelse(preOperative_infection_status!=
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
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by whether
# ## or not the patient had a record of chronic cardiac disease before their surgery.
# ##    1. Yes
# ##    1. No
table1_chronic_cardiac_disease <- 
  data_to_use %>% dplyr::group_by(era, chronic_cardiac_disease) %>%
  dplyr::summarise(n_all_intervals = sum(ifelse(preOperative_infection_status!=
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
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by whether
# ## or not the patient had a record of diabetes before their surgery.
# ##    1. Yes
# ##    1. No
table1_diabetes <- 
  data_to_use %>% dplyr::group_by(era, diabetes) %>%
  dplyr::summarise(n_all_intervals = sum(ifelse(preOperative_infection_status!=
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
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by whether
# ## or not the patient had a record of chronic respiratory disease before their surgery.
# ##    1. Yes
# ##    1. No
table1_chronic_respiratory_disease <- 
  data_to_use %>% dplyr::group_by(era, chronic_respiratory_disease) %>%
  dplyr::summarise(n_all_intervals = sum(ifelse(preOperative_infection_status!=
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
# ----

#######################################################################
# Ensure tibbles show zero values when categories are not in the data #
#######################################################################
# ----
# ## table1_totals_preOp_infection_status.
table1_totals_preOp_infection_status <- 
            expand.grid(
              era = 
                c("Error: No surgery", "Pre-pandemic", "Pandemic no vaccine", "Pandemic with vaccine"),
              "preOperative_infection_status" = 
                c("Error: Test result after surgery. Check study_definition.",
                  "No record of pre-operative SARS-CoV-2 infection",
                  "0-2 weeks record of pre-operative SARS-CoV-2 infection",
                  "3-4 weeks record of pre-operative SARS-CoV-2 infection",
                  "5-6 weeks record of pre-operative SARS-CoV-2 infection",
                  ">=7 weeks record of pre-operative SARS-CoV-2 infection")) %>%
            dplyr::full_join(table1_totals_preOp_infection_status) %>%
            dplyr::arrange(era) %>%
            tidyr::replace_na(list("n" = 0))
# ## table1_ageGroup.
table1_ageGroup <- 
            expand.grid(
              era = 
                c("Error: No surgery", "Pre-pandemic", "Pandemic no vaccine", "Pandemic with vaccine"),
              age_group_surgery = 
                c("0-29",
                  "30-49",
                  "50-69",
                  "70-79",
                  "80+",
                  "Missing")) %>%
              dplyr::full_join(table1_ageGroup) %>%
              dplyr::arrange(era) %>%
              tidyr::replace_na(list("n_all_intervals" = 0,
                                     "n_infection_none" = 0,
                                     "n_infection_0to2wk" = 0,
                                     "n_infection_3to4wk" = 0,
                                     "n_infection_5to6wk" = 0,
                                     "n_infection_7wk" = 0))
# ## table1_Sex.
table1_Sex <- 
            expand.grid(
              era = 
                c("Error: No surgery", "Pre-pandemic", "Pandemic no vaccine", "Pandemic with vaccine"),
              Sex = 
                c("Female",
                  "Male",
                  "Missing")) %>%
            dplyr::full_join(table1_Sex) %>%
            dplyr::arrange(era) %>%
            tidyr::replace_na(list("n_all_intervals" = 0,
                                   "n_infection_none" = 0,
                                   "n_infection_0to2wk" = 0,
                                   "n_infection_3to4wk" = 0,
                                   "n_infection_5to6wk" = 0,
                                   "n_infection_7wk" = 0))
# ## table1_postOp_mortality_30day.
table1_postOp_mortality_30day <- 
            expand.grid(
              era = 
                c("Error: No surgery", "Pre-pandemic", "Pandemic no vaccine", "Pandemic with vaccine"),
              postOp_mortality_30day = 
                c("Alive within 30 days post-operation",
                  "Dead within 30 days post-operation",
                  "Error: Surgery after death",
                  "No death recorded",
                  "No surgery recorded",
                  "Missing")) %>%
            dplyr::full_join(table1_postOp_mortality_30day) %>%
            dplyr::arrange(era) %>%
            tidyr::replace_na(list("n_all_intervals" = 0,
                                   "n_infection_none" = 0,
                                   "n_infection_0to2wk" = 0,
                                   "n_infection_3to4wk" = 0,
                                   "n_infection_5to6wk" = 0,
                                   "n_infection_7wk" = 0))
# ## table1_postOp_mortality_90day.
table1_postOp_mortality_90day <- 
  expand.grid(
    era = 
      c("Error: No surgery", "Pre-pandemic", "Pandemic no vaccine", "Pandemic with vaccine"),
    postOp_mortality_90day = 
      c("Alive within 90 days post-operation",
        "Dead within 90 days post-operation",
        "Error: Surgery after death",
        "No death recorded",
        "No surgery recorded",
        "Missing")) %>%
  dplyr::full_join(table1_postOp_mortality_90day) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(list("n_all_intervals" = 0,
                         "n_infection_none" = 0,
                         "n_infection_0to2wk" = 0,
                         "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0,
                         "n_infection_7wk" = 0))
# ## table1_postOp_mortality_6mth.
table1_postOp_mortality_6mth <- 
  expand.grid(
    era = 
      c("Error: No surgery", "Pre-pandemic", "Pandemic no vaccine", "Pandemic with vaccine"),
    postOp_mortality_6mth = 
      c("Alive within 6 months post-operation",
        "Dead within 6 months post-operation",
        "Error: Surgery after death",
        "No death recorded",
        "No surgery recorded",
        "Missing")) %>%
  dplyr::full_join(table1_postOp_mortality_6mth) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(list("n_all_intervals" = 0,
                         "n_infection_none" = 0,
                         "n_infection_0to2wk" = 0,
                         "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0,
                         "n_infection_7wk" = 0))
# ## table1_postOp_mortality_12mth.
table1_postOp_mortality_12mth <- 
  expand.grid(
    era = 
      c("Error: No surgery", "Pre-pandemic", "Pandemic no vaccine", "Pandemic with vaccine"),
    postOp_mortality_12mth = 
      c("Alive within 12 months post-operation",
        "Dead within 12 months post-operation",
        "Error: Surgery after death",
        "No death recorded",
        "No surgery recorded",
        "Missing")) %>%
  dplyr::full_join(table1_postOp_mortality_12mth) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(list("n_all_intervals" = 0,
                         "n_infection_none" = 0,
                         "n_infection_0to2wk" = 0,
                         "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0,
                         "n_infection_7wk" = 0))
# ## table1_chronic_cardiac_disease.
table1_chronic_cardiac_disease <- 
  expand.grid(
    era = 
      c("Error: No surgery", "Pre-pandemic", "Pandemic no vaccine", "Pandemic with vaccine"),
    chronic_cardiac_disease = 
      c("Yes",
        "No",
        "Missing")) %>%
  dplyr::full_join(table1_chronic_cardiac_disease) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(list("n_all_intervals" = 0,
                         "n_infection_none" = 0,
                         "n_infection_0to2wk" = 0,
                         "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0,
                         "n_infection_7wk" = 0))
# ## table1_diabetes.
table1_diabetes <- 
  expand.grid(
    era = 
      c("Error: No surgery", "Pre-pandemic", "Pandemic no vaccine", "Pandemic with vaccine"),
    diabetes = 
      c("Yes",
        "No",
        "Missing")) %>%
  dplyr::full_join(table1_diabetes) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(list("n_all_intervals" = 0,
                         "n_infection_none" = 0,
                         "n_infection_0to2wk" = 0,
                         "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0,
                         "n_infection_7wk" = 0))
# ## table1_chronic_respiratory_disease.
table1_chronic_respiratory_disease <- 
  expand.grid(
    era = 
      c("Error: No surgery", "Pre-pandemic", "Pandemic no vaccine", "Pandemic with vaccine"),
    chronic_respiratory_disease = 
      c("Yes",
        "No",
        "Missing")) %>%
  dplyr::full_join(table1_chronic_respiratory_disease) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(list("n_all_intervals" = 0,
                         "n_infection_none" = 0,
                         "n_infection_0to2wk" = 0,
                         "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0,
                         "n_infection_7wk" = 0))
# ----

#######################################################
# Save tibbles that will inform vectors for the table #
#######################################################
# ----
# write.csv(
#   x = table1_ageGroup,
#   file = here::here("output",paste0("table1_ageGroup",sensitivity_cohort,".csv")))
# )
# write.csv(
#   x = table1_Sex,
#   file = here::here("output",paste0("table1_Sex",sensitivity_cohort,".csv"))
# )
# write.csv(
#   x = table1_postOp_mortality_30day,)
#   file = here::here("output",paste0("table1_postOp_mortality_30day",sensitivity_cohort,".csv")
# )
# write.csv(
#   x = table1_postOp_mortality_90day,
#   file = here::here("output",paste0("table1_postOp_mortality_90day",sensitivity_cohort,".csv"))
# )
# write.csv(
#   x = table1_postOp_mortality_6mth,
#   file = here::here("output",paste0("table1_postOp_mortality_6mth",sensitivity_cohort,".csv"))
# )
# write.csv(
#   x = table1_postOp_mortality_12mth,
#   file = here::here("output",paste0("table1_postOp_mortality_12mth",sensitivity_cohort,".csv"))
# )
# write.csv(
#   x = table1_chronic_cardiac_disease,
#   file = here::here("output",paste0("table1_chronic_cardiac_disease",sensitivity_cohort,".csv"))
# )
# write.csv(
#   x = table1_diabetes,
#   file = here::here("output",paste0("table1_diabetes",sensitivity_cohort,".csv"))
# )
# write.csv(
#   x = table1_chronic_respiratory_disease,
#   file = here::here("output",paste0("table1_chronic_respiratory_disease",sensitivity_cohort,".csv"))
# )
# ----

######################################
# Make vectors to inform the tables. #
######################################
# Age band. ----
# ## Pre-pandemic
# ## ## Get counts per intervals and overall.
PP_n_ageGroup <- 
  table1_ageGroup %>% dplyr::filter(era=="Pre-pandemic", age_group_surgery !="Missing") %>%
  dplyr::arrange(age_group_surgery) %>% dplyr::ungroup() %>% dplyr::select("n_all_intervals")
# ## ## Get percentages per intervals and overall.
PP_pct_ageGroup <- (PP_n_ageGroup / sum(PP_n_ageGroup)) %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0)) %>%
  `colnames<-`(c("pct_all_intervals"))
PP_ageGroup <- table1_ageGroup %>%  dplyr::filter(era=="Pre-pandemic", age_group_surgery !="Missing") %>%
  dplyr::arrange(age_group_surgery) %>% dplyr::select("age_group_surgery") %>% dplyr::bind_cols(., PP_n_ageGroup, PP_pct_ageGroup)
# ## ## Clean up.
rm(PP_n_ageGroup, PP_pct_ageGroup)

# ## Pandemic no vaccine.
# ## ## Get counts per intervals and overall.
PNV_n_ageGroup <- 
  table1_ageGroup %>%  dplyr::filter(era=="Pandemic no vaccine", age_group_surgery !="Missing") %>%
  dplyr::arrange(age_group_surgery) %>% dplyr::ungroup() %>% dplyr::select(-c("era", "age_group_surgery"))
# ## ## Get percentages per intervals and overall.
PNV_pct_ageGroup <- 
  table1_ageGroup %>% dplyr::filter(era=="Pandemic no vaccine", age_group_surgery !="Missing") %>% select(-c("era", "age_group_surgery")) %>%
  colSums() %>% sweep(PNV_n_ageGroup, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0
                         )) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                   "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
PNV_ageGroup <- matrix(0,
                      nrow = length(rownames(PNV_n_ageGroup)),
                      ncol = length(colnames(PNV_n_ageGroup))*2) %>%
               as.data.frame()
PNV_ageGroup[,seq(1,length(colnames(PNV_ageGroup)),2)] <- PNV_n_ageGroup
PNV_ageGroup[,seq(2,length(colnames(PNV_ageGroup)),2)] <- PNV_pct_ageGroup
colnames(PNV_ageGroup)[seq(1,length(colnames(PNV_ageGroup)),2)] <- colnames(PNV_n_ageGroup)
colnames(PNV_ageGroup)[seq(2,length(colnames(PNV_ageGroup)),2)] <- colnames(PNV_pct_ageGroup)
PNV_ageGroup <- table1_ageGroup %>%  dplyr::filter(era=="Pandemic no vaccine", age_group_surgery !="Missing") %>%
  dplyr::arrange(age_group_surgery) %>% dplyr::select("age_group_surgery") %>% dplyr::bind_cols(PNV_ageGroup)
# ## ## Clean up.
rm(PNV_n_ageGroup, PNV_pct_ageGroup)

# ## Pandemic with vaccine.
# ## ## Get counts per intervals and overall.
PWV_n_ageGroup <- 
  table1_ageGroup %>%  dplyr::filter(era=="Pandemic with vaccine", age_group_surgery !="Missing") %>%
  dplyr::arrange(age_group_surgery) %>% dplyr::ungroup() %>% dplyr::select(-c("era", "age_group_surgery"))
# ## ## Get percentages per intervals and overall.
PWV_pct_ageGroup <- 
  table1_ageGroup %>% dplyr::filter(era=="Pandemic with vaccine", age_group_surgery !="Missing") %>% select(-c("era", "age_group_surgery")) %>%
  colSums() %>% sweep(PWV_n_ageGroup, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0
  )) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
PWV_ageGroup <- matrix(0,
                       nrow = length(rownames(PWV_n_ageGroup)),
                       ncol = length(colnames(PWV_n_ageGroup))*2) %>%
  as.data.frame()
PWV_ageGroup[,seq(1,length(colnames(PWV_ageGroup)),2)] <- PWV_n_ageGroup
PWV_ageGroup[,seq(2,length(colnames(PWV_ageGroup)),2)] <- PWV_pct_ageGroup
colnames(PWV_ageGroup)[seq(1,length(colnames(PWV_ageGroup)),2)] <- colnames(PWV_n_ageGroup)
colnames(PWV_ageGroup)[seq(2,length(colnames(PWV_ageGroup)),2)] <- colnames(PWV_pct_ageGroup)
PWV_ageGroup <- table1_ageGroup %>%  dplyr::filter(era=="Pandemic with vaccine", age_group_surgery !="Missing") %>%
  dplyr::arrange(age_group_surgery) %>% dplyr::select("age_group_surgery") %>% dplyr::bind_cols(PWV_ageGroup)
# ## ## Clean up
rm(PWV_n_ageGroup, PWV_pct_ageGroup)
# ----

# Sex. ----
# ## Pre-pandemic
# ## ## Get counts per intervals and overall.
PP_n_Sex <- 
  table1_Sex %>% dplyr::filter(era=="Pre-pandemic", Sex!="Missing") %>%
  dplyr::arrange(Sex) %>% dplyr::ungroup() %>% dplyr::select("n_all_intervals")
# ## ## Get percentages per intervals and overall.
PP_pct_Sex <- (PP_n_Sex / sum(PP_n_Sex)) %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0)) %>%
  `colnames<-`(c("pct_all_intervals"))
PP_Sex <- table1_Sex %>% dplyr::filter(era=="Pandemic no vaccine", Sex!="Missing") %>%
  dplyr::arrange(Sex) %>% dplyr::select("Sex") %>% dplyr::bind_cols(., PP_n_Sex, PP_pct_Sex)
# ## ## Clean up.
rm(PP_n_Sex, PP_pct_Sex)

# ## Pandemic no vaccine.
# ## ## Get counts per intervals and overall.
PNV_n_Sex <- 
  table1_Sex %>%  dplyr::filter(era=="Pandemic no vaccine", Sex!="Missing") %>%
  dplyr::arrange(Sex) %>% dplyr::ungroup() %>% dplyr::select(-c("era", "Sex"))
# ## ## Get percentages per intervals and overall.
PNV_pct_Sex <- 
  table1_Sex %>% dplyr::filter(era=="Pandemic no vaccine", Sex!="Missing") %>% select(-c("era", "Sex")) %>%
  colSums() %>% sweep(PNV_n_Sex, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0
  )) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
PNV_Sex <- matrix(0,
                       nrow = length(rownames(PNV_n_Sex)),
                       ncol = length(colnames(PNV_n_Sex))*2) %>%
  as.data.frame()
PNV_Sex[,seq(1,length(colnames(PNV_Sex)),2)] <- PNV_n_Sex
PNV_Sex[,seq(2,length(colnames(PNV_Sex)),2)] <- PNV_pct_Sex
colnames(PNV_Sex)[seq(1,length(colnames(PNV_Sex)),2)] <- colnames(PNV_n_Sex)
colnames(PNV_Sex)[seq(2,length(colnames(PNV_Sex)),2)] <- colnames(PNV_pct_Sex)
PNV_Sex <- table1_Sex %>%  dplyr::filter(era=="Pandemic no vaccine", Sex!="Missing") %>%
  dplyr::arrange(Sex) %>% dplyr::select("Sex") %>% dplyr::bind_cols(PNV_Sex)
# ## ## Clean up.
rm(PNV_n_Sex, PNV_pct_Sex)

# ## Pandemic with vaccine.
# ## ## Get counts per intervals and overall.
PWV_n_Sex <- 
  table1_Sex %>%  dplyr::filter(era=="Pandemic with vaccine", Sex!="Missing") %>%
  dplyr::arrange(Sex) %>% dplyr::ungroup() %>% dplyr::select(-c("era", "Sex"))
# ## ## Get percentages per intervals and overall.
PWV_pct_Sex <- 
  table1_Sex %>% dplyr::filter(era=="Pandemic with vaccine") %>% select(-c("era", "Sex")) %>%
  colSums() %>% sweep(PWV_n_Sex, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0
  )) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
PWV_Sex <- matrix(0,
                       nrow = length(rownames(PWV_n_Sex)),
                       ncol = length(colnames(PWV_n_Sex))*2) %>%
  as.data.frame()
PWV_Sex[,seq(1,length(colnames(PWV_Sex)),2)] <- PWV_n_Sex
PWV_Sex[,seq(2,length(colnames(PWV_Sex)),2)] <- PWV_pct_Sex
colnames(PWV_Sex)[seq(1,length(colnames(PWV_Sex)),2)] <- colnames(PWV_n_Sex)
colnames(PWV_Sex)[seq(2,length(colnames(PWV_Sex)),2)] <- colnames(PWV_pct_Sex)
PWV_Sex <- table1_Sex %>%  dplyr::filter(era=="Pandemic with vaccine", Sex!="Missing") %>%
  dplyr::arrange(Sex) %>% dplyr::select("Sex") %>% dplyr::bind_cols(PWV_Sex)
# ## ## Clean up
rm(PWV_n_Sex, PWV_pct_Sex)
# ----

# 30-day post-operative mortality. ----
# ## Pre-pandemic
# ## ## Get counts per intervals and overall.
PP_n_postOp_mortality_30day <- 
  table1_postOp_mortality_30day %>%
  dplyr::filter(era=="Pre-pandemic",
                (postOp_mortality_30day=="Alive within 30 days post-operation"|
                   postOp_mortality_30day=="Dead within 30 days post-operation")) %>%
  dplyr::arrange(postOp_mortality_30day) %>%
  dplyr::ungroup() %>% dplyr::select("n_all_intervals")
# ## ## Get percentages per intervals and overall.
PP_pct_postOp_mortality_30day <-
  (PP_n_postOp_mortality_30day / sum(PP_n_postOp_mortality_30day)) %>%
  "*"(100) %>% tidyr::replace_na(list("n_all_intervals" = 0)) %>%
  `colnames<-`(c("pct_all_intervals"))
# ## ## Bind the counts and percentages.
PP_postOp_mortality_30day <-
  table1_postOp_mortality_30day %>%
  dplyr::filter(era=="Pre-pandemic",
                (postOp_mortality_30day=="Alive within 30 days post-operation"|
                   postOp_mortality_30day=="Dead within 30 days post-operation")) %>%
  dplyr::arrange(postOp_mortality_30day) %>%
  dplyr::select("postOp_mortality_30day") %>%
  dplyr::bind_cols(PP_n_postOp_mortality_30day, PP_pct_postOp_mortality_30day)
# ## ## Clean up.
rm(PP_n_postOp_mortality_30day, PP_pct_postOp_mortality_30day)

# ## Pandemic no vaccine.
# ## ## Get counts per intervals and overall.
PNV_n_postOp_mortality_30day <- 
  table1_postOp_mortality_30day %>%
  dplyr::filter(era=="Pandemic no vaccine",
                (postOp_mortality_30day=="Alive within 30 days post-operation"|
                   postOp_mortality_30day=="Dead within 30 days post-operation")) %>%
  dplyr::arrange(postOp_mortality_30day) %>%
  dplyr::ungroup() %>% dplyr::select(-c("era", "postOp_mortality_30day"))
# ## ## Get percentages per intervals and overall.
PNV_pct_postOp_mortality_30day <- 
  table1_postOp_mortality_30day %>%
  dplyr::filter(era=="Pandemic no vaccine",
                (postOp_mortality_30day=="Alive within 30 days post-operation"|
                   postOp_mortality_30day=="Dead within 30 days post-operation")) %>%
  select(-c("era", "postOp_mortality_30day")) %>%
  colSums() %>% sweep(PNV_n_postOp_mortality_30day, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0)) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
PNV_postOp_mortality_30day <-
  matrix(0,
         nrow = length(rownames(PNV_n_postOp_mortality_30day)),
         ncol = length(colnames(PNV_n_postOp_mortality_30day))*2) %>%
  as.data.frame()
PNV_postOp_mortality_30day[,seq(1,length(colnames(PNV_postOp_mortality_30day)),2)] <- PNV_n_postOp_mortality_30day
PNV_postOp_mortality_30day[,seq(2,length(colnames(PNV_postOp_mortality_30day)),2)] <- PNV_pct_postOp_mortality_30day
colnames(PNV_postOp_mortality_30day)[seq(1,length(colnames(PNV_postOp_mortality_30day)),2)] <- colnames(PNV_n_postOp_mortality_30day)
colnames(PNV_postOp_mortality_30day)[seq(2,length(colnames(PNV_postOp_mortality_30day)),2)] <- colnames(PNV_pct_postOp_mortality_30day)
PNV_postOp_mortality_30day <-
  table1_postOp_mortality_30day %>%
  dplyr::filter(era=="Pandemic no vaccine",
                (postOp_mortality_30day=="Alive within 30 days post-operation"|
                   postOp_mortality_30day=="Dead within 30 days post-operation")) %>%
  dplyr::arrange(postOp_mortality_30day) %>%
  dplyr::select("postOp_mortality_30day") %>%
  dplyr::bind_cols(PNV_postOp_mortality_30day)
# ## ## Clean up.
rm(PNV_n_postOp_mortality_30day, PNV_pct_postOp_mortality_30day)

# ## Pandemic with vaccine.
# ## ## Get counts per intervals and overall.
PWV_n_postOp_mortality_30day <- 
  table1_postOp_mortality_30day %>%
  dplyr::filter(era=="Pandemic with vaccine",
                (postOp_mortality_30day=="Alive within 30 days post-operation"|
                   postOp_mortality_30day=="Dead within 30 days post-operation")) %>%
  dplyr::arrange(postOp_mortality_30day) %>%
  dplyr::ungroup() %>% dplyr::select(-c("era", "postOp_mortality_30day"))
# ## ## Get percentages per intervals and overall.
PWV_pct_postOp_mortality_30day <- 
  table1_postOp_mortality_30day %>%
  dplyr::filter(era=="Pandemic with vaccine",
                (postOp_mortality_30day=="Alive within 30 days post-operation"|
                   postOp_mortality_30day=="Dead within 30 days post-operation")) %>%
  select(-c("era", "postOp_mortality_30day")) %>%
  colSums() %>% sweep(PWV_n_postOp_mortality_30day, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0
  )) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
PWV_postOp_mortality_30day <-
  matrix(0,
         length(rownames(PWV_n_postOp_mortality_30day)),
         ncol = length(colnames(PWV_n_postOp_mortality_30day))*2) %>%
  as.data.frame()
PWV_postOp_mortality_30day[,seq(1,length(colnames(PWV_postOp_mortality_30day)),2)] <- PWV_n_postOp_mortality_30day
PWV_postOp_mortality_30day[,seq(2,length(colnames(PWV_postOp_mortality_30day)),2)] <- PWV_pct_postOp_mortality_30day
colnames(PWV_postOp_mortality_30day)[seq(1,length(colnames(PWV_postOp_mortality_30day)),2)] <- colnames(PWV_n_postOp_mortality_30day)
colnames(PWV_postOp_mortality_30day)[seq(2,length(colnames(PWV_postOp_mortality_30day)),2)] <- colnames(PWV_pct_postOp_mortality_30day)
PWV_postOp_mortality_30day <-
  table1_postOp_mortality_30day %>% 
  dplyr::filter(era=="Pandemic with vaccine",
                (postOp_mortality_30day=="Alive within 30 days post-operation"|
                   postOp_mortality_30day=="Dead within 30 days post-operation")) %>%
  dplyr::arrange(postOp_mortality_30day) %>%
  dplyr::select("postOp_mortality_30day") %>%
  dplyr::bind_cols(PWV_postOp_mortality_30day)
# ## ## Clean up
rm(PWV_n_postOp_mortality_30day, PWV_pct_postOp_mortality_30day)
# ----

# 90-day post-operative mortality. ----
# ## Pre-pandemic
# ## ## Get counts per intervals and overall.
PP_n_postOp_mortality_90day <- 
  table1_postOp_mortality_90day %>%
  dplyr::filter(era=="Pre-pandemic",
                (postOp_mortality_90day=="Alive within 90 days post-operation"|
                   postOp_mortality_90day=="Dead within 90 days post-operation")) %>%
  dplyr::arrange(postOp_mortality_90day) %>%
  dplyr::ungroup() %>% dplyr::select("n_all_intervals")
# ## ## Get percentages per intervals and overall.
PP_pct_postOp_mortality_90day <-
  (PP_n_postOp_mortality_90day / sum(PP_n_postOp_mortality_90day)) %>%
  "*"(100) %>% tidyr::replace_na(list("n_all_intervals" = 0)) %>%
  `colnames<-`(c("pct_all_intervals"))
# ## ## Bind the counts and percentages.
PP_postOp_mortality_90day <-
  table1_postOp_mortality_90day %>%
  dplyr::filter(era=="Pre-pandemic",
                (postOp_mortality_90day=="Alive within 90 days post-operation"|
                   postOp_mortality_90day=="Dead within 90 days post-operation")) %>%
  dplyr::arrange(postOp_mortality_90day) %>%
  dplyr::select("postOp_mortality_90day") %>%
  dplyr::bind_cols(PP_n_postOp_mortality_90day, PP_pct_postOp_mortality_90day)
# ## ## Clean up.
rm(PP_n_postOp_mortality_90day, PP_pct_postOp_mortality_90day)

# ## Pandemic no vaccine.
# ## ## Get counts per intervals and overall.
PNV_n_postOp_mortality_90day <- 
  table1_postOp_mortality_90day %>%
  dplyr::filter(era=="Pandemic no vaccine",
                (postOp_mortality_90day=="Alive within 90 days post-operation"|
                   postOp_mortality_90day=="Dead within 90 days post-operation")) %>%
  dplyr::arrange(postOp_mortality_90day) %>%
  dplyr::ungroup() %>% dplyr::select(-c("era", "postOp_mortality_90day"))
# ## ## Get percentages per intervals and overall.
PNV_pct_postOp_mortality_90day <- 
  table1_postOp_mortality_90day %>%
  dplyr::filter(era=="Pandemic no vaccine",
                (postOp_mortality_90day=="Alive within 90 days post-operation"|
                   postOp_mortality_90day=="Dead within 90 days post-operation")) %>%
  select(-c("era", "postOp_mortality_90day")) %>%
  colSums() %>% sweep(PNV_n_postOp_mortality_90day, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0)) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
PNV_postOp_mortality_90day <-
  matrix(0,
         nrow = length(rownames(PNV_n_postOp_mortality_90day)),
         ncol = length(colnames(PNV_n_postOp_mortality_90day))*2) %>%
  as.data.frame()
PNV_postOp_mortality_90day[,seq(1,length(colnames(PNV_postOp_mortality_90day)),2)] <- PNV_n_postOp_mortality_90day
PNV_postOp_mortality_90day[,seq(2,length(colnames(PNV_postOp_mortality_90day)),2)] <- PNV_pct_postOp_mortality_90day
colnames(PNV_postOp_mortality_90day)[seq(1,length(colnames(PNV_postOp_mortality_90day)),2)] <- colnames(PNV_n_postOp_mortality_90day)
colnames(PNV_postOp_mortality_90day)[seq(2,length(colnames(PNV_postOp_mortality_90day)),2)] <- colnames(PNV_pct_postOp_mortality_90day)
PNV_postOp_mortality_90day <-
  table1_postOp_mortality_90day %>%
  dplyr::filter(era=="Pandemic no vaccine",
                (postOp_mortality_90day=="Alive within 90 days post-operation"|
                   postOp_mortality_90day=="Dead within 90 days post-operation")) %>%
  dplyr::arrange(postOp_mortality_90day) %>%
  dplyr::select("postOp_mortality_90day") %>%
  dplyr::bind_cols(PNV_postOp_mortality_90day)
# ## ## Clean up.
rm(PNV_n_postOp_mortality_90day, PNV_pct_postOp_mortality_90day)

# ## Pandemic with vaccine.
# ## ## Get counts per intervals and overall.
PWV_n_postOp_mortality_90day <- 
  table1_postOp_mortality_90day %>%
  dplyr::filter(era=="Pandemic with vaccine",
                (postOp_mortality_90day=="Alive within 90 days post-operation"|
                   postOp_mortality_90day=="Dead within 90 days post-operation")) %>%
  dplyr::arrange(postOp_mortality_90day) %>%
  dplyr::ungroup() %>% dplyr::select(-c("era", "postOp_mortality_90day"))
# ## ## Get percentages per intervals and overall.
PWV_pct_postOp_mortality_90day <- 
  table1_postOp_mortality_90day %>%
  dplyr::filter(era=="Pandemic with vaccine",
                (postOp_mortality_90day=="Alive within 90 days post-operation"|
                   postOp_mortality_90day=="Dead within 90 days post-operation")) %>%
  select(-c("era", "postOp_mortality_90day")) %>%
  colSums() %>% sweep(PWV_n_postOp_mortality_90day, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0
  )) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
PWV_postOp_mortality_90day <-
  matrix(0,
         length(rownames(PWV_n_postOp_mortality_90day)),
         ncol = length(colnames(PWV_n_postOp_mortality_90day))*2) %>%
  as.data.frame()
PWV_postOp_mortality_90day[,seq(1,length(colnames(PWV_postOp_mortality_90day)),2)] <- PWV_n_postOp_mortality_90day
PWV_postOp_mortality_90day[,seq(2,length(colnames(PWV_postOp_mortality_90day)),2)] <- PWV_pct_postOp_mortality_90day
colnames(PWV_postOp_mortality_90day)[seq(1,length(colnames(PWV_postOp_mortality_90day)),2)] <- colnames(PWV_n_postOp_mortality_90day)
colnames(PWV_postOp_mortality_90day)[seq(2,length(colnames(PWV_postOp_mortality_90day)),2)] <- colnames(PWV_pct_postOp_mortality_90day)
PWV_postOp_mortality_90day <-
  table1_postOp_mortality_90day %>% 
  dplyr::filter(era=="Pandemic with vaccine",
                (postOp_mortality_90day=="Alive within 90 days post-operation"|
                   postOp_mortality_90day=="Dead within 90 days post-operation")) %>%
  dplyr::arrange(postOp_mortality_90day) %>%
  dplyr::select("postOp_mortality_90day") %>%
  dplyr::bind_cols(PWV_postOp_mortality_90day)
# ## ## Clean up
rm(PWV_n_postOp_mortality_90day, PWV_pct_postOp_mortality_90day)
# ----

# 6-month post-operative mortality. ----
# ## Pre-pandemic
# ## ## Get counts per intervals and overall.
PP_n_postOp_mortality_6mth <- 
  table1_postOp_mortality_6mth %>%
  dplyr::filter(era=="Pre-pandemic",
                (postOp_mortality_6mth=="Alive within 6 months post-operation"|
                   postOp_mortality_6mth=="Dead within 6 months post-operation")) %>%
  dplyr::arrange(postOp_mortality_6mth) %>%
  dplyr::ungroup() %>% dplyr::select("n_all_intervals")
# ## ## Get percentages per intervals and overall.
PP_pct_postOp_mortality_6mth <-
  (PP_n_postOp_mortality_6mth / sum(PP_n_postOp_mortality_6mth)) %>%
  "*"(100) %>% tidyr::replace_na(list("n_all_intervals" = 0)) %>%
  `colnames<-`(c("pct_all_intervals"))
# ## ## Bind the counts and percentages.
PP_postOp_mortality_6mth <-
  table1_postOp_mortality_6mth %>%
  dplyr::filter(era=="Pre-pandemic",
                (postOp_mortality_6mth=="Alive within 6 months post-operation"|
                   postOp_mortality_6mth=="Dead within 6 months post-operation")) %>%
  dplyr::arrange(postOp_mortality_6mth) %>%
  dplyr::select("postOp_mortality_6mth") %>%
  dplyr::bind_cols(PP_n_postOp_mortality_6mth, PP_pct_postOp_mortality_6mth)
# ## ## Clean up.
rm(PP_n_postOp_mortality_6mth, PP_pct_postOp_mortality_6mth)

# ## Pandemic no vaccine.
# ## ## Get counts per intervals and overall.
PNV_n_postOp_mortality_6mth <- 
  table1_postOp_mortality_6mth %>%
  dplyr::filter(era=="Pandemic no vaccine",
                (postOp_mortality_6mth=="Alive within 6 months post-operation"|
                   postOp_mortality_6mth=="Dead within 6 months post-operation")) %>%
  dplyr::arrange(postOp_mortality_6mth) %>%
  dplyr::ungroup() %>% dplyr::select(-c("era", "postOp_mortality_6mth"))
# ## ## Get percentages per intervals and overall.
PNV_pct_postOp_mortality_6mth <- 
  table1_postOp_mortality_6mth %>%
  dplyr::filter(era=="Pandemic no vaccine",
                (postOp_mortality_6mth=="Alive within 6 months post-operation"|
                   postOp_mortality_6mth=="Dead within 6 months post-operation")) %>%
  select(-c("era", "postOp_mortality_6mth")) %>%
  colSums() %>% sweep(PNV_n_postOp_mortality_6mth, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0)) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
PNV_postOp_mortality_6mth <-
  matrix(0,
         nrow = length(rownames(PNV_n_postOp_mortality_6mth)),
         ncol = length(colnames(PNV_n_postOp_mortality_6mth))*2) %>%
  as.data.frame()
PNV_postOp_mortality_6mth[,seq(1,length(colnames(PNV_postOp_mortality_6mth)),2)] <- PNV_n_postOp_mortality_6mth
PNV_postOp_mortality_6mth[,seq(2,length(colnames(PNV_postOp_mortality_6mth)),2)] <- PNV_pct_postOp_mortality_6mth
colnames(PNV_postOp_mortality_6mth)[seq(1,length(colnames(PNV_postOp_mortality_6mth)),2)] <- colnames(PNV_n_postOp_mortality_6mth)
colnames(PNV_postOp_mortality_6mth)[seq(2,length(colnames(PNV_postOp_mortality_6mth)),2)] <- colnames(PNV_pct_postOp_mortality_6mth)
PNV_postOp_mortality_6mth <-
  table1_postOp_mortality_6mth %>%
  dplyr::filter(era=="Pandemic no vaccine",
                (postOp_mortality_6mth=="Alive within 6 months post-operation"|
                   postOp_mortality_6mth=="Dead within 6 months post-operation")) %>%
  dplyr::arrange(postOp_mortality_6mth) %>%
  dplyr::select("postOp_mortality_6mth") %>%
  dplyr::bind_cols(PNV_postOp_mortality_6mth)
# ## ## Clean up.
rm(PNV_n_postOp_mortality_6mth, PNV_pct_postOp_mortality_6mth)

# ## Pandemic with vaccine.
# ## ## Get counts per intervals and overall.
PWV_n_postOp_mortality_6mth <- 
  table1_postOp_mortality_6mth %>%
  dplyr::filter(era=="Pandemic with vaccine",
                (postOp_mortality_6mth=="Alive within 6 months post-operation"|
                   postOp_mortality_6mth=="Dead within 6 months post-operation")) %>%
  dplyr::arrange(postOp_mortality_6mth) %>%
  dplyr::ungroup() %>% dplyr::select(-c("era", "postOp_mortality_6mth"))
# ## ## Get percentages per intervals and overall.
PWV_pct_postOp_mortality_6mth <- 
  table1_postOp_mortality_6mth %>%
  dplyr::filter(era=="Pandemic with vaccine",
                (postOp_mortality_6mth=="Alive within 6 months post-operation"|
                   postOp_mortality_6mth=="Dead within 6 months post-operation")) %>%
  select(-c("era", "postOp_mortality_6mth")) %>%
  colSums() %>% sweep(PWV_n_postOp_mortality_6mth, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0
  )) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
PWV_postOp_mortality_6mth <-
  matrix(0,
         length(rownames(PWV_n_postOp_mortality_6mth)),
         ncol = length(colnames(PWV_n_postOp_mortality_6mth))*2) %>%
  as.data.frame()
PWV_postOp_mortality_6mth[,seq(1,length(colnames(PWV_postOp_mortality_6mth)),2)] <- PWV_n_postOp_mortality_6mth
PWV_postOp_mortality_6mth[,seq(2,length(colnames(PWV_postOp_mortality_6mth)),2)] <- PWV_pct_postOp_mortality_6mth
colnames(PWV_postOp_mortality_6mth)[seq(1,length(colnames(PWV_postOp_mortality_6mth)),2)] <- colnames(PWV_n_postOp_mortality_6mth)
colnames(PWV_postOp_mortality_6mth)[seq(2,length(colnames(PWV_postOp_mortality_6mth)),2)] <- colnames(PWV_pct_postOp_mortality_6mth)
PWV_postOp_mortality_6mth <-
  table1_postOp_mortality_6mth %>% 
  dplyr::filter(era=="Pandemic with vaccine",
                (postOp_mortality_6mth=="Alive within 6 months post-operation"|
                   postOp_mortality_6mth=="Dead within 6 months post-operation")) %>%
  dplyr::arrange(postOp_mortality_6mth) %>%
  dplyr::select("postOp_mortality_6mth") %>%
  dplyr::bind_cols(PWV_postOp_mortality_6mth)
# ## ## Clean up
rm(PWV_n_postOp_mortality_6mth, PWV_pct_postOp_mortality_6mth)
# ----

# 12-month post-operative mortality. ----
# ## Pre-pandemic
# ## ## Get counts per intervals and overall.
PP_n_postOp_mortality_12mth <- 
  table1_postOp_mortality_12mth %>%
  dplyr::filter(era=="Pre-pandemic",
                (postOp_mortality_12mth=="Alive within 12 months post-operation"|
                   postOp_mortality_12mth=="Dead within 12 months post-operation")) %>%
  dplyr::arrange(postOp_mortality_12mth) %>%
  dplyr::ungroup() %>% dplyr::select("n_all_intervals")
# ## ## Get percentages per intervals and overall.
PP_pct_postOp_mortality_12mth <-
  (PP_n_postOp_mortality_12mth / sum(PP_n_postOp_mortality_12mth)) %>%
  "*"(100) %>% tidyr::replace_na(list("n_all_intervals" = 0)) %>%
  `colnames<-`(c("pct_all_intervals"))
# ## ## Bind the counts and percentages.
PP_postOp_mortality_12mth <-
  table1_postOp_mortality_12mth %>%
  dplyr::filter(era=="Pre-pandemic",
                (postOp_mortality_12mth=="Alive within 12 months post-operation"|
                   postOp_mortality_12mth=="Dead within 12 months post-operation")) %>%
  dplyr::arrange(postOp_mortality_12mth) %>%
  dplyr::select("postOp_mortality_12mth") %>%
  dplyr::bind_cols(PP_n_postOp_mortality_12mth, PP_pct_postOp_mortality_12mth)
# ## ## Clean up.
rm(PP_n_postOp_mortality_12mth, PP_pct_postOp_mortality_12mth)

# ## Pandemic no vaccine.
# ## ## Get counts per intervals and overall.
PNV_n_postOp_mortality_12mth <- 
  table1_postOp_mortality_12mth %>%
  dplyr::filter(era=="Pandemic no vaccine",
                (postOp_mortality_12mth=="Alive within 12 months post-operation"|
                   postOp_mortality_12mth=="Dead within 12 months post-operation")) %>%
  dplyr::arrange(postOp_mortality_12mth) %>%
  dplyr::ungroup() %>% dplyr::select(-c("era", "postOp_mortality_12mth"))
# ## ## Get percentages per intervals and overall.
PNV_pct_postOp_mortality_12mth <- 
  table1_postOp_mortality_12mth %>%
  dplyr::filter(era=="Pandemic no vaccine",
                (postOp_mortality_12mth=="Alive within 12 months post-operation"|
                   postOp_mortality_12mth=="Dead within 12 months post-operation")) %>%
  select(-c("era", "postOp_mortality_12mth")) %>%
  colSums() %>% sweep(PNV_n_postOp_mortality_12mth, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0)) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
PNV_postOp_mortality_12mth <-
  matrix(0,
         nrow = length(rownames(PNV_n_postOp_mortality_12mth)),
         ncol = length(colnames(PNV_n_postOp_mortality_12mth))*2) %>%
  as.data.frame()
PNV_postOp_mortality_12mth[,seq(1,length(colnames(PNV_postOp_mortality_12mth)),2)] <- PNV_n_postOp_mortality_12mth
PNV_postOp_mortality_12mth[,seq(2,length(colnames(PNV_postOp_mortality_12mth)),2)] <- PNV_pct_postOp_mortality_12mth
colnames(PNV_postOp_mortality_12mth)[seq(1,length(colnames(PNV_postOp_mortality_12mth)),2)] <- colnames(PNV_n_postOp_mortality_12mth)
colnames(PNV_postOp_mortality_12mth)[seq(2,length(colnames(PNV_postOp_mortality_12mth)),2)] <- colnames(PNV_pct_postOp_mortality_12mth)
PNV_postOp_mortality_12mth <-
  table1_postOp_mortality_12mth %>%
  dplyr::filter(era=="Pandemic no vaccine",
                (postOp_mortality_12mth=="Alive within 12 months post-operation"|
                   postOp_mortality_12mth=="Dead within 12 months post-operation")) %>%
  dplyr::arrange(postOp_mortality_12mth) %>%
  dplyr::select("postOp_mortality_12mth") %>%
  dplyr::bind_cols(PNV_postOp_mortality_12mth)
# ## ## Clean up.
rm(PNV_n_postOp_mortality_12mth, PNV_pct_postOp_mortality_12mth)

# ## Pandemic with vaccine.
# ## ## Get counts per intervals and overall.
PWV_n_postOp_mortality_12mth <- 
  table1_postOp_mortality_12mth %>%
  dplyr::filter(era=="Pandemic with vaccine",
                (postOp_mortality_12mth=="Alive within 12 months post-operation"|
                   postOp_mortality_12mth=="Dead within 12 months post-operation")) %>%
  dplyr::arrange(postOp_mortality_12mth) %>%
  dplyr::ungroup() %>% dplyr::select(-c("era", "postOp_mortality_12mth"))
# ## ## Get percentages per intervals and overall.
PWV_pct_postOp_mortality_12mth <- 
  table1_postOp_mortality_12mth %>%
  dplyr::filter(era=="Pandemic with vaccine",
                (postOp_mortality_12mth=="Alive within 12 months post-operation"|
                   postOp_mortality_12mth=="Dead within 12 months post-operation")) %>%
  select(-c("era", "postOp_mortality_12mth")) %>%
  colSums() %>% sweep(PWV_n_postOp_mortality_12mth, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0
  )) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
PWV_postOp_mortality_12mth <-
  matrix(0,
         length(rownames(PWV_n_postOp_mortality_12mth)),
         ncol = length(colnames(PWV_n_postOp_mortality_12mth))*2) %>%
  as.data.frame()
PWV_postOp_mortality_12mth[,seq(1,length(colnames(PWV_postOp_mortality_12mth)),2)] <- PWV_n_postOp_mortality_12mth
PWV_postOp_mortality_12mth[,seq(2,length(colnames(PWV_postOp_mortality_12mth)),2)] <- PWV_pct_postOp_mortality_12mth
colnames(PWV_postOp_mortality_12mth)[seq(1,length(colnames(PWV_postOp_mortality_12mth)),2)] <- colnames(PWV_n_postOp_mortality_12mth)
colnames(PWV_postOp_mortality_12mth)[seq(2,length(colnames(PWV_postOp_mortality_12mth)),2)] <- colnames(PWV_pct_postOp_mortality_12mth)
PWV_postOp_mortality_12mth <-
  table1_postOp_mortality_12mth %>% 
  dplyr::filter(era=="Pandemic with vaccine",
                (postOp_mortality_12mth=="Alive within 12 months post-operation"|
                   postOp_mortality_12mth=="Dead within 12 months post-operation")) %>%
  dplyr::arrange(postOp_mortality_12mth) %>%
  dplyr::select("postOp_mortality_12mth") %>%
  dplyr::bind_cols(PWV_postOp_mortality_12mth)
# ## ## Clean up
rm(PWV_n_postOp_mortality_12mth, PWV_pct_postOp_mortality_12mth)
# ----

# Chronic cardiac disease. ----
# ## Pre-pandemic
# ## ## Get counts per intervals and overall.
PP_n_chronic_cardiac_disease <- 
  table1_chronic_cardiac_disease %>%
  dplyr::filter(era=="Pre-pandemic",
                (chronic_cardiac_disease=="Yes"|
                   chronic_cardiac_disease=="No")) %>%
  dplyr::arrange(chronic_cardiac_disease) %>%
  dplyr::ungroup() %>% dplyr::select("n_all_intervals")
# ## ## Get percentages per intervals and overall.
PP_pct_chronic_cardiac_disease <-
  (PP_n_chronic_cardiac_disease / sum(PP_n_chronic_cardiac_disease)) %>%
  "*"(100) %>% tidyr::replace_na(list("n_all_intervals" = 0)) %>%
  `colnames<-`(c("pct_all_intervals"))
# ## ## Bind the counts and percentages.
PP_chronic_cardiac_disease <-
  table1_chronic_cardiac_disease %>%
  dplyr::filter(era=="Pre-pandemic",
                (chronic_cardiac_disease=="Yes"|
                   chronic_cardiac_disease=="No")) %>%
  dplyr::arrange(chronic_cardiac_disease) %>%
  dplyr::select("chronic_cardiac_disease") %>%
  dplyr::bind_cols(PP_n_chronic_cardiac_disease, PP_pct_chronic_cardiac_disease)
# ## ## Clean up.
rm(PP_n_chronic_cardiac_disease, PP_pct_chronic_cardiac_disease)

# ## Pandemic no vaccine.
# ## ## Get counts per intervals and overall.
PNV_n_chronic_cardiac_disease <- 
  table1_chronic_cardiac_disease %>%
  dplyr::filter(era=="Pandemic no vaccine",
                (chronic_cardiac_disease=="Yes"|
                   chronic_cardiac_disease=="No")) %>%
  dplyr::arrange(chronic_cardiac_disease) %>%
  dplyr::ungroup() %>% dplyr::select(-c("era", "chronic_cardiac_disease"))
# ## ## Get percentages per intervals and overall.
PNV_pct_chronic_cardiac_disease <- 
  table1_chronic_cardiac_disease %>%
  dplyr::filter(era=="Pandemic no vaccine",
                (chronic_cardiac_disease=="Yes"|
                   chronic_cardiac_disease=="No")) %>%
  select(-c("era", "chronic_cardiac_disease")) %>%
  colSums() %>% sweep(PNV_n_chronic_cardiac_disease, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0)) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
PNV_chronic_cardiac_disease <-
  matrix(0,
         nrow = length(rownames(PNV_n_chronic_cardiac_disease)),
         ncol = length(colnames(PNV_n_chronic_cardiac_disease))*2) %>%
  as.data.frame()
PNV_chronic_cardiac_disease[,seq(1,length(colnames(PNV_chronic_cardiac_disease)),2)] <- PNV_n_chronic_cardiac_disease
PNV_chronic_cardiac_disease[,seq(2,length(colnames(PNV_chronic_cardiac_disease)),2)] <- PNV_pct_chronic_cardiac_disease
colnames(PNV_chronic_cardiac_disease)[seq(1,length(colnames(PNV_chronic_cardiac_disease)),2)] <- colnames(PNV_n_chronic_cardiac_disease)
colnames(PNV_chronic_cardiac_disease)[seq(2,length(colnames(PNV_chronic_cardiac_disease)),2)] <- colnames(PNV_pct_chronic_cardiac_disease)
PNV_chronic_cardiac_disease <-
  table1_chronic_cardiac_disease %>%
  dplyr::filter(era=="Pandemic no vaccine",
                (chronic_cardiac_disease=="Yes"|
                   chronic_cardiac_disease=="No")) %>%
  dplyr::arrange(chronic_cardiac_disease) %>%
  dplyr::select("chronic_cardiac_disease") %>%
  dplyr::bind_cols(PNV_chronic_cardiac_disease)
# ## ## Clean up.
rm(PNV_n_chronic_cardiac_disease, PNV_pct_chronic_cardiac_disease)

# ## Pandemic with vaccine.
# ## ## Get counts per intervals and overall.
PWV_n_chronic_cardiac_disease <- 
  table1_chronic_cardiac_disease %>%
  dplyr::filter(era=="Pandemic with vaccine",
                (chronic_cardiac_disease=="Yes"|
                   chronic_cardiac_disease=="No")) %>%
  dplyr::arrange(chronic_cardiac_disease) %>%
  dplyr::ungroup() %>% dplyr::select(-c("era", "chronic_cardiac_disease"))
# ## ## Get percentages per intervals and overall.
PWV_pct_chronic_cardiac_disease <- 
  table1_chronic_cardiac_disease %>%
  dplyr::filter(era=="Pandemic with vaccine",
                (chronic_cardiac_disease=="Yes"|
                   chronic_cardiac_disease=="No")) %>%
  select(-c("era", "chronic_cardiac_disease")) %>%
  colSums() %>% sweep(PWV_n_chronic_cardiac_disease, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0
  )) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
PWV_chronic_cardiac_disease <-
  matrix(0,
         length(rownames(PWV_n_chronic_cardiac_disease)),
         ncol = length(colnames(PWV_n_chronic_cardiac_disease))*2) %>%
  as.data.frame()
PWV_chronic_cardiac_disease[,seq(1,length(colnames(PWV_chronic_cardiac_disease)),2)] <- PWV_n_chronic_cardiac_disease
PWV_chronic_cardiac_disease[,seq(2,length(colnames(PWV_chronic_cardiac_disease)),2)] <- PWV_pct_chronic_cardiac_disease
colnames(PWV_chronic_cardiac_disease)[seq(1,length(colnames(PWV_chronic_cardiac_disease)),2)] <- colnames(PWV_n_chronic_cardiac_disease)
colnames(PWV_chronic_cardiac_disease)[seq(2,length(colnames(PWV_chronic_cardiac_disease)),2)] <- colnames(PWV_pct_chronic_cardiac_disease)
PWV_chronic_cardiac_disease <-
  table1_chronic_cardiac_disease %>% 
  dplyr::filter(era=="Pandemic with vaccine",
                (chronic_cardiac_disease=="Yes"|
                   chronic_cardiac_disease=="No")) %>%
  dplyr::arrange(chronic_cardiac_disease) %>%
  dplyr::select("chronic_cardiac_disease") %>%
  dplyr::bind_cols(PWV_chronic_cardiac_disease)
# ## ## Clean up
rm(PWV_n_chronic_cardiac_disease, PWV_pct_chronic_cardiac_disease)
# ----

# Diabetes ----
# ## Pre-pandemic
# ## ## Get counts per intervals and overall.
PP_n_diabetes <- 
  table1_diabetes %>%
  dplyr::filter(era=="Pre-pandemic",
                (diabetes=="Yes"|
                   diabetes=="No")) %>%
  dplyr::arrange(diabetes) %>%
  dplyr::ungroup() %>% dplyr::select("n_all_intervals")
# ## ## Get percentages per intervals and overall.
PP_pct_diabetes <-
  (PP_n_diabetes / sum(PP_n_diabetes)) %>%
  "*"(100) %>% tidyr::replace_na(list("n_all_intervals" = 0)) %>%
  `colnames<-`(c("pct_all_intervals"))
# ## ## Bind the counts and percentages.
PP_diabetes <-
  table1_diabetes %>%
  dplyr::filter(era=="Pre-pandemic",
                (diabetes=="Yes"|
                   diabetes=="No")) %>%
  dplyr::arrange(diabetes) %>%
  dplyr::select("diabetes") %>%
  dplyr::bind_cols(PP_n_diabetes, PP_pct_diabetes)
# ## ## Clean up.
rm(PP_n_diabetes, PP_pct_diabetes)

# ## Pandemic no vaccine.
# ## ## Get counts per intervals and overall.
PNV_n_diabetes <- 
  table1_diabetes %>%
  dplyr::filter(era=="Pandemic no vaccine",
                (diabetes=="Yes"|
                   diabetes=="No")) %>%
  dplyr::arrange(diabetes) %>%
  dplyr::ungroup() %>% dplyr::select(-c("era", "diabetes"))
# ## ## Get percentages per intervals and overall.
PNV_pct_diabetes <- 
  table1_diabetes %>%
  dplyr::filter(era=="Pandemic no vaccine",
                (diabetes=="Yes"|
                   diabetes=="No")) %>%
  select(-c("era", "diabetes")) %>%
  colSums() %>% sweep(PNV_n_diabetes, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0)) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
PNV_diabetes <-
  matrix(0,
         nrow = length(rownames(PNV_n_diabetes)),
         ncol = length(colnames(PNV_n_diabetes))*2) %>%
  as.data.frame()
PNV_diabetes[,seq(1,length(colnames(PNV_diabetes)),2)] <- PNV_n_diabetes
PNV_diabetes[,seq(2,length(colnames(PNV_diabetes)),2)] <- PNV_pct_diabetes
colnames(PNV_diabetes)[seq(1,length(colnames(PNV_diabetes)),2)] <- colnames(PNV_n_diabetes)
colnames(PNV_diabetes)[seq(2,length(colnames(PNV_diabetes)),2)] <- colnames(PNV_pct_diabetes)
PNV_diabetes <-
  table1_diabetes %>%
  dplyr::filter(era=="Pandemic no vaccine",
                (diabetes=="Yes"|
                   diabetes=="No")) %>%
  dplyr::arrange(diabetes) %>%
  dplyr::select("diabetes") %>%
  dplyr::bind_cols(PNV_diabetes)
# ## ## Clean up.
rm(PNV_n_diabetes, PNV_pct_diabetes)

# ## Pandemic with vaccine.
# ## ## Get counts per intervals and overall.
PWV_n_diabetes <- 
  table1_diabetes %>%
  dplyr::filter(era=="Pandemic with vaccine",
                (diabetes=="Yes"|
                   diabetes=="No")) %>%
  dplyr::arrange(diabetes) %>%
  dplyr::ungroup() %>% dplyr::select(-c("era", "diabetes"))
# ## ## Get percentages per intervals and overall.
PWV_pct_diabetes <- 
  table1_diabetes %>%
  dplyr::filter(era=="Pandemic with vaccine",
                (diabetes=="Yes"|
                   diabetes=="No")) %>%
  select(-c("era", "diabetes")) %>%
  colSums() %>% sweep(PWV_n_diabetes, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0
  )) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
PWV_diabetes <-
  matrix(0,
         length(rownames(PWV_n_diabetes)),
         ncol = length(colnames(PWV_n_diabetes))*2) %>%
  as.data.frame()
PWV_diabetes[,seq(1,length(colnames(PWV_diabetes)),2)] <- PWV_n_diabetes
PWV_diabetes[,seq(2,length(colnames(PWV_diabetes)),2)] <- PWV_pct_diabetes
colnames(PWV_diabetes)[seq(1,length(colnames(PWV_diabetes)),2)] <- colnames(PWV_n_diabetes)
colnames(PWV_diabetes)[seq(2,length(colnames(PWV_diabetes)),2)] <- colnames(PWV_pct_diabetes)
PWV_diabetes <-
  table1_diabetes %>% 
  dplyr::filter(era=="Pandemic with vaccine",
                (diabetes=="Yes"|
                   diabetes=="No")) %>%
  dplyr::arrange(diabetes) %>%
  dplyr::select("diabetes") %>%
  dplyr::bind_cols(PWV_diabetes)
# ## ## Clean up
rm(PWV_n_diabetes, PWV_pct_diabetes)
# ----

# Chronic respiratory disease. ----
# ## Pre-pandemic
# ## ## Get counts per intervals and overall.
PP_n_chronic_respiratory_disease <- 
  table1_chronic_respiratory_disease %>%
  dplyr::filter(era=="Pre-pandemic",
                (chronic_respiratory_disease=="Yes"|
                   chronic_respiratory_disease=="No")) %>%
  dplyr::arrange(chronic_respiratory_disease) %>%
  dplyr::ungroup() %>% dplyr::select("n_all_intervals")
# ## ## Get percentages per intervals and overall.
PP_pct_chronic_respiratory_disease <-
  (PP_n_chronic_respiratory_disease / sum(PP_n_chronic_respiratory_disease)) %>%
  "*"(100) %>% tidyr::replace_na(list("n_all_intervals" = 0)) %>%
  `colnames<-`(c("pct_all_intervals"))
# ## ## Bind the counts and percentages.
PP_chronic_respiratory_disease <-
  table1_chronic_respiratory_disease %>%
  dplyr::filter(era=="Pre-pandemic",
                (chronic_respiratory_disease=="Yes"|
                   chronic_respiratory_disease=="No")) %>%
  dplyr::arrange(chronic_respiratory_disease) %>%
  dplyr::select("chronic_respiratory_disease") %>%
  dplyr::bind_cols(PP_n_chronic_respiratory_disease, PP_pct_chronic_respiratory_disease)
# ## ## Clean up.
rm(PP_n_chronic_respiratory_disease, PP_pct_chronic_respiratory_disease)

# ## Pandemic no vaccine.
# ## ## Get counts per intervals and overall.
PNV_n_chronic_respiratory_disease <- 
  table1_chronic_respiratory_disease %>%
  dplyr::filter(era=="Pandemic no vaccine",
                (chronic_respiratory_disease=="Yes"|
                   chronic_respiratory_disease=="No")) %>%
  dplyr::arrange(chronic_respiratory_disease) %>%
  dplyr::ungroup() %>% dplyr::select(-c("era", "chronic_respiratory_disease"))
# ## ## Get percentages per intervals and overall.
PNV_pct_chronic_respiratory_disease <- 
  table1_chronic_respiratory_disease %>%
  dplyr::filter(era=="Pandemic no vaccine",
                (chronic_respiratory_disease=="Yes"|
                   chronic_respiratory_disease=="No")) %>%
  select(-c("era", "chronic_respiratory_disease")) %>%
  colSums() %>% sweep(PNV_n_chronic_respiratory_disease, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0)) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
PNV_chronic_respiratory_disease <-
  matrix(0,
         nrow = length(rownames(PNV_n_chronic_respiratory_disease)),
         ncol = length(colnames(PNV_n_chronic_respiratory_disease))*2) %>%
  as.data.frame()
PNV_chronic_respiratory_disease[,seq(1,length(colnames(PNV_chronic_respiratory_disease)),2)] <- PNV_n_chronic_respiratory_disease
PNV_chronic_respiratory_disease[,seq(2,length(colnames(PNV_chronic_respiratory_disease)),2)] <- PNV_pct_chronic_respiratory_disease
colnames(PNV_chronic_respiratory_disease)[seq(1,length(colnames(PNV_chronic_respiratory_disease)),2)] <- colnames(PNV_n_chronic_respiratory_disease)
colnames(PNV_chronic_respiratory_disease)[seq(2,length(colnames(PNV_chronic_respiratory_disease)),2)] <- colnames(PNV_pct_chronic_respiratory_disease)
PNV_chronic_respiratory_disease <-
  table1_chronic_respiratory_disease %>%
  dplyr::filter(era=="Pandemic no vaccine",
                (chronic_respiratory_disease=="Yes"|
                   chronic_respiratory_disease=="No")) %>%
  dplyr::arrange(chronic_respiratory_disease) %>%
  dplyr::select("chronic_respiratory_disease") %>%
  dplyr::bind_cols(PNV_chronic_respiratory_disease)
# ## ## Clean up.
rm(PNV_n_chronic_respiratory_disease, PNV_pct_chronic_respiratory_disease)

# ## Pandemic with vaccine.
# ## ## Get counts per intervals and overall.
PWV_n_chronic_respiratory_disease <- 
  table1_chronic_respiratory_disease %>%
  dplyr::filter(era=="Pandemic with vaccine",
                (chronic_respiratory_disease=="Yes"|
                   chronic_respiratory_disease=="No")) %>%
  dplyr::arrange(chronic_respiratory_disease) %>%
  dplyr::ungroup() %>% dplyr::select(-c("era", "chronic_respiratory_disease"))
# ## ## Get percentages per intervals and overall.
PWV_pct_chronic_respiratory_disease <- 
  table1_chronic_respiratory_disease %>%
  dplyr::filter(era=="Pandemic with vaccine",
                (chronic_respiratory_disease=="Yes"|
                   chronic_respiratory_disease=="No")) %>%
  select(-c("era", "chronic_respiratory_disease")) %>%
  colSums() %>% sweep(PWV_n_chronic_respiratory_disease, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0
  )) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
PWV_chronic_respiratory_disease <-
  matrix(0,
         length(rownames(PWV_n_chronic_respiratory_disease)),
         ncol = length(colnames(PWV_n_chronic_respiratory_disease))*2) %>%
  as.data.frame()
PWV_chronic_respiratory_disease[,seq(1,length(colnames(PWV_chronic_respiratory_disease)),2)] <- PWV_n_chronic_respiratory_disease
PWV_chronic_respiratory_disease[,seq(2,length(colnames(PWV_chronic_respiratory_disease)),2)] <- PWV_pct_chronic_respiratory_disease
colnames(PWV_chronic_respiratory_disease)[seq(1,length(colnames(PWV_chronic_respiratory_disease)),2)] <- colnames(PWV_n_chronic_respiratory_disease)
colnames(PWV_chronic_respiratory_disease)[seq(2,length(colnames(PWV_chronic_respiratory_disease)),2)] <- colnames(PWV_pct_chronic_respiratory_disease)
PWV_chronic_respiratory_disease <-
  table1_chronic_respiratory_disease %>% 
  dplyr::filter(era=="Pandemic with vaccine",
                (chronic_respiratory_disease=="Yes"|
                   chronic_respiratory_disease=="No")) %>%
  dplyr::arrange(chronic_respiratory_disease) %>%
  dplyr::select("chronic_respiratory_disease") %>%
  dplyr::bind_cols(PWV_chronic_respiratory_disease)
# ## ## Clean up
rm(PWV_n_chronic_respiratory_disease, PWV_pct_chronic_respiratory_disease)
# ----

####################
# Make the tables. #
####################
# ----
# Pre-pandemic tables.
# ## Stratification table.
tbl_PP_strata <-
  rbind(
  as.matrix(PP_ageGroup),
  as.matrix(PP_Sex),
  as.matrix(PP_chronic_cardiac_disease),
  as.matrix(PP_diabetes),
  as.matrix(PP_chronic_respiratory_disease)) %>%
    as.data.frame()  %>%
    tibble::add_column(., c(
      rep("Age group",5),
      rep("Sex",2),
      rep("Chronic cardiac disease",2),
      rep("Diabetes",2),
      rep("Chronic respiratory disease",2)),
      .before = "age_group_surgery"
      ) %>%
    `colnames<-`(c("variable", "strata", "n", "pct"))
# Save table.
write.csv(
  x = tbl_PP_strata,
  file = here::here("output",paste0("table1_PP_strata",sensitivity_cohort,".csv"))
)
# ## Outcomes table.
tbl_PP_outcome <-
  rbind(
    as.matrix(PP_postOp_mortality_30day),
    as.matrix(PP_postOp_mortality_90day),
    as.matrix(PP_postOp_mortality_6mth),
    as.matrix(PP_postOp_mortality_12mth)) %>%
  as.data.frame()  %>%
  tibble::add_column(., c(
    rep("30-day post-operative mortality",2),
    rep("90-day post-operative mortality",2),
    rep("6-month post-operative mortality",2),
    rep("12-month post-operative mortality",2)),
    .before = "postOp_mortality_30day"
  ) %>%
  `colnames<-`(c("variable", "strata", "n", "pct"))
# Save table.
write.csv(
  x = tbl_PP_outcome,
  file = here::here("output","table1_PP_outcome.csv")
)

# Pandemic no vaccine tables.
# ## Stratification table.
tbl_PNV_strata <-
  rbind(
    as.matrix(PNV_ageGroup),
    as.matrix(PNV_Sex),
    as.matrix(PNV_chronic_cardiac_disease),
    as.matrix(PNV_diabetes),
    as.matrix(PNV_chronic_respiratory_disease)) %>%
  as.data.frame()  %>%
  tibble::add_column(., c(
    rep("Age group",5),
    rep("Sex",2),
    rep("Chronic cardiac disease",2),
    rep("Diabetes",2),
    rep("Chronic respiratory disease",2)),
    .before = "age_group_surgery"
  ) %>%
  `colnames<-`(c("variable", "strata", "n", "pct"))
# Save table.
write.csv(
  x = tbl_PNV_strata,
  file = here::here("output",paste0("table1_PNV_strata",sensitivity_cohort,".csv"))
)
# ## Outcomes table.
tbl_PNV_outcome <-
  rbind(
    as.matrix(PNV_postOp_mortality_30day),
    as.matrix(PNV_postOp_mortality_90day),
    as.matrix(PNV_postOp_mortality_6mth),
    as.matrix(PNV_postOp_mortality_12mth)) %>%
  as.data.frame()  %>%
  tibble::add_column(., c(
    rep("30-day post-operative mortality",2),
    rep("90-day post-operative mortality",2),
    rep("6-month post-operative mortality",2),
    rep("12-month post-operative mortality",2)),
    .before = "postOp_mortality_30day"
  ) %>%
  `colnames<-`(c("variable", "strata",
                 "n_all_intervals", "pct_all_intervals",
                 "n_no_infection", "pct_no_infection",
                 "n_0to2_weeks","pct_0to2_weeks",
                 "n_3to4_weeks","pct_3to4_weeks",
                 "n_5to6_weeks","pct_5to6_weeks",
                 "n_>7_weeks","pct_>7_weeks"))
# Save table.
write.csv(
  x = tbl_PNV_outcome,
  file = here::here("output",paste0("table1_PNV_outcome",sensitivity_cohort,".csv"))
)

# Pandemic with vaccine tables.
# ## Stratification table.
tbl_PWV_strata <-
  rbind(
    as.matrix(PWV_ageGroup),
    as.matrix(PWV_Sex),
    as.matrix(PWV_chronic_cardiac_disease),
    as.matrix(PWV_diabetes),
    as.matrix(PWV_chronic_respiratory_disease)) %>%
  as.data.frame()  %>%
  tibble::add_column(., c(
    rep("Age group",5),
    rep("Sex",2),
    rep("Chronic cardiac disease",2),
    rep("Diabetes",2),
    rep("Chronic respiratory disease",2)),
    .before = "age_group_surgery"
  ) %>%
  `colnames<-`(c("variable", "strata", "n", "pct"))
# Save table.
write.csv(
  x = tbl_PWV_strata,
  file = here::here("output",paste0("table1_PWV_strata",sensitivity_cohort,".csv"))
)
# ## Outcomes table.
tbl_PWV_outcome <-
  rbind(
    as.matrix(PWV_postOp_mortality_30day),
    as.matrix(PWV_postOp_mortality_90day),
    as.matrix(PWV_postOp_mortality_6mth),
    as.matrix(PWV_postOp_mortality_12mth)) %>%
  as.data.frame()  %>%
  tibble::add_column(., c(
    rep("30-day post-operative mortality",2),
    rep("90-day post-operative mortality",2),
    rep("6-month post-operative mortality",2),
    rep("12-month post-operative mortality",2)),
    .before = "postOp_mortality_30day"
  ) %>%
  `colnames<-`(c("variable", "strata",
                 "n_all_intervals", "pct_all_intervals",
                 "n_no_infection", "pct_no_infection",
                 "n_0to2_weeks","pct_0to2_weeks",
                 "n_3to4_weeks","pct_3to4_weeks",
                 "n_5to6_weeks","pct_5to6_weeks",
                 "n_>7_weeks","pct_>7_weeks"))
# Save table.
write.csv(
  x = tbl_PWV_outcome,
  file = here::here("output",paste0("table1_PWV_outcome",sensitivity_cohort,".csv"))
)
# ----

