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
myData_noConstraints <- myData

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
  myData_noConstraints %>% dplyr::group_by(era,
                      preOperative_infection_status) %>% dplyr::summarise(n = n())
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by age band:
# ##    1. 0-29
# ##    2. 30-49
# ##    3. 50-69
# ##    4. 70-79
# ##    5. 80+
table1_ageGroup <- 
  myData_noConstraints %>% dplyr::group_by(era, age_group_surgery) %>%
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
  myData_noConstraints %>% dplyr::group_by(era, Sex) %>%
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
  myData_noConstraints %>% dplyr::group_by(era, postOp_mortality_30day) %>%
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
  myData_noConstraints %>% dplyr::group_by(era, postOp_mortality_90day) %>%
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
  myData_noConstraints %>% dplyr::group_by(era, postOp_mortality_6mth) %>%
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
  myData_noConstraints %>% dplyr::group_by(era, postOp_mortality_12mth) %>%
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
  myData_noConstraints %>% dplyr::group_by(era, chronic_cardiac_disease) %>%
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
  myData_noConstraints %>% dplyr::group_by(era, diabetes) %>%
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
  myData_noConstraints %>% dplyr::group_by(era, chronic_respiratory_disease) %>%
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
write.csv(
  x = table1_ageGroup,
  file = here::here("output","table1_ageGroup.csv")
)
write.csv(
  x = table1_Sex,
  file = here::here("output","table1_Sex.csv")
)
write.csv(
  x = table1_postOp_mortality_30day,
  file = here::here("output","table1_postOp_mortality_30day.csv")
)
write.csv(
  x = table1_postOp_mortality_90day,
  file = here::here("output","table1_postOp_mortality_90day.csv")
)
write.csv(
  x = table1_postOp_mortality_6mth,
  file = here::here("output","table1_postOp_mortality_6mth.csv")
)
write.csv(
  x = table1_postOp_mortality_12mth,
  file = here::here("output","table1_postOp_mortality_12mth.csv")
)
write.csv(
  x = table1_chronic_cardiac_disease,
  file = here::here("output","table1_chronic_cardiac_disease.csv")
)
write.csv(
  x = table1_diabetes,
  file = here::here("output","table1_diabetes.csv")
)
write.csv(
  x = table1_chronic_respiratory_disease,
  file = here::here("output","table1_chronic_respiratory_disease.csv")
)
# ----

#####################################
# Make vectors to inform the table. #
#####################################
# Age band. ----
# ## Pre-pandemic
# ## ## Get counts per intervals and overall.
PP_n_ageGroup <- 
  table1_ageGroup %>% dplyr::filter(era=="Pre-pandemic") %>%
  dplyr::arrange(age_group_surgery) %>% dplyr::ungroup() %>% dplyr::select("n_all_intervals")
# ## ## Get percentages per intervals and overall.
PP_pct_ageGroup <- (PP_n_ageGroup / sum(PP_n_ageGroup)) %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0)) %>%
  `colnames<-`(c("pct_all_intervals"))
PP_ageGroup <- table1_ageGroup %>%  dplyr::filter(era=="Pandemic no vaccine") %>%
  dplyr::arrange(age_group_surgery) %>% dplyr::select("age_group_surgery") %>% dplyr::bind_cols(., PP_n_ageGroup, PP_pct_ageGroup)
# ## ## Clean up.
rm(PP_n_ageGroup, PP_pct_ageGroup)

# ## Pandemic no vaccine.
# ## ## Get counts per intervals and overall.
PNV_n_ageGroup <- 
  table1_ageGroup %>%  dplyr::filter(era=="Pandemic no vaccine") %>%
  dplyr::arrange(age_group_surgery) %>% dplyr::ungroup() %>% dplyr::select(-c("era", "age_group_surgery"))
# ## ## Get percentages per intervals and overall.
PNV_pct_ageGroup <- 
  table1_ageGroup %>% dplyr::filter(era=="Pandemic no vaccine") %>% select(-c("era", "age_group_surgery")) %>%
  colSums() %>% sweep(PNV_n_ageGroup, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0
                         )) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                   "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
PNV_ageGroup <- matrix(0,
                      nrow = length(ageGroup_rownames),
                      ncol = length(colnames(PNV_n_ageGroup))*2) %>%
               as.data.frame()
PNV_ageGroup[,seq(1,length(colnames(PNV_ageGroup)),2)] <- PNV_n_ageGroup
PNV_ageGroup[,seq(2,length(colnames(PNV_ageGroup)),2)] <- PNV_pct_ageGroup
colnames(PNV_ageGroup)[seq(1,length(colnames(PNV_ageGroup)),2)] <- colnames(PNV_n_ageGroup)
colnames(PNV_ageGroup)[seq(2,length(colnames(PNV_ageGroup)),2)] <- colnames(PNV_pct_ageGroup)
PNV_ageGroup <- table1_ageGroup %>%  dplyr::filter(era=="Pandemic no vaccine") %>%
  dplyr::arrange(age_group_surgery) %>% dplyr::select("age_group_surgery") %>% dplyr::bind_cols(PNV_ageGroup)
# ## ## Clean up.
rm(PNV_n_ageGroup, PNV_pct_ageGroup)

# ## Pandemic with vaccine.
# ## ## Get counts per intervals and overall.
PWV_n_ageGroup <- 
  table1_ageGroup %>%  dplyr::filter(era=="Pandemic with vaccine") %>%
  dplyr::arrange(age_group_surgery) %>% dplyr::ungroup() %>% dplyr::select(-c("era", "age_group_surgery"))
# ## ## Get percentages per intervals and overall.
PWV_pct_ageGroup <- 
  table1_ageGroup %>% dplyr::filter(era=="Pandemic with vaccine") %>% select(-c("era", "age_group_surgery")) %>%
  colSums() %>% sweep(PWV_n_ageGroup, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0
  )) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
PWV_ageGroup <- matrix(0,
                       nrow = length(ageGroup_rownames),
                       ncol = length(colnames(PWV_n_ageGroup))*2) %>%
  as.data.frame()
PWV_ageGroup[,seq(1,length(colnames(PWV_ageGroup)),2)] <- PWV_n_ageGroup
PWV_ageGroup[,seq(2,length(colnames(PWV_ageGroup)),2)] <- PWV_pct_ageGroup
colnames(PWV_ageGroup)[seq(1,length(colnames(PWV_ageGroup)),2)] <- colnames(PWV_n_ageGroup)
colnames(PWV_ageGroup)[seq(2,length(colnames(PWV_ageGroup)),2)] <- colnames(PWV_pct_ageGroup)
PWV_ageGroup <- table1_ageGroup %>%  dplyr::filter(era=="Pandemic with vaccine") %>%
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
  table1_Sex %>%  dplyr::filter(era=="Pandemic with vaccine") %>%
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
                       nrow = length(ageGroup_rownames),
                       ncol = length(colnames(PWV_n_Sex))*2) %>%
  as.data.frame()
PWV_Sex[,seq(1,length(colnames(PWV_Sex)),2)] <- PWV_n_Sex
PWV_Sex[,seq(2,length(colnames(PWV_Sex)),2)] <- PWV_pct_Sex
colnames(PWV_Sex)[seq(1,length(colnames(PWV_Sex)),2)] <- colnames(PWV_n_Sex)
colnames(PWV_Sex)[seq(2,length(colnames(PWV_Sex)),2)] <- colnames(PWV_pct_Sex)
PWV_Sex <- table1_Sex %>%  dplyr::filter(era=="Pandemic with vaccine") %>%
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

#################################################
#################################################
# CONTINUE FROM HERE
#################################################
#################################################



# ## Totals. `n` is count. `prop` is proportion.
n_totals_preMarch2020 <- 
                      rbind(n_preMarch2020_ageGroup,
                            n_preMarch2020_Sex,
                            n_preMarch2020_postOp_mortality_30day,
                            n_preMarch2020_chronic_cardiac_disease,
                            n_preMarch2020_diabetes,
                            n_preMarch2020_chronic_respiratory_disease)
prop_totals_preMarch2020 <- 
                      rbind(prop_preMarch2020_ageGroup,
                            prop_preMarch2020_Sex,
                            prop_preMarch2020_postOp_mortality_30day,
                            prop_preMarch2020_chronic_cardiac_disease,
                            prop_preMarch2020_diabetes,
                            prop_preMarch2020_chronic_respiratory_disease)

#################################################
#################################################
# CONTINUE FROM HERE
#################################################
#################################################

# Post-March 2020 totals.
# ## Age band.
n_postMarch2020_ageGroup <- 
          table1_ageGroup %>%
          dplyr::filter(era=="Pandemic no vaccine") %>%
          dplyr::arrange(age_group_surgery) %>% dplyr::ungroup() %>% dplyr::select(n_all_intervals)
prop_postMarch2020_ageGroup <-
          n_postMarch2020_ageGroup /
            table1_ageGroup %>%
              dplyr::filter(era=="Pandemic no vaccine") %>%
                select(n_all_intervals) %>% sum()
# ## Sex.
n_postMarch2020_Sex <-
  table1_Sex %>%
  dplyr::filter(era=="Pandemic no vaccine") %>%
  dplyr::arrange(Sex) %>% dplyr::ungroup() %>%
  dplyr::select(n_all_intervals)
prop_postMarch2020_Sex <- n_postMarch2020_Sex / 
                            table1_Sex %>%
                              dplyr::filter(era=="Pandemic no vaccine") %>%
                                select(n_all_intervals) %>% sum()
# ## 30-day post-operative mortality.
n_postMarch2020_postOp_mortality_30day <-
          table1_postOp_mortality_30day %>%
          dplyr::filter(era=="Pandemic no vaccine",
                        (postOp_mortality_30day=="Alive within 30 days post-operation"|
                           postOp_mortality_30day=="Dead within 30 days post-operation"|
                    postOp_mortality_30day=="Missing")) %>%
          dplyr::arrange(postOp_mortality_30day) %>% dplyr::ungroup() %>%
          dplyr::select(n_all_intervals)
prop_postMarch2020_postOp_mortality_30day <-
          n_postMarch2020_postOp_mortality_30day / 
            table1_postOp_mortality_30day %>%
              dplyr::filter(era=="Pandemic no vaccine") %>%
                select(n_all_intervals) %>% sum()
# ## Totals. `n` is count. `prop` is proportion.
n_totals_postMarch2020 <- 
          rbind(n_postMarch2020_ageGroup,
                n_postMarch2020_Sex,
                n_postMarch2020_postOp_mortality_30day)
prop_totals_postMarch2020 <- 
          rbind(prop_postMarch2020_ageGroup,
                prop_postMarch2020_Sex,
                prop_postMarch2020_postOp_mortality_30day)

# No pre-operative infection.
# ## Age band.
n_subtotals_infection_none_ageGroup <- 
          table1_ageGroup %>%
          dplyr::filter(era=="Pandemic no vaccine") %>%
          dplyr::arrange(age_group_surgery) %>% dplyr::ungroup() %>% dplyr::select(n_infection_none)
prop_subtotals_infection_none_ageGroup <-
          n_subtotals_infection_none_ageGroup /
            table1_ageGroup %>%
              dplyr::filter(era=="Pandemic no vaccine") %>%
                select(n_infection_none) %>% sum()
# ## Sex.
n_subtotals_infection_none_Sex <-
  table1_Sex %>%
  dplyr::filter(era=="Pandemic no vaccine") %>%
  dplyr::arrange(Sex) %>% dplyr::ungroup() %>%
  dplyr::select(n_infection_none)
prop_subtotals_infection_none_Sex <-
          n_subtotals_infection_none_Sex /
            table1_Sex %>%
              dplyr::filter(era=="Pandemic no vaccine") %>%
                select(n_infection_none) %>% sum()
# ## 30-day post-operative mortality.
n_subtotals_infection_none_postOp_mortality_30day <-
          table1_postOp_mortality_30day %>%
          dplyr::filter(era=="Pandemic no vaccine",
                 (postOp_mortality_30day=="Alive within 30 days post-operation"|
                    postOp_mortality_30day=="Dead within 30 days post-operation"|
                    postOp_mortality_30day=="Missing")) %>%
          dplyr::arrange(postOp_mortality_30day) %>% dplyr::ungroup() %>%
          dplyr::select(n_infection_none)
prop_subtotals_infection_none_postOp_mortality_30day <-
          n_subtotals_infection_none_postOp_mortality_30day / 
            table1_postOp_mortality_30day %>%
              dplyr::filter(era=="Pandemic no vaccine") %>%
                select(n_infection_none) %>% sum()
# ## Totals. `n` is count. `prop` is proportion.
n_subtotals_infection_none <- 
          rbind(n_subtotals_infection_none_ageGroup,
                n_subtotals_infection_none_Sex,
                n_subtotals_infection_none_postOp_mortality_30day)
prop_subtotals_infection_none <- 
          rbind(prop_subtotals_infection_none_ageGroup,
                prop_subtotals_infection_none_Sex,
                prop_subtotals_infection_none_postOp_mortality_30day)

# Pre-operative infection (0-2 weeks).
# ## Age band.
n_subtotals_infection_0to2wk_ageGroup <- 
          table1_ageGroup %>%
          dplyr::filter(era=="Pandemic no vaccine") %>%
          dplyr::arrange(age_group_surgery) %>% dplyr::ungroup() %>% dplyr::select(n_infection_0to2wk)
prop_subtotals_infection_0to2wk_ageGroup <-
          n_subtotals_infection_0to2wk_ageGroup /
            table1_ageGroup %>%
              dplyr::filter(era=="Pandemic no vaccine") %>%
                select(n_infection_0to2wk) %>% sum()
# ## Sex.
n_subtotals_infection_0to2wk_Sex <-
  table1_Sex %>%
  dplyr::filter(era=="Pandemic no vaccine") %>%
  dplyr::arrange(Sex) %>% dplyr::ungroup() %>%
  dplyr::select(n_infection_0to2wk)
prop_subtotals_infection_0to2wk_Sex <-
          n_subtotals_infection_0to2wk_Sex /
            table1_Sex %>%
              dplyr::filter(era=="Pandemic no vaccine") %>%
                select(n_infection_0to2wk) %>% sum()
# ## 30-day post-operative mortality.
n_subtotals_infection_0to2wk_postOp_mortality_30day <-
          table1_postOp_mortality_30day %>%
          dplyr::filter(era=="Pandemic no vaccine",
                 (postOp_mortality_30day=="Alive within 30 days post-operation"|
                    postOp_mortality_30day=="Dead within 30 days post-operation"|
                    postOp_mortality_30day=="Missing")) %>%
          dplyr::arrange(postOp_mortality_30day) %>% dplyr::ungroup() %>%
          dplyr::select(n_infection_0to2wk)
prop_subtotals_infection_0to2wk_postOp_mortality_30day <-
          n_subtotals_infection_0to2wk_postOp_mortality_30day / 
            table1_postOp_mortality_30day %>%
              dplyr::filter(era=="Pandemic no vaccine") %>%
                select(n_infection_0to2wk) %>% sum()
# ## Totals. `n` is count. `prop` is proportion.
n_subtotals_infection_0to2wk <- 
          rbind(n_subtotals_infection_0to2wk_ageGroup,
                n_subtotals_infection_0to2wk_Sex,
                n_subtotals_infection_0to2wk_postOp_mortality_30day)
prop_subtotals_infection_0to2wk <- 
          rbind(prop_subtotals_infection_0to2wk_ageGroup,
                prop_subtotals_infection_0to2wk_Sex,
                prop_subtotals_infection_0to2wk_postOp_mortality_30day)

# Pre-operative infection (3-4 weeks).
# ## Age band.
n_subtotals_infection_3to4wk_ageGroup <- 
          table1_ageGroup %>%
          dplyr::filter(era=="Pandemic no vaccine") %>%
          dplyr::arrange(age_group_surgery) %>% dplyr::ungroup() %>% dplyr::select(n_infection_3to4wk)
prop_subtotals_infection_3to4wk_ageGroup <-
          n_subtotals_infection_3to4wk_ageGroup /
            table1_ageGroup %>%
              dplyr::filter(era=="Pandemic no vaccine") %>%
                select(n_infection_3to4wk) %>% sum()
# ## Sex.
n_subtotals_infection_3to4wk_Sex <-
  table1_Sex %>%
  dplyr::filter(era=="Pandemic no vaccine") %>%
  dplyr::arrange(Sex) %>% dplyr::ungroup() %>%
  dplyr::select(n_infection_3to4wk)
prop_subtotals_infection_3to4wk_Sex <-
          n_subtotals_infection_3to4wk_Sex /
            table1_Sex %>%
              dplyr::filter(era=="Pandemic no vaccine") %>%
                select(n_infection_3to4wk) %>% sum()
# ## 30-day post-operative mortality.
n_subtotals_infection_3to4wk_postOp_mortality_30day <-
          table1_postOp_mortality_30day %>%
          dplyr::filter(era=="Pandemic no vaccine",
                 (postOp_mortality_30day=="Alive within 30 days post-operation"|
                    postOp_mortality_30day=="Dead within 30 days post-operation"|
                    postOp_mortality_30day=="Missing")) %>%
          dplyr::arrange(postOp_mortality_30day) %>% dplyr::ungroup() %>%
          dplyr::select(n_infection_3to4wk)
prop_subtotals_infection_3to4wk_postOp_mortality_30day <-
          n_subtotals_infection_3to4wk_postOp_mortality_30day / 
            table1_postOp_mortality_30day %>%
              dplyr::filter(era=="Pandemic no vaccine") %>%
                select(n_infection_3to4wk) %>% sum()
# ## Totals. `n` is count. `prop` is proportion.
n_subtotals_infection_3to4wk <- 
          rbind(n_subtotals_infection_3to4wk_ageGroup,
                n_subtotals_infection_3to4wk_Sex,
                n_subtotals_infection_3to4wk_postOp_mortality_30day)
prop_subtotals_infection_3to4wk <- 
          rbind(prop_subtotals_infection_3to4wk_ageGroup,
                prop_subtotals_infection_3to4wk_Sex,
                prop_subtotals_infection_3to4wk_postOp_mortality_30day)

# Pre-operative infection (5-6 weeks).
# ## Age band.
n_subtotals_infection_5to6wk_ageGroup <- 
          table1_ageGroup %>%
          dplyr::filter(era=="Pandemic no vaccine") %>%
          dplyr::arrange(age_group_surgery) %>% dplyr::ungroup() %>% dplyr::select(n_infection_5to6wk)
prop_subtotals_infection_5to6wk_ageGroup <-
          n_subtotals_infection_5to6wk_ageGroup /
            table1_ageGroup %>%
              dplyr::filter(era=="Pandemic no vaccine") %>%
                select(n_infection_5to6wk) %>% sum()
# ## Sex.
n_subtotals_infection_5to6wk_Sex <-
  table1_Sex %>%
  dplyr::filter(era=="Pandemic no vaccine") %>%
  dplyr::arrange(Sex) %>% dplyr::ungroup() %>%
  dplyr::select(n_infection_5to6wk)
prop_subtotals_infection_5to6wk_Sex <-
          n_subtotals_infection_5to6wk_Sex /
            table1_Sex %>%
              dplyr::filter(era=="Pandemic no vaccine") %>%
                select(n_infection_5to6wk) %>% sum()
# ## 30-day post-operative mortality.
n_subtotals_infection_5to6wk_postOp_mortality_30day <-
          table1_postOp_mortality_30day %>%
          dplyr::filter(era=="Pandemic no vaccine",
                 (postOp_mortality_30day=="Alive within 30 days post-operation"|
                    postOp_mortality_30day=="Dead within 30 days post-operation"|
                    postOp_mortality_30day=="Missing")) %>%
          dplyr::arrange(postOp_mortality_30day) %>% dplyr::ungroup() %>%
          dplyr::select(n_infection_5to6wk)
prop_subtotals_infection_5to6wk_postOp_mortality_30day <-
          n_subtotals_infection_5to6wk_postOp_mortality_30day / 
            table1_postOp_mortality_30day %>%
              dplyr::filter(era=="Pandemic no vaccine") %>%
                select(n_infection_5to6wk) %>% sum()
# ## Totals. `n` is count. `prop` is proportion.
n_subtotals_infection_5to6wk <- 
          rbind(n_subtotals_infection_5to6wk_ageGroup,
                n_subtotals_infection_5to6wk_Sex,
                n_subtotals_infection_5to6wk_postOp_mortality_30day)
prop_subtotals_infection_5to6wk <- 
          rbind(prop_subtotals_infection_5to6wk_ageGroup,
                prop_subtotals_infection_5to6wk_Sex,
                prop_subtotals_infection_5to6wk_postOp_mortality_30day)

# Pre-operative infection (>=7 weeks).
# ## Age band.
n_subtotals_infection_7wk_ageGroup <- 
          table1_ageGroup %>%
          dplyr::filter(era=="Pandemic no vaccine") %>%
          dplyr::arrange(age_group_surgery) %>% dplyr::ungroup() %>% dplyr::select(n_infection_7wk)
prop_subtotals_infection_7wk_ageGroup <-
          n_subtotals_infection_7wk_ageGroup /
            table1_ageGroup %>%
              dplyr::filter(era=="Pandemic no vaccine") %>%
                select(n_infection_7wk) %>% sum()
# ## Sex.
n_subtotals_infection_7wk_Sex <-
  table1_Sex %>%
  dplyr::filter(era=="Pandemic no vaccine") %>%
  dplyr::arrange(Sex) %>% dplyr::ungroup() %>%
  dplyr::select(n_infection_7wk)
prop_subtotals_infection_7wk_Sex <-
          n_subtotals_infection_7wk_Sex /
            table1_Sex %>%
              dplyr::filter(era=="Pandemic no vaccine") %>%
                select(n_infection_7wk) %>% sum()
# ## 30-day post-operative mortality.
n_subtotals_infection_7wk_postOp_mortality_30day <-
          table1_postOp_mortality_30day %>%
          dplyr::filter(era=="Pandemic no vaccine",
                 (postOp_mortality_30day=="Alive within 30 days post-operation"|
                    postOp_mortality_30day=="Dead within 30 days post-operation"|
                    postOp_mortality_30day=="Missing")) %>%
          dplyr::arrange(postOp_mortality_30day) %>% dplyr::ungroup() %>%
          dplyr::select(n_infection_7wk)
prop_subtotals_infection_7wk_postOp_mortality_30day <-
          n_subtotals_infection_7wk_postOp_mortality_30day / 
            table1_postOp_mortality_30day %>%
              dplyr::filter(era=="Pandemic no vaccine") %>%
                select(n_infection_7wk) %>% sum()
# ## Totals. `n` is count. `prop` is proportion.
n_subtotals_infection_7wk <- 
          rbind(n_subtotals_infection_7wk_ageGroup,
                n_subtotals_infection_7wk_Sex,
                n_subtotals_infection_7wk_postOp_mortality_30day)
prop_subtotals_infection_7wk <- 
          rbind(prop_subtotals_infection_7wk_ageGroup,
                prop_subtotals_infection_7wk_Sex,
                prop_subtotals_infection_7wk_postOp_mortality_30day)
# ----


########################################################################
# Make kable Table 1, for the data relating to the 4 week on-boarding. #
########################################################################
# ----
# Make data frame.
df_4wk <- data.frame(
          # Pre-March 2020 totals, n.
          n_totals_preMarch2020,
          # Pre-March 2020 totals, %.
          round(prop_totals_preMarch2020 * 100, 0),
          # Post-March 2020 totals, n.
          n_totals_postMarch2020,
          # Post-March 2020 totals, %.
          round(prop_totals_postMarch2020 * 100, 0),
          # No pre-operative infection, n.
          n_subtotals_infection_none,
          # No pre-operative infection, %.
          round(prop_subtotals_infection_none * 100, 0),
          # Pre-operative infection (0-2 weeks), n.
          n_subtotals_infection_0to2wk,
          # Pre-operative infection (0-2 weeks), %.
          round(prop_subtotals_infection_0to2wk * 100, 0),
          # Pre-operative infection (3-4 weeks), n.
          n_subtotals_infection_3to4wk,
          # Pre-operative infection (3-4 weeks), %.
          round(prop_subtotals_infection_3to4wk * 100, 0),
          # Pre-operative infection (5-6 weeks), n.
          n_subtotals_infection_5to6wk,
          # Pre-operative infection (5-6 weeks), %.
          round(prop_subtotals_infection_5to6wk * 100, 0),
          # Pre-operative infection (>=7 weeks), n.
          n_subtotals_infection_7wk,
          # Pre-operative infection (>=7 weeks), %.
          round(prop_subtotals_infection_7wk * 100, 0)
        )
# Label data frame.
colnames(df_4wk) <- rep(c("n", "%"),7)
rownames(df_4wk) <- c(
          # ## Age groups.
          "0-29","30-49","50-69","70-79",">=80","Missing",
          # ## Sex.
          "Female", "Male", "Missing ",
          # ## 30-day post-operative mortality.
          "Alive within 30 days post-operation",
          "Dead within 30 days post-operation", "Missing  ")
# Save data frame.
write.csv(
  x = df_4wk,
  file = here::here("output","table1_4wk_onboarding.csv")
)
# Make kable table.
df_4wk %>%
  kableExtra::kbl(caption = paste0("<b>Table 1</b> Baseline characteristics and outcomes for ",
                       "patients undergoing surgery stratified by time from ",
                       "indication of SARS-CoV-2 infection. Values are ",
                       "counts and percentages.", collapse = ""),
      format = 'html') %>%
  kableExtra::kable_classic(full_width = F,
                fixed_thead = T,
                html_font="Cambria") %>%
  kableExtra::pack_rows(index = c("Age" = 6, "Sex" = 3,
                      "30-day post-operative mortality" = 3)) %>%
  kableExtra::add_header_above(c(" " = 7, "0-2 weeks" = 2, "3-4 weeks" = 2,
                     "5-6 weeks" = 2, ">=7 weeks" = 2)) %>%
  kableExtra::add_header_above(c(" " = 1, "Pre-March 2020,\ntotals" = 2,
                     "Post-March 2020,\ntotals" = 2,
                     "No indication of infection" = 2,
                     "\nIndication of infection" = 8)) %>%
  kableExtra::add_header_above(c(" " = 5, "Post-March 2020" = 10)) %>%
  kableExtra::column_spec(c(1:15), width = "5em") %>%
  kableExtra::row_spec(0, align = "c") #%>%
  #kableExtra::save_kable(file = here::here("output","Table1_4wk_onboarding.png"))
# ----
