# Make_Table1.R
#
# This script processes data from the myData dataframe to create a table
# in the style of Table 1 from the primary publication that this project is
# emulating (doi: 10.1111/anae.15458).
#
# This script should not be used in isolation. Rather, it is called from
# Make_all_Table1s.R, in which some parameters are specified.
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

#############################
# Load requisite functions. #
#############################
source(here::here("analysis", "fnc_countsAndPercentages.R"))
source(here::here("analysis", "fnc_make7wkTable.R"))

##########################################
# Make tibbles that will inform Table 1. #
##########################################
# ----
# ## Get count of patients per era.
if(!exists("OS_all_counts"))
{source(here::here("analysis","Make_table_COVIDSurg_compare.R"))}
all_counts <- 
  OS_all_counts %>% dplyr::group_by(era) %>% dplyr::summarise(n = sum(n))
  
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
# ##    3. Pandemic with vaccine
table1_totals_preOp_infection_status <- 
  data_to_use %>% 
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::group_by(era, preOperative_infection_status) %>% dplyr::summarise(n = n())
table1_CSP_totals_preOp_infection_status <- 
  data_to_use %>% dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
  dplyr::group_by(COVIDSurg_data_collection_period, preOperative_infection_status) %>%
  dplyr::summarise(n = n()) %>%
  `colnames<-`(c("era",colnames(table1_totals_preOp_infection_status)[2:ncol(table1_totals_preOp_infection_status)]))
table1_totals_preOp_infection_status <-
  dplyr::bind_rows(table1_totals_preOp_infection_status,
                   table1_CSP_totals_preOp_infection_status)
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by age band:
# ##    1. 0-29
# ##    2. 30-49
# ##    3. 50-69
# ##    4. 70-79
# ##    5. 80+
table1_ageGroup <- 
  data_to_use %>% 
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::group_by(era, age_group_surgery) %>%
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
table1_CSP_ageGroup <- 
  data_to_use %>% 
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
  dplyr::group_by(COVIDSurg_data_collection_period, age_group_surgery) %>%
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
                   ) %>% `colnames<-`(c("era", colnames(table1_ageGroup)[2:ncol(table1_ageGroup)]))
                   
table1_ageGroup <-
  dplyr::bind_rows(table1_ageGroup, table1_CSP_ageGroup)
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by sex:
# ##    1. Female
# ##    2. Male
table1_Sex <- 
  data_to_use %>% 
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::group_by(era, Sex) %>%
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
table1_CSP_Sex <- 
  data_to_use %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
  dplyr::group_by(COVIDSurg_data_collection_period, Sex) %>%
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
  ) %>% `colnames<-`(c("era", colnames(table1_Sex)[2:ncol(table1_Sex)]))

table1_Sex <- dplyr::bind_rows(table1_Sex, table1_CSP_Sex)
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by whether
# ## their surgery was during an elective or Emergency admission:
# ##    1. Elective
# ##    2. Emergency
table1_admission_method <- 
  data_to_use %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::group_by(era, category_admission_method) %>%
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
table1_CSP_admission_method <- 
  data_to_use %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
  dplyr::group_by(COVIDSurg_data_collection_period, category_admission_method) %>%
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
  ) %>% `colnames<-`(c("era", colnames(table1_admission_method)[2:ncol(table1_admission_method)]))

table1_admission_method <-
  dplyr::bind_rows(table1_admission_method, table1_CSP_admission_method)
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by whether
# ## or not the patient died within 30 days of their surgery:
# ##    1. "Alive within 30 days post-operation"
# ##    2. "Dead within 30 days post-operation" 
# ##    3. "Error: Surgery after death"
# ##    4. "No surgery recorded"
# ##    5. "No death recorded"
table1_postOp_mortality_30day <- 
  data_to_use %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::group_by(era, postOp_mortality_30day) %>%
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
table1_CSP_postOp_mortality_30day <- 
  data_to_use %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
  dplyr::group_by(COVIDSurg_data_collection_period, postOp_mortality_30day) %>%
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
  ) %>% `colnames<-`(c("era", colnames(table1_postOp_mortality_30day)[2:ncol(table1_postOp_mortality_30day)]))
table1_postOp_mortality_30day <-
  dplyr::bind_rows(table1_postOp_mortality_30day,table1_CSP_postOp_mortality_30day)
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by whether
# ## or not the patient died within 90 days of their surgery:
# ##    1. "Alive within 90 days post-operation"
# ##    2. "Dead within 90 days post-operation" 
# ##    3. "Error: Surgery after death"
# ##    4. "No surgery recorded"
# ##    5. "No death recorded"
table1_postOp_mortality_90day <- 
  data_to_use %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::group_by(era, postOp_mortality_90day) %>%
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
table1_CSP_postOp_mortality_90day <- 
  data_to_use %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
  dplyr::group_by(COVIDSurg_data_collection_period, postOp_mortality_90day) %>%
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
  ) %>% `colnames<-`(c("era", colnames(table1_postOp_mortality_90day)[2:ncol(table1_postOp_mortality_90day)]))
table1_postOp_mortality_90day <-
  dplyr::bind_rows(table1_postOp_mortality_90day,table1_CSP_postOp_mortality_90day)
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by whether
# ## or not the patient died within 6 months of their surgery:
# ##    1. "Alive within 6 months post-operation"
# ##    2. "Dead within 6 months post-operation" 
# ##    3. "Error: Surgery after death"
# ##    4. "No surgery recorded"
# ##    5. "No death recorded"
table1_postOp_mortality_6mth <- 
  data_to_use %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::group_by(era, postOp_mortality_6mth) %>%
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
table1_CSP_postOp_mortality_6mth <- 
  data_to_use %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
  dplyr::group_by(COVIDSurg_data_collection_period, postOp_mortality_6mth) %>%
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
  ) %>% `colnames<-`(c("era", colnames(table1_postOp_mortality_6mth)[2:ncol(table1_postOp_mortality_6mth)]))
table1_postOp_mortality_6mth <-
  dplyr::bind_rows(table1_postOp_mortality_6mth,table1_CSP_postOp_mortality_6mth)
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by whether
# ## or not the patient died within 12 months of their surgery:
# ##    1. "Alive within 12 months post-operation"
# ##    2. "Dead within 12 months post-operation" 
# ##    3. "Error: Surgery after death"
# ##    4. "No surgery recorded"
# ##    5. "No death recorded"
table1_postOp_mortality_12mth <- 
  data_to_use %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::group_by(era, postOp_mortality_12mth) %>%
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
table1_CSP_postOp_mortality_12mth <- 
  data_to_use %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
  dplyr::group_by(COVIDSurg_data_collection_period, postOp_mortality_12mth) %>%
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
  ) %>% `colnames<-`(c("era", colnames(table1_postOp_mortality_12mth)[2:ncol(table1_postOp_mortality_12mth)]))
table1_postOp_mortality_12mth <-
  dplyr::bind_rows(table1_postOp_mortality_12mth,table1_CSP_postOp_mortality_12mth)
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by whether
# ## or not the patient experience cerebrovascular complications (i.e. TIA or
# stroke) within 30 days of their surgery:
# ##    1. "No complications"
# ##    2. "Complications" 
# ##    3. "Ignore: Pre-operative complication"
# ##    4. "No cerebrovascular complication recorded"
# ##    5. "No surgery recorded"
table1_postOp_cerebrovascular_complication_30day <- 
  data_to_use %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::mutate(
    cerebrovascular_complication_30day = dplyr::case_when(
      postOp_cerebrovascular_complication_30day ==
        "Complications" ~ "Complications",
      TRUE ~ "No complications"
    )
  ) %>%
  dplyr::group_by(era, cerebrovascular_complication_30day) %>%
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
table1_CSP_postOp_cerebrovascular_complication_30day <- 
  data_to_use %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
  dplyr::mutate(
    cerebrovascular_complication_30day = dplyr::case_when(
      postOp_cerebrovascular_complication_30day ==
        "Complications" ~ "Complications", TRUE ~ "No complications")
  ) %>%
  dplyr::group_by(COVIDSurg_data_collection_period, cerebrovascular_complication_30day) %>%
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
  ) %>% `colnames<-`(c("era", colnames(table1_postOp_cerebrovascular_complication_30day)[2:ncol(table1_postOp_cerebrovascular_complication_30day)]))
table1_postOp_cerebrovascular_complication_30day <-
  dplyr::bind_rows(table1_postOp_cerebrovascular_complication_30day,table1_CSP_postOp_cerebrovascular_complication_30day)
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by whether
# ## or not the patient experience pulmonary complications (i.e. TIA or
# stroke) within 30 days of their surgery:
# ##    1. "No complications"
# ##    2. "Complications" 
# ##    3. "Ignore: Pre-operative complication"
# ##    4. "No pulmonary complication recorded"
# ##    5. "No surgery recorded"
table1_postOp_pulmonary_complication_30day <- 
  data_to_use %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::mutate(
    pulmonary_complication_30day = dplyr::case_when(
      postOp_pulmonary_complication_30day ==
        "Complications" ~ "Complications",
      TRUE ~ "No complications"
    )
  ) %>%
  dplyr::group_by(era, pulmonary_complication_30day) %>%
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
table1_CSP_postOp_pulmonary_complication_30day <- 
  data_to_use %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
  dplyr::mutate(
    pulmonary_complication_30day = dplyr::case_when(
      postOp_pulmonary_complication_30day ==
        "Complications" ~ "Complications", TRUE ~ "No complications")
  ) %>%
  dplyr::group_by(COVIDSurg_data_collection_period, pulmonary_complication_30day) %>%
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
  ) %>% `colnames<-`(c("era", colnames(table1_postOp_pulmonary_complication_30day)[2:ncol(table1_postOp_pulmonary_complication_30day)]))
table1_postOp_pulmonary_complication_30day <-
  dplyr::bind_rows(table1_postOp_pulmonary_complication_30day,table1_CSP_postOp_pulmonary_complication_30day)
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by whether
# ## or not the patient experience cardiac complications (i.e. TIA or
# stroke) within 30 days of their surgery:
# ##    1. "No complications"
# ##    2. "Complications" 
# ##    3. "Ignore: Pre-operative complication"
# ##    4. "No cardiac complication recorded"
# ##    5. "No surgery recorded"
table1_postOp_cardiac_complication_30day <- 
  data_to_use %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::mutate(
    cardiac_complication_30day = dplyr::case_when(
      postOp_cardiac_complication_30day ==
        "Complications" ~ "Complications",
      TRUE ~ "No complications"
    )
  ) %>%
  dplyr::group_by(era, cardiac_complication_30day) %>%
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
table1_CSP_postOp_cardiac_complication_30day <- 
  data_to_use %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
  dplyr::mutate(
    cardiac_complication_30day = dplyr::case_when(
      postOp_cardiac_complication_30day ==
        "Complications" ~ "Complications", TRUE ~ "No complications")
  ) %>%
  dplyr::group_by(COVIDSurg_data_collection_period, cardiac_complication_30day) %>%
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
  ) %>% `colnames<-`(c("era", colnames(table1_postOp_cardiac_complication_30day)[2:ncol(table1_postOp_cardiac_complication_30day)]))
table1_postOp_cardiac_complication_30day <-
  dplyr::bind_rows(table1_postOp_cardiac_complication_30day,table1_CSP_postOp_cardiac_complication_30day)
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by whether
# ## or not the patient had a record of chronic cardiac disease before their surgery.
# ##    1. Yes
# ##    2. No
table1_chronic_cardiac_disease <- 
  data_to_use %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::group_by(era, chronic_cardiac_disease) %>%
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
table1_CSP_chronic_cardiac_disease <- 
  data_to_use %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
  dplyr::group_by(COVIDSurg_data_collection_period, chronic_cardiac_disease) %>%
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
  ) %>% `colnames<-`(c("era", colnames(table1_chronic_cardiac_disease)[2:ncol(table1_chronic_cardiac_disease)]))

table1_chronic_cardiac_disease <-
  dplyr::bind_rows(table1_chronic_cardiac_disease, table1_CSP_chronic_cardiac_disease)
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by whether
# ## or not the patient had a record of diabetes before their surgery.
# ##    1. Yes
# ##    2. No
table1_diabetes <- 
  data_to_use %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::group_by(era, diabetes) %>%
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
table1_CSP_diabetes <- 
  data_to_use %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
  dplyr::group_by(COVIDSurg_data_collection_period, diabetes) %>%
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
  ) %>% `colnames<-`(c("era", colnames(table1_diabetes)[2:ncol(table1_diabetes)]))

table1_diabetes <- dplyr::bind_rows(table1_diabetes, table1_CSP_diabetes)
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by whether
# ## or not the patient had a record of chronic respiratory disease before their surgery.
# ##    1. Yes
# ##    2. No
table1_chronic_respiratory_disease <- 
  data_to_use %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::group_by(era, chronic_respiratory_disease) %>%
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
table1_CSP_chronic_respiratory_disease <- 
  data_to_use %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
  dplyr::group_by(COVIDSurg_data_collection_period, chronic_respiratory_disease) %>%
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
  ) %>% `colnames<-`(c("era", colnames(table1_chronic_respiratory_disease)[2:ncol(table1_chronic_respiratory_disease)]))

table1_chronic_respiratory_disease <-
  dplyr::bind_rows(table1_chronic_respiratory_disease, table1_CSP_chronic_respiratory_disease)
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by whether
# ## or not the patient had a record of cerebrovascular disease before their surgery.
# ##    1. Yes
# ##    2. No
table1_cerebrovascular_disease <- 
  data_to_use %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::group_by(era, cerebrovascular_disease) %>%
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
table1_CSP_cerebrovascular_disease <- 
  data_to_use %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
  dplyr::group_by(COVIDSurg_data_collection_period, cerebrovascular_disease) %>%
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
  ) %>% `colnames<-`(c("era", colnames(table1_cerebrovascular_disease)[2:ncol(table1_cerebrovascular_disease)]))

table1_cerebrovascular_disease <-
  dplyr::bind_rows(table1_cerebrovascular_disease, table1_CSP_cerebrovascular_disease)
# Clean up.
rm(table1_CSP_totals_preOp_infection_status, table1_CSP_ageGroup,
   table1_CSP_chronic_cardiac_disease, table1_CSP_chronic_respiratory_disease,
   table1_CSP_cerebrovascular_disease, table1_CSP_admission_method,
   table1_CSP_diabetes, table1_CSP_postOp_cerebrovascular_complication_30day,
   table1_CSP_postOp_cardiac_complication_30day, table1_CSP_Sex,
   table1_CSP_postOp_mortality_12mth, table1_CSP_postOp_mortality_30day,
   table1_CSP_postOp_mortality_6mth, table1_CSP_postOp_mortality_90day,
   table1_CSP_postOp_pulmonary_complication_30day
   )
# ----

#######################################################################
# Ensure tibbles show zero values when categories are not in the data #
#######################################################################
# ----
era_set <-
  c("Error: No surgery", "Pre-pandemic", "Pandemic no vaccine",
    "Pandemic with vaccine", "COVIDSurg data collection period",
    "Not COVIDSurg data collection period")
na_replace_list <-
  list("n_all_intervals" = 0,
     "n_infection_none" = 0,
     "n_infection_0to2wk" = 0,
     "n_infection_3to4wk" = 0,
     "n_infection_5to6wk" = 0,
     "n_infection_7wk" = 0)
# ## table1_totals_preOp_infection_status.
table1_totals_preOp_infection_status <- 
  expand.grid(
    era = era_set,
    preOperative_infection_status = 
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
    era = era_set,
    age_group_surgery = 
      c("0-29",
        "30-49",
        "50-69",
        "70-79",
        "80+",
        "Missing")) %>%
    dplyr::full_join(table1_ageGroup) %>%
    dplyr::arrange(era) %>%
    tidyr::replace_na(na_replace_list)
# ## table1_Sex.
table1_Sex <- 
  expand.grid(
    era = 
      era_set,
    Sex = 
      c("Female",
        "Male",
        "Missing")) %>%
  dplyr::full_join(table1_Sex) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(na_replace_list)
# ## table1_admission_method.
table1_admission_method <- 
  expand.grid(
    era = 
      era_set,
    category_admission_method = 
      c("Elective",
        "Emergency",
        "Missing")) %>%
  dplyr::full_join(table1_admission_method) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(na_replace_list)
# ## table1_postOp_mortality_30day.
table1_postOp_mortality_30day <- 
  expand.grid(
    era = era_set,
    postOp_mortality_30day = 
      c("Alive within 30 days post-operation",
        "Dead within 30 days post-operation",
        "Error: Surgery after death",
        "No death recorded",
        "No surgery recorded",
        "Missing")) %>%
  dplyr::full_join(table1_postOp_mortality_30day) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(na_replace_list)
# ## table1_postOp_mortality_90day.
table1_postOp_mortality_90day <- 
  expand.grid(
    era = era_set,
    postOp_mortality_90day = 
      c("Alive within 90 days post-operation",
        "Dead within 90 days post-operation",
        "Error: Surgery after death",
        "No death recorded",
        "No surgery recorded",
        "Missing")) %>%
  dplyr::full_join(table1_postOp_mortality_90day) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(na_replace_list)
# ## table1_postOp_mortality_6mth.
table1_postOp_mortality_6mth <- 
  expand.grid(
    era = era_set,
    postOp_mortality_6mth = 
      c("Alive within 6 months post-operation",
        "Dead within 6 months post-operation",
        "Error: Surgery after death",
        "No death recorded",
        "No surgery recorded",
        "Missing")) %>%
  dplyr::full_join(table1_postOp_mortality_6mth) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(na_replace_list)
# ## table1_postOp_mortality_12mth.
table1_postOp_mortality_12mth <- 
  expand.grid(
    era = era_set,
    postOp_mortality_12mth = 
      c("Alive within 12 months post-operation",
        "Dead within 12 months post-operation",
        "Error: Surgery after death",
        "No death recorded",
        "No surgery recorded",
        "Missing")) %>%
  dplyr::full_join(table1_postOp_mortality_12mth) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(na_replace_list)
# ## table1_postOp_pulmonary_complication_30day
table1_postOp_pulmonary_complication_30day <- 
  expand.grid(
    era = era_set,
    pulmonary_complication_30day = 
      c("No complications",
        "Complications")) %>%
  dplyr::full_join(table1_postOp_pulmonary_complication_30day) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(na_replace_list)
# ## table1_postOp_cardiac_complication_30day
table1_postOp_cardiac_complication_30day <- 
  expand.grid(
    era = era_set,
    cardiac_complication_30day = 
      c("No complications",
        "Complications")) %>%
  dplyr::full_join(table1_postOp_cardiac_complication_30day) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(na_replace_list)
# ## table1_postOp_cerebrovascular_complication_30day
table1_postOp_cerebrovascular_complication_30day <- 
  expand.grid(
    era = era_set,
    cerebrovascular_complication_30day = 
      c("No complications",
        "Complications")) %>%
  dplyr::full_join(table1_postOp_cerebrovascular_complication_30day) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(na_replace_list)
# ## table1_chronic_cardiac_disease.
table1_chronic_cardiac_disease <- 
  expand.grid(
    era = era_set,
    chronic_cardiac_disease = 
      c("Yes",
        "No",
        "Missing")) %>%
  dplyr::full_join(table1_chronic_cardiac_disease) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(na_replace_list)
# ## table1_diabetes.
table1_diabetes <- 
  expand.grid(
    era = era_set,
    diabetes = 
      c("Yes",
        "No",
        "Missing")) %>%
  dplyr::full_join(table1_diabetes) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(na_replace_list)
# ## table1_chronic_respiratory_disease.
table1_chronic_respiratory_disease <- 
  expand.grid(
    era = era_set,
    chronic_respiratory_disease = 
      c("Yes",
        "No",
        "Missing")) %>%
  dplyr::full_join(table1_chronic_respiratory_disease) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(na_replace_list)
# ## table1_cerebrovascular_disease.
table1_cerebrovascular_disease <- 
  expand.grid(
    era = era_set,
    cerebrovascular_disease = 
      c("Yes",
        "No",
        "Missing")) %>%
  dplyr::full_join(table1_cerebrovascular_disease) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(na_replace_list)
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
#   x = table1_admission_method,
#   file = here::here("output",paste0("table1_admission_method",sensitivity_cohort,".csv"))
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
#   x = table1_postOp_pulmonary_complication_30day,
#   file = here::here("output",paste0("table1_postOp_pulmonary_complication_30day",sensitivity_cohort,".csv"))
# )
# write.csv(
#   x = table1_postOp_cardiac_complication_30day,
#   file = here::here("output",paste0("table1_postOp_cardiac_complication_30day",sensitivity_cohort,".csv"))
# )
# write.csv(
#   x = table1_postOp_cerebrovascular_complication_30day,
#   file = here::here("output",paste0("table1_postOp_cerebrovascular_complication_30day",sensitivity_cohort,".csv"))
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
# write.csv(
#   x = table1_cerebrovascular_disease,
#   file = here::here("output",paste0("table1_cerebrovascular_disease",sensitivity_cohort,".csv"))
# )
# ----

######################################
# Make vectors to inform the tables. #
###################################### 
# ----
# Age band.
list_ageGroup <-
  fnc_countsAndPercentages(table_to_use = table1_ageGroup,
                           strata = "_ageGroup",
                           strata_col = "age_group_surgery")
# Sex.
list_Sex <-
  fnc_countsAndPercentages(table_to_use = table1_Sex,
                           strata = "_Sex",
                           strata_col = "Sex")
# Admission method.
list_admission_method <-
  fnc_countsAndPercentages(table_to_use = table1_admission_method,
                           strata = "_admission_method",
                           strata_col = "category_admission_method")
# 30-day post-operative mortality.
list_postOp_mortality_30day <-
  fnc_countsAndPercentages(table_to_use = table1_postOp_mortality_30day,
                           strata = "_postOp_mortality_30day",
                           strata_col = "postOp_mortality_30day")
# 30-day post-operative mortality.
list_postOp_mortality_30day <-
  fnc_countsAndPercentages(table_to_use = table1_postOp_mortality_30day,
                           strata = "_postOp_mortality_30day",
                           strata_col = "postOp_mortality_30day")
# 90-day post-operative mortality.
list_postOp_mortality_90day <-
  fnc_countsAndPercentages(table_to_use = table1_postOp_mortality_90day,
                           strata = "_postOp_mortality_90day",
                           strata_col = "postOp_mortality_90day")
# 6-month post-operative mortality.
list_postOp_mortality_6mth <-
  fnc_countsAndPercentages(table_to_use = table1_postOp_mortality_6mth,
                           strata = "_postOp_mortality_6mth",
                           strata_col = "postOp_mortality_6mth")
# 12-month post-operative mortality.
list_postOp_mortality_12mth <-
  fnc_countsAndPercentages(table_to_use = table1_postOp_mortality_12mth,
                           strata = "_postOp_mortality_12mth",
                           strata_col = "postOp_mortality_12mth")
# 30-day post-operative pulmonary complication.
list_postOp_pulmonary_complication_30day <-
  fnc_countsAndPercentages(table_to_use = table1_postOp_pulmonary_complication_30day,
                           strata = "_pulmonary_complication_30day",
                           strata_col = "pulmonary_complication_30day")
# 30-day post-operative cardiac complication.
list_postOp_cardiac_complication_30day <-
  fnc_countsAndPercentages(table_to_use = table1_postOp_cardiac_complication_30day,
                           strata = "_cardiac_complication_30day",
                           strata_col = "cardiac_complication_30day")
# 30-day post-operative pulmonary complication.
list_postOp_cerebrovascular_complication_30day <-
  fnc_countsAndPercentages(table_to_use = table1_postOp_cerebrovascular_complication_30day,
                           strata = "_cerebrovascular_complication_30day",
                           strata_col = "cerebrovascular_complication_30day")
# Chronic cardiac disease.
list_chronic_cardiac_disease <-
  fnc_countsAndPercentages(table_to_use = table1_chronic_cardiac_disease,
                           strata = "_chronic_cardiac_disease",
                           strata_col = "chronic_cardiac_disease")
# Diabetes.
list_diabetes <-
  fnc_countsAndPercentages(table_to_use = table1_diabetes,
                           strata = "_diabetes",
                           strata_col = "diabetes")
# Chronic respiratory disease.
list_chronic_respiratory_disease <-
  fnc_countsAndPercentages(table_to_use = table1_chronic_respiratory_disease,
                           strata = "_chronic_respiratory_disease",
                           strata_col = "chronic_respiratory_disease")
# Cerebrovascular disease.
list_cerebrovascular_disease <-
  fnc_countsAndPercentages(table_to_use = table1_cerebrovascular_disease,
                           strata = "_cerebrovascular_disease",
                           strata_col = "cerebrovascular_disease")
# ----

####################
# Make the tables. #
####################
n_intervals_less_than_7wks <- c("n_infection_0to2wk", "n_infection_3to4wk",
                              "n_infection_5to6wk")
pct_intervals_less_than_7wks <- c("pct_infection_0to2wk", "pct_infection_3to4wk",
                                "pct_infection_5to6wk")
# Pre-pandemic tables. ----
# ## Demographics table.
tbl_PP_demogs <-
  rbind(
    list_ageGroup$PP_ageGroup,
    list_Sex$PP_Sex,
    list_admission_method$PP_admission_method,
    list_chronic_cardiac_disease$PP_chronic_cardiac_disease,
    list_diabetes$PP_diabetes,
    list_chronic_respiratory_disease$PP_chronic_respiratory_disease,
    list_cerebrovascular_disease$PP_cerebrovascular_disease) %>%
  dplyr::select(strata, n_all_intervals, pct_all_intervals) %>%
    tibble::add_column(., c(
      rep("Age group",5),
      rep("Sex",2),
      rep("Admission method",2),
      rep("Chronic cardiac disease",2),
      rep("Diabetes",2),
      rep("Chronic respiratory disease",2),
      rep("Cerebrovascular disease",2)),
      .before = "strata"
      ) %>%
    `colnames<-`(c("variable", "strata", "n", "pct"))
# Save table.
write.csv(
  x = tbl_PP_demogs,
  file = here::here("output",paste0("table1Demogs_PP",sensitivity_cohort,".csv"))
)
# ## Outcomes table.
tbl_PP_outcomes <-
  rbind(
    list_postOp_mortality_30day$PP_postOp_mortality_30day,
    list_postOp_mortality_90day$PP_postOp_mortality_90day,
    list_postOp_mortality_6mth$PP_postOp_mortality_6mth,
    list_postOp_mortality_12mth$PP_postOp_mortality_12mth,
    list_postOp_pulmonary_complication_30day$PP_pulmonary_complication_30day,
    list_postOp_cardiac_complication_30day$PP_cardiac_complication_30day,
    list_postOp_cerebrovascular_complication_30day$PP_cerebrovascular_complication_30day) %>%
  dplyr::select(strata, n_all_intervals, pct_all_intervals) %>%
  dplyr::filter(!strata %in% c("Error: Surgery after death",
                               "No death recorded", "No surgery recorded")) %>%
  tibble::add_column(., c(
    rep("30-day post-operative mortality",2),
    rep("90-day post-operative mortality",2),
    rep("6-month post-operative mortality",2),
    rep("12-month post-operative mortality",2),
    rep("30-day post-operative pulmonary complication",2),
    rep("30-day post-operative cardiac complication",2),
    rep("30-day post-operative cerebrovascular complication",2)),
    .before = "strata"
  ) %>%
  `colnames<-`(c("variable", "strata", "n", "pct"))
# Save table.
write.csv(
  x = tbl_PP_outcomes,
  file = here::here("output",paste0("table1Outcomes_PP",sensitivity_cohort,".csv"))
)
# ----

# Pandemic no vaccine tables. ----
# ## Demographics table.
tbl_PNV_demogs <-
  rbind(
    list_ageGroup$PNV_ageGroup,
    list_Sex$PNV_Sex,
    list_admission_method$PNV_admission_method,
    list_chronic_cardiac_disease$PNV_chronic_cardiac_disease,
    list_diabetes$PNV_diabetes,
    list_chronic_respiratory_disease$PNV_chronic_respiratory_disease,
    list_cerebrovascular_disease$PNV_cerebrovascular_disease) %>%
  tibble::add_column(., c(
    rep("Age group",5),
    rep("Sex",2),
    rep("Admission method",2),
    rep("Chronic cardiac disease",2),
    rep("Diabetes",2),
    rep("Chronic respiratory disease",2),
    rep("Cerebrovascular disease",2)),
    .before = "strata"
  ) %>%
  `colnames<-`(c("variable", "strata", colnames(.)[3:ncol(.)]))
# Save table.
write.csv(
  x = tbl_PNV_demogs,
  file = here::here("output",paste0("table1Demogs_PNV",sensitivity_cohort,".csv"))
)
# Make table with 7week threshold instead of interim intervals.
tbl_PNV_demogs_7wkThreshold <- fnc_make7wkTable(tbl_PNV_demogs)
write.csv(
  x = tbl_PNV_demogs_7wkThreshold,
  file = here::here("output",paste0("table1Demogs_PNV",sensitivity_cohort,"_7wkThreshold.csv"))
)
# ## Outcomes table.
tbl_PNV_outcomes <-
  rbind(
    list_postOp_mortality_30day$PNV_postOp_mortality_30day,
    list_postOp_mortality_90day$PNV_postOp_mortality_90day,
    list_postOp_mortality_6mth$PNV_postOp_mortality_6mth,
    list_postOp_mortality_12mth$PNV_postOp_mortality_12mth,
    list_postOp_pulmonary_complication_30day$PNV_pulmonary_complication_30day,
    list_postOp_cardiac_complication_30day$PNV_cardiac_complication_30day,
    list_postOp_cerebrovascular_complication_30day$PNV_cerebrovascular_complication_30day) %>%
  dplyr::filter(!strata %in% c("Error: Surgery after death",
                               "No death recorded", "No surgery recorded")) %>%
  tibble::add_column(., c(
    rep("30-day post-operative mortality",2),
    rep("90-day post-operative mortality",2),
    rep("6-month post-operative mortality",2),
    rep("12-month post-operative mortality",2),
    rep("30-day post-operative pulmonary complication",2),
    rep("30-day post-operative cardiac complication",2),
    rep("30-day post-operative cerebrovascular complication",2)),
    .before = "strata"
  ) %>%
  `colnames<-`(c("variable", "strata", colnames(.)[3:ncol(.)]))
# Save table.
write.csv(
  x = tbl_PNV_outcomes,
  file = here::here("output",paste0("table1Outcomes_PNV",sensitivity_cohort,".csv"))
)
# Make table with only 7week threshold.
tbl_PNV_outcomes_7wkThreshold <- fnc_make7wkTable(tbl_PNV_outcomes)
write.csv(
  x = tbl_PNV_outcomes_7wkThreshold,
  file = here::here("output",paste0("table1Outcomes_PNV",sensitivity_cohort,"_7wkThreshold.csv"))
)
# ----

# COVIDSurg data collection period tables. ----
# ## Demographics table.
tbl_CSP_demogs <-
  rbind(
    list_ageGroup$CSP_ageGroup,
    list_Sex$CSP_Sex,
    list_admission_method$CSP_admission_method,
    list_chronic_cardiac_disease$CSP_chronic_cardiac_disease,
    list_diabetes$CSP_diabetes,
    list_chronic_respiratory_disease$CSP_chronic_respiratory_disease,
    list_cerebrovascular_disease$CSP_cerebrovascular_disease) %>%
  tibble::add_column(., c(
    rep("Age group",5),
    rep("Sex",2),
    rep("Admission method",2),
    rep("Chronic cardiac disease",2),
    rep("Diabetes",2),
    rep("Chronic respiratory disease",2),
    rep("Cerebrovascular disease",2)),
    .before = "strata"
  ) %>%
  `colnames<-`(c("variable", "strata", colnames(.)[3:ncol(.)]))
# Save table.
write.csv(
  x = tbl_CSP_demogs,
  file = here::here("output",paste0("table1Demogs_CSP",sensitivity_cohort,".csv"))
)
# Make table with 7week threshold instead of interim intervals.
tbl_CSP_demogs_7wkThreshold <- fnc_make7wkTable(tbl_CSP_demogs)
write.csv(
  x = tbl_CSP_demogs_7wkThreshold,
  file = here::here("output",paste0("table1Demogs_CSP",sensitivity_cohort,"_7wkThreshold.csv"))
)
# ## Outcomes table.
tbl_CSP_outcomes <-
  rbind(
    list_postOp_mortality_30day$CSP_postOp_mortality_30day,
    list_postOp_mortality_90day$CSP_postOp_mortality_90day,
    list_postOp_mortality_6mth$CSP_postOp_mortality_6mth,
    list_postOp_mortality_12mth$CSP_postOp_mortality_12mth,
    list_postOp_pulmonary_complication_30day$CSP_pulmonary_complication_30day,
    list_postOp_cardiac_complication_30day$CSP_cardiac_complication_30day,
    list_postOp_cerebrovascular_complication_30day$CSP_cerebrovascular_complication_30day) %>%
  dplyr::filter(!strata %in% c("Error: Surgery after death",
                               "No death recorded", "No surgery recorded")) %>%
  tibble::add_column(., c(
    rep("30-day post-operative mortality",2),
    rep("90-day post-operative mortality",2),
    rep("6-month post-operative mortality",2),
    rep("12-month post-operative mortality",2),
    rep("30-day post-operative pulmonary complication",2),
    rep("30-day post-operative cardiac complication",2),
    rep("30-day post-operative cerebrovascular complication",2)),
    .before = "strata"
  ) %>%
  `colnames<-`(c("variable", "strata", colnames(.)[3:ncol(.)]))
# Save table.
write.csv(
  x = tbl_CSP_outcomes,
  file = here::here("output",paste0("table1Outcomes_CSP",sensitivity_cohort,".csv"))
)
# Make table with only 7week threshold.
tbl_CSP_outcomes_7wkThreshold <- fnc_make7wkTable(tbl_CSP_outcomes)
write.csv(
  x = tbl_CSP_outcomes_7wkThreshold,
  file = here::here("output",paste0("table1Outcomes_CSP",sensitivity_cohort,"_7wkThreshold.csv"))
)
# ----

# Pandemic with vaccine tables. ----
# ## Demographics table.
tbl_PWV_demogs <-
  rbind(
    list_ageGroup$PWV_ageGroup,
    list_Sex$PWV_Sex,
    list_admission_method$PWV_admission_method,
    list_chronic_cardiac_disease$PWV_chronic_cardiac_disease,
    list_diabetes$PWV_diabetes,
    list_chronic_respiratory_disease$PWV_chronic_respiratory_disease,
    list_cerebrovascular_disease$PWV_cerebrovascular_disease) %>%
  tibble::add_column(., c(
    rep("Age group",5),
    rep("Sex",2),
    rep("Admission method",2),
    rep("Chronic cardiac disease",2),
    rep("Diabetes",2),
    rep("Chronic respiratory disease",2),
    rep("Cerebrovascular disease",2)),
    .before = "strata"
  ) %>%
  `colnames<-`(c("variable", "strata", colnames(.)[3:ncol(.)]))
# Save table.
write.csv(
  x = tbl_PWV_demogs,
  file = here::here("output",paste0("table1Demogs_PWV",sensitivity_cohort,".csv"))
)
# Make table with 7week threshold instead of interim intervals.
tbl_PWV_demogs_7wkThreshold <- fnc_make7wkTable(tbl_PWV_demogs)
write.csv(
  x = tbl_PWV_demogs_7wkThreshold,
  file = here::here("output",paste0("table1Demogs_PWV",sensitivity_cohort,"_7wkThreshold.csv"))
)
# ## Outcomes table.
tbl_PWV_outcomes <-
  rbind(
    list_postOp_mortality_30day$PWV_postOp_mortality_30day,
    list_postOp_mortality_90day$PWV_postOp_mortality_90day,
    list_postOp_mortality_6mth$PWV_postOp_mortality_6mth,
    list_postOp_mortality_12mth$PWV_postOp_mortality_12mth,
    list_postOp_pulmonary_complication_30day$PWV_pulmonary_complication_30day,
    list_postOp_cardiac_complication_30day$PWV_cardiac_complication_30day,
    list_postOp_cerebrovascular_complication_30day$PWV_cerebrovascular_complication_30day) %>%
  dplyr::filter(!strata %in% c("Error: Surgery after death",
                               "No death recorded", "No surgery recorded")) %>%
  tibble::add_column(., c(
    rep("30-day post-operative mortality",2),
    rep("90-day post-operative mortality",2),
    rep("6-month post-operative mortality",2),
    rep("12-month post-operative mortality",2),
    rep("30-day post-operative pulmonary complication",2),
    rep("30-day post-operative cardiac complication",2),
    rep("30-day post-operative cerebrovascular complication",2)),
    .before = "strata"
  ) %>%
  `colnames<-`(c("variable", "strata", colnames(.)[3:ncol(.)]))
# Save table.
write.csv(
  x = tbl_PWV_outcomes,
  file = here::here("output",paste0("table1Outcomes_PWV",sensitivity_cohort,".csv"))
)
# Make table with only 7week threshold.
tbl_PWV_outcomes_7wkThreshold <- fnc_make7wkTable(tbl_PWV_outcomes)
write.csv(
  x = tbl_PWV_outcomes_7wkThreshold,
  file = here::here("output",paste0("table1Outcomes_PWV",sensitivity_cohort,"_7wkThreshold.csv"))
)
# ----
