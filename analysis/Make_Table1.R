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


##########################################
# Make tibbles that will inform Table 1. #
##########################################
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
# ##    3. Pandemic with vaccine
table1_totals_preOp_infection_status <- 
  data_to_use %>% dplyr::group_by(era,
                      preOperative_infection_status) %>% dplyr::summarise(n = n())
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
table1_CSP_ageGroup <- 
  data_to_use %>% dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
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
  dplyr::bind_rows(table1_ageGroup, table1_CSP_ageGroup)# %>% 
  #dplyr::arrange(era, gsub("\\D","",age_group_surgery))
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
table1_CSP_Sex <- 
  data_to_use %>% dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
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
  data_to_use %>% dplyr::group_by(era, category_admission_method) %>%
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
  data_to_use %>% dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
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
table1_CSP_postOp_mortality_30day <- 
  data_to_use %>% dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
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
table1_CSP_postOp_mortality_90day <- 
  data_to_use %>% dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
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
table1_CSP_postOp_mortality_6mth <- 
  data_to_use %>% dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
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
table1_CSP_postOp_mortality_12mth <- 
  data_to_use %>% dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
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
# ##    1. "No cerebrovascular complication within 30 days post-operation"
# ##    2. "Cerebrovascular complication within 30 days post-operation" 
# ##    3. "Ignore: Pre-operative complication"
# ##    4. "No cerebrovascular complication recorded"
# ##    5. "No surgery recorded"
table1_postOp_cerebrovascular_complication_30day <- 
  data_to_use %>% dplyr::group_by(era, postOp_cerebrovascular_complication_30day) %>%
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
  data_to_use %>% dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
  dplyr::group_by(COVIDSurg_data_collection_period, postOp_cerebrovascular_complication_30day) %>%
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
# ## or not the patient had a record of chronic cardiac disease before their surgery.
# ##    1. Yes
# ##    2. No
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
table1_CSP_chronic_cardiac_disease <- 
  data_to_use %>% dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
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
table1_CSP_diabetes <- 
  data_to_use %>% dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
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
table1_CSP_chronic_respiratory_disease <- 
  data_to_use %>% dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
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
# Clean up.
rm(table1_CSP_totals_preOp_infection_status, table1_CSP_ageGroup,
   table1_CSP_chronic_cardiac_disease, table1_CSP_chronic_respiratory_disease,
   table1_CSP_admission_method, table1_CSP_diabetes,
   table1_CSP_postOp_cerebrovascular_complication_30day,
   table1_CSP_postOp_mortality_12mth, table1_CSP_postOp_mortality_30day,
   table1_CSP_postOp_mortality_6mth, table1_CSP_postOp_mortality_90day,
   table1_CSP_Sex)
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
# ## table1_postOp_cerebrovascular_complication_30day
table1_postOp_cerebrovascular_complication_30day <- 
  expand.grid(
    era = era_set,
    postOp_cerebrovascular_complication_30day = 
      c("No cerebrovascular complication within 30 days post-operation",
        "Cerebrovascular complication within 30 days post-operation",
        "Ignore: Pre-operative complication",
        "No cerebrovascular complication recorded",
        "No surgery recorded",
        "Missing")) %>%
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

# ## COVIDSurg data collection period.
# ## ## Get counts per intervals and overall.
CSP_n_ageGroup <- 
  table1_ageGroup %>%  dplyr::filter(era=="COVIDSurg data collection period", age_group_surgery !="Missing") %>%
  dplyr::arrange(age_group_surgery) %>% dplyr::ungroup() %>% dplyr::select(-c("era", "age_group_surgery"))
# ## ## Get percentages per intervals and overall.
CSP_pct_ageGroup <- 
  table1_ageGroup %>% dplyr::filter(era=="COVIDSurg data collection period",
                                    age_group_surgery !="Missing") %>%
  select(-c("era", "age_group_surgery")) %>%
  colSums() %>% sweep(CSP_n_ageGroup, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0
  )) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
CSP_ageGroup <- matrix(0,
                       nrow = length(rownames(CSP_n_ageGroup)),
                       ncol = length(colnames(CSP_n_ageGroup))*2) %>%
  as.data.frame()
CSP_ageGroup[,seq(1,length(colnames(CSP_ageGroup)),2)] <- CSP_n_ageGroup
CSP_ageGroup[,seq(2,length(colnames(CSP_ageGroup)),2)] <- CSP_pct_ageGroup
colnames(CSP_ageGroup)[seq(1,length(colnames(CSP_ageGroup)),2)] <- colnames(CSP_n_ageGroup)
colnames(CSP_ageGroup)[seq(2,length(colnames(CSP_ageGroup)),2)] <- colnames(CSP_pct_ageGroup)
CSP_ageGroup <-
  table1_ageGroup %>% 
  dplyr::filter(era=="COVIDSurg data collection period",
                age_group_surgery !="Missing") %>%
  dplyr::arrange(age_group_surgery) %>%
  dplyr::select("age_group_surgery") %>% dplyr::bind_cols(CSP_ageGroup)
# ## ## Clean up.
rm(CSP_n_ageGroup, CSP_pct_ageGroup)

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

# ## COVIDSurg data collection period.
# ## ## Get counts per intervals and overall.
CSP_n_Sex <- 
  table1_Sex %>%  dplyr::filter(era=="COVIDSurg data collection period", Sex!="Missing") %>%
  dplyr::arrange(Sex) %>% dplyr::ungroup() %>% dplyr::select(-c("era", "Sex"))
# ## ## Get percentages per intervals and overall.
CSP_pct_Sex <- 
  table1_Sex %>% dplyr::filter(era=="COVIDSurg data collection period", Sex!="Missing") %>% select(-c("era", "Sex")) %>%
  colSums() %>% sweep(CSP_n_Sex, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0
  )) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
CSP_Sex <- matrix(0,
                  nrow = length(rownames(CSP_n_Sex)),
                  ncol = length(colnames(CSP_n_Sex))*2) %>%
  as.data.frame()
CSP_Sex[,seq(1,length(colnames(CSP_Sex)),2)] <- CSP_n_Sex
CSP_Sex[,seq(2,length(colnames(CSP_Sex)),2)] <- CSP_pct_Sex
colnames(CSP_Sex)[seq(1,length(colnames(CSP_Sex)),2)] <- colnames(CSP_n_Sex)
colnames(CSP_Sex)[seq(2,length(colnames(CSP_Sex)),2)] <- colnames(CSP_pct_Sex)
CSP_Sex <- table1_Sex %>%  dplyr::filter(era=="COVIDSurg data collection period", Sex!="Missing") %>%
  dplyr::arrange(Sex) %>% dplyr::select("Sex") %>% dplyr::bind_cols(CSP_Sex)
# ## ## Clean up.
rm(CSP_n_Sex, CSP_pct_Sex)

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

# Admission method. ----
# ## Pre-pandemic
# ## ## Get counts per intervals and overall.
PP_n_admission_method <- 
  table1_admission_method %>% dplyr::filter(era=="Pre-pandemic", category_admission_method!="Missing") %>%
  dplyr::arrange(category_admission_method) %>% dplyr::ungroup() %>% dplyr::select("n_all_intervals")
# ## ## Get percentages per intervals and overall.
PP_pct_admission_method <- (PP_n_admission_method / sum(PP_n_admission_method)) %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0)) %>%
  `colnames<-`(c("pct_all_intervals"))
PP_admission_method <- table1_admission_method %>% dplyr::filter(era=="Pandemic no vaccine", category_admission_method!="Missing") %>%
  dplyr::arrange(category_admission_method) %>% dplyr::select("category_admission_method") %>% dplyr::bind_cols(., PP_n_admission_method, PP_pct_admission_method)
# ## ## Clean up.
rm(PP_n_admission_method, PP_pct_admission_method)

# ## Pandemic no vaccine.
# ## ## Get counts per intervals and overall.
PNV_n_admission_method <- 
  table1_admission_method %>%  dplyr::filter(era=="Pandemic no vaccine", category_admission_method!="Missing") %>%
  dplyr::arrange(category_admission_method) %>% dplyr::ungroup() %>% dplyr::select(-c("era", "category_admission_method"))
# ## ## Get percentages per intervals and overall.
PNV_pct_admission_method <- 
  table1_admission_method %>% dplyr::filter(era=="Pandemic no vaccine", category_admission_method!="Missing") %>% select(-c("era", "category_admission_method")) %>%
  colSums() %>% sweep(PNV_n_admission_method, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0
  )) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
PNV_admission_method <- matrix(0,
                  nrow = length(rownames(PNV_n_admission_method)),
                  ncol = length(colnames(PNV_n_admission_method))*2) %>%
  as.data.frame()
PNV_admission_method[,seq(1,length(colnames(PNV_admission_method)),2)] <- PNV_n_admission_method
PNV_admission_method[,seq(2,length(colnames(PNV_admission_method)),2)] <- PNV_pct_admission_method
colnames(PNV_admission_method)[seq(1,length(colnames(PNV_admission_method)),2)] <- colnames(PNV_n_admission_method)
colnames(PNV_admission_method)[seq(2,length(colnames(PNV_admission_method)),2)] <- colnames(PNV_pct_admission_method)
PNV_admission_method <- table1_admission_method %>%  dplyr::filter(era=="Pandemic no vaccine", category_admission_method!="Missing") %>%
  dplyr::arrange(category_admission_method) %>% dplyr::select("category_admission_method") %>% dplyr::bind_cols(PNV_admission_method)
# ## ## Clean up.
rm(PNV_n_admission_method, PNV_pct_admission_method)

# ## COVIDSurg data collection period.
# ## ## Get counts per intervals and overall.
CSP_n_admission_method <- 
  table1_admission_method %>%  dplyr::filter(era=="COVIDSurg data collection period", category_admission_method!="Missing") %>%
  dplyr::arrange(category_admission_method) %>% dplyr::ungroup() %>% dplyr::select(-c("era", "category_admission_method"))
# ## ## Get percentages per intervals and overall.
CSP_pct_admission_method <- 
  table1_admission_method %>% dplyr::filter(era=="COVIDSurg data collection period", category_admission_method!="Missing") %>% select(-c("era", "category_admission_method")) %>%
  colSums() %>% sweep(CSP_n_admission_method, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0
  )) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
CSP_admission_method <- matrix(0,
                  nrow = length(rownames(CSP_n_admission_method)),
                  ncol = length(colnames(CSP_n_admission_method))*2) %>%
  as.data.frame()
CSP_admission_method[,seq(1,length(colnames(CSP_admission_method)),2)] <- CSP_n_admission_method
CSP_admission_method[,seq(2,length(colnames(CSP_admission_method)),2)] <- CSP_pct_admission_method
colnames(CSP_admission_method)[seq(1,length(colnames(CSP_admission_method)),2)] <- colnames(CSP_n_admission_method)
colnames(CSP_admission_method)[seq(2,length(colnames(CSP_admission_method)),2)] <- colnames(CSP_pct_admission_method)
CSP_admission_method <- table1_admission_method %>%  dplyr::filter(era=="COVIDSurg data collection period", category_admission_method!="Missing") %>%
  dplyr::arrange(category_admission_method) %>% dplyr::select("category_admission_method") %>% dplyr::bind_cols(CSP_admission_method)
# ## ## Clean up.
rm(CSP_n_admission_method, CSP_pct_admission_method)

# ## Pandemic with vaccine.
# ## ## Get counts per intervals and overall.
PWV_n_admission_method <- 
  table1_admission_method %>%  dplyr::filter(era=="Pandemic with vaccine", category_admission_method!="Missing") %>%
  dplyr::arrange(category_admission_method) %>% dplyr::ungroup() %>% dplyr::select(-c("era", "category_admission_method"))
# ## ## Get percentages per intervals and overall.
PWV_pct_admission_method <- 
  table1_admission_method %>% dplyr::filter(era=="Pandemic with vaccine") %>% select(-c("era", "category_admission_method")) %>%
  colSums() %>% sweep(PWV_n_admission_method, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0
  )) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
PWV_admission_method <- matrix(0,
                  nrow = length(rownames(PWV_n_admission_method)),
                  ncol = length(colnames(PWV_n_admission_method))*2) %>%
  as.data.frame()
PWV_admission_method[,seq(1,length(colnames(PWV_admission_method)),2)] <- PWV_n_admission_method
PWV_admission_method[,seq(2,length(colnames(PWV_admission_method)),2)] <- PWV_pct_admission_method
colnames(PWV_admission_method)[seq(1,length(colnames(PWV_admission_method)),2)] <- colnames(PWV_n_admission_method)
colnames(PWV_admission_method)[seq(2,length(colnames(PWV_admission_method)),2)] <- colnames(PWV_pct_admission_method)
PWV_admission_method <- table1_admission_method %>%  dplyr::filter(era=="Pandemic with vaccine", category_admission_method!="Missing") %>%
  dplyr::arrange(category_admission_method) %>% dplyr::select("category_admission_method") %>% dplyr::bind_cols(PWV_admission_method)
# ## ## Clean up
rm(PWV_n_admission_method, PWV_pct_admission_method)
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

# ## COVIDSurg data collection period.
# ## ## Get counts per intervals and overall.
CSP_n_postOp_mortality_30day <- 
  table1_postOp_mortality_30day %>%
  dplyr::filter(era=="COVIDSurg data collection period",
                (postOp_mortality_30day=="Alive within 30 days post-operation"|
                   postOp_mortality_30day=="Dead within 30 days post-operation")) %>%
  dplyr::arrange(postOp_mortality_30day) %>%
  dplyr::ungroup() %>% dplyr::select(-c("era", "postOp_mortality_30day"))
# ## ## Get percentages per intervals and overall.
CSP_pct_postOp_mortality_30day <- 
  table1_postOp_mortality_30day %>%
  dplyr::filter(era=="COVIDSurg data collection period",
                (postOp_mortality_30day=="Alive within 30 days post-operation"|
                   postOp_mortality_30day=="Dead within 30 days post-operation")) %>%
  select(-c("era", "postOp_mortality_30day")) %>%
  colSums() %>% sweep(CSP_n_postOp_mortality_30day, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0)) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
CSP_postOp_mortality_30day <-
  matrix(0,
         nrow = length(rownames(CSP_n_postOp_mortality_30day)),
         ncol = length(colnames(CSP_n_postOp_mortality_30day))*2) %>%
  as.data.frame()
CSP_postOp_mortality_30day[,seq(1,length(colnames(CSP_postOp_mortality_30day)),2)] <- CSP_n_postOp_mortality_30day
CSP_postOp_mortality_30day[,seq(2,length(colnames(CSP_postOp_mortality_30day)),2)] <- CSP_pct_postOp_mortality_30day
colnames(CSP_postOp_mortality_30day)[seq(1,length(colnames(CSP_postOp_mortality_30day)),2)] <- colnames(CSP_n_postOp_mortality_30day)
colnames(CSP_postOp_mortality_30day)[seq(2,length(colnames(CSP_postOp_mortality_30day)),2)] <- colnames(CSP_pct_postOp_mortality_30day)
CSP_postOp_mortality_30day <-
  table1_postOp_mortality_30day %>%
  dplyr::filter(era=="COVIDSurg data collection period",
                (postOp_mortality_30day=="Alive within 30 days post-operation"|
                   postOp_mortality_30day=="Dead within 30 days post-operation")) %>%
  dplyr::arrange(postOp_mortality_30day) %>%
  dplyr::select("postOp_mortality_30day") %>%
  dplyr::bind_cols(CSP_postOp_mortality_30day)
# ## ## Clean up.
rm(CSP_n_postOp_mortality_30day, CSP_pct_postOp_mortality_30day)

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

# ## COVIDSurg data collection period.
# ## ## Get counts per intervals and overall.
CSP_n_postOp_mortality_90day <- 
  table1_postOp_mortality_90day %>%
  dplyr::filter(era=="COVIDSurg data collection period",
                (postOp_mortality_90day=="Alive within 90 days post-operation"|
                   postOp_mortality_90day=="Dead within 90 days post-operation")) %>%
  dplyr::arrange(postOp_mortality_90day) %>%
  dplyr::ungroup() %>% dplyr::select(-c("era", "postOp_mortality_90day"))
# ## ## Get percentages per intervals and overall.
CSP_pct_postOp_mortality_90day <- 
  table1_postOp_mortality_90day %>%
  dplyr::filter(era=="COVIDSurg data collection period",
                (postOp_mortality_90day=="Alive within 90 days post-operation"|
                   postOp_mortality_90day=="Dead within 90 days post-operation")) %>%
  select(-c("era", "postOp_mortality_90day")) %>%
  colSums() %>% sweep(CSP_n_postOp_mortality_90day, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0)) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
CSP_postOp_mortality_90day <-
  matrix(0,
         nrow = length(rownames(CSP_n_postOp_mortality_90day)),
         ncol = length(colnames(CSP_n_postOp_mortality_90day))*2) %>%
  as.data.frame()
CSP_postOp_mortality_90day[,seq(1,length(colnames(CSP_postOp_mortality_90day)),2)] <- CSP_n_postOp_mortality_90day
CSP_postOp_mortality_90day[,seq(2,length(colnames(CSP_postOp_mortality_90day)),2)] <- CSP_pct_postOp_mortality_90day
colnames(CSP_postOp_mortality_90day)[seq(1,length(colnames(CSP_postOp_mortality_90day)),2)] <- colnames(CSP_n_postOp_mortality_90day)
colnames(CSP_postOp_mortality_90day)[seq(2,length(colnames(CSP_postOp_mortality_90day)),2)] <- colnames(CSP_pct_postOp_mortality_90day)
CSP_postOp_mortality_90day <-
  table1_postOp_mortality_90day %>%
  dplyr::filter(era=="COVIDSurg data collection period",
                (postOp_mortality_90day=="Alive within 90 days post-operation"|
                   postOp_mortality_90day=="Dead within 90 days post-operation")) %>%
  dplyr::arrange(postOp_mortality_90day) %>%
  dplyr::select("postOp_mortality_90day") %>%
  dplyr::bind_cols(CSP_postOp_mortality_90day)
# ## ## Clean up.
rm(CSP_n_postOp_mortality_90day, CSP_pct_postOp_mortality_90day)

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

# ## COVIDSurg data collection period.
# ## ## Get counts per intervals and overall.
CSP_n_postOp_mortality_6mth <- 
  table1_postOp_mortality_6mth %>%
  dplyr::filter(era=="COVIDSurg data collection period",
                (postOp_mortality_6mth=="Alive within 6 months post-operation"|
                   postOp_mortality_6mth=="Dead within 6 months post-operation")) %>%
  dplyr::arrange(postOp_mortality_6mth) %>%
  dplyr::ungroup() %>% dplyr::select(-c("era", "postOp_mortality_6mth"))
# ## ## Get percentages per intervals and overall.
CSP_pct_postOp_mortality_6mth <- 
  table1_postOp_mortality_6mth %>%
  dplyr::filter(era=="COVIDSurg data collection period",
                (postOp_mortality_6mth=="Alive within 6 months post-operation"|
                   postOp_mortality_6mth=="Dead within 6 months post-operation")) %>%
  select(-c("era", "postOp_mortality_6mth")) %>%
  colSums() %>% sweep(CSP_n_postOp_mortality_6mth, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0)) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
CSP_postOp_mortality_6mth <-
  matrix(0,
         nrow = length(rownames(CSP_n_postOp_mortality_6mth)),
         ncol = length(colnames(CSP_n_postOp_mortality_6mth))*2) %>%
  as.data.frame()
CSP_postOp_mortality_6mth[,seq(1,length(colnames(CSP_postOp_mortality_6mth)),2)] <- CSP_n_postOp_mortality_6mth
CSP_postOp_mortality_6mth[,seq(2,length(colnames(CSP_postOp_mortality_6mth)),2)] <- CSP_pct_postOp_mortality_6mth
colnames(CSP_postOp_mortality_6mth)[seq(1,length(colnames(CSP_postOp_mortality_6mth)),2)] <- colnames(CSP_n_postOp_mortality_6mth)
colnames(CSP_postOp_mortality_6mth)[seq(2,length(colnames(CSP_postOp_mortality_6mth)),2)] <- colnames(CSP_pct_postOp_mortality_6mth)
CSP_postOp_mortality_6mth <-
  table1_postOp_mortality_6mth %>%
  dplyr::filter(era=="COVIDSurg data collection period",
                (postOp_mortality_6mth=="Alive within 6 months post-operation"|
                   postOp_mortality_6mth=="Dead within 6 months post-operation")) %>%
  dplyr::arrange(postOp_mortality_6mth) %>%
  dplyr::select("postOp_mortality_6mth") %>%
  dplyr::bind_cols(CSP_postOp_mortality_6mth)
# ## ## Clean up.
rm(CSP_n_postOp_mortality_6mth, CSP_pct_postOp_mortality_6mth)

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

# ## COVIDSurg data collection period.
# ## ## Get counts per intervals and overall.
CSP_n_postOp_mortality_12mth <- 
  table1_postOp_mortality_12mth %>%
  dplyr::filter(era=="COVIDSurg data collection period",
                (postOp_mortality_12mth=="Alive within 12 months post-operation"|
                   postOp_mortality_12mth=="Dead within 12 months post-operation")) %>%
  dplyr::arrange(postOp_mortality_12mth) %>%
  dplyr::ungroup() %>% dplyr::select(-c("era", "postOp_mortality_12mth"))
# ## ## Get percentages per intervals and overall.
CSP_pct_postOp_mortality_12mth <- 
  table1_postOp_mortality_12mth %>%
  dplyr::filter(era=="COVIDSurg data collection period",
                (postOp_mortality_12mth=="Alive within 12 months post-operation"|
                   postOp_mortality_12mth=="Dead within 12 months post-operation")) %>%
  select(-c("era", "postOp_mortality_12mth")) %>%
  colSums() %>% sweep(CSP_n_postOp_mortality_12mth, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0)) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
CSP_postOp_mortality_12mth <-
  matrix(0,
         nrow = length(rownames(CSP_n_postOp_mortality_12mth)),
         ncol = length(colnames(CSP_n_postOp_mortality_12mth))*2) %>%
  as.data.frame()
CSP_postOp_mortality_12mth[,seq(1,length(colnames(CSP_postOp_mortality_12mth)),2)] <- CSP_n_postOp_mortality_12mth
CSP_postOp_mortality_12mth[,seq(2,length(colnames(CSP_postOp_mortality_12mth)),2)] <- CSP_pct_postOp_mortality_12mth
colnames(CSP_postOp_mortality_12mth)[seq(1,length(colnames(CSP_postOp_mortality_12mth)),2)] <- colnames(CSP_n_postOp_mortality_12mth)
colnames(CSP_postOp_mortality_12mth)[seq(2,length(colnames(CSP_postOp_mortality_12mth)),2)] <- colnames(CSP_pct_postOp_mortality_12mth)
CSP_postOp_mortality_12mth <-
  table1_postOp_mortality_12mth %>%
  dplyr::filter(era=="COVIDSurg data collection period",
                (postOp_mortality_12mth=="Alive within 12 months post-operation"|
                   postOp_mortality_12mth=="Dead within 12 months post-operation")) %>%
  dplyr::arrange(postOp_mortality_12mth) %>%
  dplyr::select("postOp_mortality_12mth") %>%
  dplyr::bind_cols(CSP_postOp_mortality_12mth)
# ## ## Clean up.
rm(CSP_n_postOp_mortality_12mth, CSP_pct_postOp_mortality_12mth)

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

# 30-day post-operative cerebrovascular_complication. ----
# ## Pre-pandemic
# ## ## Get counts per intervals and overall.
PP_n_postOp_cerebrovascular_complication_30day <- 
  table1_postOp_cerebrovascular_complication_30day %>%
  dplyr::filter(era=="Pre-pandemic",
                (postOp_cerebrovascular_complication_30day=="Cerebrovascular complication within 30 days post-operation"|
                   postOp_cerebrovascular_complication_30day=="No cerebrovascular complication within 30 days post-operation")) %>%
  dplyr::arrange(postOp_cerebrovascular_complication_30day) %>%
  dplyr::ungroup() %>% dplyr::select("n_all_intervals")
# ## ## Get percentages per intervals and overall.
PP_pct_postOp_cerebrovascular_complication_30day <-
  (PP_n_postOp_cerebrovascular_complication_30day / sum(PP_n_postOp_cerebrovascular_complication_30day)) %>%
  "*"(100) %>% tidyr::replace_na(list("n_all_intervals" = 0)) %>%
  `colnames<-`(c("pct_all_intervals"))
# ## ## Bind the counts and percentages.
PP_postOp_cerebrovascular_complication_30day <-
  table1_postOp_cerebrovascular_complication_30day %>%
  dplyr::filter(era=="Pre-pandemic",
                (postOp_cerebrovascular_complication_30day=="Cerebrovascular complication within 30 days post-operation"|
                   postOp_cerebrovascular_complication_30day=="No cerebrovascular complication within 30 days post-operation")) %>%
  dplyr::arrange(postOp_cerebrovascular_complication_30day) %>%
  dplyr::select(postOp_cerebrovascular_complication_30day) %>%
  dplyr::bind_cols(PP_n_postOp_cerebrovascular_complication_30day, PP_pct_postOp_cerebrovascular_complication_30day)
# ## ## Clean up.
rm(PP_n_postOp_cerebrovascular_complication_30day, PP_pct_postOp_cerebrovascular_complication_30day)

# ## Pandemic no vaccine.
# ## ## Get counts per intervals and overall.
PNV_n_postOp_cerebrovascular_complication_30day <- 
  table1_postOp_cerebrovascular_complication_30day %>%
  dplyr::filter(era=="Pandemic no vaccine",
                (postOp_cerebrovascular_complication_30day=="Cerebrovascular complication within 30 days post-operation"|
                   postOp_cerebrovascular_complication_30day=="No cerebrovascular complication within 30 days post-operation")) %>%
  dplyr::arrange(postOp_cerebrovascular_complication_30day) %>%
  dplyr::ungroup() %>% dplyr::select(-c("era", "postOp_cerebrovascular_complication_30day"))
# ## ## Get percentages per intervals and overall.
PNV_pct_postOp_cerebrovascular_complication_30day <- 
  table1_postOp_cerebrovascular_complication_30day %>%
  dplyr::filter(era=="Pandemic no vaccine",
                (postOp_cerebrovascular_complication_30day=="Cerebrovascular complication within 30 days post-operation"|
                   postOp_cerebrovascular_complication_30day=="No cerebrovascular complication within 30 days post-operation")) %>%
  select(-c("era", "postOp_cerebrovascular_complication_30day")) %>%
  colSums() %>% sweep(PNV_n_postOp_cerebrovascular_complication_30day, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0)) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
PNV_postOp_cerebrovascular_complication_30day <-
  matrix(0,
         nrow = length(rownames(PNV_n_postOp_cerebrovascular_complication_30day)),
         ncol = length(colnames(PNV_n_postOp_cerebrovascular_complication_30day))*2) %>%
  as.data.frame()
PNV_postOp_cerebrovascular_complication_30day[,seq(1,length(colnames(PNV_postOp_cerebrovascular_complication_30day)),2)] <- PNV_n_postOp_cerebrovascular_complication_30day
PNV_postOp_cerebrovascular_complication_30day[,seq(2,length(colnames(PNV_postOp_cerebrovascular_complication_30day)),2)] <- PNV_pct_postOp_cerebrovascular_complication_30day
colnames(PNV_postOp_cerebrovascular_complication_30day)[seq(1,length(colnames(PNV_postOp_cerebrovascular_complication_30day)),2)] <- colnames(PNV_n_postOp_cerebrovascular_complication_30day)
colnames(PNV_postOp_cerebrovascular_complication_30day)[seq(2,length(colnames(PNV_postOp_cerebrovascular_complication_30day)),2)] <- colnames(PNV_pct_postOp_cerebrovascular_complication_30day)
PNV_postOp_cerebrovascular_complication_30day <-
  table1_postOp_cerebrovascular_complication_30day %>%
  dplyr::filter(era=="Pandemic no vaccine",
                (postOp_cerebrovascular_complication_30day=="Cerebrovascular complication within 30 days post-operation"|
                   postOp_cerebrovascular_complication_30day=="No cerebrovascular complication within 30 days post-operation")) %>%
  dplyr::arrange(postOp_cerebrovascular_complication_30day) %>%
  dplyr::select(postOp_cerebrovascular_complication_30day) %>%
  dplyr::bind_cols(PNV_postOp_cerebrovascular_complication_30day)
# ## ## Clean up.
rm(PNV_n_postOp_cerebrovascular_complication_30day, PNV_pct_postOp_cerebrovascular_complication_30day)

# ## COVIDSurg data collection period.
# ## ## Get counts per intervals and overall.
CSP_n_postOp_cerebrovascular_complication_30day <- 
  table1_postOp_cerebrovascular_complication_30day %>%
  dplyr::filter(era=="COVIDSurg data collection period",
                (postOp_cerebrovascular_complication_30day=="Cerebrovascular complication within 30 days post-operation"|
                   postOp_cerebrovascular_complication_30day=="No cerebrovascular complication within 30 days post-operation")) %>%
  dplyr::arrange(postOp_cerebrovascular_complication_30day) %>%
  dplyr::ungroup() %>% dplyr::select(-c("era", "postOp_cerebrovascular_complication_30day"))
# ## ## Get percentages per intervals and overall.
CSP_pct_postOp_cerebrovascular_complication_30day <- 
  table1_postOp_cerebrovascular_complication_30day %>%
  dplyr::filter(era=="COVIDSurg data collection period",
                (postOp_cerebrovascular_complication_30day=="Cerebrovascular complication within 30 days post-operation"|
                   postOp_cerebrovascular_complication_30day=="No cerebrovascular complication within 30 days post-operation")) %>%
  select(-c("era", "postOp_cerebrovascular_complication_30day")) %>%
  colSums() %>% sweep(CSP_n_postOp_cerebrovascular_complication_30day, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0)) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
CSP_postOp_cerebrovascular_complication_30day <-
  matrix(0,
         nrow = length(rownames(CSP_n_postOp_cerebrovascular_complication_30day)),
         ncol = length(colnames(CSP_n_postOp_cerebrovascular_complication_30day))*2) %>%
  as.data.frame()
CSP_postOp_cerebrovascular_complication_30day[,seq(1,length(colnames(CSP_postOp_cerebrovascular_complication_30day)),2)] <- CSP_n_postOp_cerebrovascular_complication_30day
CSP_postOp_cerebrovascular_complication_30day[,seq(2,length(colnames(CSP_postOp_cerebrovascular_complication_30day)),2)] <- CSP_pct_postOp_cerebrovascular_complication_30day
colnames(CSP_postOp_cerebrovascular_complication_30day)[seq(1,length(colnames(CSP_postOp_cerebrovascular_complication_30day)),2)] <- colnames(CSP_n_postOp_cerebrovascular_complication_30day)
colnames(CSP_postOp_cerebrovascular_complication_30day)[seq(2,length(colnames(CSP_postOp_cerebrovascular_complication_30day)),2)] <- colnames(CSP_pct_postOp_cerebrovascular_complication_30day)
CSP_postOp_cerebrovascular_complication_30day <-
  table1_postOp_cerebrovascular_complication_30day %>%
  dplyr::filter(era=="COVIDSurg data collection period",
                (postOp_cerebrovascular_complication_30day=="Cerebrovascular complication within 30 days post-operation"|
                   postOp_cerebrovascular_complication_30day=="No cerebrovascular complication within 30 days post-operation")) %>%
  dplyr::arrange(postOp_cerebrovascular_complication_30day) %>%
  dplyr::select(postOp_cerebrovascular_complication_30day) %>%
  dplyr::bind_cols(CSP_postOp_cerebrovascular_complication_30day)
# ## ## Clean up.
rm(CSP_n_postOp_cerebrovascular_complication_30day, CSP_pct_postOp_cerebrovascular_complication_30day)

# ## Pandemic with vaccine.
# ## ## Get counts per intervals and overall.
PWV_n_postOp_cerebrovascular_complication_30day <- 
  table1_postOp_cerebrovascular_complication_30day %>%
  dplyr::filter(era=="Pandemic with vaccine",
                (postOp_cerebrovascular_complication_30day=="Cerebrovascular complication within 30 days post-operation"|
                   postOp_cerebrovascular_complication_30day=="No cerebrovascular complication within 30 days post-operation")) %>%
  dplyr::arrange(postOp_cerebrovascular_complication_30day) %>%
  dplyr::ungroup() %>% dplyr::select(-c("era", "postOp_cerebrovascular_complication_30day"))
# ## ## Get percentages per intervals and overall.
PWV_pct_postOp_cerebrovascular_complication_30day <- 
  table1_postOp_cerebrovascular_complication_30day %>%
  dplyr::filter(era=="Pandemic with vaccine",
                (postOp_cerebrovascular_complication_30day=="Cerebrovascular complication within 30 days post-operation"|
                   postOp_cerebrovascular_complication_30day=="No cerebrovascular complication within 30 days post-operation")) %>%
  select(-c("era", "postOp_cerebrovascular_complication_30day")) %>%
  colSums() %>% sweep(PWV_n_postOp_cerebrovascular_complication_30day, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0
  )) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
PWV_postOp_cerebrovascular_complication_30day <-
  matrix(0,
         length(rownames(PWV_n_postOp_cerebrovascular_complication_30day)),
         ncol = length(colnames(PWV_n_postOp_cerebrovascular_complication_30day))*2) %>%
  as.data.frame()
PWV_postOp_cerebrovascular_complication_30day[,seq(1,length(colnames(PWV_postOp_cerebrovascular_complication_30day)),2)] <- PWV_n_postOp_cerebrovascular_complication_30day
PWV_postOp_cerebrovascular_complication_30day[,seq(2,length(colnames(PWV_postOp_cerebrovascular_complication_30day)),2)] <- PWV_pct_postOp_cerebrovascular_complication_30day
colnames(PWV_postOp_cerebrovascular_complication_30day)[seq(1,length(colnames(PWV_postOp_cerebrovascular_complication_30day)),2)] <- colnames(PWV_n_postOp_cerebrovascular_complication_30day)
colnames(PWV_postOp_cerebrovascular_complication_30day)[seq(2,length(colnames(PWV_postOp_cerebrovascular_complication_30day)),2)] <- colnames(PWV_pct_postOp_cerebrovascular_complication_30day)
PWV_postOp_cerebrovascular_complication_30day <-
  table1_postOp_cerebrovascular_complication_30day %>% 
  dplyr::filter(era=="Pandemic with vaccine",
                (postOp_cerebrovascular_complication_30day=="Cerebrovascular complication within 30 days post-operation"|
                   postOp_cerebrovascular_complication_30day=="No cerebrovascular complication within 30 days post-operation")) %>%
  dplyr::arrange(postOp_cerebrovascular_complication_30day) %>%
  dplyr::select(postOp_cerebrovascular_complication_30day) %>%
  dplyr::bind_cols(PWV_postOp_cerebrovascular_complication_30day)
# ## ## Clean up
rm(PWV_n_postOp_cerebrovascular_complication_30day, PWV_pct_postOp_cerebrovascular_complication_30day)
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

# ## COVIDSurg data collection period.
# ## ## Get counts per intervals and overall.
CSP_n_chronic_cardiac_disease <- 
  table1_chronic_cardiac_disease %>%
  dplyr::filter(era=="COVIDSurg data collection period",
                (chronic_cardiac_disease=="Yes"|
                   chronic_cardiac_disease=="No")) %>%
  dplyr::arrange(chronic_cardiac_disease) %>%
  dplyr::ungroup() %>% dplyr::select(-c("era", "chronic_cardiac_disease"))
# ## ## Get percentages per intervals and overall.
CSP_pct_chronic_cardiac_disease <- 
  table1_chronic_cardiac_disease %>%
  dplyr::filter(era=="COVIDSurg data collection period",
                (chronic_cardiac_disease=="Yes"|
                   chronic_cardiac_disease=="No")) %>%
  select(-c("era", "chronic_cardiac_disease")) %>%
  colSums() %>% sweep(CSP_n_chronic_cardiac_disease, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0)) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
CSP_chronic_cardiac_disease <-
  matrix(0,
         nrow = length(rownames(CSP_n_chronic_cardiac_disease)),
         ncol = length(colnames(CSP_n_chronic_cardiac_disease))*2) %>%
  as.data.frame()
CSP_chronic_cardiac_disease[,seq(1,length(colnames(CSP_chronic_cardiac_disease)),2)] <- CSP_n_chronic_cardiac_disease
CSP_chronic_cardiac_disease[,seq(2,length(colnames(CSP_chronic_cardiac_disease)),2)] <- CSP_pct_chronic_cardiac_disease
colnames(CSP_chronic_cardiac_disease)[seq(1,length(colnames(CSP_chronic_cardiac_disease)),2)] <- colnames(CSP_n_chronic_cardiac_disease)
colnames(CSP_chronic_cardiac_disease)[seq(2,length(colnames(CSP_chronic_cardiac_disease)),2)] <- colnames(CSP_pct_chronic_cardiac_disease)
CSP_chronic_cardiac_disease <-
  table1_chronic_cardiac_disease %>%
  dplyr::filter(era=="COVIDSurg data collection period",
                (chronic_cardiac_disease=="Yes"|
                   chronic_cardiac_disease=="No")) %>%
  dplyr::arrange(chronic_cardiac_disease) %>%
  dplyr::select("chronic_cardiac_disease") %>%
  dplyr::bind_cols(CSP_chronic_cardiac_disease)
# ## ## Clean up.
rm(CSP_n_chronic_cardiac_disease, CSP_pct_chronic_cardiac_disease)

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

# ## COVIDSurg data collection period.
# ## ## Get counts per intervals and overall.
CSP_n_diabetes <- 
  table1_diabetes %>%
  dplyr::filter(era=="COVIDSurg data collection period",
                (diabetes=="Yes"|
                   diabetes=="No")) %>%
  dplyr::arrange(diabetes) %>%
  dplyr::ungroup() %>% dplyr::select(-c("era", "diabetes"))
# ## ## Get percentages per intervals and overall.
CSP_pct_diabetes <- 
  table1_diabetes %>%
  dplyr::filter(era=="COVIDSurg data collection period",
                (diabetes=="Yes"|
                   diabetes=="No")) %>%
  select(-c("era", "diabetes")) %>%
  colSums() %>% sweep(CSP_n_diabetes, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0)) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
CSP_diabetes <-
  matrix(0,
         nrow = length(rownames(CSP_n_diabetes)),
         ncol = length(colnames(CSP_n_diabetes))*2) %>%
  as.data.frame()
CSP_diabetes[,seq(1,length(colnames(CSP_diabetes)),2)] <- CSP_n_diabetes
CSP_diabetes[,seq(2,length(colnames(CSP_diabetes)),2)] <- CSP_pct_diabetes
colnames(CSP_diabetes)[seq(1,length(colnames(CSP_diabetes)),2)] <- colnames(CSP_n_diabetes)
colnames(CSP_diabetes)[seq(2,length(colnames(CSP_diabetes)),2)] <- colnames(CSP_pct_diabetes)
CSP_diabetes <-
  table1_diabetes %>%
  dplyr::filter(era=="COVIDSurg data collection period",
                (diabetes=="Yes"|
                   diabetes=="No")) %>%
  dplyr::arrange(diabetes) %>%
  dplyr::select("diabetes") %>%
  dplyr::bind_cols(CSP_diabetes)
# ## ## Clean up.
rm(CSP_n_diabetes, CSP_pct_diabetes)

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

# ## COVIDSurg data collection period.
# ## ## Get counts per intervals and overall.
CSP_n_chronic_respiratory_disease <- 
  table1_chronic_respiratory_disease %>%
  dplyr::filter(era=="COVIDSurg data collection period",
                (chronic_respiratory_disease=="Yes"|
                   chronic_respiratory_disease=="No")) %>%
  dplyr::arrange(chronic_respiratory_disease) %>%
  dplyr::ungroup() %>% dplyr::select(-c("era", "chronic_respiratory_disease"))
# ## ## Get percentages per intervals and overall.
CSP_pct_chronic_respiratory_disease <- 
  table1_chronic_respiratory_disease %>%
  dplyr::filter(era=="COVIDSurg data collection period",
                (chronic_respiratory_disease=="Yes"|
                   chronic_respiratory_disease=="No")) %>%
  select(-c("era", "chronic_respiratory_disease")) %>%
  colSums() %>% sweep(CSP_n_chronic_respiratory_disease, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0)) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## Interlace counts and percentages.
CSP_chronic_respiratory_disease <-
  matrix(0,
         nrow = length(rownames(CSP_n_chronic_respiratory_disease)),
         ncol = length(colnames(CSP_n_chronic_respiratory_disease))*2) %>%
  as.data.frame()
CSP_chronic_respiratory_disease[,seq(1,length(colnames(CSP_chronic_respiratory_disease)),2)] <- CSP_n_chronic_respiratory_disease
CSP_chronic_respiratory_disease[,seq(2,length(colnames(CSP_chronic_respiratory_disease)),2)] <- CSP_pct_chronic_respiratory_disease
colnames(CSP_chronic_respiratory_disease)[seq(1,length(colnames(CSP_chronic_respiratory_disease)),2)] <- colnames(CSP_n_chronic_respiratory_disease)
colnames(CSP_chronic_respiratory_disease)[seq(2,length(colnames(CSP_chronic_respiratory_disease)),2)] <- colnames(CSP_pct_chronic_respiratory_disease)
CSP_chronic_respiratory_disease <-
  table1_chronic_respiratory_disease %>%
  dplyr::filter(era=="COVIDSurg data collection period",
                (chronic_respiratory_disease=="Yes"|
                   chronic_respiratory_disease=="No")) %>%
  dplyr::arrange(chronic_respiratory_disease) %>%
  dplyr::select("chronic_respiratory_disease") %>%
  dplyr::bind_cols(CSP_chronic_respiratory_disease)
# ## ## Clean up.
rm(CSP_n_chronic_respiratory_disease, CSP_pct_chronic_respiratory_disease)

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
intervals_less_than_7wks <- c("n_infection_none", "n_infection_0to2wk",
                              "n_infection_3to4wk", "n_infection_5to6wk")
# Pre-pandemic tables.
# ## Stratification table.
tbl_PP_strata <-
  rbind(
  as.matrix(PP_ageGroup),
  as.matrix(PP_Sex),
  as.matrix(PP_admission_method),
  as.matrix(PP_chronic_cardiac_disease),
  as.matrix(PP_diabetes),
  as.matrix(PP_chronic_respiratory_disease)) %>%
    as.data.frame()  %>%
    tibble::add_column(., c(
      rep("Age group",5),
      rep("Sex",2),
      rep("Admission method",2),
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
    as.matrix(PP_postOp_mortality_12mth),
    as.matrix(PP_postOp_cerebrovascular_complication_30day)) %>%
  as.data.frame()  %>%
  tibble::add_column(., c(
    rep("30-day post-operative mortality",2),
    rep("90-day post-operative mortality",2),
    rep("6-month post-operative mortality",2),
    rep("12-month post-operative mortality",2),
    rep("30-day post-operative cerebrovascular complication",2)),
    .before = "postOp_mortality_30day"
  ) %>%
  `colnames<-`(c("variable", "strata", "n", "pct"))
# Save table.
write.csv(
  x = tbl_PP_outcome,
  file = here::here("output",paste0("table1_PP_outcome",sensitivity_cohort,".csv"))
)

# Pandemic no vaccine tables.
# ## Stratification table.
tbl_PNV_strata <-
  rbind(
    as.matrix(PNV_ageGroup),
    as.matrix(PNV_Sex),
    as.matrix(PNV_admission_method),
    as.matrix(PNV_chronic_cardiac_disease),
    as.matrix(PNV_diabetes),
    as.matrix(PNV_chronic_respiratory_disease)) %>%
  as.data.frame()  %>%
  tibble::add_column(., c(
    rep("Age group",5),
    rep("Sex",2),
    rep("Admission method",2),
    rep("Chronic cardiac disease",2),
    rep("Diabetes",2),
    rep("Chronic respiratory disease",2)),
    .before = "age_group_surgery"
  ) %>%
  `colnames<-`(c("variable", "strata", colnames(.)[3:ncol(.)]))
# Save table.
write.csv(
  x = tbl_PNV_strata,
  file = here::here("output",paste0("table1_PNV_strata",sensitivity_cohort,".csv"))
)
# Make table with only 7week threshold.
tbl_PNV_strata_7wkThreshold <- 
  tbl_PNV_strata %>% dplyr::select(intervals_less_than_7wks) %>% data.matrix() %>%
  rowSums() %>% as.data.frame() %>% 
  dplyr::mutate("pct_infection_<7wks" = (./ sum(.))*100) %>%
  tibble::add_column(tbl_PNV_strata %>% dplyr::select(c(variable, strata)),
                     .before = ".") %>%
  tibble::add_column(tbl_PNV_strata %>%
                       dplyr::select(c(n_infection_7wk, pct_infection_7wk))) %>%
  `colnames<-`(c("variable", "strata", "n_infection_<7wks",
                 "pct_infection_<7wks","n_infection_>7wks","pct_infection_>7wks")) %>%
  tidyr::replace_na(list("pct_infection_<7wks" = 0))
# Save table.
write.csv(
  x = tbl_PNV_strata_7wkThreshold,
  file = here::here("output",paste0("table1_PNV_strata",sensitivity_cohort,"_7wkThreshold.csv"))
)
# ## Outcomes table.
tbl_PNV_outcome <-
  rbind(
    as.matrix(PNV_postOp_mortality_30day),
    as.matrix(PNV_postOp_mortality_90day),
    as.matrix(PNV_postOp_mortality_6mth),
    as.matrix(PNV_postOp_mortality_12mth),
    as.matrix(PNV_postOp_cerebrovascular_complication_30day)) %>%
  as.data.frame()  %>%
  tibble::add_column(., c(
    rep("30-day post-operative mortality",2),
    rep("90-day post-operative mortality",2),
    rep("6-month post-operative mortality",2),
    rep("12-month post-operative mortality",2),
    rep("30-day post-operative cerebrovascular complication",2)),
    .before = "postOp_mortality_30day"
  ) %>%
  `colnames<-`(c("variable", "strata", colnames(.)[3:ncol(.)]))
# Save table.
write.csv(
  x = tbl_PNV_outcome,
  file = here::here("output",paste0("table1_PNV_outcome",sensitivity_cohort,".csv"))
)
# Make table with only 7week threshold.
tbl_PNV_outcome_7wkThreshold <- 
  tbl_PNV_outcome %>% dplyr::select(intervals_less_than_7wks) %>% data.matrix() %>%
  rowSums() %>% as.data.frame() %>% 
  dplyr::mutate("pct_infection_<7wks" = (./ sum(.))*100) %>%
  tibble::add_column(tbl_PNV_outcome %>% dplyr::select(c(variable, strata)),
                     .before = ".") %>%
  tibble::add_column(tbl_PNV_outcome %>%
                       dplyr::select(c(n_infection_7wk, pct_infection_7wk))) %>%
  `colnames<-`(c("variable", "strata", "n_infection_<7wks",
                 "pct_infection_<7wks","n_infection_>7wks","pct_infection_>7wks")) %>%
  tidyr::replace_na(list("pct_infection_<7wks" = 0))
# Save table.
write.csv(
  x = tbl_PNV_outcome_7wkThreshold,
  file = here::here("output",paste0("table1_PNV_outcome",sensitivity_cohort,"_7wkThreshold.csv"))
)

# COVIDSurg data collection period tables.
# ## Stratification table.
tbl_CSP_strata <-
  rbind(
    as.matrix(CSP_ageGroup),
    as.matrix(CSP_Sex),
    as.matrix(CSP_admission_method),
    as.matrix(CSP_chronic_cardiac_disease),
    as.matrix(CSP_diabetes),
    as.matrix(CSP_chronic_respiratory_disease)) %>%
  as.data.frame()  %>%
  tibble::add_column(., c(
    rep("Age group",5),
    rep("Sex",2),
    rep("Admission method",2),
    rep("Chronic cardiac disease",2),
    rep("Diabetes",2),
    rep("Chronic respiratory disease",2)),
    .before = "age_group_surgery"
  ) %>%
  `colnames<-`(c("variable", "strata", colnames(.)[3:ncol(.)]))
# Save table.
write.csv(
  x = tbl_CSP_strata,
  file = here::here("output",paste0("table1_CSP_strata",sensitivity_cohort,".csv"))
)
# Make table with only 7week threshold.
tbl_CSP_strata_7wkThreshold <- 
  tbl_CSP_strata %>% dplyr::select(intervals_less_than_7wks) %>% data.matrix() %>%
  rowSums() %>% as.data.frame() %>% 
  dplyr::mutate("pct_infection_<7wks" = (./ sum(.))*100) %>%
  tibble::add_column(tbl_CSP_strata %>% dplyr::select(c(variable, strata)),
                     .before = ".") %>%
  tibble::add_column(tbl_CSP_strata %>%
                       dplyr::select(c(n_infection_7wk, pct_infection_7wk))) %>%
  `colnames<-`(c("variable", "strata", "n_infection_<7wks",
                 "pct_infection_<7wks","n_infection_>7wks","pct_infection_>7wks")) %>%
  tidyr::replace_na(list("pct_infection_<7wks" = 0))
# Save table.
write.csv(
  x = tbl_CSP_strata_7wkThreshold,
  file = here::here("output",paste0("table1_CSP_strata",sensitivity_cohort,"_7wkThreshold.csv"))
)
# ## Outcomes table.
tbl_CSP_outcome <-
  rbind(
    as.matrix(CSP_postOp_mortality_30day),
    as.matrix(CSP_postOp_mortality_90day),
    as.matrix(CSP_postOp_mortality_6mth),
    as.matrix(CSP_postOp_mortality_12mth),
    as.matrix(CSP_postOp_cerebrovascular_complication_30day)) %>%
  as.data.frame()  %>%
  tibble::add_column(., c(
    rep("30-day post-operative mortality",2),
    rep("90-day post-operative mortality",2),
    rep("6-month post-operative mortality",2),
    rep("12-month post-operative mortality",2),
    rep("30-day post-operative cerebrovascular complication",2)),
    .before = "postOp_mortality_30day"
  ) %>%
  `colnames<-`(c("variable", "strata", colnames(.)[3:ncol(.)]))
# Save table.
write.csv(
  x = tbl_CSP_outcome,
  file = here::here("output",paste0("table1_CSP_outcome",sensitivity_cohort,".csv"))
)
# Make table with only 7week threshold.
tbl_CSP_outcome_7wkThreshold <- 
  tbl_CSP_outcome %>% dplyr::select(intervals_less_than_7wks) %>% data.matrix() %>%
  rowSums() %>% as.data.frame() %>% 
  dplyr::mutate("pct_infection_<7wks" = (./ sum(.))*100) %>%
  tibble::add_column(tbl_CSP_outcome %>% dplyr::select(c(variable, strata)),
                     .before = ".") %>%
  tibble::add_column(tbl_CSP_outcome %>%
                       dplyr::select(c(n_infection_7wk, pct_infection_7wk))) %>%
  `colnames<-`(c("variable", "strata", "n_infection_<7wks",
                 "pct_infection_<7wks","n_infection_>7wks","pct_infection_>7wks")) %>%
  tidyr::replace_na(list("pct_infection_<7wks" = 0))
# Save table.
write.csv(
  x = tbl_CSP_outcome_7wkThreshold,
  file = here::here("output",paste0("table1_CSP_outcome",sensitivity_cohort,"_7wkThreshold.csv"))
)

# Pandemic with vaccine tables.
# ## Stratification table.
tbl_PWV_strata <-
  rbind(
    as.matrix(PWV_ageGroup),
    as.matrix(PWV_Sex),
    as.matrix(PWV_admission_method),
    as.matrix(PWV_chronic_cardiac_disease),
    as.matrix(PWV_diabetes),
    as.matrix(PWV_chronic_respiratory_disease)) %>%
  as.data.frame()  %>%
  tibble::add_column(., c(
    rep("Age group",5),
    rep("Sex",2),
    rep("Admission method",2),
    rep("Chronic cardiac disease",2),
    rep("Diabetes",2),
    rep("Chronic respiratory disease",2)),
    .before = "age_group_surgery"
  ) %>%
  `colnames<-`(c("variable", "strata", colnames(.)[3:ncol(.)]))
# Save table.
write.csv(
  x = tbl_PWV_strata,
  file = here::here("output",paste0("table1_PWV_strata",sensitivity_cohort,".csv"))
)
# Make table with only 7week threshold.
tbl_PWV_strata_7wkThreshold <- 
  tbl_PWV_strata %>% dplyr::select(intervals_less_than_7wks) %>% data.matrix() %>%
  rowSums() %>% as.data.frame() %>% 
  dplyr::mutate("pct_infection_<7wks" = (./ sum(.))*100) %>%
  tibble::add_column(tbl_PWV_strata %>% dplyr::select(c(variable, strata)),
                     .before = ".") %>%
  tibble::add_column(tbl_PWV_strata %>%
                       dplyr::select(c(n_infection_7wk, pct_infection_7wk))) %>%
  `colnames<-`(c("variable", "strata", "n_infection_<7wks",
                 "pct_infection_<7wks","n_infection_>7wks","pct_infection_>7wks")) %>%
  tidyr::replace_na(list("pct_infection_<7wks" = 0))
# Save table.
write.csv(
  x = tbl_PWV_strata_7wkThreshold,
  file = here::here("output",paste0("table1_PWV_strata",sensitivity_cohort,"_7wkThreshold.csv"))
)
# ## Outcomes table.
tbl_PWV_outcome <-
  rbind(
    as.matrix(PWV_postOp_mortality_30day),
    as.matrix(PWV_postOp_mortality_90day),
    as.matrix(PWV_postOp_mortality_6mth),
    as.matrix(PWV_postOp_mortality_12mth),
    as.matrix(PWV_postOp_cerebrovascular_complication_30day)) %>%
  as.data.frame()  %>%
  tibble::add_column(., c(
    rep("30-day post-operative mortality",2),
    rep("90-day post-operative mortality",2),
    rep("6-month post-operative mortality",2),
    rep("12-month post-operative mortality",2),
    rep("30-day post-operative cerebrovascular complication",2)),
    .before = "postOp_mortality_30day"
  ) %>%
  `colnames<-`(c("variable", "strata", colnames(.)[3:ncol(.)]))
# Save table.
write.csv(
  x = tbl_PWV_outcome,
  file = here::here("output",paste0("table1_PWV_outcome",sensitivity_cohort,".csv"))
)
# Make table with only 7week threshold.
tbl_PWV_outcome_7wkThreshold <- 
tbl_PWV_outcome %>% dplyr::select(intervals_less_than_7wks) %>% data.matrix() %>%
  rowSums() %>% as.data.frame() %>% 
  dplyr::mutate("pct_infection_<7wks" = (./ sum(.))*100) %>%
  tibble::add_column(tbl_PWV_outcome %>% dplyr::select(c(variable, strata)),
                     .before = ".") %>%
  tibble::add_column(tbl_PWV_outcome %>%
                       dplyr::select(c(n_infection_7wk, pct_infection_7wk))) %>%
  `colnames<-`(c("variable", "strata", "n_infection_<7wks",
                 "pct_infection_<7wks","n_infection_>7wks","pct_infection_>7wks")) %>%
  tidyr::replace_na(list("pct_infection_<7wks" = 0))
# Save table.
write.csv(
  x = tbl_PWV_outcome_7wkThreshold,
  file = here::here("output",paste0("table1_PWV_outcome",sensitivity_cohort,"_7wkThreshold.csv"))
)


  

# ----

