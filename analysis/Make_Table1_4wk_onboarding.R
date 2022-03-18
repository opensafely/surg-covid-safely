# Make_Table1.R
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
# webshot::install_phantomjs(force = T)

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
# ##    4. ">=7 weeks record of pre-operative SARS-CoV-2 infection"
# ##    5. "Error: Test result after surgery. Check study_definition."
# ##
# ## The counts are also stratified by surgery era:
# ##    1. "preCOVID sugery"
# ##    2. "postCOVID surgery" (although labelled "post", this means during, too)
# ##    3. "No surgery"
table1_totals_preOp_infection_status <- 
  myData %>% group_by(surgery_pre_or_post_COVID_UK,
                      preOperative_infection_status) %>% summarise(n = n())
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by age band:
# ##    1. 0-29
# ##    2. 30-49
# ##    3. 50-69
# ##    4. 70-79
# ##    5. 80+
table1_ageGroup <- 
  myData %>% group_by(surgery_pre_or_post_COVID_UK, age_group_surgery) %>%
  summarise(n_per_group = sum(ifelse(preOperative_infection_status!=
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
  myData %>% group_by(surgery_pre_or_post_COVID_UK,Sex) %>%
  summarise(n_per_group = sum(ifelse(preOperative_infection_status!=
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
# ##    1. "Alive within 30-day post-operation"
# ##    2. "Dead within 30-day post-operation" 
# ##    3. "Error: Surgery after death"
# ##    4. "No surgery recorded"
# ##    5. "No death recorded"
table1_postOp_mortality_30day <- 
  myData %>% group_by(surgery_pre_or_post_COVID_UK, postOp_mortality_30day) %>%
  summarise(n_per_group = sum(ifelse(preOperative_infection_status!=
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
              surgery_pre_or_post_COVID_UK = 
                c("No surgery", "preCOVID surgery", "postCOVID surgery"),
              "preOperative_infection_status" = 
                c("Error: Test result after surgery. Check study_definition.",
                  "No record of pre-operative SARS-CoV-2 infection",
                  "0-2 weeks record of pre-operative SARS-CoV-2 infection",
                  "3-4 weeks record of pre-operative SARS-CoV-2 infection",
                  ">=7 weeks record of pre-operative SARS-CoV-2 infection")) %>%
            dplyr::full_join(table1_totals_preOp_infection_status) %>%
            arrange(surgery_pre_or_post_COVID_UK) %>%
            tidyr::replace_na(list("n" = 0))
# ## table1_ageGroup.
table1_ageGroup <- 
            expand.grid(
              surgery_pre_or_post_COVID_UK = 
                c("No surgery", "preCOVID surgery", "postCOVID surgery"),
              age_group_surgery = 
                c("0-29",
                  "30-49",
                  "50-69",
                  "70-79",
                  "80+",
                  "Missing")) %>%
              dplyr::full_join(table1_ageGroup) %>%
              arrange(surgery_pre_or_post_COVID_UK) %>%
              tidyr::replace_na(list("n_per_group" = 0,
                                     "n_infection_none" = 0,
                                     "n_infection_0to2wk" = 0,
                                     "n_infection_3to4wk" = 0,
                                     "n_infection_5to6wk" = 0,
                                     "n_infection_7wk" = 0))
# ## table1_Sex.
table1_Sex <- 
            expand.grid(
              surgery_pre_or_post_COVID_UK = 
                c("No surgery", "preCOVID surgery", "postCOVID surgery"),
              Sex = 
                c("Female",
                  "Male",
                  "Missing")) %>%
            dplyr::full_join(table1_Sex) %>%
            arrange(surgery_pre_or_post_COVID_UK) %>%
            tidyr::replace_na(list("n_per_group" = 0,
                                   "n_infection_none" = 0,
                                   "n_infection_0to2wk" = 0,
                                   "n_infection_3to4wk" = 0,
                                   "n_infection_5to6wk" = 0,
                                   "n_infection_7wk" = 0))
# ## table1_postOp_mortality_30day.
table1_postOp_mortality_30day <- 
            expand.grid(
              surgery_pre_or_post_COVID_UK = 
                c("No surgery", "preCOVID surgery", "postCOVID surgery"),
              postOp_mortality_30day = 
                c("Alive within 30-day post-operation",
                  "Dead within 30-day post-operation",
                  "Error: Surgery after death",
                  "No death recorded",
                  "No surgery recorded",
                  "Missing")) %>%
            dplyr::full_join(table1_postOp_mortality_30day) %>%
            arrange(surgery_pre_or_post_COVID_UK) %>%
            tidyr::replace_na(list("n_per_group" = 0,
                                   "n_infection_none" = 0,
                                   "n_infection_0to2wk" = 0,
                                   "n_infection_3to4wk" = 0,
                                   "n_infection_5to6wk" = 0,
                                   "n_infection_7wk" = 0))
# ----

#############################################################
# Save tibbles that will inform vectors for the kable table #
#############################################################
# ----
write.csv(
  x = table1_ageGroup,
  file = paste0(here::here("output"),"/table1_ageGroup.csv")
)
write.csv(
  x = table1_Sex,
  file = paste0(here::here("output"),"/table1_Sex.csv")
)
write.csv(
  x = table1_postOp_mortality_30day,
  file = paste0(here::here("output"),"/table1_postOp_mortality_30day.csv")
)
# ----

###########################################
# Make vectors to inform the kable table. #
###########################################
# ----
# Pre-March 2020 totals.
# ## Age band.
n_preMarch2020_ageGroup <- 
          table1_ageGroup %>%
            dplyr::filter(surgery_pre_or_post_COVID_UK=="preCOVID surgery") %>%
            arrange(age_group_surgery) %>% ungroup() %>% select(n_per_group)
prop_preMarch2020_ageGroup <- n_preMarch2020_ageGroup /
                                sum(n_preMarch2020_ageGroup)
# ## Sex.
n_preMarch2020_Sex <-
          table1_Sex %>%
            dplyr::filter(surgery_pre_or_post_COVID_UK=="preCOVID surgery",
                   Sex != "Male") %>% arrange(Sex) %>% ungroup() %>%
            select(n_per_group)
prop_preMarch2020_Sex <- n_preMarch2020_Sex / sum(n_preMarch2020_Sex)
# ## 30-day post-operative mortality.
n_preMarch2020_postOp_mortality_30day <-
          table1_postOp_mortality_30day %>%
            dplyr::filter(surgery_pre_or_post_COVID_UK=="preCOVID surgery",
                   (postOp_mortality_30day=="Dead within 30-day post-operation"|
                      postOp_mortality_30day=="Missing")) %>%
            arrange(postOp_mortality_30day) %>% ungroup() %>%
            select(n_per_group)
prop_preMarch2020_postOp_mortality_30day <-
          n_preMarch2020_postOp_mortality_30day / 
            sum(n_preMarch2020_postOp_mortality_30day)
# ## Totals. `n` is count. `prop` is proportion.
n_totals_preMarch2020 <- 
                      rbind(n_preMarch2020_ageGroup,
                            n_preMarch2020_Sex,
                            n_preMarch2020_postOp_mortality_30day)
prop_totals_preMarch2020 <- 
                      rbind(prop_preMarch2020_ageGroup,
                            prop_preMarch2020_Sex,
                            prop_preMarch2020_postOp_mortality_30day)

# Post-March 2020 totals.
# ## Age band.
n_postMarch2020_ageGroup <- 
          table1_ageGroup %>%
          dplyr::filter(surgery_pre_or_post_COVID_UK=="postCOVID surgery") %>%
          arrange(age_group_surgery) %>% ungroup() %>% select(n_per_group)
prop_postMarch2020_ageGroup <-
          n_postMarch2020_ageGroup /
            sum(n_postMarch2020_ageGroup)
# ## Sex.
n_postMarch2020_Sex <-
          table1_Sex %>%
          dplyr::filter(surgery_pre_or_post_COVID_UK=="postCOVID surgery",
                 Sex != "Male") %>% arrange(Sex) %>% ungroup() %>%
          select(n_per_group)
prop_postMarch2020_Sex <- n_postMarch2020_Sex / sum(n_postMarch2020_Sex)
# ## 30-day post-operative mortality.
n_postMarch2020_postOp_mortality_30day <-
          table1_postOp_mortality_30day %>%
          dplyr::filter(surgery_pre_or_post_COVID_UK=="postCOVID surgery",
                 (postOp_mortality_30day=="Dead within 30-day post-operation"|
                    postOp_mortality_30day=="Missing")) %>%
          arrange(postOp_mortality_30day) %>% ungroup() %>%
          select(n_per_group)
prop_postMarch2020_postOp_mortality_30day <-
          n_postMarch2020_postOp_mortality_30day / 
            sum(n_postMarch2020_postOp_mortality_30day)
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
          dplyr::filter(surgery_pre_or_post_COVID_UK=="postCOVID surgery") %>%
          arrange(age_group_surgery) %>% ungroup() %>% select(n_infection_none)
prop_subtotals_infection_none_ageGroup <-
          n_subtotals_infection_none_ageGroup /
            sum(n_subtotals_infection_none_ageGroup)
# ## Sex.
n_subtotals_infection_none_Sex <-
          table1_Sex %>%
          dplyr::filter(surgery_pre_or_post_COVID_UK=="postCOVID surgery",
                 Sex != "Male") %>% arrange(Sex) %>% ungroup() %>%
          select(n_infection_none)
prop_subtotals_infection_none_Sex <-
          n_subtotals_infection_none_Sex /
            sum(n_subtotals_infection_none_Sex)
# ## 30-day post-operative mortality.
n_subtotals_infection_none_postOp_mortality_30day <-
          table1_postOp_mortality_30day %>%
          dplyr::filter(surgery_pre_or_post_COVID_UK=="postCOVID surgery",
                 (postOp_mortality_30day=="Dead within 30-day post-operation"|
                    postOp_mortality_30day=="Missing")) %>%
          arrange(postOp_mortality_30day) %>% ungroup() %>%
          select(n_infection_none)
prop_subtotals_infection_none_postOp_mortality_30day <-
          n_subtotals_infection_none_postOp_mortality_30day / 
            sum(n_subtotals_infection_none_postOp_mortality_30day)
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
          dplyr::filter(surgery_pre_or_post_COVID_UK=="postCOVID surgery") %>%
          arrange(age_group_surgery) %>% ungroup() %>% select(n_infection_0to2wk)
prop_subtotals_infection_0to2wk_ageGroup <-
          n_subtotals_infection_0to2wk_ageGroup /
            sum(n_subtotals_infection_0to2wk_ageGroup)
# ## Sex.
n_subtotals_infection_0to2wk_Sex <-
          table1_Sex %>%
          dplyr::filter(surgery_pre_or_post_COVID_UK=="postCOVID surgery",
                 Sex != "Male") %>% arrange(Sex) %>% ungroup() %>%
          select(n_infection_0to2wk)
prop_subtotals_infection_0to2wk_Sex <-
          n_subtotals_infection_0to2wk_Sex /
            sum(n_subtotals_infection_0to2wk_Sex)
# ## 30-day post-operative mortality.
n_subtotals_infection_0to2wk_postOp_mortality_30day <-
          table1_postOp_mortality_30day %>%
          dplyr::filter(surgery_pre_or_post_COVID_UK=="postCOVID surgery",
                 (postOp_mortality_30day=="Dead within 30-day post-operation"|
                    postOp_mortality_30day=="Missing")) %>%
          arrange(postOp_mortality_30day) %>% ungroup() %>%
          select(n_infection_0to2wk)
prop_subtotals_infection_0to2wk_postOp_mortality_30day <-
          n_subtotals_infection_0to2wk_postOp_mortality_30day / 
            sum(n_subtotals_infection_0to2wk_postOp_mortality_30day)
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
          dplyr::filter(surgery_pre_or_post_COVID_UK=="postCOVID surgery") %>%
          arrange(age_group_surgery) %>% ungroup() %>% select(n_infection_3to4wk)
prop_subtotals_infection_3to4wk_ageGroup <-
          n_subtotals_infection_3to4wk_ageGroup /
            sum(n_subtotals_infection_3to4wk_ageGroup)
# ## Sex.
n_subtotals_infection_3to4wk_Sex <-
          table1_Sex %>%
          dplyr::filter(surgery_pre_or_post_COVID_UK=="postCOVID surgery",
                 Sex != "Male") %>% arrange(Sex) %>% ungroup() %>%
          select(n_infection_3to4wk)
prop_subtotals_infection_3to4wk_Sex <-
          n_subtotals_infection_3to4wk_Sex /
            sum(n_subtotals_infection_3to4wk_Sex)
# ## 30-day post-operative mortality.
n_subtotals_infection_3to4wk_postOp_mortality_30day <-
          table1_postOp_mortality_30day %>%
          dplyr::filter(surgery_pre_or_post_COVID_UK=="postCOVID surgery",
                 (postOp_mortality_30day=="Dead within 30-day post-operation"|
                    postOp_mortality_30day=="Missing")) %>%
          arrange(postOp_mortality_30day) %>% ungroup() %>%
          select(n_infection_3to4wk)
prop_subtotals_infection_3to4wk_postOp_mortality_30day <-
          n_subtotals_infection_3to4wk_postOp_mortality_30day / 
            sum(n_subtotals_infection_3to4wk_postOp_mortality_30day)
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
          dplyr::filter(surgery_pre_or_post_COVID_UK=="postCOVID surgery") %>%
          arrange(age_group_surgery) %>% ungroup() %>% select(n_infection_5to6wk)
prop_subtotals_infection_5to6wk_ageGroup <-
          n_subtotals_infection_5to6wk_ageGroup /
            sum(n_subtotals_infection_5to6wk_ageGroup)
# ## Sex.
n_subtotals_infection_5to6wk_Sex <-
          table1_Sex %>%
          dplyr::filter(surgery_pre_or_post_COVID_UK=="postCOVID surgery",
                 Sex != "Male") %>% arrange(Sex) %>% ungroup() %>%
          select(n_infection_5to6wk)
prop_subtotals_infection_5to6wk_Sex <-
          n_subtotals_infection_5to6wk_Sex /
            sum(n_subtotals_infection_5to6wk_Sex)
# ## 30-day post-operative mortality.
n_subtotals_infection_5to6wk_postOp_mortality_30day <-
          table1_postOp_mortality_30day %>%
          dplyr::filter(surgery_pre_or_post_COVID_UK=="postCOVID surgery",
                 (postOp_mortality_30day=="Dead within 30-day post-operation"|
                    postOp_mortality_30day=="Missing")) %>%
          arrange(postOp_mortality_30day) %>% ungroup() %>%
          select(n_infection_5to6wk)
prop_subtotals_infection_5to6wk_postOp_mortality_30day <-
          n_subtotals_infection_5to6wk_postOp_mortality_30day / 
            sum(n_subtotals_infection_5to6wk_postOp_mortality_30day)
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
          dplyr::filter(surgery_pre_or_post_COVID_UK=="postCOVID surgery") %>%
          arrange(age_group_surgery) %>% ungroup() %>% select(n_infection_7wk)
prop_subtotals_infection_7wk_ageGroup <-
          n_subtotals_infection_7wk_ageGroup /
            sum(n_subtotals_infection_7wk_ageGroup)
# ## Sex.
n_subtotals_infection_7wk_Sex <-
          table1_Sex %>%
          dplyr::filter(surgery_pre_or_post_COVID_UK=="postCOVID surgery",
                 Sex != "Male") %>% arrange(Sex) %>% ungroup() %>%
          select(n_infection_7wk)
prop_subtotals_infection_7wk_Sex <-
          n_subtotals_infection_7wk_Sex /
            sum(n_subtotals_infection_7wk_Sex)
# ## 30-day post-operative mortality.
n_subtotals_infection_7wk_postOp_mortality_30day <-
          table1_postOp_mortality_30day %>%
          dplyr::filter(surgery_pre_or_post_COVID_UK=="postCOVID surgery",
                 (postOp_mortality_30day=="Dead within 30-day post-operation"|
                    postOp_mortality_30day=="Missing")) %>%
          arrange(postOp_mortality_30day) %>% ungroup() %>%
          select(n_infection_7wk)
prop_subtotals_infection_7wk_postOp_mortality_30day <-
          n_subtotals_infection_7wk_postOp_mortality_30day / 
            sum(n_subtotals_infection_7wk_postOp_mortality_30day)
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
          "Female", " Missing",
          # ## 30-day post-operative mortality.
          "Yes", "  Missing")
# Save data frame.
write.csv(
  x = df_4wk,
  file = paste0(here::here("output"),"/table1_4wk_onboarding.csv")
)
# Make kable table.
df_4wk %>%
  kbl(caption = paste0("<b>Table 1</b> Baseline characteristics and outcomes for ",
                       "patients undergoing surgery stratified by time from ",
                       "indication of SARS-CoV-2 infection. Values are ",
                       "counts and percentages.", collapse = ""),
      format = 'html') %>%
  kable_classic(full_width = F,
                fixed_thead = T,
                html_font="Cambria") %>%
  pack_rows(index = c("Age" = 6, "Sex" = 2,
                      "30-day post-operative mortality" = 2)) %>%
  add_header_above(c(" " = 7, "0-2 weeks" = 2, "3-4 weeks" = 2,
                     "5-6 weeks" = 2, ">=7 weeks" = 2)) %>%
  add_header_above(c(" " = 1, "Pre-March 2020, totals" = 2,
                     "Post-March 2020, totals" = 2,
                     "No indication of infection" = 2,
                     "\nIndication of infection" = 8)) %>%
  add_header_above(c(" " = 5, "Post-March 2020" = 10)) %>%
  column_spec(c(1:15), width = "5em") %>%
  row_spec(0, align = "c") #%>%
  #save_kable(file = paste0(here::here("output"),"/Table1_4wk_onboarding.png"))
# ----
