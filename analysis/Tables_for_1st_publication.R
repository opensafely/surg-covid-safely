# Tables_for_1st_publication.R
#
# The purpose of this R script is to build the tables for the 1st publication
# from this study, entilted "<TBC>".
#
# The tables are
# Table1Demogs
# - Shows the demographics in the style of Table1 from COVIDSurg publications.
# - Cohort = all surgery patients, regardless of cancer diagnosis.
# - Era = Pandemic-with-vaccines (from 12th Jan 2021 to 17th Mar 2022), but
#         tables for each era with be provided in the supplementary material.
# - Age is not included because the counts are so low and risk disclosure.
#
# Table1Outcomes
# - Shows the outcomes = {30-day mortality, 6-month mortality,
#                         pulmonary complications,
#                         cerebrovascular complications, cardiac complications}
#
# TableEra
# - Shows 30-day mortality across era and cohort.
#

###################
## Prerequisites ##
###################
# ----
source(here::here("analysis","dataset_preparation.R"))
# Make Table1 for all patients.
myDataSelect <- myData %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded"),
                preOperative_infection_status!=
                  "Error: Test result after surgery. Check study_definition.")
data_to_use <- myDataSelect %>% dplyr::filter(has_surgery == TRUE)
sensitivity_cohort <- "_EntireSurgeryCohort"
source(here::here("analysis","Make_Table1.R"))
# ----

####################
## Table 1 Demogs ##
####################
# Define base tables. ----
# Pre-pandemic.
table1Demogs_PP <-
  tbl_PP_demogs %>% dplyr::filter(variable != "Age group")
table1Demogs_PP[,3:ncol(table1Demogs_PP)] <-
  table1Demogs_PP %>% dplyr::select(-c(variable, strata)) %>% 
  sapply(as.double)
# Pandemic no vaccine.
table1Demogs_PNV <-
  tbl_PNV_demogs %>% dplyr::filter(variable != "Age group")
table1Demogs_PNV[,3:ncol(table1Demogs_PNV)] <-
  table1Demogs_PNV %>% dplyr::select(-c(variable, strata)) %>% 
  sapply(as.double)
# COVIDSurg data collection period.
table1Demogs_CSP <-
  tbl_CSP_demogs %>% dplyr::filter(variable != "Age group")
table1Demogs_CSP[,3:ncol(table1Demogs_CSP)] <-
  table1Demogs_CSP %>% dplyr::select(-c(variable, strata)) %>% 
  sapply(as.double)
# Pandemic with vaccine.
table1Demogs_PWV <-
  tbl_PWV_demogs %>% dplyr::filter(variable != "Age group")
table1Demogs_PWV[,3:ncol(table1Demogs_PWV)] <-
  table1Demogs_PWV %>% dplyr::select(-c(variable, strata)) %>% 
  sapply(as.double)
# ----

# Make a tibble for the 'Timing of cancer diagnosis'. ----
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by whether
# ## they a cancer diagnosis is received within plus/minus 3 months of their 
# ## surgery:
# ##    1. Within 3 months
# ##    2. Outwith 3 months
# ##    3. No cancer diagnosis
table1_timing_of_cancer_diagnosis <- 
  data_to_use %>% 
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::mutate(
    timing_of_cancer_diagnosis = dplyr::case_when(
      .$category_cancer_within_3mths_surgery %in%
        c("Cancer diagnosis within 3mths after surgery",
          "Cancer diagnosis within 3mths before surgery") ~ "Within 3 months",
      .$category_cancer_within_3mths_surgery ==
        "No cancer diagnosis within 3mths before or after surgery" ~ "Outwith 3 months",
      .$has_cancer == FALSE ~ "No cancer diagnosis",
    )
  ) %>%
  dplyr::group_by(era, timing_of_cancer_diagnosis) %>%
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
table1_CSP_timing_of_cancer_diagnosis <- 
  data_to_use %>% 
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded")) %>%
  dplyr::mutate(
    timing_of_cancer_diagnosis = dplyr::case_when(
      .$category_cancer_within_3mths_surgery %in%
        c("Cancer diagnosis within 3mths after surgery",
          "Cancer diagnosis within 3mths before surgery") ~ "Within 3 months",
      .$category_cancer_within_3mths_surgery ==
        "No cancer diagnosis within 3mths before or after surgery" ~ "Outwith 3 months",
      .$has_cancer == FALSE ~ "No cancer diagnosis",
    )
  ) %>%
  dplyr::group_by(COVIDSurg_data_collection_period, timing_of_cancer_diagnosis) %>%
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
  ) %>% dplyr::rename(era = COVIDSurg_data_collection_period)


table1_timing_of_cancer_diagnosis <- dplyr::bind_rows(table1_CSP_timing_of_cancer_diagnosis, table1_timing_of_cancer_diagnosis)
rm(table1_CSP_timing_of_cancer_diagnosis)
# ## Ensure tibble shows zero values when categories are not in the data.
table1_timing_of_cancer_diagnosis <- 
  expand.grid(
    era = 
      era_set,
    timing_of_cancer_diagnosis = 
      c("Within 3 months",
        "Outwith 3 months",
        "No cancer diagnosis")) %>%
  dplyr::full_join(table1_timing_of_cancer_diagnosis) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(na_replace_list)
# ----

# Make vectors for tables. ----
list_timing_of_cancer_diagnosis <-
  fnc_countsAndPercentages(table_to_use = table1_timing_of_cancer_diagnosis,
                           strata = "_timing_of_cancer_diagnosis",
                           strata_col = "timing_of_cancer_diagnosis")
# ----

# Insert rows of timing data into the main tibble. ----
# Pre-pandemic.
rows_to_add <-
  list_timing_of_cancer_diagnosis$PP_timing_of_cancer_diagnosis %>%
  dplyr::select(strata, n_all_intervals, pct_all_intervals) %>%
  tibble::add_column(variable = rep("Timing of cancer diagnosis",3),
                     .before = "strata") %>%
  `colnames<-`(c("variable", "strata", "n", "pct"))
table1Demogs_PP <-
  table1Demogs_PP %>% tibble::add_row(., rows_to_add, .before = 5)
# Pandemic no vaccine.
rows_to_add <-
  list_timing_of_cancer_diagnosis$PNV_timing_of_cancer_diagnosis %>%
  tibble::add_column(., variable = rep("Timing of cancer diagnosis",3),
                     .before = "strata")
colnames(rows_to_add)[2] <- "strata"
table1Demogs_PNV <-
  table1Demogs_PNV %>% tibble::add_row(., rows_to_add, .before = 5)
# COVIDSurg data collection period.
rows_to_add <-
  list_timing_of_cancer_diagnosis$CSP_timing_of_cancer_diagnosis %>%
  tibble::add_column(., variable = rep("Timing of cancer diagnosis",3),
                     .before = "strata")
colnames(rows_to_add)[2] <- "strata"
table1Demogs_CSP <-
  table1Demogs_CSP %>% tibble::add_row(., rows_to_add, .before = 5)
# Pandemic with vaccine.
rows_to_add <-
  list_timing_of_cancer_diagnosis$PWV_timing_of_cancer_diagnosis %>%
  tibble::add_column(., variable = rep("Timing of cancer diagnosis",3),
                     .before = "strata")
colnames(rows_to_add)[2] <- "strata"
table1Demogs_PWV <-
  table1Demogs_PWV %>% tibble::add_row(., rows_to_add, .before = 5)
# Clean up
rm(rows_to_add)
# ----

# Save tibbles to CSV. ----
# Pre-pandemic.
write.csv(
  x = table1Demogs_PP,
  file = here::here("output",
                    "table1Demogs_PP.csv")
)
# Pandemic no vaccine.
write.csv(
  x = table1Demogs_PNV,
  file = here::here("output",
                    "table1Demogs_PNV.csv")
)
# COVIDSurg data collection period.
write.csv(
  x = table1Demogs_CSP,
  file = here::here("output",
                    "table1Demogs_CSP.csv")
)
# Pandemic with vaccine.
write.csv(
  x = table1Demogs_PWV,
  file = here::here("output",
                    "table1Demogs_PWV.csv")
)
# ----


######################
## Table 1 Outcomes ##
######################
# Define base tables. ----
# Pre-pandemic.
table1Outcomes_PP <-
  tbl_PP_outcomes %>%
    dplyr::filter(!variable %in% c("90-day post-operative mortality",
                                   "12-month post-operative mortality"))
table1Outcomes_PP[,3:ncol(table1Outcomes_PP)] <-
  table1Outcomes_PP %>% dplyr::select(-c(variable, strata)) %>% 
  sapply(as.double)
# Pandemic no vaccine.
table1Outcomes_PNV <-
  tbl_PNV_outcomes %>%
    dplyr::filter(!variable %in% c("90-day post-operative mortality",
                                   "12-month post-operative mortality"))
table1Outcomes_PNV[,3:ncol(table1Outcomes_PNV)] <-
  table1Outcomes_PNV %>% dplyr::select(-c(variable, strata)) %>% 
  sapply(as.double)
# COVIDSurg data collection period.
table1Outcomes_CSP <-
  tbl_CSP_outcomes %>%
    dplyr::filter(!variable %in% c("90-day post-operative mortality",
                                   "12-month post-operative mortality"))
table1Outcomes_CSP[,3:ncol(table1Outcomes_CSP)] <-
  table1Outcomes_CSP %>% dplyr::select(-c(variable, strata)) %>% 
  sapply(as.double)
# Pandemic with vaccine.
table1Outcomes_PWV <-
  tbl_PWV_outcomes %>%
    dplyr::filter(!variable %in% c("90-day post-operative mortality",
                                   "12-month post-operative mortality"))
table1Outcomes_PWV[,3:ncol(table1Outcomes_PWV)] <-
  table1Outcomes_PWV %>% dplyr::select(-c(variable, strata)) %>% 
  sapply(as.double)
# ----

# Redactions based on servers results on 2022 07 08. ----
# # Outcomes
# # # Pandemic no vaccine.
table1Outcomes_PNV[which(table1Outcomes_PNV$variable ==
                           "30-day post-operative cardiac complication"),
                   c("n_infection_3to4wk", "pct_infection_3to4wk",
                     "n_infection_5to6wk", "pct_infection_5to6wk")] <- NA 
table1Outcomes_PNV[which(table1Outcomes_PNV$variable ==
                           "30-day post-operative pulmonary complication"),
                   c("n_infection_0to2wk", "pct_infection_0to2wk",
                     "n_infection_5to6wk", "pct_infection_5to6wk")] <- NA
table1Outcomes_PNV[which(table1Outcomes_PNV$variable ==
                           "30-day post-operative cerebrovascular complication"),
                   c("n_infection_0to2wk", "pct_infection_0to2wk",
                     "n_infection_5to6wk", "pct_infection_5to6wk")] <- NA

# # # Pandemic with vaccine.
table1Outcomes_PWV[which(table1Outcomes_PWV$variable ==
                           "30-day post-operative cardiac complication"),
                   c("n_infection_0to2wk", "pct_infection_0to2wk",
                     "n_infection_3to4wk", "pct_infection_3to4wk",
                     "n_infection_5to6wk", "pct_infection_5to6wk")] <- NA 
table1Outcomes_PWV[which(table1Outcomes_PWV$variable ==
                           "30-day post-operative pulmonary complication"),
                   c("n_infection_3to4wk", "pct_infection_3to4wk",
                     "n_infection_5to6wk", "pct_infection_5to6wk")] <- NA
table1Outcomes_PWV[which(table1Outcomes_PWV$variable ==
                           "30-day post-operative cerebrovascular complication"),
                   c("n_infection_0to2wk", "pct_infection_0to2wk",
                     "n_infection_5to6wk", "pct_infection_5to6wk",
                     "n_infection_7wk", "pct_infection_7wk")] <- NA

# # # COVIDSurg data collection period.
table1Outcomes_CSP[, !colnames(table1Outcomes_CSP) %in%
                     c("variable", "strata",
                       "n_all_intervals", "pct_all_intervals",
                       "n_infection_none", "pct_infection_none",
                       "n_infection_7wk", "pct_infection_7wk")] <- NA

# # Demographics
# # #
table1Demogs_CSP[, !colnames(table1Outcomes_CSP) %in%
                   c("variable", "strata",
                     "n_all_intervals", "pct_all_intervals",
                     "n_infection_none", "pct_infection_none",
                     "n_infection_7wk", "pct_infection_7wk")] <- NA
table1Demogs_CSP[which(table1Demogs_CSP$strata %in%
                         c("Outwith 3 months", "Within 3 months")),
                   c("n_infection_7wk", "pct_infection_7wk")] <- NA

# ----

# Save tibbles to CSV. ----
# Pre-pandemic.
write.csv(
  x = table1Outcomes_PP,
  file = here::here("output",
                    "table1Outcomes_PP.csv")
)
# Pandemic no vaccine.
write.csv(
  x = table1Outcomes_PNV,
  file = here::here("output",
                    "table1Outcomes_PNV.csv")
)
# COVIDSurg data collection period.
write.csv(
  x = table1Outcomes_CSP,
  file = here::here("output",
                    "table1Outcomes_CSP.csv")
)
# Pandemic with vaccine.
write.csv(
  x = table1Outcomes_PWV,
  file = here::here("output",
                    "table1Outcomes_PWV.csv")
)
# ----


###############
## Table Era ##
###############
# ----
# This table is just the table_mortality_intervals tibble produced in 
# Make_table_COVIDSurg_compare.R, with some rearranging.
#
# Here, we just make sure it has been created, then rearrange and rename it.
#

source(here::here("analysis","Make_table_COVIDSurg_compare.R"))

TableEra <-
  rbind(
    table_mortality_intervals[c("PNV_OS_all", "PNV_OS_NC"),],
    PNV_OS_C = table_mortality_intervals[c("PNV_OS_C_within3m", "PNV_OS_C_outwith3m"),] %>% colSums,
    table_mortality_intervals[c("CSP_COVIDSurg","CSP_OS_all", "CSP_OS_NC"),],
    CSP_OS_C = table_mortality_intervals[c("PNV_OS_C_within3m", "PNV_OS_C_outwith3m"),] %>% colSums,
    table_mortality_intervals[c("PWV_OS_all", "PWV_OS_NC"),],
    PWV_OS_C = table_mortality_intervals[c("PWV_OS_C_within3m", "PWV_OS_C_outwith3m"),] %>% colSums
  )

write.csv(
  x = TableEra,
  file = here::here("output", "TableEra.csv")
)
# ----

