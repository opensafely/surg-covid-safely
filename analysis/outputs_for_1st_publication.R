# output_for_1st_publication.R
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
# - table_counts is needed for columns totals.
#

###################
## Prerequisites ##
###################
# ----
source(here::here("analysis","Make_table_COVIDSurg_compare.R"))
# Make Table1 for all patients.
myDataSelect <- myData %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded"),
                preOperative_infection_status!=
                  "Error: Test result after surgery. Check study_definition.",
                era != "No surgery recorded")
data_to_use <- myDataSelect %>% dplyr::filter(has_surgery == TRUE)
sensitivity_cohort <- "all"
source(here::here("analysis","Make_Table1.R"))
# ----

#####################################################
## Remove unwanted variables from Table 1 Outcomes ##
#####################################################
# ----
# Pre-pandemic.
table1Outcomes_PP <-
  table1Outcomes_PP %>%
    dplyr::filter(!variable %in% c("90-day post-operative mortality",
                                   "12-month post-operative mortality"))
table1Outcomes_PP[,3:ncol(table1Outcomes_PP)] <-
  table1Outcomes_PP %>% dplyr::select(-c(variable, strata)) %>% 
  sapply(as.double)
# Pandemic no vaccine.
table1Outcomes_PNV <-
  table1Outcomes_PNV %>%
    dplyr::filter(!variable %in% c("90-day post-operative mortality",
                                   "12-month post-operative mortality"))
table1Outcomes_PNV[,3:ncol(table1Outcomes_PNV)] <-
  table1Outcomes_PNV %>% dplyr::select(-c(variable, strata)) %>% 
  sapply(as.double)
# COVIDSurg data collection period.
table1Outcomes_CSP <-
  table1Outcomes_CSP %>%
    dplyr::filter(!variable %in% c("90-day post-operative mortality",
                                   "12-month post-operative mortality"))
table1Outcomes_CSP[,3:ncol(table1Outcomes_CSP)] <-
  table1Outcomes_CSP %>% dplyr::select(-c(variable, strata)) %>% 
  sapply(as.double)
# Pandemic with vaccine.
table1Outcomes_PWV <-
  table1Outcomes_PWV %>%
    dplyr::filter(!variable %in% c("90-day post-operative mortality",
                                   "12-month post-operative mortality"))
table1Outcomes_PWV[,3:ncol(table1Outcomes_PWV)] <-
  table1Outcomes_PWV %>% dplyr::select(-c(variable, strata)) %>% 
  sapply(as.double)
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

# Combine both cancer groups.
PNV_OS_C <-
  table_mortality_intervals[c("PNV_OS_C_within3m", "PNV_OS_C_outwith3m"),] %>%
  colSums()
PNV_OS_C[seq(2, ncol(table_mortality_intervals), 2)] <-
  PNV_OS_C[seq(1, ncol(table_mortality_intervals), 2)] /
  colSums(rbind(PNV_OS_C_within3m_counts, PNV_OS_C_outwith3m_counts)) * 100
CSP_OS_C <-
  table_mortality_intervals[c("CSP_OS_C_within3m", "CSP_OS_C_outwith3m"),] %>%
  colSums()
CSP_OS_C[seq(2, ncol(table_mortality_intervals), 2)] <-
  CSP_OS_C[seq(1, ncol(table_mortality_intervals), 2)] /
  colSums(rbind(CSP_OS_C_within3m_counts, CSP_OS_C_outwith3m_counts)) * 100
PWV_OS_C <-
  table_mortality_intervals[c("PWV_OS_C_within3m", "PWV_OS_C_outwith3m"),] %>%
  colSums()
PWV_OS_C[seq(2, ncol(table_mortality_intervals), 2)] <-
  PWV_OS_C[seq(1, ncol(table_mortality_intervals), 2)] /
  colSums(rbind(PWV_OS_C_within3m_counts, PWV_OS_C_outwith3m_counts)) * 100

# Make table.
TableEra <-
  rbind(
    table_mortality_intervals[c("PNV_OS_all", "PNV_OS_NC"),],
    PNV_OS_C = PNV_OS_C,
    table_mortality_intervals[c("CSP_COVIDSurg","CSP_OS_all", "CSP_OS_NC"),],
    CSP_OS_C = CSP_OS_C,
    table_mortality_intervals[c("PWV_OS_all", "PWV_OS_NC"),],
    PWV_OS_C = PWV_OS_C
  ) %>%
  dplyr::mutate(dplyr::across(, ~ ifelse(is.nan(.),NA,.))) %>%
  tidyr::replace_na(list("pct_total" = 0,
                         "pct_all_intervals" = 0,
                         "pct_infection_none" = 0,
                         "pct_infection_0to2wk" = 0,
                         "pct_infection_3to4wk" = 0,
                         "pct_infection_5to6wk" = 0,
                         "pct_infection_7wk" = 0)
                    )
# ----

########################################################
## Redactions based on servers results on 2022 08 19. ##
########################################################
# ----
# table1Outcomes
## Pandemic no vaccine.
# table1Outcomes_PNV[which(table1Outcomes_PNV$variable ==
#                            "30-day post-operative cardiac complication"),
#                    c("n_infection_3to4wk", "pct_infection_3to4wk",
#                      "n_infection_5to6wk", "pct_infection_5to6wk")] <- NA
# table1Outcomes_PNV[which(table1Outcomes_PNV$variable ==
#                            "30-day post-operative pulmonary complication"),
#                    c("n_infection_0to2wk", "pct_infection_0to2wk",
#                      "n_infection_5to6wk", "pct_infection_5to6wk")] <- NA
# table1Outcomes_PNV[which(table1Outcomes_PNV$variable ==
#                            "30-day post-operative cerebrovascular complication"),
#                    c("n_infection_0to2wk", "pct_infection_0to2wk",
#                      "n_infection_5to6wk", "pct_infection_5to6wk")] <- NA
# ## COVIDSurg data collection period.
# table1Outcomes_CSP[, !colnames(table1Outcomes_CSP) %in%
#                      c("variable", "strata",
#                        "n_all_intervals", "pct_all_intervals",
#                        "n_infection_none", "pct_infection_none",
#                       "n_infection_7wk", "pct_infection_7wk")] <- NA
## Pandemic with vaccine.
table1Outcomes_PWV[which(table1Outcomes_PWV$variable ==
                           "30-day post-operative cerebrovascular complication"),
                   c("n_infection_0to2wk", "pct_infection_0to2wk",
                     "n_infection_3to4wk", "pct_infection_3to4wk")] <- NA


# table1Demogs
# No redactions needed.

# TableEra
TableEra[2,        c("d_infection_3to4wk", "pct_infection_3to4wk",
                     "d_infection_5to6wk", "pct_infection_5to6wk")] <- NA
TableEra[c(3,5),   c("d_infection_0to2wk", "pct_infection_0to2wk",
                     "d_infection_3to4wk", "pct_infection_3to4wk",
                     "d_infection_5to6wk", "pct_infection_5to6wk",
                     "d_infection_7wk", "pct_infection_7wk")] <- NA
TableEra[c(6,7),   c("d_infection_5to6wk", "pct_infection_5to6wk",
                     "d_infection_7wk", "pct_infection_7wk")] <- NA
TableEra[10,       c("d_infection_0to2wk", "pct_infection_0to2wk",
                     "d_infection_3to4wk", "pct_infection_3to4wk",
                     "d_infection_5to6wk", "pct_infection_5to6wk")] <- NA
# tableCounts
table_counts[c(14:15),  c("n_infection_0to2wk", "n_infection_3to4wk",
                          "n_infection_5to6wk")] <- NA
table_counts[14,        c("d_infection_7wk")] <- NA
# ----

##########################
## Save tibbles to CSV. ##
##########################
# ----
# Pre-pandemic.
write.csv(
  x = table1Outcomes_PP,
  file = here::here("output",
                    paste0("table1Outcomes_PP","_",sensitivity_cohort,".csv"))
)
# Pandemic no vaccine.
write.csv(
  x = table1Outcomes_PNV,
  file = here::here("output",
                    paste0("table1Outcomes_PNV","_",sensitivity_cohort,".csv"))
)
# COVIDSurg data collection period.
write.csv(
  x = table1Outcomes_CSP,
  file = here::here("output",
                    paste0("table1Outcomes_CSP","_",sensitivity_cohort,".csv"))
)
# Pandemic with vaccine.
write.csv(
  x = table1Outcomes_PWV,
  file = here::here("output",
                    paste0("table1Outcomes_PWV","_",sensitivity_cohort,".csv"))
)
# TableEra
write.csv(
  x = TableEra,
  file = here::here("output", "TableEra.csv"),
  row.names = T
)
# ----