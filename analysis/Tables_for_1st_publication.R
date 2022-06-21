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
# Check if sub tables already been made.
if(file.exists(
  here::here("output",
             "table_30day_post-op_mortality_for_each_cohort_in_each_era.csv")
  ) == F)
{
  source(here::here("analysis","dataset_preparation.R"))
  # Make Table1 for all patients.
  data_to_use <- myData
  sensitivity_cohort <- ""
  source(here::here("analysis","Make_Table1.R"))
}
# ----

####################
## Table 1 Demogs ##
####################
# Define base tables. ----
# Pre-pandemic.
table1Demogs_PP <-
  tbl_PP_strata %>% dplyr::filter(variable != "Age group")
table1Demogs_PP[,3:ncol(table1Demogs_PP)] <-
  table1Demogs_PP %>% dplyr::select(-c(variable, strata)) %>% 
  sapply(as.double)
# Pandemic no vaccine.
table1Demogs_PNV <-
  tbl_PNV_strata %>% dplyr::filter(variable != "Age group")
table1Demogs_PNV[,3:ncol(table1Demogs_PNV)] <-
  table1Demogs_PNV %>% dplyr::select(-c(variable, strata)) %>% 
  sapply(as.double)
# COVIDSurg data collection period.
table1Demogs_CSP <-
  tbl_CSP_strata %>% dplyr::filter(variable != "Age group")
table1Demogs_CSP[,3:ncol(table1Demogs_CSP)] <-
  table1Demogs_CSP %>% dplyr::select(-c(variable, strata)) %>% 
  sapply(as.double)
# Pandemic with vaccine.
table1Demogs_PWV <-
  tbl_PWV_strata %>% dplyr::filter(variable != "Age group")
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
tbl_timing_of_cancer_diagnosis <- 
  myData %>% dplyr::filter(era != "Error: No surgery") %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation")) %>%
    dplyr::filter(category_cancer_within_3mths_surgery != "No surgery recorded") %>%
    dplyr::mutate(
      cancer_diagnosis_within_3mths = dplyr::case_when(
        .$category_cancer_within_3mths_surgery %in%
          c("Cancer diagnosis within 3mths after surgery",
            "Cancer diagnosis within 3mths before surgery") ~ "Within 3 months",
        .$category_cancer_within_3mths_surgery ==
          "No cancer diagnosis within 3mths before or after surgery" ~ "Outwith 3 months",
        .$category_cancer_within_3mths_surgery ==
          "No cancer diagnosis recorded" ~ "No cancer diagnosis",
      )
    ) %>%
    dplyr::group_by(era, cancer_diagnosis_within_3mths) %>%
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
tbl_CSP <- 
  myData %>% dplyr::filter(era != "Error: No surgery") %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation")) %>%
  dplyr::filter(category_cancer_within_3mths_surgery != "No surgery recorded") %>%
  dplyr::mutate(
    cancer_diagnosis_within_3mths = dplyr::case_when(
      .$category_cancer_within_3mths_surgery %in%
        c("Cancer diagnosis within 3mths after surgery",
          "Cancer diagnosis within 3mths before surgery") ~ "Within 3 months",
      .$category_cancer_within_3mths_surgery ==
        "No cancer diagnosis within 3mths before or after surgery" ~ "Outwith 3 months",
      .$category_cancer_within_3mths_surgery ==
        "No cancer diagnosis recorded" ~ "No cancer diagnosis",
    )
  ) %>%
  dplyr::group_by(COVIDSurg_data_collection_period, cancer_diagnosis_within_3mths) %>%
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
  ) %>% `colnames<-`(c("era", colnames(tbl_timing_of_cancer_diagnosis)[2:ncol(tbl_timing_of_cancer_diagnosis)]))
tbl_timing_of_cancer_diagnosis <- dplyr::bind_rows(tbl_timing_of_cancer_diagnosis, tbl_CSP)
rm(tbl_CSP)
# ## Ensure tibble shows zero values when categories are not in the data.
tbl_timing_of_cancer_diagnosis <- 
  expand.grid(
    era = 
      era_set,
    cancer_diagnosis_within_3mths = 
      c("Within 3 months",
        "Outwith 3 months",
        "No cancer diagnosis")) %>%
  dplyr::full_join(tbl_timing_of_cancer_diagnosis) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(na_replace_list)
# ----

# Make vectors for tables. ----
# ## ## Pre-pandemic
# ## ## ## Get counts per intervals and overall.
PP_n_timing_of_cancer_diagnosis <- 
  tbl_timing_of_cancer_diagnosis %>% dplyr::filter(era=="Pre-pandemic") %>%
  dplyr::arrange(cancer_diagnosis_within_3mths) %>% dplyr::ungroup() %>% dplyr::select("n_all_intervals")
# ## ## ## Get percentages per intervals and overall.
PP_pct_timing_of_cancer_diagnosis <- (PP_n_timing_of_cancer_diagnosis / sum(PP_n_timing_of_cancer_diagnosis)) %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0)) %>%
  `colnames<-`(c("pct_all_intervals"))
PP_timing_of_cancer_diagnosis <- tbl_timing_of_cancer_diagnosis %>% dplyr::filter(era=="Pandemic no vaccine") %>%
  dplyr::arrange(cancer_diagnosis_within_3mths) %>% dplyr::select(cancer_diagnosis_within_3mths) %>%
  dplyr::bind_cols(., PP_n_timing_of_cancer_diagnosis, PP_pct_timing_of_cancer_diagnosis)
# ## ## ## Clean up.
rm(PP_n_timing_of_cancer_diagnosis, PP_pct_timing_of_cancer_diagnosis)

# ## ## Pandemic no vaccine.
# ## ## ## Get counts per intervals and overall.
PNV_n_timing_of_cancer_diagnosis <- 
  tbl_timing_of_cancer_diagnosis %>% dplyr::filter(era=="Pandemic no vaccine") %>%
  dplyr::arrange(cancer_diagnosis_within_3mths) %>% dplyr::ungroup() %>% dplyr::select(-c(era,cancer_diagnosis_within_3mths))
# ## ## ## Get percentages per intervals and overall.
PNV_pct_timing_of_cancer_diagnosis <- 
  tbl_timing_of_cancer_diagnosis %>% dplyr::filter(era=="Pandemic no vaccine") %>% dplyr::select(-c(era,cancer_diagnosis_within_3mths)) %>%
  colSums() %>% sweep(PNV_n_timing_of_cancer_diagnosis, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0
  )) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## ## Interlace counts and percentages.
PNV_timing_of_cancer_diagnosis <- matrix(0,
                  nrow = length(rownames(PNV_n_timing_of_cancer_diagnosis)),
                  ncol = length(colnames(PNV_n_timing_of_cancer_diagnosis))*2) %>%
  as.data.frame()
PNV_timing_of_cancer_diagnosis[,seq(1,length(colnames(PNV_timing_of_cancer_diagnosis)),2)] <- PNV_n_timing_of_cancer_diagnosis
PNV_timing_of_cancer_diagnosis[,seq(2,length(colnames(PNV_timing_of_cancer_diagnosis)),2)] <- PNV_pct_timing_of_cancer_diagnosis
colnames(PNV_timing_of_cancer_diagnosis)[seq(1,length(colnames(PNV_timing_of_cancer_diagnosis)),2)] <- colnames(PNV_n_timing_of_cancer_diagnosis)
colnames(PNV_timing_of_cancer_diagnosis)[seq(2,length(colnames(PNV_timing_of_cancer_diagnosis)),2)] <- colnames(PNV_pct_timing_of_cancer_diagnosis)
PNV_timing_of_cancer_diagnosis <- tbl_timing_of_cancer_diagnosis %>%  dplyr::filter(era=="Pandemic no vaccine") %>%
  dplyr::arrange(cancer_diagnosis_within_3mths) %>% dplyr::select("cancer_diagnosis_within_3mths") %>% dplyr::bind_cols(PNV_timing_of_cancer_diagnosis)
# ## ## ## Clean up.
rm(PNV_n_timing_of_cancer_diagnosis, PNV_pct_timing_of_cancer_diagnosis)

# ## ## COVIDSurg data collection period.
# ## ## ## Get counts per intervals and overall.
CSP_n_timing_of_cancer_diagnosis <- 
  tbl_timing_of_cancer_diagnosis %>%  dplyr::filter(era=="COVIDSurg data collection period") %>%
  dplyr::arrange(cancer_diagnosis_within_3mths) %>% dplyr::ungroup() %>% dplyr::select(-c(era,cancer_diagnosis_within_3mths))
# ## ## ## Get percentages per intervals and overall.
CSP_pct_timing_of_cancer_diagnosis <- 
  tbl_timing_of_cancer_diagnosis %>% dplyr::filter(era=="COVIDSurg data collection period") %>% dplyr::select(-c(era,cancer_diagnosis_within_3mths)) %>%
  colSums() %>% sweep(CSP_n_timing_of_cancer_diagnosis, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0
  )) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## ## Interlace counts and percentages.
CSP_timing_of_cancer_diagnosis <- matrix(0,
                  nrow = length(rownames(CSP_n_timing_of_cancer_diagnosis)),
                  ncol = length(colnames(CSP_n_timing_of_cancer_diagnosis))*2) %>%
  as.data.frame()
CSP_timing_of_cancer_diagnosis[,seq(1,length(colnames(CSP_timing_of_cancer_diagnosis)),2)] <- CSP_n_timing_of_cancer_diagnosis
CSP_timing_of_cancer_diagnosis[,seq(2,length(colnames(CSP_timing_of_cancer_diagnosis)),2)] <- CSP_pct_timing_of_cancer_diagnosis
colnames(CSP_timing_of_cancer_diagnosis)[seq(1,length(colnames(CSP_timing_of_cancer_diagnosis)),2)] <- colnames(CSP_n_timing_of_cancer_diagnosis)
colnames(CSP_timing_of_cancer_diagnosis)[seq(2,length(colnames(CSP_timing_of_cancer_diagnosis)),2)] <- colnames(CSP_pct_timing_of_cancer_diagnosis)
CSP_timing_of_cancer_diagnosis <- tbl_timing_of_cancer_diagnosis %>%  dplyr::filter(era=="COVIDSurg data collection period") %>%
  dplyr::arrange(cancer_diagnosis_within_3mths) %>% dplyr::select("cancer_diagnosis_within_3mths") %>% dplyr::bind_cols(CSP_timing_of_cancer_diagnosis)
# ## ## ## Clean up.
rm(CSP_n_timing_of_cancer_diagnosis, CSP_pct_timing_of_cancer_diagnosis)

# ## ## Pandemic with vaccine.
# ## ## ## Get counts per intervals and overall.
PWV_n_timing_of_cancer_diagnosis <- 
  tbl_timing_of_cancer_diagnosis %>%  dplyr::filter(era=="Pandemic with vaccine") %>%
  dplyr::arrange(cancer_diagnosis_within_3mths) %>% dplyr::ungroup() %>% dplyr::select(-c(era,cancer_diagnosis_within_3mths))
# ## ## ## Get percentages per intervals and overall.
PWV_pct_timing_of_cancer_diagnosis <- 
  tbl_timing_of_cancer_diagnosis %>% dplyr::filter(era=="Pandemic with vaccine") %>% dplyr::select(-c(era,cancer_diagnosis_within_3mths)) %>%
  colSums() %>% sweep(PWV_n_timing_of_cancer_diagnosis, 2, ., "/") %>% "*"(100) %>%
  tidyr::replace_na(list("n_all_intervals" = 0, "n_infection_none" = 0,
                         "n_infection_0to2wk"  = 0, "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0, "n_infection_7wk" = 0
  )) %>%
  `colnames<-`(c("pct_all_intervals", "pct_infection_none", "pct_infection_0to2wk",
                 "pct_infection_3to4wk", "pct_infection_5to6wk", "pct_infection_7wk"))
# ## ## ## Interlace counts and percentages.
PWV_timing_of_cancer_diagnosis <- matrix(0,
                  nrow = length(rownames(PWV_n_timing_of_cancer_diagnosis)),
                  ncol = length(colnames(PWV_n_timing_of_cancer_diagnosis))*2) %>%
  as.data.frame()
PWV_timing_of_cancer_diagnosis[,seq(1,length(colnames(PWV_timing_of_cancer_diagnosis)),2)] <- PWV_n_timing_of_cancer_diagnosis
PWV_timing_of_cancer_diagnosis[,seq(2,length(colnames(PWV_timing_of_cancer_diagnosis)),2)] <- PWV_pct_timing_of_cancer_diagnosis
colnames(PWV_timing_of_cancer_diagnosis)[seq(1,length(colnames(PWV_timing_of_cancer_diagnosis)),2)] <- colnames(PWV_n_timing_of_cancer_diagnosis)
colnames(PWV_timing_of_cancer_diagnosis)[seq(2,length(colnames(PWV_timing_of_cancer_diagnosis)),2)] <- colnames(PWV_pct_timing_of_cancer_diagnosis)
PWV_timing_of_cancer_diagnosis <- tbl_timing_of_cancer_diagnosis %>%  dplyr::filter(era=="Pandemic with vaccine") %>%
  dplyr::arrange(cancer_diagnosis_within_3mths) %>% dplyr::select("cancer_diagnosis_within_3mths") %>% dplyr::bind_cols(PWV_timing_of_cancer_diagnosis)
# ## ## ## Clean up
rm(PWV_n_timing_of_cancer_diagnosis, PWV_pct_timing_of_cancer_diagnosis)
# ----

# Insert rows of timing data into the main tibble. ----
# Pre-pandemic.
rows_to_add <-
  PP_timing_of_cancer_diagnosis %>%
  tibble::add_column(., variable = rep("Timing of cancer diagnosis",3),
                     .before = "cancer_diagnosis_within_3mths") %>%
  `colnames<-`(c("variable", "strata", "n", "pct"))
table1Demogs_PP <-
  table1Demogs_PP %>% tibble::add_row(., rows_to_add, .before = 5)
# Pandemic no vaccine.
rows_to_add <-
  PNV_timing_of_cancer_diagnosis %>%
  tibble::add_column(., variable = rep("Timing of cancer diagnosis",3),
                     .before = "cancer_diagnosis_within_3mths")
colnames(rows_to_add)[2] <- "strata"
table1Demogs_PNV <-
  table1Demogs_PNV %>% tibble::add_row(., rows_to_add, .before = 5)
# COVIDSurg data collection period.
rows_to_add <-
  CSP_timing_of_cancer_diagnosis %>%
  tibble::add_column(., variable = rep("Timing of cancer diagnosis",3),
                     .before = "cancer_diagnosis_within_3mths")
colnames(rows_to_add)[2] <- "strata"
table1Demogs_CSP <-
  table1Demogs_CSP %>% tibble::add_row(., rows_to_add, .before = 5)
# Pandemic with vaccine.
rows_to_add <-
  PWV_timing_of_cancer_diagnosis %>%
  tibble::add_column(., variable = rep("Timing of cancer diagnosis",3),
                     .before = "cancer_diagnosis_within_3mths")
colnames(rows_to_add)[2] <- "strata"
table1Demogs_PWV <-
  table1Demogs_PWV %>% tibble::add_row(., rows_to_add, .before = 5)
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
  tbl_PP_outcome %>%
    dplyr::filter(!variable %in% c("90-day post-operative mortality",
                                   "12-month post-operative mortality"))
table1Outcomes_PP[,3:ncol(table1Outcomes_PP)] <-
  table1Outcomes_PP %>% dplyr::select(-c(variable, strata)) %>% 
  sapply(as.double)
# Pandemic no vaccine.
table1Outcomes_PNV <-
  tbl_PNV_outcome %>%
    dplyr::filter(!variable %in% c("90-day post-operative mortality",
                                   "12-month post-operative mortality"))
table1Outcomes_PNV[,3:ncol(table1Outcomes_PNV)] <-
  table1Outcomes_PNV %>% dplyr::select(-c(variable, strata)) %>% 
  sapply(as.double)
# COVIDSurg data collection period.
table1Outcomes_CSP <-
  tbl_CSP_outcome %>%
    dplyr::filter(!variable %in% c("90-day post-operative mortality",
                                   "12-month post-operative mortality"))
table1Outcomes_CSP[,3:ncol(table1Outcomes_CSP)] <-
  table1Outcomes_CSP %>% dplyr::select(-c(variable, strata)) %>% 
  sapply(as.double)
# Pandemic with vaccine.
table1Outcomes_PWV <-
  tbl_PWV_outcome %>%
    dplyr::filter(!variable %in% c("90-day post-operative mortality",
                                   "12-month post-operative mortality"))
table1Outcomes_PWV[,3:ncol(table1Outcomes_PWV)] <-
  table1Outcomes_PWV %>% dplyr::select(-c(variable, strata)) %>% 
  sapply(as.double)
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
# This table is just the table_mortality_totals tibble produced in 
# Make_table_COVIDSurg_compare.R.
#
# Here, we just make sure it has been created and then rename it.
#

data_to_use_C_within3m <- myData %>% 
  dplyr::filter(category_cancer_within_3mths_surgery == 
                  "Cancer diagnosis within 3mths before surgery" |
                  category_cancer_within_3mths_surgery == 
                  "Cancer diagnosis within 3mths after surgery")
data_to_use_C_outwith3m <- myData %>% 
  dplyr::filter(category_cancer_within_3mths_surgery == 
                  "No cancer diagnosis within 3mths before or after surgery")
data_to_use_NC <- myData %>% dplyr::filter(has_cancer == FALSE)
source(here::here("analysis","Make_table_COVIDSurg_compare.R"))
TableEra <- table_mortality_totals
write.csv(
  x = TableEra,
  file = here::here("output", "TableEra.csv")
)
# ----

