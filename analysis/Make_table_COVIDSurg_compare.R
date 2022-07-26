# Make_table_COVIDSurg_compare.R
#
# This script processes data from the myData dataframe to create a table
# that compares the 30-day post-operative mortality in the following cohorts:
#   1. COVIDSurg study estimates (doi: 10.1111/anae.15458)
#   2. OpenSAFELY surgery patients who do not have a cancer diagnosis within 3
#      months before or after their surgery.
#   3. OpenSAFELY surgery patients who do have a cancer diagnosis within 3
#      months before or after their surgery.
#   4. OpenSAFELY surgery patients, regardless of cancer diagnosis.
#

################################################
## Hardcode values from COVIDSurg publication ##
################################################
# ----
## Source is Table 1 from doi = 10.1111/anae.15458.
# Counts of patients, across intervals.
COVIDSurg_counts <- data.frame(
  n_total = 140231,
  n_infection_none = 137104,
  n_infection_0to2wk = 1138,
  n_infection_3to4wk = 461,
  n_infection_5to6wk = 326,
  n_infection_7wk = 1202
)
# Counts of 30-day post-operative mortality, across intervals.
COVIDSurg_mortality_intervals <- data.frame(
  d_total = 3938,
  pct_total = NA,
  d_infection_none = 3654,
  pct_infection_none = NA,
  d_infection_0to2wk = 149,
  pct_infection_0to2wk = NA,
  d_infection_3to4wk = 60,
  pct_infection_3to4wk = NA,
  d_infection_5to6wk = 33,
  pct_infection_5to6wk = NA,
  d_infection_7wk = 42,
  pct_infection_7wk = NA
)
# Percentages of 30-day post-operative mortality, across intervals.
COVIDSurg_mortality_intervals[seq(2,12,2)] <- 
  (COVIDSurg_mortality_intervals[seq(1,12,2)] / COVIDSurg_counts) * 100
# Counts of 30-day post-operative mortality, across era.
COVIDSurg_mortality_totals <- data.frame(
  d_PP = NA,
  pct_PP = NA,
  d_PNV = NA,
  pct_PNV = NA,
  d_CSP = COVIDSurg_mortality_intervals[['d_total']],
  pct_CSP =  COVIDSurg_mortality_intervals[['pct_total']],
  d_PWV = NA,
  pct_PWV = NA
)
# ----

####################
## Generate data. ##
####################
# ----
source(here::here("analysis","dataset_preparation.R"))
myDataSelect <- myData %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded"),
                preOperative_infection_status!=
                  "Error: Test result after surgery. Check study_definition.")
data_to_use_all <- myDataSelect %>% dplyr::filter(has_surgery == TRUE)
data_to_use_NC <- myDataSelect %>% dplyr::filter(has_surgery == TRUE) %>%
  dplyr::filter(has_cancer == FALSE)
data_to_use_C_within3m <- myDataSelect %>% 
  dplyr::filter(has_cancer == TRUE) %>%
  dplyr::filter(has_surgery == TRUE) %>%
  dplyr::filter(category_cancer_within_3mths_surgery == 
                  "Cancer diagnosis within 3mths before surgery" |
                  category_cancer_within_3mths_surgery == 
                  "Cancer diagnosis within 3mths after surgery")
data_to_use_C_outwith3m <- myDataSelect %>%
  dplyr::filter(has_cancer == TRUE) %>%
  dplyr::filter(has_surgery == TRUE) %>%
  dplyr::filter(category_cancer_within_3mths_surgery == 
                  "No cancer diagnosis within 3mths before or after surgery")


# ----


###########################################
## Counts of patients, across intervals. ##
###########################################
# ----
# Count  of patients in each of the categories for 
# pre-operative infection status:
#     1. "No record of pre-operative SARS-CoV-2 infection"
#     2. "0-2 weeks record of pre-operative SARS-CoV-2 infection"
#     3. "3-4 weeks record of pre-operative SARS-CoV-2 infection"
#     4. "5-6 weeks record of pre-operative SARS-CoV-2 infection"
#     5. ">=7 weeks record of pre-operative SARS-CoV-2 infection"
#     6. "Error: Test result after surgery. Check study_definition."
# 
#  The counts are also stratified by surgery era:
#     1. "Pre-pandemic" <= "2020-03-17"
#     2. "Pandemic no vaccine" <= ("2020-12-08" +
#                                  3 weeks for national roll-out +
#                                  2 weeks to take effect)
#     3. "Pandemic with vaccine" > ("2020-12-08" + 5 weeks ) [explained above]
#
# Naming convention:
#   - PP  = Pre-pandemic
#   - PNV = Pandemic no vaccine
#   - PWV = Pandemic with vaccine
#
intervals <- c(
  "No record of pre-operative SARS-CoV-2 infection",
  "0-2 weeks record of pre-operative SARS-CoV-2 infection",
  "3-4 weeks record of pre-operative SARS-CoV-2 infection",
  "5-6 weeks record of pre-operative SARS-CoV-2 infection",
  ">=7 weeks record of pre-operative SARS-CoV-2 infection"
)
## #  OpenSAFELY data, all surgery patients.----
OS_all_counts <-
  data_to_use_all %>%
  dplyr::group_by(era,
                  preOperative_infection_status) %>%
  dplyr::summarise(n = n())
CSP_OS_all_counts <- 
  data_to_use_all %>% dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
  dplyr::group_by(COVIDSurg_data_collection_period, preOperative_infection_status) %>%
  dplyr::summarise(n = n()) %>%
  `colnames<-`(colnames(OS_all_counts))
OS_all_counts <-
  dplyr::bind_rows(OS_all_counts, CSP_OS_all_counts)
OS_all_counts <- 
  expand.grid(
    era = 
      c("Error: No surgery", "Pre-pandemic", "Pandemic no vaccine",
        "Pandemic with vaccine", "COVIDSurg data collection period",
        "Not COVIDSurg data collection period"),
    "preOperative_infection_status" = 
      c("Error: Test result after surgery. Check study_definition.",
        "No record of pre-operative SARS-CoV-2 infection",
        "0-2 weeks record of pre-operative SARS-CoV-2 infection",
        "3-4 weeks record of pre-operative SARS-CoV-2 infection",
        "5-6 weeks record of pre-operative SARS-CoV-2 infection",
        ">=7 weeks record of pre-operative SARS-CoV-2 infection")) %>%
  dplyr::full_join(OS_all_counts) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(list("n" = 0))
rm(CSP_OS_all_counts)
## ## # Pre-pandemic, OpenSAFELY data, no-cancer patients.
PP_OS_all_counts <- OS_all_counts %>%
  dplyr::filter(era == "Pre-pandemic" &
                  preOperative_infection_status %in% intervals) %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
PP_OS_all_counts <- cbind(sum(PP_OS_all_counts), PP_OS_all_counts)
colnames(PP_OS_all_counts) <- colnames(COVIDSurg_counts)
# In pre-pandemic era, there should not be any instances of test results. Therefore,
# there should not be any instances counted in the intervals. Any counts within
# intervals are data quality issues and are expected to be low. These erroneous 
# data will be imputed with the expected NA.
if(sum(PP_OS_all_counts[2:ncol(PP_OS_all_counts)]) > 0)
{
  message(paste0("\nThere are unexpected counts of patients with test results ",
  "for SARS-CoV-2 in the pre-pandemic era.\nThis is likely a data qaulity issue.\n",
  "Please, review the input data.\n\n"))
  }
PP_OS_all_counts[2:ncol(PP_OS_all_counts)] <- NA
## ## # Pandemic no vaccines, OpenSAFELY data, no-cancer patients.
PNV_OS_all_counts <- OS_all_counts %>%
  dplyr::filter(era == "Pandemic no vaccine" &
                  preOperative_infection_status %in% intervals) %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
PNV_OS_all_counts <- cbind(sum(PNV_OS_all_counts), PNV_OS_all_counts)
colnames(PNV_OS_all_counts) <- colnames(COVIDSurg_counts)
## ## # COVIDSurg data collection period, OpenSAFELY data, no-cancer patients.
CSP_OS_all_counts <- OS_all_counts %>%
  dplyr::filter(era == "COVIDSurg data collection period" &
                  preOperative_infection_status %in% intervals) %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
CSP_OS_all_counts <- cbind(sum(CSP_OS_all_counts), CSP_OS_all_counts)
colnames(CSP_OS_all_counts) <- colnames(COVIDSurg_counts)
## ## # Pandemic with vaccines, OpenSAFELY data, no-cancer patients.
PWV_OS_all_counts <- OS_all_counts %>%
  dplyr::filter(era == "Pandemic with vaccine" &
                  preOperative_infection_status %in% intervals) %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
PWV_OS_all_counts <- cbind(sum(PWV_OS_all_counts), PWV_OS_all_counts)
colnames(PWV_OS_all_counts) <- colnames(COVIDSurg_counts)
# ----

## #  OpenSAFELY data, no-cancer patients.----
OS_NC_counts <-
  data_to_use_NC %>%
    dplyr::group_by(era,
                    preOperative_infection_status) %>%
    dplyr::summarise(n = n())
CSP_OS_NC_counts <- 
  data_to_use_NC %>% dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
  dplyr::group_by(COVIDSurg_data_collection_period, preOperative_infection_status) %>%
  dplyr::summarise(n = n()) %>%
  `colnames<-`(colnames(OS_NC_counts))
OS_NC_counts <-
  dplyr::bind_rows(OS_NC_counts, CSP_OS_NC_counts)
OS_NC_counts <- 
  expand.grid(
    era = 
      c("Error: No surgery", "Pre-pandemic", "Pandemic no vaccine",
        "Pandemic with vaccine", "COVIDSurg data collection period",
        "Not COVIDSurg data collection period"),
    "preOperative_infection_status" = 
      c("Error: Test result after surgery. Check study_definition.",
        "No record of pre-operative SARS-CoV-2 infection",
        "0-2 weeks record of pre-operative SARS-CoV-2 infection",
        "3-4 weeks record of pre-operative SARS-CoV-2 infection",
        "5-6 weeks record of pre-operative SARS-CoV-2 infection",
        ">=7 weeks record of pre-operative SARS-CoV-2 infection")) %>%
  dplyr::full_join(OS_NC_counts) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(list("n" = 0))
rm(CSP_OS_NC_counts)
## ## # Pre-pandemic, OpenSAFELY data, no-cancer patients.
PP_OS_NC_counts <- OS_NC_counts %>%
  dplyr::filter(era == "Pre-pandemic" &
                  preOperative_infection_status %in% intervals) %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
PP_OS_NC_counts <- cbind(sum(PP_OS_NC_counts), PP_OS_NC_counts)
colnames(PP_OS_NC_counts) <- colnames(COVIDSurg_counts)
# In pre-pandemic era, there should not be any instances of test results. Therefore,
# there should not be any instances counted in the intervals. Any counts within
# intervals are data quality issues and are expected to be low. These erroneous 
# data will be imputed with the expected NA.
if(sum(PP_OS_NC_counts[2:ncol(PP_OS_NC_counts)]) > 0)
{
  message(paste0("\nThere are unexpected counts of patients with test results ",
                 "for SARS-CoV-2 in the pre-pandemic era.\nThis is likely a data qaulity issue.\n",
                 "Please, review the input data.\n\n"))
}
PP_OS_NC_counts[2:ncol(PP_OS_NC_counts)] <- NA
## ## # Pandemic no vaccines, OpenSAFELY data, no-cancer patients.
PNV_OS_NC_counts <- OS_NC_counts %>%
  dplyr::filter(era == "Pandemic no vaccine" &
                  preOperative_infection_status %in% intervals) %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
PNV_OS_NC_counts <- cbind(sum(PNV_OS_NC_counts), PNV_OS_NC_counts)
colnames(PNV_OS_NC_counts) <- colnames(COVIDSurg_counts)
## ## # COVIDSurg data collection period, OpenSAFELY data, no-cancer patients.
CSP_OS_NC_counts <- OS_NC_counts %>%
  dplyr::filter(era == "COVIDSurg data collection period" &
                  preOperative_infection_status %in% intervals) %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
CSP_OS_NC_counts <- cbind(sum(CSP_OS_NC_counts), CSP_OS_NC_counts)
colnames(CSP_OS_NC_counts) <- colnames(COVIDSurg_counts)
## ## # Pandemic with vaccines, OpenSAFELY data, no-cancer patients.
PWV_OS_NC_counts <- OS_NC_counts %>%
  dplyr::filter(era == "Pandemic with vaccine" &
                  preOperative_infection_status %in% intervals) %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
PWV_OS_NC_counts <- cbind(sum(PWV_OS_NC_counts), PWV_OS_NC_counts)
colnames(PWV_OS_NC_counts) <- colnames(COVIDSurg_counts)
# ----

## #  OpenSAFELY data, cancer within 3 months patients.----
OS_C_within3m_counts <-
  data_to_use_C_within3m %>%
  dplyr::group_by(era,
                  preOperative_infection_status) %>%
  dplyr::summarise(n = n())
CSP_OS_C_within3m_counts <- 
  data_to_use_C_within3m %>% dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
  dplyr::group_by(COVIDSurg_data_collection_period, preOperative_infection_status) %>%
  dplyr::summarise(n = n()) %>%
  `colnames<-`(colnames(OS_C_within3m_counts))
OS_C_within3m_counts <-
  dplyr::bind_rows(OS_C_within3m_counts, CSP_OS_C_within3m_counts)
OS_C_within3m_counts <- 
  expand.grid(
    era = 
      c("Error: No surgery", "Pre-pandemic", "Pandemic no vaccine",
        "Pandemic with vaccine", "COVIDSurg data collection period",
        "Not COVIDSurg data collection period"),
    "preOperative_infection_status" = 
      c("Error: Test result after surgery. Check study_definition.",
        "No record of pre-operative SARS-CoV-2 infection",
        "0-2 weeks record of pre-operative SARS-CoV-2 infection",
        "3-4 weeks record of pre-operative SARS-CoV-2 infection",
        "5-6 weeks record of pre-operative SARS-CoV-2 infection",
        ">=7 weeks record of pre-operative SARS-CoV-2 infection")) %>%
  dplyr::full_join(OS_C_within3m_counts) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(list("n" = 0))
rm(CSP_OS_C_within3m_counts)
## ## # Pre-pandemic, OpenSAFELY data, no-cancer patients.
PP_OS_C_within3m_counts <- OS_C_within3m_counts %>%
  dplyr::filter(era == "Pre-pandemic" &
                  preOperative_infection_status %in% intervals) %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
PP_OS_C_within3m_counts <- cbind(sum(PP_OS_C_within3m_counts), PP_OS_C_within3m_counts)
colnames(PP_OS_C_within3m_counts) <- colnames(COVIDSurg_counts)
# In pre-pandemic era, there should not be any instances of test results. Therefore,
# there should not be any instances counted in the intervals. Any counts within
# intervals are data quality issues and are expected to be low. These erroneous 
# data will be imputed with the expected NA.
if(sum(PP_OS_C_within3m_counts[2:ncol(PP_OS_C_within3m_counts)]) > 0)
{
  message(paste0("\nThere are unexpected counts of patients with test results ",
                 "for SARS-CoV-2 in the pre-pandemic era.\nThis is likely a data qaulity issue.\n",
                 "Please, review the input data.\n\n"))
}
PP_OS_C_within3m_counts[2:ncol(PP_OS_C_within3m_counts)] <- NA
## ## # Pandemic no vaccines, OpenSAFELY data, no-cancer patients.
PNV_OS_C_within3m_counts <- OS_C_within3m_counts %>%
  dplyr::filter(era == "Pandemic no vaccine" &
                  preOperative_infection_status %in% intervals) %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
PNV_OS_C_within3m_counts <- cbind(sum(PNV_OS_C_within3m_counts), PNV_OS_C_within3m_counts)
colnames(PNV_OS_C_within3m_counts) <- colnames(COVIDSurg_counts)
## ## # COVIDSurg data collection period, OpenSAFELY data, no-cancer patients.
CSP_OS_C_within3m_counts <- OS_C_within3m_counts %>%
  dplyr::filter(era == "COVIDSurg data collection period" &
                  preOperative_infection_status %in% intervals) %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
CSP_OS_C_within3m_counts <- cbind(sum(CSP_OS_C_within3m_counts), CSP_OS_C_within3m_counts)
colnames(CSP_OS_C_within3m_counts) <- colnames(COVIDSurg_counts)
## ## # Pandemic with vaccines, OpenSAFELY data, no-cancer patients.
PWV_OS_C_within3m_counts <- OS_C_within3m_counts %>%
  dplyr::filter(era == "Pandemic with vaccine" &
                  preOperative_infection_status %in% intervals) %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
PWV_OS_C_within3m_counts <- cbind(sum(PWV_OS_C_within3m_counts), PWV_OS_C_within3m_counts)
colnames(PWV_OS_C_within3m_counts) <- colnames(COVIDSurg_counts)
# ----

## #  OpenSAFELY data, cancer outwith 3 months patients.----
OS_C_outwith3m_counts <-
  data_to_use_C_outwith3m %>%
  dplyr::group_by(era,
                  preOperative_infection_status) %>%
  dplyr::summarise(n = n())
CSP_OS_C_outwith3m_counts <- 
  data_to_use_C_outwith3m %>% dplyr::filter(COVIDSurg_data_collection_period != "Error: No surgery") %>%
  dplyr::group_by(COVIDSurg_data_collection_period, preOperative_infection_status) %>%
  dplyr::summarise(n = n()) %>%
  `colnames<-`(colnames(OS_C_outwith3m_counts))
OS_C_outwith3m_counts <-
  dplyr::bind_rows(OS_C_outwith3m_counts, CSP_OS_C_outwith3m_counts)
OS_C_outwith3m_counts <- 
  expand.grid(
    era = 
      c("Error: No surgery", "Pre-pandemic", "Pandemic no vaccine",
        "Pandemic with vaccine", "COVIDSurg data collection period",
        "Not COVIDSurg data collection period"),
    "preOperative_infection_status" = 
      c("Error: Test result after surgery. Check study_definition.",
        "No record of pre-operative SARS-CoV-2 infection",
        "0-2 weeks record of pre-operative SARS-CoV-2 infection",
        "3-4 weeks record of pre-operative SARS-CoV-2 infection",
        "5-6 weeks record of pre-operative SARS-CoV-2 infection",
        ">=7 weeks record of pre-operative SARS-CoV-2 infection")) %>%
  dplyr::full_join(OS_C_outwith3m_counts) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(list("n" = 0))
rm(CSP_OS_C_outwith3m_counts)
## ## # Pre-pandemic, OpenSAFELY data, no-cancer patients.
PP_OS_C_outwith3m_counts <- OS_C_outwith3m_counts %>%
  dplyr::filter(era == "Pre-pandemic" &
                  preOperative_infection_status %in% intervals) %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
PP_OS_C_outwith3m_counts <- cbind(sum(PP_OS_C_outwith3m_counts), PP_OS_C_outwith3m_counts)
colnames(PP_OS_C_outwith3m_counts) <- colnames(COVIDSurg_counts)
# In pre-pandemic era, there should not be any instances of test results. Therefore,
# there should not be any instances counted in the intervals. Any counts within
# intervals are data quality issues and are expected to be low. These erroneous 
# data will be imputed with the expected NA.
if(sum(PP_OS_C_outwith3m_counts[2:ncol(PP_OS_C_outwith3m_counts)]) > 0)
{
  message(paste0("\nThere are unexpected counts of patients with test results ",
                 "for SARS-CoV-2 in the pre-pandemic era.\nThis is likely a data qaulity issue.\n",
                 "Please, review the input data.\n\n"))
}
PP_OS_C_outwith3m_counts[2:ncol(PP_OS_C_outwith3m_counts)] <- NA
## ## # Pandemic no vaccines, OpenSAFELY data, no-cancer patients.
PNV_OS_C_outwith3m_counts <- OS_C_outwith3m_counts %>%
  dplyr::filter(era == "Pandemic no vaccine" &
                  preOperative_infection_status %in% intervals) %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
PNV_OS_C_outwith3m_counts <- cbind(sum(PNV_OS_C_outwith3m_counts), PNV_OS_C_outwith3m_counts)
colnames(PNV_OS_C_outwith3m_counts) <- colnames(COVIDSurg_counts)
## ## # COVIDSurg data collection period, OpenSAFELY data, no-cancer patients.
CSP_OS_C_outwith3m_counts <- OS_C_outwith3m_counts %>%
  dplyr::filter(era == "COVIDSurg data collection period" &
                  preOperative_infection_status %in% intervals) %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
CSP_OS_C_outwith3m_counts <- cbind(sum(CSP_OS_C_outwith3m_counts), CSP_OS_C_outwith3m_counts)
colnames(CSP_OS_C_outwith3m_counts) <- colnames(COVIDSurg_counts)
## ## # Pandemic with vaccines, OpenSAFELY data, no-cancer patients.
PWV_OS_C_outwith3m_counts <- OS_C_outwith3m_counts %>%
  dplyr::filter(era == "Pandemic with vaccine" &
                  preOperative_infection_status %in% intervals) %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
PWV_OS_C_outwith3m_counts <- cbind(sum(PWV_OS_C_outwith3m_counts), PWV_OS_C_outwith3m_counts)
colnames(PWV_OS_C_outwith3m_counts) <- colnames(COVIDSurg_counts)
# ----

##################################################################
## Counts of 30-day post-operative mortality, across intervals. ##
## OpenSAFELY data, all surgery patients.                         ##
##################################################################
# ----
# Count of patients in each of the categories for pre-operative infection
# status (stratified by surgery era; see above) also stratified by whether
# or not the patient died within 30 days of their surgery:
#   1. "Alive within 30 days post-operation"
#   2. "Dead within 30 days post-operation" 
#
## # OpenSAFELY data, all surgery patients.
OS_all_mortality <- 
  data_to_use_all %>% dplyr::group_by(era, postOp_mortality_30day) %>%
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
OS_all_mortality <- 
  expand.grid(
    era = 
      c("Error: No surgery", "Pre-pandemic", "Pandemic no vaccine",
        "Pandemic with vaccine", "COVIDSurg data collection period",
        "Not COVIDSurg data collection period"),
    postOp_mortality_30day = 
      c("Alive within 30 days post-operation",
        "Dead within 30 days post-operation",
        "Error: Surgery after death",
        "No death recorded",
        "No surgery recorded",
        "Missing")) %>%
  dplyr::full_join(OS_all_mortality) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(list("n_per_group" = 0,
                         "n_infection_none" = 0,
                         "n_infection_0to2wk" = 0,
                         "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0,
                         "n_infection_7wk" = 0))
## ## # Pre-pandemic, OpenSAFELY data, all surgery patients.----
PP_OS_all_mortality <- OS_all_mortality %>%
  dplyr::filter(era == "Pre-pandemic" &
                  postOp_mortality_30day=="Dead within 30 days post-operation") %>%
  select(-c(era, postOp_mortality_30day)) %>% dplyr::ungroup()
## ## ## # Counts of 30-day post-operative mortality, across intervals.
PP_OS_all_mortality_intervals <- data.frame(
  d_total = 0,
  pct_total = 0,
  d_infection_none = NA,
  pct_infection_none = NA,
  d_infection_0to2wk = NA,
  pct_infection_0to2wk = NA,
  d_infection_3to4wk = NA,
  pct_infection_3to4wk = NA,
  d_infection_5to6wk = NA,
  pct_infection_5to6wk = NA,
  d_infection_7wk = NA,
  pct_infection_7wk = NA
)
PP_OS_all_mortality_intervals['d_total'] <- PP_OS_all_mortality['n_per_group']
## ## ## # Percentages of 30-day post-operative mortality, across intervals.
PP_OS_all_mortality_intervals['pct_total'] <- 
  (PP_OS_all_mortality_intervals['d_total'] / PP_OS_all_counts['n_total']) * 100
PP_OS_all_mortality_intervals <- 
  tidyr::replace_na(PP_OS_all_mortality_intervals, list("pct_total" = 0))
# ----

## ## # Pandemic no vaccines, OpenSAFELY data, all surgery patients.----
PNV_OS_all_mortality <- OS_all_mortality %>%
  dplyr::filter(era == "Pandemic no vaccine" &
                  postOp_mortality_30day=="Dead within 30 days post-operation") %>%
  select(-c(era, postOp_mortality_30day)) %>% dplyr::ungroup()
## ## ## # Counts of 30-day post-operative mortality, across intervals.
PNV_OS_all_mortality_intervals <- data.frame(
  d_total = 0,
  pct_total = 0,
  d_infection_none = 0,
  pct_infection_none = 0,
  d_infection_0to2wk = 0,
  pct_infection_0to2wk = 0,
  d_infection_3to4wk = 0,
  pct_infection_3to4wk = 0,
  d_infection_5to6wk = 0,
  pct_infection_5to6wk = 0,
  d_infection_7wk = 0,
  pct_infection_7wk = 0
)
PNV_OS_all_mortality_intervals[seq(1,12,2)] <- PNV_OS_all_mortality
## ## ## # Percentages of 30-day post-operative mortality, across intervals.
PNV_OS_all_mortality_intervals[seq(2,12,2)] <- 
  (PNV_OS_all_mortality_intervals[seq(1,12,2)] / PNV_OS_all_counts) * 100
PNV_OS_all_mortality_intervals <-
  tidyr::replace_na(PNV_OS_all_mortality_intervals,
                    list("pct_total" = 0,
                         "pct_infection_none" = 0,
                         "pct_infection_0to2wk" = 0,
                         "pct_infection_3to4wk" = 0,
                         "pct_infection_5to6wk" = 0,
                         "pct_infection_7wk" = 0))
# ----

## ## # COVIDSurg data collection period, OpenSAFELY data, all surgery patients.----
CSP_OS_all_mortality <- OS_all_mortality %>%
  dplyr::filter(era == "COVIDSurg data collection period" &
                  postOp_mortality_30day=="Dead within 30 days post-operation") %>%
  select(-c(era, postOp_mortality_30day)) %>% dplyr::ungroup()
## ## ## # Counts of 30-day post-operative mortality, across intervals.
CSP_OS_all_mortality_intervals <- data.frame(
  d_total = 0,
  pct_total = 0,
  d_infection_none = 0,
  pct_infection_none = 0,
  d_infection_0to2wk = 0,
  pct_infection_0to2wk = 0,
  d_infection_3to4wk = 0,
  pct_infection_3to4wk = 0,
  d_infection_5to6wk = 0,
  pct_infection_5to6wk = 0,
  d_infection_7wk = 0,
  pct_infection_7wk = 0
)
CSP_OS_all_mortality_intervals[seq(1,12,2)] <- CSP_OS_all_mortality
## ## ## # Percentages of 30-day post-operative mortality, across intervals.
CSP_OS_all_mortality_intervals[seq(2,12,2)] <- 
  (CSP_OS_all_mortality_intervals[seq(1,12,2)] / CSP_OS_all_counts) * 100
CSP_OS_all_mortality_intervals <-
  tidyr::replace_na(CSP_OS_all_mortality_intervals,
                    list("pct_total" = 0,
                         "pct_infection_none" = 0,
                         "pct_infection_0to2wk" = 0,
                         "pct_infection_3to4wk" = 0,
                         "pct_infection_5to6wk" = 0,
                         "pct_infection_7wk" = 0))
# ----

## ## # Pandemic with vaccines, OpenSAFELY data, all surgery patients.----
PWV_OS_all_mortality <- OS_all_mortality %>%
  dplyr::filter(era == "Pandemic with vaccine" &
                  postOp_mortality_30day=="Dead within 30 days post-operation") %>%
  select(-c(era, postOp_mortality_30day)) %>% dplyr::ungroup()
## ## ## # Counts of 30-day post-operative mortality, across intervals.
PWV_OS_all_mortality_intervals <- data.frame(
  d_total = 0,
  pct_total = 0,
  d_infection_none = 0,
  pct_infection_none = 0,
  d_infection_0to2wk = 0,
  pct_infection_0to2wk = 0,
  d_infection_3to4wk = 0,
  pct_infection_3to4wk = 0,
  d_infection_5to6wk = 0,
  pct_infection_5to6wk = 0,
  d_infection_7wk = 0,
  pct_infection_7wk = 0
)
PWV_OS_all_mortality_intervals[seq(1,12,2)] <- PWV_OS_all_mortality
## ## ## # Percentages of 30-day post-operative mortality, across intervals.
PWV_OS_all_mortality_intervals[seq(2,12,2)] <- 
  (PWV_OS_all_mortality_intervals[seq(1,12,2)] / PWV_OS_all_counts) * 100
PWV_OS_all_mortality_intervals <-
  tidyr::replace_na(PWV_OS_all_mortality_intervals,
                    list("pct_total" = 0,
                         "pct_infection_none" = 0,
                         "pct_infection_0to2wk" = 0,
                         "pct_infection_3to4wk" = 0,
                         "pct_infection_5to6wk" = 0,
                         "pct_infection_7wk" = 0))
# ----

##################################################################
## Counts of 30-day post-operative mortality, across intervals. ##
## OpenSAFELY data, no-cancer patients.                         ##
##################################################################
# ----
# Count of patients in each of the categories for pre-operative infection
# status (stratified by surgery era; see above) also stratified by whether
# or not the patient died within 30 days of their surgery:
#   1. "Alive within 30 days post-operation"
#   2. "Dead within 30 days post-operation" 
#
## # OpenSAFELY data, no-cancer patients.
OS_NC_mortality <- 
  data_to_use_NC %>% dplyr::group_by(era, postOp_mortality_30day) %>%
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
OS_NC_mortality <- 
  expand.grid(
    era = 
      c("Error: No surgery", "Pre-pandemic", "Pandemic no vaccine",
        "Pandemic with vaccine", "COVIDSurg data collection period",
        "Not COVIDSurg data collection period"),
    postOp_mortality_30day = 
      c("Alive within 30 days post-operation",
        "Dead within 30 days post-operation",
        "Error: Surgery after death",
        "No death recorded",
        "No surgery recorded",
        "Missing")) %>%
  dplyr::full_join(OS_NC_mortality) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(list("n_per_group" = 0,
                         "n_infection_none" = 0,
                         "n_infection_0to2wk" = 0,
                         "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0,
                         "n_infection_7wk" = 0))
## ## # Pre-pandemic, OpenSAFELY data, no-cancer patients.----
PP_OS_NC_mortality <- OS_NC_mortality %>%
  dplyr::filter(era == "Pre-pandemic" &
                  postOp_mortality_30day=="Dead within 30 days post-operation") %>%
  select(-c(era, postOp_mortality_30day)) %>% dplyr::ungroup()
## ## ## # Counts of 30-day post-operative mortality, across intervals.
PP_OS_NC_mortality_intervals <- data.frame(
  d_total = 0,
  pct_total = 0,
  d_infection_none = NA,
  pct_infection_none = NA,
  d_infection_0to2wk = NA,
  pct_infection_0to2wk = NA,
  d_infection_3to4wk = NA,
  pct_infection_3to4wk = NA,
  d_infection_5to6wk = NA,
  pct_infection_5to6wk = NA,
  d_infection_7wk = NA,
  pct_infection_7wk = NA
)
PP_OS_NC_mortality_intervals['d_total'] <- PP_OS_NC_mortality['n_per_group']
## ## ## # Percentages of 30-day post-operative mortality, across intervals.
PP_OS_NC_mortality_intervals['pct_total'] <- 
  (PP_OS_NC_mortality_intervals['d_total'] / PP_OS_NC_counts['n_total']) * 100
PP_OS_NC_mortality_intervals <- 
  tidyr::replace_na(PP_OS_NC_mortality_intervals, list("pct_total" = 0))
# ----

## ## # Pandemic no vaccines, OpenSAFELY data, no-cancer patients.----
PNV_OS_NC_mortality <- OS_NC_mortality %>%
  dplyr::filter(era == "Pandemic no vaccine" &
                  postOp_mortality_30day=="Dead within 30 days post-operation") %>%
  select(-c(era, postOp_mortality_30day)) %>% dplyr::ungroup()
## ## ## # Counts of 30-day post-operative mortality, across intervals.
PNV_OS_NC_mortality_intervals <- data.frame(
  d_total = 0,
  pct_total = 0,
  d_infection_none = 0,
  pct_infection_none = 0,
  d_infection_0to2wk = 0,
  pct_infection_0to2wk = 0,
  d_infection_3to4wk = 0,
  pct_infection_3to4wk = 0,
  d_infection_5to6wk = 0,
  pct_infection_5to6wk = 0,
  d_infection_7wk = 0,
  pct_infection_7wk = 0
)
PNV_OS_NC_mortality_intervals[seq(1,12,2)] <- PNV_OS_NC_mortality
## ## ## # Percentages of 30-day post-operative mortality, across intervals.
PNV_OS_NC_mortality_intervals[seq(2,12,2)] <- 
  (PNV_OS_NC_mortality_intervals[seq(1,12,2)] / PNV_OS_NC_counts) * 100
PNV_OS_NC_mortality_intervals <-
  tidyr::replace_na(PNV_OS_NC_mortality_intervals,
                    list("pct_total" = 0,
                         "pct_infection_none" = 0,
                         "pct_infection_0to2wk" = 0,
                         "pct_infection_3to4wk" = 0,
                         "pct_infection_5to6wk" = 0,
                         "pct_infection_7wk" = 0))
# ----

## ## # COVIDSurg data collection period, OpenSAFELY data, no-cancer patients.----
CSP_OS_NC_mortality <- OS_NC_mortality %>%
  dplyr::filter(era == "COVIDSurg data collection period" &
                  postOp_mortality_30day=="Dead within 30 days post-operation") %>%
  select(-c(era, postOp_mortality_30day)) %>% dplyr::ungroup()
## ## ## # Counts of 30-day post-operative mortality, across intervals.
CSP_OS_NC_mortality_intervals <- data.frame(
  d_total = 0,
  pct_total = 0,
  d_infection_none = 0,
  pct_infection_none = 0,
  d_infection_0to2wk = 0,
  pct_infection_0to2wk = 0,
  d_infection_3to4wk = 0,
  pct_infection_3to4wk = 0,
  d_infection_5to6wk = 0,
  pct_infection_5to6wk = 0,
  d_infection_7wk = 0,
  pct_infection_7wk = 0
)
CSP_OS_NC_mortality_intervals[seq(1,12,2)] <- CSP_OS_NC_mortality
## ## ## # Percentages of 30-day post-operative mortality, across intervals.
CSP_OS_NC_mortality_intervals[seq(2,12,2)] <- 
  (CSP_OS_NC_mortality_intervals[seq(1,12,2)] / CSP_OS_NC_counts) * 100
CSP_OS_NC_mortality_intervals <-
  tidyr::replace_na(CSP_OS_NC_mortality_intervals,
                    list("pct_total" = 0,
                         "pct_infection_none" = 0,
                         "pct_infection_0to2wk" = 0,
                         "pct_infection_3to4wk" = 0,
                         "pct_infection_5to6wk" = 0,
                         "pct_infection_7wk" = 0))
# ----

## ## # Pandemic with vaccines, OpenSAFELY data, no-cancer patients.----
PWV_OS_NC_mortality <- OS_NC_mortality %>%
  dplyr::filter(era == "Pandemic with vaccine" &
                  postOp_mortality_30day=="Dead within 30 days post-operation") %>%
  select(-c(era, postOp_mortality_30day)) %>% dplyr::ungroup()
## ## ## # Counts of 30-day post-operative mortality, across intervals.
PWV_OS_NC_mortality_intervals <- data.frame(
  d_total = 0,
  pct_total = 0,
  d_infection_none = 0,
  pct_infection_none = 0,
  d_infection_0to2wk = 0,
  pct_infection_0to2wk = 0,
  d_infection_3to4wk = 0,
  pct_infection_3to4wk = 0,
  d_infection_5to6wk = 0,
  pct_infection_5to6wk = 0,
  d_infection_7wk = 0,
  pct_infection_7wk = 0
)
PWV_OS_NC_mortality_intervals[seq(1,12,2)] <- PWV_OS_NC_mortality
## ## ## # Percentages of 30-day post-operative mortality, across intervals.
PWV_OS_NC_mortality_intervals[seq(2,12,2)] <- 
  (PWV_OS_NC_mortality_intervals[seq(1,12,2)] / PWV_OS_NC_counts) * 100
PWV_OS_NC_mortality_intervals <-
  tidyr::replace_na(PWV_OS_NC_mortality_intervals,
                    list("pct_total" = 0,
                         "pct_infection_none" = 0,
                         "pct_infection_0to2wk" = 0,
                         "pct_infection_3to4wk" = 0,
                         "pct_infection_5to6wk" = 0,
                         "pct_infection_7wk" = 0))
# ----


##################################################################
## Counts of 30-day post-operative mortality, across intervals. ##
## OpenSAFELY data, cancer within 3 months patients.            ##
##################################################################
# ----
# Count of patients in each of the categories for pre-operative infection
# status (stratified by surgery era; see above) also stratified by whether
# or not the patient died within 30 days of their surgery:
#   1. "Alive within 30 days post-operation"
#   2. "Dead within 30 days post-operation" 
#
## # OpenSAFELY data, cancer patients.
OS_C_within3m_mortality <- 
  data_to_use_C_within3m %>% dplyr::group_by(era, postOp_mortality_30day) %>%
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
OS_C_within3m_mortality <- 
  expand.grid(
    era = 
      c("Error: No surgery", "Pre-pandemic", "Pandemic no vaccine",
        "Pandemic with vaccine", "COVIDSurg data collection period",
        "Not COVIDSurg data collection period"),
    postOp_mortality_30day = 
      c("Alive within 30 days post-operation",
        "Dead within 30 days post-operation",
        "Error: Surgery after death",
        "No death recorded",
        "No surgery recorded",
        "Missing")) %>%
  dplyr::full_join(OS_C_within3m_mortality) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(list("n_per_group" = 0,
                         "n_infection_none" = 0,
                         "n_infection_0to2wk" = 0,
                         "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0,
                         "n_infection_7wk" = 0))
## ## # Pre-pandemic, OpenSAFELY data, no-cancer patients.----
PP_OS_C_within3m_mortality <- OS_C_within3m_mortality %>%
  dplyr::filter(era == "Pre-pandemic" &
                  postOp_mortality_30day=="Dead within 30 days post-operation") %>%
  select(-c(era, postOp_mortality_30day)) %>% dplyr::ungroup()
## ## ## # Counts of 30-day post-operative mortality, across intervals.
PP_OS_C_within3m_mortality_intervals <- data.frame(
  d_total = 0,
  pct_total = 0,
  d_infection_none = NA,
  pct_infection_none = NA,
  d_infection_0to2wk = NA,
  pct_infection_0to2wk = NA,
  d_infection_3to4wk = NA,
  pct_infection_3to4wk = NA,
  d_infection_5to6wk = NA,
  pct_infection_5to6wk = NA,
  d_infection_7wk = NA,
  pct_infection_7wk = NA
)
PP_OS_C_within3m_mortality_intervals['d_total'] <- PP_OS_C_within3m_mortality['n_per_group']
## ## ## # Percentages of 30-day post-operative mortality, across intervals.
PP_OS_C_within3m_mortality_intervals['pct_total'] <- 
  (PP_OS_C_within3m_mortality_intervals['d_total'] / PP_OS_C_within3m_counts['n_total']) * 100
PP_OS_C_within3m_mortality_intervals <- 
  tidyr::replace_na(PP_OS_C_within3m_mortality_intervals, list("pct_total" = 0))
# ----

## ## # Pandemic no vaccines, OpenSAFELY data, no-cancer patients.----
PNV_OS_C_within3m_mortality <- OS_C_within3m_mortality %>%
  dplyr::filter(era == "Pandemic no vaccine" &
                  postOp_mortality_30day=="Dead within 30 days post-operation") %>%
  select(-c(era, postOp_mortality_30day)) %>% dplyr::ungroup()
## ## ## # Counts of 30-day post-operative mortality, across intervals.
PNV_OS_C_within3m_mortality_intervals <- data.frame(
  d_total = 0,
  pct_total = 0,
  d_infection_none = 0,
  pct_infection_none = 0,
  d_infection_0to2wk = 0,
  pct_infection_0to2wk = 0,
  d_infection_3to4wk = 0,
  pct_infection_3to4wk = 0,
  d_infection_5to6wk = 0,
  pct_infection_5to6wk = 0,
  d_infection_7wk = 0,
  pct_infection_7wk = 0
)
PNV_OS_C_within3m_mortality_intervals[seq(1,12,2)] <- PNV_OS_C_within3m_mortality
## ## ## # Percentages of 30-day post-operative mortality, across intervals.
PNV_OS_C_within3m_mortality_intervals[seq(2,12,2)] <- 
  (PNV_OS_C_within3m_mortality_intervals[seq(1,12,2)] / PNV_OS_C_within3m_counts) * 100
PNV_OS_C_within3m_mortality_intervals <-
  tidyr::replace_na(PNV_OS_C_within3m_mortality_intervals,
                    list("pct_total" = 0,
                         "pct_infection_none" = 0,
                         "pct_infection_0to2wk" = 0,
                         "pct_infection_3to4wk" = 0,
                         "pct_infection_5to6wk" = 0,
                         "pct_infection_7wk" = 0))
# ----

## ## # COVIDSurg data collection period, OpenSAFELY data, no-cancer patients.----
CSP_OS_C_within3m_mortality <- OS_C_within3m_mortality %>%
  dplyr::filter(era == "COVIDSurg data collection period" &
                  postOp_mortality_30day=="Dead within 30 days post-operation") %>%
  select(-c(era, postOp_mortality_30day)) %>% dplyr::ungroup()
## ## ## # Counts of 30-day post-operative mortality, across intervals.
CSP_OS_C_within3m_mortality_intervals <- data.frame(
  d_total = 0,
  pct_total = 0,
  d_infection_none = 0,
  pct_infection_none = 0,
  d_infection_0to2wk = 0,
  pct_infection_0to2wk = 0,
  d_infection_3to4wk = 0,
  pct_infection_3to4wk = 0,
  d_infection_5to6wk = 0,
  pct_infection_5to6wk = 0,
  d_infection_7wk = 0,
  pct_infection_7wk = 0
)
CSP_OS_C_within3m_mortality_intervals[seq(1,12,2)] <- CSP_OS_C_within3m_mortality
## ## ## # Percentages of 30-day post-operative mortality, across intervals.
CSP_OS_C_within3m_mortality_intervals[seq(2,12,2)] <- 
  (CSP_OS_C_within3m_mortality_intervals[seq(1,12,2)] / CSP_OS_C_within3m_counts) * 100
CSP_OS_C_within3m_mortality_intervals <-
  tidyr::replace_na(CSP_OS_C_within3m_mortality_intervals,
                    list("pct_total" = 0,
                         "pct_infection_none" = 0,
                         "pct_infection_0to2wk" = 0,
                         "pct_infection_3to4wk" = 0,
                         "pct_infection_5to6wk" = 0,
                         "pct_infection_7wk" = 0))
# ----

## ## # Pandemic with vaccines, OpenSAFELY data, no-cancer patients.----
PWV_OS_C_within3m_mortality <- OS_C_within3m_mortality %>%
  dplyr::filter(era == "Pandemic with vaccine" &
                  postOp_mortality_30day=="Dead within 30 days post-operation") %>%
  select(-c(era, postOp_mortality_30day)) %>% dplyr::ungroup()
## ## ## # Counts of 30-day post-operative mortality, across intervals.
PWV_OS_C_within3m_mortality_intervals <- data.frame(
  d_total = 0,
  pct_total = 0,
  d_infection_none = 0,
  pct_infection_none = 0,
  d_infection_0to2wk = 0,
  pct_infection_0to2wk = 0,
  d_infection_3to4wk = 0,
  pct_infection_3to4wk = 0,
  d_infection_5to6wk = 0,
  pct_infection_5to6wk = 0,
  d_infection_7wk = 0,
  pct_infection_7wk = 0
)
PWV_OS_C_within3m_mortality_intervals[seq(1,12,2)] <- PWV_OS_C_within3m_mortality
## ## ## # Percentages of 30-day post-operative mortality, across intervals.
PWV_OS_C_within3m_mortality_intervals[seq(2,12,2)] <- 
  (PWV_OS_C_within3m_mortality_intervals[seq(1,12,2)] / PWV_OS_C_within3m_counts) * 100
PWV_OS_C_within3m_mortality_intervals <-
  tidyr::replace_na(PWV_OS_C_within3m_mortality_intervals,
                    list("pct_total" = 0,
                         "pct_infection_none" = 0,
                         "pct_infection_0to2wk" = 0,
                         "pct_infection_3to4wk" = 0,
                         "pct_infection_5to6wk" = 0,
                         "pct_infection_7wk" = 0))
# ----

##################################################################
## Counts of 30-day post-operative mortality, across intervals. ##
## OpenSAFELY data, cancer outwith 3 months patients.           ##
##################################################################
# ----
# Count of patients in each of the categories for pre-operative infection
# status (stratified by surgery era; see above) also stratified by whether
# or not the patient died outwith 30 days of their surgery:
#   1. "Alive outwith 30 days post-operation"
#   2. "Dead outwith 30 days post-operation" 
#
## # OpenSAFELY data, cancer patients.
OS_C_outwith3m_mortality <- 
  data_to_use_C_outwith3m %>% dplyr::group_by(era, postOp_mortality_30day) %>%
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
OS_C_outwith3m_mortality <- 
  expand.grid(
    era = 
      c("Error: No surgery", "Pre-pandemic", "Pandemic no vaccine",
        "Pandemic with vaccine", "COVIDSurg data collection period",
        "Not COVIDSurg data collection period"),
    postOp_mortality_30day = 
      c("Alive outwith 30 days post-operation",
        "Dead outwith 30 days post-operation",
        "Error: Surgery after death",
        "No death recorded",
        "No surgery recorded",
        "Missing")) %>%
  dplyr::full_join(OS_C_outwith3m_mortality) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(list("n_per_group" = 0,
                         "n_infection_none" = 0,
                         "n_infection_0to2wk" = 0,
                         "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0,
                         "n_infection_7wk" = 0))
## ## # Pre-pandemic, OpenSAFELY data, no-cancer patients.----
PP_OS_C_outwith3m_mortality <- OS_C_outwith3m_mortality %>%
  dplyr::filter(era == "Pre-pandemic" &
                  postOp_mortality_30day=="Dead outwith 30 days post-operation") %>%
  select(-c(era, postOp_mortality_30day)) %>% dplyr::ungroup()
## ## ## # Counts of 30-day post-operative mortality, across intervals.
PP_OS_C_outwith3m_mortality_intervals <- data.frame(
  d_total = 0,
  pct_total = 0,
  d_infection_none = NA,
  pct_infection_none = NA,
  d_infection_0to2wk = NA,
  pct_infection_0to2wk = NA,
  d_infection_3to4wk = NA,
  pct_infection_3to4wk = NA,
  d_infection_5to6wk = NA,
  pct_infection_5to6wk = NA,
  d_infection_7wk = NA,
  pct_infection_7wk = NA
)
PP_OS_C_outwith3m_mortality_intervals['d_total'] <- PP_OS_C_outwith3m_mortality['n_per_group']
## ## ## # Percentages of 30-day post-operative mortality, across intervals.
PP_OS_C_outwith3m_mortality_intervals['pct_total'] <- 
  (PP_OS_C_outwith3m_mortality_intervals['d_total'] / PP_OS_C_outwith3m_counts['n_total']) * 100
PP_OS_C_outwith3m_mortality_intervals <- 
  tidyr::replace_na(PP_OS_C_outwith3m_mortality_intervals, list("pct_total" = 0))
# ----

## ## # Pandemic no vaccines, OpenSAFELY data, no-cancer patients.----
PNV_OS_C_outwith3m_mortality <- OS_C_outwith3m_mortality %>%
  dplyr::filter(era == "Pandemic no vaccine" &
                  postOp_mortality_30day=="Dead outwith 30 days post-operation") %>%
  select(-c(era, postOp_mortality_30day)) %>% dplyr::ungroup()
## ## ## # Counts of 30-day post-operative mortality, across intervals.
PNV_OS_C_outwith3m_mortality_intervals <- data.frame(
  d_total = 0,
  pct_total = 0,
  d_infection_none = 0,
  pct_infection_none = 0,
  d_infection_0to2wk = 0,
  pct_infection_0to2wk = 0,
  d_infection_3to4wk = 0,
  pct_infection_3to4wk = 0,
  d_infection_5to6wk = 0,
  pct_infection_5to6wk = 0,
  d_infection_7wk = 0,
  pct_infection_7wk = 0
)
PNV_OS_C_outwith3m_mortality_intervals[seq(1,12,2)] <- PNV_OS_C_outwith3m_mortality
## ## ## # Percentages of 30-day post-operative mortality, across intervals.
PNV_OS_C_outwith3m_mortality_intervals[seq(2,12,2)] <- 
  (PNV_OS_C_outwith3m_mortality_intervals[seq(1,12,2)] / PNV_OS_C_outwith3m_counts) * 100
PNV_OS_C_outwith3m_mortality_intervals <-
  tidyr::replace_na(PNV_OS_C_outwith3m_mortality_intervals,
                    list("pct_total" = 0,
                         "pct_infection_none" = 0,
                         "pct_infection_0to2wk" = 0,
                         "pct_infection_3to4wk" = 0,
                         "pct_infection_5to6wk" = 0,
                         "pct_infection_7wk" = 0))
# ----

## ## # COVIDSurg data collection period, OpenSAFELY data, no-cancer patients.----
CSP_OS_C_outwith3m_mortality <- OS_C_outwith3m_mortality %>%
  dplyr::filter(era == "COVIDSurg data collection period" &
                  postOp_mortality_30day=="Dead outwith 30 days post-operation") %>%
  select(-c(era, postOp_mortality_30day)) %>% dplyr::ungroup()
## ## ## # Counts of 30-day post-operative mortality, across intervals.
CSP_OS_C_outwith3m_mortality_intervals <- data.frame(
  d_total = 0,
  pct_total = 0,
  d_infection_none = 0,
  pct_infection_none = 0,
  d_infection_0to2wk = 0,
  pct_infection_0to2wk = 0,
  d_infection_3to4wk = 0,
  pct_infection_3to4wk = 0,
  d_infection_5to6wk = 0,
  pct_infection_5to6wk = 0,
  d_infection_7wk = 0,
  pct_infection_7wk = 0
)
CSP_OS_C_outwith3m_mortality_intervals[seq(1,12,2)] <- CSP_OS_C_outwith3m_mortality
## ## ## # Percentages of 30-day post-operative mortality, across intervals.
CSP_OS_C_outwith3m_mortality_intervals[seq(2,12,2)] <- 
  (CSP_OS_C_outwith3m_mortality_intervals[seq(1,12,2)] / CSP_OS_C_outwith3m_counts) * 100
CSP_OS_C_outwith3m_mortality_intervals <-
  tidyr::replace_na(CSP_OS_C_outwith3m_mortality_intervals,
                    list("pct_total" = 0,
                         "pct_infection_none" = 0,
                         "pct_infection_0to2wk" = 0,
                         "pct_infection_3to4wk" = 0,
                         "pct_infection_5to6wk" = 0,
                         "pct_infection_7wk" = 0))
# ----

## ## # Pandemic with vaccines, OpenSAFELY data, no-cancer patients.----
PWV_OS_C_outwith3m_mortality <- OS_C_outwith3m_mortality %>%
  dplyr::filter(era == "Pandemic with vaccine" &
                  postOp_mortality_30day=="Dead outwith 30 days post-operation") %>%
  select(-c(era, postOp_mortality_30day)) %>% dplyr::ungroup()
## ## ## # Counts of 30-day post-operative mortality, across intervals.
PWV_OS_C_outwith3m_mortality_intervals <- data.frame(
  d_total = 0,
  pct_total = 0,
  d_infection_none = 0,
  pct_infection_none = 0,
  d_infection_0to2wk = 0,
  pct_infection_0to2wk = 0,
  d_infection_3to4wk = 0,
  pct_infection_3to4wk = 0,
  d_infection_5to6wk = 0,
  pct_infection_5to6wk = 0,
  d_infection_7wk = 0,
  pct_infection_7wk = 0
)
PWV_OS_C_outwith3m_mortality_intervals[seq(1,12,2)] <- PWV_OS_C_outwith3m_mortality
## ## ## # Percentages of 30-day post-operative mortality, across intervals.
PWV_OS_C_outwith3m_mortality_intervals[seq(2,12,2)] <- 
  (PWV_OS_C_outwith3m_mortality_intervals[seq(1,12,2)] / PWV_OS_C_outwith3m_counts) * 100
PWV_OS_C_outwith3m_mortality_intervals <-
  tidyr::replace_na(PWV_OS_C_outwith3m_mortality_intervals,
                    list("pct_total" = 0,
                         "pct_infection_none" = 0,
                         "pct_infection_0to2wk" = 0,
                         "pct_infection_3to4wk" = 0,
                         "pct_infection_5to6wk" = 0,
                         "pct_infection_7wk" = 0))
# ----




#############################################################
## Counts of 30-day post-operative mortality, in each era. ##
#############################################################
# ----
# OpenSAFELY data, no-cancer patients.
OS_all_mortality_totals <- data.frame(
  d_PP = PP_OS_all_mortality_intervals[['d_total']],
  pct_PP = PP_OS_all_mortality_intervals[['pct_total']],
  d_PNV = PNV_OS_all_mortality_intervals[['d_total']],
  pct_PNV = PNV_OS_all_mortality_intervals[['pct_total']],
  d_CSP = CSP_OS_all_mortality_intervals[['d_total']],
  pct_CSP = CSP_OS_all_mortality_intervals[['pct_total']],
  d_PWV = PWV_OS_all_mortality_intervals[['d_total']],
  pct_PWV = PWV_OS_all_mortality_intervals[['pct_total']]
)
# OpenSAFELY data, no-cancer patients.
OS_NC_mortality_totals <- data.frame(
    d_PP = PP_OS_NC_mortality_intervals[['d_total']],
    pct_PP = PP_OS_NC_mortality_intervals[['pct_total']],
    d_PNV = PNV_OS_NC_mortality_intervals[['d_total']],
    pct_PNV = PNV_OS_NC_mortality_intervals[['pct_total']],
    d_CSP = CSP_OS_NC_mortality_intervals[['d_total']],
    pct_CSP = CSP_OS_NC_mortality_intervals[['pct_total']],
    d_PWV = PWV_OS_NC_mortality_intervals[['d_total']],
    pct_PWV = PWV_OS_NC_mortality_intervals[['pct_total']]
  )
# OpenSAFELY data, cancer within 3 months patients.
OS_C_within3m_mortality_totals <- data.frame(
  d_PP = PP_OS_C_within3m_mortality_intervals[['d_total']],
  pct_PP = PP_OS_C_within3m_mortality_intervals[['pct_total']],
  d_PNV = PNV_OS_C_within3m_mortality_intervals[['d_total']],
  pct_PNV = PNV_OS_C_within3m_mortality_intervals[['pct_total']],
  d_CSP = CSP_OS_C_within3m_mortality_intervals[['d_total']],
  pct_CSP = CSP_OS_C_within3m_mortality_intervals[['pct_total']],
  d_PWV = PWV_OS_C_within3m_mortality_intervals[['d_total']],
  pct_PWV = PWV_OS_C_within3m_mortality_intervals[['pct_total']]
)
# OpenSAFELY data, cancer outwith 3 months patients.
OS_C_outwith3m_mortality_totals <- data.frame(
  d_PP = PP_OS_C_outwith3m_mortality_intervals[['d_total']],
  pct_PP = PP_OS_C_outwith3m_mortality_intervals[['pct_total']],
  d_PNV = PNV_OS_C_outwith3m_mortality_intervals[['d_total']],
  pct_PNV = PNV_OS_C_outwith3m_mortality_intervals[['pct_total']],
  d_CSP = CSP_OS_C_outwith3m_mortality_intervals[['d_total']],
  pct_CSP = CSP_OS_C_outwith3m_mortality_intervals[['pct_total']],
  d_PWV = PWV_OS_C_outwith3m_mortality_intervals[['d_total']],
  pct_PWV = PWV_OS_C_outwith3m_mortality_intervals[['pct_total']]
)
# ----


#############################
## Construct counts table. ##
#############################
# ----
table_counts <- 
  rbind(
    rep(NA, 6),               # Pre-pandemic, COVIDSurg.
    PP_OS_all_counts,         # Pre-pandemic, OpenSAFELY data, all surgery patients.
    PP_OS_NC_counts,          # Pre-pandemic, OpenSAFELY data, no-cancer patients.
    PP_OS_C_within3m_counts,  # Pre-pandemic, OpenSAFELY data, cancer within 3 months patients.
    PP_OS_C_outwith3m_counts, # Pre-pandemic, OpenSAFELY data, cancer outwith 3 months patients.
    rep(NA, 6),               # Pandemic no vaccines, COVIDSurg.
    PNV_OS_all_counts,        # Pandemic no vaccines, OpenSAFELY data, all surgery patients.
    PNV_OS_NC_counts,         # Pandemic no vaccines, OpenSAFELY data, no-cancer patients.
    PNV_OS_C_within3m_counts, # Pandemic no vaccines, OpenSAFELY data, cancer within 3 months patients.
    PNV_OS_C_outwith3m_counts,# Pandemic no vaccines, OpenSAFELY data, cancer outwith 3 months patients.
    COVIDSurg_counts,         # COVIDSurg data collection period, COVIDSurg.
    CSP_OS_all_counts,         # COVIDSurg data collection period, OpenSAFELY data, all surgery patients.
    CSP_OS_NC_counts,         # COVIDSurg data collection period, OpenSAFELY data, no-cancer patients.
    CSP_OS_C_within3m_counts, # COVIDSurg data collection period, OpenSAFELY data, cancer within 3 months patients.
    CSP_OS_C_outwith3m_counts,# COVIDSurg data collection period, OpenSAFELY data, cancer outwith 3 months patients.
    rep(NA, 6),               # Pandemic with vaccines, COVIDSurg.
    PWV_OS_all_counts,         # Pandemic with vaccines, OpenSAFELY data, all surgery patients.
    PWV_OS_NC_counts,         # Pandemic with vaccines, OpenSAFELY data, no-cancer patients.
    PWV_OS_C_within3m_counts, # Pandemic with vaccines, OpenSAFELY data, cancer within 3 months patients.
    PWV_OS_C_outwith3m_counts # Pandemic with vaccines, OpenSAFELY data, cancer outwith 3 months patients.
    ) %>% data.frame()
rownames(table_counts) <-
  c(
    "PP_COVIDSurg",
    "PP_OS_all",
    "PP_OS_NC",
    "PP_OS_C_within3m",
    "PP_OS_C_outwith3m",
    "PNV_COVIDSurg",
    "PNV_OS_all",
    "PNV_OS_NC",
    "PNV_OS_C_within3m",
    "PNV_OS_C_outwith3m",
    "CSP_COVIDSurg",
    "CSP_OS_all",
    "CSP_OS_NC",
    "CSP_OS_C_within3m",
    "CSP_OS_C_outwith3m",
    "PWV_COVIDSurg",
    "PWV_OS_all",
    "PWV_OS_NC",
    "PWV_OS_C_within3m",
    "PWV_OS_C_outwith3m"
  )
# Save table.
write.csv(
  x = table_counts,
  file = here::here("output",
                    "table_Count_of_patients_in_each_cohort_in_each_era_across_all_intervals.csv")
)
# Make table with only 7week threshold.
intervals_less_than_7wks <- c("n_infection_0to2wk", "n_infection_3to4wk",
                              "n_infection_5to6wk")
table_counts_7wkThreshold <- 
  table_counts %>% dplyr::select(tidyselect::all_of(intervals_less_than_7wks)) %>% data.matrix() %>%
  rowSums() %>% as.data.frame() %>%
  tibble::add_column(table_counts %>% dplyr::select(n_total, n_infection_none),
                     .before = ".") %>%
  tibble::add_column(table_counts %>%
                       dplyr::select(n_infection_7wk)) %>%
  `colnames<-`(c("n_total", "n_infection_none", "n_infection_<7wks", "n_infection_>7wks"))
# Save table.
write.csv(
  x = table_counts_7wkThreshold,
  file = here::here("output",
                    "table_7wkThreshold_Count_of_patients_in_each_cohort_in_each_era_across_all_intervals.csv")
)
# ----


#################################################
## Construct mortality-across-intervals table. ##
#################################################
# ----
table_mortality_intervals <-
  rbind(
    rep(NA, 12),                             # Pandemic no vaccines, COVIDSurg.
    PNV_OS_all_mortality_intervals,           # Pandemic no vaccines, OpenSAFELY data, all surgery patients.
    PNV_OS_NC_mortality_intervals,           # Pandemic no vaccines, OpenSAFELY data, no-cancer patients.
    PNV_OS_C_within3m_mortality_intervals,   # Pandemic no vaccines, OpenSAFELY data, cancer within 3 months patients.
    PNV_OS_C_outwith3m_mortality_intervals,  # Pandemic no vaccines, OpenSAFELY data, cancer outwith 3 months patients.
    COVIDSurg_mortality_intervals,           # COVIDSurg data collection period, COVIDSurg.
    CSP_OS_all_mortality_intervals,           # COVIDSurg data collection period, OpenSAFELY data, all surgery patients.
    CSP_OS_NC_mortality_intervals,           # COVIDSurg data collection period, OpenSAFELY data, no-cancer patients.
    CSP_OS_C_within3m_mortality_intervals,   # COVIDSurg data collection period, OpenSAFELY data, cancer within 3 months patients.
    CSP_OS_C_outwith3m_mortality_intervals,  # COVIDSurg data collection period, OpenSAFELY data, cancer outwith 3 months patients.
    rep(NA, 12),                             # Pandemic with vaccines, COVIDSurg.
    PWV_OS_all_mortality_intervals,           # Pandemic with vaccines, OpenSAFELY data, all surgery patients.
    PWV_OS_NC_mortality_intervals,           # Pandemic with vaccines, OpenSAFELY data, no-cancer patients.
    PWV_OS_C_within3m_mortality_intervals,   # Pandemic with vaccines, OpenSAFELY data, cancer within 3 months patients.
    PWV_OS_C_outwith3m_mortality_intervals   # Pandemic with vaccines, OpenSAFELY data, cancer outwith 3 months patients.
    )
rownames(table_mortality_intervals) <-
  c(
    "PNV_COVIDSurg",
    "PNV_OS_all",
    "PNV_OS_NC",
    "PNV_OS_C_within3m",
    "PNV_OS_C_outwith3m",
    "CSP_COVIDSurg",
    "CSP_OS_all",
    "CSP_OS_NC",
    "CSP_OS_C_within3m",
    "CSP_OS_C_outwith3m",
    "PWV_COVIDSurg",
    "PWV_OS_all",
    "PWV_OS_NC",
    "PWV_OS_C_within3m",
    "PWV_OS_C_outwith3m"
  )

# Save table.
write.csv(
  x = table_mortality_intervals,
  file = here::here("output",
                      "table_30day_post-op_mortality_in_each_era_across_all_intervals.csv")
)
# Make table with only 7week threshold.
intervals_less_than_7wks <- c("d_infection_0to2wk","d_infection_3to4wk",
                              "d_infection_5to6wk")

table_mortality_intervals_7wkThreshold <- 
  table_mortality_intervals %>% dplyr::select(tidyselect::all_of(intervals_less_than_7wks)) %>% data.matrix() %>%
  rowSums() %>% as.data.frame() %>%
  mutate(`pct_infection_<7wks` = (. / table_counts_7wkThreshold$`n_infection_<7wks`[6:nrow(table_counts_7wkThreshold)]) * 100) %>%
  tibble::add_column(table_mortality_intervals %>% dplyr::select(d_total, pct_total, d_infection_none, pct_infection_none),
                     .before = ".") %>%
  tibble::add_column(table_mortality_intervals %>%
                       dplyr::select(d_infection_7wk, pct_infection_7wk)) %>%
  `colnames<-`(c("d_total", "pct_total", "d_infection_none", "pct_infection_none",
                 "d_infection_<7wks", "pct_infection_<7wks", "d_infection_>7wks",
                 "pct_infection_>7wks"))
# Save table.
write.csv(
  x = table_mortality_intervals_7wkThreshold,
  file = here::here("output",
                    "table_7wkThreshold_30day_post-op_mortality_in_each_era_across_all_intervals.csv")
)
# ----


#######################################
## Construct mortality-totals table. ##
#######################################
# ----
table_mortality_totals <-
  rbind(
    COVIDSurg_mortality_totals,
    OS_all_mortality_totals,
    OS_NC_mortality_totals,
    OS_C_within3m_mortality_totals,
    OS_C_outwith3m_mortality_totals
    )
rownames(table_mortality_totals) <-
  c(
    "COVIDSurg",
    "OS_all",
    "OS_NC",
    "OS_C_within3m",
    "OS_C_outwith3m"
  )
write.csv(
  x = table_mortality_totals,
  file = here::here("output",
                    "table_30day_post-op_mortality_for_each_cohort_in_each_era.csv")
)
# ----


##############################################################################
## Plot weekly, monthly, 2monthly and 3monthly counts of surgeries within 7 ##
## weeks of a positive test.                                                ##
##############################################################################
# ----
source(here::here("analysis","serviceEvaluationFigures.R"))
# ----

