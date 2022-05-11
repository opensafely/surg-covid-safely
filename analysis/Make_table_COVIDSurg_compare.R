# Make_table_COVIDSurg_compare.R
#
# This script processes data from the myData dataframe to create a table
# that compares the 30-day post-operative mortality in the following cohorts:
#   1. COVIDSurg study estimates (doi: 10.1111/anae.15458)
#   2. OpenSAFELY surgery patients who do not have a cancer diagnosis within 6
#      months before or after their surgery.
#   3. OpenSAFELY surgery patients who do have a cancer diagnosis within 6
#      months before or after their surgery.
#

################################################
## Hardcode values from COVIDSurg publication ##
################################################
# ----
## Source is Table 1 from doi = 10.1111/anae.15458.
# Counts of patients, across intervals.
COVIDSurg_counts <- data.frame(
  n_total = NA,
  n_infection_none = 137104,
  n_infection_0to2wk = 1138,
  n_infection_3to4wk = 461,
  n_infection_5to6wk = 326,
  n_infection_7wk = 1202
)
COVIDSurg_counts$n_total <- sum(COVIDSurg_counts, na.rm = T)
# Counts of 30-day post-operative mortality, across intervals.
COVIDSurg_mortality_intervals <- data.frame(
  n_infection_none = 3654,
  pct_no_infection = NA,
  n_infection_0to2wk = 149,
  pct_infection_0to2wk = NA,
  n_infection_3to4wk = 60,
  pct_infection_3to4wk = NA,
  n_infection_5to6wk = 33,
  pct_infection_5to6wk = NA,
  n_infection_7wk = 42,
  pct_infection_7wk = NA
)
# Percentages of 30-day post-operative mortality, across intervals.
COVIDSurg_mortality_intervals[seq(2,10,2)] <- 
  (COVIDSurg_mortality_intervals[seq(1,10,2)] /
     COVIDSurg_counts[2:ncol(COVIDSurg_counts)]) * 100
# Counts of 30-day post-operative mortality, across era.
COVIDSurg_mortality_totals <- data.frame(
  n_PrePandemic = NA,
  pct_PrePandemic = NA,
  n_PandemicNoVacc = sum(COVIDSurg_mortality_intervals[seq(1,10,2)]),
  pct_PandemicNoVacc = NA,
  n_PandemicWithVacc = NA,
  pct_PandemicWithVacc = NA
)
# Percentages of 30-day post-operative mortality, across total.
COVIDSurg_mortality_totals$pct_PandemicNoVacc <- 
  (COVIDSurg_mortality_totals$n_PandemicNoVacc / COVIDSurg_counts$n_total) * 100
# ----

####################
## Generate data. ##
####################
# ----
source(here::here("analysis","dataset_preparation.R"))
# ----

#####################
## Filter datasets ##
#####################
# ----
# Filter the dataset for patients with a record of a cancer diagnosis within
# 6 months before or after their surgery.
# Naming convention is:
#   - OS = OpenSAFELY data
#   - C  = Cancer patient
#   - NC = Non-cancer patient
#
# OpenSAFELY data, cancer patients.
myData_OS_C <- myData %>% 
  dplyr::filter(category_cancer_within_6mths_surgery == 
                  "Cancer diagnosis within 6mths before surgery" |
                  category_cancer_within_6mths_surgery == 
                  "Cancer diagnosis within 6mths after surgery")

# OpenSAFELY data, non-cancer patients.
myData_OS_NC <- myData %>% dplyr::filter(has_cancer == FALSE)
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
## #  OpenSAFELY data, no-cancer patients.
OS_NC_counts <-
  myData_OS_NC %>%
    dplyr::group_by(era,
                    preOperative_infection_status) %>%
    dplyr::summarise(n = n())
OS_NC_counts <- 
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
  dplyr::full_join(OS_NC_counts) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(list("n" = 0))
## ## # Pre-pandemic, OpenSAFELY data, no-cancer patients.
intervals <- c(
  "No record of pre-operative SARS-CoV-2 infection",
  "0-2 weeks record of pre-operative SARS-CoV-2 infection",
  "3-4 weeks record of pre-operative SARS-CoV-2 infection",
  "5-6 weeks record of pre-operative SARS-CoV-2 infection",
  ">=7 weeks record of pre-operative SARS-CoV-2 infection"
  )

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
PP_OS_NC_counts[2:ncol(PP_OS_NC_counts)] <- NA
## ## # Pandemic no vaccines, OpenSAFELY data, no-cancer patients.
PNV_OS_NC_counts <- OS_NC_counts %>%
  dplyr::filter(era == "Pandemic no vaccine" &
                  preOperative_infection_status %in% intervals) %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
PNV_OS_NC_counts <- cbind(sum(PNV_OS_NC_counts), PNV_OS_NC_counts)
colnames(PNV_OS_NC_counts) <- colnames(COVIDSurg_counts)
## ## # Pandemic with vaccines, OpenSAFELY data, no-cancer patients.
PWV_OS_NC_counts <- OS_NC_counts %>%
  dplyr::filter(era == "Pandemic with vaccine" &
                  preOperative_infection_status %in% intervals) %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
PWV_OS_NC_counts <- cbind(sum(PWV_OS_NC_counts), PWV_OS_NC_counts)
colnames(PWV_OS_NC_counts) <- colnames(COVIDSurg_counts)


## #  OpenSAFELY data, cancer patients.
OS_C_counts <-
  myData_OS_C %>%
  dplyr::group_by(era,
                  preOperative_infection_status) %>%
  dplyr::summarise(n = n())
OS_C_counts <- 
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
  dplyr::full_join(OS_C_counts) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(list("n" = 0))
## ## # Pre-pandemic, OpenSAFELY data, no-cancer patients.
PP_OS_C_counts <- OS_C_counts %>%
  dplyr::filter(era == "Pre-pandemic" &
                  preOperative_infection_status %in% intervals) %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
PP_OS_C_counts <- cbind(sum(PP_OS_C_counts), PP_OS_C_counts)
colnames(PP_OS_C_counts) <- colnames(COVIDSurg_counts)
# In pre-pandemic era, there should not be any instances of test results. Therefore,
# there should not be any instances counted in the intervals. Any counts within
# intervals are data quality issues and are expected to be low. These erroneous 
# data will be imputed with the expected NA.
PP_OS_C_counts[2:ncol(PP_OS_C_counts)] <- NA
## ## # Pandemic no vaccines, OpenSAFELY data, no-cancer patients.
PNV_OS_C_counts <- OS_C_counts %>%
  dplyr::filter(era == "Pandemic no vaccine" &
                  preOperative_infection_status %in% intervals) %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
PNV_OS_C_counts <- cbind(sum(PNV_OS_C_counts), PNV_OS_C_counts)
colnames(PNV_OS_C_counts) <- colnames(COVIDSurg_counts)
## ## # Pandemic with vaccines, OpenSAFELY data, no-cancer patients.
PWV_OS_C_counts <- OS_C_counts %>%
  dplyr::filter(era == "Pandemic with vaccine" &
                  preOperative_infection_status %in% intervals) %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
PWV_OS_C_counts <- cbind(sum(PWV_OS_C_counts), PWV_OS_C_counts)
colnames(PWV_OS_C_counts) <- colnames(COVIDSurg_counts)
# ----

##################################################################
## Counts of 30-day post-operative mortality, across intervals. ##
## Non-cancer patients.                                         ##
##################################################################
# ----
# Count of patients in each of the categories for pre-operative infection
# status (stratified by surgery era; see above) also stratified by whether
# or not the patient died within 30 days of their surgery:
#   1. "Alive within 30-day post-operation"
#   2. "Dead within 30-day post-operation" 
#
## # OpenSAFELY data, no-cancer patients.
OS_NC_mortality <- 
myData_OS_NC %>% dplyr::group_by(era, postOp_mortality_30day) %>%
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
    c("Error: No surgery", "Pre-pandemic", "Pandemic no vaccine", "Pandemic with vaccine"),
  postOp_mortality_30day = 
    c("Alive within 30-day post-operation",
      "Dead within 30-day post-operation",
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
## ## # Pre-pandemic, OpenSAFELY data, no-cancer patients.
PP_OS_NC_mortality <- OS_NC_mortality %>%
  dplyr::filter(era == "Pre-pandemic" &
                  postOp_mortality_30day=="Dead within 30-day post-operation") %>%
  select(-c(era, postOp_mortality_30day, n_per_group)) %>% dplyr::ungroup()
## ## ## # Counts of 30-day post-operative mortality, across intervals.
PP_OS_NC_mortality_intervals <- data.frame(
  n_infection_none = 0,
  pct_no_infection = 0,
  n_infection_0to2wk = 0,
  pct_infection_0to2wk = 0,
  n_infection_3to4wk = 0,
  pct_infection_3to4wk = 0,
  n_infection_5to6wk = 0,
  pct_infection_5to6wk = 0,
  n_infection_7wk = 0,
  pct_infection_7wk = 0
)
PP_OS_NC_mortality_intervals[seq(1,10,2)] <- PP_OS_NC_mortality
## ## ## # Percentages of 30-day post-operative mortality, across intervals.
PP_OS_NC_mortality_intervals[seq(2,10,2)] <- 
  (PP_OS_NC_mortality_intervals[seq(1,10,2)] /
     PP_OS_NC_counts[2:ncol(PP_OS_NC_counts)]) * 100
## ## # Pandemic no vaccines, OpenSAFELY data, no-cancer patients.
PNV_OS_NC_mortality <- OS_NC_mortality %>%
  dplyr::filter(era == "Pandemic no vaccine" &
                  postOp_mortality_30day=="Dead within 30-day post-operation") %>%
  select(-c(era, postOp_mortality_30day, n_per_group)) %>% dplyr::ungroup()
## ## ## # Counts of 30-day post-operative mortality, across intervals.
PNV_OS_NC_mortality_intervals <- data.frame(
  n_infection_none = 0,
  pct_no_infection = 0,
  n_infection_0to2wk = 0,
  pct_infection_0to2wk = 0,
  n_infection_3to4wk = 0,
  pct_infection_3to4wk = 0,
  n_infection_5to6wk = 0,
  pct_infection_5to6wk = 0,
  n_infection_7wk = 0,
  pct_infection_7wk = 0
)
PNV_OS_NC_mortality_intervals[seq(1,10,2)] <- PNV_OS_NC_mortality
## ## ## # Percentages of 30-day post-operative mortality, across intervals.
PNV_OS_NC_mortality_intervals[seq(2,10,2)] <- 
  (PNV_OS_NC_mortality_intervals[seq(1,10,2)] /
     PNV_OS_NC_counts[2:ncol(PNV_OS_NC_counts)]) * 100
## ## # Pandemic with vaccines, OpenSAFELY data, no-cancer patients.
PWV_OS_NC_mortality <- OS_NC_mortality %>%
  dplyr::filter(era == "Pandemic with vaccine" &
                  postOp_mortality_30day=="Dead within 30-day post-operation") %>%
  select(-c(era, postOp_mortality_30day, n_per_group)) %>% dplyr::ungroup()
## ## ## # Counts of 30-day post-operative mortality, across intervals.
PWV_OS_NC_mortality_intervals <- data.frame(
  n_infection_none = 0,
  pct_no_infection = 0,
  n_infection_0to2wk = 0,
  pct_infection_0to2wk = 0,
  n_infection_3to4wk = 0,
  pct_infection_3to4wk = 0,
  n_infection_5to6wk = 0,
  pct_infection_5to6wk = 0,
  n_infection_7wk = 0,
  pct_infection_7wk = 0
)
PWV_OS_NC_mortality_intervals[seq(1,10,2)] <- PWV_OS_NC_mortality
## ## ## # Percentages of 30-day post-operative mortality, across intervals.
PWV_OS_NC_mortality_intervals[seq(2,10,2)] <- 
  (PWV_OS_NC_mortality_intervals[seq(1,10,2)] /
     PWV_OS_NC_counts[2:ncol(PWV_OS_NC_counts)]) * 100



# ----

##################################################################
## Counts of 30-day post-operative mortality, across intervals. ##
## Cancer patients.                                             ##
##################################################################
# ----
# Count of patients in each of the categories for pre-operative infection
# status (stratified by surgery era; see above) also stratified by whether
# or not the patient died within 30 days of their surgery:
#   1. "Alive within 30-day post-operation"
#   2. "Dead within 30-day post-operation" 
#
## # OpenSAFELY data, cancer patients.
OS_C_mortality <- 
  myData_OS_C %>% dplyr::group_by(era, postOp_mortality_30day) %>%
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
OS_C_mortality <- 
  expand.grid(
    era = 
      c("Error: No surgery", "Pre-pandemic", "Pandemic no vaccine", "Pandemic with vaccine"),
    postOp_mortality_30day = 
      c("Alive within 30-day post-operation",
        "Dead within 30-day post-operation",
        "Error: Surgery after death",
        "No death recorded",
        "No surgery recorded",
        "Missing")) %>%
  dplyr::full_join(OS_C_mortality) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(list("n_per_group" = 0,
                         "n_infection_none" = 0,
                         "n_infection_0to2wk" = 0,
                         "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0,
                         "n_infection_7wk" = 0))
## ## # Pre-pandemic, OpenSAFELY data, no-cancer patients.
PP_OS_C_mortality <- OS_C_mortality %>%
  dplyr::filter(era == "Pre-pandemic" &
                  postOp_mortality_30day=="Dead within 30-day post-operation") %>%
  select(-c(era, postOp_mortality_30day, n_per_group)) %>% dplyr::ungroup()
## ## ## # Counts of 30-day post-operative mortality, across intervals.
PP_OS_C_mortality_intervals <- data.frame(
  n_infection_none = 0,
  pct_no_infection = 0,
  n_infection_0to2wk = 0,
  pct_infection_0to2wk = 0,
  n_infection_3to4wk = 0,
  pct_infection_3to4wk = 0,
  n_infection_5to6wk = 0,
  pct_infection_5to6wk = 0,
  n_infection_7wk = 0,
  pct_infection_7wk = 0
)
PP_OS_C_mortality_intervals[seq(1,10,2)] <- PP_OS_C_mortality
## ## ## # Percentages of 30-day post-operative mortality, across intervals.
PP_OS_C_mortality_intervals[seq(2,10,2)] <- 
  (PP_OS_C_mortality_intervals[seq(1,10,2)] /
     PP_OS_C_counts[2:ncol(PP_OS_C_counts)]) * 100
## ## # Pandemic no vaccines, OpenSAFELY data, no-cancer patients.
PNV_OS_C_mortality <- OS_C_mortality %>%
  dplyr::filter(era == "Pandemic no vaccine" &
                  postOp_mortality_30day=="Dead within 30-day post-operation") %>%
  select(-c(era, postOp_mortality_30day, n_per_group)) %>% dplyr::ungroup()
## ## ## # Counts of 30-day post-operative mortality, across intervals.
PNV_OS_C_mortality_intervals <- data.frame(
  n_infection_none = 0,
  pct_no_infection = 0,
  n_infection_0to2wk = 0,
  pct_infection_0to2wk = 0,
  n_infection_3to4wk = 0,
  pct_infection_3to4wk = 0,
  n_infection_5to6wk = 0,
  pct_infection_5to6wk = 0,
  n_infection_7wk = 0,
  pct_infection_7wk = 0
)
PNV_OS_C_mortality_intervals[seq(1,10,2)] <- PNV_OS_C_mortality
## ## ## # Percentages of 30-day post-operative mortality, across intervals.
PNV_OS_C_mortality_intervals[seq(2,10,2)] <- 
  (PNV_OS_C_mortality_intervals[seq(1,10,2)] /
     PNV_OS_C_counts[2:ncol(PNV_OS_C_counts)]) * 100
## ## # Pandemic with vaccines, OpenSAFELY data, no-cancer patients.
PWV_OS_C_mortality <- OS_C_mortality %>%
  dplyr::filter(era == "Pandemic with vaccine" &
                  postOp_mortality_30day=="Dead within 30-day post-operation") %>%
  select(-c(era, postOp_mortality_30day, n_per_group)) %>% dplyr::ungroup()
## ## ## # Counts of 30-day post-operative mortality, across intervals.
PWV_OS_C_mortality_intervals <- data.frame(
  n_infection_none = 0,
  pct_no_infection = 0,
  n_infection_0to2wk = 0,
  pct_infection_0to2wk = 0,
  n_infection_3to4wk = 0,
  pct_infection_3to4wk = 0,
  n_infection_5to6wk = 0,
  pct_infection_5to6wk = 0,
  n_infection_7wk = 0,
  pct_infection_7wk = 0
)
PWV_OS_C_mortality_intervals[seq(1,10,2)] <- PWV_OS_C_mortality
## ## ## # Percentages of 30-day post-operative mortality, across intervals.
PWV_OS_C_mortality_intervals[seq(2,10,2)] <- 
  (PWV_OS_C_mortality_intervals[seq(1,10,2)] /
     PWV_OS_C_counts[2:ncol(PWV_OS_C_counts)]) * 100
# ----

#############################################################
## Counts of 30-day post-operative mortality, in each era. ##
## Non-cancer patients.                                    ##
#############################################################
# ----
# OpenSAFELY data, no-cancer patients.
OS_NC_mortality_totals <- data.frame(
    n_PP = sum(PP_OS_NC_mortality_intervals[seq(1,10,2)]),
    pct_PP = 0,
    n_PNV = sum(PNV_OS_NC_mortality_intervals[seq(1,10,2)]),
    pct_PNV = 0,
    n_PWV = sum(PWV_OS_NC_mortality_intervals[seq(1,10,2)]),
    pct_PWV = 0
  )
OS_NC_mortality_totals[seq(2,6,2)] <-
  (OS_NC_mortality_totals[seq(1,6,2)] / 
     c(PP_OS_NC_counts$n_total,
       PNV_OS_NC_counts$n_total,
       PWV_OS_NC_counts$n_total)
       ) * 100
colnames(OS_NC_mortality_totals)<- colnames(COVIDSurg_mortality_totals)
# OpenSAFELY data, cancer patients.
OS_C_mortality_totals <- data.frame(
  n_PP = sum(PP_OS_C_mortality_intervals[seq(1,10,2)]),
  pct_PP = 0,
  n_PNV = sum(PNV_OS_C_mortality_intervals[seq(1,10,2)]),
  pct_PNV = 0,
  n_PWV = sum(PWV_OS_C_mortality_intervals[seq(1,10,2)]),
  pct_PWV = 0
)
OS_C_mortality_totals[seq(2,6,2)] <-
  (OS_C_mortality_totals[seq(1,6,2)] / 
     c(PP_OS_C_counts$n_total,
       PNV_OS_C_counts$n_total,
       PWV_OS_C_counts$n_total)
  ) * 100
colnames(OS_C_mortality_totals)<- colnames(COVIDSurg_mortality_totals)

# ----

#############################
## Construct counts table. ##
#############################
# ----
table_counts <- 
  rbind(
    rep(NA, 6), # Pre-pandemic, COVIDSurg.
    PP_OS_NC_counts, # Pre-pandemic, OpenSAFELY data, no-cancer patients.
    PP_OS_C_counts, # Pre-pandemic, OpenSAFELY data, cancer patients.
    COVIDSurg_counts,
    PNV_OS_NC_counts, # Pandemic no vaccines, OpenSAFELY data, no-cancer patients.
    PNV_OS_C_counts, # Pandemic no vaccines, OpenSAFELY data, cancer patients.
    rep(NA, 6), # Pandemic with vaccines, COVIDSurg.
    PWV_OS_NC_counts, # Pandemic with vaccines, OpenSAFELY data, no-cancer patients.
    PWV_OS_C_counts # Pandemic with vaccines, OpenSAFELY data, cancer patients.
    ) %>% data.frame()
rownames(table_counts) <-
  c(
    "PP_COVIDSurg",
    "PP_OS_NC",
    "PP_OS_C",
    "PNV_COVIDSurg",
    "PNV_OS_NC",
    "PNV_OS_C",
    "PWV_COVIDSurg",
    "PWV_OS_NC",
    "PWV_OS_C"
  )
write.csv(
  x = table_counts,
  file = here::here("output",
                    "table_Count_of_patients_in_each_cohort_in_each_era_across_all_intervals.csv")
)
# ----

#################################################
## Construct mortality-across-intervals table. ##
#################################################
# ----
table_mortality_intervals <-
  rbind(
    COVIDSurg_mortality_intervals,
    PNV_OS_NC_mortality_intervals,
    PNV_OS_C_mortality_intervals,
    rep(NA, 10),
    PWV_OS_NC_mortality_intervals,
    PWV_OS_C_mortality_intervals
    )
colnames(table_mortality_intervals)[seq(1,ncol(table_mortality_intervals),2)] <-
  c("d_infection_none", "d_infection_0to2wk", "d_infection_3to4wk",
    "d_infection_5to6wk", "d_infection_7wk")
rownames(table_mortality_intervals) <-
  c(
    "PNV_COVIDSurg",
    "PNV_OS_NC",
    "PNV_OS_C",
    "PWV_COVIDSurg",
    "PWV_OS_NC",
    "PWV_OS_C"
  )
# Redact small numbers.
table_mortality_intervals[c("PNV_OS_C", "PWV_OS_C"),
                          3:ncol(table_mortality_intervals)] <- NA
cols2redact <-  c("d_infection_3to4wk", "pct_infection_3to4wk",
                  "d_infection_5to6wk", "pct_infection_5to6wk")
table_mortality_intervals["PNV_OS_NC", cols2redact] <- NA
# Save table.
write.csv(
  x = table_mortality_intervals,
  file = here::here("output",
                      "table_30day_post-op_mortality_in_each_era_across_all_intervals.csv")
)
# ----

#######################################
## Construct mortality-totals table. ##
#######################################
# ----
table_mortality_totals <-
  rbind(
    COVIDSurg_mortality_totals,
    OS_NC_mortality_totals,
    OS_C_mortality_totals
    )
colnames(table_mortality_totals)[seq(1,ncol(table_mortality_totals),2)] <-
  c("d_PrePandemic", "d_PandemicNoVacc", "d_PandemicWithVacc")
rownames(table_mortality_totals) <-
  c(
    "COVIDSurg",
    "OS_NC",
    "OS_C"
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
source(here::here("analysis","preprintFigure.R"))
# ----

