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
COVIDSurg_totals <- data.frame(
  n_total = NA,
  n_infection_none = 137104,
  n_infection_0to2wk = 1138,
  n_infection_3to4wk = 461,
  n_infection_5to6wk = 326,
  n_infection_7wk = 1202
)
COVIDSurg_totals$n_total <- sum(COVIDSurg_totals, na.rm = T)
# Counts of 30-day post-operative mortality, across intervals.
intervals_COVIDSurg_values <- data.frame(
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
intervals_COVIDSurg_values[seq(2,10,2)] <- 
  (intervals_COVIDSurg_values[seq(1,10,2)] /
     COVIDSurg_totals[2:ncol(COVIDSurg_totals)]) * 100
# Counts of 30-day post-operative mortality, across era.
era_COVIDSurg_values <- data.frame(
  n_PrePandemic = NA,
  pct_PrePandemic = NA,
  n_PandemicNoVacc = sum(intervals_COVIDSurg_values[seq(1,10,2)]),
  pct_PandemicNoVacc = NA,
  n_PandemicWithVacc = NA,
  pct_PandemicWithVacc = NA
)
# Percentages of 30-day post-operative mortality, across total.
era_COVIDSurg_values$pct_PandemicNoVacc <- 
  (era_COVIDSurg_values$n_PandemicNoVacc / COVIDSurg_totals$n_total) * 100
# ----

#####################
## Filter datasets ##
#####################
# ----
# Filter the dataset for patients with a record of a cancer diagnosis within
# 6 months before or after their surgery.
# OpenSAFELY data, no-cancer patients.
myData_OS_C <- myData %>% 
  dplyr::filter(category_cancer_within_6mths_surgery == 
                  "Cancer diagnosis within 6mths before surgery" |
                  category_cancer_within_6mths_surgery == 
                  "Cancer diagnosis within 6mths after surgery")

# OpenSAFELY data, cancer patients.
myData_OS_NC <- myData %>% 
  dplyr::filter(category_cancer_within_6mths_surgery == 
                  "No cancer diagnosis within 6mths before or after surgery")
# ----


###########################################
## Counts of patients, across intervals. ##
###########################################
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
# ##    1. "Pre-pandemic" <= "2020-03-17"
# ##    2. "Pandemic no vaccine" <= ("2020-12-08" +
# ##                                 3 weeks for national roll-out +
# ##                                 2 weeks to take effect)
# ##    3. "Pandemic with vaccine" > ("2020-12-08" + 5 weeks ) [explained above]
#
## #  OpenSAFELY data, no-cancer patients. ----
totals_OS_NC <-
  myData_OS_NC %>%
    dplyr::group_by(era,
                    preOperative_infection_status) %>%
    dplyr::summarise(n = n())
totals_OS_NC <- 
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
  dplyr::full_join(totals_OS_NC) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(list("n" = 0))
## ## # Pre-pandemic, OpenSAFELY data, no-cancer patients.
PP_OS_NC_totals <- totals_OS_NC %>%
  dplyr::filter(era == "Pre-pandemic" &
                  preOperative_infection_status !=
                  "Error: Test result after surgery. Check study_definition.") %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
PP_OS_NC_totals <- cbind(sum(PP_OS_NC_totals), PP_OS_NC_totals)
colnames(PP_OS_NC_totals) <- colnames(COVIDSurg_totals)
## ## # Pandemic no vaccines, OpenSAFELY data, no-cancer patients.
PNV_OS_NC_totals <- totals_OS_NC %>%
  dplyr::filter(era == "Pandemic no vaccine" &
                  preOperative_infection_status !=
                  "Error: Test result after surgery. Check study_definition.") %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
PNV_OS_NC_totals <- cbind(sum(PNV_OS_NC_totals), PNV_OS_NC_totals)
colnames(PNV_OS_NC_totals) <- colnames(COVIDSurg_totals)
## ## # Pandemic with vaccines, OpenSAFELY data, no-cancer patients.
PWV_OS_NC_totals <- totals_OS_NC %>%
  dplyr::filter(era == "Pandemic with vaccine" &
                  preOperative_infection_status !=
                  "Error: Test result after surgery. Check study_definition.") %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
PWV_OS_NC_totals <- cbind(sum(PWV_OS_NC_totals), PWV_OS_NC_totals)
colnames(PWV_OS_NC_totals) <- colnames(COVIDSurg_totals)
# ----

## #  OpenSAFELY data, cancer patients. ----
totals_OS_C <-
  myData_OS_C %>%
  dplyr::group_by(era,
                  preOperative_infection_status) %>%
  dplyr::summarise(n = n())
totals_OS_C <- 
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
  dplyr::full_join(totals_OS_C) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(list("n" = 0))
## ## # Pre-pandemic, OpenSAFELY data, no-cancer patients.
PP_OS_C_totals <- totals_OS_C %>%
  dplyr::filter(era == "Pre-pandemic" &
                  preOperative_infection_status !=
                  "Error: Test result after surgery. Check study_definition.") %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
PP_OS_C_totals <- cbind(sum(PP_OS_C_totals), PP_OS_C_totals)
colnames(PP_OS_C_totals) <- colnames(COVIDSurg_totals)
## ## # Pandemic no vaccines, OpenSAFELY data, no-cancer patients.
PNV_OS_C_totals <- totals_OS_C %>%
  dplyr::filter(era == "Pandemic no vaccine" &
                  preOperative_infection_status !=
                  "Error: Test result after surgery. Check study_definition.") %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
PNV_OS_C_totals <- cbind(sum(PNV_OS_C_totals), PNV_OS_C_totals)
colnames(PNV_OS_C_totals) <- colnames(COVIDSurg_totals)
## ## # Pandemic with vaccines, OpenSAFELY data, no-cancer patients.
PWV_OS_C_totals <- totals_OS_C %>%
  dplyr::filter(era == "Pandemic with vaccine" &
                  preOperative_infection_status !=
                  "Error: Test result after surgery. Check study_definition.") %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
PWV_OS_C_totals <- cbind(sum(PWV_OS_C_totals), PWV_OS_C_totals)
colnames(PWV_OS_C_totals) <- colnames(COVIDSurg_totals)
# ----

##################################################################
## Counts of 30-day post-operative mortality, across intervals. ##
##################################################################
# ## Count of patients in each of the categories for pre-operative infection
# ## status (stratified by surgery era; see above) also stratified by whether
# ## or not the patient died within 30 days of their surgery:
# ##    1. "Alive within 30-day post-operation"
# ##    2. "Dead within 30-day post-operation" 
#
## # OpenSAFELY data, no-cancer patients. ----
values_OS_NC <- 
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
values_OS_NC <- 
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
  dplyr::full_join(values_OS_C) %>%
  dplyr::arrange(era) %>%
  tidyr::replace_na(list("n_per_group" = 0,
                         "n_infection_none" = 0,
                         "n_infection_0to2wk" = 0,
                         "n_infection_3to4wk" = 0,
                         "n_infection_5to6wk" = 0,
                         "n_infection_7wk" = 0))
## ## # Pre-pandemic, OpenSAFELY data, no-cancer patients.
PP_OS_NC_values <- values_OS_NC %>%
  dplyr::filter(era == "Pre-pandemic" &
                  postOp_mortality_30day=="Alive within 30-day post-operation") %>%
  select(-c(era, postOp_mortality_30day, n_per_group)) %>% dplyr::ungroup()
# Counts of 30-day post-operative mortality, across intervals.
intervals_PP_OS_NC_values <- data.frame(
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
intervals_PP_OS_NC_values[seq(1,10,2)] <- PP_OS_NC_values
# Percentages of 30-day post-operative mortality, across intervals.
intervals_PP_OS_NC_values[seq(2,10,2)] <- 
  (intervals_PP_OS_NC_values[seq(1,10,2)] /
     PP_OS_NC_totals[2:ncol(PP_OS_NC_totals)]) * 100
## ## # Pandemic no vaccines, OpenSAFELY data, no-cancer patients.
PNV_OS_NC_values <- values_OS_NC %>%
  dplyr::filter(era == "Pre-pandemic" &
                  postOp_mortality_30day=="Alive within 30-day post-operation") %>%
  select(-c(era, postOp_mortality_30day, n_per_group)) %>% dplyr::ungroup()
# Counts of 30-day post-operative mortality, across intervals.
intervals_PNV_OS_NC_values <- data.frame(
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
intervals_PNV_OS_NC_values[seq(1,10,2)] <- PNV_OS_NC_values
# Percentages of 30-day post-operative mortality, across intervals.
intervals_PNV_OS_NC_values[seq(2,10,2)] <- 
  (intervals_PNV_OS_NC_values[seq(1,10,2)] /
     PNV_OS_NC_totals[2:ncol(PNV_OS_NC_totals)]) * 100
################# Either PNV_OS_NC_totals is wrong or values_OS_NC is wrong 
################# because intervals_PNV_OS_NC_values$pct_no_infection is > 100%





## ## # Pandemic no vaccines, OpenSAFELY data, no-cancer patients.
PNV_OS_NC_totals <- totals_OS_NC %>%
  dplyr::filter(era == "Pandemic no vaccine" &
                  preOperative_infection_status !=
                  "Error: Test result after surgery. Check study_definition.") %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
PNV_OS_NC_totals <- cbind(sum(PNV_OS_NC_totals), PNV_OS_NC_totals)
colnames(PNV_OS_NC_totals) <- colnames(COVIDSurg_totals)
## ## # Pandemic with vaccines, OpenSAFELY data, no-cancer patients.
PWV_OS_NC_totals <- totals_OS_NC %>%
  dplyr::filter(era == "Pandemic with vaccine" &
                  preOperative_infection_status !=
                  "Error: Test result after surgery. Check study_definition.") %>%
  dplyr::arrange(preOperative_infection_status) %>% select(n) %>% t() %>% data.frame()
PWV_OS_NC_totals <- cbind(sum(PWV_OS_NC_totals), PWV_OS_NC_totals)
colnames(PWV_OS_NC_totals) <- colnames(COVIDSurg_totals)
# ----


# ----

#######################################################################
# Ensure tibbles show zero values when categories are not in the data #
#######################################################################
# ----
# ## table1_totals_preOp_infection_status.
table1_totals_preOp_infection_status <- 
            expand.grid(
              era = 
                c("No surgery", "preCOVID surgery", "postCOVID surgery"),
              "preOperative_infection_status" = 
                c("Error: Test result after surgery. Check study_definition.",
                  "No record of pre-operative SARS-CoV-2 infection",
                  "0-2 weeks record of pre-operative SARS-CoV-2 infection",
                  "3-4 weeks record of pre-operative SARS-CoV-2 infection",
                  ">=7 weeks record of pre-operative SARS-CoV-2 infection")) %>%
            dplyr::full_join(table1_totals_preOp_infection_status) %>%
            dplyr::arrange(era) %>%
            tidyr::replace_na(list("n" = 0))
# ## table1_ageGroup.
table1_ageGroup <- 
            expand.grid(
              era = 
                c("No surgery", "preCOVID surgery", "postCOVID surgery"),
              age_group_surgery = 
                c("0-29",
                  "30-49",
                  "50-69",
                  "70-79",
                  "80+",
                  "Missing")) %>%
              dplyr::full_join(table1_ageGroup) %>%
              dplyr::arrange(era) %>%
              tidyr::replace_na(list("n_per_group" = 0,
                                     "n_infection_none" = 0,
                                     "n_infection_0to2wk" = 0,
                                     "n_infection_3to4wk" = 0,
                                     "n_infection_5to6wk" = 0,
                                     "n_infection_7wk" = 0))
# ## table1_Sex.
table1_Sex <- 
            expand.grid(
              era = 
                c("No surgery", "preCOVID surgery", "postCOVID surgery"),
              Sex = 
                c("Female",
                  "Male",
                  "Missing")) %>%
            dplyr::full_join(table1_Sex) %>%
            dplyr::arrange(era) %>%
            tidyr::replace_na(list("n_per_group" = 0,
                                   "n_infection_none" = 0,
                                   "n_infection_0to2wk" = 0,
                                   "n_infection_3to4wk" = 0,
                                   "n_infection_5to6wk" = 0,
                                   "n_infection_7wk" = 0))
# ## table1_postOp_mortality_30day.
table1_postOp_mortality_30day <- 
            expand.grid(
              era = 
                c("No surgery", "preCOVID surgery", "postCOVID surgery"),
              postOp_mortality_30day = 
                c("Alive within 30-day post-operation",
                  "Dead within 30-day post-operation",
                  "Error: Surgery after death",
                  "No death recorded",
                  "No surgery recorded",
                  "Missing")) %>%
            dplyr::full_join(table1_postOp_mortality_30day) %>%
            dplyr::arrange(era) %>%
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
table1_ageGroup_6mths <- table1_ageGroup
write.csv(
  x = table1_ageGroup,
  file = here::here("output","table1_ageGroup_6mths.csv")
)
table1_Sex_6mths <- table1_Sex
write.csv(
  x = table1_Sex,
  file = here::here("output","table1_Sex_6mths.csv")
)
table1_postOp_mortality_30day_6mths <- table1_postOp_mortality_30day
write.csv(
  x = table1_postOp_mortality_30day,
  file = here::here("output","table1_postOp_mortality_30day_6mths.csv")
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
  dplyr::filter(era=="preCOVID surgery") %>%
  dplyr::arrange(age_group_surgery) %>% dplyr::ungroup() %>% dplyr::select(n_per_group)
prop_preMarch2020_ageGroup <- n_preMarch2020_ageGroup /
  table1_ageGroup %>%
  dplyr::filter(era=="preCOVID surgery") %>%
  select(n_per_group) %>% sum()
# ## Sex.
n_preMarch2020_Sex <-
  table1_Sex %>%
  dplyr::filter(era=="preCOVID surgery") %>%
  dplyr::arrange(Sex) %>% dplyr::ungroup() %>%
  dplyr::select(n_per_group)
prop_preMarch2020_Sex <- n_preMarch2020_Sex / 
  table1_Sex %>%
  dplyr::filter(era=="preCOVID surgery") %>%
  select(n_per_group) %>% sum()
# ## 30-day post-operative mortality.
n_preMarch2020_postOp_mortality_30day <-
  table1_postOp_mortality_30day %>%
  dplyr::filter(era=="preCOVID surgery",
                (postOp_mortality_30day=="Alive within 30-day post-operation"|
                   postOp_mortality_30day=="Dead within 30-day post-operation"|
                   postOp_mortality_30day=="Missing")) %>%
  dplyr::arrange(postOp_mortality_30day) %>% dplyr::ungroup() %>%
  dplyr::select(n_per_group)
prop_preMarch2020_postOp_mortality_30day <-
  n_preMarch2020_postOp_mortality_30day /
  table1_postOp_mortality_30day %>%
  dplyr::filter(era=="preCOVID surgery") %>%
  select(n_per_group) %>% sum()
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
  dplyr::filter(era=="postCOVID surgery") %>%
  dplyr::arrange(age_group_surgery) %>% dplyr::ungroup() %>% dplyr::select(n_per_group)
prop_postMarch2020_ageGroup <-
  n_postMarch2020_ageGroup /
  table1_ageGroup %>%
  dplyr::filter(era=="postCOVID surgery") %>%
  select(n_per_group) %>% sum()
# ## Sex.
n_postMarch2020_Sex <-
  table1_Sex %>%
  dplyr::filter(era=="postCOVID surgery") %>%
  dplyr::arrange(Sex) %>% dplyr::ungroup() %>%
  dplyr::select(n_per_group)
prop_postMarch2020_Sex <- n_postMarch2020_Sex / 
  table1_Sex %>%
  dplyr::filter(era=="postCOVID surgery") %>%
  select(n_per_group) %>% sum()
# ## 30-day post-operative mortality.
n_postMarch2020_postOp_mortality_30day <-
  table1_postOp_mortality_30day %>%
  dplyr::filter(era=="postCOVID surgery",
                (postOp_mortality_30day=="Alive within 30-day post-operation"|
                   postOp_mortality_30day=="Dead within 30-day post-operation"|
                   postOp_mortality_30day=="Missing")) %>%
  dplyr::arrange(postOp_mortality_30day) %>% dplyr::ungroup() %>%
  dplyr::select(n_per_group)
prop_postMarch2020_postOp_mortality_30day <-
  n_postMarch2020_postOp_mortality_30day / 
  table1_postOp_mortality_30day %>%
  dplyr::filter(era=="postCOVID surgery") %>%
  select(n_per_group) %>% sum()
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
  dplyr::filter(era=="postCOVID surgery") %>%
  dplyr::arrange(age_group_surgery) %>% dplyr::ungroup() %>% dplyr::select(n_infection_none)
prop_subtotals_infection_none_ageGroup <-
  n_subtotals_infection_none_ageGroup /
  table1_ageGroup %>%
  dplyr::filter(era=="postCOVID surgery") %>%
  select(n_infection_none) %>% sum()
# ## Sex.
n_subtotals_infection_none_Sex <-
  table1_Sex %>%
  dplyr::filter(era=="postCOVID surgery") %>%
  dplyr::arrange(Sex) %>% dplyr::ungroup() %>%
  dplyr::select(n_infection_none)
prop_subtotals_infection_none_Sex <-
  n_subtotals_infection_none_Sex /
  table1_Sex %>%
  dplyr::filter(era=="postCOVID surgery") %>%
  select(n_infection_none) %>% sum()
# ## 30-day post-operative mortality.
n_subtotals_infection_none_postOp_mortality_30day <-
  table1_postOp_mortality_30day %>%
  dplyr::filter(era=="postCOVID surgery",
                (postOp_mortality_30day=="Alive within 30-day post-operation"|
                   postOp_mortality_30day=="Dead within 30-day post-operation"|
                   postOp_mortality_30day=="Missing")) %>%
  dplyr::arrange(postOp_mortality_30day) %>% dplyr::ungroup() %>%
  dplyr::select(n_infection_none)
prop_subtotals_infection_none_postOp_mortality_30day <-
  n_subtotals_infection_none_postOp_mortality_30day / 
  table1_postOp_mortality_30day %>%
  dplyr::filter(era=="postCOVID surgery") %>%
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
  dplyr::filter(era=="postCOVID surgery") %>%
  dplyr::arrange(age_group_surgery) %>% dplyr::ungroup() %>% dplyr::select(n_infection_0to2wk)
prop_subtotals_infection_0to2wk_ageGroup <-
  n_subtotals_infection_0to2wk_ageGroup /
  table1_ageGroup %>%
  dplyr::filter(era=="postCOVID surgery") %>%
  select(n_infection_0to2wk) %>% sum()
# ## Sex.
n_subtotals_infection_0to2wk_Sex <-
  table1_Sex %>%
  dplyr::filter(era=="postCOVID surgery") %>%
  dplyr::arrange(Sex) %>% dplyr::ungroup() %>%
  dplyr::select(n_infection_0to2wk)
prop_subtotals_infection_0to2wk_Sex <-
  n_subtotals_infection_0to2wk_Sex /
  table1_Sex %>%
  dplyr::filter(era=="postCOVID surgery") %>%
  select(n_infection_0to2wk) %>% sum()
# ## 30-day post-operative mortality.
n_subtotals_infection_0to2wk_postOp_mortality_30day <-
  table1_postOp_mortality_30day %>%
  dplyr::filter(era=="postCOVID surgery",
                (postOp_mortality_30day=="Alive within 30-day post-operation"|
                   postOp_mortality_30day=="Dead within 30-day post-operation"|
                   postOp_mortality_30day=="Missing")) %>%
  dplyr::arrange(postOp_mortality_30day) %>% dplyr::ungroup() %>%
  dplyr::select(n_infection_0to2wk)
prop_subtotals_infection_0to2wk_postOp_mortality_30day <-
  n_subtotals_infection_0to2wk_postOp_mortality_30day / 
  table1_postOp_mortality_30day %>%
  dplyr::filter(era=="postCOVID surgery") %>%
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
  dplyr::filter(era=="postCOVID surgery") %>%
  dplyr::arrange(age_group_surgery) %>% dplyr::ungroup() %>% dplyr::select(n_infection_3to4wk)
prop_subtotals_infection_3to4wk_ageGroup <-
  n_subtotals_infection_3to4wk_ageGroup /
  table1_ageGroup %>%
  dplyr::filter(era=="postCOVID surgery") %>%
  select(n_infection_3to4wk) %>% sum()
# ## Sex.
n_subtotals_infection_3to4wk_Sex <-
  table1_Sex %>%
  dplyr::filter(era=="postCOVID surgery") %>%
  dplyr::arrange(Sex) %>% dplyr::ungroup() %>%
  dplyr::select(n_infection_3to4wk)
prop_subtotals_infection_3to4wk_Sex <-
  n_subtotals_infection_3to4wk_Sex /
  table1_Sex %>%
  dplyr::filter(era=="postCOVID surgery") %>%
  select(n_infection_3to4wk) %>% sum()
# ## 30-day post-operative mortality.
n_subtotals_infection_3to4wk_postOp_mortality_30day <-
  table1_postOp_mortality_30day %>%
  dplyr::filter(era=="postCOVID surgery",
                (postOp_mortality_30day=="Alive within 30-day post-operation"|
                   postOp_mortality_30day=="Dead within 30-day post-operation"|
                   postOp_mortality_30day=="Missing")) %>%
  dplyr::arrange(postOp_mortality_30day) %>% dplyr::ungroup() %>%
  dplyr::select(n_infection_3to4wk)
prop_subtotals_infection_3to4wk_postOp_mortality_30day <-
  n_subtotals_infection_3to4wk_postOp_mortality_30day / 
  table1_postOp_mortality_30day %>%
  dplyr::filter(era=="postCOVID surgery") %>%
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
  dplyr::filter(era=="postCOVID surgery") %>%
  dplyr::arrange(age_group_surgery) %>% dplyr::ungroup() %>% dplyr::select(n_infection_5to6wk)
prop_subtotals_infection_5to6wk_ageGroup <-
  n_subtotals_infection_5to6wk_ageGroup /
  table1_ageGroup %>%
  dplyr::filter(era=="postCOVID surgery") %>%
  select(n_infection_5to6wk) %>% sum()
# ## Sex.
n_subtotals_infection_5to6wk_Sex <-
  table1_Sex %>%
  dplyr::filter(era=="postCOVID surgery") %>%
  dplyr::arrange(Sex) %>% dplyr::ungroup() %>%
  dplyr::select(n_infection_5to6wk)
prop_subtotals_infection_5to6wk_Sex <-
  n_subtotals_infection_5to6wk_Sex /
  table1_Sex %>%
  dplyr::filter(era=="postCOVID surgery") %>%
  select(n_infection_5to6wk) %>% sum()
# ## 30-day post-operative mortality.
n_subtotals_infection_5to6wk_postOp_mortality_30day <-
  table1_postOp_mortality_30day %>%
  dplyr::filter(era=="postCOVID surgery",
                (postOp_mortality_30day=="Alive within 30-day post-operation"|
                   postOp_mortality_30day=="Dead within 30-day post-operation"|
                   postOp_mortality_30day=="Missing")) %>%
  dplyr::arrange(postOp_mortality_30day) %>% dplyr::ungroup() %>%
  dplyr::select(n_infection_5to6wk)
prop_subtotals_infection_5to6wk_postOp_mortality_30day <-
  n_subtotals_infection_5to6wk_postOp_mortality_30day / 
  table1_postOp_mortality_30day %>%
  dplyr::filter(era=="postCOVID surgery") %>%
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
  dplyr::filter(era=="postCOVID surgery") %>%
  dplyr::arrange(age_group_surgery) %>% dplyr::ungroup() %>% dplyr::select(n_infection_7wk)
prop_subtotals_infection_7wk_ageGroup <-
  n_subtotals_infection_7wk_ageGroup /
  table1_ageGroup %>%
  dplyr::filter(era=="postCOVID surgery") %>%
  select(n_infection_7wk) %>% sum()
# ## Sex.
n_subtotals_infection_7wk_Sex <-
  table1_Sex %>%
  dplyr::filter(era=="postCOVID surgery") %>%
  dplyr::arrange(Sex) %>% dplyr::ungroup() %>%
  dplyr::select(n_infection_7wk)
prop_subtotals_infection_7wk_Sex <-
  n_subtotals_infection_7wk_Sex /
  table1_Sex %>%
  dplyr::filter(era=="postCOVID surgery") %>%
  select(n_infection_7wk) %>% sum()
# ## 30-day post-operative mortality.
n_subtotals_infection_7wk_postOp_mortality_30day <-
  table1_postOp_mortality_30day %>%
  dplyr::filter(era=="postCOVID surgery",
                (postOp_mortality_30day=="Alive within 30-day post-operation"|
                   postOp_mortality_30day=="Dead within 30-day post-operation"|
                   postOp_mortality_30day=="Missing")) %>%
  dplyr::arrange(postOp_mortality_30day) %>% dplyr::ungroup() %>%
  dplyr::select(n_infection_7wk)
prop_subtotals_infection_7wk_postOp_mortality_30day <-
  n_subtotals_infection_7wk_postOp_mortality_30day / 
  table1_postOp_mortality_30day %>%
  dplyr::filter(era=="postCOVID surgery") %>%
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
                      "Alive within 30-day post-operation",
                      "Dead within 30-day post-operation", "Missing  ")
# Save data frame.
df_4wk_6mths <- df_4wk
write.csv(
  x = df_4wk_6mths,
  file = here::here("output","table1_4wk_onboarding_6mths.csv")
)
# Make kable table.
df_4wk_6mths %>%
  kableExtra::kbl(caption = paste0("<b>Table 1</b> Baseline characteristics and outcomes for ",
                       "patients undergoing surgery stratified by time from ",
                       "indication of SARS-CoV-2 infection. Values are ",
                       "counts and percentages. Cohort are patients who have a",
                       "record of a cancer diagnosis within 6 months before or",
                       "after their surgery.",
                       collapse = ""),
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
  #kableExtra::save_kable(file = here::here("output","Table1_4wk_onboarding_6mths.png"))
# ----



##########################
# Construct totals table #
##########################
# ----
rbind(
  rep(NA, 6), # Pre-pandemic, COVIDSurg.
  PP_OS_NC_totals, # Pre-pandemic, OpenSAFELY data, no-cancer patients.
  PP_OS_C_totals, # Pre-pandemic, OpenSAFELY data, cancer patients.
  COVIDSurg_totals,
  PNV_OS_NC_totals, # Pandemic no vaccines, OpenSAFELY data, no-cancer patients.
  PNV_OS_C_totals, # Pandemic no vaccines, OpenSAFELY data, cancer patients.
  rep(NA, 6), # Pandemic with vaccines, COVIDSurg.
  PWV_OS_NC_totals, # Pandemic with vaccines, OpenSAFELY data, no-cancer patients.
  PWV_OS_C_totals # Pandemic with vaccines, OpenSAFELY data, cancer patients.
  )
# ----

#############################
# Construct intervals table #
#############################
# ----
rbind(
  intervals_COVIDSurg_values,
  intervals_PNV_OS_NC_values,
  intervals_PNV_OS_C_values,
  rep(NA, 10),
  intervals_PNV_OS_NC_values,
  intervals_PNV_OS_C_values
  )
  
# ----


#######################
# Construct era table #
#######################
# ----
rbind(
  era_COVIDSurg_values,
  era_OS_NC_values,
  era_OS_C_values
)
# ----