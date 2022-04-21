# Make_vacc_table.R
#
# This script processes data from the myData dataframe to create a table
# that shows post-operative patient outcomes for cancer patients in groups
# defined by the duration between their surgery and the most-recent, prior
# indication of a SARS-CoV-2 infection.
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



#################################################
# Make tibbles that will inform the final table #
#################################################
# ----
myData_6mths_vacc <- myData

myData_6mths_vacc <- myData_6mths_vacc %>% 
  dplyr::filter(category_cancer_within_6mths_surgery == 
                  "Cancer diagnosis within 6mths before surgery" |
                  category_cancer_within_6mths_surgery == 
                  "Cancer diagnosis within 6mths after surgery")

# ## Count of patients in each of the categories for pre-operative infection
# ## status:
# ##    1. "No record of pre-operative SARS-CoV-2 infection"
# ##    2. "0-2 weeks record of pre-operative SARS-CoV-2 infection"
# ##    3. "3-4 weeks record of pre-operative SARS-CoV-2 infection"
# ##    4. ">=7 weeks record of pre-operative SARS-CoV-2 infection"
# ##    5. "Error: Test result after surgery. Check study_definition."
# ## ...stratified by...
# ## - surgery era:
# ##    1. "preCOVID sugery"
# ##    2. "postVaccine surgery" (although labelled "post", this means during, too)
# ##    3. "No surgery"
# ## - and whether or not the patient died within 30 days of their surgery:
# ##    1. "Alive within 30-day post-operation"
# ##    2. "Dead within 30-day post-operation" 
# ##    3. "Error: Surgery after death"
# ##    4. "No surgery recorded"
# ##    5. "No death recorded"
tableVacc_postOp_mortality_30day <- 
  myData_6mths_vacc %>% dplyr::group_by(surgery_pre_or_post_vaccine_UK,
                                                category_vaccination_status_before_test,
                                                postOp_mortality_30day) %>%
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
# ----

#######################################################################
# Ensure tibbles show zero values when categories are not in the data #
#######################################################################
# ----
tableVacc_postOp_mortality_30day <- 
  expand.grid(
    surgery_pre_or_post_vaccine_UK = 
      c("No surgery", "preVaccine surgery", "postVaccine surgery"),
    category_vaccination_status_before_test = 
      c("Confirmed fully vaccinated before test",
        "Confirmed partially vaccinated before test",
        "At least partially vaccinated before test",
        "Confirmed not vaccinated before test",
        "Unknown: No data for 1st dose and 2nd dose is FALSE or also missing",
        "Error: No data on vaccine administration",
        "Unknown vaccination status before test"),
    postOp_mortality_30day = 
      c("Alive within 30-day post-operation",
        "Dead within 30-day post-operation",
        "Error: Surgery after death",
        "No death recorded",
        "No surgery recorded",
        "Missing")) %>%
  dplyr::full_join(tableVacc_postOp_mortality_30day) %>%
  dplyr::arrange(surgery_pre_or_post_vaccine_UK) %>%
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
tableVacc_postOp_mortality_30day_6mths <- tableVacc_postOp_mortality_30day
write.csv(
  x = tableVacc_postOp_mortality_30day_6mths,
  file = here::here("output","tableVacc_postOp_mortality_30day_6mths.csv")
)
# ----

###########################################
# Make vectors to inform the kable table. #
###########################################
# ----
# Row names for later table.
TableRowNames <- 
tableVacc_postOp_mortality_30day %>%
  dplyr::filter(surgery_pre_or_post_vaccine_UK=="preVaccine surgery",
                (postOp_mortality_30day=="Dead within 30-day post-operation"|
                   postOp_mortality_30day=="Alive within 30-day post-operation" |
                   postOp_mortality_30day=="Missing")) %>%
  dplyr::arrange(category_vaccination_status_before_test) %>% dplyr::ungroup() %>%
  dplyr::select(postOp_mortality_30day)
# Pre-March 2020 totals.
# ## 30-day post-operative mortality.
n_preDec2020_Vacc_postOp_mortality_30day <-
  tableVacc_postOp_mortality_30day %>%
  dplyr::filter(surgery_pre_or_post_vaccine_UK=="preVaccine surgery",
                (postOp_mortality_30day=="Dead within 30-day post-operation"|
                   postOp_mortality_30day=="Alive within 30-day post-operation" |
                   postOp_mortality_30day=="Missing")) %>%
  dplyr::arrange(category_vaccination_status_before_test) %>% dplyr::ungroup() %>%
  dplyr::select(n_per_group)
prop_preDec2020_Vacc_postOp_mortality_30day <-
  n_preDec2020_Vacc_postOp_mortality_30day /
  tableVacc_postOp_mortality_30day %>%
  dplyr::filter(surgery_pre_or_post_vaccine_UK=="preVaccine surgery") %>%
  select(n_per_group) %>% sum()

# Post-March 2020 totals.
# ## 30-day post-operative mortality.
n_postDec2020_Vacc_postOp_mortality_30day <-
  tableVacc_postOp_mortality_30day %>%
  dplyr::filter(surgery_pre_or_post_vaccine_UK=="postVaccine surgery",
                (postOp_mortality_30day=="Dead within 30-day post-operation"|
                   postOp_mortality_30day=="Alive within 30-day post-operation" |
                   postOp_mortality_30day=="Missing")) %>%
  dplyr::arrange(category_vaccination_status_before_test) %>% dplyr::ungroup() %>%
  dplyr::select(n_per_group)
prop_postDec2020_Vacc_postOp_mortality_30day <-
  n_preDec2020_Vacc_postOp_mortality_30day /
  tableVacc_postOp_mortality_30day %>%
  dplyr::filter(surgery_pre_or_post_vaccine_UK=="postVaccine surgery") %>%
  select(n_per_group) %>% sum()

# No pre-operative infection.
# ## 30-day post-operative mortality.
n_subtotals_infection_none_Vacc_postOp_mortality_30day <-
  tableVacc_postOp_mortality_30day %>%
  dplyr::filter(surgery_pre_or_post_vaccine_UK=="postVaccine surgery",
                (postOp_mortality_30day=="Dead within 30-day post-operation"|
                   postOp_mortality_30day=="Alive within 30-day post-operation" |
                   postOp_mortality_30day=="Missing")) %>%
  dplyr::arrange(category_vaccination_status_before_test) %>% dplyr::ungroup() %>%
  dplyr::select(n_infection_none)
prop_subtotals_infection_none_Vacc_postOp_mortality_30day <-
  n_subtotals_infection_none_Vacc_postOp_mortality_30day /
  tableVacc_postOp_mortality_30day %>%
  dplyr::filter(surgery_pre_or_post_vaccine_UK=="postVaccine surgery") %>%
  select(n_infection_none) %>% sum()

# Pre-operative infection (0-2 weeks).
# ## 30-day post-operative mortality.
n_subtotals_infection_0to2wk_Vacc_postOp_mortality_30day <-
  tableVacc_postOp_mortality_30day %>%
  dplyr::filter(surgery_pre_or_post_vaccine_UK=="postVaccine surgery",
                (postOp_mortality_30day=="Dead within 30-day post-operation"|
                   postOp_mortality_30day=="Alive within 30-day post-operation" |
                   postOp_mortality_30day=="Missing")) %>%
  dplyr::arrange(category_vaccination_status_before_test) %>% dplyr::ungroup() %>%
  dplyr::select(n_infection_0to2wk)
prop_subtotals_infection_0to2wk_Vacc_postOp_mortality_30day <-
  n_subtotals_infection_0to2wk_Vacc_postOp_mortality_30day /
  tableVacc_postOp_mortality_30day %>%
  dplyr::filter(surgery_pre_or_post_vaccine_UK=="postVaccine surgery") %>%
  select(n_infection_0to2wk) %>% sum()

# Pre-operative infection (3-4 weeks).
# ## 30-day post-operative mortality.
n_subtotals_infection_3to4wk_Vacc_postOp_mortality_30day <-
  tableVacc_postOp_mortality_30day %>%
  dplyr::filter(surgery_pre_or_post_vaccine_UK=="postVaccine surgery",
                (postOp_mortality_30day=="Dead within 30-day post-operation"|
                   postOp_mortality_30day=="Alive within 30-day post-operation" |
                   postOp_mortality_30day=="Missing")) %>%
  dplyr::arrange(category_vaccination_status_before_test) %>% dplyr::ungroup() %>%
  dplyr::select(n_infection_3to4wk)
prop_subtotals_infection_3to4wk_Vacc_postOp_mortality_30day <-
  n_subtotals_infection_3to4wk_Vacc_postOp_mortality_30day /
  tableVacc_postOp_mortality_30day %>%
  dplyr::filter(surgery_pre_or_post_vaccine_UK=="postVaccine surgery") %>%
  select(n_infection_3to4wk) %>% sum()

# Pre-operative infection (5-6 weeks).
# ## 30-day post-operative mortality.
n_subtotals_infection_5to6wk_Vacc_postOp_mortality_30day <-
  tableVacc_postOp_mortality_30day %>%
  dplyr::filter(surgery_pre_or_post_vaccine_UK=="postVaccine surgery",
                (postOp_mortality_30day=="Dead within 30-day post-operation"|
                   postOp_mortality_30day=="Alive within 30-day post-operation" |
                   postOp_mortality_30day=="Missing")) %>%
  dplyr::arrange(category_vaccination_status_before_test) %>% dplyr::ungroup() %>%
  dplyr::select(n_infection_5to6wk)
prop_subtotals_infection_5to6wk_Vacc_postOp_mortality_30day <-
  n_subtotals_infection_5to6wk_Vacc_postOp_mortality_30day /
  tableVacc_postOp_mortality_30day %>%
  dplyr::filter(surgery_pre_or_post_vaccine_UK=="postVaccine surgery") %>%
  select(n_infection_5to6wk) %>% sum()

# Pre-operative infection (>=7 weeks).
# ## 30-day post-operative mortality.
n_subtotals_infection_7wk_Vacc_postOp_mortality_30day <-
  tableVacc_postOp_mortality_30day %>%
  dplyr::filter(surgery_pre_or_post_vaccine_UK=="postVaccine surgery",
                (postOp_mortality_30day=="Dead within 30-day post-operation"|
                   postOp_mortality_30day=="Alive within 30-day post-operation" |
                   postOp_mortality_30day=="Missing")) %>%
  dplyr::arrange(category_vaccination_status_before_test) %>% dplyr::ungroup() %>%
  dplyr::select(n_infection_7wk)
prop_subtotals_infection_7wk_Vacc_postOp_mortality_30day <-
  n_subtotals_infection_7wk_Vacc_postOp_mortality_30day /
  tableVacc_postOp_mortality_30day %>%
  dplyr::filter(surgery_pre_or_post_vaccine_UK=="postVaccine surgery") %>%
  select(n_infection_7wk) %>% sum()

# ----


#####################
# Make kable table. #
#####################
# ----
# Make data frame.
df_Vacc <- data.frame(
  # Row names.
  TableRowNames,
  # Pre-December 2020 totals, n.
  n_preDec2020_Vacc_postOp_mortality_30day,
  # Pre-December 2020 totals, %.
  round(prop_preDec2020_Vacc_postOp_mortality_30day * 100, 0),
  # Post-December 2020 totals, n.
  n_postDec2020_Vacc_postOp_mortality_30day,
  # Post-December 2020 totals, %.
  round(prop_postDec2020_Vacc_postOp_mortality_30day * 100, 0),
  # No pre-operative infection, n.
  n_subtotals_infection_none_Vacc_postOp_mortality_30day,
  # No pre-operative infection, %.
  round(prop_subtotals_infection_none_Vacc_postOp_mortality_30day * 100, 0),
  # Pre-operative infection (0-2 weeks), n.
  n_subtotals_infection_0to2wk_Vacc_postOp_mortality_30day,
  # Pre-operative infection (0-2 weeks), %.
  round(prop_subtotals_infection_0to2wk_Vacc_postOp_mortality_30day * 100, 0),
  # Pre-operative infection (3-4 weeks), n.
  n_subtotals_infection_3to4wk_Vacc_postOp_mortality_30day,
  # Pre-operative infection (3-4 weeks), %.
  round(prop_subtotals_infection_3to4wk_Vacc_postOp_mortality_30day * 100, 0),
  # Pre-operative infection (5-6 weeks), n.
  n_subtotals_infection_5to6wk_Vacc_postOp_mortality_30day,
  # Pre-operative infection (5-6 weeks), %.
  round(prop_subtotals_infection_5to6wk_Vacc_postOp_mortality_30day * 100, 0),
  # Pre-operative infection (>=7 weeks), n.
  n_subtotals_infection_7wk_Vacc_postOp_mortality_30day,
  # Pre-operative infection (>=7 weeks), %.
  round(prop_subtotals_infection_7wk_Vacc_postOp_mortality_30day * 100, 0)
)
# Label data frame.
colnames(df_Vacc) <- c(" ", rep(c("n", "%"),7))
# Make kable table.
df_Vacc %>%
  kableExtra::kbl(caption = paste0("<b>Table *</b> Thirty-day post-operative ",
                                   "mortality for cancer patients undergoing surgery. ",
                                   "Stratified by time from indication of ",
                                   "SARS-CoV-2 infection (columns) and by ",
                                   "vaccination status (rows). Values are counts ",
                                   "and percentages.", collapse = ""),
                  format = 'html') %>%
  kableExtra::kable_classic(full_width = F,
                            fixed_thead = T,
                            html_font="Cambria") %>%
  kableExtra::pack_rows(index = c("Confirmed fully vaccinated before test" = 3,
                                  "Confirmed partially vaccinated before test" = 3,
                                  "At least partially vaccinated before test" = 3,
                                  "Confirmed not vaccinated before test" = 3,
                                  "Unknown: No data for 1st dose and 2nd dose is FALSE or also missing" = 3,
                                  "Error: No data on vaccine administration" = 3,
                                  "Unknown vaccination status before test" = 3
  )) %>%
  kableExtra::add_header_above(c(" " = 7, "0-2 weeks" = 2, "3-4 weeks" = 2,
                                 "5-6 weeks" = 2, ">=7 weeks" = 2)) %>%
  kableExtra::add_header_above(c(" " = 1, "Pre-December 2020,\ntotals" = 2,
                                 "Post-December 2020,\ntotals" = 2,
                                 "No indication of infection" = 2,
                                 "\nIndication of infection" = 8)) %>%
  kableExtra::add_header_above(c(" " = 5, "Post-December 2020" = 10)) %>%
  kableExtra::column_spec(1, width = "25em") %>%
  kableExtra::column_spec(c(2:15), width = "5em") %>%
  kableExtra::row_spec(0, align = "c") #%>%
#kableExtra::save_kable(file = here::here("output","Tables_pretty","TableVacc_6mths.png"))

# Save data frame.
df_Vacc_6mths <- data.frame(" " = 
                              rep(c("Confirmed fully vaccinated before test",
                                    "Confirmed partially vaccinated before test",
                                    "At least partially vaccinated before test",
                                    "Confirmed not vaccinated before test",
                                    "Unknown: No data for 1st dose and 2nd dose is FALSE or also missing",
                                    "Error: No data on vaccine administration",
                                    "Unknown vaccination status before test"
                              ),
                              each = 3),
                            df_Vacc)
write.csv(
  x = df_Vacc_6mths,
  file = here::here("output","table_Vaccination_status_6mths.csv")
)
# ----
