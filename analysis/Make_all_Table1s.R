
source(here::here("analysis","dataset_preparation.R"))

myDataSelect <- myData %>%
  dplyr::filter(postOp_mortality_30day %in% c("Dead within 30 days post-operation",
                                              "Alive within 30 days post-operation",
                                              "No death recorded"),
                preOperative_infection_status!=
                  "Error: Test result after surgery. Check study_definition.",
                era != "No surgery date recorded",
                COVIDSurg_data_collection_period != "No surgery date recorded")
data_to_use_all <- myDataSelect %>% dplyr::filter(has_surgery == TRUE)
data_to_use_NC <- myDataSelect %>% dplyr::filter(has_surgery == TRUE) %>%
  dplyr::filter(has_cancer == FALSE)
data_to_use_C_within3m <- myDataSelect %>% dplyr::filter(has_surgery == TRUE) %>% 
  dplyr::filter(has_cancer == TRUE) %>%
  dplyr::filter(category_cancer_within_3mths_surgery == 
                  "Cancer diagnosis within 3mths before surgery" |
                  category_cancer_within_3mths_surgery == 
                  "Cancer diagnosis within 3mths after surgery")
data_to_use_C_outwith3m <- myDataSelect %>% dplyr::filter(has_surgery == TRUE) %>% 
  dplyr::filter(has_cancer == TRUE) %>%
  dplyr::filter(category_cancer_within_3mths_surgery == 
                  "No cancer diagnosis within 3mths before or after surgery")
data_to_use_C_within6m <- myDataSelect %>% dplyr::filter(has_surgery == TRUE) %>% 
  dplyr::filter(has_cancer == TRUE) %>%
  dplyr::filter(category_cancer_within_6mths_surgery == 
                  "Cancer diagnosis within 3mths before surgery" |
                  category_cancer_within_6mths_surgery == 
                  "Cancer diagnosis within 3mths after surgery")
source(here::here("analysis","Make_table_COVIDSurg_compare.R"))
# Make Table1 for all surgery patients.
data_to_use <- data_to_use_all
sensitivity_cohort <- "all"
source(here::here("analysis","Make_Table1.R"))
rm(data_to_use, sensitivity_cohort)
# Make Table1 for patients who do not have a cancer diagnosis
# since the start of our data collection period (17th March 2018).
data_to_use <- data_to_use_NC
sensitivity_cohort <- "NC"
source(here::here("analysis","Make_Table1.R"))
rm(data_to_use, sensitivity_cohort)
# Make Table1 for patients whose cancer diagnosis was within
# 3 months of their surgery.
data_to_use <- data_to_use_C_within3m
sensitivity_cohort <- "C_within3m"
source(here::here("analysis","Make_Table1.R"))
rm(data_to_use, sensitivity_cohort)
# Make Table1 for patients whose cancer diagnosis was beyond
# 3 months of their surgery.
data_to_use <- data_to_use_C_outwith3m
sensitivity_cohort <- "C_outwith3m"
source(here::here("analysis","Make_Table1.R"))
rm(data_to_use, sensitivity_cohort)
# Make Table1 for patients whose cancer diagnosis was within
# 6 months of their surgery.
data_to_use <- data_to_use_C_within6m
sensitivity_cohort <- "C_within6m"
source(here::here("analysis","Make_Table1.R"))
rm(data_to_use, sensitivity_cohort)