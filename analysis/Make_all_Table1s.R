
source(here::here("analysis","dataset_preparation.R"))
# Make Table1 for all patients.
data_to_use <- myData
sensitivity_cohort <- "_EntireSurgeryCohort"
source(here::here("analysis","Make_Table1.R"))
# Make Table1 for patients whose cancer diagnosis was within
# 3 months of their surgery.
data_to_use <- myData %>% 
  dplyr::filter(category_cancer_within_3mths_surgery == 
                  "Cancer diagnosis within 3mths before surgery" |
                  category_cancer_within_3mths_surgery == 
                  "Cancer diagnosis within 3mths after surgery")
sensitivity_cohort <- "_3mths"
source(here::here("analysis","Make_Table1.R"))
# Make Table1 for patients whose cancer diagnosis was within
# 6 months of their surgery.
data_to_use <- myData %>% 
  dplyr::filter(category_cancer_within_6mths_surgery == 
                  "Cancer diagnosis within 6mths before surgery" |
                  category_cancer_within_6mths_surgery == 
                  "Cancer diagnosis within 6mths after surgery")
sensitivity_cohort <- "_6mths"
source(here::here("analysis","Make_Table1.R"))
rm(data_to_use, sensitivity_cohort)
# Make Table1 for patients who do not have a cancer diagnosis
# since the start of our data collection period (17th March 2018).
data_to_use <- myData %>% dplyr::filter(has_cancer == FALSE)
sensitivity_cohort <- "_CohortNoCancer"
source(here::here("analysis","Make_Table1.R"))
rm(data_to_use, sensitivity_cohort)


# Make Table1 for patients identified by:
# 1. having a cancer diagnosis within 3 months of their surgery.
# 2. not having any cancer diagnosis after the study start date, 2018-03-17.
data_to_use_C_within3m <- myData %>% 
  dplyr::filter(category_cancer_within_3mths_surgery == 
                  "Cancer diagnosis within 3mths before surgery" |
                  category_cancer_within_3mths_surgery == 
                  "Cancer diagnosis within 3mths after surgery")
data_to_use_C_outwith3m <- myData %>% 
  dplyr::filter(category_cancer_within_3mths_surgery == 
                  "No cancer diagnosis within 3mths before or after surgery")
data_to_use_NC <- myData %>% dplyr::filter(has_cancer == FALSE)
data_to_use_all <- myData 
source(here::here("analysis","Make_table_COVIDSurg_compare.R"))
