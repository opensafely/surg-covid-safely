
source(here::here("analysis","dataset_preparation.R"))
# Make Table1 for all patients.
data_to_use <- myData
sensitivity_cohort <- ""
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
