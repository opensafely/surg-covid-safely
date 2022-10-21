
source(here::here("analysis","dataset_preparation.R"))
source(here::here("analysis","Make_table_COVIDSurg_compare.R"))
# Make Table1 for all surgery patients.
message("\nMaking Table1 for all surgery patients...")
data_to_use <- data_to_use_all
sensitivity_cohort <- "all"
source(here::here("analysis","Make_Table1.R"))
rm(data_to_use, sensitivity_cohort)
message("\n\t\t...table complete.\n\n")
# Make Table1 for patients who do not have a cancer diagnosis
# since the start of our data collection period (17th March 2018).
message(paste0("\nMaking Table1 for patients who do not have a cancer diagnosis ",
               "since the start of our data collection period (17th March 2018)..."))
data_to_use <- data_to_use_NC
sensitivity_cohort <- "NC"
source(here::here("analysis","Make_Table1.R"))
rm(data_to_use, sensitivity_cohort)
message("\n\t\t...table complete.\n\n")
# Make Table1 for patients whose cancer diagnosis was within
# 3 months of their surgery.
message(paste0("\nMaking Table1 for patients whose cancer diagnosis was within ",
               "3 months of their surgery...."))
data_to_use <- data_to_use_C_within3m
sensitivity_cohort <- "C_within3m"
source(here::here("analysis","Make_Table1.R"))
rm(data_to_use, sensitivity_cohort)
message("\n\t\t...table complete.\n\n")
# Make Table1 for patients whose cancer diagnosis was beyond
# 3 months of their surgery.
message(paste0("\nMaking Table1 for patients whose cancer diagnosis was beyond ",
               "3 months of their surgery...."))
data_to_use <- data_to_use_C_outwith3m
sensitivity_cohort <- "C_outwith3m"
source(here::here("analysis","Make_Table1.R"))
rm(data_to_use, sensitivity_cohort)
message("\n\t\t...table complete.\n\n")
# Make Table1 for patients whose cancer diagnosis was within
# 6 months of their surgery.
message(paste0("\nMaking Table1 for patients whose cancer diagnosis was within ",
               "6 months of their surgery...."))
data_to_use <- data_to_use_C_within6m
sensitivity_cohort <- "C_within6m"
source(here::here("analysis","Make_Table1.R"))
rm(data_to_use, sensitivity_cohort)
message("\n\t\t...table complete.\n\n")