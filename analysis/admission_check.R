#
#
# This script is part of a debugging exercise. The purpose of this script is to
# check the overlap between elective/emergency admissions as indicated by
# HES codes, and the "day case" indication that is part of
# 'patient_classification'.
#

# Process th raw input.
source(here::here("analysis","dataset_preparation.R"))

# mydata
tbl_admission_check <-
  myData %>%
  dplyr::select(category_admission_method,
                admission_method_patient_classification) %>%
  table()

# Save output.
write.csv(
  x = tbl_admission_check,
  file = here::here("output",
                    "tbl_admission_check.csv")
)