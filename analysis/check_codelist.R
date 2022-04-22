# check_cohort.R
#
# This script counts the patients returned by the codelists for cancer defined
# using CTV3 (https://www.opencodelists.org/codelist/user/jkua/cancer/1d9bf8ff/)
# and using SNOMED-CT (https://www.opencodelists.org/codelist/user/ciaranmci/cancer-snomed-ct/4602666f/).
#
# The script also checks the component codelists that were used to create the
# amalgamated codelists mentioned above
#

library("tidyverse")
# Read data.
df_input <- read_csv(
  here::here("output", "input_codelist_check.csv")
)
myData <- df_input

check_cohort <- data.frame(
                          all_patients = nrow(myData),
                          has_cancer_SNOMED = length(which(myData$has_cancer_SNOMED == "1")),
                          has_cancer_CTV3 = length(which(myData$has_cancer_ctv3 == "1")),
                          has_cancer_SNOMED_combine = length(which(myData$has_cancer_SNOMED_combine == "1")),
                          has_cancer_CTV3_combine = length(which(myData$has_cancer_ctv3_combine == "1")),
                          has_cancer_SNOMED_Rcombine = length(which(myData$has_cancer_lung == "1" |
                                                             myData$has_cancer_haematological == "1" |
                                                             myData$has_cancer_excluding_lung_and_haematological == "1")),
                          has_cancer_CTV3_Rcombine = length(which(myData$has_cancer_lung_ctv3 == "1" |
                                                                      myData$has_cancer_haematological_ctv3 == "1" |
                                                                      myData$has_cancer_excluding_lung_and_haematological_ctv3 == "1")),
                          has_cancer_lung_SNOMED = length(which(myData$has_cancer_lung == "1")),
                          has_cancer_lung_CTV3 = length(which(myData$has_cancer_lung_ctv3 == "1")),
                          has_cancer_haematological_SNOMED = length(which(myData$has_cancer_haematological == "1")),
                          has_cancer_haematological_CTV3 = length(which(myData$has_cancer_haematological_ctv3 == "1")),
                          has_cancer_excl_SNOMED = length(which(myData$has_cancer_excluding_lung_and_haematological == "1")),
                          has_cancer_excl_CTV3 = length(which(myData$has_cancer_excluding_lung_and_haematological_ctv3 == "1"))
                          )


write.csv(
  x = check_cohort,
  file = here::here("output","check_codelist.csv")
)