# map surgery codes from SNOMED to OPCS4.R
#
# As the file name suggests, the purpose of this script is to map our surgery
# codes from SNOMED-CT to OPCS-4.10.
# This is needed so that we we can query the same surgery procedures using both
# patients.with_these_clinical_events() call and with the
# patients.admitted_to_hospital() call with the `with_these_procedures` argument.
#
# NOTE: This script is provided for review, only. The script will not run on the
# OpenSAFELY job sever because it requires the TRUD mapping table that must be 
# requested from NHS Digital. I do not have permission to upload the TRUD 
# mapping table to OpenSAFELY and the tables are regularly updated anyway, so
# you'll want the lastest one.
# You can find the latest TRUD mapping tables on the NHS Digital website at
# https://isd.digital.nhs.uk/trud/users/guest/filters/0/categories/26
#


list_of_packages <- c("tidyverse", "here", "qpcR")
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)
for (i in 1:length(list_of_packages))
{
  library(list_of_packages[i],character.only = T)
}

# Set location of the TRUD mapping table that is saved to the user's machine.
folder_loc <- choose.dir()

# Load our surgery codelist.
SNOMED_surgeries <-
  read_csv(here::here(
    "codelists",
    "/user-ciaranmci-surgery-covidsurg-replication-excluding-exclusions.csv"),
    col_types = cols(code = col_character()
                     )
    )
# Load the Abbot additions.
SNOMED_Abbott <-
  read_csv(here::here(
    "codelists",
    "/user-alwynkotze-surgery-covidsurg-replication-codes-from-abbott.csv"),
    col_types = cols(code = col_character()
                     )
    )
# Combine the two codelists.
surgeries <- SNOMED_surgeries %>% dplyr::bind_rows(SNOMED_Abbott)
# Load TRUD map.
TRUDmap <- read_delim(paste0(folder_loc, "\\SnomedCT_UKClinicalRF2_PRODUCTION_20220608T000001Z", "\\Full",
                             "\\RefSet", "\\Map", "\\der2_iisssciRefset_ExtendedMapUKCLFull_GB1000000_20220608.txt"),
                      delim = "\t", escape_double = FALSE,
                      col_types = cols(effectiveTime = col_character(),
                                       moduleId = col_character(),
                                       refsetId = col_character(),
                                       referencedComponentId = col_character()
                      ),
                      trim_ws = TRUE)
# Join the two list based on the SNOMED CT code.
mapping_join <- surgeries %>%
  dplyr::left_join(TRUDmap,
                   by = c("code" = "referencedComponentId")
  )
colnames(mapping_join)[1] <- "SNOMED_CT"
colnames(mapping_join)[12] <- "OPCS4_or_ICD10"
## *********************************************************************
## ** This little block of script might be useful for users in future **
## ** but OpenSAFELY reads OPCS-4.10 codes without the decimal point. **
## *********************************************************************
# # Convert OPCS4_or_ICD10 to XX.X format.
# mapping_join$OPCS4_or_ICD10 <-
#   ifelse(mapping_join$OPCS4_or_ICD10 != "#NIS",
#          sub("(.{3})(.*)", "\\1.\\2",
#              mapping_join$OPCS4_or_ICD10),
#          mapping_join$OPCS4_or_ICD10 )
## *********************************************************************
# Select out only the clinical codes for saving.
to_save <- mapping_join %>% dplyr::select(OPCS4_or_ICD10) %>%
  dplyr::filter(!OPCS4_or_ICD10 %in% c("#NIS", "#HLT", "#EPO", "#NC")) %>%
  unique() %>% data.frame()
# Save file.
write.csv(
  x = to_save,
  file = here::here("codelists","/our_surgeries_in_OPCS4.csv"),
  row.names = FALSE)
