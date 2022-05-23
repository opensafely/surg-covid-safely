from cohortextractor import (codelist_from_csv, combine_codelists)

## #####################
## Cohort definition
## #####################

# Patients with a cancer diagnosis (SNOMED)
# (https://www.opencodelists.org/codelist/user/ciaranmci/cancer-snomed-ct/4602666f/).
codelist_cancer = codelist_from_csv(
   "codelists/user-ciaranmci-cancer-snomed-ct.csv",
   system="snomed",
   column="code",
)
# Patients with a cancer diagnosis (CTV3)
# (https://www.opencodelists.org/codelist/user/jkua/cancer/1d9bf8ff/).
codelist_cancer_ctv3 = codelist_from_csv(
   "codelists/user-jkua-cancer.csv",
   system="ctv3",
   column="code",
)

# Patients undergoing surgery
# (https://www.opencodelists.org/codelist/user/ciaranmci/surgery-covidsurg-replication-excluding-exclusions/5c09dd62/).
codelist_cancer_surgery = codelist_from_csv(
    "codelists/user-ciaranmci-surgery-covidsurg-replication-excluding-exclusions.csv",
    system="snomed",
    column="code",
)

# OPCS_codelist_cancer_surgery = codelist_from_csv(
    # "codelists/user-ciaranmci-?????????.csv",
    # system="OPCS",
    # column="code",
# )

# First COVID vaccination administerd (https://www.opencodelists.org/codelist/primis-covid19-vacc-uptake/covadm1/v1/)
codelist_COVID_first_vaccination = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-covadm1.csv",
    system="snomed",
    column="code",
)

# Second COVID vaccination administerd (https://www.opencodelists.org/codelist/primis-covid19-vacc-uptake/covadm2/v1/)
codelist_COVID_second_vaccination = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-covadm2.csv",
    system="snomed",
    column="code",
)

# First COVID vaccination declined (https://www.opencodelists.org/codelist/primis-covid19-vacc-uptake/cov1decl/v1.1/)
codelist_COVID_first_vaccination_declined = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-cov1decl.csv",
    system="snomed",
    column="code",
)

# Second COVID vaccination declined (https://www.opencodelists.org/codelist/primis-covid19-vacc-uptake/cov2decl/v1.1/)
codelist_COVID_second_vaccination_declined = codelist_from_csv(
    "codelists/primis-covid19-vacc-uptake-cov2decl.csv",
    system="snomed",
    column="code",
)

# Patients with a cancer diagnosis, excluding lung and haematological cancers (SNOMED)
# (https://www.opencodelists.org/codelist/opensafely/cancer-excluding-lung-and-haematological-snomed/2020-04-15/).
codelist_cancer_excluding_lung_and_haematological = codelist_from_csv(
   "codelists/opensafely-cancer-excluding-lung-and-haematological-snomed.csv",
   system="snomed",
   column="id",
)
# Patients with a cancer diagnosis, excluding lung and haematological cancers (CTV3)
# (https://www.opencodelists.org/codelist/opensafely/cancer-excluding-lung-and-haematological/2020-04-15/).
codelist_cancer_excluding_lung_and_haematological_ctv3 = codelist_from_csv(
   "codelists/opensafely-cancer-excluding-lung-and-haematological.csv",
   system="ctv3",
   column="CTV3ID",
)

# Patients with a lung cancer diagnosis (SNOMED)
# (https://www.opencodelists.org/codelist/opensafely/lung-cancer-snomed/2020-04-15/).
codelist_cancer_lung = codelist_from_csv(
   "codelists/opensafely-lung-cancer-snomed.csv",
   system="snomed",
   column="id",
)
# Patients with a lung cancer diagnosis (CTV3)
# (https://www.opencodelists.org/codelist/opensafely/lung-cancer/2020-04-15/).
codelist_cancer_lung_ctv3 = codelist_from_csv(
   "codelists/opensafely-lung-cancer.csv",
   system="ctv3",
   column="CTV3ID",
)

# Patients with a haematological cancer diagnosis (SNOMED)
# (https://www.opencodelists.org/codelist/opensafely/haematological-cancer-snomed/).
codelist_cancer_haematological = codelist_from_csv(
   "codelists/opensafely-haematological-cancer-snomed.csv",
   system="snomed",
   column="id",
)
# Patients with a haematological cancer diagnosis (CTV3)
# (https://www.opencodelists.org/codelist/opensafely/haematological-cancer/2020-04-15/.
codelist_cancer_haematological_ctv3 = codelist_from_csv(
   "codelists/opensafely-haematological-cancer.csv",
   system="ctv3",
   column="CTV3ID",
)


# Patients with a cancer diagnosis (SNOMED) using codelist_combine().
# (https://www.opencodelists.org/codelist/user/ciaranmci/cancer-snomed-ct/4602666f/).
codelist_cancer_combine = combine_codelists(
   codelist_cancer_excluding_lung_and_haematological,
   codelist_cancer_lung,
   codelist_cancer_haematological
)
# Patients with a cancer diagnosis (CTV3) using codelist_combine().
# (https://www.opencodelists.org/codelist/user/jkua/cancer/1d9bf8ff/).
codelist_cancer_ctv3_combine = combine_codelists(
   codelist_cancer_excluding_lung_and_haematological_ctv3,
   codelist_cancer_lung_ctv3,
   codelist_cancer_haematological_ctv3
)

# #####################
# Exposure
# #####################
# No codelits needed.


# #####################
# Outcomes
# #####################
# ## Outcomes used in the original COVIDSurg study.

## Post-operative mortality.
# No codelists needed.

### 30-day post-operative pulmonary complications.
## Defined by the presence of any codes for pneumonia,
## acute respiratory distress syndrome, or unexpected
## post-operative ventilation.
#
## Pneumonia.
#codelist_pneumonia = codelist_from_csv(
#    "codelists/codelist-pneumonia.csv",
#    system="snomed",
#    column="code",
#)
## Acute respiratory distress syndrome, ARDS.
#codelist_ARDS = codelist_from_csv(
#    "codelists/codelist-ARDS.csv",
#    system="snomed",
#    column="code",
#)
## Unexpected post-operative ventilation.
#codelist_unexpected_ventilation = codelist_from_csv(
#    "codelists/codelist-unexpected-ventilation.csv",
#    system="snomed",
#    column="code",
#)
## Post-operative pulmonary complications.
#codelist_pulmonary_complications = combine_codelists(
#    codelist_pneumonia,
#    codelist_ARDS,
#    codelist_unexpected_ventilation,
#)
#
## ## Outcomes in addition to those used in the original
## ## COVIDSurg study.
#
### 30-day post-operative cardiac complications.
## Defined by...
#
### Post-operative *<indicator of post-operative cardiac complications>.
#codelist_* = codelist_from_csv(
#    "codelists/codelist-*.csv",
#    system="snomed",
#    column="code",
#)
### Post-operative **<indicator of post-operative cardiac complications>.
#codelist_** = codelist_from_csv(
#    "codelists/codelist-**.csv",
#    system="snomed",
#    column="code",
#)
### Post-operative cardiac complications.
#codelist_cardiac_complications = combine_codelists(
#    codelist_*,
#    codelist_**,
#)
#
# 30-day post-operative cerebrovascular complications, as indicated by TIA and stroke.
codelist_cerebrovascular_complications = codelist_from_csv(
   "codelists/user-alwynkotze-stroke.csv",
   system="snomed",
   column="code",
)


################
## Covariates ##
################
## Revised Cardiac Risk Index
# Where COVIDSurg used the Revised Cardiac Risk Index, we 
# stratify based on the component conditions.

# Chronic cardiac disease.
codelist_chronic_cardiac_disease = codelist_from_csv(
   "codelists/opensafely-chronic-cardiac-disease-snomed.csv",
   system="snomed",
   column="id",
)
# Diabetes.
codelist_diabetes = codelist_from_csv(
   "codelists/opensafely-diabetes-snomed.csv",
   system="snomed",
   column="id",
)

## Presence of respiratory comorbidities.
# Where COVIDSurg defined the presence of respiratory comorbidities
# as the presence of either asthma or COPD, we use use an OpenSAFELY
# codelist for chronic respiratory disease.

# Chronic respiratory disease.
codelist_chronic_respiratory_disease = codelist_from_csv(
 "codelists/opensafely-chronic-respiratory-disease.csv",
 system="ctv3",
 column="CTV3ID",
)
