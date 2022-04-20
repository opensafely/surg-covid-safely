
#########################
## Imports.
#########################
# Obligatory OpenSAFELY import.
from cohortextractor import (
	StudyDefinition,
	patients, codelist,
	codelist_from_csv,
    codelist
)
# Import codelists.py script.
from codelists import *
# Import study_parameters.
#/////import study_parameters
# # Define variables explicitly from study_parameters
start_date = "2018-03-17" #study_parameters["start_date"]
end_date = "2022-01-01" #study_parameters["end_date"]

#########################
## Study definition.
#########################
study = StudyDefinition(
	default_expectations = {
		"date": {"earliest": start_date, "latest": end_date},
		"rate": "uniform",
		"incidence": 0.5,
	},
    
    # Get count of patients in entire database.
	population = patients.all(),
    
    # Get indication of patients who satisfy the cancer criterion (SNOMED).
    has_cancer_SNOMED = patients.with_these_clinical_events(
			codelist_cancer,
			on_or_after = start_date
        ),
    # Get indication of patients who satisfy the cancer criterion (CTV3).
    has_cancer_ctv3 = patients.with_these_clinical_events(
			codelist_cancer_ctv3,
			on_or_after = start_date
        ),
    
    # Get indication of patients who satisfy the lung cancer criterion (SNOMED).
    has_cancer_lung = patients.with_these_clinical_events(
			codelist_cancer_lung,
			on_or_after = start_date
        ),
    # Get indication of patients who satisfy the lung cancer criterion (CTV3).
    has_cancer_lung_ctv3 = patients.with_these_clinical_events(
			codelist_cancer_lung_ctv3,
			on_or_after = start_date
        ),
       
    # Get indication of patients who satisfy the haematological cancer criterion (SNOMED).
    has_cancer_haematological = patients.with_these_clinical_events(
			codelist_cancer_haematological,
			on_or_after = start_date
        ),
    # Get indication of patients who satisfy the haematological cancer criterion (CTV3).
    has_cancer_haematological_ctv3 = patients.with_these_clinical_events(
			codelist_cancer_haematological_ctv3,
			on_or_after = start_date
        ),
            
    # Get indication of patients who satisfy the cancer criterion,
    # excluding lung and haematological cancers (SNOMED).
    has_cancer_excluding_lung_and_haematological = patients.with_these_clinical_events(
			codelist_cancer_excluding_lung_and_haematological,
			on_or_after = start_date
        ),
    # Get indication of patients who satisfy the cancer criterion,
    # excluding lung and haematological cancers (CTV3).
    has_cancer_excluding_lung_and_haematological_ctv3 = patients.with_these_clinical_events(
			codelist_cancer_excluding_lung_and_haematological_ctv3,
			on_or_after = start_date
        ),
    
    
    # Get indication of patients who satisfy the cancer criterion (SNOMED),
    # using combine_codelists().
    has_cancer_SNOMED_combine = patients.with_these_clinical_events(
			codelist_cancer_combine,
			on_or_after = start_date
        ),
    # Get indication of patients who satisfy the cancer criterion (CTV3),
    # using combine_codelists().
    has_cancer_ctv3_combine = patients.with_these_clinical_events(
			codelist_cancer_ctv3_combine,
			on_or_after = start_date
        ),
    
) # End of StudyDefinition().