
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
    
    # Get indication of patients who satisfy the cancer criterion (CTV3).
    has_cancer = patients.with_these_clinical_events(
			codelist_cancer,
			on_or_after = start_date
        ),
    # Get indication of patients who satisfy the surgery criterion.
    has_surgery = patients.with_these_clinical_events(
            codelist_cancer_surgery,
            on_or_after = start_date
        ),
    # Get indication of patients who satisfy the positive result criterion.
    has_test_preOp_SARS_CoV_2_outcome_positive = patients.with_test_result_in_sgss(
        pathogen = "SARS-CoV-2",
        find_last_match_in_period = True,
        returning = "binary_flag",
        test_result = "positive"  
        ),
        
    # Get indication of patients who satisfy the lung cancer criterion (SNOMED).
    has_cancer_lung = patients.with_these_clinical_events(
			codelist_cancer_lung,
			on_or_after = start_date
        ),
        
    # Get indication of patients who satisfy the haematological cancer criterion (SNOMED).
    has_cancer_haematological = patients.with_these_clinical_events(
			codelist_cancer_haematological,
			on_or_after = start_date
        ),
        
    # Get indication of patients who satisfy the cancer criterion,
    # excluding lung and haematological cancers (SNOMED).
    has_cancer_excluding_lung_and_haematological = patients.with_these_clinical_events(
			codelist_cancer_excluding_lung_and_haematological,
			on_or_after = start_date
        ),
) # End of StudyDefinition().