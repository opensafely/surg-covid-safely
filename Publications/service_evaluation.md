__Type of article__:
Original Research


__Submitting author__:
Ciarán McInerney,
School of Computing,
The University of Leeds,
Sir William Bragg Building, Woodhouse, Leeds, England

Post-operative mortality and pulmonary complications in surgical patients with and without peri-operative SARS-CoV2 infection or cancer: A service evaluation of 24 million patient records using OpenSAFELY

C. D. McInerney,<sup>1,2</sup>* A. Kotzé,<sup>3</sup>* J. Cutting,<sup>4</sup> L. Fisher,<sup>5</sup> J. Kua,<sup>6</sup> D. McGuckin,<sup>7</sup> S. Ramani Moonesinghe,<sup>8</sup> Owen A. Johnson<sup>9</sup>

1. Research Fellow in Patient Safety and Digital Health, School of Computing, University of Leeds, Leeds, England
2. Research Fellow in Patient Safety and Digital Health, National Institute for Health Research Yorkshire and Humber Patient Safety Translational Research Centre
3. Consultant Anaesthetist, Leeds Teaching Hospitals NHS Trust, Leeds, England
4. …
5. Bennett Institute for Applied Data Science, Nuffield Department of Primary Care Health Sciences, University of Oxford, OX2 6GG, UK
6. …
7. …
8. …
9. Senior Teaching Fellow, School of Computing, University of Leeds, Leeds, England
\* C.D. McInerney & A. Kotzé are joint first authors.

__Correspondence to__: Ciarán McInerney<br />
__Email__: c.mcinerney@leeds.ac.uk<br />
__Twitter__: @CMc_PhD; @alwynkotzee; n/a<br />
__ORCID__:<br />
Jonathan Cutting: <br />
Louis Fisher: 0000-0002-0295-3812 <br />
Owen A. Johnson: 0000-0003-3998-541X<br />
Alwyn Kotzé: 0000-0002-9310-2895<br />
Justin Kua: <br />
Dermot McGurkin: <br />
Ciarán D. McInerney: 0000-0001-7620-7110<br />
S. Ramani Moonesinghe: 0000-0002-6730-5824<br />

__Short title__: Post-operative mortality and SARS-CoV2 infection

 
# Abstract
Patients with surgical problems must balance the risks of undergoing surgery against the risks of non-surgical alternatives. The decision to undergo surgery is complicated by the presence of comorbidity, current or recent illness including SARS-CoV2 infection, and whether their surgical problem is time-sensitive or not. Initial guidance in the UK suggested delaying all non-urgent elective operations for three months following an indication of SARS-CoV-2 infection. With the approval of NHS England, we conducted a service evaluation describing 1) the percentage of cancer-related surgeries conducted <43 days from a positive polymerase chain reaction assays, in a cohort of patients from the UK’s Pillar 1 and Pillar 2 testing routes, and 2) cancer-related surgery patients’ post-operative outcomes before and during the COVID-19 pandemic. Our service evaluation suggests that NHS England followed the principle of the initial guidance from March 2020 by operating on very few Pillar 1 and Pillar 2 cancer patients within seven weeks of a positive indication of infection from SARS-CoV-2 from polymerase chain reaction assays. We also found that patient outcomes were better for NHS England compared to the COVIDSurg Collaborative global averages. This leads us to cautiously recommend that NHS England should reduce the 7-week threshold while monitoring outcomes.
 
# Introduction
Patients with surgical problems must balance the risks of undergoing surgery against the risks of non-surgical alternatives. The decision to undergo surgery is complicated by the presence of comorbidity, current or recent illness including SARS-CoV2 infection, and whether their surgical problem is time-sensitive or not (e.g. cancer) [[1]](#1). In 2020, the COVIDSurg collaborative found that patients with perioperative SARS-CoV-2 infection have greater unadjusted mortality than even the highest-risk subgroups of the UK’s National Emergency Laparotomy Audit [2]. Perioperative indication of SARS-CoV-2 infection was associated with additional mortality risk for up to 7 weeks in patients who recover fully [2], but the increased risk persisted beyond 7 weeks in those still experiencing symptoms. A similar pattern was observed for the occurrence of post-operative pulmonary complications [2]. Subsequent guidance in the United Kingdom thus constrained the scheduling of deferrable surgery [3]. Scheduling considerations were part of a wide disruption of surgical services that has increased waiting lists in England alone by over 2 million patients since 2020 [4], contributed to avoidable mortality in cancer patients [5] and affected patients mentally and financially [6].<br />

As of July 2022, UK guidance remains to postpone elective surgery for at least 7 weeks after SARS-CoV-2 infection unless the surgical problem is time sensitive [7]. This is largely based on the COVIDSurg observational study [8]. However, guidance has been considerably more varied; initial guidance in the UK suggested delaying all non-urgent elective operations for three months [9] but a descriptive observational study of 30-day post-operative outcomes suggested that improved outcomes were associated with > 4-week delays in elective surgeries, in patients with a previous positive SARS-CoV-2 test [10]. In contrast, a model of mortality risk given SARS-CoV-2 infection suggested that the risk of death following elective orthopaedic surgery was less than 1:1000 [11] – by comparison, a 2021 meta-analysis estimated the global SARS-CoV-2 case fatality rate to be 2.67% with a 95% confidence interval of 2.25-3.13% [12]. Kovoor et al. showed notable heterogeneity in the advice provided by five expert consensus guidance documents regarding the period of delay for adequate recovery of patients following SARS-CoV-2 infection before undergoing surgery [13]. Nevertheless, multidisciplinary consensus statements based on select research continued to be communicated, e.g. [3,8]. This conflicting evidence makes it difficult for hospitals to safely plan surgeries, especially because the reasons for the heterogeneity are not clear. It is also not clear to what extent NHS hospitals enacted the guidance and how the possible variance in enactment was associated with patient outcomes. 

Furthermore, the COVIDSurg study [2]  was conducted before COVID vaccines became widely available. Since then, evidence suggests that full courses of vaccines have been variously efficacious against symptomatic SARS-CoV-2 infection, at individual level, in at-risk populations [14–16]. Although randomised controlled trials have yet to be designed or powered to assess whether the vaccines prevent deaths [17], mathematical modelling suggests vaccination has ameliorated the public health impact of the pandemic in at-risk populations [18]. A propensity-matched observational study of 30,681 patients from 1,255 US hospitals further showed lower rates of post-operative SARS-CoV-2 infection and post-operative morbidity in vaccinated patients [19]. 

It is in this context that we identified a need to evaluate the provision of timely and safe surgery during the COVID-19 pandemic both before and after vaccines became available, and clarify the relationship between delays and post-operative outcomes. We therefore conducted a retrospective observational study of 24 million linked primary and secondary care health records (the OpenSAFELY platform) from across England. Our aims were to establish:
1.	To what extent English hospitals scheduled surgery within 7 weeks from a SARS-CoV-2 diagnosis (a proxy for the extent to which current guidance was followed)
2.	Describe postoperative outcomes, stratified by:<br />
    a.	Time between surgery and a SARS-CoV-2 infection, or no infection<br />
    b.	Surgery conducted around the time of a cancer diagnosis (as proxy for cancer being the indication for surgery)<br />
    c.	Surgery before and after widespread vaccine availability.

# Methods
## Study design
Our proposed service evaluation followed an approach similar to the COVIDSurg Collaborative service evaluations. Specifically, we adapted the protocol of the main study entitled “Timing of surgery following SARS-CoV-2 infection: an international prospective cohort study” [20]. We analysed how cancer-related surgery had actually been scheduled with regards to timing after an indication of SARS-CoV-2 infection. Prior to this service evaluation, we knew what the recommendations were but we don't know what was actually done. They key variable upon which we stratified was the duration between an indication for SARS-CoV-2 infection and the patient’s date of surgery. We hope to help describe the optimal interval between SARS-CoV-2 infection and subsequent cancer surgery.

## Ethical approval

This study was conducted as part of a service evaluation with sponsorship from NHS England (Prof Ramani Moonesinghe – National Clinical Director for Critical and Perioperative Care, NHS England and NHS Improvement – as senior sponsor); therefore, institutional ethical approval was not required. Nevertheless, this work was approved by the University of Leeds Faculty for Engineering and Physical Sciences Ethics Committee (reference MEEC 21-005). 

NHS England is the data controller for OpenSAFELY-EMIS and OpenSAFELY-TPP. EMIS and TPP are the data processors. All study authors using OpenSAFELY have the approval of NHS England. This implementation of OpenSAFELY is hosted within the EMIS and TPP environments, which are accredited to the ISO 27001 information security standard and are NHS IG Toolkit compliant [21,22].

## Study population

Our population of interest is patients who underwent surgery and a subgroup who also had a cancer diagnosis within 3 months before or after their surgery date. The exact list of SNOMED-CT clinical codes used to identify patients with cancer are available at https://opencodelists.org [23]. To define “surgery”, we combined all codes in the SNOMED-CT tree pertaining to “surgery” or “surgical procedure” but excluded all terms in the COVIDSurg collaborative’s list of excluded procedures [24]. Using NHS Digital’s mapping tables (Technology Reference Update Distribution, TRUD [25]), the resultant SNOMED-CT code list was converted to OPCS 4.9 and compared to the “Intermediate” definition of surgery as described by Abbott et al. [26] to identify any missed procedure codes. The resultant combined list was then backwards-mapped to SNOMED-CT to conduct the patient records queries. All codelists are publically available at https://opencodelists.org for inspection and reuse by the wider research community.

Importantly, because our population was defined based on surgical events, only patients from the OpenSAFELY-TPP database were available for querying but not patients from the OpenSAFELY-EMIS database. At time of writing, this includes patients from more than 2,600 GP practices and a third of acute mental health trusts in the UK.

In the original COVIDSurg study [20], cancer patients were a subgroup defined as any patient undergoing surgery for which cancer was the indication. This implied an exclusion of patients for whom the surgery was an investigation for subsequent cancer diagnosis, i.e. patients for whom the date of cancer diagnosis followed rather than preceded the surgery. Because our service evaluation focused on cancer-related surgery (both investigative and for treatment), we allowed 3 months for a cancer diagnosis to be recorded in the patients’ electronic health record following surgery. Our ±3-month threshold was based on preliminary review of patient records that suggested that pre-surgery cancer diagnoses typically occurred within 2-2.5 months, and post-surgery cancer diagnoses typically occurred within 1 month (especially in the cases were the surgery was decisive for the diagnosis).

As part of a sensitivity analysis, we also identified patients based on a ±6-month threshold. This was to accommodate the possibility of administrative delays during the COVID period.

## Data source
The proposed study project studies historic, routinely-collected healthcare records. This service evaluation accessed data via the OpenSAFELY platform. All data were linked, stored and analysed securely within the OpenSAFELY platform [27]. Data include pseudonymised data such as coded diagnoses, medications and physiological parameters. No free text data are included. All code is shared openly for review and re-use under MIT open license (https://github.com/opensafely/surg-covid-safely). Detailed pseudonymised patient data is potentially re-identifiable and therefore not shared.

As stated on their website, OpenSAFELY is a highly-secure, transparent, open-source software platform for analysis of electronic health records data from the NHS primary-care patient-record systems, and other relevant national databases. The exact count of patients used in this service evaluation was determined based on the data return from the query submitted to OpenSAFELY. Our queries returned XXX  patients.

## Data governance
Patient data has been pseudonymised for analysis and linkage using industry standard cryptographic hashing techniques; all pseudonymised datasets transmitted for linkage onto OpenSAFELY are encrypted; access to the platform is via a virtual private network connection, restricted to a small group of researchers; the researchers hold contracts with NHS England and only access the platform to initiate database queries and statistical models; all database activity is logged; only aggregate statistical outputs leave the platform environment following best practice for anonymisation of results such as statistical disclosure control for low cell counts [28].

The OpenSAFELY research platform adheres to the obligations of the UK General Data Protection Regulation and the Data Protection Act 2018. In March 2020, the Secretary of State for Health and Social Care used powers under the UK Health Service (Control of Patient Information) Regulations 2002 (COPI) to require organisations to process confidential patient information for the purposes of protecting public health, providing healthcare services to the public and monitoring and managing the COVID-19 outbreak and incidents of exposure; this sets aside the requirement for patient consent [29]. This was extended in July 2022 for the NHS England OpenSAFELY COVID-19 research platform [30]. In some cases of data sharing, the common law duty of confidence is met using, for example, patient consent or support from the Health Research Authority Confidentiality Advisory Group [31].

Taken together, these provide the legal bases to link patient datasets on the OpenSAFELY platform. GP practices, from which the primary care data are obtained, are required to share relevant health information to support the public health response to the pandemic, and have been informed of the OpenSAFELY analytics platform.

## Data management
Data management was performed using Python (v3.8.2) and R (v4.0.2), with analysis carried out using R. Code for data management and analysis, as well as codelists, are archived online (https://github.com/opensafely/surg-covid-safely).

## Outcomes
Below are the outcomes used in the COVIDSurg study on which we based our service evaluation [20]:
1.	All-cause, 30-day, post-operative mortality; and
2.	30-day post-operative pulmonary complications.

Mortality was informed by the date of death provided by the Office of National Statistics.

Additionally, we described the following outcomes:
1.	All-cause, post-operative mortality at 6 months;
2.	30-day post-operative cardiac complications; and
3.	30-day post-operative cerebrovascular complications.

The exact list of SNOMED-CT clinical codes used to identify post-operative cardiac and cerebrovascular complications are publically available at https://opencodelists.org.

## Exposure
We were not able to measure delays in surgery directly because originally-planned dates of surgery were not available in the OpenSAFELY dataset. As a proxy, we used the intervals defined in the COVIDSurg study [21], which were intended to measure the delay phenomenon. Our exposure of interest was the interval between a patient’s most-recent indication of SARS-CoV-2 and their subsequent cancer-related surgery. While COVIDSurg calculated intervals in weeks, we calculated intervals in units of days and modelled as a categorical variable, i.e. {no pre-operative indication of SARS-CoV-2 infection, ≤14 days, 15-28 days, 29-42 days, ≥43 days}. Patients were categorised as no pre-operative indication of SARS-CoV-2 infection if the most-recent PCR assay before their surgery returned a negative result.

Importantly, OpenSAFELY indicates SARS-CoV-2 infection with results from polymerase chain reaction (PCR), nasal swab assays for genetic material, from the UK’s Pillar 1 and Pillar 2 testing routes. Pillar 1 and Pillar 2 refer to tests for those with a clinical need, and health and care worker (Pillar 1), and for critical key workers in the NHS, social care and other sectors (Pillar 2) [32]. Indications of SARS-CoV-2 infection that are not included are serology testing (Pillar 3), and serology and swab testing for national surveillance (Pillar 4). Therefore, our evaluations indication of SARS-CoV-2 infection does not consider antigens nor antibodies associated with a SARS-CoV-2 infection, nor does it include lay individuals not part of Pillar 1 or Pillar 2 testing. We did not expect any selection bias to be associated with the selective testing because all surgery patients fell under the Pillar 1 remit, i.e. they had a clinical need.

## Covariates for stratification
The covariates across which we stratified patients were:
1.	Sex {Male, Female};
2.	Pre-operative chronic cardiac disease {Yes, No};
3.	Pre-operative diabetes {Yes, No};
4.	Pre-operative chronic respiratory disease {Yes, No};
5.	Timing of cancer diagnosis {Within 3 months of surgery, Outwith 3 months of surgery, No cancer diagnosis}; _and_
6.	Admission method {elective, emergency}.

Where the COVIDSurg study [20] stratified on Revised Cardiac Risk Index from 1 to 6, we stratified based on the presence of chronic cardiac disease [33] and on the presence of diabetes [34]. Where the COVIDSurg study [20] defined the presence of respiratory conditions as either asthma or chronic obstructive pulmonary disease, we define the presence of respiratory conditions as asthma [35] or other chronic respiratory conditions [36]. Where the COVIDSurg study [20] defined the indication for surgery as any of {benign disease, cancer, obstetrics, trauma}, we focus on surgeries in cancer patients so stratify patients as either having a cancer diagnosis within ±3 months of their surgery, outwith ±3 months of their surgery, or no cancer diagnosis since the study start date (i.e. 17th March 2018). Where COVIDSurg Collaborative defined urgency of surgery as surgery on either emergency or elective admission, we retitle the variable as “Admission method” and define it on the basis of NHS Hospital Episode Statistics [37]. Surgeries under admissions that did not match emergency or elective NHS Hospital Episode Statistics codes were labelled “Unknown”. The exact list of SNOMED-CT clinical codes used to identify patients’ strata are publicly available at https://opencodelists.org.

We calculated counts and percentages of patients in strata of these covariates to provide some clinical context for the cohorts. We assumed all covariates confound the relationship between between the SARS-CoV-2–surgery interval and all outcomes. Further context is provided by the era variable that indicates whether the surgeries under study were conducted before the pandemic in the UK (pre-pandemic: 17th March 2018-17th March 2020), during the pandemic before vaccines were considered effective in the UK (pandemic-no-vaccine: 18th March 2020-12th Jan 2021), during the same 4-week period as the COVIDSurg data collection within the pandemic-no-vaccine era (COVIDSurg data collection period: 5th October 2020-1st November 2020), or during the pandemic when vaccines were considered effective in the UK (pandemic-with-vaccine: 12th Jan 2021-17th March 2022). We considered the vaccines to have become effective in the UK after five weeks from when they were available (8th December 2020) assuming three weeks for patients to have received the first dose and two weeks for the dose to have become effective [15]. The beginning of the per-pandemic era and the end of the pandemic-with-vaccine era were defined the study start and end date based on ±2 years from the announcement by NHS England that all ono-urgent operations should be postponed, i.e. 17th March 2020 [9].

We do not present OpenSAFELY data for stratifications by age group because of the risk of disclosure arising from small sample sizes in some stratifications of some age groups.


# Results
Figure X7wkX shows 3-monthly percentages of surgeries conducted <7 weeks (<43 days) from a positive PCR assay, from the UK’s Pillar 1 and Pillar 2 testing routes. In any 3-month window, less than 3% of surgeries were conducted within the 7-week (43-day) threshold suggested by the COVIDSurg studies. This was true for the patients without a cancer diagnosis and for patients with a cancer diagnosis either ±3 or ±6 months from their surgery date.

<table>
    <tr>
      <td><b>A</b></td>
      <td><b>B</b></td>
    </tr>
    <tr>
      <td><img src="./figures/cancer_3months__3monthly_summary_proportion_surgeries_within_7wks_after_pos_test.png" alt = "cancer 3-month image"></td>
      <td><img src="./figures/noCancer__3monthly_summary_proportion_surgeries_within_7wks_after_pos_test.png" alt = "no cancer 3-month image"></td>
    </tr>
    <tr>
	    <td colspan=2><b>Figure X7wkX</b> Percentage of surgeries conducted <7 weeks (<43 days) from a positive PCR assay (see caveats in section Exposure subsection of the Methods in the main text). Vertical black lines indicate events of note in the timeline. <i>A</i> shows data for patients with a cancer diagnosis within ±3 months of their surgery date; <i>B</i> shows data for patients without a cancer diagnosis since 17th March 2018.</td>
    </tr>
</table>

Figure XAdmMethodX shows 3-monthly percentages of surgeries conducted <7 weeks (<43 days) from a positive PCR assay, from the UK’s Pillar 1 and Pillar 2 testing routes, but stratified by patients’ admission method, i.e. elective or emergency. The cohort used for this plot are all surgery patients irrespective of cancer diagnosis. There were more emergency admissions than elective admissions throughout the period observed but always <X%. 


<table>
    <tr>
      <td><img src="./figures/noCancer__stratified_3monthly_summary_proportion_surgeries_within_7wks_after_pos_test.png" alt = "all surgery patients, stratified by admission method"></td>
    </tr>
    <tr>
	    <td colspan=2><b>Figure XAdmMethodX</b> Percentage of surgeries conducted <7 weeks (<43 days) from a positive PCR assay (see caveats in section Exposure subsection of the Methods in the main text). Vertical black lines indicate events of note in the timeline. The cohort is patients who underwent surgery during elective admission (light grey) or emergency admission (dark grey).</td>
    </tr>
</table>

Table Xtable1DemogsX presents clinical demographics of the patient cohort during the pandemic-with-vaccine era and Table Xtable1OutcomesX presents patient outcomes in the same era. Similar tables for the other era are in the supplementary materials (LINK). The surgery cohort were almost evenly split between males and females; were overwhelmingly without a cancer diagnosis ; were very unlikely to undergo surgery under emergency admission; and were very unlikely to have records of pre-operative chronic cardiac disease, diabetes, chronic respiratory disease, or cerebrovascular disease. Overall 30-day post-operative mortality was approximately <0.2% and 30-day complications were always less than approximately 1.0%

<table>
	<tr>
		<td colspan=12>
			<b>Table Xtable1DemogsX</b>
			Demographic characteristics for patients who underwent surgery stratified by duration from indication of SARS-CoV-2 infection to surgery date. Period of interest is from 12th January 2021 until 31st March 2022 (i.e. the pandemic-with-vaccines era). Values are counts (n) and percentages (%).<br />
			The sum of ‘Timing of cancer diagnosis’ strata to not add to 100% because patients were admitted from this stratification if they had a cancer diagnosis but not a date of cancer diagnosis.
		</td>
	</tr>
	<tr>
		<td colspan=2></td>
		<td colspan=2>No indication of infection</td>
		<td colspan=8>Interval between indication of infection and surgery</td>
	</tr>
	<tr>
		<td colspan=2></td>
		<td colspan=2></td>
		<td colspan=2 style="text-align: center">≤14 days</td>
		<td colspan=2 style="text-align: center">15-28 days</td>
		<td colspan=2 style="text-align: center">29-42 days</td>
		<td colspan=2 style="text-align: center">≥43 days</td>
	</tr>
	<tr>
		<td colspan=2></td>
		<td style="text-align: center">n</td><td style="text-align: center">%</td>
		<td style="text-align: center">n</td><td style="text-align: center">%</td>
		<td style="text-align: center">n</td><td style="text-align: center">%</td>
		<td style="text-align: center">n</td><td style="text-align: center">%</td>
		<td style="text-align: center">n</td><td style="text-align: center">%</td>
	</tr>
	<tr>
		<td rowspan=2>Sex</td>
		<td>Female</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td>Male</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td rowspan=2>Chronic cardiac disease</td>
		<td>Yes</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td>No</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td rowspan=2>Diabetes</td>
		<td>Yes</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td>No</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td rowspan=2>Chronic respiratory disease</td>
		<td>Yes</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td>No</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td rowspan=3>Chronic cardiac disease</td>
		<td>Within 3 months of surgery</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td>Outwith 3 months of surgery</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td>No cancer diagnosis</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td rowspan=3>Admission method</td>
		<td>Elective</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td>Emergency</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td>Unknown</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
</table>

 

<table>
	<tr>
		<td colspan=12>
			<b>Table Xtable1OutcomesX</b>
			 Outcomes for patients who underwent surgery stratified by duration from indication of SARS-CoV-2 infection to surgery date. Period of interest is from 12th January 2021 until 31st March 2022 (i.e. the pandemic-with-vaccines era). Values are counts (n) and percentages (%).
		</td>
	</tr>
	<tr>
		<td colspan=2></td>
		<td colspan=2>No indication of infection</td>
		<td colspan=8>Interval between indication of infection and surgery</td>
	</tr>
	<tr>
		<td colspan=4></td>
		<td colspan=2 style="text-align: center">≤14 days</td>
		<td colspan=2 style="text-align: center">15-28 days</td>
		<td colspan=2 style="text-align: center">29-42 days</td>
		<td colspan=2 style="text-align: center">≥43 days</td>
	</tr>
	<tr>
		<td colspan=2></td>
		<td style="text-align: center">n</td><td style="text-align: center">%</td>
		<td style="text-align: center">n</td><td style="text-align: center">%</td>
		<td style="text-align: center">n</td><td style="text-align: center">%</td>
		<td style="text-align: center">n</td><td style="text-align: center">%</td>
		<td style="text-align: center">n</td><td style="text-align: center">%</td>
	</tr>
	<tr>
		<td rowspan=2>30-day post-operative mortality</td>
		<td>Alive within 30 days</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td>Dead within 30 days</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td rowspan=2>6-month post-operative mortality</td>
		<td>Alive within 6 months</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td>Dead within 6 months</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td rowspan=2>30-day post-operative pulmonary complications</td>
		<td>No complications</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td>Complications</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td rowspan=2>30-day post-operative cardiac complications</td>
		<td>No complications</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td>Complications</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td rowspan=3>30-day post-operative cerebrovascular complications</td>
		<td>No complications</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td>Complications</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	
</table>
	 	 	 	 	 	 	 


Finally, for comparison with the COVIDSurg studies, Table XtableEraX shows 30-day post-operative mortality for various cohorts, in the pandemic-no-vaccines and pandemic-with-vaccines¬ era. In the pandemic-no-vaccines era, OpenSAFELY cohorts suffered fewer deaths within 30-days of surgery compared with the COVIDSurg study cohort. For OpenSAFELY cohorts in the pandemic-with-vaccines era, mortality was approximately X-times worse for patients with a cancer diagnosis within 3 months of their surgery (XXX%) compared to patients without a cancer diagnosis (XXX%), though still low .

<table>
	<tr>
		<td colspan=14>
			<b>Table XtableEraX</b>
			 Thirty-day post-operative mortality for various cohorts, in each era, across all intervals defined by the interval between an indication of SARS-CoV-2 infection patients’ surgery date. Values for “COVIDSurg Collaborative” cohorts are taken from [20]; “OpenSAFELY…” cohorts refer to patients from the OpenSAFELY data platform who underwent surgery; “…with cancer” cohorts refer to patient with a cancer diagnosis within ±3 months of their surgery date; cohorts within the COVIDSurg Collaboration period refer to all COVIDSurg patients, regardless of cancer diagnosis. Values are counts of deaths (n), column totals (N), and percentages (%).
		</td>
	</tr>
	<tr>
		<td colspan=2></td>
		<td colspan=2>Total</td>
		<td colspan=2>No indication of infection</td>
		<td colspan=8>Interval between indication of infection and surgery</td>
	</tr>
	<tr>
		<td colspan=6></td>
		<td colspan=2 style="text-align: center">≤14 days</td>
		<td colspan=2 style="text-align: center">15-28 days</td>
		<td colspan=2 style="text-align: center">29-42 days</td>
		<td colspan=2 style="text-align: center">≥43 days</td>
	</tr>
	<tr>
		<td colspan=2></td>
		<td style="text-align: center">n / N</td><td style="text-align: center">%</td>
		<td style="text-align: center">n / N</td><td style="text-align: center">%</td>
		<td style="text-align: center">n / N</td><td style="text-align: center">%</td>
		<td style="text-align: center">n / N</td><td style="text-align: center">%</td>
		<td style="text-align: center">n / N</td><td style="text-align: center">%</td>
		<td style="text-align: center">n / N</td><td style="text-align: center">%</td>
	</tr>
	<tr>
		<td rowspan=3>Pandemic-no-vaccine</td>
		<td>OpenSAFELY, all surgeries</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td>OpenSAFELY, no cancer</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td>OpenSAFELY, with cancer</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td rowspan=4>COVIDSurg data collection period</td>
		<td>COVIDSurg collaborative</td>
		<td style="text-align: right">3,938 / 140,231</td><td style="text-align: right">2.81%</td>
		<td style="text-align: right">3,654 / 137,104</td><td style="text-align: right">2.67%</td>
		<td style="text-align: right">149 / 1,138</td><td style="text-align: right">13.09%</td>
		<td style="text-align: right">60 / 461</td><td style="text-align: right">13.02%</td>
		<td style="text-align: right">33 / 326</td><td style="text-align: right">10.12%</td>
		<td style="text-align: right">42 / 1,202</td><td style="text-align: right">3.49%</td>
		</tr>
	<tr>
		<td>OpenSAFELY, all surgeries</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td>OpenSAFELY, no cancer</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td>OpenSAFELY, with cancer</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td rowspan=3>Pandemic-with-vaccine</td>
		<td>OpenSAFELY, all surgeries</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td>OpenSAFELY, no cancer</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
	<tr>
		<td>OpenSAFELY, with cancer</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
		<td style="text-align: right">n</td><td style="text-align: right">%</td>
	</tr>
</table>


# Discussion 
The aim of this study was to evaluate the service provided by NHS England during the COVID19 pandemic with a focus on the extent to which guidance to delay surgeries was adhered to. To this end, we described 1) the percentage of cancer-related surgeries conducted <7 weeks (<43 days) from a positive PCR assay, in a cohort of patients from the UK’s Pillar 1 and Pillar 2 testing routes, and 2) cancer-related surgery patients’ post-operative outcomes before and during the COVID-19 pandemic.

Our service evaluation suggests that NHS England followed the principle of the NICE guidance from March 2020 by operating on very few patients within seven weeks of a positive indication of infection from SARS-CoV-2 from PCR nasal swab assays. This is shown in figure X7wkX for both cancer and non-cancer patients, using a 3-month cumulative proportion of operations conducted within seven weeks. The same conclusion is drawn from cumulative proportions over 2-month and 1-month (Supplementary material LINK). Also, using the COVIDSurg Collaborative studies as a benchmark, we conclude that patient outcomes were better for OpenSAFELY patients – i.e. NHS England – compared to the COVIDSurg Collaborative global average. This is shown in smaller proportions of patients that experienced undesirable outcomes in the OpenSAFELY data (table Xtable1OutcomesX) and in the comparison of 30-day mortality across all cohorts in all era studied (table XtableEraX). The same conclusion is drawn whether or not the cohorts are defined by having or not having a cancer diagnosis within either ±3 or ±6 months of their surgery.

## Comparison with COVIDSurg studies
Readers might note how similar our cohorts’ demographics shown in Xtable1DemogsX are to those of COVIDSurg Collaborative’s 2021 publication [20] (except that our pandemic-no-vaccine cohort were less likely to undergo surgery under emergency admission). However, we cannot assume our cohort are sampled from a similar population to that of the COVIDSurg Collaborative studies, despite the apparent similarity in demographics. This is because the similarity of cohort demographics (statistically significant or otherwise) does not indicate the exchangeability necessary to assume equivalence of cohorts; rather, it is the expectation of the unknowable distribution of groups’ outcomes prior to the exposure that needs to be equivalent [38]. The demographics act as proxies for the distribution of outcome values but neither we nor COVIDSurg Collaborative justified the demographic variables for this role.

As noted above, our pandemic-no-vaccine cohort were less likely to undergo surgery under emergency admission, compared to the COVIDSurg Collaborative’s 2021 publication [20]. Our clinical experience of patient prioritisation during this era was that, in addition to prioritizing patients with a clinical need, patients who were deemed low-risk and healthy were also operated on. Thus, the relative proportion of surgeries under emergency admission would be expected to be lower with a larger cohort of low-risk surgeries under elective admission.

In a diversion from the COVIDSurg Collaborative studies on which we modelled our service evaluation, we did not undertake regression analysis to estimate odds ratios. Our decision was motivated by acknowledging the complexity of the system under study and the difficulty in appropriately specifying a regression model that could estimate the correct causal estimates. It must be understood that our findings and those of COVIDSurg Collaborative are biased to an unknown degree in an unknown direction for reasons we explained in a previous publication [39] and briefly noted in the introduction. Instead of a simple regression analysis, we focused on the proportion of surgeries conducted within seven weeks of an indication of a SARS-CoV-2 infection to address the service-evaluation question of whether surgeries were delayed during the pandemic. We provided our stratification table similar to COVIDSurg Collaborative only as a benchmark for comparison under similar biases. That is to say, we conducted the same analysis, with similar biases, but with different data. This approach is in service of future work in which we will model the relationship between outcomes and the duration between test results and surgery with appropriate statistical and inferential methods.

We assumed that all demographics across which we stratified are true confounders of the relationship between the SARS-CoV-2–surgery interval and all outcomes, but our stratification might have induced a collider bias in unobserved confounders [40]. Our basic descriptive analysis is not free from the biases induced by the statistical method used. Both injudicious statistical adjustment in regression models and injudicious stratification of simple descriptive statistics can bias the estimates of interest in unknown ways. By omitting a simple regression analysis, we have only avoided an epistemic mistake known as the Table 2 Fallacy. As described by Westreich and Greenland, the fallacy refers to the unjustified interpretation of covariate regression coefficients in a model that has been specified to minimise bias in the estimate of an exposure, only [41]. This Table 2 Fallacy has already been an issue for studies modelling of OpenSAFELY data: the uninterpretable covariate coefficients informed French government policy that was subsequently overturned following legal proceedings [42]. We implore all statisticians, epidemiologist, data sciences, and clinicians to familiarize themselves with these fallacies and bias (both statistical and inferential) and never to engage in practices that evoke them. The cost of careless evocation is no less than hindering our capability to make sense of – and to make decisions about and to act within – the complex systems in which we operate. In our study, we present the stratified descriptive statistics to provide some clinical context for the cohorts; we do not provide them to be interpreted in isolation.

## Limitations
We previously published a summary of concerns about the biases inherent in COVIDSurg Collaborative methods, which we have repeated in the stratification part of our work herein [39,43]. We’ve discussed bias induced by the inferential bias induced by the Table 2 fallacy, above. Two likely statistical biases are cohort selection bias and bias due to missingness. Studies by the COVIDSurg Collaborative often excluded patients who had an indication of SARS-CoV-2 on the day of the operation [2,20,44–46]. This limited any inferences to perioperative infection. We have followed their approach and, by doing so, have limited the inference that can be made by this service evaluation. Additionally, the selection might have induced statistical bias in our estimates if the excluded patients would be expected to demonstrate a different distribution of outcome values compared to the included patients [47]. The indications of SARS-CoV-2 infection will also have induced selection bias, e.g. our evaluations indication of SARS-CoV-2 infection does not consider antigens nor antibodies associated with a SARS-CoV-2 infection, nor patients not in Pillar 1 or Pillar 2 of the UK testing strategy. 

Regarding bias arising from handling missing data, although never specified, reports of COVIDSurg studies imply that complete-case analyses were conducted. We also only used data for patients with no missing data, even though such analyses can induce collider bias via cohort selection [48]. COVID-specific research has demonstrated the need to reveal potential colliders by illustrating how the use of data from patients admitted to hospital undermines our understanding of COVID-19 risk and severity [40]. Potential for collider bias can be identified from mapping of causal relationships [49,50].

We hope to quantify these biases in subsequent analysis but it might not be possible without information about the selection process and observations of those who were and were not selected. Furthermore, it might not be possible to adjust for the confounding effect of government testing strategy [51] and the mixed performance of tests in the early portion of the pandemic call the validity of test data into question, fundamentally [52–60].

The OpenSAFELY platform was instrumental in facilitating the analyses we conducted. Unfortunately, the approach of bringing the analysis to the data rather than the data to the analyst means that it is not possible to undertake thorough evaluations of data quality – e.g [61] – in a domain with significant data-quality challenges [62]. Thus, all findings are interpreted under the untested assumption that the underlying data is complete, correct and current.

## Recommendations for surgery scheduling
Assuming that the OpenSAFELY cohorts are representative of the COVIDSurg cohorts, our findings shown in Table xtable1Outcomesx and Table xeraComparisonx suggest that the >7 week delay to surgery is no longer warranted because mortality rates are low even at 0-2 weeks, for the patients studied. We assume that patients who underwent surgery within two weeks of an indication of SARS-CoV-2 infection were more urgent because their condition was more severe (In other words, we assume that the urgency of a patient’s surgery confounds the relationship between the SARS-CoV-2–surgery interval and mortality). We might therefore expect mortality rates for the short-interval groups to be higher than the longer-interval both because of the relatively-poor health of these patients and of the burden of SARS-CoV-2 infection. But this compound risk appears to be clinically insignificant in the patient cohorts we have studied, given how comparable the mortality rates are in the short-interval and longer-interval groups.

Cautiously, we thus recommend that NHS England should reduce the 7-week threshold while monitoring outcomes. Delaying cancer surgery is theoretically likely to worsen outcomes over timeframes beyond what we analysed in this study, but the adverse effect of a timely surgery despite an indication of SARS-CoV-2 infection does not appear to worsen outcomes at 30 days or 12 months after surgery. Crucially, any change in practice needs to be in the context of a real-time evaluation as our multi-facetted understanding the physiology and epidemiology of SARS-CoV-2 improves [63]. Our openly-available OpenSAFELY scripts provide at least a proof of concept for a dashboard to monitor high-levels outcomes of interest, which are best positioned for monitoring the performance of national healthcare as a complex system [64].


## Future work
The study presented in this article describes a service evaluation that is part of a larger programme of study. We have a planned an additional service evaluation focusing on outcomes related to capacity planning and patient safety, which will be useful for hospital management. Example outcomes include length of stay in hospital, readmission rates, emergency department rates, and noting if the discharge destination differs from the pre-operative residence.

Aside from service evaluations, our programme of study will involve a research study to estimate the causal effect of SARS-CoV-2–surgery interval and our set of outcomes. We will propose a map of the causal relationships between the variables involved in the systems within which the interval-outcomes relationships sit. This causal diagram will help us to determine whether it is possible to compute unbiased estimates of the relationships between the SARS-CoV-2–surgery interval and our set of outcomes (i.e. identifiability [65]). If it is possible to compute unbiased estimates, then we will apply the necessary causal-inference approaches to compute the estimates. This series of work will permit a quantitative bias analysis to assess the potential influence of simulated unmeasured confounders [66].

# Conclusion
Our service evaluation suggests that NHS England followed the principle of the NICE guidance from March 2020 by operating on very few Pillar 1 and Pillar 2 cancer patients within seven weeks of a positive indication of infection from SARS-CoV-2 from PCR nasal swab assays. Analysis of post-operative complications and mortality suggest that delaying cancer surgery due to an indication of SARS-CoV-2 infection does not improve outcomes at 30 days or 1 year after operation. Given that delaying cancer surgery is likely to worsen outcomes for cancer patients in the longer term, we cautiously recommend that NHS England should reduce the 7-week threshold while monitoring outcomes.


# Acknowledgments
We are very grateful for all the support received from the EMIS and TPP Technical Operations team throughout this work, and for generous assistance from the information governance and database teams at NHS England and the NHS England Transformation Directorate.

This research is funded by the National Institute for Health Research (NIHR) Yorkshire and Humber Patient Safety Translational Research Centre (NIHR Yorkshire and Humber PSTRC). The views expressed in this article / presentation are those of the author(s) and not necessarily those of the NHS, the NIHR, or the Department of Health and Social Care

# Competing Interests
No authors have competing interests to declare.

# Credit Statement 
__Jonathan Cutting__: Data curation, Writing – review & editing;<br />
  __Louis Fisher__: Project administration, Supervision;<br />
__Owen A. Johnson__: Conceptualisation, Supervision, Visualisation, Writing – review & editing;<br />
__Alwyn Kotzé__: Conceptualisation, Supervision, Methodology, Visualisation, Writing – review & editing;<br />
__Justin Kua__: Data curation, Writing – review & editing;<br />
__Dermot McGurkin__: Data curation, Writing – review & editing;<br />
__Ciarán D. McInerney__: Conceptualisation, Data curation, Formal analysis, Methodology, Investigation, Project administration, Visualisation, Writing – original draft, Writing – review & editing;<br />
__S. Ramani Moonesinghe__: Supervision.<br />
 
# References
## References
<a id="1">[1]</a>
Bartlett DL, Howe JR, Chang G et al. Management of Cancer Surgery Cases During the COVID-19 Pandemic: Considerations. Annals of Surgical Oncology 2020; 27: 1717–20.
