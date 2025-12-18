# PRE2DUP-R
A program to create treatment periods from drug purchases data

**Challenge in pharmacoepidemiologic studies: how to determine drug use periods from drug dispensing records**

Register-based drug dispensing data stored in various databases such as Medicaid and Nordic nationwide registers are widely used in pharmacoepidemiological studies. Valid methods for evaluating the drug use periods from dispensing data are needed for e.g. studies to investigate the association between drug exposure and health-related outcomes such as hospitalizations or death. 

## PRE2DUP method
The PRE2DUP (From prescription drug purchases to drug use periods) method is a novel approach based on mathematical modelling of personal drug purchasing behaviors. The method uses a decision procedure that utilizes the purchased amount in Defined Daily Doses (DDDs, defined by World Health Organization, WHO) and each person’s purchase history for constructing exposure time periods and dose estimates for each drug (defined as Anatomical Therapeutic Chemical [ATC] codes). The method takes account of stockpiling of drugs, regularity of the purchases, and periods of hospital or nursing home care when drug use is not recorded in the databases of outpatient medication use. The method can be applied to virtually all drug classes and drug formulations.  Clinically meaningful estimates are ensured by expert-defined parameters for each drug package which control joining of purchases. These parameters define e.g. the lowest clinically meaningful dose for each drug package (characterized by drug substance, specifics of the drug formulation, strength and the number of units). The method also has features which tailor drug use periods for an individual and for the study population, based on data on individual purchase regularity and most common patterns of drug use in the studied population. 


## PRE2DUP-R
There is a growing demand for open-source version of PRE2DUP in the programming language and software familiar in the research field. R-package PRE2DUP-R provides an advanced tool for generating reliable drug use estimates from large dispensing databases and practical tools for data pre-processing and cleaning. It will form the basis for development of sub-programs for specific tasks which may be required in particular research fields.   

## The use of PRE2DUP-R

Please see an example of practical use of PRE2DUP-R here: [Introduction](https://piavat.github.io/PRE2DUP-R/articles/introduction.html), and instructions how to create package parameter file for study drugs here: [Package parameters](https://piavat.github.io/PRE2DUP-R/tutorials/Instruction_to_build_package_parameter.html) (more details via the link at the bottom of the Package parameters page). 


## Reporting Guidelines
Please always specify the version of **PRE2DUP-R** used and always state using default values and/or report any deviations from those to support open and transparent reporting. 

**Example**: 
“Analysis were conducted with PRE2DUP-R (replace xxx with version number) with R version (replace xxx with version number), with default values except for zzz" (replace zzz with the list of deviations and which values were used)”

## How to Cite
If you use **PRE2DUP-R** in your research, please cite the following:

**Authors**:

- Pia Vattulainen - R Implementation, Open-Source Development, Project Maintenance, Method Development

- Antti Tanskanen - Method Development, Original Concept

- Heidi Taipale - Method Development, Original Concept

**Project title**: PRE2DUP-R: Create treatment periods from drug purchases data

**Year**: 2025

Please use the following citation format:

Vattulainen, P., Tanskanen, A., & Taipale, H. (2025). PRE2DUP-R: Create treatment periods from drug purchases data. GitHub. https://github.com/piavat/PRE2DUP-R

We recommend referencing the repository directly:  
https://github.com/piavat/PRE2DUP-R

## For more information, see introduction to method:

Tanskanen, A., Taipale, H., Koponen, M., Tolppanen, A. M., Hartikainen, S., Ahonen, R., & Tiihonen, J. (2015). From prescription drug purchases to drug use periods – a second generation method (PRE2DUP). BMC medical informatics and decision making, 15, 21. https://doi.org/10.1186/s12911-015-0140-z

Method has been validated with self-reported drug use, with expert-opinion based evaluation and with postmortem toxicological findings:

Taipale, H., Tanskanen, A., Koponen, M., Tolppanen, A. M., Tiihonen, J., & Hartikainen, S. (2016). Agreement between PRE2DUP register data modeling method and comprehensive drug use interview among older persons. Clinical epidemiology, 8, 363–371. https://doi.org/10.2147/CLEP.S116160

Tanskanen, A., Taipale, H., Koponen, M., Tolppanen, A. M., Hartikainen, S., Ahonen, R., & Tiihonen, J. (2017). Drug exposure in register-based research-An expert-opinion based evaluation of methods. PloS one, 12(9), e0184070. https://doi.org/10.1371/journal.pone.0184070

Forsman, J., Taipale, H., Masterman, T., Tiihonen, J., & Tanskanen, A. (2018). Comparison of dispensed medications and forensic-toxicological findings to assess pharmacotherapy in the Swedish population 2006 to 2013. Pharmacoepidemiology and drug safety, 27(10), 1112–1122. https://doi.org/10.1002/pds.4426

PRE2DUP has been used in more than 100 published studies, see example studies using PRE2DUP:

Tiihonen, J., Mittendorfer-Rutz, E., Majak, M., Mehtälä, J., Hoti, F., Jedenius, E., Enkusson, D., Leval, A., Sermon, J., Tanskanen, A., & Taipale, H. (2017). Real-World Effectiveness of Antipsychotic Treatments in a Nationwide Cohort of 29 823 Patients With Schizophrenia. JAMA psychiatry, 74(7), 686–693. https://doi.org/10.1001/jamapsychiatry.2017.1322

Tiihonen, J., Taipale, H., Mehtälä, J., Vattulainen, P., Correll, C. U., & Tanskanen, A. (2019). Association of Antipsychotic Polypharmacy vs Monotherapy With Psychiatric Rehospitalization Among Adults With Schizophrenia. JAMA psychiatry, 76(5), 499–507. https://doi.org/10.1001/jamapsychiatry.2018.4320

Taipale, H., Tanskanen, A., Mehtälä, J., Vattulainen, P., Correll, C. U., & Tiihonen, J. (2020). 20-year follow-up study of physical morbidity and mortality in relationship to antipsychotic treatment in a nationwide cohort of 62,250 patients with schizophrenia (FIN20). World psychiatry : official journal of the World Psychiatric Association (WPA), 19(1), 61–68. https://doi.org/10.1002/wps.20699

Lähteenvuo, M., Tanskanen, A., Taipale, H., Hoti, F., Vattulainen, P., Vieta, E., & Tiihonen, J. (2018). Real-world Effectiveness of Pharmacologic Treatments for the Prevention of Rehospitalization in a Finnish Nationwide Cohort of Patients With Bipolar Disorder. JAMA psychiatry, 75(4), 347–355. https://doi.org/10.1001/jamapsychiatry.2017.4711

Taipale, H., Schneider-Thoma, J., Pinzón-Espinosa, J., Radua, J., Efthimiou, O., Vinkers, C. H., Mittendorfer-Rutz, E., Cardoner, N., Pintor, L., Tanskanen, A., Tomlinson, A., Fusar-Poli, P., Cipriani, A., Vieta, E., Leucht, S., Tiihonen, J., & Luykx, J. J. (2022). Representation and Outcomes of Individuals With Schizophrenia Seen in Everyday Practice Who Are Ineligible for Randomized Clinical Trials. JAMA psychiatry, 79(3), 210–218. https://doi.org/10.1001/jamapsychiatry.2021.3990

Taipale, H., Solmi, M., Lähteenvuo, M., Tanskanen, A., Correll, C. U., & Tiihonen, J. (2021). Antipsychotic use and risk of breast cancer in women with schizophrenia: a nationwide nested case-control study in Finland. The lancet. Psychiatry, 8(10), 883–891. https://doi.org/10.1016/S2215-0366(21)00241-8

Taipale, H., Tanskanen, A., Correll, C. U., & Tiihonen, J. (2022). Real-world effectiveness of antipsychotic doses for relapse prevention in patients with first-episode schizophrenia in Finland: a nationwide, register-based cohort study. The lancet. Psychiatry, 9(4), 271–279. https://doi.org/10.1016/S2215-0366(22)00015-3

