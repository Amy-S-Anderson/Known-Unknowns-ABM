# Known-Unknowns-ABM
An ABM for exploring hidden variables in bioarchaeology

This code accompanies the paper 'Known unknowns and the osteological paradox: Why bioarchaeology needs agent-based models', by Amy Anderson and Sharon DeWitte

The agent-based model (ABM) in the simulate_cemetery file generates a cohort of individuals (user specifies cohort size) at age 0 who are exposed to an annual probability of developing a skeletal lesion and an annual probability of dying. The ages at which a skeletal lesion might develop are specified by the user, as is the annual hazard of dying (specified as a named Siler function). 
The ABM produces two data sets: one that specifies the age and lesion status (present/absent) of every living individual in the cohort in each year of model time, and one that specifies the age at death and lesion status of every individual in the cohort. This second data set is effectively a bioarchaeological data set from a simulated cemetery. 


The model parameters can be varied, and the model is easily adjusted to incorporate other variables, but in the current experiment the ages at which a new skeletal lesion might form (the lesion's developmental window) are ages 0-9; once an individual agent turns 10, they are no longer at risk of forming a new skeletal lesion. 

To run the model and the analyses run these three files in order:
1. setup file (runs in seconds)
2. sensitivity analysis (might take a few hours, depending on computing power)
3. main analysis (less than an hour)

The 'main analysis' file relies on reading in a subset of the data that are generated, named, and saved by running the sensitivity analysis. 

Statistical analyses on the simulated cemetery data are currently limited to
- Kaplan Meier survival analysis (individuals with lesions vs. without lesions)
- Chi-square test (lesion prevalence in individuals <15 years vs. >= 15 years)
