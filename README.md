# Modelled Needs API

This API can be used in two ways:

- Estimating metrics by GP Practice/PCN/region, and comparing to observed values to highlight successful areas, and areas of need.
- Searching NICE interventions/clinical trials, given a patient cohort.

In the first method (modelling area efficiency), the user selects a response variable, a number of predictor variables, and an area over which to aggregate. The API takes these variables and runs a model to predict the response variable given the predictor variables (using patient-level data), and then aggregates the response variables over the selected area so that a chi-squared statistic can be drawn. The observed and expected area-level response is returned, along with the ratio between the two and confidence intervals.

For the second method (intervention search), a user-selected cohort is parsed to pull out selected ages, sex, and long-term conditions. These are then sent to the NICE evidence search API which returns a list of related publications, which are returned to the user.

## How to Run

Load the project `area-efficiency.Rproj` into R, and run `RunModelledNeeds.R` to launch the plumbeR methods.

## The Models

Depending upon the chosen response variable(s), there are three different model variations that are run. These are:

- A simple Logistic Regression model for binary response variables. These include selected cohorts, single long-term conditions (e.g. COPD or cancer), or comorbid long-term conditions (e.g. COPD and cancer).
- A Gradient-Boosted Poisson model (using XGBoost) for count response variables. These include A&E attendances, and Inpatient and Outpatient appointments.
- Three Gradient-Boosted Poisson models (using XGBoost) alongside Monte-Carlo model, for a cost response variable. The three Poisson models calculate expected A&E, Inpatient, and Outpatient numbers, and the Monte-Carlo model takes these expected counts and converts them into an expected cost range (95% credible interval). This is done by taking the current distribution of costs at Blackpool Teaching Hospitals and sampling n-times, where n is given by the expected counts.

## Terms of Use

This specific code repository and all code within is © Crown copyright and available under the terms of the Open Government 3.0 licence.

The code has been developed and is maintained by the NHS and where possible we will try to adhere to the NHS Open Source Policy (<https://github.com/nhsx/open-source-policy/blob/main/open-source-policy.md>).

It shall remain free to the NHS and all UK public services.

### Contributions

This code has been authored by colleagues in the Digital Intelligence Unit @ NHS Blackpool CCG.

_This project and all code within is © Crown copyright and available under the terms of the Open Government 3.0 licence._
