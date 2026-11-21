## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4.5
)
library(SimtablR)
data(epitabl)

## ----table1-baseline----------------------------------------------------------
baseline <- table1(
  epitabl, # Input our dataset (dataframe object)
  c(age, sex, bmi, smoking, hypertension, diabetes, renal_impairment, presentation_hours),  # Choose variables of interest
  by = adjudicated_acs, # Stratify by the adjudicated ACS outcome
  test = TRUE, # Add hypothesis testing (p-values) for group comparisons
  labels = c(  # Rename variables for publication-ready output
    age = "Age (years)",
    bmi = "Body mass index (kg/m²)",
    smoking = "Smoking status",
    hypertension = "Hypertension",
    diabetes = "Diabetes mellitus",
    renal_impairment = "Renal impairment (eGFR < 60)",
    presentation_hours = "Time from symptom onset to ED (hours)"
  )
)
baseline

## ----table1-smd---------------------------------------------------------------
# Append SMDs to the computed baseline table without repeating parameters
baseline_smd <- baseline |> test(smd = TRUE) # We may also specify smd = "all" to compute SMDs for all covariates, including categorical variables.
baseline_smd

## ----tb-rr--------------------------------------------------------------------
tab_rr <- tb(
  epitabl,
  renal_impairment,
  adjudicated_acs,
  flags = c("row", "rr"),
  design = "cohort", # By specifying our study design, SimtablR automatically selects the appropriate effect measure (RR) and confidence interval method.
  ref = "No"
)
tab_rr

## ----tb-flags-----------------------------------------------------------------
# Cell percentages with explicit chi-squared p-value
tb(epitabl, 
   smoking, adjudicated_acs, # Evaluate the association between smoking status and confirmed ACS
   flags = c("cell", "p") # Add cell percentages and a chi-squared p-value for the association
   ) 

## ----tb-stratified------------------------------------------------------------
tab_strat <- tb( 
  epitabl,
  renal_impairment,
  adjudicated_acs,
  strat = sex, # Stratify by biological sex to evaluate effect measure modification
  flags = c("row", "rr"),
  design = "cohort" 
)
tab_strat

## ----tb-stack-----------------------------------------------------------------
t_smoke <- tb(epitabl, smoking, adjudicated_acs, flags = c("row", "or"), ref = "Never")
t_htn   <- tb(epitabl, hypertension, adjudicated_acs, flags = c("row", "or"), ref = "No")
t_dm    <- tb(epitabl, diabetes, adjudicated_acs, flags = c("row", "or"), ref = "No")
t_ckd   <- tb(epitabl, renal_impairment, adjudicated_acs, flags = c("row", "or"), ref = "No")

# Stack into a unified crude association table
stacked_crude <- rbind(t_smoke, t_htn, t_dm, t_ckd)
stacked_crude

