## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(here)
library(tidyverse)
library(questionr)
library(data.table)

## -----------------------------------------------------------------------------
# Install from CRAN
# install.packages("UKB.COVID19")

# Load the package
library(UKB.COVID19)

## -----------------------------------------------------------------------------
ukb.tab.file <- covid_example("sim_ukb.tab.gz")
ukb.tab <- fread((ukb.tab.file))
head(ukb.tab)

## -----------------------------------------------------------------------------
covar <- risk_factor(ukb.data=covid_example("sim_ukb.tab.gz"),
                     ABO.data=covid_example("sim_covid19_misc.txt.gz"),
                     hesin.file=covid_example("sim_hesin.txt.gz"),
                     res.eng=covid_example("sim_result_england.txt.gz"))

head(covar)

## -----------------------------------------------------------------------------
susceptibility <- makePhenotypes(ukb.data=covid_example("sim_ukb.tab.gz"),
                        res.eng=covid_example("sim_result_england.txt.gz"),
                        death.file=covid_example("sim_death.txt.gz"),
                        death.cause.file=covid_example("sim_death_cause.txt.gz"),
                        hesin.file=covid_example("sim_hesin.txt.gz"),
                        hesin_diag.file=covid_example("sim_hesin_diag.txt.gz"),
                        hesin_oper.file=covid_example("sim_hesin_oper.txt.gz"),
                        hesin_critical.file=covid_example("sim_hesin_critical.txt.gz"),
                        code.file=covid_example("coding240.txt.gz"),
                        pheno.type = "susceptibility")
head (susceptibility)

## -----------------------------------------------------------------------------
severity <- makePhenotypes(ukb.data=covid_example("sim_ukb.tab.gz"),
                        res.eng=covid_example("sim_result_england.txt.gz"),
                        death.file=covid_example("sim_death.txt.gz"),
                        death.cause.file=covid_example("sim_death_cause.txt.gz"),
                        hesin.file=covid_example("sim_hesin.txt.gz"),
                        hesin_diag.file=covid_example("sim_hesin_diag.txt.gz"),
                        hesin_oper.file=covid_example("sim_hesin_oper.txt.gz"),
                        hesin_critical.file=covid_example("sim_hesin_critical.txt.gz"),
                        code.file=covid_example("coding240.txt.gz"),
                        pheno.type = "severity")

## -----------------------------------------------------------------------------
mortality <- makePhenotypes(ukb.data=covid_example("sim_ukb.tab.gz"),
                        res.eng=covid_example("sim_result_england.txt.gz"),
                        death.file=covid_example("sim_death.txt.gz"),
                        death.cause.file=covid_example("sim_death_cause.txt.gz"),
                        hesin.file=covid_example("sim_hesin.txt.gz"),
                        hesin_diag.file=covid_example("sim_hesin_diag.txt.gz"),
                        hesin_oper.file=covid_example("sim_hesin_oper.txt.gz"),
                        hesin_critical.file=covid_example("sim_hesin_critical.txt.gz"),
                        code.file=covid_example("coding240.txt.gz"),
                        pheno.type = "mortality")

## -----------------------------------------------------------------------------
log_cov(pheno=susceptibility, covariates=covar, phe.name="pos.neg", cov.name=c("sex", "age", "bmi"))

## -----------------------------------------------------------------------------
comorb <- comorbidity_summary(ukb.data=covid_example("sim_ukb.tab.gz"),
                              hesin.file=covid_example("sim_hesin.txt.gz"), 
                              hesin_diag.file=covid_example("sim_hesin_diag.txt.gz"), 
                              ICD10.file=covid_example("ICD10.coding19.txt.gz"),
                              primary = FALSE,
                              Date.start = "16/03/2020")
comorb[1:6,1:10]

## -----------------------------------------------------------------------------
comorb.asso <- comorbidity_asso(pheno=susceptibility,
                                covariates=covar,
                                cormorbidity=comorb,
                                population="white",
                                cov.name=c("sex","age","bmi","SES","smoke","inAgedCare"),
                                phe.name="pos.neg",
                                ICD10.file=covid_example("ICD10.coding19.txt.gz"))
head(comorb.asso, 4)

## -----------------------------------------------------------------------------
# Load the package
library(UKB.COVID19)

# Summarize COVID-19 risk factors
covar <- risk_factor(ukb.data=covid_example("sim_ukb.tab.gz"), 
                         ABO.data=covid_example("sim_covid19_misc.txt.gz"),
                         hesin.file=covid_example("sim_hesin.txt.gz"),
                         res.eng=covid_example("sim_result_england.txt.gz"))

# Summarize COVID-19 test results
susceptibility <- makePhenotypes(ukb.data=covid_example("sim_ukb.tab.gz"),
                        res.eng=covid_example("sim_result_england.txt.gz"),
                        death.file=covid_example("sim_death.txt.gz"),
                        death.cause.file=covid_example("sim_death_cause.txt.gz"),
                        hesin.file=covid_example("sim_hesin.txt.gz"),
                        hesin_diag.file=covid_example("sim_hesin_diag.txt.gz"),
                        hesin_oper.file=covid_example("sim_hesin_oper.txt.gz"),
                        hesin_critical.file=covid_example("sim_hesin_critical.txt.gz"),
                        code.file=covid_example("coding240.txt.gz"),
                        pheno.type = "susceptibility")

# Perfrom association tests
log_cov(pheno=susceptibility, covariates=covar, phe.name="pos.neg", cov.name=c("sex", "age", "bmi"))

# Generate comorbidity table
comorb <- comorbidity_summary(ukb.data=covid_example("sim_ukb.tab.gz"),
                              hesin.file=covid_example("sim_hesin.txt.gz"), 
                              hesin_diag.file=covid_example("sim_hesin_diag.txt.gz"), 
                              ICD10.file=covid_example("ICD10.coding19.txt.gz"),
                              primary = FALSE,
                              Date.start = "16/03/2020")

# Perform association tests between COVID-19 phenotype and comorbidities
comorb.asso <- comorbidity_asso(pheno=susceptibility,
                                covariates=covar,
                                cormorbidity=comorb,
                                population="white",
                                cov.name=c("sex","age","bmi","SES","smoke","inAgedCare"),
                                phe.name="pos.neg",
                                ICD10.file=covid_example("ICD10.coding19.txt.gz"))


