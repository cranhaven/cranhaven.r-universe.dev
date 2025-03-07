## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, warning = FALSE, message = FALSE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(CDMConnector)
library(CohortSurvival)
library(dplyr)
library(cmprsk)
library(survival)

## -----------------------------------------------------------------------------
cdm <- CohortSurvival::mockMGUS2cdm()

## ----fig.width=5--------------------------------------------------------------
input_survival_single <- cdm$mgus_diagnosis %>%
       addCohortSurvival(
       cdm = cdm,
       outcomeCohortTable = "death_cohort",
       outcomeCohortId = 1
       ) 

input_survival_single %>% 
  glimpse()

## -----------------------------------------------------------------------------
survival::coxph(survival::Surv(time, status) ~ age + sex, data = input_survival_single)
survival::survdiff(survival::Surv(time, status) ~ sex, data = input_survival_single)

## -----------------------------------------------------------------------------
input_survival_cr <- cdm$mgus_diagnosis %>%
  addCompetingRiskCohortSurvival(
    cdm = cdm,
    outcomeCohortTable = "progression",
    outcomeCohortId = 1,
    competingOutcomeCohortTable = "death_cohort",
    competingOutcomeCohortId = 1
  ) %>% 
  glimpse()

## ----fig.height=6, fig.width=8------------------------------------------------
input_survival_cr <- input_survival_cr %>%
  dplyr::mutate(sex = dplyr::if_else(sex == "M", 0, 1))

covs <- data.frame(input_survival_cr$age, input_survival_cr$sex)
names(covs) <- c("age", "sex")

summary(cmprsk::crr(ftime = input_survival_cr$time,
            fstatus = input_survival_cr$status,
            cov1 = covs,
            failcode = 1,
            cencode = 0))

## -----------------------------------------------------------------------------
cdmDisconnect(cdm)

