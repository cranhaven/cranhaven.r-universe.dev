## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE, warning = FALSE, message = FALSE,
  out.width = "100%",
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(CDMConnector)
library(CohortSurvival)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
cdm <- CohortSurvival::mockMGUS2cdm()

## -----------------------------------------------------------------------------
cdm$mgus_diagnosis %>% 
  glimpse()

cdm$death_cohort %>% 
  glimpse()

## -----------------------------------------------------------------------------
MGUS_death <- estimateSingleEventSurvival(cdm,
  targetCohortTable = "mgus_diagnosis",
  outcomeCohortTable = "death_cohort"
)
MGUS_death %>% 
  glimpse()
class(MGUS_death)

## -----------------------------------------------------------------------------
MGUS_death %>% 
  asSurvivalResult() %>%
  glimpse()

## -----------------------------------------------------------------------------
plotSurvival(MGUS_death)

## -----------------------------------------------------------------------------
tableSurvival(MGUS_death) 

## -----------------------------------------------------------------------------
MGUS_death <- estimateSingleEventSurvival(cdm,
  targetCohortTable = "mgus_diagnosis",
  outcomeCohortTable = "death_cohort",
  strata = list(c("age_group"),
                c("sex"),
                c("age_group", "sex"))
) 

## ----fig.height=6, fig.width=8------------------------------------------------
plotSurvival(MGUS_death,
             facet = "strata_name",
             colour = "strata_level")

## -----------------------------------------------------------------------------
tableSurvival(MGUS_death)

## -----------------------------------------------------------------------------
cdmDisconnect(cdm)

