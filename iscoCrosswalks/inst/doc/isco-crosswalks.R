## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  fig.height = 2
)
library(knitr)

## ----setup--------------------------------------------------------------------
library(iscoCrosswalks)
library(data.table)

## ----example------------------------------------------------------------------
library(iscoCrosswalks)

## -----------------------------------------------------------------------------
kable(foundation_skills[seq(1 , nrow(foundation_skills), by = 5), ])

## -----------------------------------------------------------------------------
data.table::setnames(foundation_skills,
                     c("preferredLabel", "Value"),
                     c("job", "value"))

## -----------------------------------------------------------------------------
soc_foundation_skills <- isco_soc_crosswalk(foundation_skills,
                                            brkd_cols = "Skill",
                                            isco_lvl = 1,
                                            soc_lvl = "soc_1",
                                            indicator = TRUE)

## -----------------------------------------------------------------------------
soc_foundation_skills[, Occupations := gsub(" Occupations", "", soc_label)]
soc_foundation_skills[, Skill := gsub(" skills", "", Skill)]
dat <- soc_foundation_skills[order(Skill, -value)][, head(.SD, 6), by = "Skill"]
kable(dat)

