## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  eval     = FALSE
)

## ----search-------------------------------------------------------------------
# library(nhanesR)
# 
# # Confirm URXUMA is present across all ten cycles
# nhanes_search_variables("albumin", component = "Laboratory")
# 
# # Variable map for urinary albumin: maps cleanly to ALB_CR_* files
# map_uma <- nhanes_variable_map("URXUMA")
# map_uma

## ----ucr-warning--------------------------------------------------------------
# # nhanes_variable_map("URXUCR") resolves to the wrong file in early cycles:
# # URXUCR appears in phthalates / heavy-metals files (e.g. PHPYPA 1999-2000)
# # as a dilution-adjustment variable, and the catalog finds those first.
# nhanes_variable_map("URXUCR")

## ----download-----------------------------------------------------------------
# # Restrict to cycles with public-use mortality linkage (1999-2018)
# cycles <- nhanes_cycles()[nhanes_cycles()$has_lmf_public, "cycle"]
# 
# # nhanes_download_analyte resolves the changing file name automatically.
# # Each returned data frame contains the full ALB_CR file — including
# # URXUMA (urinary albumin) AND URXUCR (urinary creatinine).
# alb_cr_list <- nhanes_download_analyte("URXUMA", cycles)
# alb_cr      <- nhanes_stack(alb_cr_list)

## ----uacr---------------------------------------------------------------------
# alb_cr$UACR <- with(alb_cr, 100 * URXUMA / URXUCR)
# 
# # Clinical thresholds (KDIGO 2012)
# alb_cr$UACR_cat <- cut(
#   alb_cr$UACR,
#   breaks = c(0, 30, 300, Inf),
#   labels = c("Normal-mildly increased (<30)",
#              "Moderately increased (30-300)",
#              "Severely increased (>300)"),
#   right  = FALSE
# )

## ----methods-check, eval=FALSE------------------------------------------------
# # The NHANES laboratory methods document for each cycle is linked from the
# # online data browser.  For urinary albumin (ALB_CR files), look for entries
# # under "Albumin, Urine" describing the analyser make/model and reagent kit.
# # Key things to record for each cycle:
# #   - Analyser (e.g. Roche Hitachi 917, Beckman UniCel DxC 800, ...)
# #   - Reagent/method (immunoturbidimetric, immunonephelometric, ...)
# #   - Calibrator traceability (JCTLM-listed reference material?)
# #   - LOD (ug/mL) -- differs across cycles

## ----survival-----------------------------------------------------------------
# library(survival)
# library(survey)
# 
# # Link mortality
# alb_cr_mort <- nhanes_mortality_link(alb_cr)
# alb_cr_surv <- nhanes_survival_prep(
#   alb_cr_mort,
#   origin     = "exam",
#   time_unit  = "years",
#   weight_var = "WTMEC2YR"
# )
# 
# # Survey design
# options(survey.lonely.psu = "adjust")
# svy <- svydesign(
#   ids     = ~SDMVPSU,
#   strata  = ~SDMVSTRA,
#   weights = ~survey_weight,
#   nest    = TRUE,
#   data    = alb_cr_surv
# )
# 
# # Unadjusted rate by UACR category
# svyby(~event, ~UACR_cat, svy, svymean, na.rm = TRUE)

