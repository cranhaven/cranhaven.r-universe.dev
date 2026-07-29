## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse  = TRUE,
  comment   = "#>",
  eval      = FALSE
)

## ----load---------------------------------------------------------------------
# library(nhanesR)
# library(survival)
# library(survey)

## ----options-interactive------------------------------------------------------
# # View current cache location
# nhanes_cache_dir()
# 
# # Opt in to a persistent home-directory cache for this session
# nhanes_cache_dir("~/my_nhanes_cache")
# 
# # Suppress download messages for this session
# options(nhanesR.verbose = FALSE)

## ----cycles-------------------------------------------------------------------
# # All continuous NHANES cycles known to nhanesR
# nhanes_cycles()
# 
# # Just the cycle labels for the first ten continuous cycles (1999-2018)
# cycles <- nhanes_cycles()[1:10, "cycle"]
# cycles

## ----manifest-----------------------------------------------------------------
# nhanes_manifest("2015-2016", "Laboratory")
# nhanes_manifest("2013-2014", "Questionnaire")

## ----search-------------------------------------------------------------------
# # Find total cholesterol across all cycles (summarized by default)
# nhanes_search_variables("total cholesterol", component = "Laboratory")
# 
# # Raw one-row-per-cycle output
# nhanes_search_variables("total cholesterol", component = "Laboratory",
#                          summarize = FALSE)

## ----variable-map-------------------------------------------------------------
# # Per-cycle lookup: which file and variable name holds total cholesterol?
# nhanes_variable_map("total cholesterol")
# 
# # HDL changed variable name three times across cycles
# nhanes_variable_map("HDL")
# 
# # Questionnaire: history of MI (keep_vars filters out false positives)
# nhanes_variable_map("heart attack", component = "Questionnaire",
#                      keep_vars = c("MCQ160E", "MCQ160e"))

## ----download-lab-------------------------------------------------------------
# cycles <- nhanes_cycles()[1:10, "cycle"]  # 1999-2018
# 
# # Demographics — file name has always been DEMO; nhanes_download() works fine
# demo_list <- nhanes_download("DEMO", cycles)
# 
# # Total cholesterol — file renamed across early cycles; use download_analyte()
# tchol_list <- nhanes_download_analyte("total cholesterol", cycles)
# 
# # HDL cholesterol
# hdl_list <- nhanes_download_analyte("HDL", cycles)

## ----download-quest-----------------------------------------------------------
# # History of myocardial infarction (MCQ file)
# # MCQ160E (1999-2010) and MCQ160e (2011-2018) are the same question;
# # keep_vars filters out RXQ510 which also mentions "heart attack"
# mi_list <- nhanes_download_analyte(
#   "heart attack", cycles,
#   component = "Questionnaire",
#   keep_vars = c("MCQ160E", "MCQ160e")
# )
# 
# # Cholesterol-lowering medication (BPQ file)
# # "Ever told to take prescribed medicine to lower blood cholesterol?"
# chol_med_list <- nhanes_download_analyte(
#   "cholesterol", cycles,
#   component = "Questionnaire",
#   keep_vars = c("BPQ090D", "BPQ101D")
# )

## ----harmonize-lab------------------------------------------------------------
# # Total cholesterol — LBXTC throughout, but label_pattern narrows the match
# # in 1999-2004 when TC and HDL were bundled in the same file
# TC <- nhanes_harmonize(
#   tchol_list,
#   unit          = "mg/dL",
#   name          = "TC_mgdl",
#   label_pattern = "total cholesterol"
# )
# 
# # HDL — three different variable names across cycles; unit approach handles all
# HDL <- nhanes_harmonize(
#   hdl_list,
#   unit          = "mg/dL",
#   name          = "HDL_mgdl",
#   label_pattern = "HDL"
# )
# 
# str(TC)   # SEQN (chr), cycle (chr), TC_mgdl (num)
# str(HDL)  # SEQN (chr), cycle (chr), HDL_mgdl (num)

## ----harmonize-quest----------------------------------------------------------
# MI <- nhanes_harmonize(
#   mi_list,
#   mapping = c(MCQ160E = "MI_history", MCQ160e = "MI_history")
# )
# 
# chol_med <- nhanes_harmonize(
#   chol_med_list,
#   mapping = c(BPQ090D = "chol_med", BPQ101D = "chol_med")
# )
# 
# # Each result is a trim 3-column data frame ready for merging
# str(MI)        # SEQN, cycle, MI_history
# str(chol_med)  # SEQN, cycle, chol_med

## ----recode-------------------------------------------------------------------
# nhanes_recode_yn <- function(x) {
#   out        <- rep(NA_integer_, length(x))
#   out[x == 1] <- 1L
#   out[x == 2] <- 0L
#   out
# }
# 
# MI$MI_history      <- nhanes_recode_yn(MI$MI_history)
# chol_med$chol_med  <- nhanes_recode_yn(chol_med$chol_med)
# 
# # Verify: should see 0, 1, and NA only
# table(MI$MI_history,      useNA = "always")
# table(chol_med$chol_med,  useNA = "always")

## ----merge--------------------------------------------------------------------
# demo <- nhanes_stack(demo_list)
# 
# # Inner join lab data (keeps only participants who attended the exam)
# analytic <- Reduce(
#   function(a, b) merge(a, b, by = c("SEQN", "cycle")),
#   list(demo, TC, HDL)
# )
# 
# # Left join questionnaire data (all interviewed participants have these)
# analytic <- merge(analytic, MI,       by = c("SEQN", "cycle"), all.x = TRUE)
# analytic <- merge(analytic, chol_med, by = c("SEQN", "cycle"), all.x = TRUE)
# 
# nrow(analytic)
# names(analytic)
# 
# # Check key variables arrived
# c("TC_mgdl", "HDL_mgdl", "MI_history", "chol_med",
#   "RIDAGEYR", "RIAGENDR", "WTMEC2YR", "SDMVPSU", "SDMVSTRA") %in%
#   names(analytic)

## ----mortality----------------------------------------------------------------
# analytic_mort <- nhanes_mortality_link(analytic)
# 
# # Key variables added:
# #   ELIGSTAT     1=eligible, 2=under 18, 3=insufficient data for linkage
# #   MORTSTAT     0=assumed alive 31-Dec-2019, 1=assumed deceased
# #   UCOD_LEADING Underlying cause of death (11-category ICD-10 recode)
# #   PERMTH_EXM   Months from examination date to death or Dec 31 2019
# #   PERMTH_INT   Same, from interview date
# 
# table(analytic_mort$MORTSTAT, useNA = "always")

## ----survprep-----------------------------------------------------------------
# surv_data <- nhanes_survival_prep(
#   analytic_mort,
#   origin     = "exam",
#   time_unit  = "years",
#   weight_var = "WTMEC2YR"
# )
# 
# # Follow-up by cycle — note shrinking maximum as cycles approach 2019
# nhanes_followup_summary(surv_data)

## ----survprep-cvd-------------------------------------------------------------
# nhanes_ucod_labels()   # see available cause-of-death codes
# 
# surv_cvd <- nhanes_survival_prep(
#   analytic_mort,
#   origin     = "exam",
#   time_unit  = "years",
#   cause      = "001",   # Diseases of heart
#   weight_var = "WTMEC2YR"
# )
# 
# table(event = surv_cvd$event, cvd_death = surv_cvd$event_cause)

## ----survey-design------------------------------------------------------------
# surv_data$wt_pooled <- surv_data$survey_weight / 10
# 
# # Scale continuous predictors to per-SD units for interpretable hazard ratios
# surv_data$TC_sd  <- scale(surv_data$TC_mgdl)[,  1]
# surv_data$HDL_sd <- scale(surv_data$HDL_mgdl)[, 1]
# 
# design <- svydesign(
#   id      = ~SDMVPSU,
#   strata  = ~SDMVSTRA,
#   weights = ~wt_pooled,
#   nest    = TRUE,
#   data    = surv_data
# )

## ----cox----------------------------------------------------------------------
# fit <- svycoxph(
#   Surv(time, event) ~ TC_sd + HDL_sd + RIDAGEYR + RIAGENDR +
#                       MI_history + chol_med,
#   design = design
# )
# 
# summary(fit)
# round(exp(cbind(HR = coef(fit), confint(fit))), 3)

## ----harmonize-pattern--------------------------------------------------------
# # General pattern for any analyte
# analyte_list <- nhanes_download_analyte("search term", cycles,
#                                          component = "Laboratory")
# analyte      <- nhanes_harmonize(analyte_list,
#                                   unit  = "mg/dL",
#                                   name  = "my_variable",
#                                   label_pattern = "search term")
# 
# # For questionnaire variables (no unit to match), use mapping instead
# quest_list <- nhanes_download_analyte("keyword", cycles,
#                component = "Questionnaire",
#                keep_vars = c("VAR_OLD", "VAR_NEW"))
# quest <- nhanes_harmonize(quest_list,
#            mapping = c(VAR_OLD = "my_flag", VAR_NEW = "my_flag"))

