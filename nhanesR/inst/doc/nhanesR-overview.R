## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  eval     = FALSE
)

## ----install------------------------------------------------------------------
# # Install from GitHub (includes vignettes)
# remotes::install_github("dwinsemius/nhanesR",
#                         build_vignettes = TRUE,
#                         force           = TRUE)
# library(nhanesR)

## ----rprofile-----------------------------------------------------------------
# options(
#   nhanesR.cache_dir = "/data/nhanes_cache",  # e.g. a shared server path
#   nhanesR.verbose   = FALSE,                  # suppress progress messages
#   nhanesR.timeout   = 300L                    # 5-minute timeout
# )

## ----options-interactive------------------------------------------------------
# nhanes_cache_dir()                        # view current cache path (tempdir-based by default)
# nhanes_cache_dir("~/my_nhanes_cache")    # opt in to a persistent home-directory cache
# options(nhanesR.verbose = FALSE)     # suppress messages for this session

## ----discovery----------------------------------------------------------------
# # All cycles with metadata
# nhanes_cycles()
# 
# # Extract cycle labels for use downstream
# cycles <- nhanes_cycles()[["cycle"]]
# 
# # See what Laboratory files exist for a cycle
# nhanes_manifest("2015-2016", "Laboratory")

## ----search-------------------------------------------------------------------
# # Summarized view — which variable codes match, and in how many cycles?
# nhanes_search_variables("total cholesterol", component = "Laboratory")
# 
# # Per-cycle lookup — which file holds the analyte in each cycle?
# nhanes_variable_map("total cholesterol")
# 
# # Use keep_vars to exclude false positives (e.g. urine vs. serum creatinine)
# nhanes_variable_map("creatinine",
#                     keep_vars = c("LBXSCR", "LBDSCR", "LB2SCR"))

## ----download-----------------------------------------------------------------
# cycles <- nhanes_cycles()[1:10, "cycle"]   # 1999-2018
# 
# # Demographics — always "DEMO"; nhanes_download() works fine
# demo_list <- nhanes_download("DEMO", cycles)
# 
# # Total cholesterol — file name changed in 1999-2004; use download_analyte()
# tchol_list <- nhanes_download_analyte("total cholesterol", cycles)
# 
# # Questionnaire variable with keep_vars to filter false positives
# mi_list <- nhanes_download_analyte(
#   "heart attack", cycles,
#   component = "Questionnaire",
#   keep_vars = c("MCQ160E", "MCQ160e")
# )

## ----harmonize----------------------------------------------------------------
# # Unit-based: finds the mg/dL column by its label attribute
# tc <- nhanes_harmonize(tchol_list,
#                        unit          = "mg/dL",
#                        name          = "TC_mgdl",
#                        label_pattern = "total cholesterol")
# 
# # Mapping-based: explicit old-name → new-name translation
# mi <- nhanes_harmonize(mi_list,
#                        mapping = c(MCQ160E = "MI_history",
#                                    MCQ160e = "MI_history"))
# 
# # Stack demographics (no renaming needed)
# demo <- nhanes_stack(demo_list)
# 
# # Merge components
# analytic <- nhanes_merge(demo, tc, mi, by = c("SEQN", "cycle"))

## ----mortality----------------------------------------------------------------
# # Cycles with a public-use LMF (NHANES 1999-2018 + NHANES III)
# nhanes_lmf_cycles()
# 
# # Append mortality variables — download happens automatically
# analytic_mort <- nhanes_mortality_link(analytic)

## ----survprep-----------------------------------------------------------------
# # All-cause mortality, time from exam visit
# surv_data <- nhanes_survival_prep(analytic_mort,
#                                   origin     = "exam",
#                                   time_unit  = "years",
#                                   weight_var = "WTMEC2YR")
# 
# # Check follow-up by cycle (note shrinking window near 2017-2018)
# nhanes_followup_summary(surv_data)
# 
# # Cause-specific: what cause codes are available?
# nhanes_ucod_labels()
# 
# # Cardiovascular mortality (code "001")
# surv_cvd <- nhanes_survival_prep(analytic_mort,
#                                  origin = "exam",
#                                  cause  = "001",
#                                  weight_var = "WTMEC2YR")

