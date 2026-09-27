## ----setup, include=FALSE-----------------------------------------------------
library(surveycore)

## ----nest-diagnostic----------------------------------------------------------
# NHANES: only two distinct PSU values, but 15 strata
# Each stratum has its own PSU 1 and PSU 2 → nest = TRUE
length(unique(nhanes_2017$sdmvpsu)) # 2
length(unique(nhanes_2017$sdmvstra)) # 15

## ----nhanes-------------------------------------------------------------------
# Subset to MEC exam participants (ridstatr == 2) before using wtmec2yr.
# The 550 interview-only participants have wtmec2yr = 0 and are not part
# of the exam sample.
nhanes_exam <- nhanes_2017[nhanes_2017$ridstatr == 2, ]

svy_nhanes <- as_survey(
  nhanes_exam,
  ids = sdmvpsu,
  strata = sdmvstra,
  weights = wtmec2yr,
  nest = TRUE # PSU IDs are locally unique within strata
)
svy_nhanes

## ----nhanes-interview---------------------------------------------------------
svy_nhanes_int <- as_survey(
  nhanes_2017,
  ids = sdmvpsu,
  strata = sdmvstra,
  weights = wtint2yr,
  nest = TRUE
)

## ----anes---------------------------------------------------------------------
# Pre-election analysis (party ID, ideology, candidate preference)
svy_anes_pre <- as_survey(
  anes_2024,
  ids = v240103c,
  strata = v240103d,
  weights = v240103a
)

# Post-election analysis (validated vote choice: v242066, v242067)
svy_anes_post <- as_survey(
  anes_2024,
  ids = v240103c,
  strata = v240103d,
  weights = v240103b
)

## ----gss----------------------------------------------------------------------
# Standard analysis weight
svy_gss <- as_survey(
  gss_2024,
  ids = vpsu,
  strata = vstrat,
  weights = wtssps
)

# Non-response adjusted weight (preferred when non-response bias is a concern)
svy_gss_nr <- as_survey(
  gss_2024,
  ids = vpsu,
  strata = vstrat,
  weights = wtssnrps
)

## ----npors--------------------------------------------------------------------
svy_npors <- as_survey(
  pew_npors_2025,
  strata = stratum,
  weights = weight
)

## ----acs----------------------------------------------------------------------
svy_acs <- as_survey_replicate(
  acs_pums_wy,
  weights = pwgtp,
  repweights = pwgtp1:pwgtp80,
  type = "successive-difference"
)
svy_acs

## ----pew-jewish---------------------------------------------------------------
svy_jewish <- as_survey_replicate(
  pew_jewish_2020,
  weights = extweight,
  repweights = extweight1:extweight100,
  type = "JK1"
)
svy_jewish

## ----nwtco, eval=requireNamespace("survival", quietly=TRUE)-------------------
nwtco <- survival::nwtco

# in.subcohort is stored as 0/1 — must be logical for as_survey_twophase()
nwtco$in.subcohort <- as.logical(nwtco$in.subcohort)

# Phase 1: all 4,028 enrolled patients (each patient is their own unit)
phase1 <- as_survey(nwtco, ids = seqno)

# Phase 2: subcohort, with Phase 2 sampling stratified by relapse status
svy_twophase <- as_survey_twophase(
  phase1,
  strata2 = rel, # Phase 2 strata: cases (rel=1) vs. non-cases (rel=0)
  subset = in.subcohort, # Logical column: TRUE = selected into Phase 2
  method = "full"
)
svy_twophase

## ----apisrs-------------------------------------------------------------------
set.seed(101)
N <- 400 # total schools in district
n <- 80 # schools sampled

school_survey <- data.frame(
  school_id = sample(seq_len(N), n),
  avg_score = round(rnorm(n, mean = 72, sd = 11), 1),
  pct_frpl = round(runif(n, 0.10, 0.85), 2), # % free/reduced price lunch
  enrollment = round(runif(n, 180, 850)),
  sw = N / n, # equal sampling weight = 400/80 = 5.0
  fpc = N # population size for FPC
)

svy_srs <- as_survey(
  school_survey,
  weights = sw, # each sampled school represents 5 schools in the population
  fpc = fpc # reduces SEs: we sampled 20% of the population
)
svy_srs

## ----nationscape--------------------------------------------------------------
svy_ns <- as_survey_nonprob(ns_wave1, weights = weight)
svy_ns

# Presidential approval rating (July 2019)
get_freqs(svy_ns, pres_approval)

## ----nonprob-bootstrap--------------------------------------------------------
set.seed(1)
n <- 200
R <- 50

ns_synthetic <- data.frame(
  pres_approval = sample(c("Approve", "Disapprove", "DK"), n, replace = TRUE),
  age_grp = sample(c("18-34", "35-54", "55+"), n, replace = TRUE),
  weight = runif(n, 0.5, 2.5)
)

# Replicate columns: calibration re-applied on each bootstrap draw.
# In practice these come from your vendor alongside the main weight column.
rep_mat <- matrix(runif(n * R, 0.3, 3.0), nrow = n)
colnames(rep_mat) <- paste0("repwt_", seq_len(R))
ns_synthetic <- cbind(ns_synthetic, as.data.frame(rep_mat))

svy_np_boot <- as_survey_nonprob(
  ns_synthetic,
  weights = weight,
  repweights = starts_with("repwt_"),
  type = "bootstrap"
)
svy_np_boot

