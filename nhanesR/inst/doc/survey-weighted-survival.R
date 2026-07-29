## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>",
  eval     = FALSE
)

## ----load-data----------------------------------------------------------------
# library(nhanesR)
# library(rms)
# library(survey)
# library(survival)
# library(flextable)
# 
# dat <- readRDS("~/Documents/R.code/nhanesR/analytic_survival.rds")
# 
# # Analysis population: non-statin users, adults >= 20, landmark > 2yr,
# # complete GGT / albumin / TC / BMI / PIR
# dat2 <- subset(dat,
#   ELIGSTAT == 1 & !is.na(time) & time > 2 & statin == FALSE &
#   !is.na(GGT) & !is.na(LBXSAL) & !is.na(TC) &
#   !is.na(BMI) & !is.na(INDFMPIR) & RIDAGEYR >= 20
# )
# # N = 34,456  events = 3,975

## ----survey-design------------------------------------------------------------
# # NHANES design: create on the FULL dataset, then subset the design object.
# # Creating the design on the already-subsetted data can leave some strata with
# # a single PSU, causing svycoxph() to fail at variance estimation.
# full_design <- svydesign(
#   ids     = ~SDMVPSU,
#   strata  = ~SDMVSTRA,
#   weights = ~WTMEC2YR,
#   nest    = TRUE,
#   data    = dat
# )
# sub_design <- subset(full_design,
#   ELIGSTAT == 1 & !is.na(time) & time > 2 & statin == FALSE &
#   !is.na(GGT) & !is.na(LBXSAL) & !is.na(TC) &
#   !is.na(BMI) & !is.na(INDFMPIR) & RIDAGEYR >= 20
# )

## ----fit-models---------------------------------------------------------------
# # Shared formula: RCS(4 knots) on GGT and albumin; linear adjusters
# f <- Surv(time, event) ~ rcs(GGT, 4) + rcs(LBXSAL, 4) +
#        RIDAGEYR + RIAGENDR + RIDRETH1 + log(BMI)
# 
# # cph() fit — x=TRUE, y=TRUE, surv=TRUE needed for Predict() and survplot()
# # Inference from this fit is NOT survey-correct; used only for $Design structure
# dd <- datadist(dat2)
# options(datadist = "dd")
# fit_cph <- cph(f, data = dat2, x = TRUE, y = TRUE, surv = TRUE)
# 
# # svycoxph() fit — survey-correct coefficients and sandwich vcov
# fit_svy <- svycoxph(f, design = sub_design)

## ----examine-cph-structure----------------------------------------------------
# names(fit_cph)

## ----examine-svy-structure----------------------------------------------------
# names(fit_svy)

## ----compare-coefs-table------------------------------------------------------
# tbl_coef <- data.frame(
#   Term    = names(coef(fit_cph)),
#   cph     = round(coef(fit_cph), 4),
#   svy     = round(coef(fit_svy), 4),
#   diff    = round(coef(fit_svy) - coef(fit_cph), 4),
#   SE_cph  = round(sqrt(diag(vcov(fit_cph))), 4),
#   SE_svy  = round(sqrt(diag(vcov(fit_svy))), 4),
#   SE_ratio = round(sqrt(diag(vcov(fit_svy))) / sqrt(diag(vcov(fit_cph))), 3),
#   row.names = NULL
# )
# 
# flextable(tbl_coef) |>
#   set_header_labels(
#     Term     = "Term",
#     cph      = "β (cph)",
#     svy      = "β (svycoxph)",
#     diff     = "Δβ",
#     SE_cph   = "SE (cph)",
#     SE_svy   = "SE (svycoxph)",
#     SE_ratio = "SE ratio"
#   ) |>
#   colformat_double(digits = 4) |>
#   bold(j = "SE_ratio", bold = TRUE) |>
#   add_footer_lines("SE ratio > 1 indicates design effect from cluster sampling. Both β and SE differ materially, requiring substitution of both from svycoxph.") |>
#   autofit()

## ----design-structure---------------------------------------------------------
# str(fit_cph$Design, max.level = 2)

## ----slot-names---------------------------------------------------------------
# # Confirm positional correspondence; names will differ
# length(coef(fit_cph)) == length(coef(fit_svy))  # TRUE
# names(coef(fit_cph))   # rms short names
# names(coef(fit_svy))   # full formula names

## ----svycph-fuse-source-------------------------------------------------------
# # Source the implementation (see R/svycph_fuse.R)
# # devtools::load_all("~/Documents/R.code/nhanesR")

## ----apply-fusion-------------------------------------------------------------
# fit_fused <- svycph_fuse(fit_cph, fit_svy)

## ----test-anova---------------------------------------------------------------
# anova(fit_fused)   # survey-correct
# anova(fit_cph)     # naive — overstates significance

## ----test-predict-------------------------------------------------------------
# # Predict() works: survey-correct CIs on the GGT smooth effect
# p <- Predict(fit_fused, GGT = seq(5, 150, by = 5), fun = exp)
# plot(p, ylab = "Hazard Ratio (vs median GGT)",
#      xlab = "GGT (U/L)")

## ----weighted-basehaz---------------------------------------------------------
# # Lin design variance (default) — correct for population inference
# h0_lin <- weighted_basehaz(fit_svy, design = sub_design, se_type = "lin")
# 
# # Greenwood-weighted — interpretable survplot() confidence bands
# h0_gw  <- weighted_basehaz(fit_svy, design = sub_design, se_type = "greenwood")
# 
# head(h0_gw)

## ----compare-se-scale---------------------------------------------------------
# # SE scale comparison (log H0 scale, late follow-up):
# # cph unweighted std.err  ~ 0.005  (sample-scale statistical precision)
# # Greenwood-weighted      ~ 0.0002 (population-scale statistical precision)
# # Lin design              ~ 1e-6   (PSU-selection uncertainty)
# data.frame(
#   method    = c("cph unweighted", "Greenwood-weighted", "Lin design"),
#   std.err   = c(
#     mean(tail(fit_cph$std.err, 5), na.rm = TRUE),
#     mean(tail(h0_gw$std.err, 5)),
#     mean(tail(h0_lin$std.err, 5))
#   )
# )

## ----compare-basehaz----------------------------------------------------------
# h0_naive <- basehaz(fit_cph, centered = TRUE)
# 
# plot(h0_naive$time, h0_naive$hazard, type = "s",
#      xlab = "Time (years)", ylab = "Cumulative baseline hazard",
#      main = "Weighted vs. unweighted baseline hazard")
# lines(h0_gw$time, h0_gw$hazard, type = "s", col = "steelblue")
# legend("topleft", c("Unweighted (cph)", "Weighted (svycoxph)"),
#        col = c("black", "steelblue"), lty = 1)

## ----substitute-basehaz-------------------------------------------------------
# # Substitute Greenwood-weighted hazard for survplot() with visible bands
# fit_fused <- svycph_set_basehaz(fit_fused, h0_gw)

## ----test-survplot------------------------------------------------------------
# survplot(fit_fused, GGT = c(20, 50, 100), conf = "bands",
#          xlab = "Follow-up (years)", ylab = "Survival",
#          label.curves = list(keys = "lines"))

## ----survey-df----------------------------------------------------------------
# # svycoxph stores degf.resid = n_PSU - n_strata directly; no manual computation needed
# fit_svy$degf.resid   # e.g. 138 for the NHANES 1999-2018 analysis population
# fit_fused$svycph_vcov_df  # same value, copied into fused object by svycph_fuse()

## ----regTermTest-comparison---------------------------------------------------
# # regTermTest() as a check on borderline spline nonlinearity results
# regTermTest(fit_svy, ~ rcs(GGT, 4))    # overall GGT association
# regTermTest(fit_svy, ~ rcs(LBXSAL, 4)) # overall albumin association

## ----icc-screen---------------------------------------------------------------
# library(lme4)
# library(performance)
# 
# # ICC for each analyte across PSUs — low ICC suggests non-informative sampling
# analytes <- c("GGT", "LBXSAL", "TC", "BMI", "RIDAGEYR")
# icc_tbl  <- lapply(analytes, function(v) {
#   m   <- lmer(as.formula(paste(v, "~ 1 + (1|SDMVPSU)")), data = dat2, REML = TRUE)
#   icc <- performance::icc(m)$ICC_adjusted
#   data.frame(analyte = v, ICC = round(icc, 4))
# })
# do.call(rbind, icc_tbl)

## ----deff-screen--------------------------------------------------------------
# # DEFF from design: (design SE / naive SE)^2 for each analyte mean
# deff_tbl <- lapply(analytes, function(v) {
#   se_design <- SE(svymean(reformulate(v), sub_design))
#   se_naive  <- sd(dat2[[v]], na.rm = TRUE) / sqrt(sum(!is.na(dat2[[v]])))
#   data.frame(analyte = v, DEFF = round((se_design / se_naive)^2, 3))
# })
# do.call(rbind, deff_tbl)

## ----session-info-------------------------------------------------------------
# sessionInfo()

