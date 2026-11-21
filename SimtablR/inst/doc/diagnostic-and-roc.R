## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4.5
)
library(SimtablR)
data(epitabl)
has_pROC <- requireNamespace("pROC", quietly = TRUE)
has_ggplot2 <- requireNamespace("ggplot2", quietly = TRUE)

## ----diagnostic-sample--------------------------------------------------------
# Subset participants enrolled in the bedside biomarker substudy
diagnostic <- subset(epitabl, diagnostic_substudy == "Yes")

c(
  total_enrolled = nrow(epitabl),
  substudy_enrolled = nrow(diagnostic),
  observed_index_tests = sum(!is.na(diagnostic$poc_hstn_positive)),
  observed_reference_standards = sum(!is.na(diagnostic$adjudicated_acs))
)

## ----diag-accuracy------------------------------------------------------------
accuracy <- diag_test(
  diagnostic,
  test = poc_hstn_positive,
  ref = adjudicated_acs,
  positive = "Yes",
  test_positive = "Positive",
  ci = "exact"
)
accuracy

## ----diag-plot, fig.alt="Fourfold plot showing true positives, false positives, false negatives, and true negatives"----
plot(accuracy, main = "Point-of-Care Troponin Confusion Matrix")

## ----diag-extract-------------------------------------------------------------
# Extract structured evidence table
head(as.data.frame(accuracy, tidy = TRUE), 8)

## ----roc-curve, eval=has_pROC-------------------------------------------------
troponin_roc <- roc(
  diagnostic,
  marker = poc_hstn_value,
  outcome = adjudicated_acs,
  positive = "Yes",
  cutpoint = "none"
)
troponin_roc

## ----roc-youden, eval=has_pROC------------------------------------------------
troponin_youden <- roc(
  diagnostic,
  marker = poc_hstn_value,
  outcome = adjudicated_acs,
  positive = "Yes",
  cutpoint = "youden"
)
troponin_youden

## ----roc-plot, eval=has_pROC && has_ggplot2, fig.alt="ROC curve displaying sensitivity versus 1 - specificity"----
ggplot2::autoplot(troponin_roc) +
  ggplot2::labs(
    title = "ROC Discrimination: Point-of-Care Troponin",
    subtitle = "ESTROBE-ACS Diagnostic Substudy"
  )

## ----stard-check--------------------------------------------------------------
stard_report <- stard(accuracy)
head(as.data.frame(stard_report), 8)

## ----stard-methods------------------------------------------------------------
cat(as_methods(accuracy))

