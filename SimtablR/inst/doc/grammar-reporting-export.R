## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 7,
  fig.height = 4.5
)
library(SimtablR)
data(epitabl)
has_gt <- requireNamespace("gt", quietly = TRUE)
has_flextable <- requireNamespace("flextable", quietly = TRUE)
has_office <- has_flextable &&
  requireNamespace("officer", quietly = TRUE) &&
  requireNamespace("openxlsx", quietly = TRUE)
has_ggplot2 <- requireNamespace("ggplot2", quietly = TRUE)

## ----grammar-spec-------------------------------------------------------------
plan <- simtab(epitabl) |>
  describe(c(age, sex, bmi, renal_impairment)) |>
  stratify(adjudicated_acs) |>
  set_summary("auto") |>
  missingness(display = TRUE) |>
  overall(TRUE) |>
  test("auto") |>
  label(
    age = "Age (years)",
    bmi = "Body mass index (kg/m²)"
  ) |>
  fmt(d = 1)

# Printing displays the declared plan without computing
plan

## ----grammar-eval-------------------------------------------------------------
validate(plan)

baseline_result <- evaluate(plan)
baseline_result

## ----verb-closure-------------------------------------------------------------
# Add Standardized Mean Differences (SMD) to the computed baseline table
baseline_smd <- baseline_result |> test(smd = TRUE)
baseline_smd

## ----simtablr-report----------------------------------------------------------
report <- simtablr(
  epitabl,
  outcome = adjudicated_acs,
  exposure = renal_impairment,
  vars = c(age, sex, smoking, hypertension, diabetes, renal_impairment),
  adjust = c(age, sex, smoking, hypertension),
  design = "cohort"
)
report

## ----sensitivity-demo---------------------------------------------------------
# Primary association
primary <- tb(
  epitabl,
  renal_impairment,
  adjudicated_acs,
  flags = c("row", "rr"),
  design = "cohort"
)

# Test sensitivity to odds ratio scale and complete-case denominator
sens_res <- sensitivity(
  primary,
  measure = "OR",
  denominator = "complete"
)
sens_res

## ----evalue-demo--------------------------------------------------------------
ev <- e_value(primary)
ev

## ----advise-audit-------------------------------------------------------------
# Review fired advice entries
advise(primary)

# Conduct an exhaustive audit of all rules (fired and silent)
audit_report <- advise(primary, audit = TRUE)
audit_report

## ----why-decisions------------------------------------------------------------
why(primary)

## ----flow-demo----------------------------------------------------------------
study_flow <- flow(report)
study_flow

## ----flow-plot, eval=has_ggplot2, fig.alt="Participant flowchart showing study attrition"----
ggplot2::autoplot(study_flow) +
  ggplot2::labs(title = "Participant Flow: ESTROBE-ACS Cohort")

## ----strobe-demo--------------------------------------------------------------
strobe_summary <- strobe(report)
head(as.data.frame(strobe_summary), 6)

## ----codebook-demo------------------------------------------------------------
cb <- codebook(epitabl[, c("age", "sex", "hypertension", "adjudicated_acs")])
head(cb)

## ----journal-style------------------------------------------------------------
# List registered journal presets
list_journals()

# Apply JAMA styling (formatting, confidence intervals, p-value thresholds)
jama_table <- style(primary, "jama")
jama_table

## ----table-rendering, eval=has_gt---------------------------------------------
as_gt(report$table2)

## ----flextable-rendering, eval=has_flextable----------------------------------
flextable::as_flextable(report$table2)

## ----export-demo, eval=has_office---------------------------------------------
tmp_docx <- tempfile(fileext = ".docx")
tmp_xlsx <- tempfile(fileext = ".xlsx")

export_docx(report, path = tmp_docx)
export_xlsx(report, path = tmp_xlsx)

# Verify exported files exist
file.exists(tmp_docx)
file.exists(tmp_xlsx)

# Clean up temporary demonstration files
unlink(c(tmp_docx, tmp_xlsx))

## ----methods-prose------------------------------------------------------------
cat(as_methods(report))

