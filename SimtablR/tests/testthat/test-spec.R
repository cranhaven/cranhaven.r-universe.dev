library(testthat)
library(SimtablR)
data(epitabl, package = "SimtablR")

test_that("simtab initializes an inert simtab_spec", {
  spec <- simtab(epitabl)
  expect_s3_class(spec, "simtab_spec")
  expect_true(all(c("data_src", "engine", "roles", "summary", "effect") %in% names(spec)))
})

test_that("evaluate transforms simtab_spec into computed simtab_result", {
  spec <- simtab(epitabl) |> describe(age, sex)
  res <- suppressMessages(evaluate(spec))
  expect_s3_class(res, "simtab_result")
  expect_true(all(c("spec", "data", "meta", "used", "advice", "call") %in% names(res)))
})

test_that("grammar verbs adjust, stratify, and measure modify spec correctly", {
  spec <- simtab(epitabl) |>
    describe(hypertension) |>
    stratify(adjudicated_acs) |>
    measure("OR") |>
    adjust(age, sex)
  expect_identical(rlang::as_label(spec$roles$by), "adjudicated_acs")
  expect_identical(spec$effect$measure, "OR")
  expect_identical(unname(vapply(spec$roles$adjust, rlang::as_label, character(1))), c("age", "sex"))
})

test_that("formatting verbs set_summary, missingness, and label update layout", {
  spec <- simtab(epitabl) |>
    describe(age) |>
    set_summary(stat = "mean") |>
    missingness(display = FALSE) |>
    label(age = "Patient Age (years)")
  expect_identical(spec$summary$default, "mean")
  expect_identical(spec$missing$display, FALSE)
  expect_identical(spec$fmt$labels[["age"]], "Patient Age (years)")
})

test_that("verb closure recomputes evidence when applied to simtab_result", {
  res1 <- suppressMessages(table1(epitabl, "age", summary = "median"))
  res2 <- suppressMessages(set_summary(res1, stat = "mean"))
  expect_s3_class(res2, "simtab_result")
  expect_length(res2$data$age$summary$Overall, 2)
})

test_that("terse flags resolve properly through central machinery", {
  t_flags <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, flags = c("row", "or", "p")))
  expect_identical(t_flags$spec$effect$measure, "OR")
  expect_identical(t_flags$meta$flags$by, "row")
})

test_that("simtablr composes Table 1 and Table 2 into simtab_report", {
  rep <- suppressMessages(simtablr(epitabl, outcome = adjudicated_acs, exposure = hypertension, vars = c(age, sex)))
  expect_s3_class(rep, "simtab_report")
  expect_true(all(c("table1", "table2") %in% names(rep$items)))
})

test_that("tidy and as.data.frame methods extract evidence from simtab_result", {
  res <- suppressMessages(table1(epitabl, "age"))
  expect_s3_class(as.data.frame(res), "data.frame")
  expect_s3_class(generics::tidy(res), "data.frame")
})

test_that("spec preserves provenance and analysis decisions", {
  spec <- simtab(epitabl) |> describe(age)
  expect_type(spec$data_src$hash, "character")
  expect_true(nzchar(spec$data_src$hash))
})

test_that("invalid measure or design combinations throw classed errors", {
  expect_error(simtab(epitabl) |> measure("INVALID_MEASURE") |> evaluate())
})

test_that("stratify() on a tb result reaches the bivariate engine", {
  base <- suppressMessages(tb(epitabl, renal_impairment, mace_event, design = "cohort", ref = "No"))
  verb <- suppressMessages(stratify(base, sex))
  direct <- suppressMessages(tb(epitabl, renal_impairment, mace_event, design = "cohort", ref = "No", strat = sex))

  expect_true(verb$meta$is_stratified)
  expect_identical(verb$data$mh, direct$data$mh)
  expect_match(verb$meta$col_label, "Observed MACE event before censoring (Stratified)", fixed = TRUE)
})

test_that("adjust() on model results refits with the added covariates", {
  r <- suppressMessages(regtab(epitabl, "rehospitalized", ~ renal_impairment + age, family = binomial("log")))
  ra <- suppressMessages(adjust(r, sex, smoking))
  direct <- suppressMessages(regtab(
    epitabl, "rehospitalized", ~ renal_impairment + age + sex + smoking, family = binomial("log")
  ))
  expect_setequal(ra$data$term, direct$data$term)
  expect_equal(ra$data$estimate, direct$data$estimate)

  skip_if_not_installed("survival")
  s <- survtab(epitabl, mace_time_days, mace_event, ~ renal_impairment + age)
  expect_true("sexMale" %in% suppressMessages(adjust(s, sex))$data$terms$term)
})

test_that("label() on a regtab result relabels predictor rows", {
  r <- suppressMessages(regtab(epitabl, "rehospitalized", ~ renal_impairment + age, family = binomial("log")))
  expect_true("Age (y)" %in% as.data.frame(label(r, age = "Age (y)"))$Variable)
  expect_identical(label(r, age = "Age (y)")$data, r$data)
})

test_that("measure() on a result keeps the recorded reference and confidence level", {
  a <- suppressMessages(tb(epitabl, renal_impairment, mace_event, design = "cohort", ref = "Yes", conf.level = 0.9))
  m <- suppressMessages(measure(a, "OR"))
  expect_identical(m$spec$effect$ref, "Yes")
  expect_identical(m$spec$effect$conf.level, 0.9)
  expect_identical(m$data$ratios$level[m$data$ratios$ref], "Yes")

  explicit <- suppressMessages(measure(a, "OR", ref = "No", conf.level = 0.95))
  expect_identical(explicit$spec$effect$ref, "No")
  expect_identical(explicit$spec$effect$conf.level, 0.95)
})

test_that("label() on a stratified tb result updates the stratified column header", {
  res <- suppressMessages(tb(epitabl, renal_impairment, mace_event, rr, strat = sex))
  relabelled <- label(res, mace_event = "365-day MACE")

  expect_identical(relabelled$meta$col_label, "365-day MACE (Stratified)")
  expect_identical(relabelled$data, res$data)
  expect_output(print(relabelled), "365-day MACE (Stratified)", fixed = TRUE)

  # The unstratified table relabels without a suffix.
  plain <- label(suppressMessages(tb(epitabl, renal_impairment, mace_event)), mace_event = "MACE")
  expect_identical(plain$meta$col_label, "MACE")
})

test_that("register_engine() rejects renderers that are not functions", {
  compute <- function(spec, data) list(data = data.frame(n = nrow(data)), meta = list())
  expect_error(
    register_engine("simtab_test_bad_renderer", compute, renderers = list(print = "not a function")),
    class = "simtab_error_input"
  )
  expect_error(
    register_engine("simtab_test_bad_renderer", compute, renderers = function(x, ...) x),
    class = "simtab_error_input"
  )
  expect_false("simtab_test_bad_renderer" %in% list_engines())

  register_engine(
    "simtab_test_good_renderer", compute,
    renderers = list(as_data_frame = function(x, ...) x$data)
  )
  on.exit(rm(list = "simtab_test_good_renderer", envir = SimtablR:::.simtab_engine_registry))
  res <- evaluate(engine(simtab(epitabl), "simtab_test_good_renderer"))
  expect_identical(as.data.frame(res)$n, nrow(epitabl))
})

test_that("verbs an engine does not use warn and leave the result unchanged", {
  diag <- suppressMessages(diag_test(epitabl, poc_hstn_positive, adjudicated_acs))
  expect_warning(same <- measure(diag, "OR"), "has no effect on a result from the 'accuracy' engine")
  expect_identical(same, diag)

  fit <- suppressMessages(regtab(epitabl, "mace_event", ~ age, family = binomial()))
  expect_warning(test(fit, "chisq"), "Verbs this engine uses: adjust()", fixed = TRUE)
  expect_no_warning(suppressMessages(adjust(fit, sex)))

  tab <- suppressMessages(tb(epitabl, sex, mace_event))
  expect_warning(adjust(tab, age), "'bivariate' engine")

  compute <- function(spec, data) list(data = data.frame(n = nrow(data)), meta = list())
  expect_error(
    register_engine("simtab_test_bad_verbs", compute, verbs = "frobnicate"),
    class = "simtab_error_input"
  )
})

test_that("stratify() on a survtab result fits a stratified Cox model", {
  skip_if_not_installed("survival")
  fit <- suppressMessages(survtab(epitabl, time = mace_time_days, event = mace_event, ~ age + renal_impairment))
  strat <- stratify(fit, sex)
  ref <- survival::coxph(
    survival::Surv(mace_time_days, mace_event == "Yes") ~ age + renal_impairment + survival::strata(sex),
    data = epitabl
  )
  expect_equal(strat$data$terms$estimate, unname(exp(stats::coef(ref))))
  expect_identical(strat$meta$strata, "sex")
  expect_output(print(strat), "Stratified by: sex", fixed = TRUE)
  expect_match(as_methods(strat), "stratified by sex", fixed = TRUE)

  # A stratifier that is also a covariate leaves the linear predictor.
  both <- stratify(suppressMessages(survtab(epitabl, time = mace_time_days, event = mace_event, ~ age + sex)), sex)
  expect_identical(both$data$terms$term, "age")
  expect_error(
    stratify(suppressMessages(survtab(epitabl, time = mace_time_days, event = mace_event, ~ sex)), sex),
    class = "simtab_error_spec"
  )
})

test_that("partially labelled numeric columns stay numeric", {
  skip_if_not_installed("haven")
  dat <- data.frame(
    grp = haven::labelled(rep(1:2, 10), c(Control = 1, Treated = 2)),
    age = haven::labelled(c(30:47, 999, 999), c(Unknown = 999), label = "Age (y)"),
    smk = haven::labelled(rep(1:2, each = 10), c(Never = 1, Current = 2))
  )
  expect_message(res <- table1(dat, c(age, smk), by = grp), "Kept partially labelled numeric column")
  expect_identical(res$data$age$type, "continuous")
  expect_identical(res$data$age$label, "Age (y)")
  # Fully labelled columns are still converted to factors with their labels.
  expect_identical(res$data$smk$levels, c("Never", "Current"))
  expect_true(all(c("Control", "Treated") %in% names(res$data$age$summary)))
})

test_that("register_engine() protects built-in engines unless overwrite = TRUE", {
  compute <- function(spec, data) list(data = data.frame(n = nrow(data)), meta = list())
  expect_error(register_engine("Descriptive", compute), class = "simtab_error_input")
  expect_s3_class(suppressMessages(table1(epitabl, age)), "simtab_table1")

  # Re-registering a user engine needs no flag.
  register_engine("simtab_test_reregister", compute)
  on.exit(rm(list = "simtab_test_reregister", envir = SimtablR:::.simtab_engine_registry))
  expect_no_error(register_engine("simtab_test_reregister", compute))
  expect_error(register_engine("simtab_test_reregister", compute, overwrite = NA), class = "simtab_error_input")
})
