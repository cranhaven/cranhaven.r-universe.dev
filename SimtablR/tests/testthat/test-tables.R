library(testthat)
library(SimtablR)
data(epitabl, package = "SimtablR")

test_that("table1 produces basic descriptive table across variable types", {
  tab <- suppressMessages(table1(epitabl, vars = c("age", "sex", "hypertension")))
  expect_s3_class(tab, "simtab_table1")
  expect_s3_class(tab, "simtab_result")
  expect_true(all(c("age", "sex", "hypertension") %in% names(tab$data)))
  expect_type(tab$data$age$summary$Overall, "double")
})

test_that("table1 handles stratification by group", {
  tab <- suppressMessages(table1(epitabl, vars = c("age", "sex"), by = "adjudicated_acs"))
  expect_equal(rlang::eval_tidy(tab$spec$roles$by), "adjudicated_acs")
  expect_true(all(c("Overall", "No", "Yes") %in% colnames(tab$data$sex$freq)))
})

test_that("table1 supports continuous summary presets (mean/sd vs median/iqr)", {
  t_med <- suppressMessages(table1(epitabl, "age", summary = "median"))
  expect_length(t_med$data$age$summary$Overall, 3) # median, Q1, Q3
  t_mean <- suppressMessages(table1(epitabl, "age", summary = "mean"))
  expect_length(t_mean$data$age$summary$Overall, 2) # mean, SD
})

test_that("table1 handles missingness options", {
  t_no <- suppressMessages(table1(epitabl, "age", missing = FALSE))
  t_yes <- suppressMessages(table1(epitabl, "age", missing = TRUE))
  expect_s3_class(t_no, "simtab_table1")
  expect_s3_class(t_yes, "simtab_table1")
})

test_that("table1 reports p-values and test methods", {
  tab <- suppressMessages(table1(epitabl, vars = "sex", by = "adjudicated_acs", test = TRUE))
  expect_true(!is.null(tab$data$sex$test$p.value))
  expect_true(is.numeric(tab$data$sex$test$p.value))
})

test_that("tb computes unadjusted 2x2 contingency table and odds ratio", {
  tab <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, flags = c("row", "or")))
  expect_s3_class(tab, "tb")
  expect_s3_class(tab, "simtab_result")
  expect_true(!is.null(tab$data$ratios$estimate))
  expect_true(is.numeric(tab$data$ratios$estimate))
})

test_that("tb computes relative risk and prevalence ratio", {
  tab_rr <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, flags = "rr"))
  tab_pr <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, flags = "pr"))
  expect_identical(tab_rr$spec$effect$measure, "RR")
  expect_identical(tab_pr$spec$effect$measure, "PR")
})

test_that("tb supports Mantel-Haenszel stratification by a third variable", {
  tab <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, strat = sex, flags = "or"))
  expect_s3_class(tab, "tb")
  expect_true(!is.null(tab$data$mh))
})

test_that("tb supports hypothesis testing options (chi-square and Fisher)", {
  tab_chi <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, test = "chisq"))
  tab_fish <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, test = "fisher"))
  expect_true(is.numeric(tab_chi$meta$stats$p.value))
  expect_true(is.numeric(tab_fish$meta$stats$p.value))
})

test_that("tb calculates row, col, and cell percentages", {
  tab_row <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, flags = "row"))
  tab_col <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, flags = "col"))
  tab_cell <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs, flags = "cell"))
  expect_s3_class(tab_row, "tb")
  expect_s3_class(tab_col, "tb")
  expect_s3_class(tab_cell, "tb")
})

test_that("rbind stacks multiple tb objects into simtab_rbind_tb", {
  t1 <- suppressMessages(tb(epitabl, hypertension, adjudicated_acs))
  t2 <- suppressMessages(tb(epitabl, sex, adjudicated_acs))
  stacked <- rbind(t1, t2)
  expect_s3_class(stacked, "simtab_rbind_tb")
  expect_length(stacked$data, 2)
})

test_that("table1 and tb raise simtab errors on invalid variables or empty input", {
  expect_error(table1(epitabl, vars = "nonexistent_variable"))
  expect_error(tb(epitabl, nonexistent, adjudicated_acs))
})

test_that("tb does not report a goodness-of-fit p-value for a single-level table", {
  one_level <- data.frame(
    exposure = factor(c("x", "x", "x", "x"), levels = c("x", "y")),
    outcome = factor(c("p", "q", "p", "q"))
  )
  expect_warning(
    res <- suppressMessages(tb(one_level, exposure, outcome, test = TRUE)),
    "No association test was computed"
  )
  expect_null(res$meta$stats)
  expect_false(grepl("given probabilities", paste(capture.output(print(res)), collapse = "\n")))

  # The grammar path recomputes through the same guard.
  expect_warning(retested <- test(res, "chisq"), "No association test was computed")
  expect_null(retested$meta$stats)
})

test_that("test(smd = TRUE) on a tb result says it is ignored instead of silently dropping it", {
  res <- suppressMessages(tb(epitabl, smoking, mace_event))
  expect_warning(with_smd <- test(res, smd = TRUE), "smd = TRUE was ignored")
  expect_identical(with_smd$data, res$data)

  # table1() still honours the same request.
  t1 <- suppressMessages(table1(epitabl, smoking, by = mace_event))
  expect_true("SMD" %in% names(as.data.frame(test(t1, smd = TRUE))))
})

test_that("tidy categorical tb output carries the cell counts", {
  res <- suppressMessages(tb(epitabl, smoking, mace_event, rr, ref = "Never"))
  tidy_df <- as.data.frame(res, tidy = TRUE)
  expect_true("n" %in% names(tidy_df))
  freq <- res$data$frequencies
  expect_identical(
    tidy_df$n[tidy_df$level == "Current" & tidy_df$outcome == "Yes"],
    as.integer(freq["Current", "Yes"])
  )
  expect_identical(sum(tidy_df$n), sum(freq))
  expect_identical(generics::tidy(res)$n, tidy_df$n)

  stacked <- as.data.frame(rbind(res, suppressMessages(tb(epitabl, sex, mace_event))), tidy = TRUE)
  expect_identical(sum(stacked$n), 2L * nrow(epitabl))
})

test_that("stratified tb printing shows the Mantel-Haenszel row once", {
  res <- suppressMessages(tb(epitabl, renal_impairment, mace_event, rr, strat = sex))
  out <- withr::with_options(list(width = 80), capture.output(print(res)))
  expect_identical(sum(grepl("Mantel-Haenszel pooled", out, fixed = TRUE)), 1L)
  expect_true(any(grepl("Mantel-Haenszel pooled: Yes .*\\d\\.\\d{2} \\(", out)))

  wide <- withr::with_options(list(width = 250), capture.output(print(res)))
  expect_identical(sum(grepl("Mantel-Haenszel pooled", wide, fixed = TRUE)), 1L)
})

test_that("table1 accepts the documented per-variable ref list", {
  res <- suppressMessages(table1(
    epitabl, c(sex, smoking), by = mace_event, measure = "OR",
    ref = list(sex = "Male", smoking = "Current")
  ))
  expect_identical(res$data$sex$crude$level[res$data$sex$crude$ref], "Male")
  expect_identical(res$data$smoking$crude$level[res$data$smoking$crude$ref], "Current")

  # Same plan through the grammar, and kept by measure() on the result.
  spec <- simtab(epitabl) |>
    describe(c(sex, smoking)) |>
    stratify(mace_event) |>
    measure("OR", ref = list(sex = "Male", smoking = "Current"))
  grammar <- suppressMessages(evaluate(spec))
  expect_identical(grammar$data, res$data)
  remeasured <- suppressMessages(measure(grammar, "RR"))
  expect_identical(remeasured$data$smoking$crude$level[remeasured$data$smoking$crude$ref], "Current")

  expect_error(
    table1(epitabl, sex, by = mace_event, measure = "OR", ref = list("Male", "Female")),
    class = "simtab_error_spec"
  )
})

test_that("table1 warns when a requested reference level is never applied", {
  expect_warning(
    suppressMessages(table1(epitabl, smoking, by = mace_event, measure = "OR", ref = "Nope")),
    "Reference level not applied"
  )
  expect_warning(
    suppressMessages(table1(epitabl, c(sex, smoking), by = mace_event, measure = "OR", ref = list(sex = "X"))),
    "'X' is not a level of 'sex'"
  )
  # A shared scalar that matches some variables is the documented behaviour.
  expect_no_warning(suppressMessages(
    table1(epitabl, c(sex, smoking), by = mace_event, measure = "OR", ref = "Former")
  ))
})

test_that("stratified tb tests the exposure-outcome association within strata", {
  ref_2x2 <- stats::mantelhaen.test(table(epitabl$renal_impairment, epitabl$mace_event, epitabl$sex))
  no_measure <- suppressMessages(tb(epitabl, renal_impairment, mace_event, strat = sex, test = TRUE))
  expect_identical(no_measure$meta$stats$method, "Cochran-Mantel-Haenszel test")
  expect_equal(no_measure$meta$stats$p.value, ref_2x2$p.value)

  # With a measure the table-level test equals the pooled Mantel-Haenszel row.
  with_rr <- suppressMessages(tb(epitabl, renal_impairment, mace_event, rr, strat = sex, test = TRUE))
  expect_equal(with_rr$meta$stats$p.value, with_rr$data$mh$cmh_p[with_rr$data$mh$row_type == "pooled"])

  # A multi-level exposure gets the generalized CMH test, not its first level's.
  ref_3x2 <- stats::mantelhaen.test(table(epitabl$smoking, epitabl$mace_event, epitabl$sex))
  multi <- suppressMessages(tb(epitabl, smoking, mace_event, or, strat = sex, test = TRUE))
  expect_equal(multi$meta$stats$p.value, ref_3x2$p.value)
  expect_equal(unname(multi$meta$stats$parameter), 2)

  # The verb path recomputes the same test.
  verb <- suppressMessages(stratify(tb(epitabl, renal_impairment, mace_event, test = TRUE), sex))
  expect_equal(verb$meta$stats$p.value, ref_2x2$p.value)
})

test_that("stratified tb with the miss flag prints when a stratum value is missing", {
  dat <- epitabl
  dat$sex[1:20] <- NA
  res <- suppressMessages(tb(dat, renal_impairment, mace_event, rr, miss, strat = sex))
  expect_output(print(res), "Mantel-Haenszel pooled: Yes")
})

test_that("tb honours or rejects named test methods instead of ignoring them", {
  continuous <- suppressMessages(tb(epitabl, age, mace_event))
  wil <- test(continuous, "wilcoxon")
  expect_equal(
    wil$meta$stats$p.value,
    stats::wilcox.test(age ~ mace_event, data = epitabl, exact = FALSE)$p.value
  )
  kw <- test(suppressMessages(tb(epitabl, age, smoking)), "kruskal")
  expect_equal(kw$meta$stats$p.value, stats::kruskal.test(age ~ smoking, data = epitabl)$p.value)

  expect_error(test(continuous, "fisher"), class = "simtab_error_input")
  expect_error(suppressMessages(tb(epitabl, age, mace_event, test = "fisher")), class = "simtab_error_input")
  expect_error(test(suppressMessages(tb(epitabl, age, smoking)), "t"), class = "simtab_error_input")
  expect_error(test(suppressMessages(tb(epitabl, sex, mace_event)), "t"), class = "simtab_error_input")
})

test_that("stratified RR/PR tables test homogeneity on the ratio scale", {
  rr <- suppressMessages(tb(epitabl, renal_impairment, mace_event, rr, strat = sex))
  strata <- rr$data$mh[rr$data$mh$row_type == "stratum", ]
  pooled <- rr$data$mh[rr$data$mh$row_type == "pooled", ]
  y <- log(strata$estimate)
  se <- (log(strata$upper_ci) - log(strata$lower_ci)) / (2 * stats::qnorm(0.975))
  w <- 1 / se^2
  q <- sum(w * (y - sum(w * y) / sum(w))^2)
  expect_equal(pooled$homogeneity_statistic, q)
  expect_equal(pooled$homogeneity_p, stats::pchisq(q, 1, lower.tail = FALSE))
  expect_match(pooled$homogeneity_method, "Cochran Q", fixed = TRUE)
  out <- paste(capture.output(print(rr)), collapse = "\n")
  expect_match(out, "homogeneity p =", fixed = TRUE)
  expect_false(grepl("BD p", out, fixed = TRUE))

  # Odds-ratio tables keep Breslow-Day, also through the measure() verb.
  or <- suppressMessages(measure(rr, "OR"))
  expect_match(or$data$mh$homogeneity_method[or$data$mh$row_type == "pooled"], "Breslow-Day", fixed = TRUE)
  expect_match(paste(capture.output(print(or)), collapse = "\n"), "BD p =", fixed = TRUE)
})

test_that("stratified continuous tb compares groups within strata", {
  mean_tab <- suppressMessages(tb(epitabl, age, mace_event, strat = sex, test = TRUE))
  ref <- stats::anova(
    stats::lm(age ~ sex, data = epitabl),
    stats::lm(age ~ sex + mace_event, data = epitabl)
  )
  expect_identical(mean_tab$meta$stats$method, "Stratum-adjusted F-test (linear model)")
  expect_equal(mean_tab$meta$stats$p.value, ref$`Pr(>F)`[2])

  # Van Elteren with a single stratum equals the uncorrected normal Wilcoxon test.
  one <- transform(epitabl, all = factor("all"))
  ve_one <- suppressMessages(tb(one, presentation_hours, mace_event, strat = all, summary = "median", test = TRUE))
  expect_identical(ve_one$meta$stats$method, "Van Elteren stratified Wilcoxon test")
  expect_equal(
    ve_one$meta$stats$p.value,
    stats::wilcox.test(presentation_hours ~ mace_event, data = epitabl, exact = FALSE, correct = FALSE)$p.value
  )

  # The stratify() verb and an explicit method route through the same tests.
  verb <- suppressMessages(stratify(tb(epitabl, age, mace_event, test = TRUE), sex))
  expect_equal(verb$meta$stats$p.value, ref$`Pr(>F)`[2])
  expect_identical(test(mean_tab, "wilcoxon")$meta$stats$method, "Van Elteren stratified Wilcoxon test")

  # No stratified rank test for more than two groups: no p-value, a warning.
  expect_warning(
    multi <- suppressMessages(tb(epitabl, presentation_hours, smoking, strat = sex, summary = "median", test = TRUE)),
    "No stratified test was computed"
  )
  expect_null(multi$meta$stats)
})

test_that("continuous SMD uses the absolute Austin convention", {
  res <- suppressMessages(test(table1(epitabl, c(age, sex), by = mace_event), smd = TRUE))
  m <- tapply(epitabl$age, epitabl$mace_event, mean)
  v <- tapply(epitabl$age, epitabl$mace_event, stats::var)
  expect_equal(res$data$age$smd, unname(abs(m[[2]] - m[[1]]) / sqrt(mean(v))))

  # The sign no longer depends on group order.
  flipped <- transform(epitabl, mace_event = factor(mace_event, levels = c("Yes", "No")))
  res_flip <- suppressMessages(test(table1(flipped, age, by = mace_event), smd = TRUE))
  expect_equal(res_flip$data$age$smd, res$data$age$smd)
})

test_that("table1 d applies to continuous summaries only when supplied", {
  default <- suppressMessages(table1(epitabl, c(age, sex), by = mace_event))
  two <- suppressMessages(table1(epitabl, c(age, sex), by = mace_event, d = 2))
  expect_identical(as.data.frame(default)[1, 2], "62.0 (13.2)")
  expect_identical(as.data.frame(two)[1, 2], "61.97 (13.17)")
  expect_identical(as.data.frame(two)[3, 2], "686 (45.73%)")
  expect_identical(as.data.frame(fmt(default, d = 2)), as.data.frame(two))
  expect_identical(fmt(default, d = 2)$data, default$data)

  # Without d, the journal's continuous digits still apply.
  styled <- suppressMessages(table1(epitabl, age, by = mace_event, style = journal_style(digits_cont = 3)))
  expect_identical(as.data.frame(styled)[1, 2], "61.969 (13.166)")
})

test_that("tidy tb output attaches ratios to the event outcome only", {
  res <- suppressMessages(tb(epitabl, smoking, mace_event, rr, ref = "Never"))
  tidy_df <- as.data.frame(res, tidy = TRUE)
  expect_true(all(is.na(tidy_df$estimate[tidy_df$outcome == "No"])))
  current <- tidy_df[tidy_df$level == "Current" & tidy_df$outcome == "Yes", ]
  expect_equal(current$estimate, res$data$ratios$estimate[res$data$ratios$level == "Current"])
  expect_identical(sum(!is.na(tidy_df$estimate)), nrow(res$data$ratios))

  strat <- suppressMessages(tb(epitabl, renal_impairment, mace_event, or, strat = sex))
  st <- as.data.frame(strat, tidy = TRUE)
  expect_true("stratum" %in% names(st))
  pooled <- st[st$stratum == "Mantel-Haenszel pooled", ]
  expect_identical(nrow(pooled), 1L)
  expect_true(is.na(pooled$n))
  expect_equal(pooled$estimate, strat$data$mh$estimate[strat$data$mh$row_type == "pooled"])
  male_yes <- st[st$stratum %in% "Male" & st$level == "Yes" & st$outcome == "Yes", ]
  expect_equal(male_yes$estimate, strat$data$mh$estimate[strat$data$mh$stratum == "Male"])
  expect_identical(sum(st$n, na.rm = TRUE), nrow(epitabl))
})

test_that("table1 shows the explicit-missing adjusted estimate and notes it", {
  res <- suppressMessages(table1(
    epitabl, poc_hstn_positive, by = mace_event, measure = "OR",
    adjust = age, na_model = "explicit"
  ))
  df <- as.data.frame(res)
  adj_col <- grep("^Adjusted OR", names(df), value = TRUE)
  missing_row <- df[attr(df, "row_type") == "missing", ]
  missing_est <- res$data$poc_hstn_positive$adjusted
  missing_est <- missing_est[missing_est$level == "(Missing)", ]
  expect_match(missing_row[[adj_col]], sprintf("^%.2f ", missing_est$estimate))
  expect_identical(missing_row[["OR (95% CI)"]], "")
  expect_match(attr(df, "notes"), "shown in the Missing row", fixed = TRUE)
  expect_output(print(res), "Note: Adjusted models treated missing values", fixed = TRUE)

  # With na_model = "drop" the Missing row keeps counts only.
  dropped <- as.data.frame(suppressMessages(table1(
    epitabl, poc_hstn_positive, by = mace_event, measure = "OR", adjust = age
  )))
  expect_identical(dropped[attr(dropped, "row_type") == "missing", adj_col], "")
  expect_length(attr(dropped, "notes"), 0)
})

test_that("table1 notes when Overall includes rows with a missing by value", {
  dat <- epitabl
  dat$sex[1:50] <- NA
  res <- suppressMessages(table1(dat, age, by = sex))
  note <- attr(as.data.frame(res), "notes")
  expect_identical(note, "Overall includes 50 participants with missing Sex recorded for clinical assessment.")
  expect_output(print(res), "Overall includes 50 participants", fixed = TRUE)
  expect_equal(unname(res$meta$group_n[["Overall"]]), 1500)

  # No note without missing groups, or without an Overall column.
  expect_length(attr(as.data.frame(suppressMessages(table1(epitabl, age, by = sex))), "notes"), 0)
  expect_length(attr(as.data.frame(suppressMessages(table1(dat, age, by = sex, overall = FALSE))), "notes"), 0)

  skip_if_not_installed("gt")
  expect_true(any(grepl("Overall includes 50", unlist(as_gt(res)[["_source_notes"]]), fixed = TRUE)))
})
