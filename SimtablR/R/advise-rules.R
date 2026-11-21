# Consolidated core epidemiological and statistical advice rules.

# Registers didactic rules across 4 clinical/epidemiological pillars:
# A: Tables & Contingency Analysis
# B: Regression & Survival Models
# C: Diagnostics & ROC Analysis
# D: Epidemiological Review & Study Design

#' @keywords internal
#' @noRd
.register_core_advice_rules <- function(version) {

######
  # Tables & Contingency Analysis
  # Evaluates cross-tabulation assumptions, sparse counts, and effect measures.


  # Rule: Small expected counts in 2x2 contingency tables
  # Triggers when expected cell counts violate standard asymptotic chi-squared assumptions.
  register_rule(
    "small_expected_cells",
    applies = function(result) {
      # Checks whether low expected frequency notes were recorded during table computation
      length(.result_test_notes(result, "small_expected_cells")) > 0 ||
        length(.result_test_notes(result, "fisher_low_expected")) > 0
    },
    check = function(result) {
      # Merges Fisher's exact note (<1) and Campbell N-1 chi-squared note (<5) into one advice entry
      .combine_advice_entries(list(
        .advice_fisher_low_expected(result),
        .advice_small_expected_cells(result)
      ))
    },
    severity = 2,
    citation = "Campbell 2007",
    fix = "Use test = \"fisher\" if an exact Fisher-Irwin test is preferred for this sparse table.",
    version = version,
    example_fires = function() {
      table1(.campbell_rule_example_data("small"), "exp", by = "out", test = TRUE)
    },
    example_silent = function() {
      table1(.campbell_rule_example_data("ok"), "exp", by = "out", test = TRUE)
    }
  )


  # Rule: Odds Ratio reported for common outcomes (> 10% prevalence)
  # Warns that OR substantially overestimates Relative Risk / Prevalence Ratio when prevalence is high.

  register_rule(
    "or_as_rr_common_outcome",
    applies = function(result) {
      # Only applies to 2x2/cross-tables reporting OR in cohort or cross-sectional designs
      .is_table_result(result) &&
        .result_uses_measure(result, "OR") &&
        !identical(.result_design(result), "case_control")
    },
    check = function(result) {
      prev <- .outcome_prevalence(result) #Proportion of outcome events in the analysed sample
      if (is.na(prev) || prev <= 0.10) {
        return(NULL)
      }
      list(
        message = sprintf(
          "Outcome is common (%.1f%%); odds ratios can overstate the prevalence/risk ratio.",
          prev * 100
        ),
        why = "When outcomes are common, the odds ratio can move farther from 1 than the prevalence or risk ratio."
      )
    },
    severity = 3,
    citation = "Knol 2011; Barros & Hirakata 2003",
    fix = "Consider measure = 'PR' for cross-sectional tables or Poisson/log-binomial models for adjusted estimates.",
    version = version,
    example_fires = function() {
      dat <- data.frame(
        exposure = factor(rep(c("ref", "exp"), each = 50)),
        outcome = factor(c(rep("No", 25), rep("Yes", 25), rep("No", 10), rep("Yes", 40)))
      )
      table1(dat, "exposure", by = "outcome", measure = "OR")
    },
    example_silent = function() {
      dat <- data.frame(
        exposure = factor(rep(c("ref", "exp"), each = 50)),
        outcome = factor(c(rep("No", 25), rep("Yes", 25), rep("No", 10), rep("Yes", 40)))
      )
      table1(dat, "exposure", by = "outcome", measure = "OR", design = "case_control")
    }
  )

  # Rule: Risk or prevalence ratio reported from a case-control design
  # Case-control sampling fixes the outcome margin, so only the odds ratio is
  # estimable; an explicitly requested RR/PR still wins but is flagged.

  register_rule(
    "ratio_in_case_control",
    applies = function(result) {
      .is_table_result(result) &&
        identical(.result_design(result), "case_control") &&
        .result_uses_measure(result, c("RR", "PR"))
    },
    check = function(result) {
      measure <- toupper(result$spec$effect$measure %||% result$meta$effect %||% "RR")
      list(
        message = sprintf(
          "A %s is reported for a case-control design; only the odds ratio is estimable.",
          measure
        ),
        why = "Case-control sampling fixes the numbers of cases and controls, so outcome risks and prevalences, and their ratios, are not identified from the table."
      )
    },
    severity = 4,
    citation = "Rothman, Greenland & Lash 2008",
    fix = "Use measure = 'OR' (the case-control default), or record the actual study design.",
    version = version,
    example_fires = function() {
      dat <- data.frame(
        exposure = factor(rep(c("ref", "exp"), each = 50)),
        outcome = factor(c(rep("No", 25), rep("Yes", 25), rep("No", 10), rep("Yes", 40)))
      )
      table1(dat, "exposure", by = "outcome", measure = "RR", design = "case_control")
    },
    example_silent = function() {
      dat <- data.frame(
        exposure = factor(rep(c("ref", "exp"), each = 50)),
        outcome = factor(c(rep("No", 25), rep("Yes", 25), rep("No", 10), rep("Yes", 40)))
      )
      table1(dat, "exposure", by = "outcome", design = "case_control")
    }
  )

  # Rule: Multiple unadjusted hypothesis tests in a single descriptive table
  # Flags inflated family-wise Type I error rates when multiple p-values are presented without adjustment.

  register_rule(
    "multiplicity_unadjusted",
    applies = function(result) .is_table_result(result),
    check = function(result) {
      if (!identical((result$meta$p.adjust %||% result$meta$args$p.adjust %||% "none"), "none")) {
        return(NULL)
      }
      # p_values: Vector of raw p-values extracted from all rows in the table
      p_values <- .result_test_p_values(result)
      if (length(p_values) <= 1) {
        return(NULL)
      }
      list(
        message = sprintf("%d unadjusted p-values are reported in this table.", length(p_values)),
        why = "Interpreting several raw p-values as a family inflates the chance of at least one false-positive finding."
      )
    },
    severity = 2,
    citation = "Bender & Lange 2001",
    fix = "Pipe into test(p.adjust = 'holm') for family-wise control or test(p.adjust = 'BH') for false-discovery-rate control.",
    version = version,
    example_fires = function() {
      dat <- data.frame(
        age = c(21:50, 26:55),
        sex = factor(rep(c("female", "male"), 30)),
        smoking = factor(rep(c("never", "former", "current"), 20)),
        disease = factor(rep(c("No", "Yes"), each = 30))
      )
      table1(dat, c("age", "sex", "smoking"), by = "disease", test = TRUE)
    },
    example_silent = function() {
      dat <- data.frame(
        age = c(21:50, 26:55),
        sex = factor(rep(c("female", "male"), 30)),
        smoking = factor(rep(c("never", "former", "current"), 20)),
        disease = factor(rep(c("No", "Yes"), each = 30))
      )
      table1(dat, c("age", "sex", "smoking"), by = "disease", test = TRUE) |> test(p.adjust = "BH")
    }
  )

  # Rule: Covariate baseline imbalance measured by Standardized Mean Difference (SMD)
  # Identifies variables where SMD > 0.10, indicating potential confounding in comparative cohorts.
  register_rule(
    "smd_imbalance",
    applies = function(result) inherits(result, "simtab_table1") && isTRUE(result$meta$smd),
    check = function(result) {
      smd <- .result_smd_values(result)
      # flagged: Subset of variables exceeding standard threshold (0.10)
      flagged <- smd[abs(smd) > 0.10]
      if (length(flagged) == 0) {
        return(NULL)
      }
      lapply(names(flagged), function(v) {
        list(
          message = sprintf(
            "Standardized mean difference for '%s' is %.2f; values above 0.10 often indicate covariate imbalance.",
            result$meta$labels[[v]] %||% v,
            flagged[[v]]
          ),
          why = "SMDs describe between-group balance on a sample-size independent scale and complement, rather than replace, scientific judgement."
        )
      })
    },
    severity = 1,
    citation = "Austin 2009; Yang & Dalton 2012",
    fix = "Inspect imbalance clinically; consider adjustment, matching, weighting, or stratification when appropriate.",
    version = version,
    example_fires = function() {
      dat <- data.frame(
        group = factor(rep(c("A", "B"), each = 8)),
        age = c(45, 47, 48, 50, 52, 53, 55, 58, 50, 51, 54, 57, 59, 60, 63, 66)
      )
      table1(dat, "age", by = "group", summary = "mean", overall = FALSE) |> test(smd = TRUE)
    },
    example_silent = function() {
      dat <- data.frame(
        group = factor(rep(c("A", "B"), each = 8)),
        age = c(45, 47, 48, 50, 52, 53, 55, 58, 45, 47, 48, 50, 52, 53, 55, 58)
      )
      table1(dat, "age", by = "group", summary = "mean", overall = FALSE) |> test(smd = TRUE)
    }
  )

  # Rule: Haldane-Anscombe continuity correction for zero cells
  # Informs the user that +0.5 was added to 2x2 cells to avoid division by zero in OR/RR and SE estimates.
  register_rule(
    "zero_cell_correction",
    applies = function(result) length(.result_test_notes(result, "zero_cell_correction")) > 0,
    check = function(result) {
      lapply(.result_test_notes(result, "zero_cell_correction"), function(note) {
        list(
          message = sprintf(
            "%s for '%s' (level(s): %s) used a Haldane-Anscombe +0.5 correction because a 2x2 cell was zero; the Woolf/Katz CI is corrected.",
            note$measure %||% "The effect measure",
            note$variable %||% "a variable",
            paste(note$levels, collapse = ", ")
          ),
          why = "Adding 0.5 to all four 2x2 cells keeps the odds/risk ratio and its confidence interval estimable instead of NA when a cell is zero."
        )
      })
    },
    severity = 1,
    citation = "Haldane 1956; Anscombe 1956",
    fix = "No action needed; use an exact method (e.g. test = 'fisher') if a continuity-corrected estimate is undesirable.",
    version = version,
    example_fires = function() {
      dat <- data.frame(
        sex = factor(c(rep("F", 20), rep("M", 15))),
        outcome = factor(c(rep("Yes", 10), rep("No", 10), rep("No", 15)))
      )
      table1(dat, "sex", by = "outcome", measure = "OR")
    },
    example_silent = function() {
      dat <- data.frame(
        sex = factor(c(rep("F", 20), rep("M", 15))),
        outcome = factor(c(rep("Yes", 10), rep("No", 10), rep("Yes", 5), rep("No", 10)))
      )
      table1(dat, "sex", by = "outcome", measure = "OR")
    }
  )

  # Rule: Skewed continuous variables forced to display mean (SD)
  # Warns when mean/SD is manually requested on variables exhibiting significant distributional skewness.
  register_rule(
    "forced_mean_skewed",
    applies = function(result) .is_table_result(result) && length(.forced_mean_skewed_flagged_vars(result)) > 0,
    check = function(result) {
      lapply(.forced_mean_skewed_flagged_vars(result), function(v) {

        # x: Raw numeric vector extracted from data
        x <- .result_raw_column(result, v)

        # decision: Result of internal skewness evaluation (decision, skewness score)
        decision <- .auto_summary_decision(x)
        list(
          message = sprintf(
            "'%s' was forced to mean (SD) even though the automatic heuristic recommends median [IQR] (|skewness| = %.3f).",
            v,
            abs(decision$skew)
          ),
          why = "Forcing the mean on a skewed distribution can misrepresent central tendency and understate spread relative to the median/IQR."
        )
      })
    },
    severity = 2,
    citation = "Bland & Altman 1996",
    fix = "Use summary = 'auto' or summary = 'median' to let the skewed distribution drive the summary choice.",
    version = version,
    example_fires = function() {
      dat <- data.frame(x = c(rep(0, 33), rep(1, 16)))
      table1(dat, "x", summary = "mean", var.type = "continuous")
    },
    example_silent = function() {
      dat <- data.frame(x = c(rep(0, 32), rep(1, 17)))
      table1(dat, "x", summary = "mean", var.type = "continuous")
    }
  )


  # Rule: Unstable percentages computed over smol denominators (< 10)
  # Triggers when a comparison group has < 10 subjects while neighbouring groups are larger (>= 20).
  register_rule(
    "tiny_denominator_pct",
    applies = function(result) !is.na(.tiny_denominator_flag(result)),
    check = function(result) {
      list(
        message = sprintf(
          "At least one displayed group has a percentage denominator below 10 (minimum %d) next to a much larger group; percentages can be unstable at this size.",
          as.integer(.tiny_denominator_flag(result))
        ),
        why = "Percentages computed on fewer than 10 observations move in large, discrete jumps and can overstate precision, especially next to a well-powered comparison group."
      )
    },
    severity = 2,
    citation = "Altman 1991",
    fix = "Consider reporting raw counts instead of percentages for groups with fewer than 10 observations.",
    version = version,
    example_fires = function() {
      dat <- data.frame(
        sex = factor(sample(c("M", "F"), 60, replace = TRUE)),
        grp = factor(c(rep("A", 55), rep("B", 5)))
      )
      table1(dat, "sex", by = "grp")
    },
    example_silent = function() {
      dat <- data.frame(
        sex = factor(sample(c("M", "F"), 6, replace = TRUE)),
        grp = factor(c(rep("A", 3), rep("B", 3)))
      )
      table1(dat, "sex", by = "grp")
    }
  )

  # There are other rules I'm looking to add later. Revise the package first, then will come back.

########
  # B: Regression & Survival Models
  # Checks model assumptions, numerical stability, collinearity, and survival hazards.


  # Rule: Complete or quasi-complete separation in logistic regression
  # Warns of infinite/divergent maximum likelihood estimates; recommends Firth's penalization.
  register_rule(
    "logistic_separation_firth",
    applies = .regtab_is_logistic,
    check = function(result) {
      # Skip if Firth's penalized likelihood method is already in use
      if (identical(result$meta$method %||% "glm", "firth")) {
        return(NULL)
      }
      estimates <- result$data$estimate

      # suspicious: ORs (>1000 or <0.001), or optimizer failure
      suspicious <- any(!is.finite(estimates), na.rm = TRUE) ||
        any(estimates > 1000 | estimates < 0.001, na.rm = TRUE) ||
        any(!result$meta$model_info$converged, na.rm = TRUE)
      if (!suspicious) {
        return(NULL)
      }
      list(
        message = "Logistic model shows signs consistent with separation or unstable coefficients.",
        why = "Separation can make maximum-likelihood logistic estimates diverge."
      )
    },
    severity = 4,
    citation = "Heinze & Schemper 2002; van Smeden 2016",
    fix = "Refit with method = \"firth\" or simplify sparse predictors.",
    version = version,
    example_fires = function() {
      dat <- data.frame(y = c(rep(0, 30), rep(1, 30)), x = c(rep(0, 30), rep(1, 30)))
      suppressWarnings(regtab(dat, outcomes = "y", predictors = ~ x, family = stats::binomial("logit")))
    },
    example_silent = function() {
      dat <- data.frame(y = rep(c(0, 1), 40), x = rep(c(0, 1), each = 40))
      regtab(dat, outcomes = "y", predictors = ~ x, family = stats::binomial("logit"))
    }
  )

  # Rule: Adjusted effect model failed to converge
  # multivariable adjustment encountered numerical non-convergence and estimates are unreliable.

  register_rule(
    "adjusted_effect_not_converged",
    applies = function(result) {
      .is_table_result(result) &&
        length(.effect_nonconverged_vars(result)) > 0
    },
    check = function(result) {
      vars <- .effect_nonconverged_vars(result) # variables where the adjusted GLM fit failed convergence
      if (length(vars) == 0) {
        return(NULL)
      }
      labels <- result$meta$labels[vars] %||% vars
      labels <- unname(ifelse(is.na(labels) | !nzchar(labels), vars, labels))
      measure <- toupper(result$meta$effect %||% result$spec$effect$measure %||% "effect")
      list(
        message = sprintf(
          "The adjusted %s model for %s did not converge; the reported adjusted estimate and interval are not trustworthy.",
          measure,
          paste(labels, collapse = ", ")
        ),
        why = "Separation or a near-collinear covariate set makes maximum-likelihood estimates diverge, so the printed estimate can be arbitrarily large with a confident-looking interval."
      )
    },
    severity = 4,
    citation = "van Smeden 2016",
    fix = "Check for separation and sparse covariate cells, drop or combine the offending covariate, or estimate the effect with regtab(method = \"firth\").",
    version = version,
    example_fires = function() {
      suppressWarnings(table1(
        .nonconvergence_rule_example_data("separated"),
        "exposure",
        by = "y",
        measure = "OR",
        adjust = "x"
      ))
    },
    example_silent = function() {
      table1(
        .nonconvergence_rule_example_data("clean"),
        "exposure",
        by = "y",
        measure = "OR",
        adjust = "x"
      )
    }
  )

  # Rule: multicollinearity by Generalized Variance Inflation Factor (GVIF)
  # Severity 3 when VIF > 5; severity 4 when VIF > 10.

  register_rule(
    "collinearity_vif",
    applies = function(result) inherits(result, "simtab_regtab"),
    check = function(result) {

      rows <- .regtab_vif_rows(result)
      if (nrow(rows) == 0) {
        return(NULL)
      }

      rows <- rows[!is.na(rows$vif), , drop = FALSE]
      if (nrow(rows) == 0 || max(rows$vif) <= 5) {
        return(NULL)
      }

      max_idx <- which.max(rows$vif)
      max_vif <- rows$vif[[max_idx]]
      threshold <- if (max_vif > 10) "> 10" else "> 5"
      list(
        severity = if (max_vif > 10) 4L else 3L,
        message = sprintf(
          "Maximum VIF-equivalent is %.2f for term '%s' in outcome '%s' (%s).",
          max_vif,
          rows$vif_term[[max_idx]],
          rows$outcome[[max_idx]],
          threshold
        ),
        why = "GVIF^(1/(2*df)) is squared to the usual VIF scale so multi-df terms use the same >5 and >10 thresholds."
      )
    },
    severity = 3,
    citation = "Fox & Monette 1992",
    fix = "Inspect collinearity, pre-specify the primary contrast, and consider removing or combining redundant predictors.",
    version = version,
    example_fires = function() {
      set.seed(2303)
      n <- 100
      x1 <- stats::rnorm(n)
      x2 <- x1 + stats::rnorm(n, sd = 0.45)
      y <- 1 + 0.2 * x1 - 0.1 * x2 + stats::rnorm(n)
      regtab(data.frame(y, x1, x2), outcomes = "y", predictors = ~ x1 + x2, family = stats::gaussian())
    },
    example_silent = function() {
      n <- 80
      x1 <- rep(c(-1, 1), each = n / 2)
      x2 <- rep(c(-1, 1), times = n / 2)
      y <- 1 + 0.2 * x1 - 0.1 * x2 + rep(c(-0.2, 0.2), length.out = n)
      regtab(data.frame(y, x1, x2), outcomes = "y", predictors = ~ x1 + x2, family = stats::gaussian())
    }
  )

  # Rule: Cox proportional hazards assumption violated

  register_rule(
    "cox_ph_violation",
    applies = function(result) {
      inherits(result, "simtab_cox") &&
        is.matrix(result$meta$cox_zph) &&
        any(rownames(result$meta$cox_zph) != "GLOBAL" & result$meta$cox_zph[, "p"] < 0.05, na.rm = TRUE)
    },
    check = function(result) {
      # zph: Schoenfeld residual hypothesis test matrix (terms x statistics)
      zph <- result$meta$cox_zph
      terms <- rownames(zph)[rownames(zph) != "GLOBAL" & zph[, "p"] < 0.05]
      list(
        message = sprintf(
          "The Cox proportional hazards check suggests non-proportional hazards for: %s.",
          paste(terms, collapse = ", ")
        ),
        why = "The Grambsch-Therneau Schoenfeld residual test detected time-varying effects, so a single hazard ratio may average changing hazards over follow-up."
      )
    },
    severity = 4,
    citation = "Grambsch & Therneau 1994",
    fix = "Consider stratifying on the offending term, adding a time-varying coefficient, or splitting follow-up time.",
    version = version,
    example_fires = function() {
      set.seed(2602)
      n <- 400
      trt <- rep(c("Control", "Treatment"), each = n / 2)
      u <- stats::runif(n)
      time <- numeric(n)
      h1 <- ifelse(trt == "Treatment", 0.35, 0.06)
      h2 <- ifelse(trt == "Treatment", 0.035, 0.18)
      switch_time <- 4
      early_cumhaz <- h1 * switch_time
      for (i in seq_len(n)) {
        y <- -log(u[i])
        time[i] <- if (y <= early_cumhaz[i]) y / h1[i] else switch_time + (y - early_cumhaz[i]) / h2[i]
      }
      censor_time <- 12
      dat <- data.frame(
        time = pmin(time, censor_time),
        event = as.integer(time <= censor_time),
        trt = factor(trt, levels = c("Control", "Treatment"))
      )
      survtab(dat, time = "time", event = "event", predictors = ~trt)
    },
    example_silent = function() {
      set.seed(2601)
      n <- 240
      trt <- rep(c("Control", "Treatment"), each = n / 2)
      age <- rep(seq(42, 78, length.out = n / 2), times = 2)
      rate <- 0.09 * ifelse(trt == "Treatment", 0.55, 1) * exp(0.018 * (age - 60))
      event_time <- stats::rexp(n, rate = rate)
      censor_time <- 20
      dat <- data.frame(
        time = pmin(event_time, censor_time),
        event = as.integer(event_time <= censor_time),
        trt = factor(trt, levels = c("Control", "Treatment")),
        age = age
      )
      survtab(dat, time = "time", event = "event", predictors = ~trt + age)
    }
  )

  # Rule: Low Events Per Variable (EPV < 10) in logistic regression
  # overfitting and risk of biased coefficients due to insufficient events per predictor.

  register_rule(
    "low_events_per_parameter",
    applies = .regtab_is_logistic,
    check = function(result) {
      epv <- .regtab_epv(result)
      epv <- epv[!is.na(epv)]
      if (length(epv) == 0 || min(epv) >= 10) {
        return(NULL)
      }
      min_epv <- min(epv)
      list(
        severity = if (min_epv < 5) 4L else 2L,
        message = sprintf("Logistic model has low events per parameter (minimum EPV %.1f).", min_epv),
        why = "Sparse outcome information per coefficient can produce unstable estimates and optimistic intervals."
      )
    },
    severity = 2,
    citation = "van Smeden 2016; van Smeden 2019; Riley 2019",
    fix = "Consider fewer predictors, penalisation, or a sample-size calculation.",
    version = version,
    example_fires = function() {
      dat <- data.frame(y = c(rep(0, 96), rep(1, 4)), x = rep(c(0, 1), each = 50), z = rep(c(0, 1), 50))
      regtab(dat, outcomes = "y", predictors = ~ x + z, family = stats::binomial("logit"))
    },
    example_silent = function() {
      dat <- data.frame(y = rep(c(0, 1), each = 80), x = rep(c(0, 1), 80))
      regtab(dat, outcomes = "y", predictors = ~ x, family = stats::binomial("logit"))
    }
  )

  # Rule: Unhandled overdispersion in standard Poisson count regression
  # Pearson dispersion > 1.5 and robust SEs were not requested.
  register_rule(
    "overdispersion_poisson",
    applies = function(result) inherits(result, "simtab_regtab") && nrow(.regtab_overdispersed_rows(result)) > 0,
    check = function(result) {
      rows <- .regtab_overdispersed_rows(result)
      lapply(seq_len(nrow(rows)), function(i) {
        list(
          message = sprintf(
            "Outcome '%s' Poisson model shows overdispersion (Pearson dispersion = %.2f); consider a negative-binomial model or robust standard errors.",
            rows$outcome[i],
            rows$dispersion[i]
          ),
          why = "Poisson models assume mean equals variance; overdispersed count data understate standard errors and overstate significance when left uncorrected."
        )
      })
    },
    severity = 3,
    citation = "Ver Hoef & Boveng 2007",
    fix = "Refit with a negative-binomial family (e.g. MASS::glm.nb) or set robust = TRUE for robust standard errors.",
    version = version,
    example_fires = function() {
      set.seed(99)
      n <- 200
      x <- rep(c(0, 1), each = n / 2)
      mu <- exp(0.3 + 0.5 * x)
      lambda <- mu * stats::rgamma(n, shape = 2, rate = 2)
      y <- stats::rpois(n, lambda)
      regtab(data.frame(y = y, x = x), outcomes = "y", predictors = ~x, family = stats::poisson("log"), robust = FALSE)
    },
    example_silent = function() {
      set.seed(99)
      n <- 200
      x <- rep(c(0, 1), each = n / 2)
      mu <- exp(0.3 + 0.5 * x)
      lambda <- mu * stats::rgamma(n, shape = 2, rate = 2)
      y <- stats::rpois(n, lambda)
      regtab(data.frame(y = y, x = x), outcomes = "y", predictors = ~x, family = stats::poisson("log"), robust = TRUE)
    }
  )

  # Rule: HC0 sandwich covariance estimator used with small sample size (N < 100)
  # Recommends HC3 adjustment because asymptotic HC0 tends to underestimate SEs in small samples.
  register_rule(
    "hc0_small_n",
    applies = function(result) inherits(result, "simtab_regtab") && nrow(.regtab_hc0_small_n_rows(result)) > 0,
    check = function(result) {
      rows <- .regtab_hc0_small_n_rows(result)
      lapply(seq_len(nrow(rows)), function(i) {
        list(
          message = sprintf(
            "Outcome '%s' uses HC0 robust standard errors with model N = %d (< 100); HC3 is recommended at small sample sizes.",
            rows$outcome[i],
            rows$n[i]
          ),
          why = "HC0 can understate standard errors in small samples; HC3 is a more conservative, small-sample-appropriate heteroscedasticity-consistent estimator."
        )
      })
    },
    severity = 2,
    citation = "Long & Ervin 2000",
    fix = "Set robust = 'HC3' for a small-sample-appropriate robust covariance.",
    version = version,
    example_fires = function() {
      set.seed(5)
      n <- 60
      dat <- data.frame(y = stats::rnorm(n), x = stats::rnorm(n))
      regtab(dat, outcomes = "y", predictors = ~x, family = stats::gaussian(), robust = TRUE)
    },
    example_silent = function() {
      set.seed(5)
      n <- 60
      dat <- data.frame(y = stats::rnorm(n), x = stats::rnorm(n))
      regtab(dat, outcomes = "y", predictors = ~x, family = stats::gaussian(), robust = "HC3")
    }
  )

  # Rule: Informational note for log-binomial fallback to modified Poisson
  # Explains that modified Poisson (with robust sandwich SE) was used because log-binomial failed convergence.
  register_rule(
    "logbinomial_fallback",
    applies = function(result) {
      .is_table_result(result) &&
        .result_uses_measure(result, c("PR", "RR")) &&
        length(.logbinomial_fallback_vars(result)) > 0
    },
    check = function(result) {
      vars <- .logbinomial_fallback_vars(result)
      if (length(vars) == 0) {
        return(NULL)
      }
      labels <- result$meta$labels[vars] %||% vars
      labels <- unname(ifelse(is.na(labels) | !nzchar(labels), vars, labels))
      measure <- toupper(result$meta$effect %||% result$spec$effect$measure %||% "PR/RR")
      list(
        message = sprintf(
          "Adjusted %s for %s used modified Poisson regression with robust standard errors because %s.",
          measure,
          paste(labels, collapse = ", "),
          .logbinomial_reason_phrase(result, vars)
        ),
        why = "This is the planned convergence-safe substitution for adjusted PR/RR estimates."
      )
    },
    severity = 1,
    citation = "Zou 2004; Barros & Hirakata 2003",
    fix = "No user action needed; the fallback is the convergence-safe estimator.",
    version = version,
    example_fires = function() {
      table1(.logbinomial_rule_example_data("hard"), "exposure", by = "y", measure = "PR", adjust = "x")
    },
    example_silent = function() {
      table1(.logbinomial_rule_example_data("clean"), "exposure", by = "y", measure = "PR", adjust = "x")
    }
  )


  #########
  # C: Diagnostics & ROC Analysis
  # Evaluates diagnostic test accuracy metrics, cutpoint optimism, and validation.

  # Rule: Optimism bias from data-driven ROC optimal cutpoint selection
  register_rule(
    "roc_cutpoint_optimism",
    applies = function(result) inherits(result, "simtab_roc") && isTRUE(result$meta$rule_flags$cutpoint_present),
    check = function(result) {
      list(
        message = "A data-driven ROC cutpoint is reported from the same data used to estimate performance; its sensitivity and specificity are optimistic.",
        why = "Choosing the threshold that maximises apparent performance reuses the outcome labels and overstates test accuracy."
      )
    },
    severity = 3,
    citation = "Ewald 2006",
    fix = "Validate the threshold in external data or with resampling before treating it as a decision rule.",
    version = version,
    example_fires = function() {
      dat <- .roc_example_data()
      roc(dat, marker = marker1, outcome = outcome, positive = "Yes")
    },
    example_silent = function() {
      dat <- .roc_example_data()
      roc(dat, marker = marker1, outcome = outcome, positive = "Yes", cutpoint = "none")
    }
  )

  # Rule: Small class sample size in ROC analysis (min class N < 20)
  # AUC and confidence intervals are unstable when positive or negative cases are few.
  register_rule(
    "roc_small_sample",
    applies = function(result) inherits(result, "simtab_roc"),
    check = function(result) {
      min_n <- .roc_min_class_n(result)
      if (is.na(min_n) || min_n >= 20) {
        return(NULL)
      }
      list(
        message = sprintf("ROC analysis has only %.0f observations in the smaller outcome class; AUC and CI may be unstable.", min_n),
        why = "AUC precision depends on both diseased and non-diseased sample sizes."
      )
    },
    severity = 2,
    citation = "Hanley & McNeil 1982",
    fix = "Avoid over-interpreting the point AUC; prioritise a larger or externally validated sample.",
    version = version,
    example_fires = function() {
      dat <- .roc_example_data(n_pos = 12, n_neg = 18)
      roc(dat, marker = marker1, outcome = outcome, positive = "Yes", cutpoint = "none")
    },
    example_silent = function() {
      dat <- .roc_example_data(n_pos = 40, n_neg = 40)
      roc(dat, marker = marker1, outcome = outcome, positive = "Yes", cutpoint = "none")
    }
  )

  # Rule: Apparent AUC without internal or external validation
  #TRIPOD guidelines
  register_rule(

    "roc_auc_validation",
    applies = function(result) inherits(result, "simtab_roc") && isTRUE(result$meta$rule_flags$apparent_auc %||% TRUE),
    check = function(result) {
      list(
        message = "AUC is apparent performance from the analysed sample and can be optimistic without validation.",
        why = "Model and marker performance estimated in development data often exceeds performance in new patients."
      )
    },
    severity = 2,
    citation = "TRIPOD 2015",
    fix = "Report internal validation such as bootstrap optimism correction, or validate externally.",
    version = version,
    example_fires = function() {
      dat <- .roc_example_data()
      roc(dat, marker = marker1, outcome = outcome, positive = "Yes", cutpoint = "none")
    },
    example_silent = function() {
      dat <- .roc_example_data()
      result <- roc(dat, marker = marker1, outcome = outcome, positive = "Yes", cutpoint = "none")
      result$meta$rule_flags$apparent_auc <- FALSE
      result$advice <- advise(result)
      result
    }
  )

  # Rule: pROC auto-selected ROC direction
  # Discloses when pROC automatically oriented the marker (< or >) so AUC >= 0.5.
  register_rule(
    "roc_auto_direction",
    applies = function(result) inherits(result, "simtab_roc") && isTRUE(result$meta$rule_flags$auto_direction),
    check = function(result) {
      auc <- result$data$auc
      if (!is.data.frame(auc) || !"direction_used" %in% names(auc)) {
        return(NULL)
      }
      list(
        message = sprintf(
          "pROC auto-selected ROC direction by marker: %s.",
          paste(sprintf("%s=%s", auc$marker, auc$direction_used), collapse = ", ")
        ),
        why = "The auto direction orients the ROC curve so the AUC is at least 0.5; recording it keeps the methods decision inspectable."
      )
    },
    severity = 1,
    citation = "pROC direction = 'auto'",
    fix = "Set direction = '<' or direction = '>' when marker orientation is pre-specified.",
    version = version,
    example_fires = function() {
      dat <- .roc_example_data()
      roc(dat, marker = marker1, outcome = outcome, positive = "Yes")
    },
    example_silent = function() {
      dat <- .roc_example_data()
      roc(dat, marker = marker1, outcome = outcome, positive = "Yes", direction = "<")
    }
  )

#########
  # D: Epidemiological Review & Study Design
  #causal interpretation issues, and reporting gaps

  # Rule: Table 2 Fallacy (misinterpreting adjustment covariates as total causal effects)
  # interpreting confounder coefficients causally in multivariable regression tables.
  register_rule(
    "table2_fallacy",
    applies = function(result) inherits(result, "simtab_regtab") && length(result$meta$term_order) > 1,
    check = function(result) {
      list(
        message = "Adjusted coefficients for covariates are conditional associations, not automatically total causal effects.",
        why = "Table 2 covariate rows can invite causal interpretation even when covariates were included only for adjustment."
      )
    },
    severity = 1,
    citation = "Westreich & Greenland 2013",
    fix = "Footnote covariate rows or present the pre-specified exposure estimate separately.",
    version = version,
    example_fires = function() {
      dat <- data.frame(y = stats::rpois(80, 2), exposure = rep(c(0, 1), 40), age = rep(30:69, each = 2))
      regtab(dat, outcomes = "y", predictors = ~ exposure + age, family = stats::poisson("log"))
    },
    example_silent = function() {
      dat <- data.frame(y = stats::rpois(80, 2), exposure = rep(c(0, 1), 40))
      regtab(dat, outcomes = "y", predictors = ~ exposure, family = stats::poisson("log"))
    }
  )

  # Rule: Repeated subject identifiers treated as independent observations
  # Warns that unpaired tests violate the assumption of independent observations when cluster/subject IDs repeat.
  register_rule(
    "repeated_ids_independence",
    applies = function(result) {
      .is_table_result(result) &&
        length(.repeated_id_columns(result$used$ref$data)) > 0 &&
        .result_has_unpaired_test_or_effect(result)
    },
    check = function(result) {
      id_cols <- .repeated_id_columns(result$used$ref$data)
      list(
        message = sprintf(
          "Column(s) %s look like repeated subject identifiers, but an unpaired test/effect was computed; independence between rows may be violated.",
          paste(sprintf("'%s'", id_cols), collapse = ", ")
        ),
        why = "Unpaired tests and crude effect measures assume one independent observation per row; repeated rows per subject correlate observations and can bias standard errors."
      )
    },
    severity = 3,
    citation = "Hanley, Negassa, Edwardes & Forrester 2003",
    fix = "Consider a paired test (test(paired = TRUE)) or a clustered/GEE analysis given repeated observations per subject.",
    version = version,
    example_fires = function() {
      dat <- data.frame(
        patient_id = c(1:15, 1:5),
        exposure = factor(rep(c("A", "B"), 10)),
        outcome = factor(rep(c("No", "Yes"), each = 10))
      )
      table1(dat, "exposure", by = "outcome", test = TRUE)
    },
    example_silent = function() {
      dat <- data.frame(
        patient_id = 1:20,
        exposure = factor(rep(c("A", "B"), 10)),
        outcome = factor(rep(c("No", "Yes"), each = 10))
      )
      table1(dat, "exposure", by = "outcome", test = TRUE)
    }
  )

  # Rule: Unreported missing data exceeding 5%
  # STROBE checklist item 14
  register_rule(
    "complete_case_unreported_missingness",
    applies = .is_table_result,
    check = function(result) {
      frac <- .missing_fraction_unreported(result)
      if (is.na(frac) || frac <= 0.05) {
        return(NULL)
      }
      list(
        # Severity increases to 3 if missingness exceeds 10%
        severity = if (frac > 0.10) 3L else 2L,
        message = sprintf("Missingness exceeds 5%% (maximum %.1f%%) but missing rows are not shown.", frac * 100),
        why = "Readers need to see how much information was unavailable for each descriptive variable."
      )
    },
    severity = 2,
    citation = "STROBE item 14",
    fix = "Add the miss flag (tb) or missing = TRUE (table1), or report missingness separately.",
    version = version,
    checklist = c(strobe = "14"),
    example_fires = function() {
      dat <- data.frame(
        sex = factor(c(rep("F", 40), rep("M", 50), rep(NA, 10))),
        disease = factor(rep(c("No", "Yes"), 50))
      )
      table1(dat, "sex", by = "disease", missing = FALSE)
    },
    example_silent = function() {
      dat <- data.frame(
        sex = factor(c(rep("F", 40), rep("M", 50), rep(NA, 10))),
        disease = factor(rep(c("No", "Yes"), 50))
      )
      table1(dat, "sex", by = "disease", missing = TRUE)
    }
  )

  invisible(NULL)
}

# Merged rule helpers


#' Combine the entries of a merged rule into one advice entry
#' Concatenates text messages and preserves the maximum severity rung found across entries.
#' @keywords internal
#' @noRd
.combine_advice_entries <- function(entries) {
  entries <- Filter(Negate(is.null), entries)
  if (length(entries) == 0L) {
    return(NULL)
  }
  if (length(entries) == 1L) {
    return(entries[[1L]])
  }
  pick <- function(field) {
    vals <- unlist(lapply(entries, `[[`, field), use.names = FALSE)
    if (length(vals) == 0L) NULL else paste(unique(vals), collapse = " ")
  }
  severities <- unlist(lapply(entries, `[[`, "severity"), use.names = FALSE)
  out <- list(message = pick("message"), why = pick("why"))
  fixes <- unlist(lapply(entries, `[[`, "fix"), use.names = FALSE)
  if (length(fixes) > 0L) {
    out$fix <- fixes[[length(fixes)]]
  }
  if (length(severities) > 0L) {
    out$severity <- max(severities)
  }
  out
}

#' Advice generator when expected counts are strictly below 1
#' @keywords internal
#' @noRd
.advice_fisher_low_expected <- function(result) {
  notes <- .result_test_notes(result, "fisher_low_expected")
  if (length(notes) == 0L) {
    return(NULL)
  }
  note <- notes[[1]]
  list(
    message = sprintf(
      "%s had expected cell < 1 (min %.1f); Fisher's exact used (Campbell, 2007).",
      note$variable %||% "A 2x2 comparison",
      note$min_expected
    ),
    why = "Campbell's small-sample boundary favours Fisher-Irwin only when a 2x2 expected count is below 1.",
    fix = "Use test = \"chisq\" only when forcing N-1 chi-squared is scientifically justified.",
    severity = 1L
  )
}

#' Advice generator when expected counts are between 1 and 5
#' @keywords internal
#' @noRd
.advice_small_expected_cells <- function(result) {
  notes <- .result_test_notes(result, "small_expected_cells")
  if (length(notes) == 0L) {
    return(NULL)
  }
  note <- notes[[1]]
  list(
    message = sprintf(
      "%s had %d expected cell(s) < 5 (min %.1f) but all expected cells were >= 1; N-1 chi-squared was used (Campbell, 2007).",
      note$variable %||% "A 2x2 comparison",
      note$n_small,
      note$min_expected
    ),
    why = "Campbell's recommendation keeps N-1 chi-squared in this boundary band while making the sparse-table choice visible.",
    fix = "Use test = \"fisher\" if an exact Fisher-Irwin test is preferred for this sparse table.",
    severity = 2L
  )
}

