#' Describe a cohort across many variables (Table 1)
#'
#' `table1()` builds the descriptive "Table 1" of a study in one call: numeric
#' variables as mean (SD) or median (IQR), categorical variables as counts and
#' percentages. Add `by` to get one column per group, optionally with
#' p-values and crude or adjusted effect measures (PR, RR, or OR). For a single
#' cross-tabulation use [tb()]; for full regression output use [regtab()].
#'
#' @param data A data frame.
#' @param vars Variables to describe: bare names in `c()`, a character vector,
#'   or a tidyselect expression such as `starts_with("cm_")`.
#' @param by Optional grouping variable: a bare name or string. Produces one
#'   column per group.
#' @param ... Terse flags such as `p` or `or`, written after `by`; see
#'   *Terse flags*. Only flags are allowed here.
#' @param flags Character vector of terse flags, e.g. `c("p", "or")`. The
#'   programmatic form of the bare flags in `...`.
#' @param measure String. Effect measure to add: `"PR"`, `"RR"`, or `"OR"`.
#'   Requires a binary `by`, whose last level is taken as the event.
#' @param adjust Optional covariates for an adjusted effect-measure column,
#'   given like `vars`. Requires `measure`.
#' @param test Logical or string. `TRUE` adds a p-value column with an
#'   automatically chosen test. A string forces one test: `"chisq"`,
#'   `"fisher"`, `"mcnemar"`, or `"trend"` for categorical variables, and
#'   `"t"`, `"wilcoxon"`, `"anova"`, or `"kruskal"` for numeric ones.
#' @param summary String. Summary for numeric variables: `"auto"` chooses
#'   between `"mean"` (mean and SD) and `"median"` (median and IQR) based on
#'   sample size and skewness. A named list sets it per variable, e.g.
#'   `list(age = "mean", bmi = "median")`.
#' @param overall Logical. If `TRUE`, add an "Overall" column for the whole
#'   cohort.
#' @param missing Logical. If `TRUE`, add a "Missing" count row under each
#'   variable that has missing values. This does not change any other number.
#' @param denominator String. Which rows the percentages and summaries use:
#'   `"available"` uses every non-missing value of each variable;
#'   `"complete"` uses only rows complete on all of `vars` and `by`.
#' @param na_model String. How adjusted models handle missing values:
#'   `"drop"` removes incomplete rows, `"fail"` stops with an error, and
#'   `"explicit"` treats missing categorical predictors as a `"(Missing)"`
#'   level.
#' @param ref Reference level for effect measures: one level for all
#'   variables, or a named list per variable. If `NULL`, each variable's
#'   first level is used.
#' @param var.type Force variable types: `"continuous"` or `"categorical"`,
#'   either one string for all variables or a named vector such as
#'   `c(score = "continuous")`. If `NULL`, types are detected automatically
#'   (see *Statistical methods*).
#' @param labels Named character vector of display labels, e.g.
#'   `c(smoking = "Smoking status")`. Variable labels already stored in `data`
#'   are used by default.
#' @param style Display style: a journal preset name (see [list_journals()]),
#'   a [journal_style()] object, `"n_pct"` or `"pct_n"`, a template using
#'   `{n}` and `{p}`, or a named list per variable.
#' @param d Integer. Decimal places for percentages and continuous summaries.
#'   If `NULL` (the default), percentages use one decimal and continuous
#'   summaries use the style's `digits_cont` (one decimal for the built-in
#'   journals).
#' @param conf.level Number between 0 and 1. Confidence level for effect
#'   measure intervals.
#' @param design String. Study design used to choose the effect measure when
#'   `measure` is not set: `"cross_sectional"` gives PR, `"cohort"` gives RR,
#'   and `"case_control"` gives OR.
#'
#' @eval .flag_roxygen_section("table1")
#'
#' @details
#' ## Statistical methods
#' Crude effect measures for categorical variables use the same estimators as
#' [tb()]: the Katz log interval for prevalence and risk ratios (Katz et al.,
#' 1978) and the Woolf logit interval for odds ratios (Woolf, 1955). For
#' numeric variables, and for every adjusted estimate, a separate generalized
#' linear model is fitted for each described variable, with the `adjust`
#' covariates added. Odds ratios come from logistic regression. Prevalence and
#' risk ratios come from log-binomial regression, falling back to modified
#' Poisson regression with robust standard errors when that model does not
#' converge (Barros & Hirakata, 2003; Zou, 2004). PR and RR are computed identically; only the label
#' differs.
#'
#' With `test = TRUE`, 2x2 tables use the N-1 chi-squared test (Campbell,
#' 2007) and larger tables the Pearson chi-squared test, without continuity
#' correction; Fisher's exact test is chosen only when an expected count is
#' below 1. Numeric variables are compared with Welch's t-test or ANOVA when
#' summarised by the mean, and with the Wilcoxon or Kruskal-Wallis test when
#' summarised by the median.
#'
#' Numeric variables are treated as continuous unless they look like coded
#' categories: exactly two distinct whole numbers, or at most seven distinct
#' whole numbers with at least 20 observations. Use `var.type` to override.
#'
#' ## Missing data
#' By default each variable is described using its own non-missing values,
#' and a "Missing" row reports how many are absent (STROBE item 14). Tests are
#' always computed on observed values. `denominator = "complete"` restricts
#' the whole table to complete cases, so the displayed numbers can change.
#' `na_model` controls only the adjusted models.
#'
#' ## Modifying the result
#' Multiplicity adjustment, paired tests, and standardized mean differences
#' (Austin, 2009) are set afterwards with [test()], e.g.
#' `table1(df, vars, by = group, test = TRUE) |> test(p.adjust = "holm", smd = TRUE)`.
#' SMDs are reported as absolute values and need a two-level `by`; numeric
#' variables use the difference in means over the square root of the
#' unweighted average of the two group variances, and categorical variables
#' the multinomial generalisation of Yang & Dalton (2012).
#' With `paired = TRUE`, two equal-sized groups are treated as matched pairs in
#' row order. Use [add_effect()] to add or replace the effect-measure columns
#' and [fmt()] to change decimals.
#'
#' @return A `simtab_result` of class `simtab_table1`. Print it to see the
#'   formatted table, convert it with `as.data.frame()`, or save it with
#'   [export_docx()], [export_pptx()], or [export_xlsx()]. Unrounded results
#'   are stored in `$data`.
#' @seealso [tb()] for a single cross-tabulation, [add_effect()] and [test()]
#'   to modify the table, [journal_style()] for formatting, and
#'   [simtablr_references] for all references cited by SimtablR.
#' @examples
#' # Describe the whole cohort
#' table1(epitabl, c(age, sex, smoking, bmi))
#'
#' # Compare groups, with p-values
#' table1(epitabl, c(age, sex, smoking), by = adjudicated_acs, flags = "p")
#'
#' # Crude and age- and sex-adjusted prevalence ratios
#' table1(
#'   epitabl, c(smoking, diabetes, hypertension),
#'   by = adjudicated_acs, measure = "PR", adjust = c(age, sex)
#' )
#'
#' # Add standardized mean differences after the fact
#' table1(epitabl, c(age, sex), by = adjudicated_acs, test = TRUE) |>
#'   test(smd = TRUE)
#' @references Austin, P. C. (2009). Balance diagnostics for comparing the
#'   distribution of baseline covariates between treatment groups in
#'   propensity-score matched samples. \emph{Statistics in Medicine},
#'   28(25), 3083--3107. \doi{10.1002/sim.3697}.
#'
#'   Barros, A. J. D., & Hirakata, V. N. (2003). Alternatives for logistic
#'   regression in cross-sectional studies: an empirical comparison of models
#'   that directly estimate the prevalence ratio. \emph{BMC Medical Research
#'   Methodology}, 3, 21. \doi{10.1186/1471-2288-3-21}.
#'
#'   Campbell, I. (2007). Chi-squared and Fisher-Irwin tests of two-by-two
#'   tables with small sample recommendations. \emph{Statistics in Medicine},
#'   26(19), 3661--3675. \doi{10.1002/sim.2832}.
#'
#'   Katz, D., Baptista, J., Azen, S. P., & Pike, M. C. (1978). Obtaining
#'   confidence intervals for the risk ratio in cohort studies.
#'   \emph{Biometrics}, 34(3), 469--474. \doi{10.2307/2530610}.
#'
#'   Woolf, B. (1955). On estimating the relation between blood group and
#'   disease. \emph{Annals of Human Genetics}, 19(4), 251--253.
#'   \doi{10.1111/j.1469-1809.1955.tb01348.x}.
#'
#'   Zou, G. (2004). A modified Poisson regression approach to prospective
#'   studies with binary data. \emph{American Journal of Epidemiology},
#'   159(7), 702--706. \doi{10.1093/aje/kwh090}.
#' @export
table1 <- function(
  data,
  vars,
  by = NULL,
  ...,
  flags = character(),
  measure = NULL,
  adjust = NULL,
  test = FALSE,
  summary = "auto",
  overall = TRUE,
  missing = TRUE,
  denominator = c("available", "complete"),
  na_model = c("drop", "fail", "explicit"),
  ref = NULL,
  var.type = NULL,
  labels = NULL,
  style = "default",
  d = NULL,
  conf.level = 0.95,
  design = NULL
) {
  .check_data_frame(data)
  vars_quo <- rlang::enquo(vars)
  if (rlang::quo_is_missing(vars_quo)) {
    simtab_abort_input(c(
      "{.arg vars} is required.",
      "i" = "{.fn table1} needs to know which columns to describe.",
      "v" = "Call {.code table1(data, c(age, sex), by = group)}."
    ))
  }
  by_quo <- rlang::enquo(by)
  if (.is_guidance_call(rlang::quo_get_expr(by_quo))) {
    .abort_misplaced_guidance(rlang::quo_get_expr(by_quo), "table1")
  }
  dot_exprs <- as.list(substitute(list(...)))[-1]
  bare_flag_tokens <- .table1_scan_flags(dot_exprs)
  flag_tokens <- .table1_validate_flags(c(flags, bare_flag_tokens))
  adjust_quo <- rlang::enquo(adjust)
  test_explicit <- !missing(test)
  missing_explicit <- !missing(missing)
  if (!is.null(d)) {
    .check_d(d)
  }
  .check_conf_level(conf.level)
  denominator <- match.arg(denominator)
  na_model <- match.arg(na_model)

  spec <- rlang::inject(describe(simtab(data), !!vars_quo))
  if (!.table1_quo_is_unset(by_quo)) {
    spec <- rlang::inject(stratify(spec, !!by_quo))
  }
  if (!.table1_quo_is_unset(adjust_quo)) {
    spec <- .bind_role(spec, "adjust", rlang::new_quosures(list(adjust_quo)))
  }

  spec <- .apply_flags(spec, flag_tokens)
  if (!is.null(var.type)) {
    spec <- .set_engine_opts(spec, "descriptive", list(var.type = var.type))
  }
  spec <- .table1_apply_summary(spec, summary)
  if (isTRUE(test_explicit)) {
    spec <- test(spec, method = test)
  } else if (!is.null(spec$comparison$test)) {
    spec <- test(spec, method = spec$comparison$test)
  }
  spec <- overall(spec, isTRUE(overall))
  spec <- missingness(
    spec,
    display = if (isTRUE(missing_explicit)) missing else NULL,
    denominator = denominator,
    model = na_model
  )
  spec <- style(spec, style)
  if (!is.null(measure)) {
    spec <- measure(spec, measure, ref = ref, conf.level = conf.level)
  }
  if (!is.null(design)) {
    spec <- set_design(spec, design)
  }
  spec <- fmt(spec, d = d, conf_pct = round(conf.level * 100), labels = labels)
  spec["call"] <- list(match.call())

  evaluate(spec)
}

#' Scans bare expressions in dots for terse flag tokens
#' @keywords internal
#' @noRd
.table1_scan_flags <- function(dot_exprs) {
  if (length(dot_exprs) == 0) {
    return(character())
  }

  tokens <- character()
  bad <- character()
  dot_names <- names(dot_exprs) %||% rep("", length(dot_exprs))

  for (i in seq_along(dot_exprs)) {
    nm <- dot_names[[i]]
    expr <- dot_exprs[[i]]
    if (nzchar(nm)) {
      bad <- c(bad, nm)
      next
    }
    if (.is_guidance_call(expr)) {
      .abort_misplaced_guidance(expr, "table1")
    }
    if (is.symbol(expr)) {
      token <- as.character(expr)
      if (token %in% .reserved_flag_tokens()) {
        tokens <- c(tokens, token)
      } else {
        bad <- c(bad, token)
      }
      next
    }
    bad <- c(bad, paste(deparse(expr), collapse = " "))
  }

  if (length(bad) > 0) {
    simtab_abort_flag(c(
      sprintf("Unknown {.fn table1} flag(s) in {.code ...}: %s.", paste(bad, collapse = ", ")),
      "i" = "Only unnamed bare flag tokens are accepted in {.fn table1} dots.",
      "v" = sprintf(
        "Use one of: %s, or supply a character vector through {.arg flags}.",
        paste(setdiff(.canonical_flag_tokens(), c("row", "cell")), collapse = ", ")
      )
    ))
  }

  tokens
}

#' Validates and normalises table1 flag tokens
#' @keywords internal
#' @noRd
.table1_validate_flags <- function(tokens) {
  .validate_flag_vector(tokens)
  canonical <- .canonicalise_flag_tokens(tokens)
  unknown <- setdiff(canonical, .canonical_flag_tokens())
  if (length(unknown) > 0) {
    simtab_abort_flag(c(
      sprintf("Unknown {.fn table1} flag(s): %s.", paste(unique(unknown), collapse = ", ")),
      "v" = sprintf(
        "Use one of: %s.",
        paste(setdiff(.canonical_flag_tokens(), c("row", "cell")), collapse = ", ")
      )
    ))
  }

  deferred <- intersect(canonical, c("row", "cell"))
  if (length(deferred) > 0) {
    simtab_abort_flag(c(
      "{.fn table1} does not support {.val row} or {.val cell} percentage flags yet.",
      "i" = "Those reserved controls are currently deferred.",
      "v" = "Use {.val col} for the current table1 percentage behavior."
    ))
  }

  tokens
}

#' Sets up continuous summary options on specification
#' @keywords internal
#' @noRd
.table1_apply_summary <- function(spec, summary) {
  if (is.list(summary) && !is.null(names(summary))) {
    spec <- set_summary(spec, "auto")
    for (v in names(summary)) {
      spec <- set_summary(spec, summary[[v]], .by_var = v)
    }
    return(spec)
  }

  if (!is.character(summary) || length(summary) != 1 || !tolower(summary) %in% c("auto", "median", "mean")) {
    simtab_abort_input(c(
      "{.arg summary} must be {.val auto}, {.val median}, {.val mean}, or a named
       list of per-variable summaries.",
      "i" = "Received: {.val {summary}}.",
      "v" = "Use {.code summary = \"auto\"} to let the skewness heuristic decide."
    ))
  }
  set_summary(spec, tolower(summary))
}

#' Detect an unset optional binding argument (`by =`/`adjust =`)
#'
#' A literal `NULL` (the default) is the common case, caught by
#' `rlang::quo_is_null()` without evaluating anything. But a caller may also
#' forward a variable that merely *holds* `NULL` (e.g. a wrapper function that
#' passes through its own `by = NULL` default by symbol rather than by value),
#' which `quo_is_null()` cannot see since it only inspects the captured
#' expression. Evaluating the quosure in its own (non-data-masked) environment
#' distinguishes this case; a bare column-name symbol with no matching object
#' in scope simply fails to evaluate and is treated as set (a genuine column
#' reference to resolve later through the funnel), matching `.as_select_quo()`'s
#' own safe-eval convention.
#'
#' @param quo A quosure.
#' @return `TRUE` if the argument should be treated as unset.
#' @keywords internal
#' @noRd
.table1_quo_is_unset <- function(quo) {
  if (rlang::quo_is_null(quo)) {
    return(TRUE)
  }
  val <- tryCatch(rlang::eval_tidy(quo), error = function(e) quo)
  is.null(val)
}

#' Builds quosures expression from character variable vector
#' @keywords internal
#' @noRd
.table1_vars_quosures <- function(vars) {
  expr <- if (length(vars) == 1) {
    as.name(vars[[1]])
  } else {
    as.call(c(list(as.name("c")), lapply(vars, as.name)))
  }
  rlang::new_quosures(list(rlang::new_quosure(expr, baseenv())))
}

#' Computes mean/SD or median/IQR summary statistics for continuous vectors
#' @keywords internal
#' @noRd
.summ_cont <- function(y, stat) {
  if (length(y) == 0) {
    return(if (stat == "mean") c(NA_real_, NA_real_) else c(NA_real_, NA_real_, NA_real_))
  }
  if (stat == "mean") {
    c(mean(y), .sample_sd_stable(y))
  } else {
    as.numeric(quantile(y, probs = c(0.5, 0.25, 0.75), type = 7))
  }
}

#' Partitions dataset into logical index subsets by stratum
#' @keywords internal
#' @noRd
.compute_groups <- function(data, strat, overall) {
  n <- nrow(data)
  if (is.null(strat)) {
    return(list(names = "Overall", idx = list(Overall = rep(TRUE, n)), n = c(Overall = n)))
  }
  sv <- data[[strat]]
  levs <- levels(droplevels(factor(sv)))
  names_out <- character(0)
  idx <- list()
  ncount <- integer(0)
  if (isTRUE(overall)) {
    names_out <- "Overall"
    idx[["Overall"]] <- rep(TRUE, n)
    ncount["Overall"] <- n
  }
  for (l in levs) {
    nm <- l
    idx[[nm]] <- !is.na(sv) & sv == l
    ncount[nm] <- sum(idx[[nm]])
    names_out <- c(names_out, nm)
  }
  list(names = names_out, idx = idx, n = ncount)
}

#' Between-stratum test for one variable.
#' @keywords internal
#' @noRd
.table1_test <- function(xv, sv, vtype, stat, test, paired = FALSE) {
  test_name <- if (is.character(test) && length(test) == 1L) tolower(test) else "auto"
  continuous_methods <- c("t", "wilcoxon", "anova", "kruskal")
  categorical_methods <- c("chisq", "fisher", "mcnemar", "trend")
  sf <- factor(sv)
  ok <- !is.na(sf)
  sf <- droplevels(sf[ok])
  xv <- xv[ok]
  if (nlevels(sf) < 2) {
    return(NULL)
  }
  if (vtype == "continuous") {
    if (test_name %in% categorical_methods) {
      simtab_abort_input(c(
        "Test method {.val {test_name}} is for categorical variables, not continuous ones.",
        "i" = "This variable was summarised as continuous.",
        "v" = "Use {.val t}, {.val wilcoxon}, {.val anova}, or {.val kruskal}; or set
               {.code var.type = \"categorical\"} if the variable really is categorical."
      ))
    }
    if (isTRUE(paired) && test_name %in% c("anova", "kruskal")) {
      simtab_abort_input(c(
        "Test method {.val {test_name}} is not a paired continuous test.",
        "i" = "{.code paired = TRUE} requires a test that compares matched observations.",
        "v" = "Use {.val t} or {.val wilcoxon}."
      ))
    }
    y <- as.numeric(xv)
    if (isTRUE(paired)) {
      pair <- .paired_group_values(y, sf)
      keep <- stats::complete.cases(pair[[1]], pair[[2]])
      if (sum(keep) < 2) {
        return(NULL)
      }
      res <- tryCatch({
        paired_method <- if (test_name %in% c("t", "wilcoxon")) {
          test_name
        } else if (stat == "mean") {
          "t"
        } else {
          "wilcoxon"
        }
        if (paired_method == "t") {
          tt <- stats::t.test(pair[[1]][keep], pair[[2]][keep], paired = TRUE)
          list(p.value = tt$p.value, method = "paired t-test")
        } else {
          w <- stats::wilcox.test(pair[[1]][keep], pair[[2]][keep], paired = TRUE, exact = FALSE)
          list(p.value = w$p.value, method = "paired Wilcoxon signed-rank")
        }
      }, error = function(e) NULL)
      return(res)
    }
    keep <- !is.na(y)
    y <- y[keep]
    x <- droplevels(sf[keep])
    if (nlevels(x) < 2 || length(y) < 2) {
      return(NULL)
    }
    if (test_name %in% c("t", "wilcoxon") && nlevels(x) != 2L) {
      simtab_abort_input(c(
        "Test method {.val {test_name}} requires exactly 2 groups, but {.arg by} has
         {nlevels(x)}.",
        "i" = "Groups found: {.val {levels(x)}}.",
        "v" = "Use {.val anova} or {.val kruskal} for more than two groups."
      ))
    }
    selected_method <- if (test_name %in% continuous_methods) {
      test_name
    } else if (stat == "mean" && nlevels(x) == 2L) {
      "t"
    } else if (stat == "mean") {
      "anova"
    } else if (nlevels(x) == 2L) {
      "wilcoxon"
    } else {
      "kruskal"
    }
    res <- tryCatch({
      if (selected_method == "t") {
        tt <- t.test(y ~ x)
        list(p.value = tt$p.value, method = "Welch t-test")
      } else if (selected_method == "anova") {
        a <- anova(lm(y ~ x))
        list(p.value = a$`Pr(>F)`[1], method = "One-way ANOVA")
      } else if (selected_method == "wilcoxon") {
        w <- wilcox.test(y ~ x, exact = FALSE)
        list(p.value = w$p.value, method = "Wilcoxon rank-sum")
      } else {
        k <- kruskal.test(y ~ x)
        list(p.value = k$p.value, method = "Kruskal-Wallis")
      }
    }, error = function(e) NULL)
    return(res)
  }

  if (test_name %in% continuous_methods) {
    simtab_abort_input(c(
      "Test method {.val {test_name}} is for continuous variables, not categorical ones.",
      "i" = "This variable was summarised as categorical.",
      "v" = "Use {.val chisq}, {.val fisher}, {.val mcnemar}, or {.val trend}; or set
             {.code var.type = \"continuous\"} if the variable really is continuous."
    ))
  }
  if (isTRUE(paired) && !test_name %in% c("auto", "mcnemar")) {
    simtab_abort_input(c(
      "Paired categorical comparisons require McNemar's test.",
      "i" = "{.code paired = TRUE} was set with {.code test = {.val {test_name}}}.",
      "v" = "Use {.code test = \"mcnemar\"}, or leave the test to automatic selection."
    ))
  }
  vf <- droplevels(factor(xv))
  if (nlevels(vf) < 2) {
    return(NULL)
  }
  if (!isTRUE(paired) &&
      (identical(test, "trend") || (isTRUE(test) && is.ordered(xv) && nlevels(vf) >= 3))) {
    return(.cochran_armitage_test(vf, sf))
  }
  if (isTRUE(paired)) {
    pair <- .paired_group_values(vf, sf)
    keep <- stats::complete.cases(pair[[1]], pair[[2]])
    if (sum(keep) < 2) {
      return(NULL)
    }
    tab_paired <- table(droplevels(pair[[1]][keep]), droplevels(pair[[2]][keep]))
    r <- tryCatch(stats::mcnemar.test(tab_paired), error = function(e) NULL)
    if (is.null(r)) return(NULL)
    return(list(p.value = r$p.value, method = r$method))
  }
  tab <- table(vf, sf)
  run_fisher <- function() tryCatch(
    fisher.test(tab, workspace = 2e7),
    error = function(e) {
      sim <- fisher.test(tab, simulate.p.value = TRUE, B = 1e5)
      sim$method <- paste0(sim$method, " (Monte Carlo)")
      sim
    }
  )
  decision <- if (isTRUE(test)) .campbell_expected_decision(tab)
  method <- if (isTRUE(test)) decision$method else test
  r <- tryCatch(
    switch(method,
      fisher = run_fisher(),
      mcnemar = mcnemar.test(tab),
      suppressWarnings(.chisq_campbell_or_pearson(tab))
    ),
    error = function(e) NULL
  )
  if (is.null(r)) {
    return(NULL)
  }
  out <- list(p.value = r$p.value, method = r$method)
  if (isTRUE(test)) {
    if (identical(method, "fisher")) {
      out$method <- "Fisher's exact"
    }
    out$simtab_note <- decision$note
  }
  out
}

#' Splits variable into equal-sized pairs for paired tests
#' @keywords internal
#' @noRd
.paired_group_values <- function(value, group) {
  gf <- droplevels(factor(group))
  ok <- !is.na(gf)
  gf <- droplevels(gf[ok])
  value <- value[ok]
  if (nlevels(gf) != 2) {
    simtab_abort_input(c(
      "{.code paired = TRUE} requires exactly 2 groups, but {nlevels(gf)} were found.",
      "i" = "Groups found: {.val {levels(gf)}}.",
      "v" = "Restrict the data to two groups, or use {.code paired = FALSE}."
    ))
  }
  parts <- split(value, gf)
  n <- lengths(parts)
  if (length(unique(n)) != 1) {
    simtab_abort_input(c(
      "{.code paired = TRUE} requires 2 equal-sized groups matched by row order.",
      "i" = "Group sizes: {.val {n}}.",
      "v" = "Ensure each row in one group has its matched partner in the other, in the
             same order; otherwise use {.code paired = FALSE}."
    ))
  }
  parts
}

#' Computes Cochran-Armitage trend test for ordered categorical data
#' @keywords internal
#' @noRd
.cochran_armitage_test <- function(x, group) {
  xf <- droplevels(factor(x))
  gf <- droplevels(factor(group))
  ok <- !is.na(xf) & !is.na(gf)
  xf <- droplevels(xf[ok])
  gf <- droplevels(gf[ok])
  if (nlevels(xf) < 3 || nlevels(gf) != 2) {
    return(NULL)
  }
  tab <- table(xf, gf)
  event <- colnames(tab)[ncol(tab)]
  counts <- as.numeric(tab[, event])
  totals <- as.numeric(rowSums(tab))
  res <- tryCatch(
    stats::prop.trend.test(counts, totals, score = seq_along(counts)),
    error = function(e) NULL
  )
  if (is.null(res)) {
    return(NULL)
  }
  list(
    p.value = res$p.value,
    method = "Cochran-Armitage trend test",
    statistic = unname(res$statistic),
    parameter = unname(res$parameter),
    scores = seq_along(counts)
  )
}

#' Dispatches standardized mean difference calculation by variable type
#' @keywords internal
#' @noRd
.table1_smd <- function(x, group, vtype) {
  gf <- droplevels(factor(group))
  ok_group <- !is.na(gf)
  gf <- droplevels(gf[ok_group])
  x <- x[ok_group]
  if (nlevels(gf) != 2) {
    return(NA_real_)
  }
  if (identical(vtype, "continuous")) {
    return(.table1_smd_continuous(as.numeric(x), gf))
  }
  .table1_smd_categorical(x, gf)
}

#' Computes the absolute standardized mean difference for continuous data
#'
#' Uses the unweighted average of the two group variances (Austin, 2009), the
#' same convention as the categorical Yang-Dalton SMD, so balance thresholds
#' read the same for every variable regardless of group sizes.
#' @keywords internal
#' @noRd
.table1_smd_continuous <- function(y, group) {
  gf <- droplevels(factor(group))
  ok <- !is.na(y) & !is.na(gf)
  y <- y[ok]
  gf <- droplevels(gf[ok])
  if (nlevels(gf) != 2) {
    return(NA_real_)
  }
  parts <- split(y, gf)
  n <- lengths(parts)
  if (any(n < 2)) {
    return(NA_real_)
  }
  vars <- vapply(parts, stats::var, numeric(1))
  pooled <- sqrt((vars[[1]] + vars[[2]]) / 2)
  if (!is.finite(pooled) || pooled <= 0) {
    return(NA_real_)
  }
  abs(mean(parts[[2]]) - mean(parts[[1]])) / pooled
}

#' Computes Yang-Dalton distance SMD for categorical distributions
#' @keywords internal
#' @noRd
.table1_smd_categorical <- function(x, group) {
  gf <- droplevels(factor(group))
  xf <- droplevels(factor(x))
  ok <- !is.na(xf) & !is.na(gf)
  xf <- droplevels(xf[ok])
  gf <- droplevels(gf[ok])
  if (nlevels(gf) != 2 || nlevels(xf) < 2) {
    return(NA_real_)
  }
  tab <- table(xf, gf)
  if (any(colSums(tab) == 0)) {
    return(NA_real_)
  }
  p <- prop.table(tab, margin = 2)
  keep <- seq_len(nrow(p) - 1)
  p1 <- as.numeric(p[keep, 1])
  p2 <- as.numeric(p[keep, 2])
  v1 <- .diag_probs(p1) - tcrossprod(p1)
  v2 <- .diag_probs(p2) - tcrossprod(p2)
  v <- (v1 + v2) / 2
  diff <- p2 - p1
  solved <- tryCatch(solve(v, diff), error = function(e) NULL)
  if (is.null(solved)) {
    return(NA_real_)
  }
  smd <- sqrt(as.numeric(t(diff) %*% solved))
  if (!is.finite(smd)) NA_real_ else smd
}

#' Constructs diagonal probability matrix for multinomial covariance
#' @keywords internal
#' @noRd
.diag_probs <- function(p) {
  if (length(p) == 1) {
    return(matrix(p, nrow = 1, ncol = 1))
  }
  diag(p, nrow = length(p), ncol = length(p))
}

#' Formats SMD values to configured decimal places
#' @keywords internal
#' @noRd
.fmt_smd <- function(x, spec) {
  if (length(x) != 1 || is.na(x)) {
    return("")
  }
  .fmt_fixed(x, spec$digits_est)
}

#########
# DISPLAY MATRIX ASSEMBLY
# Formats raw numeric summaries and percentages into a presentation matrix.

#' Build the display matrix for a table1 object (formatting layer).
#' @keywords internal
#' @noRd
.build_table1 <- function(x) {
  meta <- x$meta
  d <- meta$d
  gnames <- meta$group_names
  group_header <- function(g) {
    sprintf("%s (N=%d)", g, meta$group_n[[g]])
  }
  gcols <- vapply(gnames, group_header, character(1))

  has_p <- isTRUE(meta$has_test)
  has_p_adj <- has_p && !identical(meta$p.adjust %||% "none", "none")
  has_smd <- isTRUE(meta$smd) && length(meta$strat_levels %||% character(0)) == 2
  has_crude <- !is.null(meta$effect)
  has_adj <- !is.null(meta$adjust)
  ci_pct <- meta$conf_pct

  col_names <- gcols
  p_col <- p_adj_col <- smd_col <- crude_col <- adj_col <- NULL
  if (has_smd) {
    smd_col <- "SMD"
    col_names <- c(col_names, smd_col)
  }
  if (has_p) {
    p_col <- "P-value"
    col_names <- c(col_names, p_col)
  }
  if (has_p_adj) {
    p_adj_col <- "Adjusted P-value"
    col_names <- c(col_names, p_adj_col)
  }
  if (has_crude) {
    crude_col <- sprintf("%s (%d%% CI)", meta$effect, ci_pct)
    col_names <- c(col_names, crude_col)
  }
  if (has_adj) {
    adj_col <- sprintf("Adjusted %s (%d%% CI)", meta$adjust$measure, ci_pct)
    col_names <- c(col_names, adj_col)
  }

  labels <- character(0)
  row_type <- character(0)
  row_var <- character(0)
  row_level <- character(0)
  rows <- list()

  blank_row <- function() setNames(rep("", length(col_names)), col_names)

  effect_cell <- function(df, level, sp) {
    if (is.null(df)) return("")
    if (is.na(level)) {
      i <- which(is.na(df$level))
    } else {
      i <- which(!is.na(df$level) & df$level == level)
    }
    if (length(i) == 0) return("")
    i <- i[1]
    if (isTRUE(df$ref[i])) {
      return(sprintf(paste0("%.", sp$digits_est, "f (Ref)"), 1))
    }
    .fmt_est(df$estimate[i], df$lower[i], df$upper[i], sp)
  }

  # Append the per-variable "Missing" count row (shared by the continuous and
  # categorical branches below); appends to the builder state via <<-.
  missing_row <- function(rec, v, sp) {
    if (!.table1_show_missing(meta) || sum(rec$n_missing) == 0) {
      return(invisible(NULL))
    }
    rm <- blank_row()
    for (g in gnames) {
      rm[group_header(g)] <- as.character(rec$n_missing[[g]])
    }
    # Under na_model = "explicit" the adjusted model estimates missingness as
    # its own "(Missing)" level; crude estimates use observed values only.
    if (has_adj && identical(meta$missing$model_na, "explicit")) {
      rm[adj_col] <- effect_cell(rec$adjusted, "(Missing)", sp)
    }
    labels <<- c(labels, "  Missing")
    row_type <<- c(row_type, "missing")
    row_var <<- c(row_var, v)
    row_level <<- c(row_level, "Missing")
    rows[[length(rows) + 1]] <<- rm
    invisible(NULL)
  }

  for (v in meta$vars) {
    rec <- x$data[[v]]
    rec_label <- meta$labels[[v]] %||% rec$label
    sp <- .resolve_style(.resolve_per_var(meta$style, v, "default"))
    p_str <- if (has_p && !is.null(rec$test)) .fmt_p(rec$test$p.value, sp) else ""
    p_adj_str <- if (has_p_adj && !is.null(rec$test) && !is.null(rec$test$p.value.adjusted)) {
      .fmt_p(rec$test$p.value.adjusted, sp)
    } else {
      ""
    }
    smd_str <- if (has_smd) .fmt_smd(rec$smd, sp) else ""

    if (rec$type == "continuous") {
      r <- blank_row()
      sp_cont <- sp
      if (isTRUE(x$spec$fmt$d_explicit)) {
        sp_cont$digits_cont <- d
      }
      for (g in gnames) {
        r[group_header(g)] <- .fmt_cont(rec$summary[[g]], rec$stat, sp_cont)
      }
      if (has_smd) r[smd_col] <- smd_str
      if (has_p) r[p_col] <- p_str
      if (has_p_adj) r[p_adj_col] <- p_adj_str
      if (has_crude) r[crude_col] <- effect_cell(rec$crude, NA, sp)
      if (has_adj) r[adj_col] <- effect_cell(rec$adjusted, NA, sp)
      stat_lbl <- if (rec$stat == "mean") "Mean (SD)" else "Median (IQR)"
      labels <- c(labels, paste0(rec_label, " [", stat_lbl, "]"))
      row_type <- c(row_type, "header")
      row_var <- c(row_var, v)
      row_level <- c(row_level, NA_character_)
      rows[[length(rows) + 1]] <- r

      missing_row(rec, v, sp)
    } else {
      # header row
      hr <- blank_row()
      if (has_smd) hr[smd_col] <- smd_str
      if (has_p) hr[p_col] <- p_str
      if (has_p_adj) hr[p_adj_col] <- p_adj_str
      labels <- c(labels, paste0(rec_label, ", ", sp$count_header))
      row_type <- c(row_type, "header")
      row_var <- c(row_var, v)
      row_level <- c(row_level, NA_character_)
      rows[[length(rows) + 1]] <- hr

      for (l in rec$levels) {
        r <- blank_row()
        for (g in gnames) {
          gc <- group_header(g)
          r[gc] <- .fmt_np(rec$freq[l, g], rec$pct[l, g], sp, d)
        }
        if (has_crude) r[crude_col] <- effect_cell(rec$crude, l, sp)
        if (has_adj) r[adj_col] <- effect_cell(rec$adjusted, l, sp)
        labels <- c(labels, paste0("  ", l))
        row_type <- c(row_type, "level")
        row_var <- c(row_var, v)
        row_level <- c(row_level, l)
        rows[[length(rows) + 1]] <- r
      }

      missing_row(rec, v, sp)
    }
  }

  mat <- do.call(rbind, lapply(rows, function(r) r[col_names]))
  rownames(mat) <- NULL
  colnames(mat) <- col_names

  list(labels = labels, mat = mat, col_names = col_names,
       row_type = row_type, row_var = row_var, row_level = row_level)
}

#' @keywords internal
#' @noRd
.table1_show_missing <- function(meta) {
  missing <- meta$missing
  if (is.list(missing)) {
    return(isTRUE(missing$display))
  }
  isTRUE(missing)
}

#' Resolve the table-wide style (for the visual flex theme / footer).
#' @keywords internal
#' @noRd
.resolve_table_style <- function(style) {
  if (inherits(style, "simtab_style") || (is.character(style) && length(style) == 1)) {
    return(.resolve_style(style))
  }
  .resolve_style("default")
}

#' @keywords internal
#' @noRd
.table1_print <- function(x, details = FALSE, ...) {
  details <- .validate_print_details(details)
  meta <- x$meta
  if (details) {
    strat_txt <- if (is.null(meta$strat_var)) "unstratified" else paste0("stratified by: ", meta$strat_var)
    cat(sprintf("Table 1 [%d variable%s | %s | N = %d]\n",
                length(meta$vars), if (length(meta$vars) == 1) "" else "s",
                strat_txt, meta$n_total))
    if (!is.null(x$call)) {
      cat("Call: ", paste(deparse(x$call), collapse = " "), "\n", sep = "")
    }
    cat("\n")
  }

  bd <- .build_table1(x)
  full <- cbind(Characteristic = bd$labels, bd$mat)
  colnames(full)[1] <- "Characteristic"
  widths <- vapply(seq_len(ncol(full)), function(j) {
    max(nchar(c(colnames(full)[j], full[, j])), na.rm = TRUE)
  }, integer(1))

  pad <- function(s, w, left = FALSE) {
    s <- ifelse(is.na(s), "", s)
    if (left) formatC(s, width = -w, flag = "-") else formatC(s, width = w)
  }
  # header
  hdr <- paste(mapply(function(j) pad(colnames(full)[j], widths[j], left = (j == 1)),
                      seq_len(ncol(full))), collapse = "  ")
  cat(hdr, "\n")
  cat(strrep("-", sum(widths) + 2 * (ncol(full) - 1)), "\n", sep = "")
  for (i in seq_len(nrow(full))) {
    line <- paste(mapply(function(j) pad(full[i, j], widths[j], left = (j == 1)),
                         seq_len(ncol(full))), collapse = "  ")
    cat(line, "\n")
  }

  # tests footer
  if (isTRUE(meta$has_test)) {
    methods <- vapply(meta$vars, function(v) {
      tr <- x$data[[v]]$test
      if (is.null(tr)) NA_character_ else tr$method
    }, character(1))
    methods <- methods[!is.na(methods)]
    if (length(methods) > 0) {
      um <- unique(methods)
      cat("\nTests: ", paste(um, collapse = "; "), "\n", sep = "")
    }
  }
  notes <- .table1_notes(x)
  if (length(notes) > 0) {
    cat(paste0("\nNote: ", notes, collapse = ""), "\n", sep = "")
  }
  .print_advice(x)
  invisible(x)
}

#' @keywords internal
#' @noRd
.table1_as_data_frame <- function(x, row.names = NULL, optional = FALSE, tidy = FALSE, ...) {
  if (isTRUE(tidy)) {
    return(.table1_tidy(x))
  }
  bd <- .build_table1(x)
  df <- data.frame(Characteristic = bd$labels)
  for (cn in bd$col_names) {
    df[[cn]] <- bd$mat[, cn]
  }
  attr(df, "row_type") <- bd$row_type
  attr(df, "row_var") <- bd$row_var
  attr(df, "row_level") <- bd$row_level
  attr(df, "notes") <- .table1_notes(x)
  df
}

#' Table notes that qualify the displayed numbers
#'
#' Shared by print, gt, and flextable so every rendering states the same
#' caveats: an Overall column that includes rows with a missing `by` value, and
#' an adjusted model that treated missing categories as their own level.
#' @keywords internal
#' @noRd
.table1_notes <- function(x) {
  meta <- x$meta
  notes <- character(0)
  strat <- meta$strat_var
  group_n <- meta$group_n
  if (!is.null(strat) && isTRUE(meta$overall) && "Overall" %in% names(group_n)) {
    in_groups <- sum(group_n[setdiff(names(group_n), "Overall")])
    n_missing_by <- as.integer(group_n[["Overall"]] - in_groups)
    if (n_missing_by > 0) {
      by_label <- .resolve_label(strat, x$used$ref$data, x$spec$fmt$labels)
      notes <- c(notes, sprintf(
        "Overall includes %d participant%s with missing %s.",
        n_missing_by, if (n_missing_by == 1) "" else "s", by_label
      ))
    }
  }
  if (!is.null(meta$adjust) && identical(meta$missing$model_na, "explicit")) {
    shown <- .table1_show_missing(meta) && any(vapply(meta$vars, function(v) {
      adj <- x$data[[v]]$adjusted
      is.data.frame(adj) && "(Missing)" %in% adj$level
    }, logical(1)))
    notes <- c(notes, paste0(
      "Adjusted models treated missing values of categorical variables as their own category",
      if (shown) "; its estimate is shown in the Missing row." else "."
    ))
  }
  notes
}

#' @keywords internal
#' @noRd
.table1_tidy <- function(x) {
  meta <- x$meta
  parts <- list()
  add <- function(variable, level, group, stat, value) {
    parts[[length(parts) + 1]] <<- data.frame(
      variable = variable, level = level, group = group, stat = stat,
      value = as.numeric(value)
    )
  }
  for (v in meta$vars) {
    rec <- x$data[[v]]
    if (rec$type == "continuous") {
      snames <- if (rec$stat == "mean") c("mean", "sd") else c("median", "q1", "q3")
      for (g in meta$group_names) {
        vals <- rec$summary[[g]]
        for (k in seq_along(snames)) add(v, NA_character_, g, snames[k], vals[k])
        add(v, NA_character_, g, "n", rec$counts[[g]])
      }
    } else {
      for (g in meta$group_names) {
        for (l in rec$levels) {
          add(v, l, g, "n", rec$freq[l, g])
          add(v, l, g, "pct", rec$pct[l, g])
        }
      }
    }
    for (kind in c("crude", "adjusted")) {
      df <- rec[[kind]]
      if (is.null(df)) next
      for (i in seq_len(nrow(df))) {
        lv <- df$level[i]
        add(v, lv, NA_character_, paste0(kind, "_est"), df$estimate[i])
        add(v, lv, NA_character_, paste0(kind, "_lo"), df$lower[i])
        add(v, lv, NA_character_, paste0(kind, "_hi"), df$upper[i])
        add(v, lv, NA_character_, paste0(kind, "_p"), df$p[i])
      }
    }
  }
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out
}

#' @keywords internal
#' @noRd
.table1_strat_spanner <- function(x, df) {
  meta <- x$meta
  if (is.null(meta$strat_var) || is.null(meta$strat_levels) || length(meta$strat_levels) < 2) {
    return(NULL)
  }

  cols <- names(df)
  level_cols <- vapply(meta$strat_levels, function(level) {
    nm <- sprintf("%s (N=%d)", level, meta$group_n[[level]])
    match(nm, cols)
  }, integer(1))
  level_cols <- level_cols[!is.na(level_cols)]
  if (length(level_cols) < 2) {
    return(NULL)
  }

  list(
    label = meta$strat_var,
    start = min(level_cols),
    end = max(level_cols),
    columns = seq(min(level_cols), max(level_cols))
  )
}

#' @keywords internal
#' @noRd
.table1_spanner_header <- function(ft, x, df, footnotes = NULL) {
  spanner <- .table1_strat_spanner(x, df)
  if (is.null(spanner)) {
    return(list(ft = ft, footnotes = footnotes))
  }

  n_cols <- ncol(df)
  values <- character(0)
  widths <- integer(0)
  if (spanner$start > 1L) {
    values <- c(values, "")
    widths <- c(widths, spanner$start - 1L)
  }
  values <- c(values, spanner$label)
  widths <- c(widths, length(spanner$columns))
  if (spanner$end < n_cols) {
    values <- c(values, "")
    widths <- c(widths, n_cols - spanner$end)
  }

  ft <- flextable::add_header_row(ft, values = values, colwidths = widths, top = TRUE)
  ft <- flextable::bold(ft, i = 1, part = "header")
  ft <- flextable::align(ft, i = 1, align = "center", part = "header")

  if (length(footnotes) > 0) {
    ft <- flextable::footnote(
      ft,
      i = 1,
      j = spanner$start,
      value = flextable::as_paragraph(footnotes[[1]]),
      ref_symbols = "a",
      part = "header"
    )
    footnotes <- footnotes[-1]
  }

  list(ft = ft, footnotes = footnotes)
}

#' @keywords internal
#' @noRd
.table1_as_flextable <- function(x, footnotes = NULL, ...) {
  .require_pkg("flextable")
  df <- as.data.frame(x, tidy = FALSE)
  rt <- attr(df, "row_type")
  ft <- flextable::flextable(df, ...)
  spanned <- .table1_spanner_header(ft, x, df, footnotes = footnotes)
  ft <- spanned$ft
  footnotes <- spanned$footnotes

  idx_h <- which(rt == "header")
  if (length(idx_h) > 0) ft <- flextable::bold(ft, i = idx_h, part = "body")
  idx_ind <- which(rt %in% c("level", "missing"))
  if (length(idx_ind) > 0) ft <- flextable::padding(ft, i = idx_ind, j = 1, padding.left = 18)
  idx_m <- which(rt == "missing")
  if (length(idx_m) > 0) ft <- flextable::color(ft, i = idx_m, color = "#7f7f7f")

  foot <- character(0)
  if (isTRUE(x$meta$has_test)) {
    methods <- unique(stats::na.omit(vapply(x$meta$vars, function(v) {
      tr <- x$data[[v]]$test
      if (is.null(tr)) NA_character_ else tr$method
    }, character(1))))
    if (length(methods) > 0) {
      foot <- c(foot, paste0("Tests: ", paste(methods, collapse = "; "), "."))
    }
  }
  foot <- c(foot, .table1_notes(x))
  if (!is.null(footnotes)) foot <- c(foot, footnotes)
  if (length(foot) > 0) {
    ft <- flextable::add_footer_lines(ft, values = foot)
    ft <- flextable::align(ft, part = "footer", align = "left")
  }

  spec <- .resolve_table_style(x$meta$style)
  spec$flex(ft)
}

#########
# ADDITIVE EFFECT HELPERS
# Helper verbs for modifying and extending computed table1 results.

#' Validates that an object is a computed table1 result
#' @keywords internal
#' @noRd
.check_table1 <- function(tab) {
  if (!inherits(tab, "simtab_table1")) {
    simtab_abort_input(c(
      "{.arg tab} must be a {.fn table1} result.",
      "i" = "Received an object of class {.cls {class(tab)[[1]]}}.",
      "v" = "Build one with {.code table1(data, vars, by = group)}."
    ))
  }
  invisible(TRUE)
}

#' Add or replace the effect-measure column(s) of a table1
#'
#' Recomputes `tab` with a crude (and optionally adjusted) effect-measure column.
#' Equivalent to passing `measure=`/`adjust=` to [table1()] directly; calling
#' it when an effect column already exists replaces it.
#'
#' @param tab A `table1` object.
#' @param measure One of `"OR"`, `"PR"`, `"RR"`.
#' @param adjust.for Optional covariate names for an adjusted column.
#' @param ref Optional reference level(s); see [table1()].
#' @param conf.level Optional confidence level; defaults to the table's.
#' @param ... Ignored.
#' @return The recomputed `table1` object.
#' @seealso [table1()], [test()]
#' @examples
#' data(epitabl)
#' table1(epitabl, c("sex", "smoking"), by = "adjudicated_acs") |> add_effect("OR")
#' @export
add_effect <- function(tab, measure, adjust.for = NULL, ref = NULL, conf.level = NULL, ...) {
  .check_table1(tab)
  spec <- tab$spec
  spec <- measure(
    spec,
    measure,
    ref = ref %||% spec$effect$ref,
    conf.level = conf.level %||% spec$effect$conf.level
  )
  if (!is.null(adjust.for)) {
    spec <- .bind_role(spec, "adjust", .table1_vars_quosures(adjust.for))
  }
  obj <- evaluate(spec)
  obj$call <- match.call()
  obj
}

#' Returns registered renderer methods for the table1 engine
#' @keywords internal
#' @noRd
.table1_renderers <- function() {
  list(
    print = .table1_print,
    as_data_frame = .table1_as_data_frame,
    as_flextable = .table1_as_flextable,
    autoplot = .forest_plot,
    as_methods = .methods_as_table1
  )
}

#' Validates specification roles and parameters for the descriptive engine
#' @keywords internal
#' @noRd
.validate_descriptive <- function(spec) {
  data <- spec$data_src$ref$data

  vars <- .resolve_tidyselect_role(spec, "describe")
  if (length(vars) == 0) {
    simtab_abort_spec(c(
      "A descriptive specification requires a non-empty {.val describe} role.",
      "i" = "No columns were bound before compute.",
      "v" = "Add columns with {.code describe(spec, c(age, sex))}."
    ))
  }

  by <- .resolve_single_role(spec, "by")

  effect <- spec$effect$measure
  has_effect <- !is.null(effect)
  adjust_vars <- .resolve_tidyselect_role(spec, "adjust")

  if (has_effect && is.null(by)) {
    simtab_abort_engine(c(
      "An effect measure requires a binary {.val by} role.",
      "i" = "No outcome/grouping variable was bound for the requested effect.",
      "v" = "Add one with {.fn stratify} before computing."
    ))
  }
  if (length(adjust_vars) > 0 && !has_effect) {
    simtab_abort_spec(c(
      "{.fn adjust} requires an effect measure to be set.",
      "i" = "Adjustment covariates only mean something for an effect estimate.",
      "v" = "Add {.code measure(spec, \"OR\")} (or another measure) before adjusting."
    ))
  }
  if (has_effect) {
    measure_spec <- .get_measure(effect)
    if (.is_function_measure(measure_spec)) {
      if (!all(c(is.function(measure_spec$estimator), is.function(measure_spec$ci)))) {
        simtab_abort_engine(c(
          sprintf("Function-valued measure {.val %s} has an incomplete computation contract.", effect),
          "i" = "The descriptive engine requires paired estimator and CI functions.",
          "v" = "Register both functions using the contract documented by {.fn register_measure}."
        ))
      }
      if (length(adjust_vars) > 0) {
        simtab_abort_engine(c(
          sprintf("Function-valued measure {.val %s} does not support adjusted descriptive effects.", effect),
          "i" = "The registered function contract consumes crude categorical 2x2 counts.",
          "v" = "Remove {.fn adjust}, or register a dedicated engine for adjusted estimation."
        ))
      }
      compatible <- any(tolower(measure_spec$families) %in% c("descriptive", "2x2"))
      if (!compatible) {
        simtab_abort_engine(c(
          sprintf("Measure {.val %s} is not registered for the descriptive 2x2 engine.", effect),
          "i" = "Its {.arg families} entry does not include {.val descriptive} or {.val 2x2}.",
          "v" = "Register the measure for a compatible family or select another engine."
        ))
      }
    }
    by_levels <- levels(droplevels(factor(data[[by]])))
    if (length(by_levels) != 2) {
      simtab_abort_spec(c(
        "Effect measures require a binary {.arg by}, but {.val {by}} has
         {length(by_levels)} levels.",
        "i" = "Levels found: {.val {by_levels}}.",
        "v" = "Collapse {.val {by}} to two levels, or drop the effect measure."
      ))
    }
  }

  invisible(spec)
}
