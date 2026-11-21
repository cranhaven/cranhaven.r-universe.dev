# BIVARIATE AND CONTINGENCY TABLE ANALYSIS
# Fast contingency tables, cross-tabulations, and stratum-specific association metrics.
# Computes raw frequencies, percentages, effect ratios (PR, RR, OR), and hypothesis tests.

#' Cross-tabulate one or two variables
#'
#' `tb()` builds a frequency table for one variable, or a cross-tabulation of
#' two, with optional percentages, an association test, and crude effect
#' measures (PR, RR, or OR). Put the exposure first (rows) and the outcome
#' second (columns); a numeric first variable is summarised as mean (SD) or
#' median (IQR) within each outcome group. For many variables at once use
#' [table1()]; for adjusted estimates use [regtab()].
#'
#' @param data A data frame, or a single vector to tabulate on its own.
#' @param ... One or two variables to tabulate: bare names, strings, or
#'   tidyselect expressions such as `all_of(v)`. The first becomes the rows and
#'   the second the columns. Terse flags such as `row` or `or` may follow; see
#'   *Terse flags*.
#' @param m Deprecated; use `miss` instead.
#' @param miss Logical. If `TRUE`, show missing values as their own row and
#'   column. Same as the `miss` flag.
#' @param d Integer. Decimal places for percentages and continuous summaries.
#'   Effect measures always use two decimals.
#' @param big_mark String inserted between every three digits, e.g. `","`
#'   prints `4391` as `4,391`.
#' @param decimal_mark String used as the decimal point, e.g. `","`. Must
#'   differ from `big_mark`.
#' @param style String. How counts and percentages are shown: `"n_pct"`
#'   (`12 (5.0%)`), `"pct_n"` (`5.0% (12)`), or a template using `{n}` and
#'   `{p}`, such as `"{n} [{p}%]"`.
#' @param style.rp String template for prevalence and risk ratios, using
#'   `{rp}`, `{lower}`, and `{upper}`.
#' @param style.or String template for odds ratios, using `{or}`, `{lower}`,
#'   and `{upper}`.
#' @param test Logical or string. `TRUE` adds a p-value from an automatically
#'   chosen test; or force `"chisq"`, `"fisher"`, or `"mcnemar"`. See
#'   *Statistical methods* for how the test is chosen.
#' @param subset A logical expression evaluated in `data` to keep only some
#'   rows, e.g. `subset = age >= 65`.
#' @param strat A variable to stratify by: a bare name or string. The table is
#'   repeated within each stratum and, if an effect measure is requested,
#'   a Mantel-Haenszel pooled estimate is added.
#' @param rp Deprecated; use the `pr` flag or `measure = "PR"` instead.
#' @param or Logical. If `TRUE`, add odds ratios. Same as the `or` flag.
#' @param ref Reference level of the row (exposure) variable for effect
#'   measures, given as a level name or its position. If `NULL`, the first
#'   level is used and a note is printed.
#' @param conf.level Number between 0 and 1. Confidence level for effect
#'   measure intervals.
#' @param var.type Force variable types: `"continuous"` or `"categorical"`,
#'   either one string for all variables or a named vector such as
#'   `c(score = "continuous")`. If `NULL`, types are detected automatically
#'   (see *Statistical methods*).
#' @param summary String. Summary for numeric variables: `"auto"` chooses
#'   between `"mean"` (mean and SD) and `"median"` (median and IQR) based on
#'   sample size and skewness.
#' @param flags Character vector of terse flags, e.g. `c("row", "or", "p")`.
#'   The programmatic form of the bare flags in `...`.
#' @param labels Named character vector of display labels, e.g.
#'   `c(smoking = "Smoking status")`. Variable labels already stored in `data`
#'   are used by default.
#' @param measure String. Effect measure to add: `"PR"`, `"RR"`, or `"OR"`.
#'   Overrides any measure implied by `design`.
#' @param design String. Study design used to choose the effect measure when
#'   none is requested: `"cross_sectional"` gives PR, `"cohort"` gives RR, and
#'   `"case_control"` gives OR.
#'
#' @eval .flag_roxygen_section("tb")
#'
#' @details
#' ## Statistical methods
#' Effect measures compare each row level with the reference level (`ref`),
#' taking the last column level as the event. Prevalence and risk ratios use
#' the Katz log interval (Katz et al., 1978) and odds ratios the Woolf logit
#' interval (Woolf, 1955). With `strat`, stratum-specific tables are pooled
#' with the Mantel-Haenszel estimator and tested with the Cochran-Mantel-Haenszel
#' test. Homogeneity across strata is tested with the Breslow-Day test for odds
#' ratios and, for prevalence and risk ratios, with Cochran's Q on the
#' inverse-variance weighted stratum log ratios. A stratified numeric variable
#' is compared within strata: by the F-test for the group term in a linear
#' model with stratum when summarised by the mean, and by the van Elteren
#' stratified Wilcoxon test (van Elteren, 1960) for two groups when summarised
#' by the median.
#'
#' With `test = TRUE`, 2x2 tables use the N-1 chi-squared test (Campbell,
#' 2007) and larger tables the Pearson chi-squared test, without continuity
#' correction. Fisher's exact test is chosen automatically only when an
#' expected count is below 1; request it with `test = "fisher"`. Numeric
#' variables are compared with the t-test or ANOVA when summarised by the
#' mean, and with the Wilcoxon or Kruskal-Wallis test when summarised by the
#' median.
#'
#' Numeric variables are treated as continuous unless they look like coded
#' categories: exactly two distinct whole numbers, or at most seven distinct
#' whole numbers with at least 20 observations. Use `var.type` to override.
#'
#' ## Missing data
#' Missing values are excluded from counts, percentages, tests, and effect
#' measures. Use the `miss` flag to display them as a separate category.
#'
#' ## Modifying the result
#' Paired tests and multiplicity adjustment are set afterwards with [test()],
#' e.g. `tb(df, value, group, test = TRUE) |> test(paired = TRUE)`. Use
#' [fmt()] to change decimals and [rbind()] to stack several `tb()` tables
#' that share the same column variable.
#'
#' @return A `simtab_result` of class `simtab_tb`. Print it to see the
#'   formatted table, convert it with `as.data.frame()`, or save it with
#'   [export_docx()], [export_pptx()], or [export_xlsx()]. Unrounded results
#'   are stored in `$data`.
#' @seealso [table1()] for many variables at once, [regtab()] for adjusted
#'   effect measures, [diag_test()] for diagnostic accuracy, and
#'   [simtablr_references] for all references cited by SimtablR.
#' @examples
#' # Frequencies of one variable
#' tb(epitabl, smoking)
#'
#' # Row percentages, a p-value, and crude prevalence ratios
#' tb(epitabl, smoking, adjudicated_acs, flags = c("row", "pr", "p"), ref = "Never")
#'
#' # The same request with bare flags (shorthand)
#' tb(epitabl, smoking, adjudicated_acs, row, pr, p, ref = "Never")
#'
#' # Odds ratio stratified by sex, with a Mantel-Haenszel pooled estimate
#' tb(epitabl, renal_impairment, adjudicated_acs, strat = sex, flags = "or", ref = "No")
#'
#' # A numeric variable summarised by group
#' tb(epitabl, age, adjudicated_acs, test = TRUE)
#' @references Campbell, I. (2007). Chi-squared and Fisher-Irwin tests of
#'   two-by-two tables with small sample recommendations. \emph{Statistics in
#'   Medicine}, 26(19), 3661--3675. \doi{10.1002/sim.2832}.
#'
#'   Katz, D., Baptista, J., Azen, S. P., & Pike, M. C. (1978). Obtaining
#'   confidence intervals for the risk ratio in cohort studies.
#'   \emph{Biometrics}, 34(3), 469--474. \doi{10.2307/2530610}.
#'
#'   Woolf, B. (1955). On estimating the relation between blood group and
#'   disease. \emph{Annals of Human Genetics}, 19(4), 251--253.
#'   \doi{10.1111/j.1469-1809.1955.tb01348.x}.
#'
#'   Pearson, K. (1900). On the criterion that a given system of deviations
#'   from the probable in the case of a correlated system of variables is such
#'   that it can be reasonably supposed to have arisen from random sampling.
#'   \emph{Philosophical Magazine, Series 5}, 50(302), 157--175.
#'   \doi{10.1080/14786440009463897}.
#'
#'   Fisher, R. A. (1935). \emph{The Design of Experiments}. Oliver & Boyd.
#' @export
#########
# BIVARIATE CONTINGENCY TABLE INTERFACE
# Direct user-facing constructor for cross-tabulations and stratum-specific metrics.
tb <- function(
  data,
  ...,
  m = FALSE,
  miss = FALSE,
  d = 1,
  big_mark = "",
  decimal_mark = ".",
  style = "n_pct",
  style.rp = "{rp} ({lower} - {upper})",
  style.or = "{or} ({lower} - {upper})",
  test = FALSE,
  subset = NULL,
  strat = NULL,
  rp = FALSE,
  or = FALSE,
  ref = NULL,
  conf.level = 0.95,
  var.type = NULL,
  summary = "auto",
  flags = NULL,
  labels = NULL,
  measure = NULL,
  design = NULL
) {
  subset_expr <- substitute(subset)
  strat_expr <- substitute(strat)
  caller_env <- parent.frame()

  parsed <- .tb_parse_inputs(
    data = data,
    dot_exprs = as.list(substitute(list(...)))[-1],
    dots_env = caller_env,
    m = isTRUE(m) || isTRUE(miss),
    m_deprecated = !missing(m) && isTRUE(m),
    rp = rp,
    or = or,
    flags = flags,
    measure = measure,
    subset_expr = subset_expr,
    strat_expr = strat_expr
  )

  spec <- .tb_spec(
    data = parsed$data,
    base_spec = parsed$base_spec,
    var_names = parsed$var_names,
    m = parsed$flags_list$missing,
    d = d,
    big_mark = big_mark,
    decimal_mark = decimal_mark,
    style = style,
    style.rp = style.rp,
    style.or = style.or,
    test = test,
    test_explicit = !missing(test),
    subset = subset_expr,
    subset_env = caller_env,
    strat = strat_expr,
    strat_env = caller_env,
    measure = parsed$measure,
    ref = ref,
    conf.level = conf.level,
    var.type = var.type,
    stat.cont = summary,
    flags_list = parsed$flags_list,
    flag_tokens = parsed$flag_tokens,
    pct_requested = parsed$pct_requested,
    labels = labels,
    design = design,
    call = match.call()
  )

  evaluate(spec)
}

#########
# STACKED CONTINGENCY TABLES
# Row-wise concatenation of bivariate tables sharing a common column stratifier.

#' Combine tb Objects by Rows
#'
#' Vertical stacking of `tb` objects to create multi-variable tables.
#'
#' @details
#' This method is registered as an S3 method on `base::rbind()` (it does not
#' mask `base::rbind`), so `rbind(tb1, tb2)` dispatches here while ordinary
#' matrix/data.frame `rbind()` is unaffected.
#'
#' The resulting `rbind_tb` object combines multiple bivariate tables sharing
#' a common stratifying column into a stacked summary table.
#'
#' @param ... Objects of class `simtab_tb` to be combined.
#' @param deparse.level Integer controlling label deparsing (unused).
#' @return A combined object of class `c("simtab_rbind_tb", "rbind_tb", "simtab")`.
#' @seealso [tb()]
#' @method rbind simtab_tb
#' @examples
#' t1 <- tb(epitabl, sex, diabetes)
#' t2 <- tb(epitabl, hypertension, diabetes)
#' rbind(t1, t2)
#' @export
rbind.simtab_tb <- function(..., deparse.level = 1) {
  dots <- list(...)
  if (length(dots) == 0) {
    simtab_abort_input(c(
      "No objects were provided to {.fn rbind}.",
      "i" = "Row-stacking needs at least one {.fn tb} result.",
      "v" = "Pass the tables to stack, e.g. {.code rbind(t1, t2)}."
    ))
  }
  for (i in seq_along(dots)) {
    if (!inherits(dots[[i]], "simtab_tb")) {
      simtab_abort_input(c(
        "Argument {i} is not a {.fn tb} result.",
        "i" = "Received an object of class {.cls {class(dots[[i]])[[1]]}}.",
        "v" = "Only {.fn tb} results can be row-stacked."
      ))
    }
  }
  ref_meta <- dots[[1]]$meta
  for (i in seq_along(dots)[-1]) {
    if (dots[[i]]$meta$col_var_name != ref_meta$col_var_name) {
      simtab_abort_input(c(
        "Column variable mismatch at argument {i}.",
        "i" = "Found {.val {dots[[i]]$meta$col_var_name}} but expected
               {.val {ref_meta$col_var_name}}.",
        "v" = "All stacked tables must share the same column variable."
      ))
    }
  }
  out_obj <- list(
    data = lapply(dots, function(x) x$data),
    meta = list(
      tables = lapply(dots, function(x) x$meta),
      row_label = "Variable",
      col_label = ref_meta$col_label,
      col_var_name = ref_meta$col_var_name
    ),
    call = match.call()
  )
  class(out_obj) <- c("simtab_rbind_tb", "rbind_tb", "simtab")
  return(out_obj)
}

#########
# DISPLAY MATRIX BUILDER
# Formats raw frequency counts and association metrics into visual character matrices.

#' Resolve display label for a variable name
#' @keywords internal
#' @noRd
.resolve_label <- function(var_name, data, labels_arg) {
  if (!is.null(labels_arg) && var_name %in% names(labels_arg)) {
    return(labels_arg[[var_name]])
  }
  if (is.data.frame(data) && var_name %in% names(data)) {
    lbl <- attr(data[[var_name]], "label", exact = TRUE)
    if (!is.null(lbl)) return(lbl)
  }
  return(var_name)
}

#' Build formatted display character matrix from raw bivariate table evidence
#' @keywords internal
#' @noRd
.build_display_matrix <- function(x) {
  if (inherits(x, "simtab_rbind_tb")) {
    mats <- lapply(.rbind_tb_tables(x), .build_display_matrix)
    all_cols <- unique(unlist(lapply(mats, colnames)))
    parts <- list()
    for (i in seq_along(x$data)) {
      mat_i <- mats[[i]]
      is_cont_i <- x$meta$tables[[i]]$is_continuous
      if (is_cont_i) {
        new_mat <- matrix(
          "",
          nrow = 1,
          ncol = length(all_cols),
          dimnames = list(rownames(mat_i), all_cols)
        )
        cols_intersect <- intersect(colnames(mat_i), all_cols)
        new_mat[1, cols_intersect] <- mat_i[1, cols_intersect]
        parts[[length(parts) + 1]] <- new_mat
      } else {
        nr_i <- nrow(mat_i)
        is_2d <- length(dim(x$data[[i]]$frequencies)) == 2
        has_total_row <- rownames(mat_i)[nr_i] == "Total"
        rows_to_keep <- seq_len(nr_i)
        if (has_total_row && is_2d && i < length(x$data)) {
          rows_to_keep <- seq_len(nr_i - 1)
        }
        sub_mat <- mat_i[rows_to_keep, , drop = FALSE]
        var_lbl <- x$meta$tables[[i]]$row_label
        header_mat <- matrix(
          "",
          nrow = 1,
          ncol = length(all_cols),
          dimnames = list(var_lbl, all_cols)
        )
        level_mat <- matrix(
          "",
          nrow = nrow(sub_mat),
          ncol = length(all_cols),
          dimnames = list(rownames(sub_mat), all_cols)
        )
        cols_intersect <- intersect(colnames(sub_mat), all_cols)
        level_mat[, cols_intersect] <- sub_mat[, cols_intersect]
        rnames <- rownames(level_mat)
        rownames(level_mat) <- ifelse(rnames == "Total", rnames, paste0("  ", rnames))
        parts[[length(parts) + 1]] <- header_mat
        parts[[length(parts) + 1]] <- level_mat
      }
    }
    return(do.call(rbind, parts))
  }

  is_continuous <- x$meta$is_continuous
  d <- x$meta$d
  style <- x$meta$style
  journal <- .result_journal(style)
  if (!is.null(journal)) {
    style <- journal$np_template
  }
  iqr_sep <- if (is.null(journal)) " - " else journal$ci_sep
  bm <- x$meta$big_mark %||% ""
  dm <- x$meta$decimal_mark %||% "."

  if (is_continuous) {
    raw_sum <- x$data$summary
    out_mat <- matrix(
      "",
      nrow = 1,
      ncol = length(raw_sum),
      dimnames = list(x$meta$row_label, names(raw_sum))
    )

    stat_lbl <- if (x$meta$stat.cont == "mean") "Mean (SD)" else "Median (IQR)"
    rownames(out_mat) <- paste0(rownames(out_mat), " [", stat_lbl, "]")

    for (nm in names(raw_sum)) {
      val <- raw_sum[[nm]]
      if (length(val) == 0) {
        out_mat[1, nm] <- "-"
      } else if (x$meta$stat.cont == "mean") {
        out_mat[1, nm] <- sprintf(
          "%s (%s)",
          .tb_fmt_num(val[1], d, bm, dm),
          .tb_fmt_num(val[2], d, bm, dm)
        )
      } else {
        bounds <- c(.tb_fmt_num(val[2], d, bm, dm), .tb_fmt_num(val[3], d, bm, dm))
        out_mat[1, nm] <- sprintf(
          "%s (%s%s%s)",
          .tb_fmt_num(val[1], d, bm, dm),
          bounds[1],
          .ci_sep_safe(iqr_sep, bounds),
          bounds[2]
        )
      }
    }
    if (!identical(x$meta$p.adjust %||% "none", "none") && is.data.frame(x$data$tests)) {
      out_mat <- cbind(out_mat, "Adjusted P-value" = .fmt_tb_p(x$data$tests$p_value_adjusted[1], dm, journal))
    }
    return(out_mat)
  } else {
    freq <- x$data$frequencies
    pct <- x$data$percentages
    flags <- x$meta$flags

    freq_m <- addmargins(freq)
    if (length(dim(freq)) == 1) {
      names(freq_m)[length(freq_m)] <- "Total"
      freq_mat <- as.matrix(freq_m)
      colnames(freq_mat) <- "Freq"
      if (!is.null(pct)) {
        pct_mat <- as.matrix(c(pct, NA_real_))
      } else {
        pct_mat <- NULL
      }
    } else {
      freq_mat <- freq_m
      rownames(freq_mat)[nrow(freq_mat)] <- "Total"
      colnames(freq_mat)[ncol(freq_mat)] <- "Total"
      pct_mat <- pct
    }

    nr <- nrow(freq_mat)
    nc <- ncol(freq_mat)
    out_mat <- matrix("", nrow = nr, ncol = nc, dimnames = dimnames(freq_mat))

    for (i in seq_len(nr)) {
      for (j in seq_len(nc)) {
        val <- freq_mat[i, j]
        n_str <- .tb_fmt_num(val, 0, bm, dm)
        has_pct <- !is.null(pct_mat) &&
          i <= nrow(pct_mat) &&
          j <= ncol(pct_mat) &&
          !is.na(pct_mat[i, j])

        if (flags$percent && has_pct) {
          p_str <- sprintf(paste0("%.", d, "f"), pct_mat[i, j])
          if (style == "n_pct") {
            out_mat[i, j] <- sprintf("%s (%s%%)", n_str, p_str)
          } else if (style == "pct_n") {
            out_mat[i, j] <- sprintf("%s%% (%s)", p_str, n_str)
          } else {
            txt <- gsub("{n}", n_str, style, fixed = TRUE)
            txt <- gsub("{p}", p_str, txt, fixed = TRUE)
            out_mat[i, j] <- txt
          }
        } else {
          out_mat[i, j] <- n_str
        }
      }
    }

    if (!is.null(x$data$ratios)) {
      ratios <- x$data$ratios
      ratio_text <- vapply(
        seq_len(nrow(ratios)),
        function(idx) {
          if (ratios$ref[idx]) {
            ref_digits <- if (is.null(journal)) 2 else journal$digits_est
            return(paste0(.tb_fmt_num(1, ref_digits, "", dm), " (Ref)"))
          }
          if (is.na(ratios$estimate[idx])) {
            return("-")
          }
          p_val <- ratios$p_value[idx]
          paste0(
            .tb_effect_text(ratios[idx, ], x$meta, bm, dm, journal),
            if (is.na(p_val)) "" else paste0(", ", .fmt_tb_p(p_val, dm, journal))
          )
        },
        character(1)
      )

      if (length(ratio_text) < nr) {
        ratio_text <- c(ratio_text, rep("", nr - length(ratio_text)))
      }
      lbl_col <- paste0(ratios$type[1], " (95% CI)")
      out_mat <- cbind(out_mat, ratio_text)
      colnames(out_mat)[ncol(out_mat)] <- lbl_col
    }
    if (!identical(x$meta$p.adjust %||% "none", "none") && is.data.frame(x$data$tests)) {
      adj_text <- rep("", nr)
      target <- if ("Total" %in% rownames(out_mat)) {
        match("Total", rownames(out_mat))
      } else {
        1L
      }
      adj_text[target] <- .fmt_tb_p(x$data$tests$p_value_adjusted[1], dm, journal)
      out_mat <- cbind(out_mat, adj_text)
      colnames(out_mat)[ncol(out_mat)] <- "Adjusted P-value"
    }
    if (!is.null(x$data$mh)) {
      mh <- x$data$mh
      pooled <- mh[mh$row_type == "pooled", , drop = FALSE]
      if (nrow(pooled) > 0) {
        ratio_col <- grep(" \\(95% CI\\)$| MH \\(95% CI\\)$", colnames(out_mat), value = TRUE)
        if (length(ratio_col) == 0) {
          ratio_col <- paste0(pooled$type[1], " MH (95% CI)")
          out_mat <- cbind(out_mat, "")
          colnames(out_mat)[ncol(out_mat)] <- ratio_col
        } else {
          ratio_col <- ratio_col[1]
        }

        mh_rows <- matrix(
          "",
          nrow = nrow(pooled),
          ncol = ncol(out_mat),
          dimnames = list(paste0("Mantel-Haenszel pooled: ", pooled$level), colnames(out_mat))
        )
        mh_text <- vapply(seq_len(nrow(pooled)), function(idx) {
          if (is.na(pooled$estimate[idx])) {
            return("-")
          }
          txt <- .tb_effect_text(pooled[idx, ], x$meta, bm, dm, journal)
          cmh <- if (is.na(pooled$cmh_p[idx])) "" else paste0(", CMH ", .fmt_tb_p(pooled$cmh_p[idx], dm, journal))
          homog_label <- if (startsWith(pooled$homogeneity_method[idx] %||% "Breslow-Day", "Breslow-Day")) {
            "BD"
          } else {
            "homogeneity"
          }
          bd <- if (is.na(pooled$homogeneity_p[idx])) "" else paste0(", ", homog_label, " ", .fmt_tb_p(pooled$homogeneity_p[idx], dm, journal))
          paste0(txt, cmh, bd)
        }, character(1))
        mh_rows[, ratio_col] <- mh_text
        out_mat <- rbind(out_mat, mh_rows)
      }
    }
    return(out_mat)
  }
}

#########
# FORMATTING AND CONSOLE PRINTING HELPERS
# Text alignment, grid borders, and terminal display formatting for bivariate tables.

#' Format numeric value for display with configurable locale marks
#' @keywords internal
#' @noRd
.tb_fmt_num <- function(value, digits, big_mark = "", decimal_mark = ".") {
  if (length(value) == 0) {
    return(NA_character_)
  }
  if (is.na(value)) {
    # Format NA values consistently as character NA
    return("NA")
  }
  .unsign_zero(formatC(
    value,
    format = "f",
    digits = digits,
    big.mark = big_mark,
    decimal.mark = decimal_mark
  ))
}

#' Fill effect ratio template string with estimates and confidence limits
#'
#' An explicitly customised `style.rp`/`style.or` template wins; otherwise an
#' attached journal preset supplies digits, CI separator, brackets and template.
#' @keywords internal
#' @noRd
.tb_effect_text <- function(row, meta, bm, dm, journal = NULL) {
  is_rp <- row$type %in% c("PR", "RR")
  txt <- if (is_rp) meta$style.rp else meta$style.or
  default_txt <- if (is_rp) formals(tb)$style.rp else formals(tb)$style.or
  if (!is.null(journal) && (is.null(txt) || identical(txt, default_txt))) {
    de <- journal$digits_est
    return(.fmt_est(
      row$estimate, row$lower_ci, row$upper_ci, journal,
      num = function(v) .tb_fmt_num(v, de, bm, dm)
    ))
  }
  txt <- gsub(if (is_rp) "{rp}" else "{or}", .tb_fmt_num(row$estimate, 2, bm, dm), txt, fixed = TRUE)
  txt <- gsub("{lower}", .tb_fmt_num(row$lower_ci, 2, bm, dm), txt, fixed = TRUE)
  gsub("{upper}", .tb_fmt_num(row$upper_ci, 2, bm, dm), txt, fixed = TRUE)
}

#' Format p-value with inequality threshold for small values
#'
#' With a journal preset, its threshold, digits and leading-zero rules apply.
#' @keywords internal
#' @noRd
.fmt_tb_p <- function(p, decimal_mark = ".", journal = NULL) {
  if (is.na(p)) {
    return("p = NA")
  }
  if (!is.null(journal)) {
    txt <- .fmt_p(p, journal)
    if (!identical(decimal_mark, ".")) {
      txt <- sub(".", decimal_mark, txt, fixed = TRUE)
    }
    if (substr(txt, 1, 1) %in% c("<", ">")) {
      return(paste0("p ", substr(txt, 1, 1), " ", substring(txt, 2)))
    }
    return(paste0("p = ", txt))
  }
  if (p < 0.001) {
    paste0("p < ", .tb_fmt_num(0.001, 3, "", decimal_mark))
  } else {
    paste0("p = ", .tb_fmt_num(p, 3, "", decimal_mark))
  }
}

#' Extract individual component bivariate tables from a stacked container
#' @keywords internal
#' @noRd
.rbind_tb_tables <- function(x) {
  lapply(seq_along(x$data), function(i) {
    structure(
      list(data = x$data[[i]], meta = x$meta$tables[[i]], call = NULL),
      class = c("simtab_tb", "tb", "simtab")
    )
  })
}

#' Convert display matrix into a data frame with row labels in the first column
#' @keywords internal
#' @noRd
.tb_display_df <- function(x) {
  out_mat <- .build_display_matrix(x)
  df <- cbind(Row_Label = rownames(out_mat), as.data.frame(out_mat))
  colnames(df)[1] <- x$meta$row_label
  rownames(df) <- NULL
  df
}

#' Print method implementation for bivariate table objects
#' @keywords internal
#' @noRd
.tb_print <- function(x, digits = NULL, ...) {
  out_mat <- .build_display_matrix(x)
  .print_grid_adapted(out_mat, x$meta$row_label, x$meta$col_label, x)
  .print_advice(x)
  invisible(x)
}

#' Print Method for simtab_rbind_tb Objects
#'
#' @param x A `simtab_rbind_tb` object.
#' @param digits Minimum number of significant digits to be printed.
#' @param ... Additional arguments.
#' @return Invisibly returns `x`.
#' @examples
#' t1 <- tb(epitabl, sex, diabetes)
#' t2 <- tb(epitabl, hypertension, diabetes)
#' print(rbind(t1, t2))
#' @export
print.simtab_rbind_tb <- function(x, digits = NULL, ...) {
  .tb_print(x, digits = digits, ...)
}

#' Render formatted character grid with border rules to the console
#' @keywords internal
#' @noRd
.print_grid_adapted <- function(out_mat, row_var, col_var, x) {
  safe_nchar <- function(s) nchar(ifelse(is.na(s), "NA", s))
  center_text <- function(txt, width) {
    txt <- if (is.na(txt)) "NA" else txt
    pad <- max(0L, width - nchar(txt))
    paste0(strrep(" ", floor(pad / 2)), txt, strrep(" ", pad - floor(pad / 2)))
  }
  right_text <- function(txt, width) {
    sprintf(paste0("%", width, "s"), if (is.na(txt)) "NA" else txt)
  }

  nr <- nrow(out_mat)
  nc <- ncol(out_mat)
  row_labels <- rownames(out_mat)
  col_labels <- colnames(out_mat)

  # Stratified tables append Mantel-Haenszel rows after the Total row, so find
  # the Total row by label rather than assuming it is last.
  total_rows <- which(row_labels == "Total")
  total_row <- if (length(total_rows) > 0) max(total_rows) else NA_integer_
  has_row_total <- !is.na(total_row)
  # Row labels can be NA under the `miss` flag.
  is_mh_row <- !is.na(row_labels) & startsWith(row_labels, "Mantel-Haenszel pooled")
  extra_cols <- if (any(grepl("PR \\(|RR \\(|OR \\(", col_labels))) 1L else 0L

  width_row <- max(safe_nchar(c(row_var, row_labels))) + 1L
  col_widths <- vapply(
    seq_len(nc),
    function(j) {
      max(safe_nchar(col_labels[j]), max(safe_nchar(out_mat[, j]))) + 2L
    },
    integer(1)
  )

  console_width <- if (requireNamespace("cli", quietly = TRUE)) {
    cli::console_width()
  } else {
    80L
  }
  j_start <- 1L

  while (j_start <= nc) {
    used <- width_row + 3L
    j_end <- j_start
    while (j_end <= nc) {
      if (used + col_widths[j_end] + 1L > console_width && j_end > j_start) {
        j_end <- j_end - 1L
        break
      }
      used <- used + col_widths[j_end] + 1L
      j_end <- j_end + 1L
    }
    if (j_end > nc) {
      j_end <- nc
    }
    cols_page <- j_start:j_end
    start_extra <- nc - extra_cols + 1L
    header_cols <- cols_page[cols_page < start_extra]

    # One row of cells: a divider before the Total column and before each
    # effect column ("|" in text rows, "+" in dash rules), then the cell.
    emit_cells <- function(sep, cell) {
      idx_sum <- nc - extra_cols
      for (j in cols_page) {
        if (
          isTRUE(has_row_total) &&
            j == idx_sum &&
            idx_sum > 1 &&
            j != cols_page[1]
        ) {
          cat(sep)
        }
        if (j > idx_sum && j != cols_page[1]) {
          cat(sep)
        }
        cat(cell(j))
      }
    }
    dash_cell <- function(j) strrep("-", col_widths[j])

    if (!is.null(col_var) && col_var != "" && length(header_cols) > 0) {
      cat(right_text("", width_row), " | ", sep = "")
      data_w <- sum(col_widths[header_cols]) + max(0L, length(header_cols) - 1L)
      cat(center_text(col_var, data_w), "\n")
    }

    cat(right_text(row_var, width_row), " |", sep = "")
    emit_cells("|", function(j) center_text(col_labels[j], col_widths[j]))
    cat("\n", strrep("-", width_row), "-+", sep = "")
    emit_cells("+", dash_cell)
    cat("\n")

    for (i in seq_len(nr)) {
      # A pooled estimate only has content in the effect column; do not repeat
      # an empty Mantel-Haenszel row in every frequency panel.
      if (is_mh_row[i] && all(is.na(out_mat[i, cols_page]) | !nzchar(trimws(out_mat[i, cols_page])))) {
        next
      }
      if (identical(i, total_row) && nr > 1) {
        cat(strrep("-", width_row), "-+", sep = "")
        emit_cells("+", dash_cell)
        cat("\n")
      }
      cat(right_text(row_labels[i], width_row), " |", sep = "")
      emit_cells("|", function(j) center_text(out_mat[i, j], col_widths[j]))
      cat("\n")
    }
    j_start <- j_end + 1L
    if (j_start <= nc) cat("\n")
  }

  stats <- x$meta$stats
  if (!is.null(stats)) {
    p_str <- sub("^p ", "", .fmt_tb_p(stats$p.value, x$meta$decimal_mark %||% "."))
    cat("\n  Test:", stats$method, " p-value", p_str, "\n")
  }
}

#########
# TIDY AND DATA FRAME COERCION
# Export bivariate table evidence into wide presentation or long tidy formats.

#' Convert bivariate table result into a data frame
#' @keywords internal
#' @noRd
.tb_as_data_frame <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  tidy = FALSE,
  ...
) {
  if (isTRUE(tidy)) {
    if (x$meta$is_continuous) {
      # Continuous long schema: variable, group, stat, value. Built from raw
      # numerics only; no statistic is discarded.
      raw <- x$data$summary
      cnts <- x$data$counts
      is_mean <- x$meta$stat.cont == "mean"
      parts <- list()
      for (g in names(raw)) {
        v <- raw[[g]]
        n_g <- if (!is.null(cnts) && g %in% names(cnts)) {
          as.numeric(cnts[[g]])
        } else {
          NA_real_
        }
        stat <- if (is_mean) c("n", "mean", "sd") else c("n", "median", "q1", "q3")
        value <- c(n_g, if (length(v) > 0) v[seq_len(length(stat) - 1)] else rep(NA_real_, length(stat) - 1))
        parts[[length(parts) + 1]] <- data.frame(
          variable = x$meta$row_var_name,
          group = g,
          stat = stat,
          value = as.numeric(value)
        )
      }
      df_tidy <- do.call(rbind, parts)
      rownames(df_tidy) <- NULL
      st <- x$meta$stats
      attr(df_tidy, "test") <- if (is.null(st)) {
        NULL
      } else {
        list(method = st$method, p.value = st$p.value)
      }
      return(df_tidy)
    } else {
      # Categorical tidy schema:
      # variable, level, estimate, lower_ci, upper_ci, p_value, outcome, n
      # (+ stratum for stratified tables). One row per table cell; `n` is the
      # cell count. A ratio describes the risk/odds of the event outcome, so it
      # is attached only to the event-outcome row of each level. Stratified
      # tables add one row per pooled Mantel-Haenszel estimate (n = NA).
      freq <- x$data$frequencies
      df0 <- as.data.frame(freq)
      n <- nrow(df0)

      level <- as.character(df0[[1]])
      outcome <- if (length(dim(freq)) == 1) {
        rep(NA_character_, n)
      } else {
        as.character(df0[[2]])
      }
      stratified <- isTRUE(x$meta$is_stratified) && is.data.frame(x$data$mh)
      stratum <- NULL
      if (stratified) {
        # Stratified columns are "<stratum> : <outcome>".
        parts <- strsplit(outcome, " : ", fixed = TRUE)
        stratum <- vapply(parts, function(p) if (length(p) >= 2) p[[1]] else NA_character_, character(1))
        outcome <- vapply(parts, function(p) p[[length(p)]], character(1))
      }

      estimate <- rep(NA_real_, n)
      lower_ci <- rep(NA_real_, n)
      upper_ci <- rep(NA_real_, n)
      p_value <- rep(NA_real_, n)
      ratios <- if (stratified) x$data$mh[x$data$mh$row_type == "stratum", , drop = FALSE] else x$data$ratios
      if (is.data.frame(ratios) && nrow(ratios) > 0) {
        event_level <- if (!is.null(ratios$event_level)) {
          ratios$event_level[[1]]
        } else {
          .tb_event_level(freq)
        }
        key <- if (stratified) paste(level, stratum, sep = "\r") else level
        ratio_key <- if (stratified) paste(ratios$level, ratios$stratum, sep = "\r") else ratios$level
        idx <- match(key, ratio_key)
        idx[is.na(outcome) | outcome != event_level] <- NA_integer_
        estimate <- ratios$estimate[idx]
        lower_ci <- ratios$lower_ci[idx]
        upper_ci <- ratios$upper_ci[idx]
        p_value <- ratios$p_value[idx]
      }

      df_tidy <- data.frame(
        variable = rep(x$meta$row_var_name, n),
        level = level,
        estimate = as.numeric(estimate),
        lower_ci = as.numeric(lower_ci),
        upper_ci = as.numeric(upper_ci),
        p_value = as.numeric(p_value),
        outcome = outcome,
        n = as.integer(df0[[ncol(df0)]])
      )
      if (stratified) {
        df_tidy$stratum <- stratum
        pooled <- x$data$mh[x$data$mh$row_type == "pooled", , drop = FALSE]
        if (nrow(pooled) > 0) {
          df_tidy <- rbind(df_tidy, data.frame(
            variable = rep(x$meta$row_var_name, nrow(pooled)),
            level = pooled$level,
            estimate = pooled$estimate,
            lower_ci = pooled$lower_ci,
            upper_ci = pooled$upper_ci,
            p_value = pooled$p_value,
            outcome = pooled$event_level,
            n = NA_integer_,
            stratum = pooled$stratum
          ))
        }
      }
      rownames(df_tidy) <- NULL
      return(df_tidy)
    }
  } else {
    df <- .tb_display_df(x)
    attr(df, "stats") <- x$meta$stats
    return(df)
  }
}

#' Event (last observed, non-missing) outcome level of a frequency table
#'
#' Mirrors the engine, which scores the last column of the complete-case table
#' (zero margins and the `miss` NA column dropped) as the event.
#' @keywords internal
#' @noRd
.tb_event_level <- function(freq) {
  if (length(dim(freq)) != 2) {
    return(NA_character_)
  }
  cols <- colnames(freq)
  keep <- !is.na(cols) & cols != "NA" & colSums(freq[!is.na(rownames(freq)), , drop = FALSE]) > 0
  if (!any(keep)) {
    return(NA_character_)
  }
  utils::tail(cols[keep], 1)
}

#' Convert rbind_tb to Data Frame
#'
#' @param x An `rbind_tb` object.
#' @param row.names NULL or a character vector giving the row names for the data frame.
#' @param optional Logical. If TRUE, setting row names and converting column names is optional.
#' @param tidy Logical. If `TRUE`, returns a long-format tidy data frame.
#' @param ... Additional arguments.
#' @return A data.frame.
#' @examples
#' t1 <- tb(epitabl, sex, diabetes)
#' t2 <- tb(epitabl, hypertension, diabetes)
#' as.data.frame(rbind(t1, t2))
#' @export
as.data.frame.simtab_rbind_tb <- function(
  x,
  row.names = NULL,
  optional = FALSE,
  tidy = FALSE,
  ...
) {
  if (isTRUE(tidy)) {
    parts <- lapply(.rbind_tb_tables(x), .tb_as_data_frame, tidy = TRUE)

    all_cols <- unique(unlist(lapply(parts, colnames)))
    standardized_parts <- lapply(parts, function(df) {
      missing_cols <- setdiff(all_cols, colnames(df))
      for (co in missing_cols) {
        df[[co]] <- NA
      }
      df[, all_cols, drop = FALSE]
    })

    return(do.call(rbind, standardized_parts))
  }
  .tb_display_df(x)
}

#########
# FLEXTABLE RENDERING
# Publication-quality flextable formatting with theme styling and statistical footers.

#' Apply default booktabs typography and alignment theme to flextable
#' @keywords internal
#' @noRd
simtab_theme <- function(ft) {
  .require_pkg("flextable")
  ft |>
    flextable::theme_booktabs() |>
    flextable::autofit() |>
    flextable::align(align = "center", part = "header") |>
    flextable::align(j = -1, align = "center", part = "body") |>
    flextable::align(j = 1, align = "left", part = "body")
}

#' Convert bivariate table result into a publication-ready flextable
#' @keywords internal
#' @noRd
.tb_as_flextable <- function(x, footnotes = NULL, ...) {
  .require_pkg("flextable")
  df <- as.data.frame(x, tidy = FALSE)
  ft <- flextable::flextable(df, ...)
  journal <- .result_journal(x$meta$style)
  stats <- x$meta$stats
  if (!is.null(stats)) {
    p_str <- sub("^p ", "", .fmt_tb_p(stats$p.value, x$meta$decimal_mark %||% ".", journal))
    stat_text <- paste0(stats$method, ": p-value ", p_str)
    ft <- flextable::add_footer_lines(ft, values = stat_text)
    ft <- flextable::align(ft, part = "footer", align = "right")
  }
  ft <- .flex_add_footnotes(ft, footnotes)
  if (is.null(journal)) simtab_theme(ft) else journal$flex(ft)
}

#' Convert simtab_rbind_tb Object to Flextable
#'
#' @param x A `simtab_rbind_tb` object.
#' @param footnotes Optional character vector of footer lines appended to the
#'   table.
#' @param ... Additional arguments passed to `flextable::flextable()`.
#' @return A `flextable` object.
#' @examples
#' if (requireNamespace("flextable", quietly = TRUE)) {
#'   t1 <- tb(epitabl, sex, diabetes)
#'   t2 <- tb(epitabl, hypertension, diabetes)
#'   as_flextable.simtab_rbind_tb(rbind(t1, t2))
#' }
#' @export
as_flextable.simtab_rbind_tb <- function(x, footnotes = NULL, ...) {
  .require_pkg("flextable")
  df <- as.data.frame(x, tidy = FALSE)
  ft <- flextable::flextable(df, ...)
  ft <- .flex_add_footnotes(ft, footnotes)
  simtab_theme(ft)
}

#' Dispatch list of renderer functions for bivariate contingency tables
#' @keywords internal
#' @noRd
.tb_renderers <- function() {
  list(
    print = .tb_print,
    as_data_frame = .tb_as_data_frame,
    as_flextable = .tb_as_flextable,
    autoplot = .forest_plot,
    as_methods = .methods_as_tb
  )
}
