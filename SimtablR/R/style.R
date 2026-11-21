# JOURNAL PRESENTATION AND TYPOGRAPHIC STYLING
# Defines journal style presets (NEJM, JAMA, Lancet, STROBE), typographic templates,
# p-value rounding policies, and flextable styling themes.

#########
# STYLE PRESET CONSTRUCTOR AND REGISTRY
# Manages journal_style definitions and user-extensible journal preset registration.

# Internal registry of named style presets, populated in .onLoad() via
# .seed_builtin_styles(). Keys are lower-cased preset names.
.simtab_style_registry <- new.env(parent = emptyenv())

#' Create a SimtablR style / journal preset
#'
#' Builds a `simtab_style` object: the single specification that controls both the
#' text conventions of a table (how counts, percentages, confidence intervals and
#' p-values are written) and its visual flextable theme. Pass the result to the
#' `style` argument of [table1()], or register it under a name with
#' [register_journal()] so it can be referred to as `style = "myjournal"`.
#'
#' @param np_template Character template for a categorical "count (percent)" cell,
#'   using `{n}` and `{p}` placeholders. If `NULL`, derived from `percent_sign`
#'   (`"{n} ({p}%)"` or `"{n} ({p})"`).
#' @param count_header Character appended to categorical variable headers to note
#'   the cell convention, e.g. `"n (%)"` or `"No. (%)"`.
#' @param digits_cont Integer decimals for continuous summaries. Default `1`.
#' @param digits_est Integer decimals for effect-measure estimates. Default `2`.
#' @param ci_sep Character separating confidence-interval bounds (and IQR bounds),
#'   e.g. `" - "`, `"\u2013"`, `" to "`. Default `" - "`. Where the separator
#'   would be ambiguous it is replaced for that interval only: a comma
#'   separator becomes `"; "` when a bound contains a comma (a decimal comma
#'   or grouping mark), and a dash separator becomes `" to "` when a bound is
#'   negative.
#' @param ci_parens Two characters wrapping the confidence interval, e.g. `"()"`
#'   or `"[]"`. Default `"()"`.
#' @param est_template Character template for an estimate-with-CI string, using
#'   `{est}`, `{lower}`, `{upper}`, `{sep}` and the bracket placeholders
#'   `{lp}`/`{rp}`. If `NULL`, `"{est} {lp}{lower}{sep}{upper}{rp}"`.
#' @param pval_thresh Numeric; p-values below this print as `"<thresh"`. Default `0.001`.
#' @param pval_digits Integer decimals for p-values. Default `3`.
#' @param pval_upper Optional numeric ceiling; p-values above this print as
#'   `">upper"`. Default `NULL`.
#' @param pval_leading_zero Logical; whether p-values keep a leading zero before
#'   the decimal point. Default `TRUE`.
#' @param pval_adaptive Logical; whether p-values above `0.01` use two decimals
#'   and smaller p-values use `pval_digits`. Default `FALSE`.
#' @param percent_sign Logical; whether the default `np_template` includes a `%`.
#'   Ignored when `np_template` is supplied. Default `TRUE`.
#' @param flex A function of one argument `function(ft) ...` that styles and returns
#'   a flextable, applied by `as_flextable()` on a `table1` result. If `NULL`,
#'   the SimtablR house theme (booktabs styling).
#' @return An object of class `"simtab_style"`.
#' @seealso [register_journal()], [list_journals()], [table1()]
#' @examples
#' s <- journal_style(count_header = "No. (%)", ci_sep = " to ", percent_sign = FALSE)
#' @export
journal_style <- function(
  np_template = NULL,
  count_header = "n (%)",
  digits_cont = 1,
  digits_est = 2,
  ci_sep = " - ",
  ci_parens = "()",
  est_template = NULL,
  pval_thresh = 0.001,
  pval_digits = 3,
  pval_upper = NULL,
  pval_leading_zero = TRUE,
  pval_adaptive = FALSE,
  percent_sign = TRUE,
  flex = NULL
) {
  if (is.null(np_template)) {
    np_template <- if (isTRUE(percent_sign)) "{n} ({p}%)" else "{n} ({p})"
  }
  if (is.null(est_template)) {
    est_template <- "{est} {lp}{lower}{sep}{upper}{rp}"
  }
  if (is.null(flex)) {
    flex <- simtab_theme
  }

  if (!is.character(np_template) || length(np_template) != 1) {
    simtab_abort_input(c(
      "{.arg np_template} must be a single string.",
      "i" = "Received an object of class {.cls {class(np_template)[[1]]}} of length {length(np_template)}.",
      "v" = "Use {.code np_template = \"{{n}} ({{p}}%)\"}."
    ))
  }
  if (!grepl("{n}", np_template, fixed = TRUE)) {
    # `{n}` is a template placeholder, not a cli field: double the braces so cli
    # prints it literally instead of trying to interpolate a variable named `n`.
    simtab_abort_input(c(
      "{.arg np_template} must contain the {.code {{n}}} placeholder.",
      "i" = "Received: {.val {np_template}}.",
      "v" = "Use {.code np_template = \"{{n}} ({{p}}%)\"}."
    ))
  }
  if (!is.character(ci_parens) || length(ci_parens) != 1 || nchar(ci_parens) != 2) {
    simtab_abort_input(c(
      "{.arg ci_parens} must be a two-character string.",
      "i" = "Received: {.val {ci_parens}}.",
      "v" = "Use {.val ()} or {.val []}."
    ))
  }
  for (nm in c("digits_cont", "digits_est", "pval_digits")) {
    v <- get(nm)
    if (!is.numeric(v) || length(v) != 1 || v < 0) {
      simtab_abort_input(c(
        "{.arg {nm}} must be a non-negative number.",
        "i" = "Received: {.val {v}}.",
        "v" = "Use a whole number of digits, e.g. {.code {nm} = 2}."
      ))
    }
  }
  if (!is.numeric(pval_thresh) || length(pval_thresh) != 1 || pval_thresh <= 0) {
    simtab_abort_input(c(
      "{.arg pval_thresh} must be a positive number.",
      "i" = "Received: {.val {pval_thresh}}.",
      "v" = "Use {.code pval_thresh = 0.001} to print smaller p-values as {.val <0.001}."
    ))
  }
  if (!is.null(pval_upper) &&
      (!is.numeric(pval_upper) || length(pval_upper) != 1 ||
       is.na(pval_upper) || pval_upper <= 0 || pval_upper >= 1)) {
    simtab_abort_input(c(
      "{.arg pval_upper} must be NULL or a number between 0 and 1.",
      "i" = "Received: {.val {pval_upper}}.",
      "v" = "Use {.code pval_upper = 0.99} to cap printed p-values, or {.code NULL} for no cap."
    ))
  }
  if (!is.logical(pval_leading_zero) || length(pval_leading_zero) != 1 || is.na(pval_leading_zero)) {
    simtab_abort_input(c(
      "{.arg pval_leading_zero} must be {.code TRUE} or {.code FALSE}.",
      "i" = "Received: {.val {pval_leading_zero}}.",
      "v" = "Use {.code FALSE} for the {.val .03} style favoured by some journals."
    ))
  }
  if (!is.logical(pval_adaptive) || length(pval_adaptive) != 1 || is.na(pval_adaptive)) {
    simtab_abort_input(c(
      "{.arg pval_adaptive} must be {.code TRUE} or {.code FALSE}.",
      "i" = "Received: {.val {pval_adaptive}}.",
      "v" = "Use {.code TRUE} to vary p-value digits with magnitude."
    ))
  }
  if (!is.function(flex)) {
    simtab_abort_input(c(
      "{.arg flex} must be a function of one argument returning a flextable, or NULL.",
      "i" = "Received an object of class {.cls {class(flex)[[1]]}}.",
      "v" = "Pass {.code flex = function(ft) flextable::theme_booktabs(ft)}."
    ))
  }

  structure(
    list(
      np_template = np_template,
      count_header = count_header,
      digits_cont = as.integer(digits_cont),
      digits_est = as.integer(digits_est),
      ci_sep = ci_sep,
      ci_parens = ci_parens,
      est_template = est_template,
      pval_thresh = pval_thresh,
      pval_digits = as.integer(pval_digits),
      pval_upper = pval_upper,
      pval_leading_zero = pval_leading_zero,
      pval_adaptive = pval_adaptive,
      percent_sign = isTRUE(percent_sign),
      flex = flex
    ),
    class = "simtab_style"
  )
}

#' @export
print.simtab_style <- function(x, ...) {
  cat("<simtab_style>\n")
  cat("  cell:    ", x$np_template, "   header: ", x$count_header, "\n", sep = "")
  cat("  CI:      {est} ", substr(x$ci_parens, 1, 1), "{lo}",
      x$ci_sep, "{hi}", substr(x$ci_parens, 2, 2),
      "   (", x$digits_est, " dp)\n", sep = "")
  cat("  p-value: <", x$pval_thresh, " else ", x$pval_digits, " dp\n", sep = "")
  invisible(x)
}

#' Register a named journal style preset
#'
#' Stores a `simtab_style` in SimtablR's preset registry under `name` so it can be
#' used as `style = name` in [table1()] and the `add_*()` helpers. Registering a
#' name that already exists overwrites it.
#'
#' @param name Character preset name (case-insensitive).
#' @param style A `simtab_style` object from [journal_style()].
#' @return Invisibly, `name`.
#' @seealso [journal_style()], [list_journals()]
#' @examples
#' register_journal("mylab", journal_style(count_header = "No. (%)", ci_sep = " to "))
#' list_journals()
#' @export
register_journal <- function(name, style) {
  if (!is.character(name) || length(name) != 1 || !nzchar(name)) {
    simtab_abort_input(c(
      "{.arg name} must be a non-empty string.",
      "i" = "The name is how the preset is looked up later, e.g. {.code style = \"mylab\"}.",
      "v" = "Pass a single name, e.g. {.code register_journal(\"mylab\", style)}."
    ))
  }
  if (!inherits(style, "simtab_style")) {
    simtab_abort_input(c(
      "{.arg style} must be a {.cls simtab_style} object.",
      "i" = "Received an object of class {.cls {class(style)[[1]]}}.",
      "v" = "Build one with {.fn journal_style}."
    ))
  }
  assign(tolower(name), style, envir = .simtab_style_registry)
  invisible(name)
}

#' List available journal style presets
#'
#' @return A character vector of registered preset names.
#' @seealso [journal_style()], [register_journal()]
#' @examples
#' list_journals()
#' @export
list_journals <- function() {
  sort(ls(envir = .simtab_style_registry))
}

#' Resolve a `style` value to a `simtab_style` spec
#'
#' Accepts an existing spec, a registered preset name, the shorthands `"n_pct"` /
#' `"pct_n"`, or a raw `{n}/{p}` template string.
#' @keywords internal
#' @noRd
.resolve_style <- function(style) {
  if (inherits(style, "simtab_style")) {
    return(style)
  }
  if (is.null(style)) {
    return(.lookup_preset("default"))
  }
  if (is.character(style) && length(style) == 1) {
    key <- tolower(style)
    if (exists(key, envir = .simtab_style_registry, inherits = FALSE)) {
      return(get(key, envir = .simtab_style_registry, inherits = FALSE))
    }
    if (key == "n_pct") {
      return(journal_style(np_template = "{n} ({p}%)"))
    }
    if (key == "pct_n") {
      return(journal_style(np_template = "{p}% ({n})"))
    }
    if (grepl("{n}", style, fixed = TRUE) || grepl("{p}", style, fixed = TRUE)) {
      return(journal_style(np_template = style))
    }
    simtab_abort_input(c(
      "Unknown style {.val {style}}.",
      "i" = "Registered presets: {.val {c('n_pct', 'pct_n', list_journals())}}.",
      "v" = "Use a registered name, or a {.code {{n}}}/{.code {{p}}} template such as
             {.code \"{{n}} ({{p}}%)\"}."
    ))
  }
  simtab_abort_input(c(
    "{.arg style} must be a string, a {.cls simtab_style} object, or a named
     per-variable list.",
    "i" = "Received an object of class {.cls {class(style)[[1]]}}.",
    "v" = "Use {.code style = \"default\"} or build one with {.fn journal_style}."
  ))
}

#' Resolve the journal preset attached to a result for render-edge formatting
#'
#' Returns the resolved `simtab_style` when `style` is a `simtab_style` object
#' or a registered preset name, and `NULL` otherwise (cell-template shorthands
#' such as `"n_pct"`, `"pct_n"` or `"{n} [{p}%]"`). Names in `legacy` also
#' return `NULL`, so engines whose unstyled output predates journal presets keep
#' their established formatting until a journal is explicitly applied.
#' @keywords internal
#' @noRd
.result_journal <- function(style, legacy = character(0)) {
  if (inherits(style, "simtab_style")) {
    return(style)
  }
  if (is.character(style) && length(style) == 1 && !is.na(style) && !style %in% legacy) {
    key <- tolower(style)
    if (exists(key, envir = .simtab_style_registry, inherits = FALSE)) {
      return(get(key, envir = .simtab_style_registry, inherits = FALSE))
    }
  }
  NULL
}

#' Retrieve style preset by name from internal registry with fallback
#' @keywords internal
#' @noRd
.lookup_preset <- function(name) {
  key <- tolower(name)
  if (exists(key, envir = .simtab_style_registry, inherits = FALSE)) {
    return(get(key, envir = .simtab_style_registry, inherits = FALSE))
  }
  # Fallback in case .onLoad seeding has not run (e.g. direct sourcing).
  journal_style()
}

#########
# NUMERIC FORMATTING AND CELL COMPOSITION
# Formats categorical cells, continuous summaries, confidence intervals, and p-values.

#' Compose categorical count and percentage display cell string
#' @keywords internal
#' @noRd
.fmt_np <- function(n, p, spec, d) {
  if (is.na(n)) {
    return("")
  }
  if (is.na(p)) {
    return(as.character(n))
  }
  p_str <- sprintf(paste0("%.", d, "f"), p)
  txt <- gsub("{n}", as.character(n), spec$np_template, fixed = TRUE)
  gsub("{p}", p_str, txt, fixed = TRUE)
}

#' Compose continuous summary statistics display cell string
#' @keywords internal
#' @noRd
.fmt_cont <- function(vals, stat, spec) {
  if (length(vals) == 0 || all(is.na(vals))) {
    return("-")
  }
  dc <- spec$digits_cont
  num <- function(v) .fmt_fixed(v, dc)
  if (stat == "mean") {
    paste0(num(vals[1]), " (", num(vals[2]), ")")
  } else {
    bounds <- c(num(vals[2]), num(vals[3]))
    paste0(num(vals[1]), " (", bounds[1], .ci_sep_safe(spec$ci_sep, bounds), bounds[2], ")")
  }
}

#' Choose an interval separator that cannot be misread
#'
#' A comma separator next to bounds written with a comma (decimal or grouping
#' mark) reads as more numbers, e.g. `1,94 (1,51, 2,49)`; a dash separator
#' next to a negative bound reads as a range, e.g. `(-0.57 - 3.44)`. Only
#' those ambiguous cases change: to `"; "` and `" to "` respectively.
#' @param sep The configured separator.
#' @param bounds Character vector of the already formatted bounds.
#' @keywords internal
#' @noRd
.ci_sep_safe <- function(sep, bounds) {
  bounds <- bounds[!is.na(bounds)]
  core <- trimws(sep)
  if (grepl(",", core, fixed = TRUE) && any(grepl(",", bounds, fixed = TRUE))) {
    return("; ")
  }
  if (core %in% c("-", intToUtf8(0x2013), intToUtf8(0x2014)) && any(startsWith(trimws(bounds), "-"))) {
    return(" to ")
  }
  sep
}

#' Compose effect estimate and confidence interval display string
#' @keywords internal
#' @noRd
.fmt_est <- function(estimate, lower, upper, spec, num = NULL) {
  if (is.na(estimate)) {
    return("-")
  }
  de <- spec$digits_est
  if (is.null(num)) {
    num <- function(v) .fmt_fixed(v, de)
  }
  lp <- substr(spec$ci_parens, 1, 1)
  rp <- substr(spec$ci_parens, 2, 2)
  txt <- spec$est_template
  bounds <- c(num(lower), num(upper))
  txt <- gsub("{est}", num(estimate), txt, fixed = TRUE)
  txt <- gsub("{lower}", bounds[1], txt, fixed = TRUE)
  txt <- gsub("{upper}", bounds[2], txt, fixed = TRUE)
  txt <- gsub("{sep}", .ci_sep_safe(spec$ci_sep, bounds), txt, fixed = TRUE)
  txt <- gsub("{lp}", lp, txt, fixed = TRUE)
  gsub("{rp}", rp, txt, fixed = TRUE)
}

#' Calculate decimal places required to display p-value threshold boundary
#' @keywords internal
#' @noRd
.p_bound_digits <- function(bound, digits) {
  digits <- as.integer(digits)
  if (!is.numeric(bound) || length(bound) != 1 || is.na(bound)) {
    return(digits)
  }
  for (d in seq(max(digits, 0L), 15L)) {
    if (isTRUE(all.equal(as.numeric(sprintf("%.*f", d, bound)), bound))) {
      return(d)
    }
  }
  digits
}

#' Format p-value string with journal threshold, digits, and leading-zero rules
#' @keywords internal
#' @noRd
.fmt_p <- function(p, spec) {
  if (is.na(p)) {
    return("")
  }
  fmt <- function(value, digits) {
    out <- sprintf(paste0("%.", digits, "f"), value)
    if (isFALSE(spec$pval_leading_zero)) {
      out <- sub("^0\\.", ".", out)
      out <- sub("^-0\\.", "-.", out)
    }
    out
  }
  if (!is.null(spec$pval_upper) && p > spec$pval_upper) {
    upper_digits <- if (isTRUE(spec$pval_adaptive) && spec$pval_upper > 0.01) {
      2L
    } else {
      spec$pval_digits
    }
    return(paste0(">", fmt(spec$pval_upper, .p_bound_digits(spec$pval_upper, upper_digits))))
  }
  if (p < spec$pval_thresh) {
    return(paste0("<", fmt(spec$pval_thresh, .p_bound_digits(spec$pval_thresh, spec$pval_digits))))
  }
  digits <- if (isTRUE(spec$pval_adaptive) && p > 0.01) {
    2L
  } else {
    spec$pval_digits
  }
  fmt(p, digits)
}

#########
# BUILT-IN JOURNAL STYLE PRESETS
# Flextable styling definitions and registry seeding for standard medical journals.

#' Apply NEJM-style serif typography and subtle shading to flextable
#' @keywords internal
#' @noRd
.nejm_flex <- function(ft) {
  .require_pkg("flextable")
  ft <- flextable::theme_booktabs(ft)
  ft <- flextable::font(ft, fontname = "Times New Roman", part = "all")
  ft <- flextable::fontsize(ft, size = 10, part = "all")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::bg(ft, bg = "#F2F2F2", part = "header")
  ft <- flextable::padding(ft, padding.top = 2, padding.bottom = 2, part = "all")
  ft <- flextable::align(ft, align = "center", part = "header")
  ft <- flextable::align(ft, j = 1, align = "left", part = "body")
  ft <- flextable::align(ft, j = -1, align = "center", part = "body")
  flextable::autofit(ft)
}

#' Seed built-in journal style presets into package registry
#' @keywords internal
#' @noRd
.seed_builtin_styles <- function() {
  register_journal(
    "default",
    journal_style(
      np_template = "{n} ({p}%)",
      count_header = "n (%)",
      ci_sep = " - ",
      ci_parens = "()",
      flex = simtab_theme
    )
  )
  register_journal(
    "nejm",
    journal_style(
      np_template = "{n} ({p})",
      count_header = "No. (%)",
      digits_cont = 1,
      digits_est = 2,
      ci_sep = "\u2013",
      ci_parens = "()",
      pval_thresh = 0.001,
      percent_sign = FALSE,
      flex = .nejm_flex
    )
  )
  register_journal(
    "jama",
    journal_style(
      np_template = "{n} ({p}%)",
      count_header = "No. (%)",
      digits_cont = 1,
      digits_est = 2,
      ci_sep = ", ",
      ci_parens = "()",
      pval_thresh = 0.001,
      pval_digits = 3,
      pval_upper = 0.99,
      pval_leading_zero = FALSE,
      pval_adaptive = TRUE,
      flex = .nejm_flex
    )
  )
  register_journal(
    "strobe-default",
    journal_style(
      np_template = "{n} ({p}%)",
      count_header = "n/N (%)",
      digits_cont = 1,
      digits_est = 2,
      ci_sep = ", ",
      ci_parens = "()",
      pval_thresh = 0.001,
      pval_digits = 3,
      pval_upper = 0.99,
      pval_leading_zero = FALSE,
      pval_adaptive = TRUE,
      flex = simtab_theme
    )
  )
  register_journal(
    "lancet",
    journal_style(
      np_template = "{n} ({p}%)",
      count_header = "n (%)",
      digits_cont = 1,
      digits_est = 2,
      ci_sep = ", ",
      ci_parens = "()",
      pval_thresh = 0.001,
      pval_digits = 3,
      pval_upper = 0.99,
      pval_leading_zero = FALSE,
      pval_adaptive = TRUE,
      flex = .nejm_flex
    )
  )
  invisible(TRUE)
}
