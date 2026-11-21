# Terse Formatting Flags
#
# Terse interactive flags (row, col, cell, or, pr, rr, p, miss) parsed from
# bare dots or programmatic vectors and mapped to specification mutations.

#########
# FLAG REGISTRY AND ACTION DISPATCH
# Terse flags registry, aliases, synonyms, and spec mutation dispatch.

#' Generate roxygen documentation section for supported terse flags
#' @keywords internal
#' @noRd
.flag_roxygen_section <- function(surface = c("tb", "table1")) {
  surface <- match.arg(surface)

  intro <- if (identical(surface, "tb")) {
    paste0(
      "Flags may be supplied as bare words in `...` alongside the selected ",
      "variables, or programmatically with `flags =`."
    )
  } else {
    paste0(
      "Flags may be supplied as bare words in `...` after `by`, or ",
      "programmatically with `flags =`. Anything else in `...` is treated as ",
      "a typo."
    )
  }

  pct <- if (identical(surface, "tb")) {
    c(
      "\\item{`row`}{Row percentages.}",
      "\\item{`col`}{Column percentages.}",
      "\\item{`cell`, `perc`}{Percentages of the table total. `perc` reads better in one-variable tables.}"
    )
  } else {
    c(
      "\\item{`col`}{Column percentages (the default).}",
      "\\item{`row`, `cell`, `perc`}{Not yet supported by `table1()`.}"
    )
  }

  c(
    "@section Terse flags:",
    intro,
    "\\describe{",
    pct,
    "\\item{`pr`, `rr`, `or`}{Add the corresponding crude effect measure.}",
    "\\item{`p`}{Add a p-value from an automatically chosen test, like `test = TRUE`.}",
    "\\item{`miss`}{Show missing values.}",
    "}",
    "The old flags `rp` and `m` still work as aliases for `pr` and `miss` but are deprecated."
  )
}

#' Return registry list of canonical flag action functions
#' @keywords internal
#' @noRd
.simtab_flags <- function() {
  list(
    row = function(spec) .set_pct_direction(spec, "row"),
    col = function(spec) .set_pct_direction(spec, "col"),
    cell = function(spec) .set_pct_direction(spec, "total"),
    or = function(spec) measure(spec, "OR"),
    pr = function(spec) measure(spec, "PR"),
    rr = function(spec) measure(spec, "RR"),
    p = function(spec) {
      .p_flag_note()
      test(spec, "auto")
    },
    miss = function(spec) missingness(spec, display = TRUE)
  )
}

#' Return named character vector of deprecated flag aliases
#' @keywords internal
#' @noRd
.simtab_flag_aliases <- function() {
  c(rp = "pr", m = "miss")
}

#' Return named character vector of valid flag synonyms
#' @keywords internal
#' @noRd
.simtab_flag_synonyms <- function() {
  c(perc = "cell")
}

#' Return character vector of canonical flag names
#' @keywords internal
#' @noRd
.canonical_flag_tokens <- function() {
  names(.simtab_flags())
}

#' Return character vector of all reserved flag tokens
#' @keywords internal
#' @noRd
.reserved_flag_tokens <- function() {
  c(.canonical_flag_tokens(), names(.simtab_flag_aliases()), names(.simtab_flag_synonyms()))
}

#' Set percentage calculation direction margin on specification layout
#' @keywords internal
#' @noRd
.set_pct_direction <- function(spec, direction) {
  spec <- validate_simtab_spec(spec)
  if (!is.character(direction) || length(direction) != 1 ||
      !direction %in% c("row", "col", "total")) {
    simtab_abort_flag(c(
      "{.arg direction} must be one of {.val row}, {.val col}, or {.val total}.",
      "i" = "Received: {.val {direction}}.",
      "v" = "Pick the margin the percentages should sum to."
    ))
  }
  spec$layout$pct <- direction
  spec
}

#' Emit one-time deprecation warning for obsolete flag alias
#' @keywords internal
#' @noRd
.flag_deprecate <- function(old, new) {
  rlang::warn(
    sprintf("`%s` is deprecated; use `%s` instead.", old, new),
    .frequency = "once",
    .frequency_id = paste0("simtab_flag_", old, "_", new)
  )
}

#' Emit one-time informational note on p-value flag behavior
#' @keywords internal
#' @noRd
.p_flag_note <- function() {
  if (isTRUE(getOption("simtab.p_flag_note_shown", FALSE))) {
    return(invisible(NULL))
  }
  options(simtab.p_flag_note_shown = TRUE)
  cli::cli_inform("`p` now adds the p-value column (SimtablR 3.0); use `perc` or `cell` for total percentages.")
  invisible(NULL)
}

#########
# FLAG PARSING AND INTERACTIVE DOT SCANNING
# Token validation, normalization, and bare dot-argument extraction.

#' Normalize single flag token resolving aliases and synonyms
#' @keywords internal
#' @noRd
.normalise_flag_token <- function(flag) {
  if (!is.character(flag) || length(flag) != 1 || !nzchar(flag)) {
    simtab_abort_flag(c(
      "Flags must be non-empty strings.",
      "i" = "Received an object of class {.cls {class(flag)[[1]]}} of length {length(flag)}.",
      "v" = "Pass bare tokens such as {.code col} or {.code p}."
    ))
  }

  aliases <- .simtab_flag_aliases()
  if (flag %in% names(aliases)) {
    .flag_deprecate(flag, aliases[[flag]])
    return(unname(aliases[[flag]]))
  }
  synonyms <- .simtab_flag_synonyms()
  if (flag %in% names(synonyms)) {
    return(unname(synonyms[[flag]]))
  }
  flag
}

#' Normalize vector of flag tokens to canonical names
#' @keywords internal
#' @noRd
.canonicalise_flag_tokens <- function(flags) {
  aliases <- .simtab_flag_aliases()
  synonyms <- .simtab_flag_synonyms()
  vapply(as.character(flags), function(flag) {
    if (flag %in% names(aliases)) {
      return(unname(aliases[[flag]]))
    }
    if (flag %in% names(synonyms)) {
      return(unname(synonyms[[flag]]))
    }
    flag
  }, character(1), USE.NAMES = FALSE)
}

#' Validate vector of supplied flag tokens
#' @keywords internal
#' @noRd
.validate_flag_vector <- function(flags) {
  if (is.null(flags)) {
    return(invisible(NULL))
  }
  if (!is.character(flags) || anyNA(flags) || any(!nzchar(flags))) {
    simtab_abort_flag(c(
      "{.arg flags} must be a character vector of non-empty flag names.",
      "i" = "Lists, missing values, empty strings, and other types are not flag tokens.",
      "v" = sprintf("Use one or more of: %s.", paste(.canonical_flag_tokens(), collapse = ", "))
    ))
  }
  invisible(NULL)
}

#' Apply sequence of flag mutations to analysis specification
#' @keywords internal
#' @noRd
.apply_flags <- function(spec, flags) {
  spec <- validate_simtab_spec(spec)
  if (is.null(flags)) {
    return(spec)
  }
  .validate_flag_vector(flags)

  table <- .simtab_flags()
  for (flag in flags) {
    flag <- .normalise_flag_token(flag)
    if (!flag %in% names(table)) {
      simtab_abort_flag(c(
        sprintf("Unknown formatting flag {.val %s}.", flag),
        "i" = "Flags are terse display/effect tokens shared by tb() and table1().",
        "v" = sprintf("Use one of: %s.", paste(.canonical_flag_tokens(), collapse = ", "))
      ))
    }
    spec <- table[[flag]](spec)
  }
  spec
}

#' Test whether expression is a call to simtablr_guidance
#' @keywords internal
#' @noRd
.is_guidance_call <- function(expr) {
  if (!is.call(expr)) {
    return(FALSE)
  }
  fn <- expr[[1L]]
  if (is.symbol(fn)) {
    return(identical(as.character(fn), "simtablr_guidance"))
  }
  is.call(fn) &&
    length(fn) >= 3L &&
    identical(as.character(fn[[1L]]), "::") &&
    identical(as.character(fn[[3L]]), "simtablr_guidance")
}

#' Raise informative error when simtablr_guidance is passed inside dot arguments
#' @keywords internal
#' @noRd
.abort_misplaced_guidance <- function(expr, surface) {
  call_text <- paste(deparse(expr, width.cutoff = 500L), collapse = " ")
  simtab_abort_flag(c(
    sprintf(
      "{.fn simtablr_guidance} is a separate display setting, not a {.fn %s} argument.",
      surface
    ),
    "i" = "Guidance is read when a stored result is printed; the analysis does not need to be recomputed.",
    "v" = sprintf("Run {.code %s} separately before printing the result.", call_text)
  ))
}

#' Separate bare flag tokens from variable selections in dot arguments
#' @keywords internal
#' @noRd
.scan_dot_flags <- function(data, dot_exprs, surface = "tb") {
  flag_tokens <- character()
  value_exprs <- list()
  reserved <- .reserved_flag_tokens()
  dot_names <- names(dot_exprs) %||% rep("", length(dot_exprs))

  for (i in seq_along(dot_exprs)) {
    expr <- dot_exprs[[i]]
    dot_name <- dot_names[[i]]
    if (nzchar(dot_name)) {
      simtab_abort_flag(c(
        sprintf("Named {.code ...} argument {.arg %s} is not supported by {.fn %s}.", dot_name, surface),
        "i" = "Names on selection expressions can be silently misleading.",
        "v" = "Pass variables unnamed in {.code ...}, and use documented named arguments for options."
      ))
    }
    if (.is_guidance_call(expr)) {
      .abort_misplaced_guidance(expr, surface)
    }
    if (is.symbol(expr)) {
      sym_name <- as.character(expr)
      if (sym_name %in% reserved) {
        if (is.data.frame(data) && sym_name %in% names(data)) {
          simtab_abort_flag(c(
            sprintf("Can't use {.val %s} as a bare variable in {.fn %s}.", sym_name, surface),
            setNames(sprintf("{.val %s} is a reserved formatting flag and is also a column name.", sym_name), "i"),
            setNames(sprintf("Select the column explicitly: {.code %s(data, \"%s\", ...)}.", surface, sym_name), "v"),
            setNames(sprintf("Use the flag deliberately: {.code flags = \"%s\"}.", sym_name), "v")
          ))
        }

        flag_tokens <- c(flag_tokens, sym_name)
        next
      }
    }

    value_exprs[[length(value_exprs) + 1]] <- expr
  }

  list(flags = flag_tokens, values = value_exprs)
}
