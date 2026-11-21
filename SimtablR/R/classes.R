# Constructors and validators for the SimtablR object spine.
# Implements the core architecture: simtab_spec -> simtab_result -> simtab_report.

#########
# DATA SOURCE AND SPECIFICATION
# Captures dataset references and constructs inert analysis specifications.

#' Captures dataset references and computes an integrity checksum hash
#' @keywords internal
#' @noRd
.capture_data_src <- function(data) {
  .check_data_frame(data)
  data <- .as_simtab_data(data)

  ref <- new.env(parent = emptyenv())
  ref$data <- data

  structure(
    list(
      ref = ref,
      hash = .hash_data(data),
      nrow = nrow(data),
      ncol = ncol(data),
      names = names(data)
    ),
    class = "simtab_data_src"
  )
}

#' Computes xxhash64 digest of a dataset for reproducibility tracking
#' @keywords internal
#' @noRd
.hash_data <- function(data) {
  digest::digest(data, algo = "xxhash64")
}

#' Returns the default role mapping template for an analysis specification
#' @keywords internal
#' @noRd
.default_spec_roles <- function() {
  list(
    describe = NULL,
    by = NULL,
    adjust = NULL,
    time = NULL,
    event = NULL,
    offset = NULL,
    test = NULL,
    ref_std = NULL
  )
}

#' Low-level constructor for inert simtab_spec objects
#' @keywords internal
#' @noRd
new_simtab_spec <- function(
  data_src,
  engine = NULL,
  roles = .default_spec_roles(),
  summary = list(default = "auto", overrides = list()),
  comparison = list(test = NULL),
  effect = list(
    measure = NULL,
    estimator = NULL,
    ref = NULL,
    conf.level = 0.95,
    resolved_from = NULL
  ),
  design = NULL,
  missing = list(display = TRUE, denominator = "available", model_na = "drop"),
  layout = list(overall = TRUE, order = NULL, binds = list(), pct = "total"),
  style = "default",
  fmt = list(d = 1, labels = NULL, conf_pct = 95),
  engine_opts = list(),
  accum = list(footnotes = character(), patches = list(), sections = list()),
  call = NULL
) {
  structure(
    list(
      data_src = data_src,
      engine = engine,
      roles = roles,
      summary = summary,
      comparison = comparison,
      effect = effect,
      design = design,
      missing = missing,
      layout = layout,
      style = style,
      fmt = fmt,
      engine_opts = engine_opts,
      accum = accum,
      call = call
    ),
    class = "simtab_spec"
  )
}

#' Validates structural slots and schema conformance of a simtab_spec object
#' @keywords internal
#' @noRd
validate_simtab_spec <- function(x) {
  if (!inherits(x, "simtab_spec")) {
    simtab_abort_spec(c(
      "{.arg x} must be a {.cls simtab_spec}.",
      "i" = "Received an object of class {.cls {class(x)[[1]]}}.",
      "v" = "Start a specification with {.code simtab(data)}."
    ))
  }

  required <- c(
    "data_src", "engine", "roles", "summary", "comparison", "effect",
    "design", "missing", "layout", "style", "fmt", "engine_opts", "accum", "call"
  )
  if (!identical(names(x), required)) {
    simtab_abort_spec(c(
      "Malformed {.cls simtab_spec}: slot names do not match the v3 schema.",
      "i" = "Expected: {.val {required}}.",
      "v" = "Rebuild the specification with {.code simtab()} rather than editing its slots."
    ))
  }

  x
}

#########
# COMPUTED RESULT OBJECTS
# Constructs and validates evaluated analysis evidence containers.

#' Low-level constructor for evaluated simtab_result evidence objects
#' @keywords internal
#' @noRd
new_simtab_result <- function(
  spec,
  data = list(),
  meta = list(),
  used = NULL,
  advice = list(),
  call = NULL,
  subclass = NULL
) {
  spec <- validate_simtab_spec(spec)
  if (is.null(used)) {
    used <- spec$data_src
  }
  class <- c(subclass, "simtab_result", "simtab")

  structure(
    list(
      spec = spec,
      data = data,
      meta = meta,
      used = used,
      advice = advice,
      call = call
    ),
    class = unique(class[!is.na(class) & nzchar(class)])
  )
}

#' Validates structural integrity and slot schema of a simtab_result object
#' @keywords internal
#' @noRd
validate_simtab_result <- function(x) {
  if (!inherits(x, "simtab_result")) {
    simtab_abort_spec(c(
      "{.arg x} must be a {.cls simtab_result}.",
      "i" = "Received an object of class {.cls {class(x)[[1]]}}.",
      "v" = "Compute a specification first, e.g. {.code evaluate(spec)}."
    ))
  }

  required <- c("spec", "data", "meta", "used", "advice", "call")
  if (!identical(names(x), required)) {
    simtab_abort_spec(c(
      "Malformed {.cls simtab_result}: slot names do not match the v3 schema.",
      "i" = "Expected: {.val {required}}.",
      "v" = "Recompute the result rather than editing its slots."
    ))
  }

  x
}

#########
# COMPOSED REPORT OBJECTS
# Multi-table reporting containers and accessors.

#' Low-level constructor for multi-table simtab_report objects
#' @keywords internal
#' @noRd
new_simtab_report <- function(items = list(), methods = NULL, used = NULL, advice = NULL) {
  if (!is.list(items)) {
    simtab_abort_spec(c(
      "{.arg items} must be a named list of {.cls simtab_result} objects.",
      "i" = "Received an object of class {.cls {class(items)[[1]]}}.",
      "v" = "Pass {.code list(\"Table 1\" = t1, \"Table 2\" = t2)}."
    ))
  }
  if (length(items) > 0 && (is.null(names(items)) || any(!nzchar(names(items))))) {
    simtab_abort_spec(c(
      "{.arg items} must be a named list of {.cls simtab_result} objects.",
      "i" = "Every element needs a name; the names become the report's table titles.",
      "v" = "Pass {.code list(\"Table 1\" = t1, \"Table 2\" = t2)}."
    ))
  }
  bad <- vapply(items, function(x) !inherits(x, "simtab_result"), logical(1))
  if (any(bad)) {
    simtab_abort_spec(c(
      "All report {.arg items} must be {.cls simtab_result} objects.",
      "i" = "Not a result: {.val {names(items)[bad] %||% which(bad)}}.",
      "v" = "Compute each specification before composing the report."
    ))
  }
  if (is.null(used) && length(items) > 0) {
    used <- items[[1]]$used
  }
  if (!is.null(used)) {
    items <- .share_report_data_ref(items, used)
  }
  if (is.null(methods)) {
    methods <- .report_methods(items)
  }
  if (is.null(advice)) {
    advice <- .report_advice_from_items(items)
  }

  structure(
    list(
      items = items,
      methods = methods,
      used = used,
      advice = advice
    ),
    class = c("simtab_report", "simtab")
  )
}

#' Validates structural schema conformance of a simtab_report object
#' @keywords internal
#' @noRd
validate_simtab_report <- function(x) {
  if (!inherits(x, "simtab_report")) {
    simtab_abort_spec(c(
      "{.arg x} must be a {.cls simtab_report}.",
      "i" = "Received an object of class {.cls {class(x)[[1]]}}.",
      "v" = "Compose one with {.fn simtablr}."
    ))
  }

  required <- c("items", "methods", "used", "advice")
  if (!identical(names(x), required)) {
    simtab_abort_spec(c(
      "Malformed {.cls simtab_report}: slot names do not match the v3 schema.",
      "i" = "Expected: {.val {required}}.",
      "v" = "Rebuild the report rather than editing its slots."
    ))
  }

  x
}

#' @export
`$.simtab_report` <- function(x, name) {
  raw <- unclass(x)
  if (name %in% names(raw)) {
    return(raw[[name]])
  }
  items <- raw$items
  if (name %in% names(items)) {
    return(items[[name]])
  }
  NULL
}

#' @export
`[[.simtab_report` <- function(x, i, ..., exact = TRUE) {
  raw <- unclass(x)
  items <- raw$items
  if (is.character(i) && length(i) == 1) {
    if (i %in% names(raw)) {
      return(raw[[i]])
    }
    if (i %in% names(items)) {
      return(items[[i]])
    }
  }
  if (is.numeric(i) && length(i) == 1 && !is.na(i)) {
    return(items[[i]])
  }
  raw[[i, exact = exact]]
}

#' Shares primary data reference environment across bundled report items
#' @keywords internal
#' @noRd
.share_report_data_ref <- function(items, used) {
  lapply(items, function(item) {
    item$used <- used
    item$spec$data_src <- used
    item
  })
}

#' Aggregates methods sentences across individual report items
#' @keywords internal
#' @noRd
.report_methods <- function(items) {
  methods <- vapply(items, function(item) {
    tryCatch(
      as_methods(item),
      simtab_error_render = function(e) ""
    )
  }, character(1))
  methods <- unique(methods[nzchar(methods)])
  paste(methods, collapse = " ")
}

#' Collects and deduplicates advice entries across report items
#' @keywords internal
#' @noRd
.report_advice_from_items <- function(items) {
  advice <- list()
  for (item in items) {
    item_advice <- item$advice %||% list()
    advice <- c(advice, item_advice)
  }
  .dedupe_advice(advice)
}

#' @export
print.simtab_report <- function(x, details = FALSE, ...) {
  x <- validate_simtab_report(x)
  details <- .validate_print_details(details)
  cat("<simtab_report>\n")
  cat("  items: ", length(x$items), "\n", sep = "")
  cat("  data hash: ", substr(x$used$hash %||% "", 1, 12), "\n", sep = "")
  if (!is.null(x$methods) && nzchar(x$methods)) {
    cat("  methods: recorded\n")
  }
  for (nm in names(x$items)) {
    cat("\n", nm, "\n", sep = "")
    item <- x$items[[nm]]
    item$advice <- list()
    if (inherits(item, "simtab_table1") || inherits(item, "simtab_regtab")) {
      print(item, details = details)
    } else {
      print(item)
    }
  }
  .print_advice(x)
  invisible(x)
}
