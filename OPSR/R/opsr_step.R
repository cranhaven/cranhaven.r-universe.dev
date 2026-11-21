#' Step Function for OPSR Model Fits
#'
#' Excludes all coefficients with p-values below `pval` and fits again.
#'
#' @param object an object of class `"opsr"`.
#' @param pval coefficients with p-values < `pval` are dropped.
#' @param log environment to keep track of changes to `object` (in particular
#'   variables being eliminated). This could be useful in a function that
#'   repeatedly calls `opsr_step` internally in order to keep track of the full
#'   elimination history.
#' @param .step used to generate identifier in `log` environment.
#' @param ... additional arguments passed to [`update`] (and hence [`opsr`]).
#'
#' @return An object of class `"opsr"`.
#' @example R/examples/ex-opsr_step.R
#' @export
opsr_step <- function(object, pval, log = new.env(), .step = 1, ...) {
  partial_match <- function(x, y) {
    sapply(x, function(v) sum(grepl(v, y, fixed = TRUE)))
  }

  vars_from_f <- function(formula, rhs) {
    attr(stats::terms(stats::formula(formula, rhs = rhs)), "term.labels")
  }

  get_factors <- function(object) {
    mf <- model.frame(object)
    fcts <- sapply(mf, is.factor)
    fcts <- names(fcts)[fcts]
    sapply(fcts, function(x) length(levels(mf[[x]])))
  }

  get_terms2drop <- function(object, pval) {
    coef_table <- summary(object)$coef_table
    candi <- row.names(coef_table[coef_table[["Pr(> t)"]] > pval, ])
    candi <- candi[grepl("^s_|^o[0-9]_", candi)]
    nm <- sub("^((s|o[0-9])).*", "\\1", candi)
    candi <- sub("^s_|^o[0-9]_", "", candi)
    names(candi) <- nm

    fcts <- get_factors(object)

    reg <- c("s", paste0("o", 1:object$nReg))
    terms2drop <- lapply(seq_along(reg), function(i) {
      vars. <- vars_from_f(object$formula, rhs = i)
      candi. <- candi[nm == reg[i]]
      if (length(candi.) == 0) {
        return(NULL)
      }
      ## identify elems in vars. that are partial matches in candi.
      pm <- partial_match(vars., candi.)
      ## if any factor level is significant => don't drop the term
      ## check for factors whether all levels (n-1) are in candi.
      pm. <- sapply(seq_along(pm), function(i) {
        x <- pm[i]
        nx <- names(x)
        if (names(pm)[i] %in% names(fcts)) {
          if (fcts[nx]-1 != x) {  # don't drop it
            x[1] <- 0
          }
        }
        out <- x
        out
      })

      names(pm.)[pm > 0]
    })
    names(terms2drop) <- reg
    terms2drop
  }

  update_formula <- function(object, terms2drop) {
    form <- object$formula
    for (i in seq_along(terms2drop)) {
      if (is.null(terms2drop[[i]]))
        next
      rhs <- paste(terms2drop[[i]], collapse = " + ")
      u <- paste0(". -(", rhs, ")")
      fs <- paste0(". ~ ", paste(rep(". | ", i-1), collapse = ""), u)
      fu <- stats::as.formula(fs)
      form <- update(form, fu)
    }
    form
  }

  t2d <- get_terms2drop(object, pval)
  fu <- update_formula(object, t2d)

  ## keep track
  if (!is.null(log)) {
    key <- paste0("step.", .step)
    to.log <- list()
    to.log$call <- match.call()
    to.log$formula <- fu
    to.log$pval <- pval
    to.log$eliminate <- t2d
    assign(key, to.log, envir = log)
  }

  ## don't fit the same model again...
  if (all(sapply(t2d, is.null))) {
    return(object)
  }

  fit <- update(object, fu, ...)
  fit
}
