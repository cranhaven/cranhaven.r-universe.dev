## documented below
extract.opsr <- function(model, beside = FALSE, include.structural = TRUE,
                         include.selection = TRUE, include.outcome = TRUE,
                         include.pseudoR2 = FALSE, include.R2 = FALSE,
                         repeat.gofs = FALSE, ...) {
  ## unpack some stuff
  s <- summary(model, ...)
  str.names <- names(coef(model, component = "structural"))
  sel.names <- names(coef(model, component = "selection"))
  out.names <- names(coef(model, component = "outcome"))

  str.co <- s$coef_table[str.names, ]
  sel.co <- s$coef_table[sel.names, ]
  out.co <- s$coef_table[out.names, ]

  ## prepare gofs
  gof <- numeric()
  gof.names <- character()
  gof.decimal <- logical()

  aic <- AIC(model)
  gof <- c(gof, aic)
  gof.names <- c(gof.names, "AIC")
  gof.decimal <- c(gof.decimal, TRUE)

  bic <- BIC(model)
  gof <- c(gof, bic)
  gof.names <- c(gof.names, "BIC")
  gof.decimal <- c(gof.decimal, TRUE)

  lik <- stats::logLik(model)
  gof <- c(gof, lik)
  gof.names <- c(gof.names, "Log Likelihood")
  gof.decimal <- c(gof.decimal, TRUE)

  if (include.pseudoR2) {
    pseudoR2el <- s$GOFcomponents$pseudoR2el
    pseudoR2ms <- s$GOFcomponents$pseudoR2ms
    gof <- c(gof, pseudoR2el, pseudoR2ms)
    gof.names <- c(gof.names, "Pseudo R$^2$ (EL)", "Pseudo R$^2$ (MS)")
    gof.decimal <- c(gof.decimal, TRUE, TRUE)
  }

  if (include.R2) {
    R2 <- s$GOFcomponents$R2
    for (r2 in R2) {
      gof <- c(gof, r2)
    }
    gof.names <- c(gof.names, "R$^2$ (total)")
    for (i in seq_len(s$nReg)) {
      gof.names <- c(gof.names, paste0("R$^2$ (", i, ")"))
    }
    gof.decimal <- c(gof.decimal, rep(TRUE, length(R2)))
  }

  n <- stats::nobs(model)
  gof <- c(gof, n)
  gof.names <- c(gof.names, "Num. obs.")
  gof.decimal <- c(gof.decimal, FALSE)

  if (beside) {
    trList <- list()
    str.se <- str.co[, "Std. error"]
    str.pval <- str.co[, "Pr(> t)"]
    str.co <- str.co[, "Estimate"]
    sel.names <- gsub("^s_", "", sel.names)
    sel.se <- sel.co[, "Std. error"]
    sel.pval <- sel.co[, "Pr(> t)"]
    sel.co <- sel.co[, "Estimate"]

    out.all <- lapply(seq_len(model$nReg), function(i) {
      out <- list()
      pattern <- paste0("^o", i, "_")
      idx <- grepl(pattern, out.names)
      out.names.i <- out.names[idx]
      out.co.i <- out.co[idx, ]
      out$out.names <- gsub(pattern, "", out.names.i)
      out$out.se <- out.co.i[, "Std. error"]
      out$out.pval <- out.co.i[, "Pr(> t)"]
      out$out.co <- out.co.i[, "Estimate"]
      out
    })
    if (include.structural) {
      tr <- texreg::createTexreg(coef.names = str.names, coef = str.co, se = str.se,
                                 pvalues = str.pval, gof.names = gof.names, gof = gof,
                                 gof.decimal = gof.decimal, model.name = "Structural")
      trList[[length(trList) + 1]] <- tr
    }
    if (include.selection) {
      tr <- texreg::createTexreg(coef.names = sel.names, coef = sel.co, se = sel.se,
                                 pvalues = sel.pval, gof.names = gof.names, gof = gof,
                                 gof.decimal = gof.decimal, model.name = "Selection")
      trList[[length(trList) + 1]] <- tr
    }
    if (include.outcome) {
      for (i in seq_along(out.all)) {
        out.i <- out.all[[i]]
        tr <- texreg::createTexreg(coef.names = out.i$out.names, coef = out.i$out.co,
                                   se = out.i$out.se, pvalues = out.i$out.pval,
                                   gof.names = gof.names, gof = gof,
                                   gof.decimal = gof.decimal,
                                   model.name = paste0("Outcome ", i))
        trList[[length(trList) + 1]] <- tr
      }
    }
    if (!repeat.gofs) {
      tmp <- trList[[1]]@gof
      trList <- lapply(trList, function(x) {
        x@gof <- NA_real_
        x
      })
      trList[[1]]@gof <- tmp
    }
    return(trList)

  } else {  # beside == FALSE
    coef.block <- data.frame()
    if (include.structural) {
      coef.block <- rbind(coef.block, str.co)
    }
    if (include.selection) {
      coef.block <- rbind(coef.block, sel.co)
    }
    if (include.outcome) {
      coef.block <- rbind(coef.block, out.co)
    }
    names <- rownames(coef.block)
    co <- coef.block[, "Estimate"]
    se <- coef.block[, "Std. error"]
    pval <- coef.block[, "Pr(> t)"]

    ## pass to texreg
    tr <- texreg::createTexreg(coef.names = names, coef = co, se = se,
                               pvalues = pval, gof.names = gof.names,
                               gof = gof, gof.decimal = gof.decimal
    )
    return(tr)
  }
}

#' Extract Method for OPSR Model Fits
#'
#' This is the main method called when using functions from the `texreg-package`.
#'
#' @rdname extract.opsr
#' @aliases extract.opsr
#'
#' @param model an object of class `"opsr"`.
#' @param beside if `TRUE`, prints structural, selection and outcome coefficients side-by-side.
#' @param include.structural whether or not structural coefficients should be printed.
#' @param include.selection whether or not selection coefficients should be printed.
#' @param include.outcome whether or not outcome coefficients should be printed.
#' @param include.pseudoR2 whether or not the pseudo R2 statistic for the selection
#'   component should be printed. See also the 'Details' section.
#' @param include.R2 whether or not the R2 statistic for the outcome components
#'   should be printed.
#' @param repeat.gofs if `beside = TRUE` whether or not to repeat the gofs.
#' @param ... additional arguments passed to [`summary.opsr`].
#'
#' @return A `texreg-class` object representing the statistical model.
#' @method extract opsr
#' @seealso `texreg-package`, [`texreg::texreg`], [`texreg::screenreg`] and related functions.
#'
#' @details
#' The `extract` method is called internally. Higher-level functions from the
#' `texreg-package` pass arguments via `...` to `extract`.
#'
#' `include.pseudoR2` reports both the "equally likely" (EL) and "market share" (MS)
#' pseudo R2.
#'
#' @example R/examples/ex-texreg.R
methods::setMethod("extract", signature = className("opsr", "OPSR"),
                   definition = extract.opsr)

methods::setMethod("extract", signature = className("opsr.null", "OPSR"),
                   definition = extract.opsr)
