##' S3method VarCorr mmDML
VarCorr.mmdml <- function(x, ...) {
  x$varcor # here we call print.VarCorr.merMod
}

##' S3method vcov mmDML
vcov.mmdml <- function(object, ...) {
  object$vcov
}

##' S3method fixef mmDML
fixef.mmdml <- function(object, ...) {
  object$beta
}

##' S3method ranef mmDML
ranef.mmdml <- function(object, ...) {
  object$random_eff
}

##' S3method print mmdml (adapted from the lmer print method)
print.mmdml <- function(x, digits = max(3, getOption("digits") - 3),
                        ranef.comp = "Std.Dev.", ...) {

  .prt.methTit(x$methTitle, class(x)[1])
  .prt.VC(VarCorr(x), digits = digits, useScale = x$useScale,
          comp = ranef.comp, ...)
  .prt.grps(x$ngrps, nobs = x$nobs)

  if (length(cf <- fixef(x)) > 0) {
    cat("Fixed Effects:\n")
    print.default(format(cf, digits = digits),
                  print.gap = 2L, quote = FALSE, ...)
  } else cat("No fixed effect coefficients\n")

  fitMsgs <- x$fitMsgs
  if (any(nchar(fitMsgs) > 0)) {
    cat("fit warnings:\n"); writeLines(fitMsgs)
  }
  .prt.warn(x$optinfo, summary = TRUE)

  invisible(x)
}

##' S3method summary mmDML
summary.mmdml <- function(object,
                          correlation = (p <= getOption("lme4.summary.cor.max")),
                          nr_res = NULL, ...) {
  if (...length() > 0) {
    warning("additional arguments ignored")
  }

  nr_res <- if (is.null(nr_res)) {
    attr(object, "nr_res")
  } else {
    if (nr_res > attr(object, "nr_res")) {
      stop("choose smaller nr_res")
    } else {
      nr_res
    }
  }

  p <- length(coefs <- fixef(object))

  vc <- matrix(object$vcov@x, nrow = p, ncol = p)
  stdError <- sqrt(diag(vc))
  coefs <- cbind("Estimate" = coefs,
                 "Std. Error" = stdError)
  if (p > 0) {
    coefs <- cbind(coefs, (cf3 <- coefs[, 1] / coefs[, 2]), deparse.level = 0)
    colnames(coefs)[3] <- paste("z", "value")
    coefs <- cbind(coefs, "Pr(>|z|)" =
                     2 * pnorm(abs(cf3), lower.tail = FALSE))
  }

  structure(list(methTitle = object$methTitle,
                 objClass = class(object)[1],
                 ngrps = object$ngrps,
                 nobs = object$nobs,
                 coefficients = coefs,
                 sigma = object$sigma,
                 vcov = object$vcov,
                 varcor = object$varcor,
                 residuals = object$residuals,
                 fitMsgs = object$fitMsgs,
                 optinfo = object$optinfo,
                 nr_res = nr_res,
                 correlation = correlation, ...
  ), class = "summary.mmdml")
}

##' Adapted from lme4
.prt.resids.mmdml <- function (resids, digits, nr_res,
                               title = "Scaled residuals", ...) {
  title <- paste(title, " (nr_res = ", nr_res, "):", sep = "")
  cat(title, "\n")
  rq <- setNames(zapsmall(quantile(resids, na.rm = TRUE), digits +
                            1L), c("Min", "1Q", "Median", "3Q", "Max"))
  print(rq, digits = digits, ...)
  cat("\n")
}

##' S3method print summary.mmdml
print.summary.mmdml <- function(x, digits = max(3, getOption("digits") - 3),
                                correlation = NULL,
                                symbolic.cor = FALSE,
                                signif.stars = getOption("show.signif.stars"),
                                ranef.comp = c("Variance", "Std.Dev."),
                                show.resids = TRUE, ...) {
  .prt.methTit(x$methTitle, x$objClass)
  if (show.resids)
    ## need residuals.merMod() rather than residuals():
    ##  summary.merMod has no residuals method
    .prt.resids.mmdml(unlist(x$residuals), digits = digits, nr_res = x$nr_res)
  .prt.VC(x$varcor, digits = digits, useScale = x$useScale,
          comp = ranef.comp, ...)
  .prt.grps(x$ngrps, nobs = x$nobs)

  p <- nrow(x$coefficients)
  if (p > 0) {
    cat("\nFixed effects:\n")
    printCoefmat(x$coefficients,
                 digits = digits, signif.stars = signif.stars)
    ## do not show correlation when   summary(*, correlation=FALSE)  was used:
    hasCor <- !is.null(VC <- x$vcov) && !is.null(VC@factors$correlation)
    if (is.null(correlation)) { # default
      cor.max <- getOption("lme4.summary.cor.max")
      correlation <- hasCor && p <= cor.max
      if (!correlation && p > cor.max) {
        nam <- deparse(substitute(x))
        if (length(nam) > 1 || nchar(nam) >= 32) nam <- "...."
        message(sprintf(paste(
          "\nCorrelation matrix not shown by default, as p = %d > %d.",
          "Use print(%s, correlation=TRUE)  or",
          "    vcov(%s)         if you need it\n", sep = "\n"),
          p, cor.max, nam, nam))
      }
    } else if (!is.logical(correlation)) stop("'correlation' must be NULL or logical")
    if (correlation) {
      if (is.null(VC)) VC <- x$vcov
      corF <- VC@factors$correlation
      if (is.null(corF)) {
        message("\nCorrelation of fixed effects could have been required in summary()")
        corF <- cov2cor(VC)
      }
      p <- ncol(corF)
      if (p > 1) {
        rn <- rownames(x$coefficients)
        rns <- abbreviate(rn, minlength = 11)
        cat("\nCorrelation of Fixed Effects:\n")
        if (is.logical(symbolic.cor) && symbolic.cor) {
          corf <- as(corF, "matrix")
          dimnames(corf) <- list(rns,
                                 abbreviate(rn, minlength = 1, strict = TRUE))
          print(symnum(corf))
        } else {
          corf <- matrix(format(round(corF@x, 3), nsmall = 3),
                         ncol = p,
                         dimnames = list(rns, abbreviate(rn, minlength = 6)))
          corf[!lower.tri(corf)] <- ""
          print(corf[-1, -p, drop = FALSE], quote = FALSE)
        } ## !symbolic.cor
      }  ## if (p > 1)
    } ## if (correlation)
  } ## if (p>0)

  if (length(x$fitMsgs) && any(nchar(x$fitMsgs) > 0)) {
    cat("fit warnings:\n"); writeLines(x$fitMsgs)
  }
  .prt.warn(x$optinfo, summary = FALSE)
  invisible(x)
}

##' S3method residuals mmDML
residuals.mmdml <- function(object, scaled = FALSE, ...) {
  nres <- length(object$residuals)
  resid_return <- vector(mode = "list", length = nres)
  for (i in 1:nres) {
    resid_return[[i]] <- if (!scaled)
      object$residuals[[i]] * object$sigma
    else
      object$residuals[[i]]
  }

  resid_return
}

##' S3method sigma mmDML
sigma.mmdml <- function(object, ...) {
  object$sigma
}

##' Copy from lme4 file R/modular.R
.merMod.msgs <- function(x) {
  ## currently only those found with 'X' :
  aX <- attributes(x@pp$X)
  wmsgs <- grep("^msg", names(aX))
  if (any(has.msg <- nchar(Xwmsgs <- unlist(aX[wmsgs])) > 0))
    Xwmsgs[has.msg]
  else
    character()
}

##' S3method confint mmDML
confint.mmdml <- function(object, parm = NULL, level = 0.95, ...) {
  beta <- object$beta
  p <- length(beta)
  if (is.null(parm)) {
    parm <- seq(p)
  }
  alpha <- 1 - level
  z_value <- qnorm(1 - alpha / 2, 0, 1)
  sd <- sqrt(diag(matrix(object$vcov@x, nrow = p, ncol = p)))
  CI <- cbind(beta - z_value * sd,
              beta + z_value * sd)
  colnames(CI) <- c(paste((alpha / 2) * 100, "%", sep = ""),
                    paste((1 - alpha / 2) * 100, "%", sep = ""))
  rownames(CI) <- names(beta)
  CI[parm, , drop = FALSE]
}
