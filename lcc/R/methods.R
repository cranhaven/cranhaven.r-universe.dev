#######################################################################
#                                                                     #
# Package: lcc                                                        #
#                                                                     #
# File: lcc.R                                                         #
# Contains: generic methods                                           #
#                                                                     #
# Written by Thiago de Paula Oliveira                                 #
# copyright (c) 2017-18, Thiago P. Oliveira                           #
#                                                                     #
# First version: 30/10/2019                                           #
# Last update: 01/11/2019                                             #
# License: GNU General Public License version 2 (June, 1991) or later #
#                                                                     #
#######################################################################

#=======================================================================
# is.lcc function
#=======================================================================
##' Reports whether x is a lcc object
##' @rdname is.lcc
##' @param x An object to test
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@alumni.usp.br}
##' @keywords internal
##' @return Returns if the object belongs to lcc class
##' @export
is.lcc <- function(x) inherits(x, "lcc")

#=======================================================================
# Print method
#=======================================================================
##' @rdname print.lcc
##' @method print lcc
##' @title Print an \code{lcc} Object
##' @usage \method{print}{lcc}(x, digits, ...)
##' @aliases print.lcc
##' @description Prints information about the longitudinal concordance
##'   correlation represented by an object of class
##'   \code{\link[lcc]{lcc}}. The returned object has a
##'   \code{\link[base]{print}} method.
##'
##' @return an object inheriting from class \code{print.lcc}.
##'
##' @param x an object inheriting from class
##'   \code{\link[lcc]{lcc}}, representing a fitted longitudinal
##'   concordance correlation function.
##'
##' @param digits a non-null value for \code{digits} specifies the minimum
##'   number of significant digits to be printed in values. The
##'   default, \code{NULL}.
##'
##' @param ... further arguments passed to \code{{\link{print}}}.
##' @return No return value, called for side effects
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@alumni.usp.br}
##' @seealso \code{\link[lcc]{lcc}}, \code{\link[lcc]{summary.lcc}}
##' @examples
##' \dontrun{
##' ## Second degree polynomial model with random intercept, slope and
##' ## quadratic term
##' fm1<-lcc(data = hue, subject = "Fruit", resp = "H_mean",
##'          method = "Method", time = "Time", qf = 2, qr = 2)
##' print(fm1)
##' }
##'
##' @importFrom stats AIC BIC
##' @export

print.lcc <- function(x, digits = NULL, ...){
  message("Longitudinal concordance correlation model fit by ")
  message( if(x[1]$model$method == "REML") "REML\n" else "maximum likelihood\n")
  AIC <- AIC(x[1]$model)
  BIC <- BIC(x[1]$model)
  logLik <- c(x[1]$model$logLik)
  print(data.frame(AIC, BIC, logLik, row.names = " "), digits = digits, ...)
  message("\n")
  print(x$Summary.lcc$fitted, digits =  digits,  ...)
  dd <- x$model$dims
  Ngrps <- dd$ngrps[1:dd$Q]
  message("\n")
  message("Number of Observations:", dd[["N"]])
  message("\nNumber of Groups: ")
  if ((lNgrps <- length(Ngrps)) == 1) {	# single nesting
    message(Ngrps,"\n")
  } else {				# multiple nesting
    sNgrps <- 1:lNgrps
    aux <- rep(names(Ngrps), sNgrps)
    aux <- split(aux, array(rep(sNgrps, lNgrps),
                            c(lNgrps, lNgrps))[!lower.tri(diag(lNgrps))])
    names(Ngrps) <- unlist(lapply(aux, paste, collapse = " %in% "))
    message("\n")
    print(rev(Ngrps),  digits = digits, ...)
  }
  invisible(x)
}

#=======================================================================
# fitted method
#=======================================================================
##' @rdname fitted.lcc
##' @method fitted lcc
##' @title  Extract \code{lcc} Fitted Values
##' @usage
##' \method{fitted}{lcc}(object, type, digits, ...)
##' @aliases fitted.lcc
##' @description Fitted values from object of class \code{lcc}
##'   returned by modeling functions.
##'
##' @return A data frame with columns given by methods, time, and
##'   fitted values.
##'
##' @param object an object inheriting from class \code{lcc},
##'   representing a fitted longitudinal concordance correlation
##'   function.
##'
##' @param type an optional character string specifying the type of
##'   output to be returned. If \code{type="lcc"}, prints the fitted
##'   longitudinal concordance correlation. If
##'   \code{type="lpc"}, prints the fitted longitudinal Pearson
##'   correlation. If \code{type="la"}, prints the fitted longitudinal
##'   accuracy. Defaults to \code{type="lcc"}.
##'
##' @param digits a non-null value for \code{digits} specifies the minimum
##'   number of significant digits to be printed in values. The
##'   default, \code{NULL}.
##'
##' @param ... not used.
##' 
##' @return No return value, called for side effects
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@alumni.usp.br}
##' @seealso \code{\link[lcc]{lcc}}, \code{\link[lcc]{summary.lcc}},
##'   \code{\link[lcc]{lccPlot}}
##' @examples
##' data(hue)
##' ## Second degree polynomial model with random intercept, slope and
##' ## quadratic term
##' \dontrun{
##' fm1 <- lcc(data = hue, subject = "Fruit", resp = "H_mean",
##'            method = "Method", time = "Time", qf = 2, qr = 2,
##'            components = TRUE)
##' fitted(fm1)
##' fitted(fm1, type="lpc")
##' fitted(fm1, type="la")
##' }
##'
##' @importFrom stats AIC BIC
##' @export

fitted.lcc <- function(object, type = "lcc", digits = NULL, ...){
  if (!inherits(object, "lcc")) stop("Object must inherit from class \"lcc\"",
                                call.=FALSE)
  if(missing(type)) type="lcc"
  if(object$plot_info$components == FALSE && type == "lpc" ||
     object$plot_info$components == FALSE && type == "la"){
    stop(paste0("It is necessary to include components = TRUE in the
  lcc() function to calculate the fitted values for type ", type))
  }
  if(type == "lcc" || type == "lpc" || type == "la"){
    pr <- switch(type,
                 "lcc" = cat( "Fitted longitudinal concordance correlation function", "\n"),
                 "lpc" = cat( "Fitted longitudinal Pearson correlation function", "\n"),
                 "la"  = cat( "Fitted longitudinal accuracy function", "\n"))
    message("\n")
    fittedBuilder(object = object, type = type)
  }else{
    stop("Available 'type' are 'lcc',  'lpc', or 'la'", call.=FALSE)
  }
}

#=======================================================================
# Summary method
#=======================================================================
#-----------------------------------------------------------------------
# Print method for summary.lcc
#-----------------------------------------------------------------------
##' @rdname print.summary.lcc
##' @title  Print the Summary of an \code{lcc} Object
##' @usage
##' \method{print}{summary.lcc}(x, verbose, digits, ...)
##' @method print summary.lcc
##' @aliases print.summary.lcc
##' @description Information summarizing the fitted longitudinal
##'   concordance correlation is printed. This includes the AIC, BIC,
##'   and log-likelihood at convergence. If \code{type = "lcc"}, prints
##'   the fitted values while \code{type = "model"} prints the fixed
##'   effects estimates and their standard errors, standard deviations,
##'   correlations for the random effects, within-group correlation, and
##'   variance function parameters.
##'
##' @param x an object inheriting from class
##'   \code{\link[lcc]{summary.lcc}}, representing a fitted longitudinal
##'   concordance correlation function.
##'
##' @param verbose an optional logical value used to control the amount
##'   of printed output when \code{type = "model"}. Defaults to
##'   \code{FALSE}
##'
##' @param digits a non-null value for \code{digits} specifies the
##'   minimum number of significant digits to be printed in values. The
##'   default, \code{NULL}.
##'
##' @param ... further arguments passed to \code{\link{print}}.
##'
##' @importFrom stats AIC BIC asOneSidedFormula
##'
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@alumni.usp.br}
##' @return No return value, called for side effects
##' @seealso \code{\link{summary.lcc}}, \code{\link{lccPlot}},
##'   \code{\link[lcc]{lcc}}
##'
##' @examples
##' \dontrun{
##' ## Second degree polynomial model with random intercept, slope and
##' ## quadratic term
##' fm1<-lcc(data = hue, subject = "Fruit", resp = "H_mean",
##'          method = "Method", time = "Time", qf = 2, qr = 2)
##' print(summary(fm1, type="model"))
##' }
##'
##' @export

print.summary.lcc <- function(x, verbose =  FALSE, digits = NULL, ...){
  if (inherits(x, "lcc")) {
    message("Longitudinal concordance correlation model fit by ")
    message( if(x$model$method == "REML") "REML\n" else "maximum likelihood\n")
    AIC <- AIC(x$model)
    BIC <- BIC(x$model)
    logLik <- c(x$model$logLik)
    print(data.frame(AIC, BIC, logLik, row.names = " "), digits = digits, ...)
    message("\n")
    gof <- x$gof
    message(paste0(" gof: ", round(gof, 4)), "\n")
    message("\n")
    if (inherits(x$comp, "character")) {
      if (is.null(x$info$ENV.LCC)) {
        message(x$comp, "\n")
        fitted <- x$fitted
        print(fitted, digits = digits,  ...)
      }else{
        message(paste0(" Lower and upper bound of ", (1-x$plot_info$alpha)*100,"%"), "bootstrap confidence interval", "\n")
        message(" Number of bootstrap samples: ", x$plot_info$nboot, "\n")
        message("\n")
        message(x$comp, "\n")
        fitted <- x$fitted
        print(fitted, digits = digits,  ...)
      }
    }else{
      summ <- sum(sapply(x$comp, length))
      if(is.null(x$info$ENV.LCC)){
        for(i in 1:summ){
          message(x$comp[[i]], "\n")
          fitted <- x$fitted
          print(fitted[[i]],  digits = digits, ...)
          message("\n")
        }
      }else{
        message(paste0(" Lower and upper bound of ", (1-x$info$alpha)*100,"%"), "bootstrap confidence interval", "\n")
        message(" Number of bootstrap samples: ", x$info$nboot, "\n")
        message("\n")
        for(i in 1:summ){
          message(x$comp[[i]], ": LCC", "\n")
          fitted <- x$fitted
          print(fitted$LCC[[i]],  digits = digits, ...)
          message("\n")
          message(x$comp[[i]], ": LPC", "\n")
          print(fitted$LPC[[i]],  digits = digits, ...)
          message("\n")
          message(x$comp[[i]], ": LA", "\n")
          print(fitted$LA[[i]],  digits = digits, ...)
          message("\n", "\n")
        }
      }
    }
  }else{
    if (inherits(x, "model")) {
      dd <- x$dims
      verbose <- verbose || attr(x, "verbose")
      message( "Linear mixed-effects model fit by " )
      message( if(x$method == "REML") "REML\n" else "maximum likelihood\n")
      ##  method <- x$method
      message(" Data:", deparse( x$call$data ), "\n")
      if (!is.null(x$call$subset)) {
        message("  Subset:", deparse(asOneSidedFormula(x$call$subset)[[2L]]),"\n")
      }
      print(data.frame(AIC = x$AIC, BIC = x$BIC, logLik = c(x$logLik),
                       row.names = " "), ...)
      if (verbose) { message("Convergence at iteration:",x$numIter,"\n") }
      message("\n")
      print(summary(x$modelStruct), sigma = x$sigma,
            reEstimates = x$coef$random, verbose = verbose, ...)
      message("Fixed effects: ")
      fixF <- x$call$fixed
      if (inherits(fixF, "formula") || is.call(fixF)) {
        message(deparse(x$call$fixed), "\n")
      } else {
        message(deparse(lapply(fixF, function(el) as.name(deparse(el)))), "\n")
      }
      ## fixed effects t-table and correlations
      xtTab <- as.data.frame(x$tTable)
      wchPval <- match("p-value", names(xtTab))
      for(i in names(xtTab)[-wchPval]) {
        xtTab[, i] <- format(zapsmall(xtTab[, i]))
      }
      xtTab[,wchPval] <- format(round(xtTab[,wchPval], 4))
      if (any(wchLv <- (as.double(levels(xtTab[, wchPval])) == 0))) {
        levels(xtTab[, wchPval])[wchLv] <- "<.0001"
      }
      row.names(xtTab) <- dimnames(x$tTable)[[1L]]
      print(xtTab, ...)
      if (nrow(x$tTable) > 1) {
        corr <- x$corFixed
        class(corr) <- "correlation"
        print(corr, title = " Correlation:", ...)
      }
      message("\nStandardized Within-Group Residuals:\n")
      print(x$residuals, ...)
      message("\nNumber of Observations:",x$dims[["N"]])
      message("\nNumber of Groups: ")
      Ngrps <- dd$ngrps[1:dd$Q]
      if ((lNgrps <- length(Ngrps)) == 1) {	# single nesting
        message(Ngrps,"\n")
      } else {				# multiple nesting
        sNgrps <- 1:lNgrps
        aux <- rep(names(Ngrps), sNgrps)
        aux <- split(aux, array(rep(sNgrps, lNgrps),
                                c(lNgrps, lNgrps))[!lower.tri(diag(lNgrps))])
        names(Ngrps) <- unlist(lapply(aux, paste, collapse = " %in% "))
        message("\n")
        print(rev(Ngrps), ...)
      }
      invisible(x)
    }else{
      stop("Available only for classes summary.lcc or summary.lme", call.=FALSE)
    }
  }
}

#-----------------------------------------------------------------------
# summary.lcc
#-----------------------------------------------------------------------

##' @rdname summary.lcc
##' @title  Summarize an \code{lcc} Object
##' @usage
##' \method{summary}{lcc}(object, type, adjustSigma, verbose, ...)
##'
##' @method summary lcc
##' @aliases summary.lcc
##' @description Additional information about the fit of longitudinal
##'   concordance correlation, longitudinal Pearson correlation, and
##'   longitudinal accuracy represented by an object of class
##'   \code{\link[lcc]{lcc}}. The returned object has a
##'   \code{\link[base]{print}} method.
##'
##' @return an object inheriting from class \code{summary.lcc}
##'   including: \item{fitted}{the fitted values extracted from the
##'   \code{lcc} object.} \item{gof}{the goodness of fit (gof) measurement
##'   is calculated using the concordance correlation coefficient between
##'   fitted and observed values. Value of 1 denote perfect concordance.}
##'   \item{AIC}{the Akaike Information Criterion corresponding to object.}
##'   \item{BIC}{the Bayesian Information Criterion corresponding to object.}
##'   \item{logLik}{If \code{REML=FALSE}, returns the log-likelihood value
##'   of the linear mixed-effects model; otherwise, the restricted
##'   log-likelihood is returned}
##'
##' @param object an object inheriting from class
##'   \code{\link[lcc]{lcc}}, representing a fitted longitudinal
##'   concordance correlation function.
##'
##' @param type an optional character string specifying the type of
##'   output to be returned. If \code{type="model"}, prints the summary
##'   of the polynomial mixed-effects regression model. If
##'   \code{type="lcc"}, prints the summary of the fitted and sampled
##'   values for LCC, LPC, and LA as well as the concordance correlation
##'   coefficient between fitted values from the model and observed
##'   values as goodness of fit (gof) measurement. Defaults to
##'   \code{type="model"}.
##'
##' @param adjustSigma an optional logical value used when \code{type =
##'   model}. If TRUE and the estimation method used to obtain object
##'   was maximum likelihood, the residual standard error is multiplied
##'   by \code{sqrt(nobs/(nobs - npar))}.  See
##'   \code{\link[nlme]{summary.lme}} for more information.  Default is
##'   TRUE.
##'
##' @param verbose an optional logical value used to control the amount
##' of output in the \code{print.summary.lme} method when
##' \code{type = model} is used. Defaults to FALSE.
##'
##' @param ...  not used.
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @importFrom stats AIC BIC asOneSidedFormula pt resid
##'
##' @seealso \code{\link{AIC}}, \code{\link{BIC}},
##' \code{print.summary.lcc},  \code{\link[lcc]{lcc}}
##'
##' @examples
##'
##' ## Second degree polynomial model with random intercept, slope and
##' ## quadratic term
##' fm1<-lcc(data = hue, subject = "Fruit", resp = "H_mean",
##'          method = "Method", time = "Time", qf = 2, qr = 2)
##' summary(fm1, type="model")
##' summary(fm1, type="lcc")
##' @export

summary.lcc <- function(object, type, adjustSigma = TRUE,
                        verbose = FALSE, ...)
{
  if (!inherits(object, "lcc")) stop("Object must inherit from class \"lcc\"",
                                call.=FALSE)
  if(missing(type)) type <- "model"
  if(type=="model" || type=="lcc"){
    if(type == "lcc"){
      # Object lcc
      object$fitted <- object$Summary.lcc$fitted
      object$sampled <- object$Summary.lcc$sampled
      object$gof <- object$Summary.lcc$gof
      object$data <- object$data
      object$AIC <- AIC(object[1]$model)
      object$BIC <- BIC(object[1]$model)
      object$logLik <- c(object[1]$model$logLik)
      object$info <- object$plot_info
      object$comp <- object$Summary.lcc$comp
      object <- object[names(object) != "Summary.lcc"]
      object <- object[names(object) != "plot_info"]
      object <- object[names(object) != "data"]
      #-----------------------------------------------------------------
      ## generating the final object
      #-----------------------------------------------------------------
      structure(object, type = "lcc",  oClass = class(object),
                class = c("summary.lcc", type))
    }else {
      #-------------------------------------------------------------------
      # Model
      #-------------------------------------------------------------------
      obj <- object[1]$model
      ##  variance-covariance estimates for fixed effects
      fixed <- fixef(obj)
      stdFixed <- sqrt(diag(as.matrix(obj$varFix)))
      obj$corFixed <- array(t(obj$varFix/stdFixed)/stdFixed,
                            dim(obj$varFix), list(names(fixed),names(fixed)))
      if (adjustSigma && obj$method == "ML")
        stdFixed <- stdFixed *
        sqrt(obj$dims$N/(obj$dims$N - length(stdFixed)))
      ## fixed effects coefficients, std. deviations and t-ratios
      fDF <- obj$fixDF[["X"]]
      tVal <- fixed/stdFixed
      obj$tTable <- cbind(Value = fixed, Std.Error = stdFixed, DF = fDF,
                          "t-value" = tVal, "p-value" = 2 * pt(-abs(tVal), fDF))
      ## residuals
      resd <- resid(obj, type = "pearson")
      if (length(resd) > 5) {
        resd <- quantile(resd, na.rm = TRUE) # might have NAs from na.exclude
        names(resd) <- c("Min","Q1","Med","Q3","Max")
      }
      obj$residuals <- resd
      ## generating the final obj
      aux <- logLik(obj)
      obj$BIC <- BIC(aux)
      obj$AIC <- AIC(aux)
      structure(obj, verbose = verbose, oClass = class(obj),
                class = c("summary.lcc", "model", class(obj)))
    }
  }else {
    stop("Available 'type' are lcc or model", call.=FALSE)
  }
}


#=======================================================================
# Plot method
#=======================================================================
##' @rdname plot.lcc
##' @method plot lcc
##' @title Diagnostic Plots of an \code{lcc} Object.
##'
##' @usage
##' \method{plot}{lcc}(x, which = c(1L:6L),
##'      caption = list("Residuals vs Fitted",
##'                     "Residuals vs Time",
##'                     "Residuals by Subject",
##'                     "Observed values vs Fitted values",
##'                     "Normal Q-Q Plot (Conditional residuals)",
##'                     "Normal Q-Q Plot (Random effects)"),
##'      sub.caption =  NULL,  main = NULL,
##'      panel = if(add.smooth) panel.smooth else points,
##'      add.smooth = TRUE, ask = TRUE,
##'      id.n = 3, labels.id = names(residuals(x)),
##'      label.pos = c(4, 2), cex.id = 0.75, cex.caption = 1,
##'      cex.oma.man = 1.25, ...)
##'
##' @aliases plot.lcc
##'
##' @description Diagnostic plots for conditional error and random
##' effects from the linear mixed-effects fit are obtained. Six plots
##' plots (selectable by 'which') are currently available: a plot of
##' residuals against fitted values, a plot of residuals against
##' time variable, a boxplot of residuals by subject,
##' a plot of observerd values against fitted values, a normal Q-Q plot
##' with simulation envelopes based on conditional error,  and a normal
##' Q-Q plot with simulation envelopes based on the random effects. By
##' default, all plots are provided.
##'
##' @param x an object inheriting from class \code{\link[lcc]{lcc}},
##'   representing a fitted longitudinal concordance correlation
##'   function.
##' @param which if a subset of the plots is required, specify a subset
##'   of the numbers from 1 to 6.
##' @param caption captions to appear above the plots. Vector or list of
##'   valid graphics annotations is required. All captions can be
##'   supressed using '""' or \code{NA}.
##' @param sub.caption common sub-title (at bottom). Default to
##'   \code{NULL}.
##' @param main The main title (on top) above the caption.
##' @param panel panel function. If \code{add.smooth = TRUE},
##'   \code{panel.smooth} is used rather than \code{points}.
##' @param add.smooth logical indicating if smoother should be added to
##'   most plots; see also \code{panel} above. Defaults to \code{TRUE}.
##' @param ask logical; if \code{TRUE}, the default, the user is _ask_ed
##'   before each plot, see \code{\link[graphics]{par}}.
##' @param id.n number of points to be labelled is the first three
##'   plots, starting with the most extreme.
##' @param labels.id vector of labels, from which the labels for extreme
##'   points will be chosen. Default to \code{NULL} (uses observation
##'   numbers).
##' @param label.pos positioning of labels, for the left half and right
##'   half of the graph respectively, for plots 1-3.
##' @param cex.id magnification of point label.
##' @param cex.caption controls the size of \code{caption}.
##' @param cex.oma.man controls the size of the \code{sub.caption} only
##'   if that is _above_ the figures when there is more than one.
##' @param ... further graphical parameters from 'par'.
##'
##' @details The Q-Q plot uses the normalized residuals. The
##'   standardized residuals is pre-multiplied by the inverse
##'   square-root factor of the estimated error correlation matrix while
##'   the random effects is pre-multiplied by the inverse square root of
##'   the estimated variances obtained from matrix G. The simulate
##'   envelopes are obtained from package hnp (Moral et al.,  2018).
##'
##'   Code partially adapted from \code{\link[stats]{plot.lm}}.
##' @return Return plots for conditional error and random effects from the linear mixed-effects
##' @importFrom hnp hnp
##'
##' @importFrom nlme getVarCov ranef
##'
##' @importFrom grDevices as.graphicsAnnot dev.flush dev.hold dev.interactive devAskNewPage extendrange n2mfrow
##'
##' @importFrom graphics abline boxplot mtext panel.smooth par plot points strheight text title
##'
##' @importFrom stats fitted residuals
##'
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @seealso \code{\link{lccPlot}}, \code{\link[lcc]{lcc}},
##'   \code{mtext}, \code{text}, \code{plotmath}
##'
##' @examples
##'
##' ## Second degree polynomial model with random intercept, slope and
##' ## quadratic term
##' fm1 <- lcc(data = hue, subject = "Fruit", resp = "H_mean",
##'            method = "Method", time = "Time", qf = 2, qr = 2)
##' plot(fm1)
##' @export

plot.lcc <- function(x, which = c(1L:6L),
           caption = list("Residuals vs Fitted",
                          "Residuals vs Time",
                          "Residuals by Subject",
                          "Observed values vs Fitted values",
                          "Normal Q-Q Plot (Conditional residuals)",
                          "Normal Q-Q Plot (Random effects)"),
           sub.caption =  NULL,  main = NULL,
           panel = if(add.smooth) panel.smooth else points,
           add.smooth = TRUE, ask = TRUE,
           id.n = 3, labels.id = names(residuals(x)),
           label.pos = c(4, 2), cex.id = 0.75, cex.caption = 1,
           cex.oma.man = 1.25, ...)
  {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    if (!is.lcc(x))
      stop("use only with \"lcc\" objects", call. = FALSE)
    if(!is.numeric(which) || any(which < 1) || any(which > 6))
      stop("'which' must be in 1:6")
    #-------------------------------------------------------------------
    show <- rep(FALSE, 6)
    show[which] <- TRUE
    #-------------------------------------------------------------------
    # information from lme model
    #-------------------------------------------------------------------
    model <- x$model
    r <- residuals(model)
    r_norm <- residuals(model, type = "normalized")
    yh <- fitted(model)
    n <- length(r)
    time <- model$data$time
    #-------------------------------------------------------------------
     if(id.n > 0L) { ## label the largest residuals
       if(is.null(labels.id))
         labels.id <- paste(1L:n)
       iid <- 1L:id.n
       show.r <- sort.list(abs(r), decreasing = TRUE)[iid]
       text.id <- function(x, y, ind, adj.x = TRUE) {
         if (is.factor(x)) {
           labpos <-
             if(adj.x) label.pos[1+as.numeric(y > mean(range(y)))] else 3
         }else {
           labpos <-
             if(adj.x) label.pos[1+as.numeric(x > mean(range(x)))] else 3
         }
         text(x, y, labels.id[ind], cex = cex.id, xpd = TRUE,
              pos = labpos, offset = 0.25)
       }
    }
    #-------------------------------------------------------------------
    getCaption <- function(k) # allow caption = "" , plotmath etc
      if(length(caption) < k) NA_character_ else as.graphicsAnnot(caption[[k]])
    one.fig <- prod(par("mfcol")) == 1
    if (ask) {
      oask <- devAskNewPage(TRUE)
      on.exit(devAskNewPage(oask))
    }
    #-------------------------------------------------------------------
    # Individual plots
    #-------------------------------------------------------------------
    if (show[1L]) {
      l.fit <- "Fitted values"
      ylim <- range(r, na.rm=TRUE)
      if(id.n > 0)
        ylim <- extendrange(r = ylim, f = 0.08)
      dev.hold()
      plot(yh, r, xlab = l.fit, ylab = "Residuals", main = main,
           ylim = ylim, type = "n", ...)
      panel(yh, r, ...)
      if (one.fig)
        title(sub = sub.caption, ...)
      mtext(getCaption(1), 3, 0.25, cex = cex.caption)
      if(id.n > 0) {
        y.id <- r[show.r]
        y.id[y.id < 0] <- y.id[y.id < 0] - strheight(" ")/3
        text.id(yh[show.r], y.id, show.r)
      }
      abline(h = 0, lty = 3, col = "gray")
      dev.flush()
    }
    #===================================================================
    if (show[2L]) {
      l.fit <- "Time"
      ylim <- range(r, na.rm=TRUE)
      if(id.n > 0)
        ylim <- extendrange(r = ylim, f = 0.08)
      dev.hold()
      plot(time, r, xlab = l.fit, ylab = "Residuals", main = main,
           ylim = ylim, type = "n", ...)
      panel(time, r, ...)
      if (one.fig)
      title(sub = sub.caption, ...)
      mtext(getCaption(2), 3, 0.25, cex = cex.caption)
      if(id.n > 0) {
        y.id <- r[show.r]
        y.id[y.id < 0] <- y.id[y.id < 0] - strheight(" ")/3
        text.id(time[show.r], y.id, show.r)
      }
      abline(h = 0, lty = 3, col = "gray")
      dev.flush()
    }
    #===================================================================
    if (show[3L]) {
      Subject <- model$data$subject
      boxplot(r ~ Subject,  ylab = "Residuals", main = main, ...)
      if (one.fig)
        title(sub = sub.caption, ...)
      mtext(getCaption(3), 3, 0.25, cex = cex.caption)
      if(id.n > 0) {
        y.id <- r[show.r]
        y.id[y.id < 0] <- y.id[y.id < 0] - strheight(" ")/3
        text.id(Subject[show.r], y.id, show.r)
      }
      abline(h = 0, lty = 3, col = "blue")
    }
    #===================================================================
    if (show[4L]) {
      Response <- model$data$resp
      plot(Response ~  yh, ylab = "Observed Values",
           xlab = "Fitted Values", main = main, ...)
      if (one.fig)
        title(sub = sub.caption, ...)
      mtext(getCaption(4), 3, 0.25, cex = cex.caption)
      abline(0, 1)
    }
    #===================================================================
    if (show[5L]) {
        hnp(r_norm, scale = TRUE, halfnormal = FALSE, print.on = TRUE,
                 main = main, ...)
        if (one.fig)
          title(sub = sub.caption, ...)
        mtext(getCaption(5), 3, 0.25, cex = cex.caption)
    }
    #===================================================================
    if (show[6L]) {
      vars <- sqrt(diag(getVarCov(model)))
      ranefs <- re <- as.matrix(ranef(model))
      re <- ranefs %*% diag(1 / vars)
      ncol.re <- ncol(re)
      if (ncol.re == 1) {
        if (is.null(main)) {
          main <- "Random effect (Intercept)"
        }
        hnp(re, scale = TRUE, halfnormal = FALSE, print.on = TRUE,
                   main = main, ...)
      }else {
        par(mfrow = rev(n2mfrow(ncol.re)))
        for (i in 1:ncol.re) {
          hnp(re[, i], scale = TRUE, halfnormal = FALSE, print.on = TRUE,
                   main = main, ...)
          if (one.fig)
            title(sub = sub.caption, ...)
          mtext(getCaption(6), 3, 0.25, cex = cex.caption)
          mtext(paste0("b", i - 1, "i"), 1, -1.5, cex = cex.caption)
        }
      }
    }
    par(mfrow = c(1,1))
    invisible()
  }

#=======================================================================
# coef function
# =======================================================================
##' @rdname coef.lcc
##' @title Extract Model Coefficients
##' @usage \method{coef}{lcc}(object, ...)
##' @method coef lcc
##' @aliases coef.lcc
##'
##' @description The fixed effects estimated and corresponding random
##'   effects estimates are obtained at subject levels less or equal to
##'   i. The resulting estimates are returned as a data frame, with rows
##'   corresponding to subject levels and columns to coefficients.
##'
##' @param object an object inheriting from class \code{lcc},
##'   representing a fitted longitudinal concordance correlation
##'   function.
##' @param ... optional arguments passed to the \code{coef.lme}
##'   function.
##'
##' @details See methods for \code{\link{nlme}} objects to get more
##'   details.
##' @return Coefficients extracted from the model object.
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @seealso \code{\link[lcc]{lcc}}, \code{\link{summary.lcc}},
##'   \code{\link{lccPlot}}, \code{\link{vcov.lcc}}
##'
##' @importFrom stats coef
##'
##' @examples
##'
##' \dontrun{
##' fm1<-lcc(data = hue, subject = "Fruit", resp = "H_mean",
##'          method = "Method", time = "Time", qf = 2, qr = 2)
##' coef(fm1)
##' }
##'
##' @export

coef.lcc <- function(object, ...) {
  if (!is.lcc(object))
    stop("use only with \"lcc\" objects", call. = FALSE)
  x <- coef(object$model)
  colnames(x) <- gsub(pattern = "fixed", x = colnames(x),
                      replacement = "Fixed")
  colnames(x) <- gsub(pattern = "Poly", x = colnames(x),
                      replacement = "Time")
  colnames(x) <- gsub(pattern = "fmla.", x = colnames(x),
                      replacement = "")
  colnames(x) <- gsub(pattern = "\\(time, degree = qr, raw = TRUE\\)",
                      x = colnames(x), replacement = "")
  colnames(x) <- gsub(pattern = "rand", x = colnames(x),
                      replacement = "Random")
  colnames(x) <- gsub(pattern = "poly", x = colnames(x),
                      replacement = "Time")
  colnames(x) <- gsub(pattern = "method", x = colnames(x),
                      replacement = "")
  class(x) <- c("coef.lcc", "ranef.lcc",  "data.frame")
  x
}

#=======================================================================
# vcov function
#=======================================================================
##' @rdname vcov.lcc
##' @title Extract Variance-Covariance Matrix of the Fixed Effects
##' @usage \method{vcov}{lcc}(object, ...)
##' @method vcov lcc
##' @aliases vcov.lcc
##'
##' @return Returns the variance-covariance matrix of a fitted
##'   \code{lcc} model object.
##'
##' @param object an object inheriting from class \code{lcc},
##'   representing a fitted longitudinal concordance correlation
##'   function.
##' @param ... optional arguments passed to the \code{vcov.lme}
##'   function.
##'
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @details See methods for \code{\link{nlme}} objects to get more
##'   details.
##'
##' @seealso \code{\link{summary.lcc}}, \code{\link{lccPlot}},
##'   \code{\link[lcc]{lcc}}, \code{\link{coef.lcc}}
##'
##' @importFrom stats vcov
##'
##' @examples
##' \dontrun{
##' fm1<-lcc(data = hue, subject = "Fruit", resp = "H_mean",
##'          method = "Method", time = "Time", qf = 2, qr = 2)
##' vcov(fm1)
##' }
##'
##' @export

vcov.lcc <- function(object, ...) {
   if (!is.lcc(object))
      stop("use only with \"lcc\" objects", call. = FALSE)
  x <- vcov(object$model, ...)
  rownames(x) <- colnames(x) <-
    gsub(pattern = "fixed", x = colnames(x),
         replacement = "Fixed")
  rownames(x) <- colnames(x) <-
    gsub(pattern = "Poly", x = colnames(x),
         replacement = "Time")
  rownames(x) <- colnames(x) <-
    gsub(pattern = "method", x = colnames(x),
         replacement = "")
  x
}

#=======================================================================
# getVarCov function
#=======================================================================
##' @rdname getVarCov.lcc
##' @title Extract Variance Components from a Fitted Model
##' @usage \method{getVarCov}{lcc}(obj, type, ...)
##' @method getVarCov lcc
##' @aliases getVarCov.lcc
##'
##' @return Returns the variance-covariance matrix of a fitted
##'   \code{lcc} model object.
##'
##' @param obj an object inheriting from class \code{lcc}, representing
##'   a fitted longitudinal concordance correlation function.
##' @param type specifies the type of variance covariance matrix. If
##'   \code{type = "random.effects"}, the default, extract the
##'   random-effects variance-covariance; if \code{type = "conditional"}
##'   extract the conditional variance-covariance of the responses; and
##'   if \code{type = "marginal"} extracts the the marginal
##'   variance-covariance of the responses.
##' @param ... optional arguments passed to the \code{getVarCov}
##'   function.
##'
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @seealso \code{\link[lcc]{lcc}}, \code{\link{summary.lcc}},
##'   \code{\link{coef.lcc}}, \code{\link{vcov.lcc}}
##'
##' @details See methods for \code{\link{nlme}} objects to get more
##'   details.
##'
##' @importFrom nlme getVarCov
##'
##' @examples
##'
##' \dontrun{
##' fm1<-lcc(data = hue, subject = "Fruit", resp = "H_mean",
##'          method = "Method", time = "Time", qf = 2, qr = 2)
##' getVarCov(fm1)
##' }
##'
##' @export

getVarCov.lcc <- function(obj, type = "random.effects", ...) {
   if (!is.lcc(obj))
      stop("use only with \"lcc\" objects", call. = FALSE)
  x <- getVarCov(obj$model, type = type, ...)
  rownames(x) <- colnames(x) <-
    gsub(pattern = "fmla.", x = colnames(x), replacement = "")
  rownames(x) <- colnames(x) <-
    gsub(pattern = "\\(time, degree = qr, raw = TRUE\\)",
         x = colnames(x), replacement = "")
  rownames(x) <- colnames(x) <-
    gsub(pattern = "rand", x = colnames(x), replacement = "Random")
  rownames(x) <- colnames(x) <-
    gsub(pattern = "poly", x = colnames(x),
         replacement = "Time")
  x
}

#=======================================================================
# Residuals
#=======================================================================
##' @rdname residuals.lcc
##' @title Extract Model Residuals
##' @usage \method{residuals}{lcc}(object, type, ...)
##' @method residuals lcc
##' @aliases residuals.lcc
##'
##' @description Extract the residulas from the model used to estimate
##'   the longitudinal concordance correlation function.
##'
##' @param object an object inheriting from class \code{lcc},
##'   representing a fitted longitudinal concordance correlation
##'   function.
##'
##' @param type an optional character string specifying the type of
##'   residulas to be used. If \code{type = "response"}, the default,
##'   the residuals at level i are obtained by subtracting the fitted
##'   values at that level from the response vector. If \code{type =
##'   "pearson"}, the "response" residuals is divided by the estimated
##'   within-group standard error. If \code{type = "normalized"}, the
##'   normalized residuals are used. Partial matching of arguments is
##'   used, so only the first character needs to be provided.
##'
##' @param ... optional arguments passed to the \code{residuals.lme}
##'   function.
##'
##' @details See methods for \code{\link{nlme}} objects to get more
##'   details.
##' @return Return no value, called for side effects
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @seealso \code{\link[lcc]{lcc}}, \code{\link{summary.lcc}},
##'   \code{\link{coef.lcc}}, \code{\link{vcov.lcc}}
##'
##' @examples
##'
##' \dontrun{
##' fm1<-lcc(data = hue, subject = "Fruit", resp = "H_mean",
##'          method = "Method", time = "Time", qf = 2, qr = 2)
##' getVarCov(fm1)
##' }
##'
##' @export

residuals.lcc <- function(object, type = "response", ...) {
   if (!is.lcc(object))
      stop("use only with \"lcc\" objects", call. = FALSE)
  residuals(object$model, type = type, ...)
}

#=======================================================================
# AIC and BIC
#=======================================================================
##' @rdname AIC.lcc
##' @title Akaike and Bayesian Information Criteria for an \code{lcc} Object.
##' @method AIC lcc
##' @aliases AIC.lcc
##' @description Calculate the Akaike's 'An Information Criterion' or
##'   the BIC or SBC (Schwarz's Bayesian criterion) for an object of
##'   class \code{lcc}.
##'
##' @param object an object inheriting from class \code{lcc},
##'   representing a fitted longitudinal concordance correlation
##'   function.
##'
##' @param k numeric value, use as penalty coefficient for the number of
##'   parameters in the fitted model; the default \code{k = 2} is the
##'   classical AIC.
##'
##' @param ... optional arguments passed to the \code{AIC}
##'   function.
##'
##' @return A numeric value with the corresponding AIC or BIC
##'   value. See methods for \code{\link{AIC}} objects to get more
##'   details.
##'
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @seealso \code{\link[lcc]{lcc}}, \code{\link{summary.lcc}},
##'   \code{\link{coef.lcc}}, \code{\link{vcov.lcc}}
##'
##' @importFrom stats na.omit nobs
##'
##' @export

AIC.lcc <- function(object, ..., k = 2) {
  if (!is.lcc(object))
    stop("use only with \"lcc\" objects", call. = FALSE)
  ll <- logLik
  if (!missing(...)) {
    lls <- lapply(list(object$model, ...), ll)
    vals <- sapply(lls, function(el) {
      no <- attr(el, "nobs")
      c(as.numeric(el), attr(el, "df"), if (is.null(no)) NA_integer_ else no)
    })
    val <- data.frame(df = vals[2L, ], ll = vals[1L, ])
    nos <- na.omit(vals[3L, ])
    if (length(nos) && any(nos != nos[1L]))
      warning("models are not all fitted to the same number of observations")
    val <- data.frame(df = val$df, AIC = -2 * val$ll + k *
                        val$df)
    Call <- match.call()
    Call$k <- NULL
    row.names(val) <- as.character(Call[-1L])
    val
  }
  else {
    AIC(object$model)
  }
}

##' @rdname AIC.lcc
##' @method BIC lcc
##' @aliases BIC.lcc
##'
##' @importFrom stats na.omit nobs
##'
##' @examples
##' \dontrun{
##' attach(simulated_hue)
##' fm6 <- lcc(data = simulated_hue, subject = "Fruit",
##'            resp = "Hue", method = "Method", time = "Time",
##'            qf = 2, qr = 1, components = TRUE,
##'            time_lcc = list(n=50, from=min(Time), to=max(Time)))
##' AIC(fm6)
##' BIC(fm6)
##' }
##'
##' @export

BIC.lcc <- function (object, ...)
{
  if (!is.lcc(object))
    stop("use only with \"lcc\" objects", call. = FALSE)
  ll <- logLik
  Nobs <- nobs
  if (!missing(...)) {
    lls <- lapply(list(object$model, ...), ll)
    vals <- sapply(lls, function(el) {
      no <- attr(el, "nobs")
      c(as.numeric(el), attr(el, "df"), if (is.null(no)) NA_integer_ else no)
    })
    val <- data.frame(df = vals[2L, ], ll = vals[1L, ],
      nobs = vals[3L, ])
    nos <- na.omit(val$nobs)
    if (length(nos) && any(nos != nos[1L]))
      warning("models are not all fitted to the same number of observations")
    unknown <- is.na(val$nobs)
    if (any(unknown))
      val$nobs[unknown] <- sapply(list(object$model, ...)[unknown],
        function(x) tryCatch(Nobs(x), error = function(e) NA_real_))
    val <- data.frame(df = val$df, BIC = -2 * val$ll + log(val$nobs) *
      val$df)
    row.names(val) <- as.character(match.call()[-1L])
    val
  }
  else {
   BIC(object$model)
  }
}

#=======================================================================
# ranef
# =======================================================================
##' @rdname ranef.lcc
##' @title Extract Model Random Effects
##' @usage \method{ranef}{lcc}(object, ...)
##' @method ranef lcc
##' @aliases ranef.lcc
##'
##' @description Extract the estimated random effects at level i. 
##' 
##' @return A data frame with rows given by the different groups at that level and
##'   columns given by the random effects.
##'
##' @param object an object inheriting from class \code{lcc},
##'   representing a fitted longitudinal concordance correlation
##'   function.
##' @param ... optional arguments passed to the \code{ranef.lme}
##'   function.
##'
##' @details See methods for \code{\link{nlme}} objects to get more
##'   details.
##'
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @seealso \code{\link[lcc]{lcc}}, \code{\link{coef.lcc}},
##'
##' @importFrom nlme ranef
##'
##' @examples
##' \dontrun{
##' fm1<-lcc(data = hue, subject = "Fruit", resp = "H_mean",
##'          method = "Method", time = "Time", qf = 2, qr = 2)
##' ranef(fm1)
##' }
##' @export

ranef.lcc <- function(object, ...) {
  if (!is.lcc(object))
     stop("use only with \"lcc\" objects" , call. = FALSE)
  x <- ranef(object$model)
  colnames(x) <- gsub(pattern = "fmla.", x = colnames(x),
                      replacement = "")
  colnames(x) <- gsub(pattern = "\\(time, degree = qr, raw = TRUE\\)",
                      x = colnames(x), replacement = "")
  colnames(x) <- gsub(pattern = "rand", x = colnames(x),
                      replacement = "Random")
  colnames(x) <- gsub(pattern = "poly", x = colnames(x),
                      replacement = "Time")
  class(x) <- c("ranef.lcc",  "data.frame")
  x
}

#=======================================================================
# logLik
#=======================================================================
##' @rdname logLik.lcc
##' @title Extract Log-Likelihood of an \code{lcc} Object
##' @usage \method{logLik}{lcc}(object, ..., REML)
##' @method logLik lcc
##' @aliases logLik.lcc
##'
##' @return If \code{REML=TRUE}, the default, returns the
##'   restricted log-likelihood value of the linear mixed-effects model;
##'   else the log-likelihood value
##'
##' @param object an object inheriting from class \code{lcc},
##'   representing a fitted longitudinal concordance correlation
##'   function.
##'
##' @param REML an optional logical value.  If \code{TRUE} the
##'   restricted log-likelihood is returned, else, if \code{FALSE}, the
##'   log-likelihood is returned.
##' @param ... further arguments passed to \code{\link{logLik}}.
##'
##' @details See methods for \code{\link{nlme}} objects to get more
##'   details.
##'
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @importFrom stats logLik
##'
##' @seealso \code{\link[lcc]{lcc}}, \code{\link{summary.lcc}}
##'
##' @examples
##'
##' \dontrun{
##' fm1<-lcc(data = hue, subject = "Fruit", resp = "H_mean",
##'          method = "Method", time = "Time", qf = 2, qr = 2)
##' logLik(fm1)
##' }
##'
##' @export

logLik.lcc <- function(object, ..., REML) {
   if (!is.lcc(object))
     stop("use only with \"lcc\" objects" , call. = FALSE)
   logLik(object$model,  REML = REML, ...)
}

#=======================================================================
# ANOVA
# =======================================================================
##' @rdname anova.lcc
##' @title Compare Likelihoods of Fitted Models from an \code{lcc}
##'   Object
##' @usage \method{anova}{lcc}(object, ..., test, type, adjustSigma,
##'   verbose)
##' @method anova lcc
##' @aliases anova.lcc
##'
##' @return If just one \code{lcc} model object is declared, a data
##'   frame with the numerator degrees of freedom, denominator degrees
##'   of freedom, F-values, and P-values for the fixed terms in the
##'   model. Otherwise, when multiple \code{lcc} fitted objects are
##'   being compared, a data frame with the degrees of freedom, the
##'   (restricted) log-likelihood, the Akaike Information Criterion
##'   (AIC), and the Bayesian Information Criterion (BIC) of each object
##'   is returned.
##'
##' @param object an object inheriting from class \code{lcc} or \code{lme},
##'   representing a fitted longitudinal concordance correlation
##'   function.
##'
##' @param ... other optional fitted model objects inheriting from
##'   classes "lcc", or "lme".
##
##' @param test an optional logical value controlling whether likelihood
##'   ratio tests should be used to compare the fitted models
##'   represented by object and the objects in \code{...}. Defaults to
##'   TRUE.
##'
##' @param type an optional character string specifying the type of sum
##'   of squares to be used in F-tests for the terms in the model. If
##'   \code{sequential}, the sequential sum of squares obtained by
##'   including the terms in the order they appear in the model is used;
##'   else, if \code{marginal}, the marginal sum of squares obtained by
##'   deleting a term from the model at a time is used. This argument is
##'   only used when a single fitted object is passed to the
##'   function. Partial matching of arguments is used, so only the first
##'   character needs to be provided. Defaults to \code{sequential}.
##'
##' @param adjustSigma an optional logical value. If \code{TRUE} and the
##'   estimation method used to obtain object was maximum likelihood,
##'   the residual standard error is multiplied by sqrt(nobs/(nobs -
##'   npar)), converting it to a REML-like estimate. This argument is
##'   only used when a single fitted object is passed to the
##'   function. Default is \code{TRUE}.
##'
##' @param verbose an optional logical value. If \code{TRUE}, the
##'   calling sequences for each fitted model object are printed with
##'   the rest of the output, being omitted if \code{verbose =
##'   FALSE}. Defaults to \code{FALSE}.
##'
##' @details This function is an adaptation from the
##'   \code{\link{anova.lme}}. For more details see methods for
##'   \code{\link{nlme}}.
##'
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @seealso \code{\link[lcc]{lcc}}, \code{\link{summary.lcc}}
##'
##' @examples
##' \dontrun{
##' ## Testing random effects
##' fm1.aov <- lcc(data = hue, subject = "Fruit", resp = "H_mean",
##'                method = "Method", time = "Time", qf = 2, qr = 1)
##' fm2.aov <- update(fm1.aov,  qr = 2)
##' anova(fm1.aov, fm2.aov)
##' }
##'
##' @examples
##' \dontrun{
##' # Testing fixed effects
##' fm3.aov <- update(fm2.aov,  REML = FALSE)
##' fm4.aov <- update(fm2.aov,  REML = FALSE,  qf = 3)
##' anova(fm3.aov, fm4.aov)
##' }
##'
##' @examples
##' \dontrun{
##' # Comparing the 3 lcc models
##' fm5.aov <- update(fm2.aov,  var.class = varExp, weights.form = "time")
##' anova(fm1.aov, fm2.aov, fm5.aov)
##' }
##'
##' @importFrom stats formula pchisq pf terms
##' @importFrom utils head tail
##'
##' @export

anova.lcc <- function (object, ..., test = TRUE, type = c("sequential", "marginal"),
                       adjustSigma = TRUE, verbose = FALSE)
{
  fixSig <- attr(object$model$modelStruct, "fixedSigma")
  fixSig <- !is.null(fixSig) && fixSig
  dots <- list(...)
  if ((rt <- length(dots) + 1L) == 1L) {
    if (!inherits(object$model, "lme")) {
      stop("object must inherit from class \"lme\" ")
    }
    vFix <- attr(object$model$fixDF, "varFixFact")
    if (adjustSigma && object$model$method == "ML")
      vFix <- sqrt(object$model$dims$N/(object$model$dims$N - ncol(vFix))) *
        vFix
    c0 <- solve(t(vFix), fixef(object$model))
    assign <- attr(object$model$fixDF, "assign")
    nTerms <- length(assign)
    type <- match.arg(type)
    Fval <- Pval <- double(nTerms)
    nDF <- integer(nTerms)
    dDF <- object$model$fixDF$terms
    for (i in 1:nTerms) {
      nDF[i] <- length(assign[[i]])
      if (type == "sequential") {
        c0i <- c0[assign[[i]]]
      }
      else {
        c0i <- c(qr.qty(qr(vFix[, assign[[i]], drop = FALSE]),
                        c0))[1:nDF[i]]
      }
      Fval[i] <- sum(c0i^2)/nDF[i]
      Pval[i] <- 1 - pf(Fval[i], nDF[i], dDF[i])
    }
    aod <- data.frame(numDF = nDF, denDF = dDF, `F-value` = Fval,
                      `p-value` = Pval, check.names = FALSE)
    rownames(aod) <- names(assign)
    attr(aod, "rt") <- rt
  }
  else {
    ancall <- sys.call()
    ancall$verbose <- ancall$test <- ancall$type <- NULL
    object <- list(object$model, ...)
    termsClass <- vapply(object, data.class, "")
    valid.cl <- c("lme", "lcc")
    if (!all(match(termsClass, valid.cl, 0))) {
      valid.cl <- paste0("\"", valid.cl, "\"")
      stop(gettextf("objects must inherit from classes %s, or %s",
        paste(head(valid.cl, -1), collapse = ", "),
        tail(valid.cl, 1)), domain = NA)
    }
    for (i in 1:length(object)) {
      if (is.lcc(object[[i]])) {
        object[[i]] <- object[[i]]$model
      }else {
        object[[i]] <- object[[i]]
      }
    }
    getResponseFormula <-
      function(object)
      {
        ## Return the response formula as a one sided formula
        form <- formula(object)
        if (!(inherits(form, "formula") && (length(form) == 3))) {
          stop("'form' must be a two-sided formula")
        }
        eval(parse(text = paste("~", deparse(form[[2]]))))
      }
    resp <- vapply(object, function(el) deparse(getResponseFormula(el)[[2L]]),
                   "")
    subs <- as.logical(match(resp, resp[1L], FALSE))
    if (!all(subs))
      warning("some fitted objects deleted because response differs from the first model")
    if (sum(subs) == 1)
      stop("first model has a different response from the rest")
    object <- object[subs]
    rt <- length(object)
    termsModel <- lapply(object, function(el) formula(el)[-2])
    estMeth <- vapply(object, function(el) if (is.null(val <- el[["method"]]))
      NA_character_
    else val, "")
    if (length(uEst <- unique(estMeth[!is.na(estMeth)])) >
      1) {
      stop("all fitted objects must have the same estimation method")
    }
    estMeth[is.na(estMeth)] <- uEst
    REML <- uEst == "REML"
    if (REML) {
      aux <- vapply(termsModel, function(el) {
        tt <- terms(el)
        val <- paste(sort(attr(tt, "term.labels")),
          collapse = "&")
        if (attr(tt, "intercept") == 1)
          paste(val, "(Intercept)", sep = "&")
        else val
      }, ".")
      if (length(unique(aux)) > 1) {
        warning("fitted objects with different fixed effects. REML comparisons are not meaningful.")
      }
    }
    termsCall <- lapply(object, function(el) {
      if (is.null(val <- el$call) && is.null(val <- attr(el,
        "call")))
        stop("objects must have a \"call\" component or attribute")
      val
    })
    termsCall <- vapply(termsCall, function(el) paste(deparse(el),
      collapse = ""), "")
    aux <- lapply(object, logLik, REML)
    if (length(unique(vapply(aux, attr, 1, "nall"))) > 1) {
      stop("all fitted objects must use the same number of observations")
    }
    dfModel <- vapply(aux, attr, 1, "df")
    logLik <- vapply(aux, c, 1.1)
    aod <- data.frame(call = termsCall, Model = 1:rt, df = dfModel,
      AIC = vapply(aux, AIC, 1), BIC = vapply(aux, BIC,
        1), logLik = logLik, check.names = FALSE)
    if (test) {
      ddf <- diff(dfModel)
      if (sum(abs(ddf)) > 0) {
        effects <- rep("", rt)
        for (i in 2:rt) {
          if (ddf[i - 1] != 0) {
            effects[i] <- paste(i - 1, i, sep = " vs ")
          }
        }
        pval <- rep(NA, rt - 1)
        ldf <- as.logical(ddf)
        lratio <- 2 * abs(diff(logLik))
        lratio[!ldf] <- NA
        pval[ldf] <- pchisq(lratio[ldf], abs(ddf[ldf]),
          lower.tail = FALSE)
        aod <- data.frame(aod, Test = effects, L.Ratio = c(NA,
          lratio), `p-value` = c(NA, pval), check.names = FALSE,
          stringsAsFactors = TRUE)
      }
    }
    ## local function for complete deparsing
    c_deparse <- function(...) paste(deparse(..., width.cutoff=500),
                                     collapse="")
    row.names(aod) <- vapply(as.list(ancall[-1L]), c_deparse,
      "")
    attr(aod, "rt") <- rt
    attr(aod, "verbose") <- verbose
  }
  class(aod) <- c("anova.lcc", "data.frame")
  aod
}


##' @rdname print.anova.lcc
##' @title  Print the Anova of an \code{lcc} Object
##' @usage
##' \method{print}{anova.lcc}(x, verbose, ...)
##' @method print anova.lcc
##' @aliases print.anova.lcc
##' @description Method print for the \code{anova.lcc}.
##'
##' @param x an object inheriting from class
##'   \code{\link[lcc]{anova.lcc}}, representing a fitted longitudinal
##'   concordance correlation function.
##'
##' @param verbose an optional logical value used to control the amount
##'   of printed output. If \code{TRUE}, the calling sequences for each fitted
##'   model object are printed with the rest of the output, being omitted
##'   if \code{verbose = FALSE}. Defaults to \code{FALSE}.
##'
##' @param ... further arguments passed to \code{\link{print}}.
##'
##' @details Modified from \code{\link{anova.lme}}. For more details see
##' methods for \code{\link{nlme}}.
##' @return Return no value, called for side effects
##' @author Thiago de Paula Oliveira,
##'   \email{thiago.paula.oliveira@@alumni.usp.br}
##'
##' @seealso \code{\link{summary.lcc}}, \code{\link{lccPlot}},
##'   \code{\link[lcc]{lcc}}
##'
##' @examples
##'
##' \dontrun{
##' ## Second degree polynomial model with random intercept, slope and
##' ## quadratic term
##' fm1<-lcc(data = hue, subject = "Fruit", resp = "H_mean",
##'          method = "Method", time = "Time", qf = 2, qr = 2)
##' print(anova(fm1))
##' }
##'
##' @export

print.anova.lcc <- function(x, verbose = attr(x, "verbose"), ...)
{
  obj <- x
  if ((rt <- attr(x,"rt")) == 1) {
    if (!is.null(lab <- attr(x, "label"))) {
      cat(lab)
    }
    pval <- format(round(x[, "p-value"],4))
    pval[as.double(pval) == 0] <- "<.0001"
    x[, "F-value"] <- format(zapsmall(x[, "F-value"]))
    x[, "p-value"] <- pval
    print(as.data.frame(x), ...)
  } else {
    if (verbose) {
      cat("Call:\n")
      objNams <- row.names(x)
      for(i in 1:rt) {
        cat(" ",objNams[i],":\n", sep ="")
        cat(" ",as.character(x[i,"call"]),"\n")
      }
      cat("\n")
    }
    x <- as.data.frame(x[,-1])
    for(i in names(x)) {
      org <- x[[i]]
      if (i == "p-value") {
        org <- round(org, 4)
        xna <- is.na(org)
        org[!xna] <- format(org[!xna])
        org[as.double(org) == 0] <- "<.0001"
        org[xna] <- ""
      } else {
        if (match(i, c("AIC", "BIC", "logLik", "L.Ratio"), 0)) {
          xna <- is.na(org)
          org <- zapsmall(org)
          org[xna] <- 0
          org <- format(org)
          org[xna] <- ""
        }
      }
      x[[i]] <- org
    }
    print(as.data.frame(x), ...)
  }
  invisible(obj)
}
