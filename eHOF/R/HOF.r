#' Hierarchical logistic regression
#'
#' @name HOF
#' @aliases HOF HOF.default HOF.list HOF.data.frame pick.model pick.model.HOF pick.model.HOF.list print.HOF print.HOF.list
#'
#' @description
#' Fit seven hierarchical logistic regression models and select the most appropriate model
#' by information criteria and a bootstrap approach to guarantee model stability.
#'  The first five shapes are known as Huisman-Olff-Fresco (HOF) models in ecology (Huisman et al. 1993).
#'  Additionally the package provides two bimodal shapes.
#'
#' @param occ species occurrences (=response) vector.
#' @param veg vegetation data frame. Either as matrix with species in columns and plots in rows, or in Turboveg format.
#' @param grad gradient data vector.
#' @param M maximum attainable value in the HOF model, similar to binomial denominator.
#' @param y.name name of the species.
#' @param family error distribution. Alternatives are \code{binomial}, \code{poisson} and \code{gaussian}.
#' @param lim limiting criterion for minimization function, see details.
#' @param bootstrap number of bootstrap samplings to check model robustness, see details.
#' @param freq.limit lowest frequency of species analysed.
#' @param object HOF model object, results from function HOF. Either for a single species or a list for several species.
#' @param level probability for model selection (1-P) for F or Chisq tests.
#' @param test information criterion for model selection. Alternatives are \code{"AICc"}, \code{"BIC"}, \code{"AIC"} or just \code{"Dev"iance}.
#' @param modeltypes vector of model types, when only a subset of the seven implemented modeltypes should be considered.
#' @param penal penalty term for model types, default is the number of model parameter.
#' @param gam calculate AIC of GAM model and compare it with HOF models.
#' @param selectMethod choose the model type selection method in case of divergence of the most frequent bootstrapped model or the one with highest information criterion weight from the primary chosen type.
#' @param silent messages about selectMethod
#' @param model You can specify the HOF model type to be used, otherwise it is selected through function [pick.model()].
#' @param newdata vector of gradient values to use
#' @param \dots further arguments passed to or from other methods
#'
#' @details
#'   The minimization function changed compared to package gravy (Oksanen 2002) from nlm to nlminb to be able to set a limit
#'   for estimated parameters (default= -100 to 100). The old models III and V have been often too sharp,
#'   \code{lim=Inf} will produce results similar to gravy.
#'   Function [pick.model()] finds the most adequate modeltype according to the chosen Information Criterion (AICc is default).
#'   Function \code{fitted} returns the fitted values for the used \code{grad}ient, and \code{predict} for any values in \code{newdata}.
#'
#'   To improve and check model stability a bootstrapping mechanism is implemented in function HOF. If the initially chosen model type is different from the most frequent one, the latter will be chosen by default. Bootstrapping is done with sample(length(grad), replace = TRUE).
#'
#' @returns
#'   \code{HOF.fit} returns an object of class \code{"HOF"} which contains the parameters of the call,
#'   the fitting results for every model type and a vector of chosen model types from bootstrapping.
#'
#' @seealso
#'   [plot.HOF()] provides advanced plotting schemes for HOF models.
#'   [Para()] derives model parameters like optimum, niche (width), slope etc.
#'
#' @references
#'   Jansen, F. & Oksanen, J. (2013) What shape are species responses along ecological gradients?
#'     - Huisman-Olf-Fresco models revisited. Journal of Vegetation Science, DOI: 10.1111/jvs.12050
#'
#'   Oksanen, J. & Minchin, P.R. (2002). Continuum theory revisited: what shape are species responses along ecological gradients?
#'     \emph{Ecological  Modelling} 157, 119-129.
#'
#'   Huisman, J., Olff, H. & Fresco, L.F.M. (1993). A hierarchical set of models for species response analysis.
#'   \emph{Journal of Vegetation Science} 4, 37-46.
#'
#'
#' @author Florian Jansen, Jari Oksanen
#'
#' @examples
#'   data(acre)
#'   sel <- c('MATRREC', 'RUMEACT', 'SILENOC', 'APHAARV', 'MYOSARV', 'DESUSOP', 'ARTE#VU')
#'   mo <- HOF(acre[,match(sel, names(acre))], acre.env$PH_KCL, M = 1, bootstrap = NULL)
#'   mo
#'
#' @export
#' @rdname HOF
"HOF" <- function(...) UseMethod("HOF")
#'
#' @rdname HOF
#' @export
HOF.default <- function(
    occ,
    grad,
    M = max(occ),
    y.name,
    family=binomial,
    lim=100,
    bootstrap=100,
    test = c('AICc', 'BIC', 'AIC','Dev'),
    modeltypes = eHOF.modelnames,
    ...)  {
  if(any(c('data.frame', 'matrix','list') %in% class(occ))) stop('Performance data for HOF.default must be a vector.')
  x.name <- deparse(substitute(grad))
  if (missing(y.name)) y.name <- deparse(substitute(occ))
  if(any(is.na(occ))) stop('NA in occurrence vector is not allowed!')
  if(!is.numeric(grad)) print('Gradient must be a numeric vector')
  if(!is.null(bootstrap))
    if(bootstrap == 0) stop('If you do not want to bootstrap your data use "bootstrap=NULL not 0"!') else
      options(eHOF.bootselectmessage = FALSE)
  out <- HOF.model(occ, grad, M, y.name, x.name, family=family, lim=lim,...)

  IC.weights <- function(x, test = 'AICc') {
    p <- sapply(x$models, function(x) length(x$par))
    k <- if(test == 'BIC') log(x$nobs) else 2
    if(test == 'AICc') ic <- -2 * logLik(x) + k * p + (2*k*(k + 1))/(x$nobs - k - 1)
    if(test %in% c('AIC', 'BIC'))   ic <- -2 * logLik(x) + k * p
    if (test == "Dev")   ic <- deviance(x)
    ic.W <- round(exp(-0.5 * ic)/ sum(exp(-0.5 * ic), na.rm=TRUE), 4)
    return(ic.W)
  }

  if(!is.null(bootstrap)) {
    test <- match.arg(test)
    rejectedmodels <- sapply(out$models, function(x) is.na(x$deviance))
    modeltypes <- modeltypes[!modeltypes %in% eHOF.modelnames[rejectedmodels]]
    bootmodels <- character(length=bootstrap)
    mods <- vector('list', length=bootstrap)
    weights <-  matrix(nrow=bootstrap, ncol=7); colnames(weights) <- eHOF.modelnames
    pb <- txtProgressBar (min = 0, max = bootstrap, char = '.',  width = 45, style = 3)
    for(i in 1:bootstrap) {
      take <- sample(length(grad), replace=TRUE)
      mods[[i]] <- HOF.model(occ[take], grad[take], M=M, y.name, x.name, bootstrap=NULL, family=family, lim=lim, ...)
      bootmodels[i] <- pick.model(mods[[i]], test=test, selectMethod = 'pick.model', modeltypes=modeltypes, ...)
      weights[i,] <- IC.weights(mods[[i]], test=test)
      setTxtProgressBar(pb, bootstrap - (bootstrap - i))
      #      for(m in 1:7) mods[[i]]$models[[m]]['fitted'] <- NULL
    }
    close (pb) ## Close progress bar
    out$call <- match.call()
    #    out$bootmods <- mods
    out$bootstraptest <- test
    out$bootstrapmodels <- bootmodels
    out$ICweights <- weights
  } else options(bootselectmessage = TRUE)
  out
}
#'
#' @noRd
#' @export
"HOF.list" <- function(x, s, ...) {
    out <- NextMethod("[", drop=TRUE)
    class(out) <- if(length(s) > 1) 'HOF.list' else 'HOF'
    return(out)
  }
