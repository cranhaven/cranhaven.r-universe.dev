#' @title Fit Function to Data via Nonlinear Regression
#' @description
#'   Fit predefined functions to data via nonlinear least squares using Levenberg-Marquardt
#'   algorithm via \code{\link[minpack.lm]{nlsLM}}.
#' @template dataset
#' @template predictors
#' @template responses
#' @template groups
#' @template auxiliaries
#' @template scriptvars
#' @template returnResults
#' @templateVar packagelink \code{\link{nls}}
#' @template threedots
#' @details
#'   The following script variables are summarized in \code{scriptvars} list:\cr
#'   \describe{
#'     \item{math.fun}{[\code{character(1)}]\cr
#'       Function selection for fitting data. It is possible to choose a predefined model, or
#'       compose a model manually by selecting \code{User Defined}.\cr
#'       Default is \code{User Defined}}
#'     \item{preds.frml}{[\code{character(1)}]\cr
#'       Required if \code{math.fun} is set to \code{User Defined}.
#'       Valid R \code{\link{formula}} for the right hand side (predictors) of the model equation.}
#'     \item{resp.frml}{[\code{character(1)}]\cr
#'       Required if \code{math.fun} is set to \code{User Defined}.
#'       Valid R \code{\link{formula}} for the left hand side (response) of the model equation.}
#'     \item{limits}{[\code{character(1)}]\cr
#'       Optional if \code{math.fun} is set to \code{User Defined}.
#'       Specifies minimum and maximum value for function \code{math.fun} as a comma separated list
#'       of \code{min} and \code{max}.
#'       It is possible to assign variables, e.g. \code{min=a}, which need start values in
#'       \code{start.vals}, as well as real numbers, e.g. \code{min=4.5}, with a period as decimal
#'       separator.}
#'     \item{start.vals}{[\code{character(1)}]\cr
#'       Required if \code{math.fun} is set to \code{User Defined}.
#'       Specify starting values for all terms of the right hand side as a comma separated list
#'       with a period as decimal separator.}
#'     \item{weights}{[\code{character(1)}]\cr
#'       Select a weighting variable from the auxiliary variables.}
#'     \item{max.iter}{Maximum number of iterations.
#'       For details see \code{link[minpack.lm]{nls.lm.control}}}
#'     \item{max.ftol}{Maximum relative error desired in the sum of squares. If \code{0},
#'       the default is used.
#'       For details see \code{link[minpack.lm]{nls.lm.control}}}
#'   }
#' @return
#'   Logical [\code{TRUE}] invisibly and outputs to Cornerstone or, 
#'   if \code{return.results = TRUE}, \code{\link{list}} of 
#'   resulting \code{\link{data.frame}} objects:
#'   \item{coeff}{Estimated coefficients and standard errors for each group.
#'     Convergence information is available for every group (for details see
#'     \code{link[minpack.lm]{nls.lm}}).
#'   }
#'   \item{vcov}{Variance-Covariance matrix of the main coefficients for the fitted model of each
#'     group (for details see \code{link[stats]{vcov}}).
#'   }
#'   \item{predictions}{
#'     Dataset to brush with predictions and residuals added to original values and groups,
#'     if available.
#'   }
#' @export
#' @examples
#' # Generate data from logistic function:
#' fun = function(x, a, b, c, d, sigma = 1) {
#'   a+(b-a) / (1+exp(-d*(x-c))) + rnorm(length(x), sd = sigma)
#'   }
#' library(data.table)
#' dt = data.table(  x1 = sample(seq(-10, 10, length.out = 100))
#'                   , group1 = sample(x = c("A", "B"), replace = TRUE, size = 100)
#'                   )
#' dt[group1 == "A", y1 := fun(x1, 1, 10, 1, 0.6, 0.1)]
#' dt[group1 == "B", y1 := fun(x1, 8, 2, -1, 0.3, 0.1)]
#' # Set script variables
#' scriptvars = list(math.fun = "Logistic", resp.frml = "", preds.frml = "", limits = ""
#'                   , start.vals = "", weights = "", max.iter = 50, max.ftol = 0
#'                   )
#' # Fit the logistic function:
#' res = fitFunction(dt, "x1", "y1", "group1", character(0), scriptvars, TRUE)
#' # Show estimated coefficients:
#' res$coeff
#' # Variance-Covariance matrix:
#' res$vcov
#' # Plot fitted vs. residuals:
#' plot(res$predictions$Fitted, res$predictions$Residuals)
fitFunction = function(dataset = cs.in.dataset()
                       , preds = cs.in.predictors(), resps = cs.in.responses(), groups = cs.in.groupvars()
                       , auxs = cs.in.auxiliaries()
                       , scriptvars = cs.in.scriptvars()
                       , return.results = FALSE
                       , ...
                       ) {
  # convert dataset to data.table
  dtDataset = as.data.table(dataset)
  
  # sanity checks
  assertCharacter(preds, any.missing = FALSE) # specialise at formulas
  assertCharacter(resps, any.missing = FALSE, len = 1)
  assertCharacter(groups, any.missing = FALSE)
  assertCharacter(auxs, any.missing = FALSE)
  assertDataTable(dtDataset)
  assertSetEqual(names(dtDataset), c(preds, resps, groups, auxs))
  # check protected names in dataset, conflicts with data.table usage are possible
  assertDisjunct(names(dtDataset), c("pred", "preds", "resp", "resps", "group", "groups", "brush", "brushed"))
  assertDataTable(dtDataset[, preds, with = FALSE], any.missing = FALSE)
  assertList(scriptvars, len = 8)
  assertChoice(scriptvars$math.fun, c("User Defined", "Logistic"))
  if (scriptvars$math.fun == "User Defined") {
    assertString(scriptvars$resp.frml)
    assertString(scriptvars$preds.frml)
    assertString(scriptvars$limits)
    assertString(scriptvars$start.vals, min.chars = 3)
  }
  assertInt(scriptvars$max.iter, lower = 1)
  assertNumber(scriptvars$max.ftol, lower = 0)
  if (scriptvars$max.ftol == 0) {
    scriptvars$max.ftol = sqrt(.Machine$double.eps)   # default
  }
  assertFlag(return.results)
  
  # generate valid names
  # in contrast to nls, nlsLM does not accept invalid names masked in backticks
  dtVarNames = getMatchingNames(names(dtDataset))
  names(dtDataset) = setMatchingNames(names(dtDataset), dtVarNames)
  preds = setMatchingNames(preds, dtVarNames)
  resps = setMatchingNames(resps, dtVarNames)
  groups = setMatchingNames(groups, dtVarNames)
  auxs = setMatchingNames(auxs, dtVarNames)
  
  # due to non-sense notes in R CMD check
  weighting = Fitted = StopMessage = RMSE = Residuals = pseudoR2 = initial.row.order = NULL
  
  # add rownumber to revert sorting by keys
  dtDataset[, initial.row.order := seq_len(.N)]
  
  # add group with single instance, if missing
  blnNoGroup = FALSE
  if (length(groups) == 0) {
    blnNoGroup = TRUE
    groups = tail(make.unique(c(preds, resps, "grps")), 1)
    dtDataset[, (groups) := "A"]
  }
  # set key for groups
  setkeyv(dtDataset, groups)
  # nlsLM uses missing() to check argument weights, hence weights=NULL is not valid
  # if TRUE, it is set to rep(1, n)
  dtWeights = dtDataset[, groups, with = FALSE]
  if (scriptvars$weights != "") {
    assertSubset(scriptvars$weights, choices = auxs)
    dtWeights[, weighting := dtDataset[[scriptvars$weights]]]
  } else {
    dtWeights[, weighting := 1]
  }
  
  # group values
  grp.vals = unique(dtDataset[, groups, with = FALSE])
  
  # starting values by group
  dtStart = data.table(grp.vals, key = groups)
  # arrange formula, parameters, start values
  if (scriptvars$math.fun == "User Defined") {
    # at least one predictor
    # coefficients without predictor result in an error because of arguments 'weights' in nls
    assertCharacter(preds, any.missing = FALSE, min.len = 1)
    # get limits
    limits = unlist(strsplit(scriptvars$limits, "[,]"))
    limits = strsplit(limits, "[=]")
    limits = lapply(limits, trimws)
    # checks name and values
    if (any(vapply(limits, length, numeric(1)) != 2)   # check combination of name and value
        | any(unlist(lapply(limits, grepl, pattern = "^$")))   # check empty strings
        ) {
      stop(paste0("The string of limits '", scriptvars$limits, "' is malformed.\n"
                  , "It has to be a comma separated list with a combination of 'limit=value'. "
                  , "The decimal separator in 'value' should be a period."))
    }
    # delete NA limits
    i = 1
    while (i <= length(limits)) {
      if (limits[[i]][2] == "NA")
        limits[[i]] = NULL
      else
        i = i + 1
    }
    # duplicates
    limit.names = vapply(limits, head, character(1), n = 1L)
    assertSubset(limit.names, c("min", "max"))
    if (any(duplicated(limit.names)))
      stop("Each limit, i.e. 'min' and 'max', should only occur once.")
    for (i in seq_along(limits)) {
      # extend preds.frml with apply to
      # apply(cbind(limit.value, preds.frml, 1, FUN = min)
      scriptvars$preds.frml = paste0("apply(cbind(", limits[[i]][2], ", ", scriptvars$preds.frml
                                     , "), 1, FUN = ", ifelse(limit.names[i] == "min", "max", "min")
                                     , ")")
    }
    # define formula: sanity check is done implicitly in nls
    frml = paste(scriptvars$resp.frml, scriptvars$preds.frml, sep = "~")
    # get parameter names and start values to dtStart
    start.vals = unlist(strsplit(scriptvars$start.vals, "[,]"))
    start.vals = strsplit(start.vals, "[=]")
    start.vals = lapply(start.vals, trimws)
    if (any(vapply(start.vals, length, numeric(1)) != 2)   # check combination of name and value
        | any(unlist(lapply(start.vals, grepl, pattern = "^$")))   # check empty strings
        ) {
      stop(paste0("The string of start values '", scriptvars$start.vals, "' is malformed.\n"
                  , "It has to be a comma separated list with a combination of 'name=value'. "
                  , "The decimal separator in 'value' should be a period."))
      }
    par.names = vapply(start.vals, head, character(1), n = 1L)
    start.vals = suppressWarnings(as.numeric(vapply(start.vals, tail, character(1), n = 1L)))
    assertNumeric(start.vals, any.missing = FALSE)
    dtStart[, (par.names) := data.table(t(start.vals))]
  } else if (scriptvars$math.fun == "Logistic") {
    # only one predictor
    assertCharacter(preds, any.missing = FALSE, len = 1)
    # numeric preds and resps
    assertDataTable(dtDataset[, c(preds, resps), with = FALSE], types = "numeric")
    # set parameter names and formula
    par.names = c("a", "b", "c", "d")
    frml = paste0("`", resps, "` ~ a+(b-a) / (1+exp(-d*(`", preds, "`-c)))")
    # get starting values for logistic function
    dtStart = merge(dtStart
                    , dtDataset[, head(setorderv(.SD, preds)), by = groups][, lapply(.SD, mean), .SDcols = resps, by = groups]
                    )
    dtStart = merge(dtStart
                    , dtDataset[, tail(setorderv(.SD, preds)), by = groups][, lapply(.SD, mean), .SDcols = resps, by = groups]
                    )
    dtStart = merge(dtStart
                    , dtDataset[, lapply(.SD, stats::median), .SDcols = preds, by = groups]
                    )
    dtStart = cbind(dtStart, 1)
    names(dtStart)[-seq_along(groups)] = par.names
  }
  
  # DateTime or TimeInterval in Predictor: scale by group minimum
  # get columns with POSIXct and difftime types
  posixct.preds = lapply(dtDataset, testMultiClass, classes = c("POSIXct", "difftime"))
  # only predictors are interessting for scaling
  posixct.preds = intersect(names(which(posixct.preds == TRUE)), preds)
  if (length(posixct.preds) > 0) {
    # get group minima
    dtMinDate = dtDataset[, lapply(.SD, min), .SDcols = posixct.preds, by = groups]
    # scale by group minimum
    dtDataset[, (posixct.preds) := lapply(.SD, function(x) x - min(x)), .SDcols = posixct.preds, by = groups]
    dtDataset[, (posixct.preds) := lapply(.SD, as.numeric), .SDcols = posixct.preds]
  }
  
  # match names in forumla
  frml = setMatchingNames(frml, dtVarNames, in.text = TRUE)
  
  # create coefficient data.table
  coeff.names = paste0(rep(c("Coeff_", "StdErr_"), each = length(par.names)) , par.names)
  dtCoeff = data.table(grp.vals, key = groups)
  dtCoeff[, c(coeff.names) := numeric()]
  dtCoeff[, c("pseudoR2", "RMSE") := numeric()]
  dtCoeff[, c("Converged") := logical()]
  dtCoeff[, c("Iterations", "Tolerance", "StopCode") := numeric()]
  dtCoeff[, StopMessage := character()]
  
  # create data.table for Variance-Covariance matrix for coefficients
  dtVcov = data.table(do.call("rbind", replicate(length(par.names), grp.vals, simplify = FALSE)), key = groups)
  dtVcov[, c(par.names) := numeric()]
  
  # resulting fitted and resid
  dtPredictions = data.table(dtDataset[, c(groups, resps, "initial.row.order"), with = FALSE])
  dtPredictions[, c("Fitted", "Residuals") := numeric()]
  
  # loop through groups
  # for (grp in grp.vals[[groups]]) {
  for (row in seq_len(nrow(grp.vals))) {
    # groups are key columns, hence it is possible to use filters on multiple columns directly
    grp = grp.vals[row, ]
    nls.res = try(
      minpack.lm::nlsLM(formula = stats::as.formula(frml)
                        , data = dtDataset[grp, c(preds, resps), with = FALSE]
                        , start = as.list(dtStart[grp, par.names, with = FALSE])
                        , control = minpack.lm::nls.lm.control(
                          ftol = scriptvars$max.ftol
                          , maxiter = scriptvars$max.iter
                        )
                        , weights = dtWeights[grp, weighting]
                        , ...
                        )
      , silent = TRUE)
    if (testClass(nls.res, "try-error")) {
      dtCoeff[grp, StopMessage := nls.res]
      next
    }
    # extract estimated coefficients
    dtCoeff[grp, (coeff.names) := as.list(stats::coef(summary(nls.res))[, 1:2])]
    # pseudo R2
    # https://stackoverflow.com/questions/14530770/calculating-r2-for-a-nonlinear-least-squares-fit
    # https://de.wikipedia.org/wiki/Pseudo-Bestimmtheitsma%C3%9F
    dtCoeff[grp, pseudoR2 := stats::var(stats::fitted.values(nls.res)) / 
              (stats::var(stats::fitted.values(nls.res)) + stats::var(stats::residuals(nls.res)))]
    # RMSE
    dtCoeff[grp, RMSE := summary(nls.res)$sigma]
    # algorithm messages
    dtCoeff[grp, c("Converged", "Iterations", "Tolerance", "StopCode", "StopMessage") := nls.res$convInfo]
    # variance-covariance of coefficients
    dtVcov[grp, (par.names) := as.data.table(stats::vcov(nls.res))]
    # fitted and residulas
    dtPredictions[grp, Fitted := stats::fitted(nls.res)]
    dtPredictions[grp, Residuals := stats::residuals(nls.res)]
  }
  
  # drop grouping variable if no group was passed
  if (blnNoGroup) {
    dtCoeff[, (groups) := NULL]
    dtVcov[, (groups) := NULL]
    dtPredictions[, (groups) := NULL]
  }
  
  # match names of predictions to original values
  names(dtPredictions) = setMatchingNames(names(dtPredictions), dtVarNames, to.original = TRUE)
  # revert to initial row order and remove column
  setkey(dtPredictions, "initial.row.order")
  dtPredictions[, "initial.row.order" := NULL]
  
  # Export to Cornerstone
  cs.out.dataset(dtCoeff, "Coefficient Table")
  cs.out.dataset(dtVcov, "Variance-Covariance Matrix of Coefficients")
  cs.out.dataset(dtPredictions, "Fit Estimate", brush = TRUE)
  # return results
  if (return.results) {
    res = list(coeff = dtCoeff, vcov = dtVcov, predictions = dtPredictions)
    return(res)
  } else {
    invisible(TRUE)
  }
}
