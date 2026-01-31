#' Neural Net for Complex Survey Data
#'
#' @description
#' The surveynnet package extends the functionality of nnet (Venables and Ripley, 2002),
#' which already supports survey weights, by enabling it to handle clustered and stratified data.
#' It achieves this by incorporating design effects through the use of effective sample sizes in
#' the calculations, performed by the package described in Valliant et al. (2023), by following
#' the methods outlined by Chen and Rust (2017) and Valliant et al. (2018).
#'
#' @param x Matrix or data frame of predictors. Must not contain any missing values.
#' @param y Vector of targets / response values. Must not contain any missing values.
#' @param weight The weights for each sample.
#' @param strat The stratum for each sample.
#' @param clust The cluster for each sample.
#' @param comp_cases If TRUE, filter out missing values from x, y, weight, strat, and clust. Default FALSE.
#'  Note that in either case, the dimensions of all data mentioned above must agree.
#' @param ... Additional arguments to be passed into `PracTools::deffCR` or `nnet::nnet`. See
#' documentation of those packages and functions for more details. Note that for the neural net (`nnet`),
#' the default here is set to 3 layers ("size" parameter) and maximum iterations ("maxit" parameter) is
#' set to 2000.
#'
#' @return A list containing two objects:
#' * A dataframe with the fitted values of the neural nets, using:
#'  no weights ("fitted"), the user-inputted weights ("fitted_weighted"), and the new method that adjusts the weights by using a design
#' effect incorporating cluster and strata ("fitted_deff").
#' * The fitted neural network object (from `nnet`), using the novel design-effect based weights; this
#' can be used to predict the outcomes for new observations.
#'
#' @references
#' * Chen, S., and K. F. Rust. 2017."An Extension of Kish’s Formula for Design Effects to
#' Two- and Three-Stage Designs with Stratification.”, Journal of Survey Statistics and
#' Methodology,5 (2): 111–30.
#' * Valliant, R., J. A. Dever, and F. Kreuter. 2018. Practical Tools for Designing
#' and Weighting Survey Samples .2nd ed. New York: Springer-Verlag.
#'
#' @export
#'
#' @examples
#'
#' # short example with body fat dataset
#' y <- body_fat$pct_body_fat
#' x <- body_fat[,c("Weight_kg", "Height_cm", "Age")]
#' weight <- body_fat$survey_wt
#' strat <- body_fat$stratum
#' clust <- body_fat$cluster
#' y[strat==1] <- y[strat==1] + 30*0.00015*rnorm(sum(strat==1))
#' y[strat==2] <- y[strat==2] + 30*0.15*rnorm(sum(strat==2))
#'
#' myout <- surveynnet(x,y,weight = weight, strat = strat, clust=clust)
#' myout
#'
#'
#' # NHANES example
#' # Predicting Diastolic BP from BMI, Systolic BP and Height
#' # PLEASE NOTE: for this example, pass "nest=TRUE" into the
#' # "..." parameters of the main function `surveynnet`
#'
#' x <- nhanes.demo[,c("BMXBMI", "BPXSY1", "BMXHT")]
#' weight <- nhanes.demo$WTMEC2YR
#' strat <- nhanes.demo$SDMVSTRA
#' clust <- nhanes.demo$SDMVPSU
#' y <- nhanes.demo$BPXDI1
#' myout <- surveynnet(x,y,weight = weight, strat = strat, clust=clust, nest=TRUE)
#' head(myout$results, 15)
#'
surveynnet <- function(x,y, weight, strat, clust, comp_cases = FALSE, ...){
  # check dimensionality agreement
  stopifnot(
    "x, y, weight, strat and clust must have same lengths/dimensions" =
      dim(x)[1] == length(y) && length(y) == length(weight) &&
      length(weight) == length(strat) && length(strat) == length(clust)
  )
  # filter NA's if desired
  if(comp_cases){
    dftemp = cbind(weight, strat, clust, y, x)
    dftemp = dftemp[stats::complete.cases(dftemp), ]
    weight = dftemp[,1]; strat = dftemp[,2]; clust = dftemp[,3]
    y = dftemp[,4]; x = dftemp[,-(1:4)]
  }
  args <- list(...)
  # a dummy arg for survival
  zz <- survival::Surv(1,1)
  # a dummy line for survey
  MM <- survey::paley(1)
  # get y scale and center for undoing later
  # note for later: need to remove any missing values for y and df right?
  scale.y <- max(y)-min(y)
  center.y <- min(y)
  # scale x and y to 0-1
  x.scale <- apply(x, 2, function(x2) (x2 - min(x2))/(max(x2) - min(x2)))
  y.scale <- (y - center.y) / scale.y
  # calculate design effect
  df.deff <- data.frame(weight = weight, stratum = strat, clust = clust)
  # collect args for deffCR
  args.deffCR <- args[
    intersect(names(args), names(formals(PracTools::deffCR)))
  ]
  # the below-vars are needed and hardcoded
  args.deffCR$strvar = strat
  args.deffCR$y = y.scale
  args.deffCR$clvar = clust
  args.deffCR$w = weight

  deff <- do.call(PracTools::deffCR, args.deffCR)
  # calculate deff.h
  df.deff <- dplyr::left_join(df.deff, deff$`strata components`, by = 'stratum')
  deff.h <- df.deff$deff.w*df.deff$deff.c*df.deff$deff.s
  # calculate adjusted weights
  eff_adj_weight <- weight / deff.h
  # collect args for all 3 nnet calls
  args.nnet <- args[
    intersect(names(args), names(formals(nnet::nnet.default)))
  ]
  args.nnet$trace <- FALSE # adding to suppress iter output
  args.nnet$x <- x.scale
  args.nnet$y <- y.scale
  if(!"size" %in% names(args.nnet)) {
    args.nnet$size = 3
  }
  if(!"maxit" %in% names(args.nnet)) {
    args.nnet$maxit = 2000
  }
  # run nnet without weights, with weights, with effect-adjusted weights
  nn.no_wt <- do.call(nnet::nnet.default, args.nnet)
  # add weights
  args.nnet$weights = weight
  nn.wt <- do.call(nnet::nnet.default, args.nnet)
  # modify weights with new method
  args.nnet$weights = eff_adj_weight
  nn.eff_adj_wt <- do.call(nnet::nnet.default, args.nnet)
  # collect and process results
  results <- data.frame(stratum = strat)
  results$deff.h <- deff.h
  results$survey_wt <- weight
  results$deff_wt <-eff_adj_weight
  #Predicted values
  results$target<- y
  results$fitted <- nn.no_wt$fitted.values*scale.y + center.y
  results$fitted_weighted <- nn.wt$fitted.values*scale.y + center.y
  results$fitted_deff <- nn.eff_adj_wt$fitted.values*scale.y + center.y
  results$fitted_deff_resid <- results$target - results$fitted_deff
  out.results <- list(
    results = results,
    nnet.surv = nn.eff_adj_wt
  )
  class(out.results) <- c("surveynnet", class(out.results))
  return(
    out.results
  )
}
