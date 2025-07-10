#' Bayesian ordinal regression analysis Estimate the correlation coefficients of treatment variable,
#' with and without the proportional odds assumption
#'
#' @description
#' Bayesian ordinal regression based on cumulative likelihood function Estimate the correlation coefficients
#' of treatment variable, with or without the proportional odds assumption
#'
#' @param formula a formula expression as for regression models, of the form response ~ predictors.
#' The response should be a factor (preferably an ordered factor), which will be interpreted as an
#' ordinal response with levels ordered as in the factor.
#' @param data a data frame in which to interpret the variables occurring in the formula.
#' @param structure the data structure. i.e., structure = "PO" or structure = "NPO".
#' @param U the desirability of each outcome level
#'
#' @details
#'  This function estimates the coefficients and threshold coefficients.
#'  Specifically, the numerical utilities U reflect the desirability of each outcome
#'  level. To do this, in our example, we first set U[1] = 100 and U[5] = 0, and then
#'  asked physicians to specify numerical values for the intermediate levels, that
#'  reflect their desirability relative to the best and worst levels.
#'
#' @return Bayes_ord() returns the regression coefficients, including: (1) estimator coefficients
#' (2) thresholds coefficients
#'
#' @export
#'
#' @examples
#' \donttest{
#' ### Example One: PO data structure
#' fm1 = Bayes_ord(response~treatment, example.data, "PO")
#'
#' ### Example Two: NPO data structure
#' fm2 = Bayes_ord(response~treatment, example.data, "NPO", U = c(100,80,65,25,10,0))
#' }



Bayes_ord = function(formula, data, structure, U){
  output = list()

  if (missing(U))
    U = NULL

  if (structure == "NPO" & length(U)==0) {
    stop("function needs to specify utility.")
  }

  if (structure == "PO" & is.numeric(U)) {
    stop("structure and Utility should not be specified at the same time.")
  }

  if(structure == "PO"){
    results = jags_po_model(data)
    x = all.vars(formula)[1]
    C = nlevels(data[,match(x, colnames(data))])

    #summary statistics
    coef = results$EST[1,c(1,2)]
    coef_z = coef[1]/coef[2]
    pvalue = 2*pnorm(coef_z)
    bet = c(coef, coef_z, pvalue)
    names(bet) = c("Estimate","Std.Error","z value", "Pr(>|z|)")

    thr_coef = results$EST[2:C,c(1,2)]
    thr_coef_z = thr_coef[,1]/thr_coef[,2]
    gam = cbind(thr_coef, thr_coef_z)
    colnames(gam) = c("Estimate","Std.Error","z value")

    names_gamma = c()
    for (i in 1:length(thr_coef_z)){
      names_gamma = c(names_gamma, paste0(i, "|", i+1))
    }
    rownames(gam) = c(names_gamma)

    output$Coefficients = round(bet, digits = 3)
    output$`Threshold coefficients` = round(gam, digits = 3)
  }else if(structure == "NPO"){
    results = jags_npo_model(data, U)
    x = all.vars(formula)[1]
    C = nlevels(data[,match(x, colnames(data))])

    #summary statistics
    coef = results$EST[1:(length(U)-1),c(1,2)]
    coef_z = coef[,1]/coef[,2]
    pvalue = 2*pnorm(coef_z)
    bet = cbind(coef, coef_z, pvalue)
    colnames(bet) = c("Estimate","Std.Error","z value", "Pr(>|z|)")


    thr_coef = results$EST[C:((C-1)*2),c(1,2)]
    thr_coef_z = thr_coef[,1]/thr_coef[,2]
    gam = cbind(thr_coef, thr_coef_z)
    colnames(gam) = c("Estimate","Std.Error","z value")

    names_gamma = c()
    names_beta  = c()
    for (i in 1:length(thr_coef_z)){
      names_beta = c(names_beta, paste0(i,"|",i+1,".treatment"))
      names_gamma = c(names_gamma, paste0(i, "|", i+1))
    }
    rownames(gam) = c(names_gamma)
    rownames(bet) = c(names_beta)

    output$Coefficients = round(bet, digits = 3)
    output$`Threshold coefficients` = round(gam, digits = 3)
  }
  return(output)
}
