#' Create Descriptive Statistics for Small Area Estimation Report
#'
#' This function estimates the coefficient of variation at level specified,
#' basic statistics such number of units, regions and target areas as well as
#' the threshold on which SAE is applied and the outcome indicator of interest
#' (i.e. poverty line and poverty rate). These indicators are all expressed for
#' the census and survey
#'
#' @param model an object returned by the ebp function of type "emdi ebp",
#' representing point and MSE estimates
#' @param direct an object of type "direct","emdi", representing point
#' and MSE estimates.
#' @param pop_data the population/census/training data
#' @param pop_domains the target area variable within `pop_data`
#' @param smp_data sample data
#' @param threshold  a number defining a threshold. The  argument defaults to
#' \code{NULL}. In this case, the threshold is set to 60\% of the median of the
#' variable that is selected as dependent variable similary to the
#' at-risk-of-poverty rate used in the EU (see also
#' \cite{Social Protection  Committee 2001}). However, any desired threshold can
#' be chosen.
#' @param weights a character string containing the name of a variable that
#' indicates weights in the sample data. If a character string is provided a
#' weighted version of the ebp will be used. The variable has to be numeric.
#' Defaults to NULL.
#' @param pop_weights a character string containing the name of a variable that
#' indicates population weights in the populatation data. If a character string
#' is provided weighted indicators are estimated using population weights.
#' The variable has to be numeric. Defaults to NULL.
#' @param CV_level the variable level at which Coefficient of Variation should
#' be computed
#' @param indicator a character string containing the name of the indicator to
#' compute the Coefficient of Variation for. Defaults to "Head_Count"
#' @return an list containing three dataframes (first dataframe with direct an
#' ebp CV values, second dataframe with basic statistics and third dataframe
#' with national poverline and rate for census and survey
#' @examples
#' \donttest{
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#'
#' # estimate a unit model
#' ebp_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash +
#'                     self_empl + unempl_ben + age_ben + surv_ben + sick_ben +
#'                     dis_ben + rent + fam_allow + house_allow + cap_inv +
#'                     tax_adj,
#'                  pop_data = eusilcA_pop, pop_domains = "district",
#'                  smp_data = eusilcA_smp, smp_domains = "district",
#'                  na.rm = TRUE, weights = "weight",
#'                  pop_weights = "hhsize", MSE = TRUE, weights_type = "nlme",
#'                  B = 2, L = 2)
#'
#' # estimate direct
#' direct_est <- direct(y = "eqIncome", smp_data = eusilcA_smp,
#'                      smp_domains = "district", weights = "weight",
#'                      var = TRUE, B = 2)
#'
#' # descritives
#' ebp_reportdescriptives(model = ebp_model, direct = direct_est,
#'                        smp_data = eusilcA_smp, weights = "weight",
#'                        pop_weights = "hhsize", CV_level = "state",
#'                        pop_data = eusilcA_pop, pop_domains = "district")
#' }
#' @export

ebp_reportdescriptives <- function(model,
                                   direct,
                                   pop_data,
                                   pop_domains,
                                   smp_data,
                                   threshold = NULL,
                                   weights = NULL,
                                   pop_weights = NULL,
                                   CV_level,
                                   indicator = "Head_Count"
){

  if (is.null(threshold)) {
    threshold <- 0.6 * median(model$framework$smp_data[[paste(model$fixed[2])]])
    message(strwrap(prefix = " ", initial = "",
                    paste0("The threshold for the HCR and the PG is
                          automatically set to 60% of the median of the
                          dependent variable and equals ", threshold)))
  }

  hh_varlist <- colnames(model$framework$smp_data)
  if ("benchmark_weights" %in% hh_varlist) {
    hh_varlist <- hh_varlist[- which(hh_varlist == "benchmark_weights")]
  }
  pop_varlist <- hh_varlist[!(hh_varlist %in% c(paste(model$fixed[2]),
                                                weights))]

  # subset the survey and census data
  smp_df <- smp_data[complete.cases(smp_data[,hh_varlist]),
                     c(hh_varlist, CV_level, weights)]
  pop_df <- pop_data[complete.cases(pop_data[,pop_varlist]),
                     c(pop_varlist, CV_level, pop_weights)]

  # Estimate CVs
  colnames(model$MSE)[-1] <- paste0("MSE_", colnames(model$MSE)[-1])

  df <- merge(x = model$MSE[, c("Domain", paste("MSE_",indicator,sep=""))],
              y = model$ind[, c("Domain", indicator)],
              by = "Domain")

  add_df <-
    data.frame(Domain = names(tapply(smp_df[[weights]],
                                     smp_df[[model$framework$smp_domains]],
                                     sum, na.rm = TRUE)),
               weights = tapply(smp_df[[weights]],
                                smp_df[[model$framework$smp_domains]],
                                sum, na.rm = TRUE))

  df <- merge(x = df, y = add_df, by = "Domain")

  add_df <-
    data.frame(Domain = names(tapply(pop_df[[pop_weights]],
                                     pop_df[[pop_domains]],
                                     sum, na.rm = TRUE)),
               pop_weights = tapply(pop_df[[pop_weights]],
                                    pop_df[[pop_domains]],
                                    sum, na.rm = TRUE))

  df <- merge(x = df, y = add_df, by = "Domain")

  # add the CV_level variable to df as well
  pop_df$Domain <- pop_df[[pop_domains]]
  add_df <- unique(pop_df[, c("Domain", CV_level)])

  df <- merge(x = df, y = add_df[, c("Domain", CV_level)], by = "Domain")
  df$CV <- df[,paste("MSE_",indicator,sep="")] / df[,indicator]

  # compute the cvs for census and survey at CV_level level
  naivevar_dt <- direct
  naivevar_dt$MSE$Head_Count_bench <- naivevar_dt$MSE$Head_Count
  naivevar_dt$ind$Head_Count_bench <- naivevar_dt$ind$Head_Count

  naivevar_dt$ind[,paste("Direct_",indicator,"_CV",sep="")] <-
    sqrt(naivevar_dt$MSE[,indicator]) / naivevar_dt$ind[,indicator]


  add_df <- data.frame(unique(df[[CV_level]]))
  colnames(add_df) <- CV_level

  add_df$sum_weights <- tapply(X = df$weights, INDEX = df[[CV_level]],
                               FUN = sum, na.rm = TRUE)
  add_df$sum_pop_weights <- tapply(X = df$pop_weights, INDEX = df[[CV_level]],
                                   FUN = sum, na.rm = TRUE)
  df <- merge(x = df, y = add_df, by = CV_level)

  povrate <- weighted.mean(x = df[,indicator], w = df$weights, na.rm = TRUE)


  df$weights <- df$weights / df$sum_weights
  df$pop_weights <- df$pop_weights / df$sum_pop_weights

  df <- merge(x = df,
              y = naivevar_dt$ind[, c("Domain", paste("Direct_",indicator,"_CV",sep=""))],
              by = "Domain")


  cv_df <-
    data.frame(indicator = paste0("CV for Area: ", unique(df[[CV_level]])),
               ebp_cv = tapply(X = df$CV * df$pop_weights,
                               INDEX = df[[CV_level]], FUN = sum, na.rm = TRUE),
               direct_cv = tapply(X = df[,paste("Direct_",indicator,"_CV",sep="")] * df$weights,
                                  INDEX = df[[CV_level]], FUN = sum,
                                  na.rm = TRUE))

  # compute number of households in census and survey
  basic_df <-
    data.frame(indicator = c("Number of Units","Number of Regions",
                             "Number of Target Areas"),
               census = c(model$framework$N_pop,
                          length(unique(pop_df[[CV_level]][
                            is.na(pop_df[[CV_level]]) == FALSE
                          ])),
                          length(unique(pop_df[[pop_domains]][
                            is.na(pop_df[[model$framework$smp_domains]]) == FALSE
                          ]))),
               survey = c(model$framework$N_smp,
                          length(unique(smp_df[[CV_level]][
                            is.na(smp_df[[CV_level]]) == FALSE
                          ])),
                          length(unique(smp_df[[model$framework$smp_domains]][
                            is.na(smp_df[[model$framework$smp_domains]]) == FALSE
                          ]))))

  basic_df$census <- as.integer(basic_df$census)
  basic_df$survey <- as.integer(basic_df$survey)

  # compute poverty numbers
  smp_data$poor <- ifelse(model$framework$smp_data[[paste(model$fixed[2])]] <
                            threshold, 1, 0)

  smp_data[[weights]] <- smp_data[[weights]] /
    sum(smp_data[[weights]], na.rm = TRUE)

  pov_df <-
    data.frame(indicator = c("National Poverty Rate", "National Poverty Line"),
               model = c(povrate, threshold),
               survey = c(sum(smp_data$poor * smp_data[[weights]]), threshold))

  row.names(cv_df) <- NULL

  return(list(cv_table = cv_df,
              basicinfo_df = basic_df,
              poverty_df = format(pov_df, scientific = FALSE)))


}



#' Perform test for difference between survey and census means
#'
#' This function computes weighted means of the same set of variables within the
#' census and the survey. A test for difference of the means are performed for
#' each variable with two-tailed p-values returned.
#'
#' @param varlist character vector, the set of variables of interest
#' @param pop_data the population data
#' @param smp_data the survey data
#' @param weights a character string containing the name of a variable that
#' indicates weights in the sample data. If a character string is provided
#' a weighted version of the ebp will be used. The variable has to be numeric.
#' Defaults to \code{NULL}.
#' @param pop_weights a character string containing the name of a variable that
#' indicates population weights in the populatation data. If a character string
#' is provided weighted indicators are estimated using population weights.
#' The variable has to be numeric. Defaults to \code{NULL}. Please note that
#' \code{pop_weights} should only be used if the samples and population data
#' are at different levels (e.g.: \code{smp_data} at individual level and
#' \code{pop_data} at household level, then \code{pop_weights} is needed for
#' the comparison with a variable indicating household size).
#' @return dataframe with census and survey means and test results for their
#' difference.
#' @examples
#' \donttest{
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#'
#' variables <- c("gender", "eqsize", "cash", "self_empl",
#'                "unempl_ben", "age_ben", "surv_ben",
#'                "sick_ben", "dis_ben", "rent", "fam_allow",
#'                "house_allow", "cap_inv", "tax_adj")
#'
#' ebp_test_means(varlist = variables,
#'                pop_data = eusilcA_pop,
#'                smp_data = eusilcA_smp,
#'                weights = "weight")
#'
#' }
#'
#' @export

ebp_test_means <- function(varlist,
                           smp_data,
                           pop_data,
                           weights = NULL,
                           pop_weights = NULL){

  if (is.null(weights)) {
    smp_data$weights <- rep(1, nrow(smp_data))
    weights <- "weights"
  }

  if (is.null(pop_weights)) {
    pop_data$pop_weights <- rep(1, nrow(pop_data))
    pop_weights <- "pop_weights"
  }

  smp_df <- smp_data[complete.cases(smp_data[,c(varlist, weights)]),
                     c(varlist, weights)]
  pop_df <- pop_data[complete.cases(pop_data[,c(varlist, pop_weights)]),
                     c(varlist, pop_weights)]

  smp_df <- data.frame(lapply(smp_df, as.numeric))
  pop_df <- data.frame(lapply(pop_df, as.numeric))

  smp_means_df <- data.frame(smp_means = apply(X = smp_df[,varlist],
                                               MARGIN = 2,
                                               FUN = weighted.mean,
                                               w = smp_df[[weights]]),
                             smp_sd = apply(X = smp_df[,varlist],
                                            MARGIN = 2,
                                            FUN = weighted.sd,
                                            w = smp_df[[weights]]),
                             variable = varlist)

  pop_means_df <- data.frame(pop_means = apply(X = pop_df[,varlist],
                                               MARGIN = 2,
                                               FUN = weighted.mean,
                                               w = pop_df[[pop_weights]]),
                             pop_sd = apply(X = pop_df[,varlist],
                                            MARGIN = 2,
                                            FUN = weighted.sd,
                                            w = pop_df[[pop_weights]]),
                             variable = varlist)

  means_df <- merge(smp_means_df, pop_means_df, by = "variable")

  means_df$diff_sd <- sqrt((means_df$smp_sd)^2 + (means_df$pop_sd)^2)
  means_df$diff <- means_df$pop_means - means_df$smp_means
  means_df$zscore <- means_df$diff / means_df$diff_sd
  means_df$pvalue <- 2 * (1 - pnorm(abs(means_df$zscore)))

  return(means_df[, c("variable", "smp_means", "pop_means", "diff", "pvalue")])

}


#' Produce coefficient table for reporting
#'
#' This function takes the object of class 'ebp' to present the regression
#' model results having specified the number of decimal places.
#'
#' @param model an object returned by the ebp function of type "emdi ebp",
#' representing point and MSE estimates
#' @param decimals the number of decimals to report on coefficient estimates
#' @return dataframe with regression model results
#' @examples
#' \donttest{
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#'
#' ebp_model <- ebp(
#'  fixed = eqIncome ~ gender + eqsize + cash + self_empl +
#'    unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow +
#'    house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#'  pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#'  L = 2, na.rm = TRUE)
#'
#'ebp_reportcoef_table(ebp_model, 4)
#'
#' }
#'
#' @export


ebp_reportcoef_table <- function(model,
                                 decimals = 3) {

  options(scipen = 999)

  varname_dt <- as.data.frame(rownames(coef(summary(model$model))))

  colnames(varname_dt) <- "Variable"

  coef_dt <- as.data.frame(coef(summary(model$model)))
  coef_dt <- cbind(varname_dt, coef_dt)

  coef_dt$Sig <- ifelse(coef_dt$`p-value` < 0.001, "***",
                        ifelse(coef_dt$`p-value` < 0.05 &
                                 coef_dt$`p-value` >= 0.001, "**",
                               ifelse(coef_dt$`p-value` < 0.01 &
                                        coef_dt$`p-value` >= 0.05, "*", "")))

  coef_dt$Value <- ifelse(coef_dt$Value < abs(0.0004999999),
                          signif(coef_dt$Value, 2),
                          specify_decimal(coef_dt$Value, decimals))

  coef_dt$StdError <- ifelse(coef_dt$Std.Error < abs(0.0004999999),
                             signif(coef_dt$Std.Error, 2),
                             specify_decimal(coef_dt$Std.Error, decimals))

  coef_dt$Value <- paste0(coef_dt$Value, coef_dt$Sig)

  colnames(coef_dt)[colnames(coef_dt) %in% c("Value", "StdError")] <-
    c("coeff", "std_error")

  rownames(coef_dt) <- seq(nrow(coef_dt))

  return(coef_dt[, c("Variable", "coeff", "std_error")])

}

#' Produce EBP Head Count Population/Rate by Rank
#'
#' This function combines the ebp object with the census data to produce report
#' tables that rank head count estimates either by population of poor or the
#' head count rates themselves in descending order. The function allows the user
#' to select the first/last "x" number of areas by name as well.
#'
#' @param model an object returned by the ebp function of type "emdi ebp".
#' @param pop_data the population/census/training data
#' @param pop_domains a character string containing the name of a variable that
#' indicates domains in the population data. The variable can be numeric or a
#' factor but needs to be of the same class as the variable named in
#' \code{smp_domains}.
#' @param pop_weights a character string containing the name of a variable that
#' indicates population weights in the populatation data. If a character string
#' is provided weighted indicators are estimated using population weights.
#' The variable has to be numeric. Defaults to \code{NULL}. Please note that
#' \code{pop_weights} should only be used if in the \code{pop_data} not
#' individual data is provided and thus the number of persons per unit (e.g.
#' household, grid) must be indicated.
#' @param byrank_indicator if argument is "count", the function ranks the
#' product of Head_Count (from object of class `ebp`) and `pop_weights`,
#' otherwise it the function simply ranks Head_Count output within `ebp` object
#' @param number_to_list an integer, the first `number_to_list` number of
#' target areas to produce from `byrank_indicator` ordering.
#' @param head a logical, if `TRUE` the top `number_to_list` results will be
#' returned and if `FALSE` the bottom `number_to_list` will be returned
#' @param indicator a character string containing the name of the indicator to rank.
#' Defaults to "Head_Count"
#' @return dataframe containing population size, head count values and counts of
#' poor population
#'
#' @examples
#' \donttest{
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#'
#' ebp_model <- ebp(
#'  fixed = eqIncome ~ gender + eqsize + cash + self_empl +
#'     unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow +
#'     house_allow + cap_inv + tax_adj,
#'  pop_data = eusilcA_pop, pop_domains = "district",
#'  smp_data = eusilcA_smp, smp_domains = "district", L = 2,
#'  weights = "weight", weights_type = "nlme", na.rm = TRUE,
#'  pop_weights = "hhsize")
#'
#' # full data of highest population below threshold by rank (descending order)
#' ebp_report_byrank(model = ebp_model,
#'                   pop_data = eusilcA_pop,
#'                   pop_domains = "district",
#'                   pop_weights = "hhsize")
#'
#' # full data of highest rate below threshold by rank (descending order)
#' ebp_report_byrank(model = ebp_model,
#'                   pop_data = eusilcA_pop,
#'                   pop_domains = "district",
#'                   pop_weights = "hhsize",
#'                   byrank_indicator = "rate")
#'
#'# bottom 10 poverty count below threshold by rank (in ascending order)
#' ebp_report_byrank(model = ebp_model,
#'                   pop_data = eusilcA_pop,
#'                   pop_domains = "district",
#'                   pop_weights = "hhsize",
#'                   number_to_list = 10,
#'                   head = FALSE)
#' }
#'
#' @export


ebp_report_byrank <- function(model,
                              pop_data,
                              pop_domains,
                              pop_weights = NULL,
                              byrank_indicator = "count",
                              number_to_list = NULL,
                              head = TRUE,
                              indicator = "Head_Count"){

  ### compute population totals
  pop_data <- pop_data[, c(pop_domains, pop_weights)]

  result_dt <- tapply(X = pop_data[[pop_weights]],
                      INDEX = pop_data[[pop_domains]],
                      FUN = sum,
                      na.rm = TRUE)

  result_dt <- as.data.frame(result_dt)

  result_dt <- data.frame(domain = rownames(result_dt),
                          population = result_dt[[1]])

  pop_data[[pop_domains]] <- as.character(pop_data[[pop_domains]])

  ### include the EBP Head_Count


  result_dt <- merge(x = result_dt,
                     y = model$ind[, c("Domain", indicator)],
                     by.x = "domain",
                     by.y = "Domain")

  result_dt$poor_count <- result_dt[,indicator] * result_dt$population

  ### rank order the table as requested
  if (byrank_indicator == "count") {

    result_dt <- result_dt[order(-result_dt$poor_count),]

  } else {

    result_dt <- result_dt[order(-result_dt[,indicator]),]

  }

  if (is.null(number_to_list)){

    number_to_list <- nrow(result_dt)

  }

  if (head == TRUE) {

    result_dt <- head(result_dt, number_to_list)

  } else if (head == FALSE) {

    result_dt <- tail(result_dt, number_to_list)
  }


  return(result_dt)

}

#' Coefficient of Variation (CV) estimations for Unit EBP Model Headcount
#' Estimates
#'
#' Function \code{ebp_compute_cv} estimates CVs for the headcount of the unit
#' model EBP functions using three different methods. CV, by definition, is the
#' ratio of mean square error of the head count to the  head count estimates.
#' Therefore, the CV types are distinguished by the method of estimating the
#' mean square.
#'
#' Method 1 uses the calibrated/naive bootstrapping of the MSE which allows to
#' calibrate each bootstrap sample on auxiliary information using the
#' \code{direct} function.' Calibrated bootstrap improves on the bias of the
#' naive bootstrap when used in the complex survey context
#' (see \cite{Rao and Wu (1988)}) for more details.
#'
#' Method 2 employs the Horowitz Thompson variance estimation technique to
#' compute MSE i.e. each household is assigned the probability selection within
#' the sample under a given sampling scheme. The computation employs
#' \code{sae::direct} function.
#'
#' Method 3 finally uses the design effect adjusted naive calibrated MSE. The
#' design effect is estimated using the \code{survey::svydesign} function.
#'
#' @param model an object returned by the ebp function of type "emdi ebp",
#' representing point and MSE estimates
#' @param calibvar the calibration variable to be used in method 1
#' @param boot_type the bootstrap type "calibrated" or "naive" to be used in
#' method 1
#' @param designvar the survey design variable to be used in estimating the
#' design effect for method 3.
#' @param threshold  a number defining a threshold. The  argument defaults to
#' \code{NULL}. In this case, the threshold is set to 60\% of the median of the
#' variable that is selected as dependent variable similary to the
#' at-risk-of-poverty rate used in the EU (see also
#' \cite{Social Protection  Committee 2001}). However, any desired threshold can
#' be chosen.
#' @param B number of bootstrap iterations for variance estimation. Defaults
#' to number of bootstrap iteration in ebp obeject (specified in \code{model}).
#' @return dataframe containing different types of CV values for the headcount
#'
#' @examples
#' \donttest{
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#'
#' # estimate a unit model
#' ebp_model <- ebp(fixed = eqIncome ~ gender + eqsize + cash +
#'                     self_empl + unempl_ben + age_ben + surv_ben + sick_ben +
#'                     dis_ben + rent + fam_allow + house_allow + cap_inv +
#'                     tax_adj,
#'                  pop_data = eusilcA_pop, pop_domains = "district",
#'                  smp_data = eusilcA_smp, smp_domains = "district",
#'                  na.rm = TRUE, weights = "weight",
#'                  pop_weights = "hhsize", MSE = TRUE, weights_type = "nlme",
#'                  B = 2, L = 2)
#'
#' # compute CV table
#' ebp_compute_cv(model = ebp_model, calibvar = "gender")
#' }
#'
#'@export
#'@importFrom survey svydesign svymean


ebp_compute_cv <- function(model,
                           calibvar = NULL,
                           boot_type = "calibrate",
                           designvar = NULL,
                           threshold = NULL,
                           B = model$call$B){



  if (is.null(model$call$weights)) {
    model$framework$smp_data$weights <- rep(1, nrow(model$framework$smp_data))
    model$call$weights <- "weights"
  }

  if (is.null(threshold)) {
    threshold <- 0.6 * median(model$framework$smp_data[[paste(model$fixed[2])]])
    message(strwrap(prefix = " ", initial = "",
                    paste0("The threshold for the HCR and the PG is
                          automatically set to 60% of the median of the
                          dependent variable and equals ", threshold)))
  }

  # Direct Estimate with calibration : Mean and CV
  if(!is.null(calibvar)) {
    calibmatrix <- create_calibmatrix(model$framework$smp_data[[calibvar]])

    direct_calib <- povmap::direct(y = as.character(model$fixed[[2]]),
                                   smp_data = model$framework$smp_data,
                                   smp_domains = model$framework$smp_domains,
                                   weights = model$call$weights,
                                   design = designvar, threshold = threshold,
                                   var = TRUE, boot_type = boot_type,
                                   X_calib = calibmatrix, totals = NULL,
                                   na.rm = TRUE, B = B)


    direct_calib <-
      data.frame(Domain = direct_calib$ind$Domain,
                 CB_Head_Count_CV = sqrt(direct_calib$MSE$Head_Count) /
                   direct_calib$ind$Head_Count)
  }


  # HT estimator CV for direct estimate
  direct_ht <- povmap::direct(y = as.character(model$fixed[[2]]),
                              smp_data = model$framework$smp_data,
                              smp_domains = model$framework$smp_domains,
                              weights = model$call$weights,
                              threshold = threshold,
                              var = TRUE, na.rm = TRUE, HT = T)

  direct_ht <-
    data.frame(Domain = direct_ht$ind$Domain,
               Direct_Head_Count = direct_ht$ind$Head_Count,
               HT_Head_Count_CV = sqrt(direct_ht$MSE$Head_Count) /
                 direct_ht$ind$Head_Count)

  # Compute design effect controlled direct estimates and CVs. (direct CV3)
  ## first estimate naive bootstrap, than compute design effect and include psu
  ## list into the ebp data object
  model$framework$smp_data$poor <-
    as.integer(model$framework$smp_data[[as.character(model$fixed[[2]])]] <
                 threshold)
  model$framework$smp_data$weights <-
    model$framework$smp_data[[model$call$weights]]

  if(is.null(designvar)){

    ebpobj_svy <- survey::svydesign(ids = ~1, weights = ~weights, strata = NULL,
                                    survey.lonely.psu = "adjust",
                                    data = model$framework$smp_data)

  } else {

    model$framework$smp_data$designvar <- model$framework$smp_data[[designvar]]

    ebpobj_svy <- survey::svydesign(ids = ~1, weights = ~weights,
                                    strata = ~designvar,
                                    survey.lonely.psu = "adjust",
                                    data = model$framework$smp_data)

  }

  deff_adjust <- survey::svymean(x = ~poor, ebpobj_svy, na = TRUE, deff = TRUE)
  deff_adjust <- attr(deff_adjust, "deff")[1,1]

  direct_naive <- povmap::direct(y = as.character(model$fixed[[2]]),
                                 smp_data = model$framework$smp_data,
                                 smp_domains = model$framework$smp_domains,
                                 design = designvar, weights = model$call$weights,
                                 threshold = threshold, var = TRUE, B = B)

  direct_naive <-
    data.frame(Domain = direct_naive$ind$Domain,
               DesignEffect_CV = sqrt(direct_naive$MSE$Head_Count) /
                 direct_naive$ind$Head_Count)

  # get values for table
  emdi_dt <- povmap::estimators(object = model, indicator = "Head_Count",
                                MSE = FALSE, CV = TRUE)
  result_dt <- emdi_dt$ind
  colnames(result_dt) <- c("Domain", "EBP_Head_Count", "EBP_Head_Count_CV")

  if (!is.null(calibvar)) {
    result_dt <- merge(result_dt, direct_calib, by = "Domain")
  }
  result_dt <- merge(result_dt, direct_ht, by = "Domain")
  result_dt <- merge(result_dt, direct_naive, by = "Domain")

  if (is.null(calibvar)) {
    result_dt <- result_dt[,c("Domain", "Direct_Head_Count", "EBP_Head_Count",
                              "HT_Head_Count_CV", "DesignEffect_CV",
                              "EBP_Head_Count_CV")]
  } else {
    result_dt <- result_dt[,c("Domain", "Direct_Head_Count", "EBP_Head_Count",
                              "HT_Head_Count_CV", "CB_Head_Count_CV",
                              "DesignEffect_CV", "EBP_Head_Count_CV")]
  }

  return(result_dt)
}


#' Output Model fit and normality assumptions
#'
#' The function uses the results of the \code{ebp} function to produce output a
#' table showing marginal R-square, conditional R-squared as well as the
#' skewness and kurtosis of the random and idiosyncratic error terms
#'
#' @param model an object returned by the ebp function of type "emdi ebp"
#' @return dataframe with marginal R-square, conditional R-squared as well as
#' the skewness and kurtosis of the random and idiosyncratic error term
#'
#' @examples
#' \donttest{
#' data("eusilcA_pop")
#' data("eusilcA_smp")
#'
#' ebp_model <- ebp(
#'  fixed = eqIncome ~ gender + eqsize + cash + self_empl +
#'    unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent + fam_allow +
#'    house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
#'  pop_domains = "district", smp_data = eusilcA_smp, smp_domains = "district",
#'  L = 2, na.rm = TRUE
#'  )
#'
#'  ebp_normalityfit(model = ebp_model)
#' }
#'
#' @export
#' @importFrom MuMIn r.squaredGLMM
#' @importFrom moments skewness kurtosis


ebp_normalityfit <- function(model){

  ## compute r-squared
  rsq_model <- model$model

  rsq_model$call$fixed <- model$fixed

  rsq <- suppressWarnings(MuMIn::r.squaredGLMM(rsq_model))

  if (is.matrix(rsq)) {
    r_marginal <- rsq[1, 1]
    r_conditional <- rsq[1, 2]
  } else {
    r_marginal <- rsq[1]
    r_conditional <- rsq[2]
  }

  # include the skewness and kurtosis
  skewness_res <- moments::skewness(residuals(model$model,
                                              level = 0,
                                              type = "pearson"))
  kurtosis_res <- moments::kurtosis(residuals(model$model,
                                              level = 0,
                                              type = "pearson"))

  skewness_ran <- moments::skewness(ranef(model$model)$'(Intercept)')
  kurtosis_ran <- moments::kurtosis(ranef(model$model)$'(Intercept)')


  df <- data.frame(indicator = c("rsq_marginal", "rsq_conditional",
                                 "epsilon_skewness", "epsilon_kurtosis",
                                 "random_skewness", "random_kurtosis"),
                   value = c(r_marginal, r_conditional,
                             skewness_res, kurtosis_res,
                             skewness_ran, kurtosis_ran))

  rownames(df) <- NULL

  return(df)

}