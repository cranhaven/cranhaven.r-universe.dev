### Too many chains of functions to feasibly (quickly) integrate.
### For now, will stay dependent upon MBESS - and accept that that
### will require loading the GSL library.


# ### Taken from MBESS
# ci.reliability <- function (data = NULL, S = NULL, N = NULL, aux = NULL, type = "omega",
#           interval.type = "mlr", B = 10000, conf.level = 0.95)
# {
#   if (is.null(type) || type == "default") {
#     if (!is.null(data) && all(apply(data, 2, function(x) length(table(x))) <=
#                               10)) {
#       type <- 5
#       warnings("Categorical omega is used because your variables look like ordered categorical variables. If not, please specify the 'type' argument.")
#     }
#     else {
#       type <- 4
#       warnings("Hiearachical omega is used by default for covariance matrix input or continuous data input.")
#     }
#   }
#   type1 <- c(1, "alpha", "true score equivalent", "true-score equivalent",
#              "true score", "equivalent", "tau equivalent", "cronbach",
#              "tau-equivalent", "a")
#   type2 <- c(2, "alpha-analytic", "alpha analytic", "alpha factor",
#              "alpha-factor", "alpha cfa", "alpha-cfa", "aa")
#   type3 <- c(3, "omega", "congeneric", "w")
#   type4 <- c(4, "hierarchical omega", "hierarchical", "h")
#   type5 <- c(5, "categorical omega", "categorical", "ordered",
#              "ordered categorical", "c", "cat")
#   type <- tolower(type)
#   if (type %in% type1) {
#     type <- 1
#   }
#   else if (type %in% type2) {
#     type <- 2
#   }
#   else if (type %in% type3) {
#     type <- 3
#   }
#   else if (type %in% type4) {
#     type <- 4
#   }
#   else if (type %in% type5) {
#     type <- 5
#   }
#   else {
#     stop("Please provide a correct type of reliability: 'alpha', 'alpha-analytic', 'omega', 'hierarchical', or 'categorical'.")
#   }
#   interval.type <- .translateinterval.type(interval.type, pos = 1)
#   if (!(is.vector(conf.level) && (length(conf.level) == 1) &&
#         is.numeric(conf.level)))
#     stop("Please put a number in the confidence level!")
#   if (!is.null(data) & !is.null(S)) {
#     stop("Both data and covariance matrix cannot be specified simultaneously.")
#   }
#   else if (!is.null(S)) {
#     return(.ci.reliability.cov(S = S, N = N, type = type,
#                                interval.type = interval.type, conf.level = conf.level))
#   }
#   else if (!is.null(data)) {
#     return(.ci.reliability.data(data = data, aux = aux, type = type,
#                                 interval.type = interval.type, B = B, conf.level = conf.level))
#   }
#   else {
#     stop("Either data or covariance matrix must be specified.")
#   }
# }
#
# ###-----------------------------------------------------------------------------
# ###-----------------------------------------------------------------------------
# ###-----------------------------------------------------------------------------
#
# .translateinterval.type <- function (interval.type, pos = 1)
# {
#   interval.type0 <- c(0, "none", "na")
#   interval.type11 <- c(11, "parallel", "sb")
#   interval.type12 <- c(12, "feldt", "feldt65", "feldt1965",
#                        "f", "fdist", 1)
#   interval.type13 <- c(13, "siotani", "shf", "shf85", "siotani85",
#                        "siotani1985", "f2")
#   interval.type21 <- c(21, "fisher", "naivefisher")
#   interval.type22 <- c(22, "bonett", 2)
#   interval.type23 <- c(23, "hakstian-whalen", "hakstianwhalen",
#                        "hw", "hakstian", "hw76", "hw1976", "cuberoot")
#   interval.type24 <- c(24, "hakstian-barchard", "hakstianbarchard",
#                        "hb", "hb00", "hb2000", "randomitem")
#   interval.type25 <- c(25, "intraclass correlation", "icc",
#                        "modifiedfisher", "improvedfisher")
#   interval.type31 <- c(31, "maximum likelihood (wald ci)",
#                        "ml", "normal-theory", "wald", "normal")
#   interval.type32 <- c(32, "maximum likelihood (logistic ci)",
#                        "mll", "logistic", "normall", "waldl", "normal-theory-l")
#   interval.type33 <- c(33, "robust maximum likelihood (wald ci)",
#                        "mlr", "robust", "robust ml")
#   interval.type34 <- c(34, "robust maximum likelihood (logistic ci)",
#                        "mlrl", "robustl", "robust mll", 3)
#   interval.type35 <- c(35, "asymptotic distribution free (wald ci)",
#                        "adf", "wls")
#   interval.type36 <- c(36, "asymptotic distribution free (logistic ci)",
#                        "adfl", "wlsl")
#   interval.type37 <- c(37, "profile-likelihood", "ll", "likelihood",
#                        "logl")
#   interval.type41 <- c(41, "bootstrap standard error (wald ci)",
#                        "bsi", "bse", "standardboot", "sdboot")
#   interval.type42 <- c(42, "bootstrap standard error (logistic ci)",
#                        "bsil", "bsel", "standardbootl", "sdbootl")
#   interval.type43 <- c(43, "percentile bootstrap", "perc",
#                        "percentile", "percentile ci")
#   interval.type44 <- c(44, "bca bootstrap", "bca", "boot",
#                        "bootstrap", "bias-corrected and acceleration", 4)
#   if (interval.type == "default") {
#     interval.type <- 0
#     warnings("No confidence interval method is specified. Please specify the confidence interval method in the 'interval.type' argument if you wish to have one. For example, interval.type = 'boot'")
#   }
#   if (interval.type %in% interval.type0) {
#     interval.type <- interval.type0[pos]
#   }
#   else if (interval.type %in% interval.type11) {
#     interval.type <- interval.type11[pos]
#   }
#   else if (interval.type %in% interval.type12) {
#     interval.type <- interval.type12[pos]
#   }
#   else if (interval.type %in% interval.type13) {
#     interval.type <- interval.type13[pos]
#   }
#   else if (interval.type %in% interval.type21) {
#     interval.type <- interval.type21[pos]
#   }
#   else if (interval.type %in% interval.type22) {
#     interval.type <- interval.type22[pos]
#   }
#   else if (interval.type %in% interval.type23) {
#     interval.type <- interval.type23[pos]
#   }
#   else if (interval.type %in% interval.type24) {
#     interval.type <- interval.type24[pos]
#   }
#   else if (interval.type %in% interval.type25) {
#     interval.type <- interval.type25[pos]
#   }
#   else if (interval.type %in% interval.type31) {
#     interval.type <- interval.type31[pos]
#   }
#   else if (interval.type %in% interval.type32) {
#     interval.type <- interval.type32[pos]
#   }
#   else if (interval.type %in% interval.type33) {
#     interval.type <- interval.type33[pos]
#   }
#   else if (interval.type %in% interval.type34) {
#     interval.type <- interval.type34[pos]
#   }
#   else if (interval.type %in% interval.type35) {
#     interval.type <- interval.type35[pos]
#   }
#   else if (interval.type %in% interval.type36) {
#     interval.type <- interval.type36[pos]
#   }
#   else if (interval.type %in% interval.type37) {
#     interval.type <- interval.type37[pos]
#   }
#   else if (interval.type %in% interval.type41) {
#     interval.type <- interval.type41[pos]
#   }
#   else if (interval.type %in% interval.type42) {
#     interval.type <- interval.type42[pos]
#   }
#   else if (interval.type %in% interval.type43) {
#     interval.type <- interval.type43[pos]
#   }
#   else if (interval.type %in% interval.type44) {
#     interval.type <- interval.type44[pos]
#   }
#   else {
#     stop("Please provide a correct type of confidence interval. Please see the help page. However, if we were to recommend one, we recommend the 'boot' method.")
#   }
#   interval.type
# }
#
# ###-----------------------------------------------------------------------------
# ###-----------------------------------------------------------------------------
# ###-----------------------------------------------------------------------------
#
# .run.cfa.cov <- function (S, N, varnames, se = "default", equal.loading = FALSE,
#           equal.error = FALSE)
# {
#   q <- length(varnames)
#   if (equal.loading) {
#     loadingName <- rep("a1", q)
#   }
#   else {
#     loadingName <- paste("a", 1:q, sep = "")
#   }
#   if (equal.error) {
#     errorName <- rep("b1", q)
#   }
#   else {
#     errorName <- paste("b", 1:q, sep = "")
#   }
#   model <- paste0("f1 =~ NA*", varnames[1], " + ")
#   loadingLine <- paste(paste(loadingName, "*", varnames, sep = ""),
#                        collapse = " + ")
#   factorLine <- "f1 ~~ 1*f1\n"
#   errorLine <- paste(paste(varnames, " ~~ ", errorName, "*",
#                            varnames, sep = ""), collapse = "\n")
#   sumLoading <- paste("loading :=", paste(loadingName, collapse = " + "),
#                       "\n")
#   sumError <- paste("error :=", paste(errorName, collapse = " + "),
#                     "\n")
#   relia <- "relia := (loading^2) / ((loading^2) + error) \n"
#   model <- paste(model, loadingLine, "\n", factorLine, errorLine,
#                  "\n", sumLoading, sumError, relia)
#   e <- try(fit <- lavaan::cfa(model, sample.cov = S, estimator = "default",
#                               sample.nobs = N, se = se), silent = TRUE)
#   converged <- FALSE
#   if (is(e, "try-error")) {
#     converged <- TRUE
#   }
#   else {
#     converged <- fit@Fit@converged
#     errorcheck <- diag(lavaan::inspect(fit, "se")$theta)
#     if (se != "none" && any(errorcheck <= 0))
#       converged <- FALSE
#   }
#   if (converged) {
#     loading <- unique(as.vector(lavaan::inspect(fit, "coef")$lambda))
#     error <- unique(diag(lavaan::inspect(fit, "se")$theta))
#     pe <- lavaan::parameterEstimates(fit)
#     r <- which(pe[, "lhs"] == "relia")
#     u <- pe[which(pe[, "lhs"] == "loading"), "est"]
#     v <- pe[which(pe[, "lhs"] == "error"), "est"]
#     est <- pe[r, "est"]
#     if (se == "none") {
#       paramCov <- NULL
#       stderr <- NA
#     }
#     else {
#       paramCov <- lavaan::vcov(fit)
#       stderr <- pe[r, "se"]
#     }
#     if ("fmi" %in% colnames(pe)) {
#       fmi <- pe[, "fmi"]
#       N <- N * (1 - mean(fmi, na.rm = TRUE))
#     }
#   }
#   else {
#     loading <- NA
#     error <- NA
#     if (se == "none") {
#       paramCov <- NULL
#     }
#     else {
#       paramCov <- NA
#     }
#     u <- NA
#     v <- NA
#     est <- NA
#     stderr <- NA
#   }
#   result <- list(load = loading, error = error, vcov = paramCov,
#                  converged = converged, u = u, v = v, relia = est, se = stderr,
#                  effn = ceiling(N))
#   return(result)
# }
#
# ###-----------------------------------------------------------------------------
# ###-----------------------------------------------------------------------------
# ###-----------------------------------------------------------------------------
#
# .ci.reliability.data <- function (data = NULL, aux = NULL, type = NULL, interval.type = NULL,
#           B = 1000, conf.level = 0.95)
# {
#   if (!is.data.frame(data) & !is.matrix(data))
#     stop("data must be in a data frame or matrix format.")
#   if (interval.type %in% c(41, 42, 43, 44, 45) && !(is.vector(B) &&
#                                                     (length(B) == 1) && is.numeric(B)))
#     stop("Please put a number in the number of bootstrap argument!")
#   alpha <- 1 - conf.level
#   varnames <- colnames(data)
#   if (!is.null(aux)) {
#     if (is.null(varnames))
#       stop("'data' should have variable names because the list of auxiliary variables is indicated.")
#     if (!all(aux %in% varnames))
#       stop("Some (or all) auxiliary variables are not in the specified data set.")
#     if (type == 5)
#       stop("Categorical CFA does not support the auxiliary variable feature because direct maximum likelihood is not used. Multiple imputation should be used instead.")
#     varnames <- setdiff(varnames, aux)
#   }
#   else {
#     if (is.null(varnames)) {
#       varnames <- paste0("y", 1:ncol(data))
#       colnames(data) <- varnames
#     }
#   }
#   relia <- NA
#   se <- NA
#   ci.lower <- NA
#   ci.upper <- NA
#   N <- nrow(data)
#   q <- length(varnames)
#   if (type == 1) {
#     S <- .findS(data, varnames, aux = aux)
#     relia <- .find.alpha(S)
#     N <- attr(S, "effn")
#   }
#   else if (type == 2) {
#     temp <- .run.cfa(data, varnames, aux, estimator = "mlr",
#                      missing = "ml", se = "none", equal.loading = TRUE)
#     relia <- temp$relia
#     N <- temp$effn
#   }
#   else if (type == 3) {
#     temp <- .run.cfa(data, varnames, aux, estimator = "mlr",
#                      missing = "ml", se = "none")
#     relia <- temp$relia
#     N <- temp$effn
#   }
#   else if (type == 4) {
#     relia <- .getH(data, varnames, aux = aux, estimator = "mlr",
#                    se = "none", missing = "ml")
#     N <- attr(relia, "effn")
#     attr(relia, "effn") <- NULL
#   }
#   else if (type == 5) {
#     relia <- .catOmega(dat = data)
#   }
#   crit <- qnorm(1 - (1 - conf.level)/2)
#   if (interval.type == 11) {
#     if (type == 1) {
#       temp <- .interval.type11_type1(relia, q, N, crit)
#       se <- temp[1]
#       ci.lower <- temp[2]
#       ci.upper <- temp[3]
#     }
#     else if (type == 2) {
#       temp <- .run.cfa(data, varnames, aux, estimator = "mlr",
#                        missing = "ml", se = "default", equal.loading = TRUE,
#                        equal.error = TRUE)
#       if (!is.na(temp$relia))
#         relia <- temp$relia
#       se <- temp$se
#       ci.lower <- relia - (crit * se)
#       ci.upper <- relia + (crit * se)
#     }
#     else {
#       stop("Coefficient omega, hierarchical omega, and categorical omega do not work with the parallel method for interval estimation.")
#     }
#   }
#   else if (interval.type == 12) {
#     if (type %in% 1:4) {
#       result <- .fMethod(relia, N - 1, (N - 1) * (q - 1),
#                          conf.level)
#       ci.lower <- result$ci.lower
#       ci.upper <- result$ci.upper
#     }
#     else {
#       stop("This interval estimation method was not designed for categorical omega.")
#     }
#   }
#   else if (interval.type == 13) {
#     if (type %in% 1:4) {
#       result <- .fMethod(relia, N, N * (q - 1), conf.level)
#       ci.lower <- result$ci.lower
#       ci.upper <- result$ci.upper
#     }
#     else {
#       stop("This interval estimation method was not designed for categorical omega.")
#     }
#   }
#   else if (interval.type == 21) {
#     result <- .fisherCIrelia(relia, N, crit)
#     se <- result$se
#     ci.lower <- result$ci.lower
#     ci.upper <- result$ci.upper
#   }
#   else if (interval.type == 22) {
#     result <- .iccCIrelia(relia, N, q, crit, bonett = TRUE)
#     se <- result$se
#     ci.lower <- result$ci.lower
#     ci.upper <- result$ci.upper
#   }
#   else if (interval.type == 23) {
#     S <- .findS(data, varnames, aux = aux)
#     result <- .hkCIrelia(relia, S, q, N, crit, correct = FALSE)
#     se <- result$se
#     ci.lower <- result$ci.lower
#     ci.upper <- result$ci.upper
#   }
#   else if (interval.type == 24) {
#     S <- .findS(data, varnames, aux = aux)
#     result <- .hkCIrelia(relia, S, q, N, crit, correct = TRUE)
#     se <- result$se
#     ci.lower <- result$ci.lower
#     ci.upper <- result$ci.upper
#   }
#   else if (interval.type == 25) {
#     result <- .iccCIrelia(relia, N, q, crit, bonett = FALSE)
#     se <- result$se
#     ci.lower <- result$ci.lower
#     ci.upper <- result$ci.upper
#   }
#   else if (interval.type %in% 31:36) {
#     estimator <- "ml"
#     logistic <- interval.type %in% c(32, 34, 36)
#     if (interval.type %in% 33:34)
#       estimator <- "mlr"
#     if (interval.type %in% 35:36)
#       estimator <- "wls"
#     if (type == 1) {
#       if (interval.type %in% 31:32) {
#         S <- .findS(data, varnames, aux = aux)
#         result <- .interval.type31_32_type1(relia, S,
#                                             q, N, crit, logistic = logistic)
#         se <- result$se
#         ci.lower <- result$ci.lower
#         ci.upper <- result$ci.upper
#       }
#       else if (interval.type %in% 33:34) {
#         stop("MLR estimator is not available for coefficient alpha")
#       }
#       else {
#         if (any(is.na(data)))
#           stop("data should not have any missing values to use this adf method.")
#         se <- .seReliabilityAdf(data)
#         if (logistic) {
#           temp <- .logisticT(relia, se, crit)
#           ci.upper <- temp[2]
#           ci.lower <- temp[1]
#         }
#         else {
#           ci.lower <- relia - (crit * se)
#           ci.upper <- relia + (crit * se)
#         }
#       }
#     }
#     else if (type == 2) {
#       result <- .run.cfa(data, varnames, aux, estimator = estimator,
#                          missing = "ml", se = "default", equal.loading = TRUE)
#       if (!is.na(result$relia))
#         relia <- result$relia
#       se <- result$se
#       if (logistic) {
#         temp <- .logisticT(result$relia, result$se, crit)
#         ci.upper <- temp[2]
#         ci.lower <- temp[1]
#       }
#       else {
#         ci.lower <- relia - (crit * se)
#         ci.upper <- relia + (crit * se)
#       }
#     }
#     else if (type == 3) {
#       result <- .run.cfa(data, varnames, aux, estimator = estimator,
#                          missing = "ml", se = "default")
#       se <- result$se
#       if (logistic) {
#         temp <- .logisticT(result$relia, result$se, crit)
#         ci.upper <- temp[2]
#         ci.lower <- temp[1]
#       }
#       else {
#         ci.lower <- relia - (crit * se)
#         ci.upper <- relia + (crit * se)
#       }
#     }
#     else {
#       stop("Any normal-theory or ADF methods are not available for hierarchical omega or categorical omega.")
#     }
#   }
#   else if (interval.type == 37) {
#     if (!is.null(aux))
#       stop("The profile-likelihood method does not support the auxiliary-variable feature.")
#     if (type == 1) {
#       result <- .llDataAlpha(data, conf.level = conf.level)
#       ci.lower <- result$ci.lower
#       ci.upper <- result$ci.upper
#     }
#     else if (type == 2) {
#       result <- .llDataOmega(data, eqload = TRUE, conf.level = conf.level)
#       ci.lower <- result$ci.lower
#       ci.upper <- result$ci.upper
#     }
#     else if (type == 3) {
#       result <- .llDataOmega(data, eqload = FALSE, conf.level = conf.level)
#       ci.lower <- result$ci.lower
#       ci.upper <- result$ci.upper
#     }
#     else {
#       stop("The profile-likelihood confidence interval method is not available for hierarchical and categorical omega.")
#     }
#   }
#   else if (interval.type %in% 41:44) {
#     boot.out <- NULL
#     .bs1 <- function(data, i, varnames) {
#       S <- cov(data[i, varnames])
#       .find.alpha(S)
#     }
#     .bs1miss <- function(data, i, varnames, aux) {
#       S <- .findS(data[i, ], varnames = varnames, aux = aux)
#       .find.alpha(S)
#     }
#     .bs2 <- function(data, i, varnames, aux) {
#       temp <- .run.cfa(data[i, ], varnames = varnames,
#                        aux = aux, estimator = "mlr", missing = "ml",
#                        se = "none", equal.loading = TRUE)
#       relia <- temp$relia
#     }
#     .bs3 <- function(data, i, varnames, aux) {
#       temp <- .run.cfa(data[i, ], varnames = varnames,
#                        aux = aux, estimator = "mlr", missing = "ml",
#                        se = "none", equal.loading = FALSE)
#       relia <- temp$relia
#     }
#     .bs4 <- function(data, i, varnames, aux) {
#       result <- .getH(data[i, ], varnames = varnames, aux = aux,
#                       estimator = "mlr", se = "none", missing = "ml")
#       attr(result, "effn") <- NULL
#       result
#     }
#     .bs5 <- function(data, i, varnames) {
#       .catOmega(dat = data[i, varnames])
#     }
#     if (type == 1) {
#       if (any(is.na(data))) {
#         boot.out <- boot::boot(data = data, statistic = .bs1miss,
#                                R = B, stype = "i", varnames = varnames, aux = aux)
#       }
#       else {
#         boot.out <- boot::boot(data = data, statistic = .bs1,
#                                R = B, stype = "i", varnames = varnames)
#       }
#     }
#     else if (type == 2) {
#       boot.out <- boot::boot(data = data, statistic = .bs2,
#                              R = B, stype = "i", varnames = varnames, aux = aux)
#     }
#     else if (type == 3) {
#       boot.out <- boot::boot(data = data, statistic = .bs3,
#                              R = B, stype = "i", varnames = varnames, aux = aux)
#     }
#     else if (type == 4) {
#       boot.out <- boot::boot(data = data, statistic = .bs4,
#                              R = B, stype = "i", varnames = varnames, aux = aux)
#     }
#     else if (type == 5) {
#       boot.out <- boot::boot(data = data, statistic = .bs5,
#                              R = B, stype = "i", varnames = varnames)
#     }
#     else {
#       stop("Something was wrong at the if..else in bootstrap.")
#     }
#     ci.output <- NULL
#     se <- apply(boot.out$t, 2, sd, na.rm = TRUE)
#     if (interval.type == 41) {
#       ci.lower <- relia - (crit * se)
#       ci.upper <- relia + (crit * se)
#     }
#     else if (interval.type == 42) {
#       temp <- .logisticT(relia, se, crit)
#       ci.upper <- temp[2]
#       ci.lower <- temp[1]
#     }
#     else if (interval.type == 43) {
#       ci.output <- boot::boot.ci(boot.out = boot.out, conf = conf.level,
#                                  type = "perc")$perc
#       ci.lower <- ci.output[4]
#       ci.upper <- ci.output[5]
#     }
#     else if (interval.type == 44) {
#       if (B < nrow(data))
#         warnings("The number of bootstrap samples is less than the number of cases.\nIf 'bca' cannot be calculated, please make sure that the number of cases is greater than\nthe number of bootstrap samples.")
#       ci.output <- boot::boot.ci(boot.out = boot.out, conf = conf.level,
#                                  type = "bca")$bca
#       ci.lower <- ci.output[4]
#       ci.upper <- ci.output[5]
#     }
#   }
#   if (!is.na(ci.lower) && ci.lower < 0)
#     ci.lower <- 0
#   if (!is.na(ci.upper) && ci.upper > 1)
#     ci.upper <- 1
#   out <- list(est = relia, se = se, ci.lower = ci.lower, ci.upper = ci.upper,
#               conf.level = conf.level, type = c("alpha", "alpha-cfa",
#                                                 "omega", "hierarchical omega", "categorical omega")[type],
#               interval.type = .translateinterval.type(interval.type,
#                                                       pos = 2))
#   out
# }
#
# ###-----------------------------------------------------------------------------
# ###-----------------------------------------------------------------------------
# ###-----------------------------------------------------------------------------
#
# .run.cfa <- function (data, varnames, aux, estimator = "default", se = "default",
#           missing = "default", equal.loading = FALSE, equal.error = FALSE)
# {
#   q <- length(varnames)
#   N <- nrow(data)
#   if (equal.loading) {
#     loadingName <- rep("a1", q)
#   }
#   else {
#     loadingName <- paste("a", 1:q, sep = "")
#   }
#   if (equal.error) {
#     errorName <- rep("b1", q)
#   }
#   else {
#     errorName <- paste("b", 1:q, sep = "")
#   }
#   model <- paste0("f1 =~ NA*", varnames[1], " + ")
#   loadingLine <- paste(paste(loadingName, "*", varnames, sep = ""),
#                        collapse = " + ")
#   factorLine <- "f1 ~~ 1*f1\n"
#   errorLine <- paste(paste(varnames, " ~~ ", errorName, "*",
#                            varnames, sep = ""), collapse = "\n")
#   sumLoading <- paste("loading :=", paste(loadingName, collapse = " + "),
#                       "\n")
#   sumError <- paste("error :=", paste(errorName, collapse = " + "),
#                     "\n")
#   relia <- "relia := (loading^2) / ((loading^2) + error) \n"
#   model <- paste(model, loadingLine, "\n", factorLine, errorLine,
#                  "\n", sumLoading, sumError, relia)
#   if (!is.null(aux)) {
#     e <- try(fit <- semTools::cfa.auxiliary(model, data = data,
#                                             aux = aux, missing = missing, se = se, estimator = estimator),
#              silent = TRUE)
#   }
#   else {
#     e <- try(fit <- lavaan::cfa(model, data = data, missing = missing,
#                                 se = se, estimator = estimator), silent = TRUE)
#   }
#   converged <- FALSE
#   if (is(e, "try-error")) {
#     converged <- FALSE
#   }
#   else {
#     converged <- fit@Fit@converged
#     errorcheck <- diag(lavaan::inspect(fit, "se")$theta)
#     if (se != "none" && any(errorcheck <= 0))
#       converged <- FALSE
#   }
#   if (converged) {
#     loading <- unique(as.vector(lavaan::inspect(fit, "coef")$lambda))
#     error <- unique(diag(lavaan::inspect(fit, "se")$theta))
#     pe <- lavaan::parameterEstimates(fit)
#     r <- which(pe[, "lhs"] == "relia")
#     u <- pe[which(pe[, "lhs"] == "loading"), "est"]
#     v <- pe[which(pe[, "lhs"] == "error"), "est"]
#     est <- pe[r, "est"]
#     if (se == "none") {
#       paramCov <- NULL
#       stderr <- NA
#     }
#     else {
#       paramCov <- lavaan::vcov(fit)
#       stderr <- pe[r, "se"]
#     }
#     if ("fmi" %in% colnames(pe)) {
#       fmi <- pe[, "fmi"]
#       N <- N * (1 - mean(fmi, na.rm = TRUE))
#     }
#   }
#   else {
#     loading <- NA
#     error <- NA
#     if (se == "none") {
#       paramCov <- NULL
#     }
#     else {
#       paramCov <- NA
#     }
#     u <- NA
#     v <- NA
#     est <- NA
#     stderr <- NA
#   }
#   result <- list(load = loading, error = error, vcov = paramCov,
#                  converged = converged, u = u, v = v, relia = est, se = stderr,
#                  effn = ceiling(N))
#   return(result)
# }
#
# ###-----------------------------------------------------------------------------
# ###-----------------------------------------------------------------------------
# ###-----------------------------------------------------------------------------
#
# ###-----------------------------------------------------------------------------
# ###-----------------------------------------------------------------------------
# ###-----------------------------------------------------------------------------
#
