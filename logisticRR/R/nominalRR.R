#' Print adjusted relative risk under nominal exposure variable.
#'
#' @param formula a formula term that is passed into \code{glm()} having a form of \code{response ~ terms} where \code{response} is binary response vector and \code{terms} is a collection of terms connected by \code{'+'}. The first term of predictors will be used as a predictor of interest to calculate relative risks with respect to response variable.
#' @param basecov a baseline value of exposure variable. Defaults to the first level.
#' @param comparecov a value of exposure variable for comparison. Defaults to the first level.
#' @param fixcov a data frame of fixed value for each of adjusted confounders. If there is no confounder other than an exposure variable of interest, \code{fixcov} = \code{NULL}; if \code{fixcov} is missing for covariates, they are all set to \code{0} (for numerical covariates) or first levels (for factor covariates).
#' @param data a data frame containing response variable and all the terms used in \code{formula}.
#'
#' @return
#' \item{\code{fit}}{an object of class \code{glm}.}
#' \item{\code{RR}}{(adjusted) relative risk in response under exposure at baseline (\code{basecov}) and \code{comparecov}.}
#' \item{\code{delta.var}}{estimated variance of relative risk (\code{RR}) using Delta method.}
#' \item{\code{fix.cov}}{a data frame of fixed value for each of adjsuted confounders.}
#'
#' @importFrom stats binomial coefficients glm predict
#' @export
#'
#' @author Youjin Lee
#'
#' @examples
#' n <- 500
#' set.seed(1234)
#' W <- rbinom(n, 1, 0.3); W[sample(1:n, n/3)] = 2
#' dat <- as.data.frame(W)
#' dat$X <- sample( c("low", "medium", "high"), size = n, replace = TRUE)
#' dat$Y <- ifelse(dat$X == "low", rbinom(n, 1, plogis(W + 0.5)),
#'                ifelse(dat$X == "medium", rbinom(n, 1, plogis(W + 0.2)),
#'                rbinom(n, 1, plogis(W - 0.4)) ))
#' dat$X <- as.factor(dat$X)
#' result <- printnRR(Y ~ X + W, basecov = "high", comparecov = "low", data = dat)
#'
#'
printnRR <- function(formula, basecov, comparecov, fixcov = NULL, data){
  fit <- glm(formula, family = binomial(), data = data)
  tmp <- strsplit(as.character(formula)[3], "[+]")
  varnames <- gsub(" ","", tmp[[1]])
  #varnames <- ifelse(is.na(str_extract(varnames, '(?<=\\()[:alpha:]+(?=\\))')), varnames, str_extract(varnames, '(?<=\\()[:alpha:]+(?=\\))'))

  ## levels of factor
  #if (class(data[ ,names(data) == varnames[1]]) != "factor") return("Please use logisticRR")
  q <- length(levels(as.factor(data[ ,names(data) == varnames[1]])))

  ## set the initial level as a default
  baseind = compareind <- 1
  if (is.null(basecov)) basecov <- levels(data[ ,names(data) == varnames[1]])[baseind]
  if (is.null(comparecov)) comparecov <- levels(data[ ,names(data) == varnames[1]])[compareind]
  if (class(data[ ,names(data) == varnames[1]]) == "factor"){
    if (!as.character(basecov) %in% as.character(levels(as.factor(data[ ,names(data) == varnames[1]])))) return("Invalid baseline exposure.")
    if (!as.character(comparecov) %in% as.character(levels(as.factor(data[ ,names(data) == varnames[1]])))) return("Invalid exposure variable.")
  }
  ## check the order of basecov and comparecov
  for (j in 1:(q-1)){
    if (as.character(gsub(varnames[1], "",names(coefficients(fit))[j+1])) == as.character(basecov)){
      baseind <- j+1 # j-th level
    }
    if (as.character(gsub(varnames[1], "",names(coefficients(fit))[j+1])) == as.character(comparecov)){
      compareind <- j+1 # j-th level
    }
  }

  p <- length(varnames)-1 # the number of variables to be fixed
  if (p == 0) {
    newfixcov = NULL
  } else if (p > 0) {
    ## if values of other confounders are not specified, set them all zeros.
    newfixcov <- t(as.matrix(rep(0, p)))
    subdat = as.data.frame( data[,which(names(data) %in% varnames[-1])])
    tmp <-  which(apply(subdat, 2, class)!="numeric")
    for (q in 1:p) {
      if(class(subdat[,q]) == "factor"){
        newfixcov[q] <- levels(as.factor(subdat[,q]))[1]
      }else{
        newfixcov[q] <- min(subdat[,q])
      }
    }
    newfixcov <- as.data.frame(newfixcov)
    names(newfixcov) = names(data)[which(names(data) %in% varnames[-1])]
  }

  if( sum(names(fixcov) %in% names(newfixcov)) > 0 ) {
    tmpind <- which(names(newfixcov) %in% names(fixcov))
    for(j in 1:length(tmpind)){
      newfixcov[tmpind[j]] = eval(parse(text=paste0("fixcov$", names(newfixcov[tmpind])[j])))
    }
  }

  fixcov = newfixcov

  # if exposure variable is factor
  basecov <- levels(as.factor(data[ ,names(data) == varnames[1]]))[baseind]
  comparecov <- levels(as.factor(data[ ,names(data) == varnames[1]]))[compareind]

  expose.cov <- data.frame(as.factor(comparecov)); names(expose.cov) <- varnames[1]
  unexpose.cov <- data.frame(as.factor(basecov)); names(unexpose.cov) <- varnames[1]


  if (length(fixcov) > 0 & length(names(fixcov)) > 0 & length(fixcov) == length(varnames)-1) {
    expose.cov <- cbind(expose.cov, fixcov)
    unexpose.cov <- cbind(unexpose.cov, fixcov)
  } else if (length(names(fixcov)) == 0 & length(fixcov) > 0) {
    # if the name is missing, put varnames in the order of formula
    expose.cov <- cbind(expose.cov, fixcov); names(expose.cov)[2:length(expose.cov)] = varnames[2:length(varnames)]
    unexpose.cov <- cbind(unexpose.cov, fixcov); names(unexpose.cov)[2:length(unexpose.cov)] = varnames[2:length(varnames)]
  } else if (p > 0){
    return("Invalid data frame for confounders")
  }


  for (i in 1:ncol(expose.cov)) {
    if (class(data[ , names(data) == names(expose.cov)[i]]) != "factor") {
      expose.cov[,i] <- as.numeric(expose.cov[,i]);  unexpose.cov[,i] <- as.numeric(unexpose.cov[,i])
    }
  }

  betas <- coefficients(fit)
  exposed <- exp(-predict(fit, expose.cov, type = "link"))
  unexposed <- exp(-predict(fit, unexpose.cov, type = "link"))

  RR <- (1 + unexposed) / (1 + exposed)


  n.par <- length(betas)
  B.vec <- rep(0, n.par) # intercept + main variable + variables to e fixed
  B.vec[1] <- (-exposed + unexposed) / (1 + exposed )^2

  ## define new B.vec for main exposure variable
  for(j in 2:q){
    B.vec[j] <- -(compareind == j)*exposed*(1 + unexposed) + (baseind == j)*unexposed*(1 + exposed)
    B.vec[j] <- B.vec[j] / (1 + exposed)^2
  }
  if(n.par > q){
    for (j in (q+1):n.par) {
      if (names(coefficients(fit))[j] %in% names(fixcov)) {
        tmp <- which(names(fixcov) %in% names(coefficients(fit))[j])
        B.vec[j] <- as.numeric(as.character(fixcov[tmp]))*(-exposed + unexposed) / (1 + exposed)^2
      } else if (sum(startsWith(names(coefficients(fit))[j], names(fixcov))) > 0) {
        ## factor
        tmp <- which(startsWith(names(coefficients(fit))[j], names(fixcov)))
        # if fixcov[tmp] = 0; reference.
        if (gsub(names(fixcov)[tmp], "",names(coefficients(fit))[j]) == as.character(fixcov[,tmp])) {
          B.vec[j] <- 1*(-exposed + unexposed) / (1 + exposed)^2
        } else {
          B.vec[j] <- 0*(-exposed + unexposed) / (1 + exposed)^2
        }
      }
    }
  }

  cov.mat <- summary(fit)$cov.unscaled
  deltavar <- 0
  for (i in 1:n.par) {
    for (j in 1:n.par) {
      deltavar <- deltavar + cov.mat[i,j]*B.vec[i]*B.vec[j]
    }
  }
  return (list(fit = fit, RR = RR, delta.var = deltavar, fix.cov = fixcov))
}


#' Calculate adjusted relative risks under nominal exposure variable
#'
#' When response variable is binary and exposure variable is categorical
#' this function derives adjusted relative risks conditional on fixed other confounders' value
#' from logistic regression.
#'
#' @param formula a formula term that is passed into \code{glm()} having a form of \code{response ~ terms} where \code{response} is binary response vector and \code{terms} is a collection of terms connected by \code{'+'}. The first term of predictors will be used as a predictor of interest to calculate relative risks with respect to response variable.
#' @param basecov a baseline value of exposure variable. Defaults to the first level.
#' @param comparecov a value of exposure variable for comparison. Defaults to the first level.
#' @param fixcov a data frame of fixed value for each of adjusted confounders. If there is no confounder other than an exposure variable of interest, \code{fixcov} = \code{NULL}; if \code{fixcov} is missing for covariates, they are all set to \code{0} (for numerical covariates) or first levels (for factor covariates).
#' @param data a data frame containing response variable and all the terms used in \code{formula}.
#' @param boot a logical value whether bootstrap samples are generated or not. Defaults to \code{FALSE}.
#' @param n.boot if \code{boot =  TRUE}, the number of bootstrap samples. Defaults to \code{100}.
#'
#' @return
#' \item{\code{fit}}{an object of class \code{glm}.}
#' \item{\code{RR}}{(conditional) relative risk in response under exposure at baseline (\code{basecov}) and \code{comparecov}.}
#' \item{\code{delta.var}}{estimated variance of relative risk (\code{RR}) using Delta method.}
#' \item{\code{boot.rr}}{if \code{boot = TRUE}, a vector of \code{RR}'s using bootstrap samples.}
#' \item{\code{boot.var}}{estimated sampled variance using bootstraps if \code{boot = TRUE}.}
#' \item{\code{fix.cov}}{a data frame of fixed value for each of adjsuted confounders.}
#'
#' @importFrom stats binomial coefficients glm predict
#' @export
#'
#' @author Youjin Lee
#'
#' @examples
#' n <- 500
#' set.seed(1234)
#' W <- rbinom(n, 1, 0.3); W[sample(1:n, n/3)] = 2
#' dat <- as.data.frame(W)
#' dat$X <- sample( c("low", "medium", "high"), size = n, replace = TRUE)
#' dat$Y <- ifelse(dat$X == "low", rbinom(n, 1, plogis(W + 0.5)),
#'                ifelse(dat$X == "medium", rbinom(n, 1, plogis(W + 0.2)),
#'                rbinom(n, 1, plogis(W - 0.4)) ))
#' dat$X <- as.factor(dat$X)
#' result <- nominalRR(Y ~ X + W, basecov = "low", comparecov = "high", data = dat,
#' boot = TRUE, n.boot = 200)
#'
#'
nominalRR = function(formula, basecov = NULL, comparecov = NULL, fixcov = NULL, data, boot = FALSE,
                     n.boot = 100){

  results <- printnRR(formula = formula, basecov = basecov, comparecov = comparecov, fixcov = fixcov, data = data)

  if (class(results) == "character") return(results)

  if (boot == FALSE) return(results)

  boot.rr = boot.var <- c()
  for (r in 1:n.boot) {
    newdat <- data[sample(1:nrow(data), replace = TRUE),]
    boot.results <- printnRR(formula = formula, basecov = basecov, comparecov = comparecov, fixcov = fixcov, data = newdat)
    boot.rr[r] <- boot.results$RR
    boot.var[r] <- boot.results$delta.var
  }

  return(list(fit = results$fit, RR = results$RR, delta.var = results$delta.var,
              boot.rr = boot.rr, boot.var = boot.var, fix.cov = results$fix.cov))
}



