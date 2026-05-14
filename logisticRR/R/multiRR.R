#' Print adjusted relative risk using multinomial logistic regression under binary or ordinal exposure variable.
#'
#' @param formula a formula term that is passed into \code{multinom()} where response should be a factor having \code{K} different levels. Every term appearing in the formula should be well-defined as a column of \code{data}. Reference response should be specified as the first level.
#' @param basecov a baseline value of exposure variable. Defaults to \code{0}.
#' @param fixcov a data frame of fixed value for each of adjusted confounders. If there is no confounder other than the exposure variable of interest, \code{fixcov} = \code{NULL}; if \code{fixcov} is missing for existing covariates, they are all set to \code{0} (for numerical covariates) or to the first level (for factor covariates).
#' @param data a data frame containing response variable and all the terms used in \code{formula}.
#'
#' @return
#' \item{\code{fit}}{an object of class \code{multinom}.}
#' \item{\code{RRR}}{(adjusted) relative risk ratio of \code{K} different responses compared to reference response under exposure at baseline (\code{basecov}) and \code{basecov + 1}.}
#' \item{\code{RR}}{(adjusted) relative risk of \code{K} different responses under exposure at baseline (\code{basecov}) and \code{basecov + 1}.}
#' \item{\code{delta.var}}{estimated variance of relative risk (\code{RR}) using Delta method.}
#' \item{\code{fix.cov}}{a data frame of fixed value for each of adjsuted confounders.}
#'
#' @export
#'
#' @importFrom stats binomial coefficients glm predict
#' @importFrom nnet multinom
#'
#' @examples
#' n <- 500
#' set.seed(1234)
#' X <- rbinom(n, 1, 0.3)
#' W <- rbinom(n, 1, 0.3)
#' W[sample(1:n, n/3)] = 2
#' Y <- rbinom(n, 1, plogis(X - W))
#' dat <- as.data.frame(cbind(Y, X, W))
#' result <- printmRR(W ~ X + Y, basecov = 0, data = dat)
#'
#'
#' @author Youjin Lee
#'
printmRR <- function(formula, basecov = 0, fixcov = NULL, data){

  fit <- multinom(formula, data = data, trace = FALSE, Hess = TRUE)
  tmp <- strsplit(as.character(formula)[3], "[+]")
  varnames <- gsub(" ","", tmp[[1]])

  if ( class(data[ ,names(data) == varnames[1]]) == "factor" ) return("Please use nominalRR")

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


  #else if (!is.null(fixcov) & length(fixcov) != p){
  #  return("The length of fixed confounders is incorrect")
  #}

  expose.cov <- data.frame(basecov + 1); names(expose.cov) <- varnames[1]
  unexpose.cov <- data.frame(basecov); names(unexpose.cov) <- varnames[1]
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
    #if(class(expose.cov[,i])== "numeric"){
    #  expose.cov[,i] = as.factor(expose.cov[,i]);  unexpose.cov[,i] = as.factor(unexpose.cov[,i])
    #}
    if (class(data[ , names(data) == names(expose.cov)[i]]) != "factor") {
      expose.cov[,i] <- as.numeric(expose.cov[,i]);  unexpose.cov[,i] <- as.numeric(unexpose.cov[,i])
    }
  }

  betas <- coefficients(fit) ## matrix
  exposed <- predict(fit, expose.cov, type = "probs") ## vector of length K
  unexposed <- predict(fit, unexpose.cov, type = "probs") ## vector of length K

  expose.set <- exposed / exposed[1]; expose.set <- expose.set[-1]
  unexpose.set <- unexposed / unexposed[1]; unexpose.set <- unexpose.set[-1]

  expose.sum <- sum(expose.set)
  unexpose.sum <- sum(unexpose.set)

  RR <- exposed / unexposed ## vector of length K
  RRR <- RR / RR[1] ## vector of length K

  n.par <- length(betas)
  K <- nrow(betas)
  q <- ncol(betas)

  B.vec <- matrix(0, K+1, n.par) # intercept + main variable + variables to e fixed
  # intercept
  B.vec[1, 1:K] = ((1+expose.sum)^(-2))*(expose.set*(1+unexpose.sum) - unexpose.set*(1+expose.sum))
  B.vec[1, (K+1):(2*K)] = ((1+expose.sum)^(-2))*( (basecov+1)*expose.set*(1+unexpose.sum) - basecov*unexpose.set*(1+expose.sum))
  if(q > 2){
    # for confounding factors
    for(j in 3:q){

      if (colnames(coefficients(fit))[j] %in% names(fixcov)) {
        tmp <- which(names(fixcov) %in% colnames(coefficients(fit))[j])
        B.vec[1, ((j-1)*K + 1):(j*K)] <- (1+expose.sum)^(-2)*(as.numeric(fixcov[tmp])*expose.set*(1+unexpose.sum) -  as.numeric(fixcov[tmp])*unexpose.set*(1+expose.sum) )
      } else if (sum(startsWith(colnames(coefficients(fit))[j], names(fixcov))) > 0) {
        ## factor
        tmp <- which(startsWith(colnames(coefficients(fit))[j], names(fixcov)))
        # if fixcov[tmp] = 0; reference.
        if (gsub(names(fixcov)[tmp], "",colnames(coefficients(fit))[j]) == as.character(fixcov[,tmp]) ) {
          B.vec[1, ((j-1)*K + 1):(j*K)] <- (1+expose.sum)^(-2)*(1*expose.set*(1+unexpose.sum) - 1*unexpose.set*(1+expose.sum) )
        } else {
          B.vec[1, ((j-1)*K + 1):(j*K)] <- (1+unexpose.sum)^(-2)*(0*expose.set*(1+unexpose.sum) -  0*unexpose.set*(1+expose.sum) )
        }
      }
    }
  }

  for(k in 2:(K+1)){
    coefpart = exp(betas[(k-1),2])
    B.vec[k, 1:K] = ((1+expose.sum)^(-2))*coefpart*(expose.set*(1+unexpose.sum) - unexpose.set*(1+expose.sum))
    B.vec[k, (K+1):(2*K)] = ((1+expose.sum)^(-2))*( (basecov+1)*expose.set*coefpart*(1+unexpose.sum) - basecov*unexpose.set*coefpart*(1+expose.sum))
    ##
    doublepart = coefpart*(1+unexpose.sum) + coefpart*basecov*unexpose.set[k-1]
    B.vec[k, (K+k-1)] = ((1+expose.sum)^(-2))*( (basecov+1)*expose.set[k-1]*coefpart*(1+unexpose.sum) - doublepart*(1+expose.sum))
    ##
    if(q > 2){
      # for confounding factors
      for(j in 3:q){
        if (colnames(coefficients(fit))[j] %in% names(fixcov)) {
          tmp <- which(names(fixcov) %in% colnames(coefficients(fit))[j])
          B.vec[k, ((j-1)*K + 1):(j*K)] <- (1+expose.sum)^(-2)*(as.numeric(as.character(fixcov[tmp]))*coefpart*expose.set*(1+unexpose.sum) -  as.numeric(fixcov[tmp])*coefpart*unexpose.set*(1+expose.sum) )
        } else if (sum(startsWith(colnames(coefficients(fit))[j], names(fixcov))) > 0) {
          ## factor
          tmp <- which(startsWith(colnames(coefficients(fit))[j], names(fixcov)))
          # if fixcov[tmp] = 0; reference.
          if (gsub(names(fixcov)[tmp], "",colnames(coefficients(fit))[j]) == as.character(fixcov[,tmp]) ) {
            B.vec[k, ((j-1)*K + 1):(j*K)] <- (1+expose.sum)^(-2)*(1*coefpart*expose.set*(1+unexpose.sum) - 1*coefpart*unexpose.set*(1+expose.sum) )
          } else {
            B.vec[k, ((j-1)*K + 1):(j*K)] <- (1+expose.sum)^(-2)*(0*coefpart*expose.set*(1+unexpose.sum) -  0*coefpart*unexpose.set*(1+expose.sum) )
          }
        }
      }
    }

  }

  cov.mat <- solve(fit$Hessian)
  ## re-arrange cov.mat
  orders <- c()
  for(j in 1:q){
    orders <- c(orders, ((1:K)-1)*q + j)
  }
  cov.mat <- cov.mat[orders, orders]

  deltavar <- rep(0, K+1)
  for(k in 1:(K+1)){
    for (i in 1:n.par) {
      for (j in 1:n.par) {
        deltavar[k] <- deltavar[k] + cov.mat[i,j]*B.vec[k,i]*B.vec[k,j]
      }
    }
  }
  return(list(fit = fit, RRR = RRR, RR = RR, delta.var = deltavar, fix.cov = fixcov))
}




#' Inference on relative risk under multinomial logistic regression
#'
#' @param formula a formula term that is passed into \code{multinom()} where response should be a factor having \code{K} different levels. Every term appearing in the formula should be well-defined as a column of \code{data}. Reference response should be specified as the first level.
#' @param basecov a baseline value of exposure variable. Defaults to \code{0}.
#' @param fixcov a data frame of fixed value for each of adjusted confounders. If there is no confounder other than the exposure variable of interest, \code{fixcov} = \code{NULL}; if \code{fixcov} is missing for existing covariates, they are all set to \code{0} (for numerical covariates) or to the first level (for factor covariates).
#' @param data a data frame containing response variable and all the terms used in \code{formula}.
#' @param boot a logical value whether bootstrap samples are generated or not. Defaults to \code{FALSE}.
#' @param n.boot if \code{boot =  TRUE}, the number of bootstrap samples. Defaults to \code{100}.
#'
#' @return
#' \item{\code{fit}}{an object of class \code{multinom}.}
#' \item{\code{RRR}}{(adjusted) relative risk ratio of \code{K} different responses compared to reference response under exposure at baseline (\code{basecov}) and \code{basecov + 1}.}
#' \item{\code{RR}}{(adjusted) relative risk of \code{K} different responses under exposure at baseline (\code{basecov}) and \code{basecov + 1}.}
#' \item{\code{delta.var}}{estimated variance of relative risk (\code{RR}) using Delta method.}
#' \item{\code{boot.rr}}{if \code{boot = TRUE}, a vector of \code{RR}'s using bootstrap samples.}
#' \item{\code{boot.rrr}}{if \code{boot = TRUE}, a vector of relative risk ratio (\code{RRR})'s using bootstrap samples.}
#' \item{\code{boot.var}}{estimated sampled variance using bootstraps if \code{boot = TRUE}.}
#' \item{\code{fix.cov}}{a data frame of fixed value for each of adjsuted confounders.}
#'
#' @export
#'
#' @importFrom stats binomial coefficients glm predict
#' @importFrom nnet multinom
#'
#' @examples
#' n <- 500
#' set.seed(1234)
#' X <- rbinom(n, 1, 0.3)
#' W <- rbinom(n, 1, 0.3)
#' W[sample(1:n, n/3)] = 2
#' Y <- rbinom(n, 1, plogis(X - W))
#' dat <- as.data.frame(cbind(Y, X, W))
#' result <- multiRR(W ~ X + Y, basecov = 0, data = dat, boot = TRUE, n.boot = 100)
#' print(apply(result$boot.rr, 2, sd)) # estimated standard errors using Delta method
#' print(sqrt(result$delta.var)) # estimated standard errors using bootstrap
#'
#' @author Youjin Lee
#'
multiRR = function(formula, basecov = 0, fixcov = NULL, data, boot = FALSE,
                      n.boot = 100){

  results <- printmRR(formula = formula, basecov = basecov, fixcov = fixcov,
                     data = data)

  if (class(results) == "character") return(results)

  if (boot == FALSE) return(results)

  boot.rr = boot.rrr = boot.var <- matrix(0, n.boot, length(results$RR))
  for (r in 1:n.boot) {
    newdat <- data[sample(1:nrow(data), replace = TRUE),]
    boot.results <- printmRR(formula = formula, basecov = basecov, fixcov = fixcov,
                            data = newdat)
    boot.rr[r,] <- boot.results$RR
    boot.rrr[r,] <- boot.results$RRR
    boot.var[r,] <- boot.results$delta.var
  }

  return(list(fit = results$fit, RRR = results$RRR,
              RR = results$RR, delta.var = results$delta.var,
              boot.rr = boot.rr, boot.rrr = boot.rrr,
              boot.var = boot.var, fix.cov = results$fix.cov))
}

