#' Estimate confidence intervals and standard errors for the mode regression fit
#' 
#' Performs bootstrap on the modreg object.
#' 
#' @importFrom stats qnorm sd
#' 
#' @export
#' 
#' @param reg an object of class modreg (output of the modreg function)
#' @param nboot number of bootstrap replications
#' @param level confidence level
#' @param newdata Should be a data frame containing all the variables needed for predictions. If supplied, confidence intervals are calculated for the corresponding predictions.
#' @param bw Either "\code{variable}" or "\code{fix}", determining if the bandwidth of the 
#' original fit should be used for the bootstrap fits (\code{fix}) or if the bandwith 
#' should be recalculated (\code{variable}).
#' @param quiet if TRUE, printing of the status is suppressed
#' @param terms character scalar. If supplied, uses this term for confidence intervals of the prediction
#' @param seed the seed to use
#'
#' @returns a list with the following elements
#' \item{confpredict}{data frame, the confidence intervals for the predictions.}
#' \item{confparams}{data frame, the confidence intervals and standard errors for the parametric regression coefficients.}
#' \item{level}{confidence level}
#' \item{na}{scalar, stating the number of NA bootstrap repetitions.}
#' \item{seed}{scalar, the used seed.} 
#' 
#' @details 
#' A nonparametric residual bootstrap is performed to calculate standard errors of parameters and confidence intervals. More details can be found in Seipp et al. (2022).
#' \code{newdata} can be supplied to get confidence intervals for specific predictions. \code{terms} can be specified to calculate confidence interval for the contribution of one covariate (useful for P-splines).
#' \code{variable} bandwidth is the default, which has higher coverage than \code{fix}, but is computationally much more demanding. A \code{seed} can be supplied to guarantee a reproducible result.
#' 
#' @references 
#' Seipp, A., Uslar, V., Weyhe, D., Timmer, A., & Otto-Sobotka, F. (2022). Flexible Semiparametric Mode Regression for Time-to-Event Data. Manuscript submitted for publication.
#' 
#' @examples
#' 
#' \donttest{
#' data(colcancer)
#' colcancer80 <- colcancer[1:80, ]
#' 
#' # linear mode regression
#' regL <- modreg(Surv(logfollowup, death) ~ sex + age, data = colcancer80)
#' 
#' # bootstrap with a fixed bandwidth and 3 iterations, chosen to speed up the function. 
#' # Should in practice be much more than 3 iterations.
#' btL <- boot.modreg(regL, 3, bw = "fixed", level = 0.9, seed = 100)
#' 
#' # coefficients, SE and confidence intervals
#' cbind(coef(regL), btL$confparams)
#' 
#' 
#' ## confidence inverval for smooth effect / predictions
#' 
#' reg <- modreg(Surv(logfollowup, death) ~ sex + s(age, bs = "ps"), data = colcancer80, 
#'               control = modreg.control(tol_opt = 10^-2, tol_opt2 = 10^-2, tol = 10^-3))
#' ndat <- data.frame(sex = rep(colcancer80$sex[1], 200), age = seq(50, 90, length = 200))
#' 
#' # iterations should in practice be much more than 2!
#' bt <- boot.modreg(reg, 2, bw = "fixed", newdata = ndat, terms = "s(age)", seed = 100)
#' 
#' pr <- predict(reg, newdata = ndat, type = "terms", terms = "s(age)")[, 1]
#' 
#' plot(ndat$age, pr, ylim = c(-0.75, 1.5), type = "l", xlab = "age", ylab = "s(age)")
#' lines(ndat$age, bt$confpredict$lower, lty = 2)
#' lines(ndat$age, bt$confpredict$upper, lty = 2)
#' }
#'
#' 
#' 

boot.modreg <- function(reg, nboot, level = 0.95, newdata = NULL, bw = c("variable", "fixed"), quiet = FALSE, terms = NULL, seed = NULL){

  if(!is.null(seed)) set.seed(seed)
  
  bw <- match.arg(bw)
  if(!inherits(reg, "modreg"))stop('The reg argument needs to be of class modreg.')
  if(!length(reg$reg)>1 | any(is.na(reg$reg)))stop('Incomplete reg object')
  if(!is.numeric(nboot) | length(nboot)>1 || nboot%%1!=0)stop("The nboot argument must be a whole number")
  if(!all(is.numeric(level)) | any(level>1 | level<0) )stop("level(s) must be numeric and in between 0 and 1.")
  #check if the data in the newdata is consistent with the formula, necessary oder macht das die  predict Funktion?
  if(!is.null(newdata) & any(!all.vars(rhs(reg$reg$formula)) %in% names(newdata)))stop("All variables that appear in the formula of the modreg object must be present in the data of the newdata argument.")
  if(!is.null(terms)){
    if(!is.character(terms)) stop("terms has to be a character scalar") 
    if(length(terms) != 1) stop("currently only one term is allowed") 
  }
  
  response <- reg$reg$y
  pr <- predict(reg$reg)
  residuals <- response - pr
  n <- length(response)
  
  ##################geaendert
  #dat <- reg$reg$model
  dat <- reg$called$data
 ###################### 
 
  
  na <- 0
  
  
  # Beobachtungen erhalten neue IPC Gewichte basierend auf alten Daten
  delta <- rep(1, n)
  delta[reg$KMweights == 0] <- 0
  km <- survfit(Surv(response, 1 - delta) ~ 1,  type="kaplan-meier")
  kmsurv <- km$surv
  # damit wir nicht durch 0 teilen
  if(kmsurv[length(kmsurv)] == 0) kmsurv[length(kmsurv)] <- kmsurv[length(kmsurv) - 1]
  
  
  
  if(is.null(newdata)){
    newdata <- dat
  }
  
  formula <- reg$reg$formula
  rname <- as.character(formula.tools::lhs(formula))
  if(!(rname %in% names(dat))) stop("response name not found")
  
  param_boot <- predict_boot <- numeric()
  out <- list()
  
  
  # Takes all assigned variables and puts them in a list, which is passed on to the to_loop function
  #args = sapply(ls(), function(x)eval(parse(text = x)))
  
  
  bootdat <- dat
  for(i in 1:nboot){
    if(!quiet) message(paste("replication", i, "of", nboot))

    bootdat[, rname] <- pr + sample(residuals, n, replace = TRUE, prob = reg$KMweights)
    
    ind <- findInterval(bootdat[, 1], km$time)
    Wsurv <- ifelse(ind == 0, 1, kmsurv[ind])
    KMweights <- delta / Wsurv
    KMweights <- KMweights / sum(KMweights) * sum(reg$KMweights)
    
    
    argus <- reg$called
    argus$data <- bootdat
    argus$lambda <- reg$lambda
    if(bw == "fixed"){
      argus$bw <- reg$bw
    }
    argus$KMweights <- KMweights
    fit <- do.call(modreg, argus)

    if(length(fit$reg) == 1 && is.na(fit$reg)){
      param_boot <- rbind(param_boot, rep(NA, length(summary(reg$reg)$p.coeff)))
      na <- na + 1
      predict_boot <- cbind(predict_boot, rep(NA, n))
    } else{
      param_boot <- rbind(param_boot, summary(fit$reg)$p.coeff)
  
      if(!is.null(terms)){
        predict_boot <- cbind(predict_boot, predict(fit$reg, newdata = newdata, type = "terms", terms = terms))
      } else{
        predict_boot <- cbind(predict_boot, predict(fit$reg, newdata = newdata, type = "link"))
      }
    }
    
    
    
  }

  se <- apply(param_boot,2, sd, na.rm = TRUE)
  
  confparam <- rbind(summary(reg$reg)$p.coeff - se * qnorm((1 + level) / 2), summary(reg$reg)$p.coeff + se * qnorm((1 + level) / 2))
  
  se_confpr <- apply(predict_boot,1, sd, na.rm = TRUE)
  
  if(!is.null(terms)){
    pre <- predict(reg$reg, newdata = newdata, type = "terms", terms = terms)
  } else{
    pre <- predict(reg$reg, newdata = newdata, type = "link", terms = terms)
  }
  
  confpredict <- cbind(pre - se_confpr * qnorm((1 + level) / 2), pre + se_confpr * qnorm((1 + level) / 2))
  
  out$confparams <- data.frame(SE = se, lower = confparam[1, ], upper = confparam[2, ])
  
  out$confpredict <- data.frame(lower = confpredict[, 1], upper = confpredict[, 2]) 
 
  out$level <- level
 
  out$na <- na
  
  out$seed <- seed
  
  out
}

