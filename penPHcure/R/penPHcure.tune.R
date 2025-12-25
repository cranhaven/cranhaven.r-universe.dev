# ------------------------------------------------------------------------------
# Copyright (C) 2019 University of Liège
# <penPHcure is an R package for for estimation, variable selection and 
#  simulation of the semiparametric proportional-hazards (PH) cure model with 
#  time-varying covariates.>
# Authors: Alessandro Beretta & Cédric Heuchenne
# Contact: a.beretta@uliege.be
# 
# Licence as published by the Free Software Foundation, either version 3 of the 
# Licence, or any later version. This program is distributed in the hope that it 
# will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General 
# Public Licence for more details. You should have received a copy of the GNU 
# General Public Licence along with this program.
# If not, see <http://www.gnu.org/licenses/>.
# ------------------------------------------------------------------------------

penPHcure.tune <- function(init,pen.tuneGrid,pen.type,
                           pen.weights,pen.thres.zero,epsilon){

  pen.tune_params <- list()
  tuning <- list(AIC = list(crit = Inf),
                 BIC = list(crit = Inf),
                 pen.type = pen.type,
                 tuneGrid = NULL,
                 pen.weights = pen.weights,
                 N = init$N,
                 K = init$K,
                 isTies = init$isTies,
                 censoring = init$censoring)

  i <- 1
  
  if (init$print.details)
    penPHcure.tunePrint(pen.type)
  
  if (pen.type == "SCAD") {
    tuneGrid <- data.frame(aCURE=numeric(),aSURV=numeric(),
                           lambdaCURE=numeric(),lambdaSURV=numeric(),
                           AIC=numeric(),BIC=numeric())
    for (i1 in 1:length(pen.tuneGrid$CURE$a)){
      for (i2 in 1:length(pen.tuneGrid$SURV$a)){
        for (i3 in 1:length(pen.tuneGrid$CURE$lambda)){
          for (i4 in 1:length(pen.tuneGrid$SURV$lambda)){
            pen.tune_params$CURE[2] <- pen.tuneGrid$CURE$a[i1]
            pen.tune_params$SURV[2] <- pen.tuneGrid$SURV$a[i2]
            pen.tune_params$CURE[1] <- pen.tuneGrid$CURE$lambda[i3]
            pen.tune_params$SURV[1] <- pen.tuneGrid$SURV$lambda[i4]
            
            fit <- penPHcure.penfit(init,pen.type,pen.tune_params,
                                    pen.weights,epsilon,pen.thres.zero)

            if (init$print.details)
              penPHcure.tunePrint(pen.type,pen.tune_params,fit,i)
            
            if (fit$AIC < tuning$AIC$crit){
              tuning$AIC$crit <- fit$AIC
              tuning$AIC$b <- as.vector(fit$b)
              tuning$AIC$beta <- as.vector(fit$beta)
              tuning$AIC$cumhaz <- as.vector(fit$cumhaz)
              tuning$AIC$tune_params <- pen.tune_params
            }
            if (fit$BIC < tuning$BIC$crit){
              tuning$BIC$crit <- fit$BIC
              tuning$BIC$b <- as.vector(fit$b)
              tuning$BIC$beta <- as.vector(fit$beta)
              tuning$BIC$cumhaz <- as.vector(fit$cumhaz)
              tuning$BIC$tune_params <- pen.tune_params
            }
            tuneGrid[i,] <- c(pen.tune_params$CURE[2],pen.tune_params$SURV[2],
                              pen.tune_params$CURE[1],pen.tune_params$SURV[1],
                              fit$AIC,fit$BIC)
            i <- i + 1
          }
        }
      }
    }
    names(tuning$AIC$tune_params$CURE) <-
      names(tuning$AIC$tune_params$SURV) <- 
      names(tuning$BIC$tune_params$CURE) <- 
      names(tuning$BIC$tune_params$SURV) <- c("lambda","a")
  } else if (pen.type == "LASSO") {
    tuneGrid <- data.frame(lambdaCURE=numeric(),lambdaSURV=numeric(),
                           AIC=numeric(),BIC=numeric())
    for (i1 in 1:length(pen.tuneGrid$CURE$lambda)){
      for (i2 in 1:length(pen.tuneGrid$SURV$lambda)){
        pen.tune_params$CURE[1] <- pen.tuneGrid$CURE$lambda[i1]
        pen.tune_params$SURV[1] <- pen.tuneGrid$SURV$lambda[i2]

        fit <- penPHcure.penfit(init,pen.type,pen.tune_params,
                                pen.weights,epsilon,pen.thres.zero)
        
        if (init$print.details)
          penPHcure.tunePrint(pen.type,pen.tune_params,fit,i)

        if (fit$AIC < tuning$AIC$crit){
          tuning$AIC$crit <- fit$AIC
          tuning$AIC$b <- as.vector(fit$b)
          tuning$AIC$beta <- as.vector(fit$beta)
          tuning$AIC$cumhaz <- as.vector(fit$cumhaz)
          tuning$AIC$tune_params <- pen.tune_params
        }
        
        if (fit$BIC < tuning$BIC$crit){
          tuning$BIC$crit <- fit$BIC
          tuning$BIC$b <- as.vector(fit$b)
          tuning$BIC$beta <- as.vector(fit$beta)
          tuning$BIC$cumhaz <- as.vector(fit$cumhaz)
          tuning$BIC$tune_params <- pen.tune_params
        }
        tuneGrid[i,] <- c(pen.tune_params$CURE[1],pen.tune_params$SURV[1],
                          fit$AIC,fit$BIC)
        i <- i + 1
      }
    }
    names(tuning$AIC$tune_params$CURE) <-
      names(tuning$AIC$tune_params$SURV) <-
      names(tuning$BIC$tune_params$CURE) <-
      names(tuning$BIC$tune_params$SURV) <- c("lambda")
  }
  
  if (init$standardize){
    Z_mean <- attr(init$Z,"scaled:center")
    Z_sd <- attr(init$Z,"scaled:scale")
    X_mean <- attr(init$X_FIX,"scaled:center")
    X_sd <- attr(init$X_FIX,"scaled:scale")
    
    tuning$AIC$beta <- tuning$AIC$beta/Z_sd
    temp <- tuning$AIC$b[1] - sum(tuning$AIC$b*X_mean/X_sd)
    tuning$AIC$b <- tuning$AIC$b/X_sd
    tuning$AIC$b[1] <- temp
    tuning$AIC$cumhaz <- tuning$AIC$cumhaz/exp(sum(Z_mean*tuning$AIC$beta))
    
    tuning$BIC$beta <- tuning$BIC$beta/Z_sd
    temp <- tuning$BIC$b[1] - sum(tuning$BIC$b*X_mean/X_sd)
    tuning$BIC$b <- tuning$BIC$b/X_sd
    tuning$BIC$b[1] <- temp
    tuning$BIC$cumhaz <- tuning$BIC$cumhaz/exp(sum(Z_mean*tuning$BIC$beta))
  }
  
  names(tuning$AIC$b) <- colnames(init$X_FIX)
  names(tuning$BIC$b) <- colnames(init$X_FIX)
  names(tuning$AIC$beta) <- colnames(init$Z)
  names(tuning$BIC$beta) <- colnames(init$Z)
  attr(tuning$AIC$cumhaz,"time") <- init$fail_times
  attr(tuning$BIC$cumhaz,"time") <- init$fail_times
  tuning$tuneGrid <- tuneGrid
  tuning$which.X <- init$which.X
  tuning$survform <- init$survform
  tuning$cureform <- init$cureform

  class(tuning) <- 'penPHcure'
  
  return(tuning)
}





penPHcure.tunePrint <- function(pen.type,pen.tune_params=NULL,
                                fit=NULL,iter=NULL){
  if (is.null(pen.tune_params) && is.null(fit) && is.null(iter)){
    if (pen.type=="SCAD"){
      cat('\n')
      cat(c(format("iter",justify = "right",width = 6),
            format("aCURE",justify = "right",width = 8),
            format("aSURV",justify = "right",width = 8),
            format("lambdaCURE",justify = "right",width = 10),
            format("lambdaSURV",justify = "right",width = 10),
            format("AIC",justify = "right",width = 12),
            format("BIC",justify = "right",width = 12),
            format("df",justify = "right",width = 6)),fill=TRUE,sep="  ")
    } else if(pen.type=="LASSO") {
      cat('\n')
      cat(c(format("iter",justify = "right",width = 6),
            format("lambdaCURE",justify = "right",width = 10),
            format("lambdaSURV",justify = "right",width = 10),
            format("AIC",justify = "right",width = 12),
            format("BIC",justify = "right",width = 12),
            format("df",justify = "right",width = 6)),fill=TRUE,sep="  ")
    }
  } else {
    if (pen.type=="SCAD"){
      cat(c(format(iter,
                   justify = "right",width = 6),
            format(pen.tune_params$CURE[2],
                   justify = "right",width = 8,nsmall = 2,digits = 2),
            format(pen.tune_params$SURV[2],
                   justify = "right",width = 8,nsmall = 2,digits = 2),
            format(pen.tune_params$CURE[1],
                   justify = "right",width = 10,nsmall = 2,digits = 2),
            format(pen.tune_params$SURV[1],
                   justify = "right",width = 10,nsmall = 2,digits = 2),
            format(fit$AIC,
                   justify = "right",width = 12,nsmall = 4,digits = 4),
            format(fit$BIC,
                   justify = "right",width = 12,nsmall = 4,digits = 4),
            format(fit$df,
                   justify = "right",width = 6)),fill=TRUE,sep="  ")
    } else if(pen.type=="LASSO") {
      cat(c(format(iter,
                   justify = "right",width = 6),
            format(pen.tune_params$CURE[1],
                   justify = "right",width = 10,nsmall = 2,digits = 2),
            format(pen.tune_params$SURV[1],
                   justify = "right",width = 10,nsmall = 2,digits = 2),
            format(fit$AIC,
                   justify = "right",width = 12,nsmall = 4,digits = 4),
            format(fit$BIC,
                   justify = "right",width = 12,nsmall = 4,digits = 4),
            format(fit$df,
                   justify = "right",width = 6)),fill=TRUE,sep="  ")
    }
  }
}