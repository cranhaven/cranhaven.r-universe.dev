# This file is the codes of R for the maximum likelihood method for additive hazard

library(Rcpp)
library(Matrix)
library(survival)



#' @title ah
#' 
#' @description This function offers the methods to fit the additive hazards model, including Aalen's method and Maximum likelihood method. 
#'
#' @param formula A formula, the dependent variable must be of type Surv in the survival package..
#' @param data A data frame with the covariates mentioned in the formula stored.
#' @param matrix_domain A matrix describing the domain to find the maximum likelihood. The default constraint matrix guarantees the hazards to be positive for all possible covariates.
#' @param progbar A logical value, shows the progress bar if it is TRUE, hide the progress bar if FALSE, default value is FALSE.
#' @param method A string with values "aalen", "ml_opt", "ml_enum", "ml_asc" and "ml_desc". Default value is "ml_opt".  "aalen" represents the Aalen's method.  "ml_opt" is the default method with respect to the default constraint matrix. "ml_enum","ml_asc" and "ml_desc" represents to the naive method, ascending method and descending method. Please check the referee for details.
#' @param scale A logical value, scales the input data in the interval [0,1] if it is TRUE. Default value is TRUE.
#' @param startedge a vector which satisfies the domain condition. Only used for the ascending method. 
#' 
#' 
#' @return A data frame, containing the coefficients (beta) at each time point and the cumulative beta at each time point.
#' @examples
#' X1 = rnorm(100); X2 = rnorm(100)
#' Survival_Time = rep(0,100)
#' U = runif(100,min =0, max =1)
#' for (i in 1:100){Survival_Time[i] = sqrt((-2*log(U[i]))/(0.3*X1[i] + 0.7*X2[i]))}
#' tcens = runif(100, 2.5, 7.5)
#' time =  pmin(Survival_Time, tcens)
#' event = as.numeric(Survival_Time<=tcens)
#' Data = data.frame(time = time, X1 = X1, X2 = X2, event = event)
#' 
#' Result = ah(Surv(time = time, event = event)~ X1 + X2, Data)
#' 
#' @references
#' Chengyuan Lu, Jelle Goeman, Hein Putter 
#' Maximum likelihood estimation in the additive hazards model
#' arXiv:2004.06156 




ah <-function(formula = formula(data), data = sys.parent(),  matrix_domain = NULL, progbar = FALSE, method = "ml_opt", scale = TRUE, startedge= NULL){
  
  if(0 == attr(terms(formula),"intercept")){
    warning("The formula must include the intercept.")
    return(NULL)
  }
  
  
  # change the method string to numeric
  if ("ml_opt" == method){
    method_numeric <- 1
  }else if("ml_enum" == method){
    method_numeric <- 2
  }else if("ml_desc" == method){
    method_numeric <- 3
  }else if("ml_asc" == method){
    method_numeric <- 4    
  }else if("aalen" == method){
    method_numeric <- 0
  }else{
    warning("Could not specify the input method.")
    return(NULL)
  }
  
  # design matrix
  tempmodel = coxph(formula, data, x= TRUE)
  matrix_design = cbind("time" = as.numeric(tempmodel$y[,1]),
                        "intercept" = 1,
                        tempmodel$x,
                        "event" = as.numeric(tempmodel$y[,2]))
  #matrix_design = data.frame(matrix_design)
  matrix_design = matrix_design[order(matrix_design[,1],-matrix_design[,ncol(matrix_design)]),]
  
  p <- ncol(matrix_design)-2-1 
  
  
  
  if(0 == method_numeric){
    result <- aalen_base(matrix_design)
    names(result$beta) = c("time", "intercept" ,attr(terms(formula),"term.labels"))
    names(result$cumbeta) = c("time", "intercept" ,attr(terms(formula),"term.labels"))
    
  }else{
    

    
    #test
    #print(head(matrix_design))
    #print(p)
    #print(matrix_domain)
    
    if(TRUE == scale){
      matrix_design_R = Scale_data_pre(matrix_design)
    }else{
      matrix_design_R = matrix(matrix_design, nrow = nrow(matrix_design))
    }
    
    #print(matrix_design_R)
    
    # make the domain matrix
    if(1 != method_numeric){
      if(is.null(matrix_domain)){
        matrix_domain  = Calc_BinaryDomain(p)  
      }
    }

    
    
    result <- mle_addhazardbase(InputData = matrix_design_R, matrix_domain, progbar, method_numeric, startedge)
    names(result$beta) = c("time", "intercept" ,attr(terms(formula),"term.labels"))
    
    #print(beta)
    if(TRUE == scale){
      result$beta = Scale_data_after(result$beta, matrix_design)
      #print(beta)
    }
    
    result$cumbeta = Calc_Cbeta(result$beta)
    
  }
  
  return(result)
  
}











