#'Expectile regression for right censored event times using an auxiliary likelihood
#' 
#'Estimate a set of conditional expectiles or quantiles with semiparametric predictors
#'in accelerated failure time models.
#'For the estimation, the asymmetric loss functions are reformulated into auxiliary likelihoods.
#'
#' @aliases qureg.aft
#' @name expectreg.aft
#' @export
#' 
#' @usage expectreg.aft(
#'    formula,
#'    data = NULL,
#'    smooth = c("cvgrid", "aic", "bic", "lcurve", "fixed"), 
#'    lambda = 1, 
#'    expectiles = NA, ci = FALSE)
#'
#' qureg.aft(
#'    formula, 
#'    data = NULL, 
#'    smooth = c( "cvgrid", "aic", "bic", "lcurve", "fixed"), 
#'    lambda = 1, 
#'    quantiles = NA, 
#'    ci = FALSE)
#' 
#' @param formula An R formula object consisting of the response variable, '~' and the sum of all effects that should be taken into consideration. Each semiparametric effect has to be given through the function \code{\link{rb}}. The response needs to be a call of \code{\link[survival]{Surv}}.
#' @param data Optional data frame containing the variables used in the model, if the data is not explicitely given in the formula.  
#' @param smooth There are different smoothing algorithms that tune \code{lambda} to prevent overfitting. Caution, the currently implemented smoothing algorithms can take a long time. Cross validation is done with a grid search ('\code{cvgrid}'). The function can also use a supplied fixed penalty ('\code{fixed}'). The numerical minimisation is also possible with AIC or BIC as score ('\code{aic}', '\code{bic}'). The L-curve ('\code{lcurve}') is a new experimental grid search by Frasso and Eilers.
#' @param lambda The fixed penalty can be adjusted. Also serves as starting value for the smoothing algorithms.
#' @param expectiles In default setting, the expectiles (0.01,0.02,0.05,0.1,0.2,0.5,0.8,0.9,0.95,0.98,0.99) are calculated. You may specify your own set of expectiles in a vector. The option may be set to 'density' for the calculation of a dense set of expectiles that enhances the use of \code{\link{cdf.qp}} and \code{\link{cdf.bundle}} afterwards.
#' @param quantiles Quantiles for which the regression should be performed.
#' @param ci Whether a covariance matrix for confidence intervals and a \code{\link[=summary.expectreg]{summary}} is calculated.
#' 
#' @details For expectile regression, the LAWS loss function
#' 
#' \deqn{ S = \sum_{i=1}^{n}{ w_i(p)(y_i - \mu_i(p))^2} }
#' 
#' with
#' 
#' \eqn{ w_i(p) = p 1_{(y_i > \mu_i(p))} + (1-p) 1_{(y_i < \mu_i(p))} }
#' 
#' is repackaged into the asymmetric normal distribution.
#' Then, an accelerated failure time model is estimated.
#' This function is based on the 'expectreg' package and uses the same functionality
#' to include semiparametric predictors.
#' 
#' For quantile regression, the loss function is replaced with a likelihood from the asymmetric laplace distribution.
#' 
#' @returns An object of class 'expectreg', which is basically a list consisting of:
#' \item{lambda }{The final smoothing parameters for all expectiles and for all effects in a list.}
#' \item{intercepts }{The intercept for each expectile.}
#' \item{coefficients}{ A matrix of all the coefficients, for each base element
#'   a row and for each expectile a column. }
#' \item{values}{ The fitted values for each observation and all expectiles,
#'   separately in a list for each effect in the model,
#'   sorted in order of ascending covariate values. }
#' \item{response}{ Vector of the response variable. }
#' \item{covariates}{ List with the values of the covariates. }
#' \item{formula}{ The formula object that was given to the function. }
#' \item{asymmetries}{ Vector of fitted expectile asymmetries as given by argument \code{expectiles}. }
#' \item{effects}{ List of characters giving the types of covariates. }
#' \item{helper}{ List of additional parameters like neighbourhood structure for spatial effects or \eqn{\phi} for kriging. }
#' \item{design}{ Complete design matrix. }
#' \item{bases}{ Bases components of each covariate. }
#' \item{fitted}{ Fitted values \eqn{ \hat{y} }. }
#' \item{covmat}{ Covariance matrix, estimated when \code{ci = TRUE}. }
#' \item{diag.hatma}{ Diagonal of the hat matrix. Used for model selection criteria. }
#' \item{data}{ Original data }
#' \item{smooth_orig}{ Unchanged original type of smoothing. }
#' %\item{delta_garrote}{ Values of extra weights used for non-negative garrote }
#' \code{\link[=plot.expectreg]{plot}}, \code{\link[=predict.expectreg]{predict}}, \code{\link[=resid.expectreg]{resid}},
#' \code{\link[=fitted.expectreg]{fitted}}, \code{\link[=effects.expectreg]{effects}}
#' and further convenient methods are available for class 'expectreg'.
#' 
#' @examples 
#' 
#' data(colcancer)
#' ex <- c(0.05, 0.2, 0.5, 0.8, 0.95)
#' c100 <- colcancer[1:100,]
#' exfit <- expectreg.aft(Surv(logfollowup, death) ~ LNE, data = c100, expectiles = ex, smooth="f")
#' coef(exfit)
#' 
#' qu1 <- qureg.aft(Surv(logfollowup, death) ~ LNE + sex, data=c100, smooth="fixed")
#' coef(qu1)
#' 
#' \dontrun{
#' 
#' # takes some time
#' qu2 <- qureg.aft(Surv(logfollowup, death) ~ rb(LNE) + sex, data=colcancer[1:200,])
#' }
#' 
#' @author Fabian Otto-Sobotka \cr Carl von Ossietzky University Oldenburg \cr \url{https://uol.de} \cr
#' 
#' @seealso \code{\link{expectreg.ipc}}, \code{\link[expectreg]{expectreg.ls}}
#' 
#' @importFrom expectreg rb
#' @importFrom stats terms predict nlminb
#' @import parallel


expectreg.aft <-
function(formula,data=NULL, smooth=c("cvgrid","aic","bic","lcurve","fixed"),lambda=1,expectiles=NA,ci=FALSE)
{  
  smooth = match.arg(smooth)


  if(!is.na(charmatch(expectiles[1],"density")) && charmatch(expectiles[1],"density") > 0)
  {
    pp <- seq(0.01, 0.99, by=0.01)
  }
  else if(any(is.na(expectiles)) || !is.vector(expectiles) || any(expectiles > 1) || any(expectiles < 0))
  {
    pp <- c(0.01,0.02,0.05,0.1,0.2,0.5,0.8,0.9,0.95,0.98, 0.99)
  }
  else
  {
    pp <- expectiles
  }
  np <- length(pp)
  
  # yy = eval(as.expression(formula[[2]]),envir=data,enclos=environment(formula))
  # attr(yy,"name") = deparse(formula[[2]])

  dn    <- prepare_formula(formula,data, expect = TRUE)
  yy    <- dn[[1]]
  delta <- dn[[2]]
  data  <- dn[[3]]
   
  m = length(yy)

  design = list()
  x = list()
  types = list()
  bnd = list()
  Zspathelp = list()
  nb = vector()
  krig.phi = list()
  center = TRUE
  varying = list()
  Blist = list()
  Plist = list()
  
  if (formula[[3]] == "1")
  {
      design[[1]] = rb(matrix(1,nrow=m,ncol=1),"parametric",center=FALSE)
      smooth = "fixed"
      design[[1]]$xname = "intercept"
  }
  else if(formula[[3]] == ".")
  {
      design[[1]] = rb(data[,names(data) != all.vars(formula[[2]])],"parametric")
      smooth = "fixed"
  }
  else
    for(i in 1:length(labels(terms(formula))))
    {
      types[[i]] = strsplit(labels(terms(formula))[i],"(",fixed=TRUE)[[1]][1]
      
      if(types[[i]] == labels(terms(formula))[i])
      {
        design[[i]] = rb(eval(parse(text=labels(terms(formula))[i]),envir=data,enclos=environment(formula)),"parametric")
        #formula = eval(substitute(update(formula, . ~ variable2 + . - variable1),
        #               list(variable1 = as.name(types[[i]]),variable2 = as.name(paste("rb(",types[[i]],",'parametric')",sep="")))))
        types[[i]] = "parametric"
        design[[i]]$xname = labels(terms(formula))[i]
      }
      else  
        design[[i]] = eval(parse(text=labels(terms(formula))[i]),envir=data,enclos=environment(formula))
    }
  nterms = length(design)

  varying[[1]] = design[[1]][[9]]
  if(any(!is.na(varying[[1]])))
  {
    B = design[[1]][[1]] * varying[[1]]
    Blist[[1]] = design[[1]][[1]] * varying[[1]]
  }
  else
  {
    B = design[[1]][[1]]
    Blist[[1]] = design[[1]][[1]]
  }

  DD = as.matrix(design[[1]][[2]])
  Plist[[1]] = DD
  x[[1]] = design[[1]][[3]]
  names(x)[1] = design[[1]]$xname[1]
  types[[1]] = design[[1]][[4]]
  bnd[[1]] = design[[1]][[5]]
  Zspathelp[[1]] = design[[1]][[6]]
  nb[1] = ncol(design[[1]][[1]])
  krig.phi[[1]] = design[[1]][[7]]
  center = center && design[[1]][[8]]
  constmat = as.matrix(design[[1]]$constraint)

  if(length(design) > 1)
  for(i in 2:length(labels(terms(formula))))
  {
    varying[[i]] = design[[i]][[9]]
    if(any(!is.na(varying[[i]])))
    {
      B = cbind(B,design[[i]][[1]] * varying[[i]])
      Blist[[i]] = design[[i]][[1]] * varying[[i]]
    }
    else
    {
      B = cbind(B,design[[i]][[1]])
      Blist[[i]] = design[[i]][[1]]
    }
    
    design[[i]][[2]] = as.matrix(design[[i]][[2]])
    Plist[[i]] = design[[i]][[2]]
    DD = rbind(cbind(DD,matrix(0,nrow=nrow(DD),ncol=ncol(design[[i]][[2]]))),
               cbind(matrix(0,nrow=nrow(design[[i]][[2]]),ncol=ncol(DD)),design[[i]][[2]]))
    constmat = rbind(cbind(constmat,matrix(0,nrow=nrow(constmat),ncol=ncol(design[[i]]$constraint))),
               cbind(matrix(0,nrow=nrow(design[[i]]$constraint),ncol=ncol(constmat)),design[[i]]$constraint))
    x[[i]] = design[[i]][[3]]
    names(x)[i] = design[[i]]$xname[1]
    types[[i]] = design[[i]][[4]]
    bnd[[i]] = design[[i]][[5]]
    Zspathelp[[i]] = design[[i]][[6]]
    nb[i] = ncol(design[[i]][[1]])
    krig.phi[[i]] = design[[i]][[7]]
    center = center && design[[i]][[8]]
  }

  if(center)
  {
    B = cbind(1,B)
    DD = rbind(0,cbind(0,DD))
    constmat = rbind(0,cbind(0,constmat))
  }


    coef.vector = laws.aft(B,DD,yy,delta,pp,lambda,smooth,nb,center,constmat,types)


  vector.a.ma.schall = coef.vector[[1]]
  lala = coef.vector[[2]]
  diag.hat = coef.vector[[3]]
  sigma = coef.vector[[4]]


  covariance = NULL
##############################
if(ci)
{
  W = list()
  covariance = list()

  for(i in 1:np)
  {

    W = as.vector(ifelse(yy > B %*% vector.a.ma.schall[,i], pp[i], 1 - pp[i]))
    square.dev = (yy - B %*% vector.a.ma.schall[,i])^2
    correct = 1/(1-diag.hat[,i])
    
    if(any(is.na(W)))
    {
      correct[!is.na(W)] = correct[1:(length(correct)-length(which(is.na(W))))]
      correct[is.na(W)] = 1
      W[which(is.na(W))] = 1
      square.dev[which(is.na(square.dev))] = 0
    }

    lahmda = rep(lala[,i],times=nb)
    if(center)
      lahmda = c(0,lahmda)
    K = lahmda * t(DD) %*% DD
    helpmat = solve(t(W * B) %*% B + K)
    #quadmat = helpmat%*%t(B)%*%(W^2 * B)%*%helpmat
    
    #correct = 1#c()
    #for(k in 1:m)
    #{
    #  correct = c(correct,1/(1-2*t(diag(W)[k]*B[k,])%*%helpmat%*%B[k,] + t(B[k,])%*%quadmat%*%B[k,]))
    #}
    

      covariance[[i]] = helpmat %*% (t(B * (W^2 * (correct^2* square.dev)[,1])) %*% B) %*% helpmat

  }

}
##############################




  Z <- list()
  coefficients <- list()
  final.lambdas <- list()
  helper <- list()
  
  fitted = B %*% vector.a.ma.schall
  
  if(center)
  {
    intercept = vector.a.ma.schall[1,]
    B = B[,-1,drop=FALSE]
    vector.a.ma.schall = vector.a.ma.schall[-1,,drop=FALSE]
  }
  else
    intercept = rep(0,np)

  for(k in 1:length(design))
  {
    final.lambdas[[k]] = lala[k,]
    names(final.lambdas)[k] = design[[k]]$xname
  
    partbasis = (sum(nb[0:(k-1)])+1):(sum(nb[0:k]))
  
    if(types[[k]] == "pspline" || types[[k]] == "tp")
    {
      Z[[k]] <- matrix(NA, m, np)
      coefficients[[k]] = matrix(NA,nrow=nb[k],ncol=np)
      helper[[k]] = NA
      for(i in 1:np)
      {
        Z[[k]][,i] <- design[[k]][[1]] %*% vector.a.ma.schall[partbasis,i,drop=FALSE] + intercept[i]
        coefficients[[k]][,i] = vector.a.ma.schall[partbasis,i,drop=FALSE]
      }
    }
    else if(types[[k]] == "markov")
    {
      Z[[k]] <- matrix(NA, m, np)
      coefficients[[k]] = matrix(NA,nrow=nb[k],ncol=np)
      helper[[k]] = list(bnd[[k]],Zspathelp[[k]])
      for(i in 1:np)
      {
        Z[[k]][,i] = design[[k]][[1]] %*% vector.a.ma.schall[partbasis,i,drop=FALSE] + intercept[i]
        coefficients[[k]][,i] = vector.a.ma.schall[partbasis,i,drop=FALSE]
      }
    }
    else if(types[[k]] == "2dspline")
    {
      Z[[k]] <- matrix(NA, m, np)
      coefficients[[k]] = matrix(NA,nrow=nb[k],ncol=np)
      helper[[k]] = NA
      for(i in 1:np)
      {
        Z[[k]][,i] = design[[k]][[1]] %*% vector.a.ma.schall[partbasis,i,drop=FALSE] + intercept[i]
        coefficients[[k]][,i] = vector.a.ma.schall[partbasis,i,drop=FALSE]
      }
      
    }
    else if(types[[k]] == "radial")
    {
      Z[[k]] <- matrix(NA, m, np)
      coefficients[[k]] = matrix(NA,nrow=nb[k],ncol=np)
      helper[[k]] = Zspathelp[[k]]
      for(i in 1:np)
      {
        Z[[k]][,i] = design[[k]][[1]] %*% vector.a.ma.schall[partbasis,i,drop=FALSE] + intercept[i]
        coefficients[[k]][,i] = vector.a.ma.schall[partbasis,i,drop=FALSE]
      }
    }
    else if(types[[k]] == "krig")
    {
      Z[[k]] <- matrix(NA, m, np)
      coefficients[[k]] = matrix(NA,nrow=nb[k],ncol=np)
      helper[[k]] = list(krig.phi[[k]],Zspathelp[[k]])
      for(i in 1:np)
      {
        Z[[k]][,i] = design[[k]][[1]] %*% vector.a.ma.schall[partbasis,i,drop=FALSE] + intercept[i]
        coefficients[[k]][,i] = vector.a.ma.schall[partbasis,i,drop=FALSE]
      }
    }
    else if(types[[k]] == "random")
    {
      Z[[k]] <- matrix(NA, m, np)
      coefficients[[k]] = matrix(NA,nrow=nb[k],ncol=np)
      helper[[k]] = NA
      for(i in 1:np)
      {
        Z[[k]][,i] <- design[[k]][[1]] %*% vector.a.ma.schall[partbasis,i,drop=FALSE] + intercept[i]
        coefficients[[k]][,i] = vector.a.ma.schall[partbasis,i,drop=FALSE]
      }

    }
    else if(types[[k]] == "ridge")
    {
      Z[[k]] <- matrix(NA, m, np)
      coefficients[[k]] = matrix(NA,nrow=nb[k],ncol=np)
      helper[[k]] = NA
      for(i in 1:np)
      {
        Z[[k]][,i] <- design[[k]][[1]] %*% vector.a.ma.schall[partbasis,i,drop=FALSE] + intercept[i]
        coefficients[[k]][,i] = vector.a.ma.schall[partbasis,i,drop=FALSE]
      }
    }
    else if(types[[k]] == "parametric")
    {    
      Z[[k]] <- matrix(NA, m, np)
      coefficients[[k]] = matrix(NA,nrow=nb[k],ncol=np)
      helper[[k]] = NA
      for(i in 1:np)
      {
        Z[[k]][,i] <- design[[k]][[1]] %*% vector.a.ma.schall[partbasis,i,drop=FALSE] + intercept[i]
        coefficients[[k]][,i] = vector.a.ma.schall[partbasis,i,drop=FALSE]
      }
    }
    else if(types[[k]] == "special")
    {    
      Z[[k]] <- matrix(NA, m, np)
      coefficients[[k]] = matrix(NA,nrow=nb[k],ncol=np)
      helper[[k]] = NA
      for(i in 1:np)
      {
        Z[[k]][,i] <- design[[k]][[1]] %*% vector.a.ma.schall[partbasis,i,drop=FALSE] + intercept[i]
        coefficients[[k]][,i] = vector.a.ma.schall[partbasis,i,drop=FALSE]
      }
    }
    names(Z)[k] = design[[k]]$xname[1]
    names(coefficients)[k] = design[[k]]$xname[1]
  }

  desmat = B
  if(center)
    desmat = cbind(1,B)

  result = list("lambda"=final.lambdas,"intercepts"=intercept,"coefficients"=coefficients,"values"=Z,"response"=yy,"covariates"=x,
                "formula"=formula,"asymmetries"=pp,"effects"=types,"helper"=helper,"design"=desmat,"bases"=design,"fitted"=fitted,"covmat"=covariance,"diag.hatma" = diag.hat,"sigma" = sigma)
  
             
  result$predict <- function(newdata=NULL)
         {
           BB = list()
           values = list()
           bmat = NULL
           for(k in 1:length(coefficients))
           {
             BB[[k]] = predict(design[[k]],newdata)
             values[[k]]  <- BB[[k]] %*% coefficients[[k]]
             values[[k]] = t(apply(values[[k]],1,function(x) { x + intercept } ))
             bmat = cbind(bmat,BB[[k]])
           }
           if(center)
           {
             bmat = cbind(1,bmat)
             vector.a.ma.schall = rbind(intercept,vector.a.ma.schall)
           }
           fitted = bmat %*% vector.a.ma.schall
           names(values) = names(coefficients)
           
           list("fitted"=fitted,"values"=values)
         }
  
  class(result) = c("expectreg","aft")
  
  result
}
