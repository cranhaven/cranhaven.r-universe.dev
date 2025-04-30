#' @title Predict method for \pkg{bcgam} fits 
#'
#' @description Predicted values based on \pkg{bcgam} object.
#'
#' @param object Object of class inheriting from "bcgam".
#' @param newdata An optional data frame in which to look for variable with which to predict.
#' @param interval Type of interval calculation. It can be either \code{"credible"} or \code{"prediction"}.
#' If \code{interval=prediction}, then a summary of the predictive posterior distribution is shown. 
#' @param level Tolerance/credible level. The default is \code{0.95}.
#' @param parameter The type of parameter to predict. If parameter=\code{"eta"}, then the systematic component
#' \eqn{\eta} is predicted. If parameter=\code{"mu"}, then the mean value \eqn{\mu} obtained by transforming
#' \eqn{\eta} using the inverse of the link function is predicted. The default is \eqn{\mu}. If 
#' \code{interval="prediction"}, then this variable is ignored.
#' @param ... further arguments passed to or from other methods.
#'
#' @export
#'
#' @details \code{predict.bcgam} produces estimated values, standard errors,
#' and interval estimates; obtained by the
#' fitted model in the frame \code{newdata}. Setting \code{intervals} 
#' specifies computation of credible or prediction (tolerance) intervals at the
#' specified \code{level}. 
#'
#' All predictions are based on the posterior distribution in the \code{bcgam} object.
#'
#'
#' @return \code{predict.bcgam} produces a list of predictions, standard errors, and bounds based
#' on the posterior disitribution in the \code{bcgam} object.
#'
#' If \code{interval="credible"}, a list with the following components is returned:
#' \item{cred.mean}{the mean values.}
#' \item{cred.sd}{standard error of mean values.}
#' \item{cred.lower}{lower bound of mean values.}
#' \item{cred.upper}{upper bound of mean values.}
#' If \code{interval="prediction"}, a list with the following components is returned:
#' \item{pred.mean}{the predicted values.}
#' \item{pred.sd}{standard error of predicted values.}
#' \item{pred.lower}{lower bound of predicted values.}
#' \item{pred.upper}{upper bound of predicted values.}
#'
#' @author Cristian Oliva-Aviles and Mary C. Meyer
#'
#' @keywords internal
#' 
#' @examples
#' \dontrun{
#' n<-50
#' x<-(1:n)^{1/3}
#' z<-as.factor(rbinom(n, 1, 0.6))
#' y<-x+7*as.numeric(z)+rnorm(n,sd=2) 
#' bcgam.fit <- bcgam(y~sm.incr(x)+z, nloop=100)
#' predict(bcgam.fit, newdata=data.frame(x=0.5, z="1"), parameter="mu", interval="prediction")
#' }
predict.bcgam <- function(object, newdata, interval=c("credible"), 
			level=0.95, parameter=c("mu"), ...)
{
	tt <- terms(object)
    if (!inherits(object, "bcgam")) 
        warning("calling predict.bcgam(<fake-bcgam-object>) ...")
    if (missing(newdata) || is.null(newdata)) {
        mm <- X <- model.matrix(object)
        mmDone <- TRUE
        offset <- object$offsetn
    }
    else {
        Terms <- delete.response(tt)

	  m <- model.frame(Terms, newdata, na.action = na.pass, 
            xlev = object$levels)
        if (!is.null(cl <- attr(Terms, "dataClasses"))) 
            .checkMFClasses(cl, m)
        X <- model.matrix(Terms, m, contrasts.arg = object$contrasts)
        offset <- rep(0, nrow(X))
        if (!is.null(off.num <- attr(tt, "offset"))) 
            for (i in off.num) offset <- offset + eval(attr(tt, 
                "variables")[[i + 1]], newdata)
        if (!is.null(object$call$offset)) 
            offset <- offset + eval(object$call$offset, newdata)
        mmDone <- FALSE
    }

	n=length(object$y)
	L=length(object$xmat)/n
	delta.newdata<-NULL
	incr<-NULL
	decr<-NULL
	zmatb.newdata<-NULL
	xmat.newdata<-cbind(X)

	no.rows=dim(newdata)[1]	
	v=1:no.rows*0+1
	
	
	for(i in 1:L){
	Xi=X[,object$ind_nonparam[i]]
	if(object$shapes[i]==1){
	delta.newdata=rbind( delta.newdata,monincr(Xi,object$knots[[i]])$sigma-object$center.delta[[i]] )} # delta.newdata contains basis splines in rows

	if(object$shapes[i]==2){
	delta.newdata=rbind( delta.newdata,mondecr(Xi,object$knots[[i]])$sigma-object$center.delta[[i]] )} # delta.newdata contains basis splines in rows

	if(object$shapes[i]==3){ delta.i=convex(Xi, object$knots[[i]], pred.new=TRUE)
	delta.newdata=rbind( delta.newdata,delta.i$sigma-t(delta.i$x.mat%*%object$center.delta[[i]]) )} # delta.newdata contains basis splines in rows

	if(object$shapes[i]==4){ delta.i=concave(Xi, object$knots[[i]], pred.new=TRUE)
	delta.newdata=rbind( delta.newdata,delta.i$sigma-t(delta.i$x.mat%*%object$center.delta[[i]]) )} # delta.newdata contains basis splines in rows

	if(object$shapes[i]==5){
	delta.newdata=rbind( delta.newdata,incconvex(Xi,object$knots[[i]])$sigma-object$center.delta[[i]] )} # delta.newdata contains basis splines in rows

	if(object$shapes[i]==7){
	delta.newdata=rbind( delta.newdata,incconcave(Xi,object$knots[[i]])$sigma-object$center.delta[[i]] )} # delta.newdata contains basis splines in rows

	if(object$shapes[i]==6){
	delta.newdata=rbind( delta.newdata,decconvex(Xi,object$knots[[i]])$sigma-object$center.delta[[i]] )} # delta.newdata contains basis splines in rows

	if(object$shapes[i]==8){
	delta.newdata=rbind( delta.newdata,decconcave(Xi,object$knots[[i]])$sigma-object$center.delta[[i]] )} # delta.newdata contains basis splines in rows

	

	if(object$shapes[i]==1|object$shapes[i]==5|object$shapes[i]==7){incr=c(incr,1)}else{incr=c(incr,0)}
	if(object$shapes[i]==2|object$shapes[i]==6|object$shapes[i]==8){decr=c(decr,1)}else{decr=c(decr,0)}

	if(incr[i]==0 & decr[i]==0){
		zmatb.newdata=cbind(zmatb.newdata,Xi)
	}

	}


	ind.nonparaplusinter=c(1, object$ind_nonparam)

	if(no.rows==1){
	if(object$ind_intercept==1){
		zmatb.newdata=c(v,xmat.newdata[,-(ind.nonparaplusinter)],zmatb.newdata) }
	else{zmatb.newdata=c(xmat.newdata[,-(ind.nonparaplusinter)],zmatb.newdata)}
	
	deltazmat=c(t(delta.newdata),zmatb.newdata)

	pred.sims=object$coefs.sims%*%deltazmat
	}

	else{
	if(object$ind_intercept==1){
		zmatb.newdata=cbind(v,xmat.newdata[,-(ind.nonparaplusinter)],zmatb.newdata) }
	else{zmatb.newdata=cbind(xmat.newdata[,-(ind.nonparaplusinter)],zmatb.newdata)}

	deltazmat=cbind(t(delta.newdata),zmatb.newdata)

	pred.sims=object$coefs.sims%*%t(deltazmat)
	}

alpha.upper=1-(1-level)/2
alpha.lower=(1-level)/2
q.upper=function(x){quantile(x,alpha.upper)}
q.lower=function(x){quantile(x,alpha.lower)}

get.predgaussian=function(x){rnorm( rep(1,length(x)) ,x , object$sigma.sims)}
get.predbinomial=function(x){rbinom( rep(1,length(x)), rep(1,length(x)), x) }
get.predpoisson=function(x){rpois( rep(1,length(x)), x) }
#freqtoprop=function(x){prop.table(table(x))}

if(object$family=="gaussian"){ 

	if(interval=="credible"|interval=="prediction"){
		cred.mean=apply(pred.sims,2,mean)
	
		if(interval=="credible"){
			if(parameter=="eta"){
			cred.sd=apply(pred.sims,2,sd)
			cred.upper=apply(pred.sims,2,q.upper)
			cred.lower=apply(pred.sims,2,q.lower)
			list(cred.mean=cred.mean, cred.sd=cred.sd, 
				cred.lower=cred.lower, cred.upper=cred.upper,
				y.lab="Estimated Mean")
			}
			else{
				if(parameter=="mu"){
				cred.sd=apply(pred.sims,2,sd)
				cred.upper=apply(pred.sims,2,q.upper)
				cred.lower=apply(pred.sims,2,q.lower)
				list(cred.mean=cred.mean, cred.sd=cred.sd, 
					cred.lower=cred.lower, cred.upper=cred.upper, 
					y.lab="Estimated Mean")
				}		
					else{stop("parameter not valid!")}
			}

		}
		
		else{
		pred.gaussian=apply(pred.sims, 2, get.predgaussian)
		pred.mean=apply(pred.gaussian,2,mean)	
		pred.sd=apply(pred.gaussian,2,sd)
		pred.upper=apply(pred.gaussian,2,q.upper)
		pred.lower=apply(pred.gaussian,2,q.lower)
		list(pred.mean=pred.mean, pred.sd=pred.sd, 
			pred.lower=pred.lower, pred.upper=pred.upper,
			y.lab="Prediction Mean")
		}
		
	}

	else{stop("incorrect type of interval!")}

} #end if gaussian
else{ 
if(object$family=="binomial"){
	if(interval=="credible"|interval=="prediction"){
		mu.pred.sims=exp(pred.sims)/(1+exp(pred.sims))
	
		if(interval=="credible"){
			if(parameter=="eta"){
			cred.mean=apply(pred.sims,2,mean)
			cred.sd=apply(pred.sims,2,sd)
			cred.upper=apply(pred.sims,2,q.upper)
			cred.lower=apply(pred.sims,2,q.lower)
			list(cred.mean=cred.mean, cred.sd=cred.sd, 
				cred.lower=cred.lower, cred.upper=cred.upper, 
				y.lab="Estimated log(odds)")
			}
			else{
				if(parameter=="mu"){
				cred.mean=apply(mu.pred.sims,2,mean)
				cred.sd=apply(mu.pred.sims,2,sd)
				cred.upper=apply(mu.pred.sims,2,q.upper)
				cred.lower=apply(mu.pred.sims,2,q.lower)
				list(cred.mean=cred.mean, cred.sd=cred.sd, 
					cred.lower=cred.lower, cred.upper=cred.upper,
					y.lab="Estimated probability")
				}		
					else{stop("parameter not valid!")}
			}

		}
		
		else{
		pred.binomial<-apply(mu.pred.sims, 2, get.predbinomial)
		pred.mean<-apply(pred.binomial,2,mean)
		pred.sd<-apply(pred.binomial,2,sd)
		list(pred.mean=pred.mean, pred.sd=pred.sd, 
			y.lab="Prediction probability")
		}
		
	}

	else{stop("incorrect type of interval!")}

} #end if binomial
else{ 
if(object$family=="poisson"){
	if(interval=="credible"|interval=="prediction"){
		mu.pred.sims=exp(pred.sims)
	
		if(interval=="credible"){
			if(parameter=="eta"){
			cred.mean=apply(pred.sims,2,mean)
			cred.sd=apply(pred.sims,2,sd)
			cred.upper=apply(pred.sims,2,q.upper)
			cred.lower=apply(pred.sims,2,q.lower)
			list(cred.mean=cred.mean, cred.sd=cred.sd, 
				cred.lower=cred.lower, cred.upper=cred.upper, 
				y.lab="Estimated log(counts)")
			}
			else{
				if(parameter=="mu"){
				cred.mean=apply(mu.pred.sims,2,mean)
				cred.sd=apply(mu.pred.sims,2,sd)
				cred.upper=apply(mu.pred.sims,2,q.upper)
				cred.lower=apply(mu.pred.sims,2,q.lower)
				list(cred.mean=cred.mean, cred.sd=cred.sd, 
					cred.lower=cred.lower, cred.upper=cred.upper, 
					y.lab="Estimated counts")
				}		
					else{stop("parameter not valid!")}
			}

		}
		
		else{
		pred.poisson<-apply(mu.pred.sims, 2, get.predpoisson)
		pred.mean<-apply(pred.poisson,2,mean)
		pred.sd<-apply(pred.poisson,2,sd)
		list(pred.mean=pred.mean, pred.sd=pred.sd, 
			y.lab="Prediction counts")
		}
		
	}

	else{stop("incorrect type of interval!")}

} 

}

}

}
