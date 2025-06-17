#clikcorr <-
#function(data, lower1, upper1, lower2, upper2, cp=.95, dist="n", df=4, sv=NA, nlm=FALSE, ...) {
#if(dist=="n"){
#  return(clikcorr_n(data, lower1, upper1, lower2, upper2, cp=cp, starVal=sv, nlm=nlm, ...))
#}
#else if(dist=="t"){
#  return(clikcorr_t(data, lower1, upper1, lower2, upper2, cp=cp, df=df, starVal=sv, nlm=nlm, ...))
#}
#else{
#  stop(sprintf("distribution must be either n or t!!"))
#}
#}


##### added on May24 2016 by Yanming Li for implementing the clikcorr S3 class ######
#####

#### generic function
clikcorr <- function(data, lower1, upper1, lower2, upper2, cp=.95, dist="n", df=4, sv=NA, nlm=FALSE, ...) UseMethod("clikcorr")


#### default function
clikcorr.default <- function(data, lower1, upper1, lower2, upper2, cp=.95, dist="n", df=4, sv=NA, nlm=FALSE, ...){
	data <- as.data.frame(data)
	lower1 <- toString(lower1)
	upper1 <- toString(upper1)
	lower2 <- toString(lower2)
	upper2 <- toString(upper2)

    Fr <- prepare_data(data, lower1, upper1, lower2, upper2)    

	if(dist=="n"){
  		results <- clikcorr_n(data, lower1, upper1, lower2, upper2, cp=cp, starVal=sv, nlm=nlm, ...)
	}
	else if(dist=="t"){
  		results <- clikcorr_t(data, lower1, upper1, lower2, upper2, cp=cp, df=df, starVal=sv, nlm=nlm, ...)
	}
	else{
  		stop(sprintf("distribution must be either n or t!!"))
	}

	est <- list(pairName =c(lower1, lower2),
				pairData =Fr,
				dist=dist,
				df=df,
				coefficients =results$Cor,
				Cov =as.matrix(results$Cov),
 				mean =as.vector(results$Mean),
				CI =as.vector(c(results$LCL, results$UCL, cp)),
 				p.value =results$P0,
 				logLik =results$Loglike,
				call =match.call())

	class(est) <- "clikcorr"
	est
  
}

#### add methods
### print() method

print.clikcorr <- function(x, ...){
	cat("Call:\n")
	print(x$call)
	cat("\n")
 	VEC <- as.vector(c(x$coefficients, x$CI[1], x$CI[2], x$p.value))
    names(VEC) <- c("coefficients", paste(x$CI[3]*100, "%CI.lower",sep=""), paste(x$CI[3]*100, "%CI.upper"), "p.value")
	print(VEC)
}


### logLik() method

logLik.clikcorr <- function(object, ...){
	logLik <- object$logLik
	logLik	

}


### summary() method

summary.clikcorr <- function(object, ...){
	logLik <- logLik(object)
 	VEC <- as.vector(c(object$coefficients, object$CI[1], object$CI[2], object$p.value))
    names(VEC) <- c("coefficients", paste(object$CI[3]*100, "%CI.lower",sep=""), paste(object$CI[3]*100, "%CI.upper", sep=""), "p.value")
	res <- list(call=object$call,
				coefficients=VEC,
				mean=object$mean,
				Cov=object$Cov,
				loglik=logLik)
	
	class(res) <- "summary.clikcorr"
	res
}


