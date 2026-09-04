#
#  growth : A Library of Normal Distribution Growth Curve Models
#  Copyright (C) 1998, 1999, 2000, 2001 J.K. Lindsey
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public Licence as published by
#  the Free Software Foundation; either version 2 of the Licence, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public Licence for more details.
#
#  You should have received a copy of the GNU General Public Licence
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
#  SYNOPSIS
#
#     potthoff(response, x=NULL, ccov=NULL, times=NULL, torder=0,
#	orthogonal=TRUE)
#
#  DESCRIPTION
#
#    Function to fit the Potthoff and Roy (1964) growth curve model



#' Potthoff and Roy Growth Curve Model
#' 
#' \code{potthoff} fits the Potthoff and Roy repeated measurements growth curve
#' model with unstructured covariance matrix to completely balanced data.
#' 
#' 
#' @param response A matrix or dataframe of response values.
#' @param x A matrix defining the complete intersubject differences or a
#' Wilkinson and Rogers formula that will create one.
#' @param ccov A matrix of columns of the baseline covariates to be actually
#' fitted, with one row per individual or a W&R formula that will create one.
#' @param times A vector of unequally spaced times when they are the same for
#' all individuals. Not necessary if equally spaced.
#' @param torder Order of the polynomial in time to be fitted. If non-numeric,
#' the full model in time is fitted.
#' @param orthogonal If TRUE, uses orthogonal polynomials for time, otherwise
#' only centres times at their mean.
#' @return A list of class potthoff is returned.
#' @author J.K. Lindsey
#' @seealso \code{\link[growth]{carma}}, \code{\link[growth]{elliptic}},
#' \code{\link{lm}}.
#' @keywords models
#' @examples
#' 
#' y <- matrix(rnorm(40),ncol=5)
#' x <- gl(2,4)
#' # full model with treatment effect
#' potthoff(y, ~x, torder="f", ccov=~x)
#' # no time trend with treatment effect
#' potthoff(y, ~x, torder=0, ccov=~x)
#' # quadratic time with treatment effect
#' potthoff(y, ~x, torder=2, ccov=~x)
#' # full model without treatment effect
#' potthoff(y, ~x, torder="f")
#' # linear time without treatment effect
#' potthoff(y, ~x, torder=1)
#' 
#' @export potthoff
potthoff <- function(response, x=NULL, ccov=NULL, times=NULL, torder=0,
	orthogonal=TRUE){
pcov <- function(z, s, x){
	p <- t(x)%x%z
	solve(p%*%(diag(n)%x%s)%*%t(p))}
call <- sys.call()
#
# check response
#
if(is.data.frame(response))response <- as.matrix(response)
if(!is.matrix(response))stop("response must be a matrix or dataframe")
n <- dim(response)[1]
r <- dim(response)[2]
#
# set up complete set of intersubject differences
#
if(inherits(x,"formula")){
	formula <- x
	mt <- terms(x)
	if(is.numeric(mt[[2]]))x <- matrix(1,nrow=n)
	else {
		mf <- model.frame(mt,parent.frame(),na.action=na.fail)
		x <- model.matrix(mt, mf)}}
if(is.null(x)||dim(x)[1]!=n)stop(paste("x must be a matrix with",n,"rows"))
#
# set up covariates to be fitted
#
if(inherits(ccov,"formula")){
	formula <- ccov
	mt <- terms(ccov)
	if(is.numeric(mt[[2]]))ccov <- matrix(1,nrow=n)
	else {
		mf <- model.frame(mt,parent.frame(),na.action=na.fail)
		ccov <- model.matrix(mt, mf)}}
if(is.null(ccov)) xx <- matrix(rep(1,n),ncol=1)
else xx <- x
#
# set up times and orthogonalize if necessary
#
if(is.null(times))times <- 1:r
if(orthogonal)z <- rbind(rep(1,length(times)),t(contr.poly(times)))
else {
	z <- rep(1,r)
	tt <- times-sum(times)/r
	z <- rbind(z,tt)
	for(i in 2:(r-1))z <- rbind(z,tt^i)}
#
# calculate covariance matrices
#
s <- t(response)%*%(diag(n)-xx%*%solve(t(xx)%*%xx)%*%t(xx))%*%response/n
ss <- solve(t(response)%*%(diag(n)-x%*%solve(t(x)%*%x)%*%t(x))%*%response/n)
b <- solve(t(xx)%*%xx)%*%t(xx)%*%response
if(!is.matrix(b))b <- matrix(b,nrow=1)
#
# model with polynomial in time
#
if(is.numeric(torder)){
	zz <- z[1:(torder+1),]
	if(!is.matrix(zz))zz <- matrix(zz,nrow=1)
	b1 <- b%*%ss%*%t(zz)%*%solve(zz%*%ss%*%t(zz))
	s1 <- response-xx%*%b1%*%zz
	s1 <- t(s1)%*%s1/n}
else {
	zz <- diag(r)
	b1 <- b
	s1 <- s}
res <- response-xx%*%b1%*%zz
if(!is.matrix(b1))b1 <- matrix(b1,nrow=1)
if(is.matrix(b1)&&dim(b1)[1]>1){
	if(is.null(colnames(ccov))){
		tn <- "(Intercept)"
		if(dim(ccov)[2]>1)
			tn <- c(tn,paste("ccov",1:(dim(ccov)[2]-1),sep=""))
		colnames(ccov) <- tn}
	rownames(b1) <- colnames(ccov)}
else {
	b1 <- matrix(b1,nrow=1)
	rownames(b1) <- "Mean"}
like <- n*(r*(1+log(2*pi))+log(abs(det(s1))))/2
aic <- like+length(b1)+r*(r+1)/2
#
# calculate se's
#
pc <- pcov(zz,solve(s1),xx)
if(is.matrix(pc)){
	d <- sqrt(diag(pc))
	se <- matrix(d,ncol=dim(b1)[2],byrow=TRUE)}
else se <- sqrt(pc)
corr <- pc
if(is.matrix(pc))corr <- pc/(d%o%d)
if(is.matrix(se)&&dim(se)[1]>1)rownames(se) <- colnames(ccov)
else {
	se <- matrix(se,nrow=1)
	rownames(se) <- "Mean"}
if(!is.numeric(torder)){
	if(is.null(colnames(response)))
		colnames(b1) <- paste("t",1:dim(b1)[2],sep="")
	else colnames(b1) <- colnames(response)
	if(is.null(colnames(response)))
		colnames(se) <- paste("t",1:dim(se)[2],sep="")
	else colnames(se) <- colnames(response)}
d <- sqrt(diag(s1))
c1 <- s1
for(i in 2:r)for(j in 1:(i-1))c1[i,j] <- s1[i,j]/d[i]/d[j]
z <- list(
	call=call,
	y=response,
	x=x,
	time=z,
	torder=torder,
	ns=n,
	nt=n*r,
	df=n*r-(length(b1)+r*(r+1)/2),
	beta=b1,
	ccov=s1,
	maxlike=like,
	aic=aic,
	pcov=pc,
	pcorr=corr,
	se=se,
	corr=c1,
	residuals=res)
class(z) <- "potthoff"
return(z)}

### standard methods
###
#' @export 
coef.potthoff <- function(object, ...) object$beta
#' @export 
deviance.potthoff <- function(object, ...) 2*object$maxlike
#' @export 
residuals.potthoff <- function(object, ...) object$residuals

### print method
###
#' @export 
print.potthoff <- function(x, digits = max(3, .Options$digits - 3), ...) {
  z <- x; # S3 consistency
cat("\nCall:",deparse(z$call),sep="\n")
cat("\n")
cat("Number of subjects    ",z$ns,"\n")
cat("Number of observations",z$nt,"\n")
cat("-Log likelihood   ",z$maxlike,"\n")
cat("Degrees of freedom",z$df,"\n")
cat("AIC               ",z$aic,"\n\n")
if(is.null(colnames(z$beta))&&z$torder<=4){
	tn <- "(Intercept)"
	if(z$torder>0)tn <- c(tn,paste("t^",1:z$torder,sep=""))
	colnames(z$beta) <- tn
	colnames(z$se) <- tn}
cat("Estimates of linear parameters\n")
print(z$beta)
cat("Standard errors\n")
print(z$se)
nlp <- length(z$beta)
if(nlp>1){
	cat("\nCorrelation matrix of linear parameters\n")
	dimnames(z$pcorr) <- list(seq(1,nlp),seq(1,nlp))
	print.default(z$pcorr,digits=digits)}
cat("\nEstimated covariance/correlation matrix\n")
if(is.null(colnames(z$corr))){
	tn <- paste("t^",1:dim(z$corr)[2],sep="")
	dimnames(z$corr) <- list(tn,tn)}
print(z$corr)}
