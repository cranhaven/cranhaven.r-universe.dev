#
#  event : A Library of Special Functions for Event Histories
#  Copyright (C) 1998, 1999, 2000, 2001 J.K. Lindsey
#
#  This program is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation; either version 2 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#
#  SYNOPSIS
#
#  kalsurv(response, intensity="exponential", distribution="Pareto",
#	depend="independence", update="Markov", mu=NULL, shape=NULL,
#	renewal=TRUE, density=FALSE, censor=NULL, delta=NULL, ccov=NULL,
#	tvcov=NULL,preg=NULL, ptvc=NULL, pbirth=NULL, pintercept=NULL,
#	pshape=NULL, pinitial=1, pdepend=NULL, pfamily=NULL,
#	envir=parent.frame(), print.level=0, ndigit=10, gradtol=0.00001,
#	steptol=0.00001, iterlim=100, fscale=1, typsize=abs(p),
#	stepmax=10*sqrt(p%*%p))
#
#  DESCRIPTION
#
#    Function to fit various distributions inserted in a Pareto, gamma, or
# Weibull distribution with serial dependence or gamma frailties using
# Kalman-type update for event histories.

kalsurv <- function(response, intensity="exponential",
	distribution="Pareto", depend="independence", update="Markov",
	mu=NULL, shape=NULL, renewal=TRUE, density=FALSE, censor=NULL,
	delta=NULL, ccov=NULL, tvcov=NULL, preg=NULL, ptvc=NULL, pbirth=NULL,
	pintercept=NULL, pshape=NULL, pinitial=1, pdepend=NULL,
	pfamily=NULL, envir=parent.frame(), print.level=0, ndigit=10,
	gradtol=0.00001, steptol=0.00001, iterlim=100, fscale=1,
	typsize=abs(p), stepmax=10*sqrt(p%*%p)){
#
# Pareto (beta) likelihood function for serial dependence
#
ksurvb <- function(p){
	if(rf)b <- mu1(p)
	if(sf)v <- sh1(p[nps1:np])
	z <- .C("ksurvb",
		p=as.double(p),
		y=as.double(resp$response$y),
		x=as.double(resp$ccov$ccov),
		cens=as.integer(resp$response$censor),
		nind=as.integer(nind),
		nobs=as.integer(nobs),
		nbs=as.integer(n),
		nccov=as.integer(nccov),
		model=as.integer(mdl),
		density=as.integer(density),
		pfamily=as.integer(!is.null(pfamily)),
		dep=as.integer(dep),
		birth=as.integer(birth),
		tvc=as.integer(tvc),
		tvcov=as.double(resp$tvcov$tvcov),
		fit=as.integer(0),
		pred=double(n),
		rpred=double(n),
		renewal=as.integer(renewal),
		rf=as.integer(rf),
		bb=as.double(b),
		sf=as.integer(sf),
		vv=as.double(v),
		like=double(1),
		#DUP=FALSE,
		PACKAGE="event")
	z$like}
#
# gamma and Weibull likelihood function for serial dependence
#
ksurvg <- function(p){
	if(rf)b <- mu1(p)
	if(sf)v <- sh1(p[nps1:np])
	z <- .C("ksurvg",
		p=as.double(p),
		y=as.double(resp$response$y),
		x=as.double(resp$ccov$ccov),
		cens=as.integer(resp$response$censor),
		nind=as.integer(nind),
		nobs=as.integer(nobs),
		nbs=as.integer(n),
		nccov=as.integer(nccov),
		model=as.integer(mdl),
		distribution=as.integer(dst),
		density=as.integer(density),
		dep=as.integer(dep),
		birth=as.integer(birth),
		tvc=as.integer(tvc),
		tvcov=as.double(resp$tvcov$tvcov),
		fit=as.integer(0),
		pred=double(n),
		rpred=double(n),
		renewal=as.integer(renewal),
		rf=as.integer(rf),
		bb=as.double(b),
		sf=as.integer(sf),
		vv=as.double(v),
		like=double(1),
		#DUP=FALSE,
		PACKAGE="event")
	z$like}
#
# Pareto (beta) likelihood function for frailty
#
frailb <- function(p){
	if(rf)b <- mu1(p)
	if(sf)v <- sh1(p[nps1:np])
	z <- .C("frailb",
		p=as.double(p),
		y=as.double(resp$response$y),
		x=as.double(resp$ccov$ccov),
		cens=as.integer(resp$response$censor),
		nind=as.integer(nind),
		nobs=as.integer(nobs),
		nbs=as.integer(n),
		nccov=as.integer(nccov),
		model=as.integer(mdl),
		density=as.integer(density),
		dep=as.integer(dep),
		birth=as.integer(birth),
		tvc=as.integer(tvc),
		tvcov=as.double(resp$tvcov$tvcov),
		fit=as.integer(0),
		pred=double(n),
		rpred=double(n),
		rf=as.integer(rf),
		bb=as.double(b),
		sf=as.integer(sf),
		vv=as.double(v),
		frser=as.integer(frser),
		like=double(1),
		#DUP=FALSE,
		PACKAGE="event")
	z$like}
call <- sys.call()
#
# check model
#
tmp <- c("exponential","Weibull","gamma","gen logistic",
	"log normal","log logistic","log Cauchy","log Laplace")
mdl <- match(intensity <- match.arg(intensity,tmp),tmp)
tmp <- c("Pareto","gamma","Weibull")
dst <- match(distribution <- match.arg(distribution,tmp),tmp)
if(distribution=="Pareto"){
	if(depend=="frailty")surv <- frailb
	else surv <- ksurvb}
else {
	if(depend=="frailty")stop("not yet available") # surv <- frailg 
	else surv <- ksurvg}
depend <- match.arg(depend,c("independence","serial","frailty"))
tmp <- c("elapsed Markov","serial","Markov","event","cumulated","count","kalman","time")
dep <- match(update <- match.arg(update,tmp),tmp)
rf <- !is.null(mu)
sf <- !is.null(shape)
#
# check if a data object is being supplied
#
type <- "unknown"
respenv <- exists(deparse(substitute(response)),envir=parent.frame())&&
	inherits(response,"repeated")&&!inherits(envir,"repeated")
if(respenv){
	if(dim(response$response$y)[2]>1)
		stop("kalsurv only handles univariate responses")
	if(!is.null(response$NAs)&&any(response$NAs))
		stop("kalsurv does not handle data with NAs")
	type <- response$response$type}
envname <- if(respenv)deparse(substitute(response))
	else if(!is.null(class(envir)))deparse(substitute(envir))
	else NULL
#
# if envir, remove extra (multivariate) responses
#
if(!respenv&&inherits(envir,"repeated")){
	if(!is.null(envir$NAs)&&any(envir$NAs))
		stop("kalsurv does not handle data with NAs")
	cn <- deparse(substitute(response))
	if(length(grep("\"",cn))>0)cn <- response
	if(length(cn)>1)stop("only one response variable allowed")
	response <- envir
	col <- match(cn,colnames(response$response$y))
	if(is.na(col))stop(paste("response variable",cn,"not found"))
	type <- response$response$type[col]
	if(dim(response$response$y)[2]>1){
		response$response$y <- response$response$y[,col,drop=FALSE]
		if(!is.null(response$response$delta)){
			response$response$delta <- response$response$delta[,col,drop=FALSE]
			if(all(response$response$delta==1)||all(is.na(response$response$delta)))response$response$delta <- NULL}
		if(!is.null(response$response$censor)){
			response$response$censor <- response$response$censor[,col,drop=FALSE]
			if(all(response$response$censor==1)||all(is.na(response$response$censor)))response$response$censor <- NULL}}}
#
# find the covariates
#
if(respenv||inherits(envir,"repeated")){
	if(rf)resp <- rmna(response$response)
	else {
		resp <- response
		# time-constant covariates
		if(is.null(ccov))resp$ccov <- NULL
		else if(inherits(ccov,"formula")){
			if(any(is.na(match(rownames(attr(terms(ccov,data=response),"factors")),colnames(response$ccov$ccov)))))
				stop("ccov covariate(s) not found")
			tmp <- wr(ccov,data=response,expand=FALSE)$design
			resp$ccov$ccov <- tmp[,-1,drop=FALSE]
			rm(tmp)}
		else stop("ccov must be a W&R formula")
		# time-varying covariates
		if(is.null(tvcov))resp$tvcov <- NULL
		else if(inherits(tvcov,"formula")){
			if(any(is.na(match(rownames(attr(terms(tvcov,data=response),"factors")),colnames(response$tvcov$tvcov)))))
				stop("tvcov covariate(s) not found")
			tmp <- wr(tvcov,data=response)$design
			resp$tvcov$tvcov <- tmp[,-1,drop=FALSE]
			rm(tmp)}
		else stop("tvcov must be a W&R formula")}
	nccov <- if(rf||is.null(resp$ccov$ccov)) 0
		 else  dim(resp$ccov$ccov)[2]
	ttvc <- if(rf||is.null(resp$tvcov$tvcov)) 0
		 else  dim(resp$tvcov$tvcov)[2]}
else {
	# make response object
	if(inherits(response,"response")){
		resp <- response
		type <- response$type}
	else {
		if(is.null(censor)){
			if(is.matrix(response)||is.data.frame(response))
				censor <- rep(1,dim(response)[1])
			else if(is.list(response))
				censor <- rep(1,length(response))}
		resp <- restovec(response,censor=censor,delta=delta)}
	# time-constant covariates
	if(is.null(ccov))nccov <- 0
	else {
		if(!inherits(ccov,"tccov")){
			ccname <- deparse(substitute(ccov))
			if((is.matrix(ccov)&&is.null(colnames(ccov)))){
				ccname <- deparse(substitute(ccov))
				if(dim(ccov)[2]>1){
					tmp <- NULL
					for(i in 1:dim(ccov)[2])
						tmp <- c(tmp,paste(ccname,i,sep=""))
					ccname <- tmp}}
			ccov <- tcctomat(ccov,names=ccname)}
		nccov <- if(rf) 0 else dim(ccov$ccov)[2]}
	# time-varying covariates
	if(is.null(tvcov))ttvc <- 0
	else {
		if(!inherits(tvcov,"tvcov")){
			tvcname <- deparse(substitute(tvcov))
			if(is.list(tvcov)&&dim(tvcov[[1]])[2]>1){
				if(is.null(colnames(tvcov[[1]]))){
					tvcname <- deparse(substitute(tvcov))
					tmp <- NULL
					for(i in 1:dim(tvcov[[1]])[2])tmp <- c(tmp,paste(tvcname,i,sep=""))
					tvcname <- tmp}
				else tvcname <- colnames(tvcov[[1]])}
			tvcov <- tvctomat(tvcov, names=tvcname)}
		ttvc <- if(rf) 0 else dim(tvcov$tvcov)[2]}
	resp <- rmna(response=resp, tvcov=tvcov, ccov=ccov)
	if(!is.null(ccov))rm(ccov)
	if(!is.null(tvcov))rm(tvcov)}
if(any(resp$response$y<0))stop("All times must be non-negative")
n <- dim(resp$response$y)[1]
nobs <- nobs(resp)
nind <- length(nobs)
if((inherits(envir,"repeated")&&(length(nobs)!=length(nobs(envir))||
	any(nobs!=nobs(envir))))||(inherits(envir,"tvcov")&&
	(length(nobs)!=length(envir$tvcov$nobs)||any(nobs!=envir$tvcov$nobs))))
	stop("response and envir objects are incompatible")
if(type!="unknown"&&type!="duration"&&type!="continuous")
	stop("duration data required")
#
# if a data object was supplied, modify formulae or functions to read from it
#
mu3 <- sh3 <- NULL
if(respenv||inherits(envir,"repeated")||inherits(envir,"tccov")||inherits(envir,"tvcov")){
	if(is.null(envname))envname <- deparse(substitute(envir))
	if(inherits(mu,"formula")){
		mu3 <- if(respenv)finterp(mu,.envir=response,.name=envname)
			else finterp(mu,.envir=envir,.name=envname)}
	else if(is.function(mu)){
		if(is.null(attr(mu,"model"))){
		        tmp <- parse(text=deparse(mu)[-1])
		        mu <- if(respenv)fnenvir(mu,.envir=response,.name=envname)
		        	else fnenvir(mu,.envir=envir,.name=envname)
		        mu3 <- mu
		        attr(mu3,"model") <- tmp}
		else mu3 <- mu}
	if(inherits(shape,"formula")){
		sh3 <- if(respenv)finterp(shape,.envir=response,.name=envname)
			else finterp(shape,.envir=envir,.name=envname)}
	else if(is.function(shape)){
		if(is.null(attr(shape,"model"))){
		        tmp <- parse(text=deparse(shape)[-1])
		        shape <- if(respenv)fnenvir(shape,.envir=response,.name=envname)
		        	else fnenvir(shape,.envir=envir,.name=envname)
		        sh3 <- shape
		        attr(sh3,"model") <- tmp}
		else sh3 <- shape}}
#
# transform location formula to function and check number of parameters
#
npreg <- length(preg)
mu1 <- sh1 <- v <- b <- NULL
if(inherits(mu,"formula")){
	pr <- if(npreg>0)preg else ptvc
	npr <- length(pr)
	mu2 <- if(respenv)finterp(mu,.envir=response,.name=envname,.expand=is.null(preg))
		else finterp(mu,.envir=envir,.name=envname,.expand=is.null(preg))
	npt1 <- length(attr(mu2,"parameters"))
	if(is.character(attr(mu2,"model"))){
	# W&R formula
		if(length(attr(mu2,"model"))==1){
			mu1 <- function(p) p[1]*rep(1,n)
			attributes(mu1) <- attributes(mu2)
			mu2 <- NULL}}
	else {
	# formula with unknowns
		if(npr!=npt1&&length(ptvc)!=npt1){
			cat("\nParameters are ")
			cat(attr(mu2,"parameters"),"\n")
			stop(paste("preg or ptvc should have",npt1,"estimates"))}
		if(is.list(pr)){
			if(!is.null(names(pr))){
				o <- match(attr(mu2,"parameters"),names(pr))
				pr <- unlist(pr)[o]
				if(sum(!is.na(o))!=length(pr))
					stop("invalid estimates for mu - probably wrong names")}
			else pr <- unlist(pr)
			if(npreg>0)preg <- pr else ptvc <- pr}}
	if(!is.null(mu2)){
		if(inherits(envir,"tccov")){
			cv <- covind(response)
			mu1 <- function(p) mu2(p)[cv]
			attributes(mu1) <- attributes(mu2)}
		else {
			mu1 <- mu2
			rm(mu2)}}}
else if(is.function(mu))mu1 <- mu
#
# give appropriate attributes to mu1 for printing
#
if(!is.null(mu1)&&is.null(attr(mu1,"parameters"))){
	attributes(mu1) <- if(is.function(mu)){
		if(!inherits(mu,"formulafn")){
			if(respenv)attributes(fnenvir(mu,.envir=response))
			else attributes(fnenvir(mu,.envir=envir))}
		else attributes(mu)}
		else {
			if(respenv)attributes(fnenvir(mu1,.envir=response))
			else attributes(fnenvir(mu1,.envir=envir))}}
#
# check that correct number of estimates was supplied
#
nlp <- if(is.function(mu1))length(attr(mu1,"parameters"))
	else if(is.null(mu1))NULL
	else npt1
if(!is.null(nlp)){
	if(is.null(ptvc)&&nlp!=npreg)
		stop(paste("preg should have",nlp,"initial estimates"))
	else if(!is.null(ptvc)&&length(ptvc)!=nlp)
		stop(paste("ptvc should have",nlp,"initial estimates"))}
#
# transform shape formula to function and check number of parameters
#
nps <- length(pshape)
if(inherits(shape,"formula")){
	sh2 <- if(respenv)finterp(shape,.envir=response,.name=envname)
		else finterp(shape,.envir=envir,.name=envname)
	npt2 <- length(attr(sh2,"parameters"))
	if(is.character(attr(sh2,"model"))){
	# W&R formula
		if(length(attr(sh2,"model"))==1){
			sh1 <- function(p) p[(nlp+1)]*rep(1,n)
			attributes(sh1) <- attributes(sh2)
			sh2 <- NULL}}
	else {
	# formula with unknowns
		if(nps!=npt2){
			cat("\nParameters are ")
			cat(attr(sh2,"parameters"),"\n")
			stop(paste("pshape should have",npt2,"estimates"))}
		if(is.list(pshape)){
			if(!is.null(names(pshape))){
				o <- match(attr(sh2,"parameters"),names(pshape))
				pshape <- unlist(pshape)[o]
				if(sum(!is.na(o))!=length(pshape))
					stop("invalid estimates for shape - probably wrong names")}
			else pshape <- unlist(pshape)}}
	if(!is.null(sh2)){
		if(inherits(envir,"tccov")){
			cv <- covind(response)
			sh1 <- function(p) sh2(p)[cv]
			attributes(sh1) <- attributes(sh2)}
		else {
			sh1 <- sh2
			rm(sh2)}}}
else if(is.function(shape))sh1 <- shape
#
# give appropriate attributes to sh1 for printing
#
if(!is.null(sh1)&&is.null(attr(sh1,"parameters")))
	attributes(sh1) <- if(is.function(shape)){
		if(!inherits(shape,"formulafn")){
			if(respenv)attributes(fnenvir(shape,.envir=response))
			else attributes(fnenvir(shape,.envir=envir))}
		else attributes(shape)}
		else {
			if(respenv)attributes(fnenvir(sh1,.envir=response))
			else attributes(fnenvir(sh1,.envir=envir))}
#
# check that correct number of estimates was supplied
#
nlp <- if(is.function(shape))length(attr(sh1,"parameters"))
	else if(is.null(shape))NULL
	else npt2
if(!is.null(nlp)&&nlp!=nps)
	stop(paste("pshape should have",nlp,"initial estimates"))
#
# check that appropriate functions supplied
#
if(rf&&!is.function(mu1))stop("mu must be a formula or function")
if(sf&&!is.function(sh1))stop("shape must be a formula or function")
birth <- !is.null(pbirth)
tvc <- length(ptvc)
if(!rf&&(ttvc>0&&tvc!=ttvc||ttvc==0&&tvc>0))
	stop(paste(ttvc,"initial estimates of coefficients for time-varying covariates must be supplied"))
if(rf&&birth)stop("Birth models cannot be fitted with a mean function")
if(intensity=="exponential"){
	sf <- FALSE
	pshape <- NULL}
else {
	if(is.null(pshape))
		stop("Initial value of the shape parameter must be supplied")
	if(!sf){
		if(pshape<=0)stop("Shape must be positive")
		else pshape <- log(pshape)}}
if(intensity=="gen logistic"){
	if(is.null(pintercept))
		stop("Initial value of the intercept parameter must be supplied")}
else pintercept <- NULL
np <- 1+npreg+(depend=="serial")+birth+nps+(!is.null(pintercept))+
	(!is.null(pfamily))
if(pinitial<=0)stop("Estimate of initial parameter must be > 0")
else pinitial <- log(pinitial)
#
# check what dependence is required
#
frser <- FALSE
if(depend=="independence"){
	pdepend <- NULL
	dep <- 0}
else if(depend=="serial"){
	if(update=="time")stop("time update can only be used with frailty")
	if(is.null(pdepend))
		stop("An estimate of the dependence parameter must be supplied")
	else if(pdepend<=0|pdepend>=1)
		stop("Dependence parameter must be between 0 and 1")
	else pdepend <- log(pdepend/(1-pdepend))}
else if(depend=="frailty"){
	frser <- !is.null(pdepend)
	if(frser){
		if(pdepend<=0)stop("Dependence parameter must be positive")
		np <- np+1
		pdepend <- log(pdepend)}
	if(update=="time")dep <- 1
	else {
		dep <- 0
		update <- "no"}}
if(is.null(resp$response$censor))
	resp$response$censor <- rep(1,n)
#
# check number of parameter estimates
#
if(!is.null(pfamily)){
	if(depend=="frailty")
		stop("pfamily cannot be used with frailty dependence")
	if(distribution!="Pareto")
		stop("pfamily can only be used with Pareto distribution")}
if(rf&&npreg>0)nccov <- npreg-1
if(!rf&&nccov+1!=npreg)
	stop(paste(nccov+1,"regression estimates must be supplied"))
if(ttvc>0)np <- np+tvc
#
# set up for location function and make sure it gives correct number
# of values
#
if(rf){
	if(tvc>0&&nccov>0)
		stop("With a mean function, initial estimates must be supplied either in preg or in ptvc")
	if(tvc>0){
		if(length(mu1(ptvc))!=n)
			stop("The mu function must provide an estimate for each observation")
		tvc <- tvc-1
		np <- np+tvc+1}
	else {
		lp <- length(mu1(preg))
		if(lp==1){
			if(nccov==0)mu1 <- function(p) rep(p[1],nind)
			else stop("Number of estimates does not correspond to mu function")}
		else if(lp!=nind)
			stop("The mu function must provide an estimate for each individual")}}
if(sf&&length(sh1(pshape))!=n)
	stop("The shape function must provide an estimate for each observation")
#
# check that the likelihood function gives appropriate values and call nlm
#
nps1 <- np-nps+1
p <- c(preg,pbirth,ptvc,pinitial,pdepend,pfamily,pintercept,pshape)
if(fscale==1)fscale <- surv(p)
if(is.na(surv(p)))
	stop("Likelihood returns NAs: probably invalid initial values")
z0 <- nlm(surv, p=p, hessian=TRUE, print.level=print.level,
	typsize=typsize, ndigit=ndigit, gradtol=gradtol, stepmax=stepmax,
	steptol=steptol, iterlim=iterlim, fscale=fscale)
p <- z0$estimate
like <- z0$minimum
if(!is.null(resp$response$delta))like <- like-
	if(length(resp$response$delta)==1)n*log(resp$response$delta)
	else sum(log(resp$response$delta))
#
# calculate se's
#
a <- if(any(is.na(z0$hessian))||any(abs(z0$hessian)==Inf))0
	else qr(z0$hessian)$rank
if(a==np)cov <- solve(z0$hessian)
else cov <- matrix(NA,ncol=np,nrow=np)
se <- sqrt(diag(cov))
corr <- cov/(se%o%se)
dimnames(corr) <- list(1:np,1:np)
#
# calculate recursive fitted values
#
if(mdl==4)z <- list()
else {
	z <- if(depend=="frailty"){
		if(rf)b <- mu1(p)
		if(sf)v <- sh1(p[nps1:np])
		z <- .C("frailb",
			p=as.double(p),
			y=as.double(resp$response$y),
			x=as.double(resp$ccov$ccov),
			cens=as.integer(resp$response$censor),
			nind=as.integer(nind),
			nobs=as.integer(nobs),
			nbs=as.integer(n),
			nccov=as.integer(nccov),
			model=as.integer(mdl),
			density=as.integer(density),
			dep=as.integer(dep),
			birth=as.integer(birth),
			tvc=as.integer(tvc),
			tvcov=as.double(resp$tvcov$tvcov),
			fit=as.integer(1),
			pred=double(n),
			rpred=double(n),
			rf=as.integer(rf),
			bb=as.double(b),
			sf=as.integer(sf),
			vv=as.double(v),
			frser=as.integer(frser),
			like=double(1),
			#DUP=FALSE,
			PACKAGE="event")}
	else if(distribution=="Pareto"){
		if(rf)b <- mu1(p)
		if(sf)v <- sh1(p[nps1:np])
		z <- .C("ksurvb",
			p=as.double(p),
			y=as.double(resp$response$y),
			x=as.double(resp$ccov$ccov),
			cens=as.integer(resp$response$censor),
			nind=as.integer(nind),
			nobs=as.integer(nobs),
			nbs=as.integer(n),
			nccov=as.integer(nccov),
			model=as.integer(mdl),
			density=as.integer(density),
			pfamily=as.integer(!is.null(pfamily)),
			dep=as.integer(dep),
			birth=as.integer(birth),
			tvc=as.integer(tvc),
			tvcov=as.double(resp$tvcov$tvcov),
			fit=as.integer(1),
			pred=double(n),
			rpred=double(n),
			renewal=as.integer(renewal),
			rf=as.integer(rf),
			bb=as.double(b),
			sf=as.integer(sf),
			vv=as.double(v),
			like=double(1),
			#DUP=FALSE,
			PACKAGE="event")}
	else {
		if(rf)b <- mu1(p)
		if(sf)v <- sh1(p[nps1:np])
		z <- .C("ksurvg",
			p=as.double(p),
			y=as.double(resp$response$y),
			x=as.double(resp$ccov$ccov),
			cens=as.integer(resp$response$censor),
			nind=as.integer(nind),
			nobs=as.integer(nobs),
			nbs=as.integer(n),
			nccov=as.integer(nccov),
			model=as.integer(mdl),
			distribution=as.integer(dst),
			density=as.integer(density),
			dep=as.integer(dep),
			birth=as.integer(birth),
			tvc=as.integer(tvc),
			tvcov=as.double(resp$tvcov$tvcov),
			fit=as.integer(1),
			pred=double(n),
			rpred=double(n),
			renewal=as.integer(renewal),
			rf=as.integer(rf),
			bb=as.double(b),
			sf=as.integer(sf),
			vv=as.double(v),
			like=double(1),
			#DUP=FALSE,
			PACKAGE="event")}
	for(i in 1:n)if(resp$response$y[i]==0){
		z$pred[i] <- z$pred[i-1]
		z$rpred[i] <- z$rpred[i-1]}}
#
# return appropriate attributes on functions
#
if(!is.null(mu3))mu1 <- mu3
if(!is.null(sh3))sh1 <- sh3
z <- list(
	call=call,
	intensity=intensity,
	distribution=distribution,
	pfamily=!is.null(pfamily),
	frser=frser,
	mu=mu1,
	npr=1+nccov+tvc+birth,
	shape=sh1,
	nps=nps,
	density=density,
	depend=depend,
	update=update,
	birth=birth,
	renewal=renewal,
	response=resp$response,
	pred=z$pred,
	rpred=z$rpred,
	ccov=resp$ccov,
	tvcov=resp$tvcov,
	maxlike=like,
	aic=like+np,
	df=n-np,
	npt=np,
	npv=npreg,
	coefficients=p,
	se=se,
	cov=cov,
	corr=corr,
	grad=z0$gradient,
	iterations=z0$iterations,
	code=z0$code)
class(z) <- if(mdl==4)"kalsurv" else c("kalsurv","recursive")
return(z)}

### standard methods
###

deviance.kalsurv <- function(object, ...) 2*object$maxlike

fitted.kalsurv <- function(object, recursive=TRUE, ...)
if(recursive) object$rpred else object$pred

residuals.kalsurv <- function(object, recursive=TRUE, ...)
if(recursive) object$response$y-object$rpred else object$response$y-object$pred

### print method
###
print.kalsurv <- function(x,digits=max(3,.Options$digits-3),correlation=TRUE, ...){
  z <- x ## legacy / S3methods consistency
tvc <- !is.null(z$tvcov)
expm <- z$intensity!="exponential"&&!is.function(z$shape)
glm <- z$intensity=="gen logistic"
npt <- if(is.function(z$shape)) z$npt-z$nps else z$npt
deppar <- (z$depend!="independence")&&(z$depend!="frailty")
cat("\nCall:",deparse(z$call),sep="\n")
cat("\n")
if(z$code>2)cat("Warning: no convergence - error",z$code,"\n\n")
cat("Number of subjects    ",length(nobs(z)),"\n")
cat("Number of observations",length(z$response$y),"\n")
cat(z$distribution,"distribution ")
if(z$renewal){
	if(!z$birth)cat("with renewal process\n")
	else cat("with birth process\n")}
else {
	cat("with zero origin\n")
	if(z$birth)cat("and birth process\n")}
if(z$density)cat(z$intensity," density",sep="")
else cat(z$intensity," intensity",sep="")
if(z$depend=="independence")cat(" with independence\n")
else if(z$depend=="frailty")
	cat(" with",z$depend,"dependence",if(!z$frser)"and",
		z$update,"weight",if(z$frser)"and AR","\n")
else cat(" with ",z$update," update\n",sep="")
cat("\n-Log likelihood   ",z$maxlike,"\n")
cat("Degrees of freedom",z$df,"\n")
cat("AIC               ",z$aic,"\n")
cat("Iterations        ",z$iterations,"\n\n")
cat("Location parameters\n")
if(!is.null(attr(z$mu,"formula")))
	cat(deparse(attr(z$mu,"formula")),sep="\n")
else if(!is.null(attr(z$mu,"model"))){
	t <- deparse(attr(z$mu,"model"))
	t[1] <- sub("expression\\(","",t[1])
	t[length(t)] <- sub("\\)$","",t[length(t)])
	cat(t,sep="\n")}
coef.table <- cbind(z$coef[1:z$npr],z$se[1:z$npr])
if(inherits(z$mu,"formulafn"))
	cname <- if(is.character(attr(z$mu,"model")))attr(z$mu,"model")
		else attr(z$mu,"parameters")
else {
	cname <- "(Intercept)"
	if(z$npv>0)cname <- c(cname,colnames(z$ccov$ccov))
	if(z$birth)cname <- c(cname,"birth")
	if(tvc)cname <- c(cname,colnames(z$tvcov$tvcov))}
dimnames(coef.table) <- list(cname, c("estimate","se"))
print.default(coef.table, digits=digits, print.gap=2)
if(is.function(z$shape))cat("\nDependence parameters\n")
else cat("\nNonlinear parameters\n")
coef <- exp(z$coef[(npt-deppar-z$frser-z$pfamily-glm-expm):npt])
cname <- "initial"
if(deppar){
	cname <- c(cname,"depend")
	coef[2] <- coef[2]/(1+coef[2])}
if(z$frser)cname <- c(cname,"AR")
if(z$pfamily){
	cname <- c(cname,"family")
	coef[length(coef)-glm-expm] <- z$coef[npt-glm-expm]}
if(glm){
	cname <- c(cname,"intercept")
	coef[length(coef)-expm] <- z$coef[npt-expm]
	if(expm){
		cname <- c(cname,"asymptote")
		coef[length(coef)] <- 1/coef[length(coef)]}}
else if(expm)cname <- c(cname,"shape")
coef.table <- cbind(z$coef[(npt-deppar-z$frser-z$pfamily-glm-expm):npt],
	z$se[(npt-deppar-z$frser-z$pfamily-glm-expm):npt],coef)
dimnames(coef.table) <- list(cname, c("estimate","se","parameter"))
print.default(coef.table, digits=digits, print.gap=2)
if(z$depend=="frailty"){
	tmp <- trigamma(exp(-z$coef[npt-deppar-expm-z$frser]))
	cat("Correlation =",tmp/(tmp+trigamma(1)),"\n")}
if(is.function(z$sh1)||is.function(z$shape)){
	cat("\nShape parameters\n")
	if(!is.null(attr(z$sh1,"formula")))
		cat(deparse(attr(z$sh1,"formula")),sep="\n")
	else if(!is.null(attr(z$sh1,"model"))){
		t <- deparse(attr(z$sh1,"model"))
		t[1] <- sub("expression\\(","",t[1])
		t[length(t)] <- sub("\\)$","",t[length(t)])
		cat(t,sep="\n")}
	cname <- if(is.character(attr(z$sh1,"model")))
			attr(z$sh1,"model")
		else attr(z$sh1,"parameters")
	coef.table <- cbind(z$coef[(z$npt-z$nps+1):z$npt],
		z$se[(z$npt-z$nps+1):z$npt])
	dimnames(coef.table) <- list(cname, c("estimate","se"))
	print.default(coef.table, digits=digits, print.gap=2)}
if(correlation){
	cat("\nCorrelation matrix\n")
	print.default(z$corr, digits=digits)}}
