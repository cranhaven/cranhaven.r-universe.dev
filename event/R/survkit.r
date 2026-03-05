#
#  event : A Library of Special Functions for Event Histories
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
#  survkit(times, censor=NULL, ccov=NULL, tvcov=NULL,
#	strata=NULL, id=NULL, model="Weibull", baseline=FALSE,
#	residuals=FALSE, survival=NULL, svalues=NULL, valrho=NULL,
#	constraints=NULL, impose=NULL, dist=NULL, random=NULL,
#	estimate=NULL, moments=FALSE, rule=NULL, pedigree=NULL,
#	integrate=NULL, jointmode=FALSE, within=NULL, converge=1.e-8,
#	iterlim=100)
#
#  DESCRIPTION
#
#    Function to fit  Weibull and Cox models with frailties

# .First.lib <- function(lib, pkg)
# 	library.dynam("event", pkg, lib)
# require(rmutil)

survkit <- function(times, censor=NULL, ccov=NULL, tvcov=NULL,
	strata=NULL, id=NULL, model="Weibull", baseline=FALSE,
	residuals=FALSE, survival=NULL, svalues=NULL, valrho=NULL,
	constraints=NULL, impose=NULL, dist=NULL, random=NULL,
	estimate=NULL, moments=FALSE, rule=NULL, pedigree=NULL,
	integrate=NULL, jointmode=FALSE, within=NULL, converge=1.e-8,
	iterlim=100){
#
# fixed Fortran parameters
#
#-- MXSTRA= max. number of strata, i.e. levels of the strata variable
#--    defined in the STRATA statement.
mxstra <- 25
#-- NDIMAX = max. total number of levels of effects in the model
#--    (size of the vector of solutions)
ndimax <- 100
#-- MXEF = max. number of covariates. These are discrete
#--    (CLASS) covariates and continuous covariates.
mxef <- 30
#-- NSTIMAX = maximum number of distinct times or quantiles that
#--    can be defined in the SURVIVAL statement options SPECIFIC
#--    and QUANTILE.
nstimax <- 20
#-- NTIMMAX = largest possible value of the (dependent) time variable
ntimmax <- 6000
#-- EPS_BS = convergence criterion
eps.bf <- converge
#--    no_log = 1 -> rho is used in the maximisation routine
#--    no_log = 0 -> log(rho) is used in the maximisation routine
no.log <- 0
#-- ITER_BF = maximum number of iterations.
iter.bf <- iterlim
call <- sys.call()
model <- match.arg(model,c("Weibull","Cox","Kaplan.Meier"))
#
# time, censor, id
#
if(max(times)>ntimmax)stop(paste("maximum time must be <",ntimmax))
if(any(trunc(times)!=times))warning("some times are not integers")
if(any(times<=0))stop("times must be positive integers")
nrr <- length(times)
if(is.null(censor))censor <- rep(1,nrr)
else if(length(censor)!=nrr)stop("censor must have same length as times")
if(!is.null(id)){
	if(length(id)!=nrr)stop("id must have same length as times")
	if(is.factor(id))id <- as.numeric(as.numeric(id))
	nunique <- length(unique(id))}
else nunique <- nrr
info <- cbind(times,censor)
squant <- xtabl <- itabl <- fact <- cont <- xlabelc <- xtabl2 <-
	itabl2 <- info2 <- xlabelf <- xlabelrg <- ind1 <- ind2 <-
	xlabelrn <- istime <- NULL
integam <- nwithin <- ninttdep <- nrule <- nsurv <- nrr2 <-
	nstime <- ncons <- nrelmat <- nquant <- npest <-
	xinclu <- nall.tim <- nccov <- ndcov <- ncoef <- 0
#
# covariates
# model : assume all variables in model
#
if(!is.null(ccov)){
	if(!inherits(ccov,"formula"))stop("ccov must be a model formula")
	mf <- model.frame(terms(ccov),parent.frame(),
		na.action=na.fail)
	if(dim(mf)[1]!=nunique&&dim(mf)[1]!=nrr)
		stop("time-constant covariates must have one value per individual or one per time")
	for(i in 1:dim(mf)[2]){
		if(nunique!=nrr&&dim(mf)[1]<nrr)mf[,i] <- mf[,i][id]
		if(is.factor(mf[,i])){
			xlabelf <- c(xlabelf,colnames(mf)[i])
			ndcov <- ndcov+1
			fact <- c(fact,length(levels(mf[,i])))
			itabl <- cbind(itabl,as.numeric(mf[,i]))}
		else if(is.numeric(mf[,i])){
			xlabelc <- c(xlabelc,colnames(mf)[i])
			nccov <- nccov+1
			cont <- c(cont,1)
			xtabl <- cbind(xtabl,as.numeric(mf[,i]))}}}
includ1 <- rep(1,nccov)
includ2 <- rep(1,ndcov)
ind1 <- cont
ind2 <- fact
if(model!="Weibull"){
	iplus1 <- rep(0,nccov)
	iplus2 <- rep(0,ndcov)}
if(!is.null(tvcov)){
	if(!inherits(tvcov,"formula"))stop("tvcov must be a model formula")
	mf <- model.frame(terms(tvcov),parent.frame(),
		na.action=na.fail)
	if(dim(mf)[1]!=nrr)
		stop("time-constant covariates must have one value per time")
	for(i in 1:dim(mf)[2]){
		if(is.factor(mf[,i])){
			xlabelf <- c(xlabelf,colnames(mf)[i])
			fact <- c(fact,length(levels(mf[,i])))
			ndcov <- ndcov+1
			includ2 <- c(includ2,1)
			itabl <- cbind(itabl,as.numeric(mf[,i]))
			if(model=="Cox"){
				iplus2 <- c(iplus2,1,-1)
				ndcov <- ndcov+1
				ind2 <- c(ind2,0,length(levels(mf[,i])))
				includ2 <- c(includ2,0)
				itabl <- cbind(itabl,if(censor!=-1)0
					else c(as.numeric(mf[,i])[2:nrr],0))}
			else ind2 <- c(ind2,length(levels(mf[,i])))}
		else if(is.numeric(mf[,i])){
			xlabelc <- c(xlabelc,colnames(mf)[i])
			cont <- c(cont,1)
			nccov <- nccov+1
			includ1 <- c(includ1,1)
			xtabl <- cbind(xtabl,as.numeric(mf[,i]))
			if(model=="Cox"){
				iplus1 <- c(iplus1,1,-1)
				nccov <- nccov+1
				ind1 <- c(ind1,0,1)
				includ1 <- c(includ1,0)
				xtabl <- cbind(xtabl,if(censor!=-1)0
					else c(as.numeric(mf[,i])[2:nrr],0))}
			else ind1 <- c(ind1,1)}}}

ind <- c(0,cumsum(c(ind1,ind2)))
includ <- c(includ1,includ2)
xnor <- xgam <- rep(0,nccov+ndcov+1)
inor <- igam <- matrix(0,ncol=3,nrow=nccov+ndcov+1)
if(nccov+ndcov>0)bound <- matrix(0,nrow=nccov+ndcov,ncol=3)
else bound <- NULL
#
# constraints
#
icons <- rep(0,ndimax+1)
if(!is.null(constraints)&&is.null(impose)){
	tmp <- c("none","find","largest","first")
	ncons <- match(constraints <- match.arg(constraints,tmp),tmp)-3
	if(is.na(ncons))stop("unknown constraint")
	if(ncons==1){
		icons[1] <- length(fact)
		if(icons[1]==1) icons[2] <- length(ind1)+1
		else icons[2:(length(fact)+1)] <- cumsum(c(0,fact[2:length(fact)]))+length(ind1)+1}}
#
# impose is a list of a vector of variable names and a vector of their
#	constraint levels
#
if(!is.null(impose)){
	ncons <- 1
	icons[1] <- length(fact)
	if(icons[1]==1) icons[2] <- length(ind1)+1
	else icons[2:(length(fact)+1)] <- cumsum(c(0,fact[2:length(fact)]))+length(ind1)+1
	for(i in 1:length(impose[[1]])){
		tmp <- match(impose[[1]][i],xlabelf)
		if(is.na(tmp))stop("imposed constraint variable not found")
		icons[1] <- icons[1]+1
		icons[tmp+1] <- ind[tmp+length(ind1)]+impose[[2]][i]}}
#
# baseline, kaplan, and residuals  for Cox
nbase <- as.integer(baseline)
nkaplan <- as.integer(model=="Kaplan.Meier")
if(residuals){
	if(model=="Cox")nsurv <- 2
	else stop("residuals only available for Cox model")}
#
# random requires dist, estimate, and maybe rule
#
if(!is.null(random)){
	if(!is.factor(random))stop("random must be a factor variable")
	if(length(random)!=nrr)
		stop("random covariates must have one value per time")
	if(is.null(dist))
		stop("a dist must be provided for the random covariate")
	if(is.null(estimate))
		stop("estimates must be provided for the random covariate")
	ndcov <- ndcov+1
	itabl <- cbind(itabl,as.numeric(random))
	includ <- c(includ,1)
	if(is.na(match(dist,c("loggamma","normal","multivariate"))))
		stop("unknown dist in random")
	xlabelf <- c(xlabelf,paste(deparse(substitute(random))))
	fact <- c(fact,length(levels(random)))
	if(dist=="loggamma"){
		igam[1,1] <- 1
		colgam <- ndcov
		xlabelrg <- c(xlabelrg,paste(deparse(substitute(random))))
		igam <- rbind(igam,c(1,0,0))
		inor <- rbind(inor,c(0,0,0))}
	else if(dist=="normal"){
		inor[1,1] <- 1
		colnor <- ndcov
		xlabelrn <- c(xlabelrn,paste(deparse(substitute(random))))
		igam <- rbind(igam,c(0,0,0))
		inor <- rbind(inor,c(1,0,0))}
	else if(dist=="multivariate"){
		if(is.null(pedigree)||!is.matrix(pedigree))
			stop("a pedigree matrix must be supplied with multivariate random effects")
		if(dim(pedigree)[2]!=4)
			stop("the pedigree matrix must have 4 columns: id, sex, father, mother")
		pedigree <- t(rbind(pedigree,matrix(0,ncol=4,nrow=ndimax-dim(pedigree)[1])))
		if(is.null(rule))rule <- "usual"
		tmp <- c("usual","mgs","sire.dam")
		nrule <- match(rule <- match(dist,tmp),tmp)
		nrelmat <- nccov+ndcov}
	if(length(estimate)==3){
		if(moments)npest <- 100
		else npest <- 1
		if(estimate[1]<=0)
			stop("lower estimate bound negative")
		else if(estimate[1]>=estimate[2])
			stop("incorrect estimate bounds")
		bound <- rbind(bound,estimate)
		if(dist=="loggamma"){
			xgam <- c(xgam,estimate[2])
			xnor <- c(xnor,0)}
		else {
			xgam <- c(xgam,0)
			xnor <- c(xnor,estimate[1])}}
	else {
		if(dist=="loggamma"){
			xgam <- c(xgam,estimate)
			xnor <- c(xnor,0)}
		else if(dist=="normal"){
			xnor <- c(xnor,estimate)
			xgam <- c(xgam,0)}
		bound <- rbind(bound,rep(0,3))}}
#
# integrate
#
if(!is.null(integrate)){
	if(model=="Cox")stop("integrate only possible with Weibull model")
	if(!is.factor(integrate))stop("integrate must be a factor variable")
	if(length(integrate)!=nrr)
		stop("integrate must be the same length as other variables")
	integam <- nccov+ndcov+1
	itabl <- cbind(itabl,as.numeric(integrate))
	includ <- c(includ,0)
	xlabelrg <- c(xlabelrg,paste(deparse(substitute(integrate))))
	if(!is.null(xlabelf)&&any(xlabelrg[length(xlabelrg)]==xlabelf))
		stop("the integrate variable must not be used elsewhere in the model")
	igam[1,1] <- 1
	igam <- rbind(igam,c(1,0,0))
	inor <- rbind(inor,c(0,0,0))
	if(is.null(estimate)||length(estimate)!=1)
		stop("one estimate must be supplied with integrate")
	xgam <- c(xgam,estimate)
	xnor <- c(xnor,0)
	bound <- rbind(bound,rep(0,3))
	if(!is.null(within)){
		if(!is.factor(within))stop("within must be a factor variable")
		if(nunique!=nrr&&length(within)<nrr)
			within <- within[id]
		nwithin <- 1}}
else jointmode <- FALSE
#
# strata
#
if(!is.null(strata)){
	if(!is.factor(strata))stop("strata must be a factor variable")
	else if(length(strata)!=nrr)
		stop("strata variable must have the same length as other variables")
	nstrata <- length(levels(strata))
	if(nstrata>mxstra)stop(paste("maximum",mxstra,"strata allowed"))
	info <- cbind(info,as.numeric(strata))}
else {
	nstrata <- 1
	if(nwithin==1)info <- cbind(info,as.numeric(within))
	else info <- cbind(info,as.numeric(rep(1,nrr)))}

if(model=="Weibull"&&integam>0)info <- cbind(info,itabl[,integam-nccov])
else if(is.null(id))info <- cbind(info,1:nrr)
else info <- cbind(info,id)
nind <- length(unique(info[,4]))
#
# rho_fixed
#
if(model=="Cox")icrho <- 0
else if(!is.null(valrho)){
	icrho <- 0
	if(is.vector(valrho,mode="numeric")&&length(valrho)!=nstrata)
		stop("valrho must have a value for each	stratum")
	valrho <- c(valrho,rep(0,mxstra-nstrata))}
else {
	icrho <- 1
	valrho <- if(no.log==0)rep(1,mxstra)
		else rep(0,mxstra)}
#
# survival
#
if(!is.null(survival)){
	if(nsurv==2)stop("survival and residuals cannot both be chosen")
	nsurv <- 1
	survival <- match.arg(survival,c("equal","quantiles","specific","all.times"))
	if(survival!="all.times"){
		if(is.null(svalues))
			stop("svalues must be supplied with survival")
		if(!is.vector(svalues,mode="numeric"))
			stop("svalues must be a vector")
		if(length(svalues)>nstimax)stop("too many svalues")
		if(survival!="equal"&&any(diff(svalues)<=0))
			stop("svalues must be strictly increasing")}
	else if(model!="Cox")
		stop("all.times survival only valid with Cox model")
	if(survival=="quantiles"){
		if(any(svalues<0|svalues>100))
			stop("quantiles must be between 0 and 100")
		squant <- 0.01*svalues
		nquant <- length(squant)}
	else if(survival=="equal"){
		if(length(svalues)!=2)stop("2 svalues must be given")
		else if(svalues[1]>=svalues[2])
			stop("time interval must be smaller than time limit")
		istime <- seq(svalues[1],svalues[2],by=svalues[1])
		if(length(istime)>nstimax)
			stop("time interval too small")
		else nstime <- length(istime)}
	else if(survival=="all.times")nall.tim <- 1
	else {
		if(any(svalues<=0))stop("svalues must be positive")
		istime <- svalues
		if(length(istime)>nstimax)
			stop("time interval too small")
		else nstime <- length(istime)}}

if(nsurv>0){
	info2 <- info
	itabl2 <- itabl
	xtabl2 <- xtabl
	nrr2 <- nrr}
if(model=="Weibull"){
	if(nstrata>1||integam>0){
		if(!is.null(integrate)&&nwithin==0)
			o <- order(itabl[,integam-nccov],info[,4],times)
			else o <- order(info[,3],info[,4],times)}
	else o <- order(info[,4],times)}
else {
	if(nstrata>1)o <- order(info[,3],-times,censor)
	else o <- order(-times,censor)}
info <- info[o,]
itabl <- itabl[o,,drop=FALSE]
xtabl<- xtabl[o,]
#
# call Fortran
#
iconst <- c(ncons,nsurv,nstrata,no.log,nrelmat,integam,nwithin,
	nccov+sum(includ),nall.tim,nbase,icrho,ninttdep,jointmode,
	npest,model=="Cox",nrule,nkaplan,iter.bf)
rconst <- c(xinclu,eps.bf)
if(model=="Cox"&&length(iplus2)==0)iplus2 <- 0
if(is.null(istime))istime <- 0
z0 <- if(model=="Weibull")
	.Fortran("weibull",
	iconst=as.integer(iconst),
	rconst=as.double(rconst),
	istime=as.integer(istime),
	squant=as.double(squant),
	valrho=as.double(valrho),
	includ=as.integer(includ),
	info=as.integer(info),
	itabl=as.integer(itabl),
	xtabl=as.double(xtabl),
	nrr=as.integer(nrr),
	info2=as.integer(info2),
	itabl2=as.integer(itabl2),
	xtabl2=as.double(xtabl2),
	ipedig=as.integer(pedigree),
	nrr2=as.integer(nrr2),
	nccov=as.integer(nccov),
	ndcov=as.integer(ndcov),
	nstime=as.integer(nstime),
	nquant=as.integer(nquant),
	igam=as.integer(igam),
	inor=as.integer(inor),
	xgam=as.double(xgam),
	xnor=as.double(xnor),
	bound=as.double(bound),
	icons=as.integer(icons),
	beta=double(ndimax),
	stder=double(ndimax),
	grad=double(ndimax),
	hess=double(ndimax*ndimax),
	xmom=double(4),
	surv=double(3*nstimax*nrr),
	like=double(2),
	df=integer(2),
	#DUP=FALSE,
	PACKAGE="event")
else
	.Fortran("cox",
	iconst=as.integer(iconst),
	rconst=as.double(rconst),
	istime=as.integer(istime),
	squant=as.double(squant),
	includ=as.integer(includ),
	info=as.integer(info),
	itabl=as.integer(itabl),
	xtabl=as.double(xtabl),
	nrr=as.integer(nrr),
	info2=as.integer(info2),
	itabl2=as.integer(itabl2),
	xtabl2=as.double(xtabl2),
	iplus1=as.integer(iplus1),
	iplus2=as.integer(iplus2),
	ipedig=as.integer(pedigree),
	nrr2=as.integer(nrr2),
	nccov=as.integer(nccov),
	ndcov=as.integer(ndcov),
	nstime=as.integer(nstime),
	nquant=as.integer(nquant),
	igam=as.integer(igam),
	inor=as.integer(inor),
	xgam=as.double(xgam),
	xnor=as.double(xnor),
	bound=as.double(bound),
	icons=as.integer(icons),
	beta=double(ndimax),
	stder=double(ndimax),
	grad=double(ndimax),
	hess=double(ndimax*ndimax),
	xmom=double(4),
	surv=double(3*nstimax*nrr),
	like=double(2),
	df=integer(2),
	km=double(nrr*nstrata*4),
	resid=double(nrr*nstrata*3),
	ut=as.integer(nrr*nstrata),
	#DUP=FALSE,
	PACKAGE="event")
if(z0$iconst[1]>0)switch(as.character(z0$iconst[1]),
	"1"=stop("too many error constraints"),
	"2"=stop("negative value on the diagonal of the Hessian"),
	"3"=stop("algorithm diverged"),
	"4"=stop("survival curve changes too many times"),
	"5"=stop("matrix is not semi-positive definite"),
	"6"=stop("too many parameters"))
code <- z0$iconst[3]>=iterlim+2*(z0$iconst[1]==11)
if(nkaplan>0){
	km <- matrix(z0$km,ncol=4)
	rownames(km) <- km[,1]
	km <- km[km[,1]>0,2:4]
	colnames(km) <- c("survival function","log variance","cumulative hazard")
	km <- list(km=km,nstrata=nstrata)
	class(km) <- c("survivalkit",model)
	return(km)}
else if(model=="Cox"){
	if(baseline){
		base <- matrix(z0$km,ncol=4)
		rownames(base) <- base[,1]
		base <- base[base[,1]>0,2:3]
		colnames(base) <- c("cumulative hazard","survival function")}
	else base <- NULL
	if(residuals){
		o <- order(info2[,1])
		info2 <- info2[o,]
		resid <- matrix(z0$resid,ncol=3)
		o <- resid[,1]>0
		resid <- resid[resid[,1]>0,1:3]
		resid <- cbind(info2[info2[,2]>-1&o,1:3],resid)
		colnames(resid) <- c("time","censor","stratum","generalized","martingale","deviance")}
	else resid <- NULL}
else resid <- base <- km <- NULL
if(nsurv==1){
	surv <- matrix(z0$surv,ncol=3)
	surv <- surv[surv[,1]>0,]
	tmp <- surv[,1]
	surv <- surv[,2:3]
	colnames(surv) <- c("time","S(t)")
	rownames(surv) <- tmp}
else surv <- NULL
ndim <- z0$iconst[2]-nstrata*(model=="Weibull")-jointmode
ndimf <- z0$iconst[2]
if(!is.null(xlabelc)||!is.null(xlabelf)){
	xlabel <- c(xlabelc,xlabelf)
	xlabel <- xlabel[rep(1:length(xlabel),c(cont,fact))]}
else xlabel <- NULL
cov <- matrix(z0$hess,ncol=ndimax)[1:ndimf,1:ndimf,drop=FALSE]
if(model=="Weibull"){
	rho <- z0$beta[(ndim+1):(ndimf-jointmode)]
	rhose <- z0$stder[(ndim+1):(ndimf-jointmode)]
	tmp <- (ndim-nstrata+1):ndim
	if(ndim>nstrata)tmp <- c(tmp,1:(ndim-nstrata))
	beta <- z0$beta[tmp]
	se <- z0$stder[tmp]
	tmp <- c(tmp,(ndim+1):ndimf)
	cov <- cov[tmp,tmp,drop=FALSE]}
else {
	rho <- rhose <- NULL
	beta <- z0$beta[1:ndim]
	se <- z0$stder[1:ndim]}
if(jointmode){
	beta <- c(beta,z0$beta[ndimf])
	se <- c(se,z0$stder[ndimf])}
if(npest==100)moments <- z0$xmom
else moments <- NULL
if(!is.null(xlabelf)){
	if(model=="Weibull")
		xlabel[(length(xlabelc)+1):(ndim-nstrata)] <- paste(xlabel[(length(xlabelc)+1):(ndim-nstrata)],sequence(fact),sep="")
	else xlabel[(length(xlabelc)+1):ndim] <- paste(xlabel[(length(xlabelc)+1):ndim],sequence(fact),sep="")}
if(jointmode)xlabel <- c(xlabel,integrate)
ndim <- ndim+jointmode
tmp <- se[1:ndim]>0
beta <- beta[tmp]
se <- se[tmp]
if(!is.null(xlabel)){
	if(model=="Weibull")xlabel <- xlabel[tmp[(nstrata+1):ndim]]
	else xlabel <- xlabel[tmp]}
ndim <- sum(tmp)
if(model=="Weibull")tmp <- c(tmp,rep(TRUE,nstrata))
if(ndim>0)cov <- cov[tmp,tmp,drop=FALSE]
if(model=="Weibull"){
	df <- c(z0$df[1],ndim+nstrata)
	tmp <- "(Intercept)"
	if(nstrata>1)tmp <- paste(tmp,1:nstrata,sep="")}
else {
	df <- c(z0$df[1],ndim)
	tmp <- NULL}
blabel <- c(tmp,xlabel)
betag <- betan <- seg <- sen <- blabelg <- blabeln <- NULL
ndimg <- ndimn <- 0
if(integam>0&&jointmode){
	tmp <- ndim
	betag <- beta[ndim]
	seg <- se[ndim]
	blabelg <- blabel[ndim]
	ndimg <- 1}
else if(igam[1,1]>0&&integam==0){
	tmp <- (ndim-length(unique(itabl[,colgam]))+1):ndim
	betag <- beta[tmp]
	seg <- se[tmp]
	blabelg <- blabel[tmp]
	ndimg <- length(tmp)}
else if(inor[1,1]>0){
	tmp <- (ndim-length(unique(itabl[,colnor]))+1):ndim
	betan <- beta[tmp]
	sen <- se[tmp]
	blabeln <- blabel[tmp]
	ndimn <- length(tmp)}
if((igam[1,1]>0&&integam==0)||inor[1,1]>0||(integam>0&&jointmode)){
	tmp <- 1:(tmp[1]-1)
	beta <- beta[tmp]
	se <- se[tmp]
	blabel <- blabel[tmp]
	ndim <- ndim-ndimg-ndimn}
z <- list(
	ccov=ccov,
	tvcov=tvcov,
	beta=beta,
	betag=betag,
	betan=betan,
	rho=rho,
	iterations=z0$iconst[3],
	igam=igam,
	xgam=z0$xgam,
	inor=inor,
	xnor=z0$xnor,
	integam=integam,
	moments=moments,
	ndim=ndim,
	ndimg=ndimg,
	ndimn=ndimn,
	ncov=nccov+ndcov+nstrata-inor[1,1]-igam[1,1]*(integam==0),
	nind=nind,
	nstrata=nstrata,
	jointmode=jointmode,
	blabel=blabel,
	blabelg=blabelg,
	blabeln=blabeln,
	xlabelrg=xlabelrg,
	xlabelrn=xlabelrn,
	se=se,
	seg=seg,
	sen=sen,
	rhose=rhose,
	like=z0$like,
	nrr=nrr,
	nevents=sum(censor==1),
	ncensor=sum(censor==0),
	df=df,
	grad=z0$grad,
	cov=cov,
	baseline=base,
	residuals=resid,
	survival=surv,
	call=call,
	code=code)
class(z) <- c("survivalkit",model)
z}

### standard methods
###
residuals.survivalkit <- function(object, ...){
if(inherits(object,"Cox")){
	if(!is.null(object$residuals))print(object$residuals)
	else print("residuals not available: use residuals=TRUE")}
else print("residuals only available for Cox model")}

print.survivalkit <- function(x, ...){
  z <- x ## legacy / S3methods consistency
if(inherits(z,"Kaplan.Meier")){
	cat("\nKaplan-Meier estimates with 95% interval and Nelson estimates\n\n")
	u <- log(z$km[,1])
	r <- sqrt(z$km[,2])/abs(u)
	tab <- cbind(z$km[,1],exp(exp(1.96*r)*u),exp(exp(-1.96*r)*u),z$km[,3])
	tmp <- c(colnames(z$km)[1],"lower","upper",colnames(z$km)[3])
	if(z$nstrata>1){
		tmp <- c(tmp,"stratum")
		strata <- cumsum(z$km[,1]==0)+1
		strata <- c(1,strata[1:(length(strata)-1)])
		tab <- cbind(tab,strata)}
	colnames(tab) <- tmp
	print(tab)
	return(tab)}
else if(inherits(z,"Weibull")){
	mdl <- 1
	cat("\nWeibull model\n\n")}
else {
	mdl <- 2
	cat("\nCox model\n\n")}
cat("Call:\n",deparse(z$call),"\n\n",sep="")
if(z$code%%2>0)warning("algorithm did not converge")
if(z$code>1)warning("no convergence for Gauss-Hermite integration")
if(z$code>0)cat("\n")
if(z$ndimg==0&&z$ndimn==0){
	cat("-Log likelihood (null)",z$like[1],"\n")
	cat("AIC (null)            ",z$like[1]+z$df[1],"\n")}
cat("-Log likelihood       ",z$like[2],"\n")
cat("AIC                   ",z$like[2]+z$df[2],"\n")
cat("Number of parameters  ",z$df[2],"\n")
if(z$nind<z$nevents+z$ncensor)cat("Number of individuals ",z$nind,"\n")
cat("Number of events      ",z$nevents,"\n")
if(z$ncensor>0)cat("Number censored       ",z$ncensor,"\n")
if(z$nstrata>1)cat("Number of strata      ",z$nstrata,"\n")
cat("Number of iterations  ",z$iter,"\n")
if(z$igam[1,1]>0&&!z$jointmode){
	cat("\nLog gamma random variance (mode)\n")
	tmp <- NULL
	for(i in 1:z$igam[1,1])tmp <- c(tmp,z$xgam[z$ncov-z$jointmode+i])
	names(tmp) <- z$xlabelrg
	print.default(tmp, digits=4, print.gap=2)}
if(z$inor[1,1]>0){
	cat("\nNormal random variance (mode)\n")
	tmp <- NULL
	for(i in 1:z$inor[1,1])tmp <- c(tmp,z$xnor[z$ncov+i])
	names(tmp) <- z$xlabelrn
	print.default(tmp, digits=4, print.gap=2)}
if(!is.null(z$moments)){
	tmp <- matrix(z$moments[2:4],nrow=1)
	rownames(tmp) <- "moments"
	colnames(tmp) <- c("mean","sd","skew")
	print.default(tmp, digits=4, print.gap=2)}
if(z$ndim>0){
	cat("\nRegression coefficients\n")
	coef.table <- cbind(z$beta,z$se)
	if(z$ndim==1)coef.table <- matrix(coef.table,ncol=2)
	colnames(coef.table) <- c("estimate","se")
	rownames(coef.table) <- z$blabel
	print.default(coef.table, digits=4, print.gap=2)}
if(z$ndimg>0){
	if(!z$jointmode)cat("\nLog gamma random effects\n")
	else cat("\nLog gamma random variance\n")
	coef.table <- cbind(z$betag,z$seg)
	if(z$ndimg==1)coef.table <- matrix(coef.table,ncol=2)
	colnames(coef.table) <- c("estimate","se")
	rownames(coef.table) <- z$blabelg
	print.default(coef.table, digits=4, print.gap=2)}
else if(z$ndimn>0){
	cat("\nNormal random effects\n")
	coef.table <- cbind(z$betan,z$sen)
	colnames(coef.table) <- c("estimate","se")
	rownames(coef.table) <- z$blabeln
	print.default(coef.table, digits=4, print.gap=2)}
if(inherits(z,"Weibull")){
	cat("\nWeibull power parameter\n")
	coef.table <- cbind(z$rho,z$rhose)
	colnames(coef.table) <- c("estimate","se")
	rownames(coef.table) <- paste(1:z$nstrata)
	print.default(coef.table, digits=4, print.gap=2)}
if((inherits(z,"Weibull")&&z$ndim+z$nstrata>1)||
	(any(class(z)=="Cox")&&z$ndim>1)){
	cat("\nCorrelations:\n")
	tmp <- c(z$se,z$seg,z$sen,z$rhose)
	corr <- z$cov/(tmp%o%tmp)
	if(inherits(z,"Cox"))tmp <- seq(1,z$ndim+z$ndimg+z$ndimn)
	else tmp <- seq(1,z$ndim+z$ndimg+z$ndimn+z$nstrata)
	dimnames(corr) <- list(tmp,tmp)
	print.default(corr, digits=4)}}

### special methods
###
baseline <- function(z, ...) UseMethod("baseline")

baseline.survivalkit <- function(z, ...){
if(inherits(z,"Cox")){
	if(!is.null(z$baseline))print(z$baseline)
	else print("baseline not available: use baseline=TRUE")}
else print("baseline only available for Cox model")}

survival <- function(z, ...) UseMethod("survival")

survival.survivalkit <- function(z, ...){
if(!is.null(z$survival))print(z$survival)
else print("survival not available: use survival option")}
