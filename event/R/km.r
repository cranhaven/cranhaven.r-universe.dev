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
#  km(times, censor=1, group=1, freq=1, cdf=FALSE)
#  print.km(z)
#  plot.km(z, surv, times=NULL, group=1, cdf=FALSE, plot=TRUE, add=FALSE,
#	xlim, ylim=c(0,1), main=NULL, xlab="Time",
#	ylab=NULL, lty=NULL, ...)
#  plot.surv(times, survivor, group=1, cdf=FALSE, add=FALSE, xlim=NULL,
#	ylim=c(0,1), main=NULL, xlab="Time", ylab=NULL, lty=NULL, ...)
#  plot.intensity(z, ...)
#  plot.intensity.default(times, censor=1, group=1, cl=1, mix=1, ylim=c(0,1),
#	ylab="p", xlab="Time", main="Empirical Hazard Function(s)")
#  plot.intensity.km(z, add=FALSE, xlab="Time", ylab="Hazard", type="l",
#	lty=NULL, ...)
#  plot.dist.km(z)

#
#  DESCRIPTION
#
#    Functions to compute and plot Kaplan-Meier estimates

### function to compute Kaplan-Meier estimates
###
km <- function(times, censor=1, group=1, freq=1, cdf=FALSE){
#
# list supplied, make vectors
#
cens <- gp <- tt <- NULL
j <- 0
if(is.list(times)){
	for(i in times){
		j <- j+1
		tt <- c(tt,i)
		cens <- c(cens,rep(1,length(i)))
		if(!missing(censor))cens[length(cens)] <- censor[j]
		if(!missing(group))gp <- c(gp,rep(group[j],length(i)))}
	times <- tt
	censor <- cens
	if(!missing(group))group <- gp
	rm(tt,cens,gp)}
#
# check values supplied are appropriate
#
group <- as.numeric(group)
if(!is.vector(times,mode="numeric"))stop("times must be a vector")
else if(any(times<0))stop("negative times")
if(length(group)==1)group <- rep(1,length(times))
if(length(censor)==1)censor <- rep(1,length(times))
if(length(group)!=length(times)||length(times)!=length(censor))
	stop("All vectors must be the same length")
if(any(censor!=0&censor!=1))
	stop("Censor vector must be zeros and ones")
#
# replicate data if grouped
#
if(length(freq)>1&any(freq>1)){
	times <- rep(times,freq)
	censor <- rep(censor,freq)
	group <- rep(group,freq)}
#
# calculate unique times of events
#
ss <- cens <- gg <- ff <- cg <- NULL
for(i in unique(group)){
	n <- sum(group==i)
	o <- order(times[group==i])
	cens2 <- censor[group==i][o]
	times2 <- times[group==i][o]
	f2 <- as.vector(table(list(1-cens2,times2)))
	if(!any(cens2==0)||!any(cens2==1)){
		ff2 <- rep(0,2*length(f2))
		ff2[seq(1,2*length(f2)-1,by=2)] <- f2
		f2 <- ff2}
	l2 <- length(f2)
	g2 <- rep(i,l2)
	c2 <- rep(c(1,0),l2/2)
	s2 <- vector(mode="numeric",length=l2)
	s2[seq(1,l2-1,by=2)] <- unique(times2)
	s2[seq(2,l2,by=2)] <- unique(times2)
	ss <- c(ss,s2)
	cens <- c(cens,c2)
	gg <- c(gg,g2)
	ff <- c(ff,f2)
	cg <- c(cg,f2[l2]==0)}
#
# calculate survivor curve
#
j <- (ff!=0)|(cens==1)
k <- seq(1:length(ff))
n <- sum(j)
t <- cc <- f <- g <- tt <- vector(mode="numeric",length=n)
t <- ss[k*j]
cc <- cens[k*j]
f <- ff[k*j]
g <- gg[k*j]
v <- r <- s <- rep(0,n)
for(i in unique(group)){
	tt[n:1] <- cumsum(f[n:1]*(g[n:1]==i))
	r <- r+tt*(g==i)
	tmp <- cumsum(log(ifelse(tt==f|tt==0,1,(tt-f)/tt))*(g==i)*cc)
	s <- s+exp(ifelse(is.na(tmp),0,tmp))*(g==i)*(tt!=f)
	tmp <- cumsum(f/tt/(tt-f)*(g==i)*cc)
	v <- v+ifelse(is.na(tmp),0,tmp)*(g==i)}
v <- s*s*v
#
# set up matrix to return
#
m <- NULL
for(i in unique(g))m <- c(m,sum(g==i))
j <- seq(1,length(g))
j[cumsum(m)] <- cg*j[cumsum(m)]
z <- cbind(t[j],g[j],r[j],s[j],v[j])
dp <- rep(TRUE,dim(z)[1])
for(i in 2:dim(z)[1])if(all(z[i,]==z[i-1,],na.rm=TRUE))dp[i] <- FALSE
z <- z[dp,]
colnames(z) <- c("Time","Group","At risk","S(t)","Var(S)")
rownames(z) <- paste(rep("",dim(z)[1]))
class(z) <- "km"
attr(z,"cdf") <- cdf
z}

### print method
###
print.km <- function(x, ...){
  z <- x ## legacy / S3methods consistency
attr(z,"class") <- attr(z,"cdf") <- NULL
print.default(z)}

### function to plot survivor curves output from km
###
plot.km <- function(x, add=FALSE, xlim=NULL, ylim=c(0,1), main=NULL, xlab="Time",
	ylab=NULL, lty=NULL, ...){
  z <- x ## legacy / S3methods consistency
surv <- z[,4]
times <- z[,1]
group <- z[,2]
cdf <- attr(z,"cdf")
kms <- ttt <- NULL
#
# plot one curve per group
#
k <- ln <- lt <- 0
for(i in unique(group)){
	if(is.null(lty))lt <- lt%%4+1
	else lt <- lty[k <- k+1]
	if(length(group)>1&&length(unique(group))>1){
		j <- (cumsum(group==i)+ln)*(group==i)
		s <- surv[j]
		if(!missing(times))t <- times[j]
		ln <- ln+sum(group==i)}
	else {
		s <- surv
		t <- times}
	n <- 2*length(s)-1
	# prepare survivor function
	km <- rep(0,n)
	km[seq(1,n,by=2)] <- s
	km[seq(2,n-1,by=2)] <- s[1:(length(s)-1)]
	km <- c(1,1,km)
	if(cdf){
		km <- 1-km
		if(is.null(ylab))ylab <- "Failure probability"
		if(missing(main))main <- "Kaplan-Meier cumulative probability curve"}
	else {
		if(is.null(ylab))ylab <- "Survival probability"
		if(missing(main))main <- "Kaplan-Meier survivor curve"}
	# prepare times
	tt <- rep(0,n)
	tt[seq(1,n,by=2)] <- t
	tt[seq(2,n-1,by=2)] <- t[1:(length(s)-1)]
	tt <- c(0,tt,tt[length(tt)])
	# plot
	if(add) lines(tt, km, lty=lt,...)
	else {
		if(is.null(xlim))
			xlim <- c(min(times)-1,max(times+1))
		plot(tt, km, type="l", xlim=xlim, ylim=ylim,
			xlab=xlab, ylab=ylab, main=main,
			lty=lt, ...)}
	add <- TRUE
	ttt <- c(ttt,tt)
	kms <- c(kms,km)}
invisible(cbind(ttt,kms))}

### function to plot survivor curves
###
plot.surv <- function(x=NULL, survivor, group=1, cdf=FALSE, add=FALSE,
	xlim=NULL, ylim=c(0,1), main=NULL, xlab="Time", ylab=NULL, lty=NULL,
	...){
  times <- x ## legacy / S3methods consistency
if(missing(times)||missing(survivor))
	stop("times and survivor curve must be supplied")
#
# plot one curve per group
#
kms <- ttt <- NULL
k <- ln <- lt <- 0
for(i in unique(group)){
	if(is.null(lty))lt <- lt%%4+1
	else lt <- lty[k <- k+1]
	if(length(group)>1&&length(unique(group))>1){
		j <- (cumsum(group==i)+ln)*(group==i)
		s <- survivor[j]
		if(!missing(times))t <- times[j]
		ln <- ln+sum(group==i)}
	else {
		s <- survivor
		t <- times}
	n <- 2*length(s)-1
	# prepare survivor function
	km <- rep(0,n)
	km[seq(1,n,by=2)] <- s
	km[seq(2,n-1,by=2)] <- s[1:(length(s)-1)]
	km <- c(1,1,km)
	if(cdf){
		km <- 1-km
		if(is.null(ylab))ylab <- "Failure probability"
		if(missing(main))main <- "Kaplan-Meier cumulative probability curve"}
	else {
		if(is.null(ylab))ylab <- "Survival probability"
		if(missing(main))main <- "Kaplan-Meier survivor curve"}
	# prepare times
	if(is.null(times)){
		tt <- 1 + (ceiling(1:(n+1)/2) - 1)%%length(s)
		tt <- c(0,tt)}
	else {
		tt <- rep(0,n)
		tt[seq(1,n,by=2)] <- t
		tt[seq(2,n-1,by=2)] <- t[1:(length(s)-1)]
		tt <- c(0,tt,tt[length(tt)])}
	# plot
	if(add)lines(tt, km, lty=lt,...)
	else {
		if(is.null(xlim))
			xlim <- c(min(times)-1,max(times+1))
		plot(tt, km, type="l", xlim=xlim, ylim=ylim,
			xlab=xlab, ylab=ylab, main=main,
			lty=lt, ...)}
	add <- TRUE
	ttt <- c(ttt,tt)
	kms <- c(kms,km)}
invisible(cbind(ttt,kms))}

### functions to plot intensity function
###
plot.intensity <- function(x, ...) UseMethod("plot.intensity")

plot.intensity.default <- function(x, censor=1, group=1, colour=TRUE,
	mix=1, ylim=c(0,1), ylab="p", xlab="Time",
	main="Empirical Hazard Function(s)", ...){
  times <- x ## legacy / S3methods consistency
censor2 <- censor
if(length(group)==1)group <- rep(1,length(times))
if(length(censor)==1)censor <- rep(1,length(times))
group2 <- as.numeric(group)
group <- as.factor(group)
#
# set up times
#
index <- order(times)
tim.gr <- cbind(times[index],censor[index],group2[index])
listim <- vector(mode="list",length=nlevels(group))
for(i in 1:nlevels(group))
	listim[[i]] <- tim.gr[(tim.gr[,3]==i),1:2]
tc <- vector(mode="list",length=nlevels(group))
for(i in 1:nlevels(group))
	tc[[i]] <- listim[[i]][(listim[[i]][,2]==0),1]
tnc <- vector(mode="list",length=nlevels(group))
for(i in 1:nlevels(group))
	tnc[[i]] <- listim[[i]][(listim[[i]][,2]==1),1]
breaks <- seq(0,floor(max(times+1)),1)
tccat <- vector(mode="list",length=nlevels(group))
tnccat <- vector(mode="list",length=nlevels(group))
for(i in 1:nlevels(group)){
	tccat[[i]] <- cut(as.numeric(tc[[i]]),breaks,right=FALSE)
	tnccat[[i]] <- cut(as.numeric(tnc[[i]]),breaks,right=FALSE)}
#
# set up frequencies
#
tncfreq <- matrix(ncol=nlevels(group),nrow=(floor(max(times))+1))
tfreq <- matrix(ncol=nlevels(group),nrow=(floor(max(times))+1))
for(i in 1:nlevels(group))tncfreq[,i] <- table(tnccat[[i]])
if(length(censor2)!=1){
	tcfreq <- matrix(ncol=nlevels(group),nrow=(floor(max(times))+1))
	for(i in 1:nlevels(group)){
		tcfreq[,i] <- table(tccat[[i]])
		tfreq[,i] <- tncfreq[,i]+tcfreq[,i]}}
if(length(censor2)==1)tfreq <- tncfreq
cumfreq <- matrix(ncol=nlevels(group),nrow=(floor(max(times))+1))
cumfreq2 <- matrix(ncol=nlevels(group),nrow=(floor(max(times))+1))
risk <- matrix(ncol=nlevels(group),nrow=(floor(max(times))+1))
for(i in 1:nlevels(group)){
	cumfreq[,i] <- cumsum(tfreq[,i])
	cumfreq2[,i] <- c(0,cumfreq[1:floor(max(times)),i])
	risk[,i] <- length(listim[[i]][,1])-cumfreq2[,i]}
#
# calcuate intensities
#
risk <- risk*mix
haz <- tncfreq/risk
time <- 0:floor(max(times))
haz2 <- vector(mode="list",length=nlevels(group))
hm <- vector(mode="list",length=nlevels(group))
for(i in 1:nlevels(group)){
	haz2[[i]] <- cbind(haz[,i],time)
	hm[[i]] <- haz2[[i]][(haz2[[i]][,1]!=0),]}
#
# plot curves
#
if(colour){
	plot(hm[[1]][,2],hm[[1]][,1],ylim=ylim,col=1,xlab=xlab,
		ylab=ylab,main=main,type="l")
	if(nlevels(group)!=1){
		for(i in 2:nlevels(group)){
			lines(hm[[i]][,2],hm[[i]][,1],col=i)}}}
else {
	plot(hm[[1]][,2],hm[[1]][,1],ylim=ylim,col=gray(0),
		xlab=xlab,ylab=ylab,main=main,type="l")
	if(nlevels(group)!=1){
		for(i in 2:nlevels(group)){
			lines(hm[[i]][,2],hm[[i]][,1],
				col=gray(i/(2*nlevels(group))))}}}}

### function to plot intensity function from km
###
plot.intensity.km <- function(x, add=FALSE, xlab="Time", ylab="Hazard",
	type="l", lty=NULL, ...){
  z <- x ## legacy / S3methods consistency
hazt <- NULL
group <- unique(z[,2])
if(!is.null(lty)&&length(lty)!=length(group))
	stop("lty must have one value per group")
#
# plot one curve per group
#
k <- lt <- 0
for(i in 1:length(group)){
	if(is.null(lty))lt <- lt%%4+1
	else lt <- lty[k <- k+1]
	# calculate intensities and times
	z1t <- z[z[,2]==group[i],1]
	z4t <- z[z[,2]==group[i],4]
	nt <- length(z1t)
	haz <- 2*(z4t[1:(nt-1)]-z4t[2:nt])*(z1t[2:nt]-z1t[1:(nt-1)])/
		(z4t[1:(nt-1)]+z4t[2:nt])
	# plot
	if(add)	lines(z1t[1:(nt-1)],haz,lty=lt)
	else plot(z1t[1:(nt-1)],haz,type=type,lty=lt,xlab=xlab,
		ylab=ylab,...)
	add <- TRUE
	hazt <- c(hazt,haz)}
invisible(cbind(z[1:(dim(z)[1]-length(group)),1],hazt))}

### function to plot transforms of survivor function to determine what
### distribution it might have
###
plot.dist <- function(x, ...) UseMethod("plot.dist")

plot.dist.km <- function(x, ...){
  z <- x ## legacy / S3methods consistency
  #
# set up parameters
#
oldpar <- par(mfrow=c(3,3),mar=c(5,4,4,2),font.main=1)
group <- unique(z[,2])
mn <- min(z[,4],na.rm=TRUE)
if(mn<=0)mn <- 0.01
mx <- max(z[,4],na.rm=TRUE)
if(mx>=1)mx <- 0.999
#
# exponential distribution
#
plot(z[z[,2]==group[1],1],log(z[z[,2]==group[1],4]),
	main="log[S(t)] vs t",ylab="",type="l",ylim=c(log(mn),log(mx)),
	xlab="Linear through origin if Exponential Distribution")
if(length(group)>1)for(i in 2:length(group))
	lines(z[z[,2]==group[i],1],log(z[z[,2]==group[i],4]),
	lty=(i-1)%%4+1)
#
# Pareto distribution
#
plot(log(z[z[,2]==group[1],1]),log(z[z[,2]==group[1],4]),
	main="log S(t) vs log(t)",type="l",ylim=c(log(mn),log(mx)),
	xlab="Linear if Pareto Distribution",ylab="")
if(length(group)>1)for(i in 2:length(group))
	lines(log(z[z[,2]==group[i],1]),log(z[z[,2]==group[i],4]),
	lty=(i-1)%%4+1)
#
# extreme value distribution
#
plot(z[z[,2]==group[1],1],log(-log(z[z[,2]==group[1],4])),
	main="log{-log[S(t)]} vs t",ylab="",type="l",
	ylim=c(log(-log(mx)),log(-log(mn))),
	xlab="Linear if Extreme Value Distribution")
if(length(group)>1)for(i in 2:length(group))
	lines(z[z[,2]==group[i],1],log(-log(z[z[,2]==group[i],4])),
	lty=(i-1)%%4+1)
#
# Weibull distribution
#
plot(log(z[z[,2]==group[1],1]),log(-log(z[z[,2]==group[1],4])),
	main="log{-log[S(t)]} vs log(t)",type="l",
	ylim=c(log(-log(mx)),log(-log(mn))),
	ylab="",xlab="Linear if Weibull Distribution")
if(length(group)>1)for(i in 2:length(group))
	lines(log(z[z[,2]==group[i],1]),log(-log(z[z[,2]==group[i],4]))
	,lty=(i-1)%%4+1)
#
# normal distribution
#
plot(z[z[,2]==group[1],1],qnorm(1-z[z[,2]==group[1],4]),
	main="qnorm[1-S(t)] vs t",ylab="",type="l",
	ylim=c(qnorm(1-mx),qnorm(1-mn)),
	xlab="Linear if Normal Distribution")
if(length(group)>1)for(i in 2:length(group))
	lines(z[z[,2]==group[i],1],qnorm(1-z[z[,2]==group[i],4]),
	lty=(i-1)%%4+1)
#
# log normal distribution
#
plot(log(z[z[,2]==group[1],1]),qnorm(1-z[z[,2]==group[1],4]),
	main="qnorm[1-S(t)] vs log(t)",ylab="",type="l",
	ylim=c(qnorm(1-mx),qnorm(1-mn)),
	xlab="Linear if Log Normal Distribution")
if(length(group)>1)for(i in 2:length(group))
	lines(log(z[z[,2]==group[i],1]),qnorm(1-z[z[,2]==group[i],4]),
	lty=(i-1)%%4+1)
#
# gamma distribution
#
plot(sqrt(z[z[,2]==group[1],1]),qnorm(1-z[z[,2]==group[1],4]),
	main="qnorm[1-S(t)] vs sqrt(t)",type="l",
	ylim=c(qnorm(1-mx),qnorm(1-mn)),
	ylab="",xlab="Linear if Gamma Distribution")
if(length(group)>1)for(i in 2:length(group))
	lines(sqrt(z[z[,2]==group[i],1]),qnorm(1-z[z[,2]==group[i],4]),
	lty=(i-1)%%4+1)
#
# logistic distribution
#
plot(z[z[,2]==group[1],1],log((1-z[z[,2]==group[1],4])/
	z[z[,2]==group[1],4]),main="log{[1-S(t)]/S(t)} vs t",
	ylim=c(log((1-mx)/mx),log((1-mn)/mn)),
	ylab="",xlab="Linear if Logistic Distribution",type="l")
if(length(group)>1)for(i in 2:length(group))
	lines(z[z[,2]==group[i],1],log((1-z[z[,2]==group[i],4])/
	z[z[,2]==group[i],4]),
	lty=(i-1)%%4+1)
#
# log logistic distribution
#
plot(log(z[z[,2]==group[1],1]),log((1-z[z[,2]==group[1],4])/
	z[z[,2]==group[1],4]),ylab="",
	ylim=c(log((1-mx)/mx),log((1-mn)/mn)),
	main="log{[1-S(t)]/S(t)} vs log(t)",
	xlab="Linear if Log Logistic Distribution",type="l")
if(length(group)>1)for(i in 2:length(group))
	lines(log(z[z[,2]==group[i],1]),log((1-z[z[,2]==group[i],4])/
	z[z[,2]==group[i],4]),lty=(i-1)%%4+1)
par(oldpar)}
