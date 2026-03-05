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
#	cprocess(times=NULL, events=NULL, number=FALSE, lty=NULL,
#		xlim=NULL, ylim=NULL, xlab="Time", ylab="Counts", ...)
#
#  DESCRIPTION
#
#    Plots counting process data

cprocess <- function(times=NULL, events=NULL, number=FALSE, lty=NULL,
	xlim=NULL, ylim=NULL, xlab="Time", ylab="Counts", ...){
if(is.null(times)&&is.null(events))
	stop("Either times or events must be supplied")
#
# if events are supplied, create the times
#
if(!is.null(events)){
	if(is.matrix(events)){
		if(is.null(times))tim <- list()
		ev <- list()
		for(i in 1:dim(events)[1]){
			if(is.null(times))
				tim <- c(tim,list(rep(1,dim(events)[2])))
			ev <- c(ev,list(events[i,]))}
		times <- tim
		rm(tim)
		events <- ev
		rm(ev)}
	else if(is.vector(events,mode="numeric")){
		if(is.null(times))times <- list(rep(1,length(events)))
		events <- list(events)}
	else if(inherits(events,"response")){
		tmp <- list()
		for(i in 1:length(nobs(events)))
			tmp <- c(tmp,list(c(events$times[covind(events)==i][1],diff(events$times[covind(events)==i]))))
		times <- tmp
		tmp <- list()
		for(i in 1:length(nobs(events)))
			tmp <- c(tmp,list(events$y[covind(times)==i]))
		events <- tmp
		rm(tmp)}
	else if(inherits(events,"repeated")){
		tmp <- list()
		for(i in 1:length(nobs(events)))
			tmp <- c(tmp,list(c(events$response$times[covind(events)==i][1],diff(events$response$times[covind(events)==i]))))
		times <- tmp
		tmp <- list()
		for(i in 1:length(nobs(events)))
			tmp <- c(tmp,list(events$response$y[covind(events)==i]))
		events <- tmp
		rm(tmp)}
	else if(is.list(events)){
		if(is.null(times))for(i in events)
			times <- c(times,list(rep(1,length(i))))}
	else stop("events must be a matrix, vector, or list of vectors")}
#
# if times are supplied, transform them to a list
#
if(!is.list(times)){
	if(!is.vector(times,mode="numeric"))
		stop("times must be a vector or a list of vectors")
	times <- list(times)
	if(!is.null(events)){
		if(length(times)!=length(events))
			stop("numbers of individuals for events and times differ")
		for(i in 1:length(times))
		if(length(events[[i]])!=length(times[[i]]))
			stop(paste("individual ",i,"does not have the same number of events as times"))}}
else if(inherits(times,"response")){
	tmp <- list()
	for(i in 1:length(nobs(times)))
		tmp <- c(tmp,list(times$y[covind(times)==i]))
	times <- tmp
	rm(tmp)}
k <- xl <- yl <- 0
for(i in times){
	k <- k+1
	if(is.matrix(i))i <- i[,1]
	else if(!is.vector(i,mode="numeric"))
		stop("times must be a list of vectors or matrices")
	if(any(i<0))stop("negative times")
	jx <- length(i)
	if(is.null(events))jy <- jx
	else jy <- sum(events[[k]])
	tmp <- cumsum(i)
	yl <- max(jy,yl)
	xl <- max(tmp[jx],xl)}
if(missing(xlim))xlim <- c(0,xl)
if(missing(ylim))ylim <- c(0,yl)
#
# plot the event histories
#
z <- list()
k <- j <- 0
for(i in times){
	j <- j+1
	if(is.null(events))s <- 1:length(i)
	else s <- cumsum(events[[j]])
	time <- cumsum(i)
	# double the values to create steps
	n <- 2*length(s)-1
	count <- rep(0,n)
	count[seq(1,n,by=2)] <- s
	count[seq(2,n-1,by=2)] <- s[1:(length(s)-1)]
	count <- c(0,0,count)
	tmp <- rep(0,n)
	tmp[seq(1,n,by=2)] <- time
	tmp[seq(2,n-1,by=2)] <- time[1:(length(s)-1)]
	tmp <- c(0,tmp,tmp[length(tmp)])
	if(missing(lty))llty <- k%%4+1
	else llty <- lty
	if(k)lines(tmp,count,lty=llty)
	else plot(tmp,count,type="l",lty=llty,xlim=xlim,ylim=ylim,
		xlab=xlab,ylab=ylab,...)
	if(number)text(tmp[length(tmp)],count[length(count)]+1,paste(k+1))
	k <- k+1
	z <- c(z,list(cbind(tmp,count)))}
invisible(z)}
