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
#     rmaov(response, tvcov=NULL, ccov=NULL, analysis=TRUE)
#
#  DESCRIPTION
#
#    Function to calculate the ANOVA table for a balanced split-plot design
#    Copyright (C) 1999 Ralf Goertz (ralf.goertz@uni-jena.de)



#' Split-plot ANOVA Model
#' 
#' \code{rmaov} performs the classical balanced split-plot ANOVA, with
#' \code{summary} providing the table. This is the so-called repeated measures
#' ANOVA.
#' 
#' For unbalanced data, \code{\link[growth]{elliptic}} will perform the
#' analysis for one or two levels of nesting.
#' 
#' 
#' @param response A matrix or dataframe of response values with units as rows
#' and repeated measures as columns.
#' @param tvcov A numeric vector or factor variable defining the clusters. If
#' there are several levels of nesting, a matrix or dataframe with columns of
#' such variables defining the nested clusters starting with the highest level
#' (that is, from slowest to fastest varying). If not provided, each response
#' value of a unit is assumed to belong to a different cluster (that is, one
#' factor with \code{ncol(response)} levels is assumed).
#' @param ccov A vector or factor variable for one inter-subject covariate or a
#' matrix, dataframe, or list of several such variables.
#' @param analysis If FALSE, the design matrix is set up, but the analysis is
#' not performed.
#' @return The fitted model is returned.
#' @author Ralf Goertz (ralf.goertz@@uni-jena.de)
#' @seealso \code{\link[growth]{carma}}, \code{\link[growth]{elliptic}},
#' \code{\link{lm}}, \code{\link[growth]{potthoff}}.
#' @keywords models
#' @examples
#' 
#' # vision data for 7 individuals, with response a 7x8 matrix
#' # two levels of nesting: 4 levels of power for each eye
#' y <- matrix(rnorm(56),ncol=8)
#' tvc <- data.frame(eye=c(rep(1,4),rep(2,4)),power=c(1:4,1:4))
#' summary(rmaov(y, tvc))
#' 
#' @export rmaov
rmaov <- function(response, tvcov=NULL, ccov=NULL, analysis=TRUE){
	if(!is.matrix(response)||!is.numeric(response))stop("response must be a matrix")
	if(is.null(tvcov)){
		nfac <- 1
		nrep <- nlev <- dim(response)[2]
		rfacname <- "repfac"}
	else if(is.vector(tvcov,mode="numeric")||is.factor(tvcov)){
		nfac <- 1
		nrep <- nlev <- length(unique(tvcov))
		rfacname <- paste(deparse(substitute(tvcov)))
		tvcov <- as.data.frame(tvcov)}
	else if(is.data.frame(tvcov)||is.matrix(tvcov)){
		nfac <- dim(tvcov)[2]
		if(!is.null(colnames(tvcov)))rfacname <- colnames(tvcov)
		else rfacname <- paste("repfac",1:dim(tvcov)[2],sep="")
		nlev <- NULL
		nrep <- 1
		for(i in 1:dim(tvcov)[2]){
			nlev <- c(nlev,length(unique(tvcov[,i])))
			nrep <- nrep*nlev[i]}}
	else stop("If specified, tvcov must be a vector, matrix, or dataframe containing the repeated factors")
	if(is.vector(ccov,mode="numeric")||is.factor(ccov)){
		tmp <- paste(deparse(substitute(ccov)))
		ccov <- list(ccov)
		names(ccov) <- tmp}
	else if(is.matrix(ccov))ccov <- data.frame(ccov)
	else if(is.list(ccov)&&!is.data.frame(ccov))for(i in 1:length(ccov))
		if(is.matrix(ccov[[i]]))ccov[[i]] <- as.vector(t(ccov[[i]]))
	else if(!is.null(ccov))print("ccov must be a vector, matrix, dataframe, or list of time-constant covariates")
	ncases <- dim(response)[2]*dim(response)[1]
	if(nrep!=dim(response)[2])
		stop("Number of columns in response must match product of levels")
	block <- gl(dim(response)[1],nrep)
	nc <- n <- dim(response)[2]
	response <- as.vector(t(response))
	for(i in 1:nfac){
		n <- n/nlev[i]
		com <- paste("fm",as.character(i),"<-gl(nlev[i],n,ncases",sep="")
		if(!is.null(levels(tvcov[,i])))
			com <- paste(com,",labels=levels(tvcov[,i])",sep="")
		com <- paste(com,")\n",sep="")
		eval(parse(text=com))}
	if(!is.null(ccov)){
		tccov <- ccov
		if(length(tccov[[1]])!=ncases)for(i in 1:length(tccov)){
			if(!is.factor(tccov[[i]]))
				tccov[[i]] <- as.factor(tccov[[i]])
			ccov[[i]] <- gl(nlevels(tccov[[i]]),1,ncases,labels=levels(tccov[[i]]))
			ccov[[i]][(rep(1:length(tccov[[i]]),rep(nc,length(tccov[[i]])))-1)*nc+rep(1:nc,length(tccov[[i]]))] <- tccov[[i]][rep(1:length(tccov[[i]]),rep(nc,length(tccov[[i]])))]}}
b <- NULL
		com <- paste("b<-data.frame(response=response,block=block")
	for(i in 1:nfac)com <- paste(com,",",rfacname[i],"=fm",as.character(i),sep="")
	if(!is.null(ccov)){
		if(is.null(names(ccov)))
			names(ccov) <- paste("ccov",1:length(ccov),sep="")
		for(i in 1:length(ccov))
			com <- paste(com,",",names(ccov)[i],"=ccov$",names(ccov)[i],sep="")}
	com <- paste(com,")\n",sep="")
	eval(parse(text=com))
	if(!analysis)return(b)
	efacterm <- facterm <- paste(rfacname,collapse="*")
	if(!is.null(ccov))
		for(i in 1:length(ccov))
			facterm <- paste(facterm,names(ccov)[i],sep="*")
	res <- NULL
	com <- paste("res<-aov(response~",facterm,"+Error(block/(",efacterm,")),data=b)\n",sep="")
	eval(parse(text=com))
	res}


