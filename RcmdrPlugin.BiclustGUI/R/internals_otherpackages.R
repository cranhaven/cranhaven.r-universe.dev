# Project: BiclustGUI
# 
# Author: lucp8394
###############################################################################



BCDIAG_isa2biclust <- function(x){
	Parameters <- list(seeddata=x$seeddata,rundata=x$rundata)
	RowxNumber <- (x$rows != 0)
	NumberxCol <- t(x$columns != 0)
	Number <- ncol(x$rows)
	
	out <- new("Biclust", Parameters=Parameters, RowxNumber=RowxNumber, NumberxCol=NumberxCol, Number=Number)
	return(out)
}

indexedBic<-function(dset,bres,mname=c("fabia","isa2","biclust","bicare"),bnum,fabia.thresZ=0.5,fabia.thresL=NULL){
	# which biclust object is it; 
	check<-match.arg(mname)
	l<-bnum
	if(check=="fabia"){
		#Extract biclusters:
		#get the biclust index inside the dset 
		resf <- extractBic(bres,thresZ=fabia.thresZ,thresL=fabia.thresL)
		bg<-resf$numn[l,]$numng
		bc<-resf$numn[l,]$numnp
		# the two indecies	
		indg<-bg
		indc<-bc
	}
	if(check=="isa2"){
		#convert to biclust and get the biclust indecies
		resi<-BCDIAG_isa2biclust(bres)
		indg<-which(resi@RowxNumber[,l])
		indc<-which(resi@NumberxCol[l,])
		
	}
	if(check=="biclust"){
		indg<-which(bres@RowxNumber[,l])
		indc<-which(bres@NumberxCol[l,])
		
	}
	if(check=="bicare"){
		x <- bres
		Parameters <- list(numberofbicluster=x$param[1,2],residuthreshold=x$param[2,2],genesinitialprobability=x$param[3,2],samplesinitialprobability=x$param[4,2],numberofiterations=x$param[5,2],date=x$param[6,2])
		RowxNumber <- t(x$bicRow==1)
		NumberxCol <- x$bicCol==1
		Number <- as.numeric(dim(RowxNumber)[2])
		info <- list()
		resbi <- new("Biclust",Parameters=Parameters,RowxNumber=RowxNumber,NumberxCol=NumberxCol,Number=Number,info=info)
		indg<-which(resbi@RowxNumber[,l])
		indc<-which(resbi@NumberxCol[l,])
	}
	
	# Warning for bicluster with 1 row and 1 column
	if(length(indg)==1 & length(indc)==1){stop("Bicluster only has 1 row and 1 column",call.=FALSE)}
	
	return(list(indg,indc))
}