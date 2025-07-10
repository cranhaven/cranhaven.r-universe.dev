# Project: BiclustGUI
# 
# Author: Gebruiker
###############################################################################


# Using the correct transformation. (e.g. this is used in BcDiag,... ) (Note: string as input)
.tobiclust_transf <- function(methodresult,thresZ="0.5",thresL="NULL"){

	eval(parse(text=paste("x <- ",methodresult,sep="")))
	
	if(class(x)=="Biclust"){return(methodresult)}
	if(class(x)=="Factorization"){return(paste0(".fabia2biclust(",methodresult,",thresZ=",thresZ,",thresL=",thresL,")"))}
	if(class(x)=="iBBiG"){return(methodresult)}
	if(class(x)=="QUBICBiclusterSet"){return(methodresult)}
	if(class(x)=="biclustering"){return(paste0(".bicare2biclust(",methodresult,")"))}
#	if(.isISA(x)){return(paste0(".isa2biclust(",methodresult,")"))}
	
}


# Transforming the result
.tobiclust <- function(x,fabia.thresZ=0.5,fabia.thresL=NULL){
	if(class(x)=="Biclust"){return(x)}
	if(class(x)=="Factorization"){return(.fabia2biclust(x=x,thresZ=fabia.thresZ,thresL=fabia.thresL))}
	if(class(x)=="iBBiG"){return(x)}
	if(class(x)=="QUBICBiclusterSet"){return(x)}
	if(class(x)=="biclustering"){return(.bicare2biclust(x))}
	#if(.isISA(x)){return(isa.biclust(x))}
}



## The Transformation Functions
# CONDITION OF THESE FUNCTIONS: IF INPUT IS ALREADY BICLUST, RETURN IT
.fabia2biclust <- function(x,thresZ=0.5,thresL=NULL){
	
	if(class(x)=="Biclust"){return(x)}  # This is actually for the biclust plots for fabia superbiclust-save has been used on fabia
	else{
		
		fabia.extract <- extractBic(x,thresZ,thresL)
		
#		n.rows <- dim(fabia.extract$X)[1]
#		n.cols <- dim(fabia.extract$X)[2] 
		n.rows <- dim(x@L)[1] # This was for Fabia SPARSE because the X slot is empty... but the loadings and factor scores are from the other dimension for this method...
		n.cols <- dim(x@Z)[2]
		
		RowxNumber <- c()
		NumberxCol <- c()
		
		
		for(i.index in 1:fabia.extract$np){
			
			rows.index <- fabia.extract$numn[i.index,]$numng
			cols.index <- fabia.extract$numn[i.index,]$numnp
			
			temp.rows <- rep(0,n.rows)
			temp.cols <- rep(0,n.cols)
			
			temp.rows[rows.index] <- 1
			temp.cols[cols.index] <- 1
			
			RowxNumber <- cbind(RowxNumber,temp.rows)
			NumberxCol <- rbind(NumberxCol,temp.cols)
			
		}
		
		RowxNumber <- RowxNumber == 1  # 0/1 matrix needs to be converted to a logical matrix
		NumberxCol <- NumberxCol == 1
		
		return(new("Biclust", Number = dim(RowxNumber)[2], RowxNumber = RowxNumber,NumberxCol = NumberxCol,Parameters=list()))
		
	}
	
}

.bicare2biclust <- function(x){
	if(class(x)=="Biclust"){ # In case of superbiclust is used # NOTE2: THIS IS ACTUALLY NOT NECESSARY !!
		return(x)
	} else if(class(x)=="biclustering"){
		Parameters <- list(numberofbicluster=x$param[1,2],residuthreshold=x$param[2,2],genesinitialprobability=x$param[3,2],samplesinitialprobability=x$param[4,2],numberofiterations=x$param[5,2],date=x$param[6,2])
		RowxNumber <- t(x$bicRow==1)
		NumberxCol <- x$bicCol==1
		Number <- as.numeric(dim(RowxNumber)[2])
		info <- list()
		return(new("Biclust",Parameters=Parameters,RowxNumber=RowxNumber,NumberxCol=NumberxCol,Number=Number,info=info))
	}
}

#.isa2biclust <- function(x){
#	if(class(x)=="Biclust"){
#		return(x)
#	}
#	else{
#		Parameters <- list(seeddata=x$seeddata,rundata=x$rundata)
#		RowxNumber <- (x$rows != 0)
#		NumberxCol <- t(x$columns != 0)
#		Number <- ncol(x$rows)
#		
#		out <- new("Biclust", Parameters=Parameters, RowxNumber=RowxNumber, NumberxCol=NumberxCol, Number=Number)
#		return(out)
#	}
#
#}