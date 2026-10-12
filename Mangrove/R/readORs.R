readORs <-
function(ORfile,header=TRUE){
	fields <- count.fields(ORfile)
	if (all(fields == 4)){
		ORs <- read.table(ORfile,colClasses=c("character","character","numeric","numeric"),header=header)
		meanOR <- ORs[,3]*ORs[,3]*(ORs[,4]^2) + ORs[,3]*2*ORs[,4]*(1 - ORs[,4]) + (1 - ORs[,4])^2
		ORs <- data.frame(rsID=as.character(ORs[,1]),RiskAllele=as.character(ORs[,2]),ORhet=as.numeric(ORs[,3]),ORhom=as.numeric(ORs[,3]^2),Freq=as.numeric(ORs[,4]),meanOR=as.numeric(meanOR), stringsAsFactors=F)
	} else if (all(fields == 5)){
		ORs <- read.table(ORfile,colClasses=c("character","character","numeric","numeric","numeric"),header=header)
		meanOR <- ORs[,4]*(ORs[,5]^2) + ORs[,3]*2*ORs[,5]*(1 - ORs[,5]) + (1 - ORs[,5])^2
		ORs <- data.frame(rsID=as.character(ORs[,1]),RiskAllele=as.character(ORs[,2]),ORhet=as.numeric(ORs[,3]),ORhom=as.numeric(ORs[,4]),Freq=as.numeric(ORs[,5]),meanOR=as.numeric(meanOR),stringsAsFactors=F)
	} else {
		stop("Error: Unexpected number of fields in OR file")
	}
	class(ORs) <- c("MangroveORs","data.frame")
	return(ORs)
}

