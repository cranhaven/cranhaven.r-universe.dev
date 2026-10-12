readBetas <- function(betafile,header=TRUE){
	fields <- count.fields(betafile)
	if (all(fields == 4)){
		beta <- read.table(betafile,colClasses=c("character","character","numeric","numeric"),header=header)
		betabar <- beta[,3]*2*beta[,4]
		beta <- data.frame(rsID=as.character(beta[,1]),RiskAllele=as.character(beta[,2]),beta=as.numeric(beta[,3]),Freq=as.numeric(beta[,4]), betaBar=betabar,stringsAsFactors=F)
	} else {
		stop("Error: Unexpected number of fields in beta file")
	}
	class(beta) <- c("MangroveBetas","data.frame")
	return(beta)
}
