# Project: BiclustGUI
# 
# Author: Ewoud
###############################################################################


#### THE PROVIDED EXPORT SCRIPTS FOR FURBY ###
export.Biclust = function(biClust, x, baseName, clusterAssignments = NULL, clusterNames = NULL) {
	#x = record x dimension
	l = biClust@RowxNumber * 1    #record x bicluster
	z = t(biClust@NumberxCol * 1)    #dimension x bicluster
	n = nrow(l)
	p = ncol(l)
	q = ncol(z)
	write.table(x,paste0(baseName,'_X.csv'),quote=FALSE,col.names=NA,dec=".",sep="\t")
	write.table(l,paste0(baseName,'_L.csv'),quote=FALSE,col.names=NA,dec=".",sep="\t")
	write.table(z,paste0(baseName,'_Z.csv'),quote=FALSE,col.names=NA,dec=".",sep="\t")
	
	if (!is.null(clusterAssignments)) {
		if (!is.null(clusterNames))
			assignments = as.matrix(sapply(clusterAssignments,function(x) { clusterNames[x] }))
		else
			assignments = as.matrix(clusterAssignments)
		rownames(assignments) = colnames(x)
		colnames(assignments) = paste("Clustering",1:ncol(assignments))
		write.table(assignments,paste0(baseName,'_chemicalClusters.csv'),quote=FALSE,col.names=NA,dec=".",sep="\t")
	}
}


export.fabia.threshL = function(fabiaRes, threshZ = 0.5) {
	noL <- as.matrix(L(fabiaRes))
	nZ <- as.matrix(Z(fabiaRes))
	X <- as.matrix(X(fabiaRes))
	n <- nrow(noL)
	p <- ncol(noL)
	l <- ncol(nZ)
	
	mom <- 0
	for (i in 1:p) {
		mom <- mom + sum(noL[, i]^2) * sum(nZ[i, ]^2)
	}
	mom <- mom/(as.double(n) * as.double(l) * as.double(p))
	threshL <- sqrt(mom)/threshZ
	print(threshL)
	threshL
}

export.fabia = function(fabiaRes, baseName, clusterAssignments = NULL, clusterNames = NULL, threshZ = 0.5, threshL = NULL) {
	x = X(fabiaRes)    #gene x sample
	l = L(fabiaRes)    #gene x bicluster
	z = t(Z(fabiaRes)) #sample x bicluster
	n = nrow(l)
	p = ncol(l)
	q <- ncol(z)
	write.table(x,paste0(baseName,'_X.csv'),quote=FALSE,col.names=NA,dec=".",sep="\t")
	write.table(l,paste0(baseName,'_L.csv'),quote=FALSE,col.names=NA,dec=".",sep="\t")
	write.table(z,paste0(baseName,'_Z.csv'),quote=FALSE,col.names=NA,dec=".",sep="\t")
	#create a threshold table in the form 
	#rows: bicluster ids
	#col 1 named L : L thresholds
	#col 2 named Z : Z thresholds
	threshZ = rep(threshZ,length.out=p)
	if (is.null(threshL)) {
		threshL = export.fabia.threshL(fabiaRes,threshZ)
	}
	threshL = rep(threshL,length.out=p)
	
	thresh = cbind(threshZ,threshL)
	rownames(thresh) = colnames(l)
	colnames(thresh) = c('Z','L')
	write.table(thresh,paste0(baseName,'_thresholds.csv'),quote=FALSE,col.names=NA,dec=".",sep="\t")
	
	
	if (!is.null(clusterAssignments)) {
		if (!is.null(clusterNames))
			assignments = as.matrix(sapply(clusterAssignments,function(x) { clusterNames[x] }))
		else
			assignments = as.matrix(clusterAssignments)
		rownames(assignments) = colnames(x)
		colnames(assignments) = paste("Chemical Clustering",1:ncol(assignments))
		write.table(assignments,paste0(baseName,'_chemicalClusters.csv'),quote=FALSE,col.names=NA,dec=".",sep="\t")
	}
}



