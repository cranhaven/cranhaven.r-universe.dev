# Project: BiclustGUI
# 
# Author: lucp8394
###############################################################################



HeatmapBC.GUI <- function(data,res,BC=c(),legend=TRUE,reorder=FALSE,background=FALSE,zeroBC=TRUE,
		transf=c("none","bin","disc"),bin.thres=NA,disc.nof=10,disc.quant=FALSE,
		
		BC.highlight=NULL,BC.highlight.opacity=1){
	
	makeTransparent<-function(someColor, opacity=1)
	{
		newColor<-col2rgb(someColor)
		apply(newColor, 2, function(curcoldata){rgb(red=curcoldata[1], green=curcoldata[2],
							blue=curcoldata[3],alpha=opacity*255, maxColorValue=255)})
	}
	
	if(res@Number==0){stop("No biclusters available",call.=FALSE)}
	
	
	
	orderRec <- function(col, resBC) { ## THIS ALGORITHM IS BORROWED FROM THE plot.iBBiG function in the iBBiG package.
		if (col <= ncol(resBC)) {
			currentCovs <- resBC[, col] == 1
			sub1 <- sum(currentCovs) > 0
			if (sub1) {
				subMod <- resBC
				subMod[!currentCovs, ] <- 2
				covOrder <- orderRec(col + 1, subMod)
			}
			rest <- resBC[, col] == 0
			sub2 <- sum(rest) > 0
			if (sub2) {
				subMod <- resBC
				subMod[!rest, ] <- 2
				restOrder <- orderRec(col + 1, subMod)
			}
			if (sub1 && sub2) {
				combOrder <- c(covOrder, restOrder)
			}
			else if (sub1) {
				combOrder <- covOrder
			}
			else {
				combOrder <- restOrder
			}
		}
		else {
			combOrder <- (1:nrow(resBC))[resBC[, 1] != 	2]
		}
		return(combOrder)
	}
	
	### PREP 1
	data <- as.matrix(data)
	
	
	### TRANFORM DATA IF ASKED
	if(transf=="bin"){
		data <- binarize(data,threshold=bin.thres)
	}
	if(transf=="disc"){
		data <- discretize(data,nof=disc.nof,quant=disc.quant)
	}	
	
	### PREP 2
	BIN <- .is.binary.matrix(data)
	
	### DETERMINE BICLUSTERS TO PLOT
	if(length(BC)==0){
		nBC <- c(1:res@Number)
	}
	else{
		nBC <- BC
	}
	
	# Some checks for BC.highlight
	if(!is.null(BC.highlight)){
		if(length(BC.highlight)>1){
			warning("BC.highlight should be a single numeric")
			BC.highlight <- NULL
		}
		if(!(BC.highlight%in%nBC)){
			warning("BC.highlight was not one of the visualised BC's")
			BC.highlight <- NULL
		}
	}
	
	### Look if any biclusters have 0 rows or columns, these will be deleted!
	
	### MAKE START MATRIX + Background color
	
	if(background){
		whiteCol <- "#FFFFFF00"
	}
	else{
		whiteCol <- "#FFFFFF"
	}
	
	
	if(BIN){
		image.mat <- data
		colorBlackWhite = c(whiteCol, "#666666")
	}		
	else{
		image.mat <- matrix(0,nrow=dim(data)[1],ncol=dim(data)[2])
		colorBlackWhite = c(whiteCol)
	}
	
	
	### Fill in color for BC's + DELETING EMPTY BICLUSTERS (0 rows or columns)
	remove.BC <- c()
	
	
	# Change order of BC's if highlight is chosen (the highlight should be plotted on top of all the rest)
	if(!is.null(BC.highlight)){
		nBC <- c(nBC[-BC.highlight],BC.highlight)
	}
	
	for (i in 1:length(nBC)) {
		row.index <- res@RowxNumber[, nBC[i]] == 1
		
		if(res@Number==1){  ## IF THERE IS ONLY 1 BICLUSTERS, YOU CANNOT TAKE ROWS
			col.index <- res@NumberxCol==1
		}
		else{
			col.index <- res@NumberxCol[nBC[i], ] == 1
		}
		
		if(sum(row.index)>0 & sum(col.index)>0){
			if(zeroBC){
				image.mat[row.index, col.index] <- (i + 1)
			}
			else{
				image.mat[row.index, col.index][data[row.index, col.index] != 0] <- (i + 1)
				
			}
		}
		else{
			remove.BC <- c(remove.BC,i)			
			
		}
	}
	
	if(length(remove.BC)>0){nBC <- nBC[-remove.BC]}
	
	# Extra check if highlighted BC was not removed
	if(!is.null(BC.highlight)){
		if(!(BC.highlight%in%nBC)){
			warning("BC.highlight was removed because it was empty.")
			BC.highlight <- NULL
		}
	}	
	
	### Making color vector	 + legend color
	col <- colors()[c(610,565,589,367,22,554,41,35,48,59,64,76,91,96,100,116,143,381,389,394.439,448,471,529,559,568,631,646)]
	while (length(col) < length(nBC)){ col = c(col, col)}
	legendcol = col[1:length(nBC)]
	
	
	
	# When highlighting BC, apply transparancy to all the other colors if asked	
	if(!is.null(BC.highlight)){
		col[-BC.highlight] <- makeTransparent(col[-BC.highlight],opacity=BC.highlight.opacity)
		legendcol=col[1:length(nBC)]
		
	}
	
	
	
	col = c(colorBlackWhite, legendcol)
	
	# For reordering and legend, put the BC's and legendcols in the original order
	if(!is.null(BC.highlight)){
		legendcol <- legendcol[order(nBC)]
		nBC <- sort(nBC)
	}
	
	### Reordering if necessary
	if(reorder & length(nBC)>1){
		colOrder <- orderRec(1, t(res@NumberxCol)[,nBC])
		rowOrder <- orderRec(1, res@RowxNumber[,nBC])
		image.mat <- image.mat[rowOrder, colOrder]
		data <- data[rowOrder,colOrder]
	}
	if(reorder & length(nBC)==1){
		colOrder <- orderRec(1, matrix(t(res@NumberxCol)[,nBC],ncol=1))
		rowOrder <- orderRec(1, matrix(res@RowxNumber[,nBC],ncol=1))
		image.mat <- image.mat[rowOrder, colOrder]
		data <- data[rowOrder,colOrder]
	}
	
	### Making the image plot
	oldpar = par(c("mai", "mar", "mgp", "xpd"))
	#par(mar=c(1,1,1,6),xpd=TRUE,mgp=c(0.1,1,0))
	
	
	## Background data image
	add <- FALSE
	if(background & !BIN){
		if(transf=="disc"){
			backCol <- viridis(511,begin=1,end=0,alpha=0.3) 
		}
		else{
			backCol <- viridis(511,alpha=0.3) 
		}
		image(c(1:dim(data)[2]),c(1:dim(data)[1]),t(data),col=backCol,axes=FALSE,useRaster=TRUE,,xlab="Samples",ylab="Genes")
		add <- TRUE
	}
	
	image(c(1:dim(image.mat)[2]),c(1:dim(image.mat)[1]),t(image.mat),col=col,axes=FALSE,useRaster=TRUE,xlab="Samples",ylab="Genes",add=add)
#	if (length(legendcol) > 0) {legend("topright",inset=c(-0.2,0), legend = as.character(paste("BC", nBC, sep = "")), fill = legendcol, col = legendcol)}
#	if(BIN){legend("bottomright",inset=c(-0.2,0),legend=c("0","1"),fill=colorBlackWhite,col=colorBlackWhite)}
	if(legend){
		if (length(legendcol) > 0) {
			
			legend("topright", legend = as.character(paste("BC", nBC, sep = "")), fill = legendcol, col = legendcol)
			
		}
		if(BIN){legend("bottomright",legend=c("0","1"),fill=colorBlackWhite,col=colorBlackWhite)}	
	}
	#par(oldpar)
	
	
}



#HeatmapBC(data,res,BC=c(),transf="bin",zeroBC=FALSE,reorder=TRUE,background=TRUE)

#pdf("test.pdf")
#HeatmapBC(data,res,BC=c(),transf="none",zeroBC=TRUE,reorder=FALSE)
#dev.off()

#rgb(0,0,0,0) <-transparent OR just put NA's

#heat.colors(10,alpha=0.3)
#test <- sapply(greenred(511),FUN=function(x){return(paste0(x,"4D"))})

#### NOTE:
# Use re-ordering algorithm from iBBiG plot
# Give user option to select biclusters (handy if many biclusters)
# Device a colour scheme
# Build in special case for binary data (namely grey for if it is 1, but not included in a bicluster). Note: use the binary check
# Investigate if painting biclusters over original heatmap is feasible (like option 'keep original heatmap')
# VIGNETTE: Put in explanation that it is not possible to see overlapping biclusters + external graphics device Architect
# + explain use of zeroBC + explain order in which overlaps are plotted (plot only some clusters if you want..)
