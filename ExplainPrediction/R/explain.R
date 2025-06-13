# The code for generation of individual instance explanations and model explanations.
# Methods: EXPLAIN, IME
# 
# Author: rmarko
###############################################################################


#explain<- function(object, instances, ...) UseMethod("explanation", object)


# return explanation for given classifier, instance and explanation mode using method EXPLAIN
explain <- function(model, testData, explainInfo, naMode=c("avg", "na"), explainType=c("WE","infGain","predDiff"), 
		classValue=1, nLaplace=nrow(testData)) 
{
	if (! inherits(model, "CoreModel"))
		stop("Model shall be of class CoreModel.")
	isRegression <- model$noClasses == 0
	noClasses <- model$noClasses
	noInst <- nrow(testData)
	if (! isRegression) {
		class.lev <- model$class.lev;
		noClasses <- model$noClasses
		if (is.numeric(classValue)) {
			selClass <- factor(class.lev[classValue],levels=class.lev)
			classIdx <- classValue
		}
		else if (is.factor(classValue)) {
			selClass <- classValue
			classIdx <- as.integer(classValue)
			0}
		else if (is.character(classValue)) {
			selClass <- factor(classValue,levels=class.lev)
			if (is.na(selClass))
				stop("Invalid classValue parameter supplied, shall be compatible with the model: ", classValue)
			classIdx <- as.integer(selClass)
		}
		else 
			stop("Wrong type of classValue parameter supplied: ", classValue)
	}
	naMode <- match.arg(naMode)
	explainType <- match.arg(explainType)
	if (isRegression)
		explainType <- "predDiff"
	
	newFormula <- reformulate(all.vars(model$formula)[-1])
	dat <- model.frame(newFormula, data=testData, na.action=na.pass);
	discDat <- applyDiscretization(dat, explainInfo$discretization)
	noAttr <- ncol(dat)
	
	# get prediction and predicted class
	pred <- getPredictions(model, dat, nLaplace, noClasses, classIdx)
	
	# prepare data structures for output
	expl <- matrix(data=0, nrow=noInst, ncol=noAttr)
	pCXnA <- matrix(data=0, nrow=noInst, ncol=noAttr)
	colnames(expl) <- colnames(pCXnA) <- names(dat)
	#avIdxs <- matrix(data=0, nrow=noInst, ncol=noAttr)
	#names(avIdxs) <- names(dat)
	for (a in 1:noAttr) {           
		attrName <- names(dat)[a]
		origValue <- dat[,a]
		if (naMode=="na") {
			if (is.factor(dat[1,a]))
				dat[,a] <- factor(NA, levels=origValue[1])
			else 
				dat[,a] <- NA
			pCXnA[,a] <- getPredictions(model, dat, nLaplace, noClasses, classIdx)
		}
		else if (naMode=="avg") {
			for (v in 1:length(explainInfo$"discPoints"[[attrName]])){
				dat[,a] <- explainInfo$"discPoints"[[attrName]][v]
				nApredAV <- getPredictions(model, dat, nLaplace, noClasses, classIdx)
				pCXnA[,a] <- pCXnA[,a] + explainInfo$"pAV"[[attrName]][v] * nApredAV
			}
		}
		if (!isRegression)
			pCXnA[,a] <- correctLaplace(pCXnA[,a], nLaplace, noClasses)
		expl[,a] <- explainWithMethod(pred, pCXnA[,a], explainType)
		dat[,a] <- origValue
	}
	list(expl=expl, pCXA=pred, pCxnA=pCXnA)
}

getPredictions<-function(model, instances, nLaplace=1, noClasses=2,classIdx=1) {
	
	if (model$model!="svm")
		pred <- predict(model, instances)
	else 
		pred <- predict(model, instances, probability = TRUE)
	
	if (model$noClasses == 0) {
		pCX <- pred
	}
	else {
		if (is.matrix(pred)) # nnet
			pCX <- pred[,classIdx]
		else if (model$model != "svm")
			pCX <- pred$probabilities[,classIdx]	 # CoreModel
		else if (!is.null(attr(pred, "probabilities"))) # SVM from e1071
			pCX <- attr(pred, "probabilities")[,classIdx]		
		else stop("Method predict returned unexpected object for model ", model$model)
		
		pCX <- correctLaplace(pCX, nLaplace, noClasses)
	}
	return(pCX)
}

explainWithMethod<-function(pCXA, pCXnA, explainType=c("WE","infGain","predDiff")){
	match.arg(explainType)
	noInst = length(pCXA)
	expl = vector(length=noInst)
	if (explainType=="WE") {
		for (i in 1:noInst) {
			## we should not get exactly 0, but if we do...
			if (pCXA[i]==0.0 || pCXA[i]==1.0 || pCXnA[i]==0.0 || pCXnA[i]==1.0) {
				if (pCXA[i]==pCXnA[i]) 
					expl[i] = 0.0 
				else {
					if (pCXA[i]>pCXnA[i]){
						expl[i] = 1.0
					}
					else {
						expl[i] = -1.0
					}
				}
			}
			else
				expl[i]=log2(pCXA[i]*(1-pCXnA[i])/((1.0-pCXA[i])*pCXnA[i]))
		}	
	}
	else if (explainType=="infGain") {		
		## return information gain for given probabilities
		for (i in 1:noInst) {
			## we should not get exactly 0, but if we do...
			if (pCXA[i]==0.0 || pCXnA[i]==0.0) {
				if (pCXA[i]==pCXnA[i]) 
					expl[i] = 0.0 
				else {
					if (pCXA[i]>pCXnA[i]){
						expl[i] = 1.0
					}
					else {
						expl[i] = -1.0
					}
				}
			}
			else
				expl[i]=-log2(pCXnA[i]/pCXA[i])
		}
	} else if (explainType == "predDiff")
		expl  <- pCXA - pCXnA
	
	return(expl)
}



correctLaplace <- function(pC, n, nC) {
	## return Laplace correction of given probabilities  
	if (n > 0) 
		pC = (pC*n + 1.0)/(n+nC)
	else
		return(pC)
}

prepareForExplanations <- function(model, trainData, method=c("EXPLAIN", "IME"), estimator=NULL, 
		genType=c("rf","rbf","indAttr"), noAvgBins=20) {
	
	explainInfo <- list(discretization=list(), avNames=list())
	
	method <- match.arg(method)
	
	if (method == "IME") {
		genType <- match.arg(genType)	
		explainInfo$generator <- list()
		
		explainInfo$discretization <- discretize(model$formula, trainData, method = "equalFrequency", equalDiscBins=noAvgBins)
		
		if (genType=="indAttr")
			explainInfo$generator <- indAttrGen(model$formula, trainData, cdfEstimation = "ecdf")
		else if (genType=="rbf")
			explainInfo$generator <- rbfDataGen(model$formula, trainData) 
		else if (genType=="rf")
			explainInfo$generator <- treeEnsemble(model$formula, trainData, minNodeWeight=10, noSelectedAttr=max(2,ceiling(sqrt(ncol(trainData)-1))))
		
		discData <- applyDiscretization(trainData, explainInfo$discretization)
		className <- all.vars(model$formula)[1]
		attrNames <- all.vars(model$formula)[-1]
		# prepare discretization points and names
		for (a in 1:length(attrNames)) {
			aname <- attrNames[a]  
			explainInfo$avNames[[a]] <- levels(discData[[aname]])
		}	
	}
	else if (method=="EXPLAIN") {
		if (is.null(estimator)) 
			if (model$noClasses == 0) # regression
				estimator <- "RReliefFexpRank"
			else
				estimator <- "ReliefFexpRank"
		explainInfo$pAV  <- list()
		explainInfo$discPoints <- list()
		explainInfo$discretization <- discretize(model$formula, trainData, method="greedy", estimator=estimator)
		
		midPoints <- intervalMidPoint(trainData, explainInfo$discretization, midPointMethod="equalFrequency")
		discData <- applyDiscretization(trainData, explainInfo$discretization)
		className <- all.vars(model$formula)[1]
		attrNames <- all.vars(model$formula)[-1]
		
		# prepare discretization points and names
		for (a in 1:length(attrNames)) {
			aname <- attrNames[a]  
			if (is.factor(trainData[[aname]])) {
				ordered <- is.ordered(trainData[[aname]])
				explainInfo$discPoints[[a]] <- factor(levels(trainData[[aname]]), levels=levels(trainData[[aname]]), ordered=ordered)
			}
			else {
				explainInfo$discPoints[[a]] <- midPoints[[aname]]   
			}
			explainInfo$avNames[[a]] <- levels(discData[[aname]])
		}
		names(explainInfo$discPoints) <-names(explainInfo$avNames) <- attrNames
		# prepare pAV, probabilities of attribute values or discretization intervals 
		for (a in 1:length(attrNames)) {
			aname <- attrNames[a]  
			noVal <- table(discData[[aname]], useNA="no")
			explainInfo$pAV[[a]] <- correctLaplace(noVal / sum(noVal), nrow(discData), length(noVal))
			names(explainInfo$pAV[[a]]) <- levels(discData[[aname]])
		}
		names(explainInfo$pAV) <- attrNames
	}
	explainInfo
}

# average the explanations over attribute values and discretization intervals
explanationAverages <- function(model, trainData, method=c("EXPLAIN", "IME"), explainInfo, naMode=c("avg", "na"), 
		explainType=c("WE","infGain","predDiff"),classValue=1, nLaplace=nrow(trainData),
		pError=0.05, err=0.05, batchSize=40, maxIter=1000) 
{ 
	noInst <- nrow(trainData)
	newFormula <- reformulate(all.vars(model$formula)[-1])
	dat <- model.frame(newFormula, data=trainData, na.action=na.pass);
	noAttr <- ncol(dat)
	method <- match.arg(method)
	
	discDat <- applyDiscretization(dat, explainInfo$discretization)
	
	## initialization
	avExplain <- avExplainPos <- avExplainNoPos <- avExplainNeg <- avExplainNoNeg <- avExplainNo <- list()
	aExplain <- vector(mode="numeric",length=noAttr)
	aExplainPos <- vector(mode="numeric",length=noAttr)
	aExplainNeg <- vector(mode="numeric",length=noAttr)
	aExplainNoPos <- vector(mode="numeric",length=noAttr)
	aExplainNoNeg <- vector(mode="numeric",length=noAttr)
	aExplainPosSum <- vector(mode="numeric",length=noAttr)
	aExplainNegSum <- vector(mode="numeric",length=noAttr)
	
	if (method == "EXPLAIN")
		expl = explain(model, dat, explainInfo, naMode, explainType, classValue,nLaplace)$expl
	else if (method == "IME")
		expl <- ime(model, dat, classValue=classValue, imeInfo=explainInfo, 
				pError=pError, err=err, batchSize=batchSize, maxIter=maxIter)$expl
	
	## computing for each value 
	for (a in 1:noAttr) {
		noExpl <- length(explainInfo$avNames[[a]]) 
		avExplain[[a]] <- vector(mode="numeric",length=noExpl)
		avExplainPos[[a]] <- vector(mode="numeric",length=noExpl)
		avExplainNeg[[a]] <- vector(mode="numeric",length=noExpl)
		avExplainNoPos[[a]] <- vector(mode="numeric",length=noExpl)
		avExplainNoNeg[[a]] <- vector(mode="numeric",length=noExpl)
		avExplainNo[[a]] <- vector(mode="numeric",length=noExpl)
		
		posExpl <- expl[,a] >= 0
		agg <-aggregate(data.frame(expl=expl[,a], count=rep(1, times=noInst)), by=list(ddBy=discDat[[a]],signBy=posExpl),sum )
		avExplainPos[[a]][agg[agg[,"signBy"]==TRUE,"ddBy"]] <- agg[agg[,"signBy"]==TRUE,"expl"]
		avExplainNoPos[[a]][agg[agg[,"signBy"]==TRUE,"ddBy"]] <- agg[agg[,"signBy"]==TRUE,"count"]
		avExplainNeg[[a]][agg[agg[,"signBy"]==FALSE,"ddBy"]] <- agg[agg[,"signBy"]==FALSE,"expl"]
		avExplainNoNeg[[a]][agg[agg[,"signBy"]==FALSE,"ddBy"]] <- agg[agg[,"signBy"]==FALSE,"count"]
		
		avExplainNo[[a]] <- avExplainNoPos[[a]] + avExplainNoNeg[[a]]
		avExplain[[a]] <- avExplainPos[[a]] + avExplainNeg[[a]]
		
		aExplainNoPos[a] <- sum(avExplainNoPos[[a]])  
		aExplainPosSum[a] <- sum(avExplainPos[[a]])
		aExplainNoNeg[a] <- sum(avExplainNoNeg[[a]])  
		aExplainNegSum[a] <- sum(avExplainNeg[[a]])
		
		# averages
		avExplainPos[[a]] = avExplainPos[[a]] / avExplainNoPos[[a]]
		avExplainPos[[a]][is.nan(avExplainPos[[a]]) | is.na(avExplainPos[[a]]) ] <- 0
		avExplainNeg[[a]] = avExplainNeg[[a]] / avExplainNoNeg[[a]]
		avExplainNeg[[a]][is.nan(avExplainNeg[[a]]) | is.na(avExplainNeg[[a]])] <- 0
		avExplain[[a]] = (avExplainPos[[a]] + avExplainNeg[[a]]) / avExplainNo[[a]]
		avExplain[[a]][is.nan(avExplain[[a]]) | is.na(avExplain[[a]])] <- 0
	}
	# attribute explanation averages
	aExplain = (aExplainPosSum + aExplainNegSum) / (aExplainNoPos + aExplainNoNeg)
	aExplain[is.nan(aExplain) | is.na(aExplain)] <- 0
	aExplainPos = aExplainPosSum / aExplainNoPos
	aExplainPos[is.nan(aExplainPos) | is.na(aExplainPos)] <- 0
	aExplainNeg = aExplainNegSum / aExplainNoNeg
	aExplainNeg[is.nan(aExplainNeg) | is.na(aExplainNeg)] <- 0
	
	return(list(attrAvg=aExplain, attrPosAvg=aExplainPos, attrNegAvg=aExplainNeg, avAvg=avExplain, avPosAvg=avExplainPos, avNegAvg=avExplainNeg))
}

# generate explanations and vizualizes it 
explainVis<-function(model, trainData, testData,  
		method=c("EXPLAIN", "IME"), classValue=1,
		fileType=c("none","pdf","eps","emf","jpg","png","bmp","tif","tiff"), 
		dirName=getwd(), fileName="explainVis", visLevel=c("both","model","instance"),
		explainType=c("WE","infGain","predDiff"), naMode=c("avg", "na"), nLaplace=nrow(trainData), 
		estimator=NULL,	pError=0.05, err=0.05, batchSize=40, maxIter=1000, 
		genType=c("rf", "rbf", "indAttr"), noAvgBins=20, 
		displayAttributes=NULL, modelVisCompact=FALSE, displayThreshold=0.0, normalizeTo=0, 
		colors=c("navyblue", "darkred", "blue", "red", "lightblue", "orange"), noDecimalsInValueName=2,
		modelTitle=ifelse(model$noClasses==0,"Explaining %R\nmodel: %M", "Explaining %R=%V\nmodel: %M"), 
		modelSubtitle="Method: %E, type: %X", 
		instanceTitle=ifelse(model$noClasses==0, "Explaining %R\ninstance: %I, model: %M", 
				"Explaining %R=%V\ninstance: %I, model: %M"), 
		instanceSubtitle=ifelse(model$noClasses==0, "Method: %E\nf(%I)=%P, true %R=%T", 
				"Method: %E, type: %X\nP(%R=%V)=%P, true %R=%T"), 
		recall=NULL) 
{
	if (! inherits(model, "CoreModel"))
		stop("Model shall be of class CoreModel.")
	if (is.null(trainData))
		stop("Providing training data is obligatory as it enables computation of discretization and data generator.")
	isRegression <- model$noClasses == 0
	noInst <- nrow(testData)
	if (! isRegression) {
		class.lev <- model$class.lev;
		noClasses <- model$noClasses
		if (is.numeric(classValue)) {
			selClass <- factor(class.lev[classValue],levels=class.lev)
			classIdx <- classValue
		}
		else if (is.factor(classValue)) {
			selClass <- classValue
			classIdx <- as.integer(classValue)
		}
		else if (is.character(classValue)) {
			selClass <- factor(classValue,levels=class.lev)
			if (is.na(selClass))
				stop("Invalid classValue parameter supplied, shall be compatible with the model: ", classValue)
			classIdx <- as.integer(selClass)
		}
		else 
			stop("Wrong type of classValue parameter supplied: ", classValue)
	}
	
	visLevel <- match.arg(visLevel)
	method <- match.arg(method)
	naMode <- match.arg(naMode)
	explainType <- match.arg(explainType)
	fileType <- match.arg(fileType)
	if (fileType=="none")
		fileType <- ""
	genType <- match.arg(genType)
	
	className <- all.vars(model$formula)[1]
	modelName <- model$model
	
	if (isRegression) {
		classValueName <- ""
		explainType <- "predDiff"
	}
	else {
		classValueName <- as.character(selClass)
	}
	
	# replace model based patterns in titles
	# Response variable: %R
	# Selected class value for explanation: %V 
	# Model name: %M
	# Explanation method: %E
	# Explanation type: %X
	
	titles <- c(modelTitle, modelSubtitle, instanceTitle, instanceSubtitle)
	titles <- gsub("%R", className, titles, fixed=TRUE)
	titles <- gsub("%V", classValueName, titles, fixed=TRUE)
	titles <- gsub("%M", modelName, titles, fixed=TRUE)
	titles <- gsub("%E", method, titles, fixed=TRUE)
	titles <- gsub("%X", explainType, titles, fixed=TRUE)
	
	## prepare explanations and averages
	if (is.null(recall)) {
		explainInfo <- prepareForExplanations(model, trainData, method=method, estimator=estimator, genType=genType, noAvgBins=noAvgBins)
		explAvg <- explanationAverages(model, trainData, method=method, explainInfo=explainInfo, 
				naMode=naMode, explainType=explainType, classValue=classValue, nLaplace=nLaplace,
				pError=pError, err=err, batchSize=batchSize, maxIter=maxIter)
		expl <- NULL
	}
	else {
		explainInfo <- recall$explainInfo
		explAvg <- recall$explAvg
		expl <- recall$expl
	}
	
	
	# model explanation plot
	if (visLevel %in% c("both","model")) {
		
		noAttr <- length(explainInfo$avNames)
		attrNames <- names(explainInfo$avNames)
		
		if (is.null(displayAttributes)) {
			displayAttributes <- attrNames
			matched <- 1:length(attrNames)
		}
		else { # check if provided names are correct
			matched <- match(displayAttributes, attrNames)
			if (any(is.na(matched)))
				stop("Invalid attribute name(s) in parameter displayAttributes: ", paste(displayAttributes[ is.na(matched) ], collapse=", "))
		} 
		
		preparePlot(fileName=paste(dirName,"/", fileName,"_model.", fileType, sep=""))
		modelAVexplain(titles[1], titles[2], displayAttributes, explainInfo$avNames[matched],  
				explAvg$attrPosAvg[matched], explAvg$attrNegAvg[matched], 
				explAvg$avPosAvg[matched], explAvg$avNegAvg[matched], 
				modelVisCompact=modelVisCompact, displayThreshold=displayThreshold, colors=colors, normalizeTo=normalizeTo)
		if (fileType != "") # plotting to file
			dev.off()
	}
	
	# instance explanation plot
	if (visLevel %in% c("both","instance")) {
		
		testDataDisc <- applyDiscretization(testData, explainInfo$discretization)
		
		if (method=="EXPLAIN") {
			if (is.null(recall))
				expl <- explain(model, testData, explainInfo=explainInfo, naMode=naMode, explainType=explainType, classValue=classValue, nLaplace=nLaplace)
			else 
				expl <- recall$expl
		}
		else if (method=="IME") {
			if (is.null(recall))
				expl <- ime(model, testData, classValue=classValue, imeInfo=explainInfo, pError=pError, err=err, batchSize=batchSize, maxIter=maxIter)
			else
				expl <- recall$expl
		}
		
		noAttr <- ncol(expl$expl)
		attrNames <- colnames(expl$expl)
		
		if (is.null(displayAttributes)) {
			displayAttributes <- attrNames
			matched <- 1:length(attrNames)
		}
		else { # check if provided names are correct
			matched <- match(displayAttributes, attrNames)
			if (any(is.na(matched)))
				stop("Invalid attribute name(s) in parameter displayAttributes: ", paste(displayAttributes[ is.na(matched) ], collapse=", "))
		} 
		
		testDataDiscExpl <- testDataDisc[,attrNames]
		preparePlot(fileName=paste(dirName,"/", fileName,"_inst.", fileType, sep=""))
		avPosAvg <- avNegAvg <- expl$expl
		for (a in 1:noAttr) {
			avPosAvg[,a] <- explAvg$"avPosAvg"[[a]][as.integer(testDataDiscExpl[[a]])]  
			avNegAvg[,a] <- explAvg$"avNegAvg"[[a]][as.integer(testDataDiscExpl[[a]])] 
		}
		if (className %in% names(testData))
			trueClass <- format(testData[[className]], digits=noDecimalsInValueName)
		else
			trueClass<-vector(mode="character",length=noInst)
		
		for (i in 1:noInst) {		
			# replace instance based patterns
			# Instance name: %I
			# Predicted value/probability of the response: %P 
			# True value/class of the response: %T
			
			ititle <- titles[c(3,4)]
			ititle <- gsub("%I", row.names(testData)[i], ititle, fixed=TRUE)
			ititle <- gsub("%P", format(expl$"pCXA"[i], digits=noDecimalsInValueName), ititle, fixed=TRUE)
			ititle <- gsub("%T", trueClass[i], ititle, fixed=TRUE)
			
			explainVisPN(ititle[1], ititle[2], displayAttributes, 
					as.character(format(testData[i,matched],digits=noDecimalsInValueName)), 
					expl$"expl"[i,matched], avPosAvg[i,matched], avNegAvg[i,matched], 
					threshold=displayThreshold, colors=colors, normalizeTo=normalizeTo)
		}
		if (fileType != "")
			dev.off()
	}
	lastExplainVis <- list(expl=expl, explAvg=explAvg, explainInfo=explainInfo)
	# assign(".lastExplainVis", lastExplainVis, envir=globalenv())
	invisible(lastExplainVis)
}

modelAVexplain<-function(titleName, subtitleName, attrNames, attrValues, 
		attrExplainPos,	attrExplainNeg, avExplainPos,avExplainNeg, 
		modelVisCompact=FALSE, displayThreshold=0.0, colors=NULL, normalizeTo=0)
{
	if (is.null(colors)) {
		colAttrPos <- "gray30"  
		colAttrNeg <- "gray30"
		colAVpos <- "gray90"
		colAVneg <- "gray90"
	}
	else {
		colAttrPos <- colors[1] 
		colAttrNeg <- colors[2]
		colAVpos <- colors[5]
		colAVneg <- colors[6]
	}
	boxHeight <- 1.0
	noAttr <- length(attrNames)
	aname <- as.character(attrNames)
	maxChars <- max(nchar(aname))
	allValues <- noAttr
	avname <- list()
	yLabel <- c()
	usedValues <- list()
	
	# average explanations can be NA 
	#normalization of explanations
	if (normalizeTo > 0) {
		if (modelVisCompact){ # normalize to parameter normalizeTo times the sum of absolute values of attribute-s contributions 
			absSum <- sum(abs(attrExplainPos), abs(attrExplainNeg), na.rm = TRUE)
			attrExplainPos <- attrExplainPos / absSum * normalizeTo
			attrExplainNeg <- attrExplainNeg / absSum * normalizeTo
		}
		else { # normalize to parameter normalizeTo times the sum of absolute values of attribute values' contributions 
			absSum <- 0
			for(iA in 1:noAttr) 
				absSum <- sum(absSum, abs(avExplainPos[[iA]]), abs(avExplainNeg[[iA]]), na.rm=TRUE)
			attrExplainPos <- attrExplainPos / absSum * normalizeTo
			attrExplainNeg <- attrExplainNeg / absSum * normalizeTo
			for(iA in 1:noAttr) {
				avExplainPos[[iA]] <- avExplainPos[[iA]] / absSum * normalizeTo
				avExplainNeg[[iA]] <- avExplainNeg[[iA]] / absSum * normalizeTo
			}
		}		
	}
	
	# getting limits and labels
	xLim <- max(abs(attrExplainPos), abs(attrExplainNeg), na.rm = TRUE)
	if (modelVisCompact){
		yLabel <- attrNames	
	}
	else {
		for (a in 1:noAttr) {
			usedValues[[a]] <- (abs(avExplainPos[[a]]) > displayThreshold) | (abs(avExplainNeg[[a]]) > displayThreshold)
			allValues <- allValues + sum(usedValues[[a]])
			avname[[a]] <- as.character(attrValues[[a]][usedValues[[a]]])
			yLabel <- c(yLabel,attrNames[[a]])
			yLabel <- c(yLabel,avname[[a]])
			xLim <- max(xLim, abs(avExplainPos[[a]][usedValues[[a]]]), abs(avExplainNeg[[a]][usedValues[[a]]]))      
			maxChars <- max(maxChars, nchar(avname[[a]]))	
		}  
	}
	x <- c(0, 0)
	y <- c(1, allValues)
	headerSpace <- 0.5
	if (allValues <=5)
		headerSpace <- 1
	par(xpd=NA,mgp=c(2,1,0),mar=c(5,8,4,2))
	plot(x, y, type = "l", xlim = c(-xLim, xLim), ylim = c(1, allValues+headerSpace), xlab = "",
			ylab = "", axes = F)
	atLabel <- atLabelComp(xLim)
	axis(1, at=atLabel,labels=atLabel)
	## left y axis, attribute names
	las = 1 ## horizontal
	cex.axis <- 1
	if (maxChars > 15) {
		cex.axis <- 0.75    
		if (maxChars > 20)
			cex.axis <- 0.6    
	}		
	if (allValues > 20)
		cex.axis <- max(0.4, -0.6/80 * allValues + 1.15) # linearly: 1 at 20, 0.4 at 100
	
	axis(2, at=+0.4*boxHeight+c(1:allValues), labels=yLabel,las=las, cex.axis=cex.axis)
	title(main = titleName, sub=subtitleName)
	# instead of y axis labels
	if (modelVisCompact)
		text(-xLim*1.07,(allValues+1), labels=c("attributes"), adj = c(1, 0))
	else
		text(-xLim*1.07,(allValues+1), labels=c("attributes/values"), adj = c(1, 0))
	
	chExp = 0.6  ## char expansion for boxes
	y = 1
	for(iA in 1:noAttr) {
		if (!modelVisCompact)
			segments(-xLim,y-0.1, xLim,y-0.1,lty="dashed") 
		xDown <- 0
		xUp <- attrExplainPos[[iA]]
		labelTxt =sprintf("%.2f", attrExplainPos[[iA]])
		rect(xDown, y, xUp, y+0.80*boxHeight, col=colAttrPos)
		xDown <- attrExplainNeg[[iA]]
		xUp <- 0
		rect(xDown, y, xUp, y+0.80*boxHeight, col=colAttrNeg)
		y=y+1
		if (!modelVisCompact){
			for (v in seq(1, length.out=length(attrValues[[iA]]))) {
				if (usedValues[[iA]][v]){
					xDown <- 0
					xUp <- avExplainPos[[iA]][[v]]
					rect(xDown, y, xUp, y+0.80*boxHeight, col=colAVpos)
					xDown <- avExplainNeg[[iA]][[v]]
					xUp <- 0
					rect(xDown, y, xUp, y+0.80*boxHeight, col=colAVneg)
					y=y+1
				}
			}
		}
	}
	segments(-xLim,y-0.1, xLim,y-0.1,lty="dashed") 
}


explainVisPN<-function(titleName, subtitleName, attrNames, attrValues, expl, 
		avgAVexplainPos, avgAVexplainNeg, threshold=0, colors=NULL, normalizeTo=0)
{
	if (is.null(colors)) {
		colExplainPos <- "gray60"  
		colExplainNeg <- "gray60"
		colAVpos <- "gray90"
		colAVneg <- "gray90"
	}
	else {
		colExplainPos <- colors[3] 
		colExplainNeg <- colors[4]
		colAVpos <- colors[5]
		colAVneg <- colors[6]
	}
	
	noAttr = length(attrNames)
	absSum <- sum(abs(expl))
	if (is.null(avgAVexplainPos))
		avgAVexplainPos <- rep(0, length(expl))
	if (is.null(avgAVexplainNeg))
		avgAVexplainNeg <- rep(0, length(expl))
	if (any(is.na(avgAVexplainPos)))
		avgAVexplainPos[which(is.na(avgAVexplainPos))] <- 0
	if (any(is.na(avgAVexplainNeg)))
		avgAVexplainNeg[which(is.na(avgAVexplainNeg))] <- 0
	# normalization of values
	if (normalizeTo > 0 && absSum > 0){
		expl <-  expl / absSum * normalizeTo
		avgAVexplainPos <-  avgAVexplainPos / absSum * normalizeTo	
		avgAVexplainNeg <-  avgAVexplainNeg / absSum * normalizeTo
	}
	usedAttr <- which(abs(expl[]) >= threshold)
	noUsed <- length(usedAttr)
	if (noUsed == 0) {
		plot.new()
		text(0.5,0.5,"All explanations are below treshold", vfont=c("serif","bold"))
		text(0.5,0.4,sprintf("treshold=%g",threshold), vfont=c("serif","bold"))
	}  
	else {
		usedAttrNames <- 1:noUsed
		usedAttrValues <- 1:noUsed
		maxCharsV <- 0
		uA <- 1:noUsed
		for (iA in 1:noUsed) {
			usedAttrNames[iA] <- attrNames[usedAttr[iA]]
			# usedAttrNames[iA] <- gsub("_","\n",attrNames[usedAttr[iA]],fixed=TRUE)
			uA[iA] <- attrValues[usedAttr[iA]]     
			## if usedAttrValues are too long, make them shorter
			if (is.numeric(uA[[iA]]) && uA[[iA]]!=floor(uA[[iA]]))
				usedAttrValues[[iA]]<-sprintf("%.3f",uA[[iA]])
			else if (is.factor(uA[[iA]]))
				usedAttrValues[[iA]]<-  as.character(uA[[iA]])
			else
				usedAttrValues[[iA]]<- uA[[iA]]
			
			for (v in 1:length(usedAttrValues[[iA]])) {  
				maxCharsV = max(maxCharsV, nchar(usedAttrValues[[iA]][[v]]))
				usedAttrValues[[iA]][[v]] <- usedAttrValues[[iA]][[v]]   
				# usedAttrValues[[iA]][[v]] <- gsub("_","\n",usedAttrValues[[iA]][[v]],fixed=TRUE) ;   
			}		
		}  
		
		boxHeight = 1.0
		chExp = 1.0  ## char expansion for boxes
		xLim <- max(abs(expl),abs(avgAVexplainPos),abs(avgAVexplainNeg))
		x <- c(0, 0)
		y <- c(1, noUsed)
		par(xpd=NA,mgp=c(3,0.7,0),mar=c(5.5,8,5,7))
		plot(x, y, type = "l", xlim = c(-xLim, xLim), ylim = c(1.0, noUsed+0.5), xlab = "", ylab="", axes = F)
		text(xLim*1.09,(noUsed+0.4), labels=c("attribute value"), adj = c(0, 0))
		text(-xLim*1.09,(noUsed+0.4), labels=c("attribute"), adj = c(1, 0))
		
		atLabel <- atLabelComp(xLim)
		axis(1, at=atLabel,labels=atLabel)
		## left y axis, attribute names
		anam <- as.character(usedAttrNames)
		lasA = 1 ## horizontal
		cex.axisA = 1
		maxCharsA = max(nchar(anam))
		if (maxCharsA > 15) {
			cex.axisA = 0.75    
			if (maxCharsA > 20)
				cex.axisA = 0.6    
		}	
		axis(2, at=+boxHeight/8.0+c(1:noUsed), labels=usedAttrNames,las=lasA, cex.axis=cex.axisA)
		
		lasV = 1 ## horizontal
		cex.axisV = 1
		if (maxCharsV > 15) {
			cex.axisV = 0.75    
			if (maxCharsV > 20)
				cex.axisV = 0.6    
		}		
		axis(4, at=+boxHeight/8.0+c(1:noUsed), labels=usedAttrValues, las=lasV, cex.axis=cex.axisV)
		
		title(main = titleName, sub=subtitleName)
		
		for(iA in 1:noUsed) {
			y <- iA
			if  (expl[usedAttr[[iA]]] >= 0.0) {
				xDown <- 0
				xUp <- expl[usedAttr[[iA]]]
				rect(xDown, y, xUp, y+boxHeight/4, col=colExplainPos)
			}
			else {
				xDown <- expl[usedAttr[[iA]]]
				xUp <- 0
				rect(xDown, y, xUp, y+boxHeight/4, col=colExplainNeg)
			}
			## print averages  for attribute's values
			## positive average
			xDown <- 0
			xUp <- avgAVexplainPos[usedAttr[iA]]
			rect(xDown, y+2*boxHeight/8, xUp, y+3*boxHeight/8, col=colAVpos)
			## negative average
			xDown <- avgAVexplainNeg[usedAttr[iA]]
			xUp <- 0
			rect(xDown, y+2*boxHeight/8, xUp, y+3*boxHeight/8, col=colAVneg)
		}
	}
}

atLabelComp<-function(xLim) {
	labs <- c(10,5,2,1)
	inc <-  c(2,1,0.5)
	if (xLim > labs[1])
		while (xLim > labs[1]) {
			labs <- labs * 10
			inc <- inc *10
		}
	else if (xLim < labs[4])
		while (xLim < labs[4]) {
			labs <- labs / 10
			inc <- inc / 10
		}
	i=3
	while (xLim > labs[i])
		i <- i-1
	atLabel <- seq(-labs[i],labs[i],inc[i])
	atLabel
}

# return explanation for given classifier, instance using method IME
ime <- function(model, testData, classValue=1, imeInfo, pError=0.05, err=0.05, batchSize=40, maxIter=1000) {
	if (! inherits(model, "CoreModel"))
		stop("Model shall be of class CoreModel.")
	isRegression <- model$noClasses == 0
	noClasses <- model$noClasses
	noInst <- nrow(testData)
	if (! isRegression) {
		class.lev <- model$class.lev;
		noClasses <- model$noClasses
		if (is.numeric(classValue)) {
			selClass <- factor(class.lev[classValue],levels=class.lev)
			classIdx <- classValue
		}
		else if (is.factor(classValue)) {
			selClass <- classValue
			classIdx <- as.integer(classValue)
		}
		else if (is.character(classValue)) {
			selClass <- factor(classValue,levels=class.lev)
			if (is.na(selClass))
				stop("Invalid classValue parameter supplied, shall be compatible with the model: ", classValue)
			classIdx <- as.integer(selClass)
		}
		else 
			stop("Wrong type of classValue parameter supplied: ", classValue)
	}
	
	newFormula <- reformulate(all.vars(model$formula)[-1])
	dat <- model.frame(newFormula, data=testData, na.action=na.pass);
	noAttr <- ncol(dat)
	
	# prepare data structures for output
	expl <- matrix(data=0, nrow=noInst, ncol=noAttr)
	stddev <- matrix(data=0, nrow=noInst, ncol=noAttr)
	iter <- matrix(data=0, nrow=noInst, ncol=noAttr)
	colnames(expl) <- colnames(stddev) <- names(dat)
	
	perm<-matrix(FALSE,nrow=batchSize,ncol=noAttr)
	batchMxSize <- batchSize * noAttr 
	zSq <- abs(qnorm(pError/2))^2
	errSq <- err^2 
	for (i in 1:nrow(dat)){
		inst <- dat[rep(i,batchSize),]
		for (a in 1:noAttr) { 
			noIter <- 0
			moreIterations <- TRUE
			while (moreIterations) {
				# perm is matrix of boolean values; TRUE means that index is befor a-th in the random permutation
				perm[] <- sample(c(FALSE,TRUE),size=batchMxSize,replace=TRUE)
				inst1 <- newdata(imeInfo$generator, size=batchSize)[,colnames(expl)] # random instances
				perm[,a] <- FALSE # a-th shall not be replaced with selected instance
				for (r in 1:noAttr) # a loop is needed to assure a single attribute is replaced at a time, which prevents type coercion to character
					inst1[perm[,r],r] <- inst[perm[,r],r] # replace TRUE (preeceding) with selected instance
				inst2 <- inst1 # inst2 has all succedding (including a-th) filled with random instances
				inst1[,a] <- dat[i,a] # inst1 has a-th filled with selected instance
				
				f1 <- getPredictions(model, inst1, nLaplace=0, noClasses=0, classIdx)
				f2 <- getPredictions(model, inst2, nLaplace=0, noClasses=0, classIdx)
				
				diff <- f1-f2
				expl[i,a] <- expl[i,a] + sum(diff)
				noIter <- noIter + batchSize
				stddev[i,a] <- stddev[i,a] + sum(diff * diff)
				v2 <- stddev[i,a]/noIter - (expl[i,a]/noIter)^2
				neededIter <- zSq * v2 / errSq 
				if (neededIter <= noIter || noIter >= maxIter)
					moreIterations <- FALSE
			}
			expl[i,a] <- expl[i,a] / noIter
			stddev[i,a] <- sqrt(stddev[i,a]/noIter - (expl[i,a]/noIter)^2) 
			iter[i,a] <- noIter
		}
	}
	pCXA <- getPredictions(model, dat, nLaplace=0, noClasses=0, classIdx)
	
	list(expl=expl, pCXA = pCXA, stddev=stddev, noIter=iter)	
} 


wrap4Explanation <- function(model){
	if (inherits(model, "nnet")) {
		model$model <- "nnet"
		termNames <- as.character(attr(model$terms, "variables"))[-1]
		model$formula<- reformulate(termNames[-1],response=termNames[1])
		model$class.lev <-model$lev
		if (is.null(model$class.lev))
			model$noClasses <- 0
		else model$noClasses <-length(model$class.lev)
		class(model) <- c(class(model), "CoreModel")
	}
	else if (inherits(model, "svm")) {
		model$model <- "svm"
		termNames <- as.character(attr(model$terms, "variables"))[-1]
		model$formula<- reformulate(termNames[-1],response=termNames[1])
		model$class.lev <-model$levels
		if (is.null(model$class.lev))
			model$noClasses <- 0
		else model$noClasses <-length(model$class.lev)
		class(model) <- c(class(model), "CoreModel")
	}
	else if (inherits(model, "randomForest")) {
		model$model <- "randomForest"
		termNames <- as.character(attr(model$terms, "variables"))[-1]
		model$formula <- reformulate(termNames[-1],response=termNames[1])
		model$class.lev <- model$levels
		if (is.null(model$class.lev))
			model$noClasses <- 0
		else model$noClasses <- length(model$class.lev)
		class(model) <- c(class(model), "CoreModel")
	}
	else stop("Unknown class of provided model ", class(model))
	
	model
}
