tree <-function(formula, dataset, noTrees = 1, minNodeWeight=5, problemType="byResponse") {
	treeEnsemble(formula, dataset, noTrees=noTrees, noSelectedAttr=ncol(dataset)-1, 
			minNodeWeight=minNodeWeight, problemType=problemType, densityData="no") 
}
bagging <-function(formula, dataset, noTrees = 50, minNodeWeight=2, problemType="byResponse") {
	treeEnsemble(formula, dataset, noTrees=noTrees, noSelectedAttr=ncol(dataset)-1, 
			minNodeWeight=minNodeWeight, problemType=problemType, densityData="no") 
}
rf <-function(formula, dataset, noTrees = 50, minNodeWeight=2, noSelectedAttr=max(1,integer(sqrt(ncol(dataset)-1))),
		problemType="byResponse",densityData="no") {
	treeEnsemble(formula, dataset, noTrees=noTrees, noSelectedAttr=noSelectedAttr, 
			minNodeWeight=minNodeWeight, problemType=problemType, densityData=densityData) 
}
rfDensity <-function(formula, dataset, noTrees = 50, minNodeWeight=2, noSelectedAttr=max(1,integer(sqrt(ncol(dataset)-1))), 
		problemType="byResponse", densityData="leaf", cdfEstimation="ecdf", ...) {
	treeEnsemble(formula, dataset, noTrees=noTrees, noSelectedAttr=noSelectedAttr, 
			minNodeWeight=minNodeWeight, problemType=problemType, densityData=densityData, 
			cdfEstimation=cdfEstimation, ...)
}
densityEnsemble <-function(formula, dataset, noTrees = 100, minNodeWeight=2, noSelectedAttr=2,
		problemType="density", densitySplitMethod="balancedSplit", densityData="leaf",
		cdfEstimation = "ecdf", ...) {
	treeEnsemble(formula, dataset, noTrees=noTrees, minNodeWeight=minNodeWeight,
			noSelectedAttr=noSelectedAttr, problemType=problemType, 
			densitySplitMethod=densitySplitMethod, densityData=densityData, 
			cdfEstimation=cdfEstimation, ...)				   
}
indAttrGen <-function(formula, dataset, cdfEstimation = c("ecdf","logspline","kde"), problemType="byResponse") {
	treeEnsemble(formula, dataset, cdfEstimation=cdfEstimation, noTrees=1, minNodeWeight=nrow(dataset), 
			problemType=problemType, densityData="leaf") 
}

treeEnsemble <-function(formula, dataset, noTrees = 100, minNodeWeight=2, noSelectedAttr=0, 
		problemType=c("byResponse","classification", "regression","density"),
		densityData=c("leaf", "topDown", "bottomUp","no"),
		cdfEstimation = c("ecdf","logspline","kde"), 
		densitySplitMethod=c("balancedSplit","randomSplit","maxVariance"),					
		estimator=NULL, ...) 
{
	if (!inherits(formula,"formula")) 
		stop("First argument must be a formula.");
	problemType <- match.arg(problemType)	
	densitySplitMethod <- match.arg(densitySplitMethod)
	densityData <- match.arg(densityData)
	cdfEstimation <- match.arg(cdfEstimation)
	#prepare data according to the supplied formula
	dat <- model.frame(formula, data=dataset, na.action=na.pass);
	terms <- attr(dat,"terms")
	# check and make target variable coherent with problemType
	if (attr(terms,"response")==0) {
		if (problemType %in% c("byResponse", "density"))
			problemType <- "density"
		else
			stop("Formula shall provide target variable in classification and regression problems")
	}
	else {
		if (problemType == "density")
			stop("Formula shall not define target variable for density ensembles.")
	}
	if (problemType == "classification" && ! is(dat[[1]],"factor")) {
		dat[[1]] <- factor(dat[[1]]);
		cat("Changing dependent variable to factor with levels:",levels(dat[[1]]),"\n");
	}
	if (problemType == "regression" && ! is(dat[[1]],"numeric"))
		stop("Prediction variable type shall be numeric for regression problems.")
	if (problemType == "byResponse") {
		if (is(dat[[1]],"factor"))
			problemType <- "classification"
		else if (is(dat[[1]],"numeric"))
			problemType <- "regression"
		else stop("Prediction variable type shall be either factor for classification or numeric for regression problems.")
	}
	if (problemType == "classification" || problemType == "regression") {
	  missingVals <- is.na(dat[[1]])
	  if (any(missingVals)) {
	    cat("Removing ", sum(missingVals)," instances with missing value in dependent variable\n");
	    dat <- dat[!missingVals,]
	  }
	}
	
	# set and check estimator
	if ( is.null(estimator) ) {
		if (problemType == "classification")
			estimator <- "Gini"
		else estimator <-"MSEofMean"
	}
	else {
		if (problemType == "classification"){
			if (! estimator %in% infoCore(what="attrEval"))
				stop("Estimator ", estimator, " is not a valid option for classification problem.")
		} else if (problemType == "regression"){
			if (! estimator %in% infoCore(what="attrEvalReg"))
				stop("Estimator ", estimator, " is not a valid option for regression problem.")
		}
	}
	
	noInst <- nrow(dat)
	noAttr = ncol(dat)
	
	if (problemType != "density") {
		dat <- dat[,c(2:ncol(dat),1)] # reorder, class to the end
		classIdx <- ncol(dat)
		predictorName <- names(dat)[classIdx]
		noPredictors <- noAttr - 1
	}
	else {
		classIdx <- NULL
		predictorName <- NULL
		noPredictors <- noAttr
	}
	
	if (problemType == "classification") {
		class.lev <- levels(dat[[classIdx]]);
		noClasses <- length(class.lev);
		classProb <- table(dat[[classIdx]])/nrow(dat)  } 
	else {
		class.lev <- NULL
		noClasses <- 0
		classProb <- NULL
	}
	
	if (noSelectedAttr > noPredictors || noSelectedAttr == -2)
		noSelectedAttr <- noPredictors
	else if (noSelectedAttr == 0)
		noSelectedAttr <- round(sqrt(noPredictors))
	else if (noSelectedAttr == -1)
		noSelectedAttr <- 1+round(log2(noPredictors))
	
	if (densityData != "no") { # collect additional data needed for generation of artificial data
		attrClasses<-list()
		attrLevels <-list()
		attrOrdered<-logical()
		for (i in 1:noAttr) {
			attrClasses[[i]] <- class(dat[[i]])
			attrOrdered[i] <- is.ordered(dat[[i]])
			if (is.factor(dat[[i]])){
				attrLevels[[i]] <- levels(dat[[i]])
			}
			else attrLevels[[i]] <- NULL    
		}
	}
	
	# create trees
	splitVector = vector("logical",length=noInst)
	trees<-list()
	for (t in 1:noTrees) {
		splitVector[] <-TRUE
		if (noTrees==1) { #ordinary decision or regression tree
			ib <- 1:noInst
			oob <-c()
		}
		else {
			ib <- sample(1:noInst, size = noInst, replace = TRUE)
			splitVector[ib] <- FALSE
			oob <- (1:noInst)[splitVector]
		}
		if (problemType == "density")
			rt <- densityTree(dat[ib,], minNodeWeight,  noSelectedAttr, densitySplitMethod, densityData)
		else 
			rt <- randomTree(dat[ib,], minNodeWeight, noSelectedAttr=noSelectedAttr, problemType, densityData, estimator)
		
		trees[[t]] <- list(inBag = ib, outOfBag = oob, tree = rt)
	}
	
	treeEnsemble <- list(formula=formula, terms = terms, class.lev = class.lev, noClasses = noClasses, predictorName=predictorName,                       
			noTrees = noTrees, trees = trees, problemType = problemType, densityData = densityData, 
			cdfEstimation = cdfEstimation, classProb = classProb)
	if (densityData != "no") {
		treeEnsemble$noAttr = noAttr
		treeEnsemble$attrClasses = attrClasses
		treeEnsemble$attrLevels = attrLevels
		treeEnsemble$attrNames = names(dat)
		treeEnsemble$originalNames <- names(dataset)[sort(match(names(dat), names(dataset)))]
		treeEnsemble$attrOrdered = attrOrdered
		treeEnsemble$dat <- dat  # store the generator data 
		treeEnsemble <- treeDataGenerator(treeEnsemble, dat, ...)  	
	}
	
	class(treeEnsemble) <- "TreeEnsemble"
	return(treeEnsemble)
}

predict.TreeEnsemble <- function(object, data, type=c("both","class","probability")) {
	type <- match.arg(type)
	if (object$problemType=="classification") {
		class.lev <- object$class.lev;
		noClasses <- length(class.lev);
	}
	noInst = nrow(data)
	terms <- delete.response(object$terms);
	newdat <- as.data.frame(data)
	dat <- model.frame(terms, data=newdat, na.action=na.pass);
	
	if (object$problemType=="classification") {
		ePred <- factor(c(), levels = class.lev)
		eProb <- matrix(NA, nrow = noInst, ncol=noClasses)
		prob <- matrix(NA, nrow = object$noTrees, ncol=noClasses)
		for (i in 1:noInst) {
			pred <- factor(c(), levels = class.lev)
			prob[,] <- NA
			for (t in 1:object$noTrees) {
				prediction <- predictWithTree(object$trees[[t]]$tree, dat[i,])
				pred[t] <- prediction$majorityClass
				prob[t,] <- prediction$classProb
			}
			#ensemble predictions
			ePred[i] <- factor(class.lev[which.is.max(table(pred))], levels=class.lev)
			eProb[i,] <- apply(prob, 2, sum) / object$noTrees
		}	  
		if (type == "both")
			returnList <- list(class=ePred,probabilities=eProb)
		else if (type=="class")
			returnList <- ePred
		else if (type == "probability")
			returnList <- eProb  
	}
	else {
		ePred <- vector(mode="numeric", length = noInst)  
		pred <- vector(mode="numeric", length = object$noTrees)  
		for (i in 1:noInst) {
			for (t in 1:object$noTrees) {
				pred[t] <- predictWithTree(object$trees[[t]]$tree, dat[i,])
			}
			#ensemble predictions
			ePred[i] <- sum(pred) / object$noTrees
		}	  
		returnList <- ePred
	}
	returnList
}

# newdata <- function(object, ...) UseMethod("newdata", object)

#newdataOld.TreeEnsemble <- function(object, size=1, onlyPath=FALSE, classProb=NULL, predictClass=FALSE, ...) {
#	if (object$densityData == "no")
#		stop("newdata needs proper information stored in the model to generate data")
#	if (! is(object, "TreeEnsemble"))
#		stop("newdata cannot generate data for objects of class ",class(object))
#	
#	if (object$problemType == "classification" && predictClass == TRUE) # try to assure correct class distribution
#		noInst <- 2 * size
#	else noInst <- size
#	
#	dat <- matrix(NA, nrow = noInst, ncol = object$noAttr )
#	
#	if (object$problemType == "classification") {
#		if (is.null(classProb)) { # use class probability from the generator
#			classProb <- object$classProb
#			classProb <- classProb / sum(classProb) # normalize sum to 1
#		}
#		nFromClass <- round(noInst * classProb)
#		
#		while (sum(nFromClass) < noInst) { # correct possible rounding error
#			rndIdx <- 1 + floor(runif(n=1, min=0, max=length(nFromClass)))
#			nFromClass[rndIdx] <- nFromClass[rndIdx] + 1
#		}
#		while (sum(nFromClass) > noInst) { # correct possible rounding error
#			rndIdx <- 1 + floor(runif(n=1, min=0, max=length(nFromClass)))
#			if (nFromClass[rndIdx] > 0)
#			   nFromClass[rndIdx] <- nFromClass[rndIdx] - 1
#		}
#		
#		classIdx <- object$noAttr # match(object$predictorName, object$originalNames)
#	}
#	else {
#		nFromClass <- c(noInst)
#		classIdx <- 0
#		selectedClass <- 1
#	}
#	nToGen <- noInst
#	
#	if (object$densityData == "leaf") {
#		i <- 1
#		while (any(nFromClass > 0)) { # force generation according to the given class distribution
#			treeOrder <- sample(1:object$noTrees, size=object$noTrees, replace=F)
#			treeIdx <- 1
#			if (object$problemType == "classification") 
#				selectedClass <- sample(1:object$noClasses, size=1, prob = nFromClass / nToGen )		
#			while (any(is.na(dat[i,]))) {
#				t <- treeOrder[treeIdx]
#				if ((object$problemType == "classification" && any(object$dataGenerator[[t]]$classWeights[[selectedClass]]>0)) ||
#						(object$problemType != "classification") ) { # is any leaf with instances of chosen class present
#					if (treeIdx == 1) { # the first leaf is selected based on required class distribution
#						if (object$problemType == "classification") {
#							leafIdx <- sample(1:length(object$dataGenerator[[t]]$leaves), size = 1, prob = object$dataGenerator[[t]]$classWeights[[selectedClass]], replace=T)
#						}
#						else {  # for regression or density problems select based on leaf weight distribution
#							leafIdx <- sample(1:length(object$dataGenerator[[t]]$leaves), size = 1, prob = object$dataGenerator[[t]]$weights, replace=T)
#						}		
#					}
#					else {  # find the leaf based on already assigned values 
#						leafIdx <- findLeafIdx(object$trees[[t]]$tree, dat[i,])
#					}
#					
#					# generate missing 
#					for (a in 1:object$noAttr) {
#						if (!is.na(dat[i,a])){
#							next 
#						}
#						else if (a == classIdx){
#							dat[i,classIdx] <- selectedClass 
#						}
#						else if (onlyPath && !object$dataGenerator[[t]]$leaves[[leafIdx]]$onPath[a]){
#							next 
#						}
#						else if (! any(class(object$dataGenerator[[t]]$leaves[[leafIdx]]$cdfs[[a]]) %in% c("ecdf","logspline","kde"))) {
#							dat[i,a] <- NA
#						}
#						else if ("factor" %in% object$attrClasses[[a]]) {
#							dat[i, a] <- quantile(object$dataGenerator[[t]]$leaves[[leafIdx]]$cdfs[[a]], probs=runif(1, 0, 1), type=3)						
#						}
#						else if ( "ecdf" %in% class(object$dataGenerator[[t]]$leaves[[leafIdx]]$cdfs[[a]]) ) {
#							dat[i, a] <- quantile(object$dataGenerator[[t]]$leaves[[leafIdx]]$cdfs[[a]], probs=runif(1, 0, 1), type=8, ...)
#						}
#						else if ( "logspline" %in% class(object$dataGenerator[[t]]$leaves[[leafIdx]]$cdfs[[a]]) ){
#							dat[i,a] <- rlogspline(1, object$dataGenerator[[t]]$leaves[[leafIdx]]$cdfs[[a]]) 
#						}
#						else if ( "kde" %in% class(object$dataGenerator[[t]]$leaves[[leafIdx]]$cdfs[[a]]) ) {
#							dat[i, a] <- rkde(1, object$dataGenerator[[t]]$leaves[[leafIdx]]$cdfs[[a]])
#						}
#						else stop("Invalid type of generator detected in the leaf ",class(object$dataGenerator[[t]]$leaves[[leafIdx]]$cdfs[[a]]))			  
#					}
#				}		
#				treeIdx <- treeIdx + 1
#				if (treeIdx > object$noTrees && any(is.na(dat[i,]))) { # default generator
#					for (j in 1:ncol(dat)) {
#						if (is.na(dat[i,j])) {
#							if ("factor" %in% object$attrClasses[[j]]) {
#								dat[i,j] <- useDefaultGenerator(object$defaultGenerator[[j]], TRUE)
#							}
#							else {
#								dat[i,j] <- useDefaultGenerator(object$defaultGenerator[[j]], FALSE)						      
#							}
#						}
#					}
#				}	  
#			}
#			i <- i+1
#			nToGen <- nToGen - 1
#			nFromClass[selectedClass] <- nFromClass[selectedClass]-1
#		}		
#	}
#	else { # generators other then in the leaf
#		for (i in 1:noInst) {
#			treeOrder <- sample(1:object$noTrees, object$noTrees)
#			t <- 1
#			# while not all the values are filled in
#			while (any(is.na(dat[i,])) && t <= object$noTrees) {
#				if (object$densityData == "topDown") {
#					gen <- fillDataWithTreeTopDown(object$trees[[treeOrder[t]]]$tree, dat[i,])
#				}
#				else if (object$densityData == "bottomUp") {
#					gen <- fillDataWithTreeBottomUp(object$trees[[treeOrder[t]]]$tree, dat[i,])     
#				}
#				else stop("newdata encountered unrecognized densityData type in the model ", object$densityData)
#				dat[i,] <- gen 
#				t <- t + 1
#			}
#			if (any(is.na(dat[i,]))) {
#				for (j in 1:ncol(dat))
#					if (is.na(dat[i,j])){
#						if ("factor" %in% object$attrClasses[[j]]) 
#							dat[i,j] <- useDefaultGenerator(object$defaultGenerator[[j]], TRUE)
#						else
#							dat[i,j] <- useDefaultGenerator(object$defaultGenerator[[j]], FALSE)
#					}
#			}	  
#		}
#	}
#	dat[dat == -.Machine$double.xmax] <- NA  # fix values set by NAgenerator 
#	newdat <- as.data.frame(dat)
#	
#	names(newdat) <- object$attrNames
#	for (i in 1:object$noAttr){
#		if ("factor" %in% object$attrClasses[[i]]) {
#			newdat[[i]] <- factor(newdat[[i]],levels=seq_along(object$attrLevels[[i]]), labels=object$attrLevels[[i]])
#			if (object$attrOrdered[i])
#				newdat[[i]] <- as.ordered(newdat[[i]])
#		}
#		else if ("integer" %in% object$attrClasses[[i]])
#			newdat[[i]] <- as.integer(round(newdat[[i]]))
#	}
#	
#	# set class values according to prediction
#	if (object$problemType == "classification" && predictClass == TRUE) {
#		pred <- predict(object, newdat, type = "class")
#		
#		nFromClass <- round(size * classProb)
#		while (sum(nFromClass) < size) { # correct possible rounding error
#			rndIdx <- 1 + floor(runif(n=1, min=0, max=length(nFromClass)))
#			nFromClass[rndIdx] <- nFromClass[rndIdx] + 1
#		}
#		while (sum(nFromClass) > size) { # correct possible rounding error
#			rndIdx <- 1 + floor(runif(n=1, min=0, max=length(nFromClass)))
#			nFromClass[rndIdx] <- nFromClass[rndIdx] - 1
#		}
#		returnData <- newdat[NULL,]
#		for (cl in 1:length(nFromClass)) {
#			if (nFromClass[cl] == 0)
#				next ;
#			clData <- newdat[as.integer(pred) == cl,]
#			if (nFromClass[cl] <= nrow(clData)) { # take all with predicted class cl
#				prepData <- clData[1:nFromClass[cl],]
#			}
#			else { # take all with predicted class cl and the missing ones with sampled class cl 
#				prepData <- rbind(clData, newdat[as.integer(newdat[,classIdx])==cl,][1:(nFromClass[cl]-nrow(clData)),])
#			}
#			# assign the class cl to all of them
#			prepData[[classIdx]] <- factor(cl,levels=1:length(object$attrLevels[[classIdx]]), labels=object$attrLevels[[classIdx]]) 
#			returnData <- rbind(returnData, prepData)
#		}
#		returnData <- returnData[sample(1:size,size=size,replace=FALSE),] # shuffle instances
#		returnData <- returnData[,object$originalNames] # reorder attributes
#		return(returnData)
#	}
#	else {
#		newdat <- newdat[,object$originalNames] # reorder attributes
#		return(newdat)
#	}			
#	
#}

newdata.TreeEnsemble <- function(object, fillData=NULL, size=ifelse(is.null(fillData),1,nrow(fillData)), onlyPath=FALSE, classProb=NULL, predictClass=FALSE, ...) {
	if (object$densityData == "no")
		stop("newdata needs proper information stored in the model to generate data")
	if (! is(object, "TreeEnsemble"))
		stop("newdata cannot generate data for objects of class ",class(object))
	
	if (object$problemType == "classification" && predictClass == TRUE) # try to assure correct class distribution
		noInst <- 2 * size
	else noInst <- size
	
	
	if (object$problemType == "classification") {
		if (is.null(classProb)) { # use class probability from the generator
			classProb <- object$classProb
			classProb <- classProb / sum(classProb) # normalize sum to 1
		}
		nFromClass <- round(noInst * classProb)
		
		while (sum(nFromClass) < noInst) { # correct possible rounding error
			rndIdx <- 1 + floor(runif(n=1, min=0, max=length(nFromClass)))
			nFromClass[rndIdx] <- nFromClass[rndIdx] + 1
		}
		while (sum(nFromClass) > noInst) { # correct possible rounding error
			rndIdx <- 1 + floor(runif(n=1, min=0, max=length(nFromClass)))
			if (nFromClass[rndIdx] > 0)
				nFromClass[rndIdx] <- nFromClass[rndIdx] - 1
		}
		
		classIdx <- object$noAttr # match(object$predictorName, object$originalNames)
	}
	else {
		nFromClass <- c(noInst)
		classIdx <- 0
		selectedClass <- 1
	}
	nToGen <- noInst
	
	if (is.null(fillData)) { # no prespecified value, generate everything from scratch
		dat <- matrix(NA, nrow = noInst, ncol = object$noAttr)
	}
	else {
		dat <- fillData 
		if (classIdx > 0) {
			for (col in 1:ncol(dat)) {
				if (is.factor(dat[[col]]))
		           dat[[col]] <- as.integer(dat[[col]])
		   }
	   }
	}
	
	if (object$densityData == "leaf") {
		i <- 1
		while (any(nFromClass > 0)) { # force generation according to the given class distribution
			treeOrder <- sample(1:object$noTrees, size=object$noTrees, replace=F)
			treeIdx <- 1
			if (object$problemType == "classification") 
				selectedClass <- sample(1:object$noClasses, size=1, prob = nFromClass / nToGen )		
			while (any(is.na(dat[i,]))) {
				t <- treeOrder[treeIdx]
				if ((object$problemType == "classification" && any(object$dataGenerator[[t]]$classWeights[[selectedClass]]>0)) ||
						(object$problemType != "classification") ) { # is any leaf with instances of chosen class present
					if (treeIdx == 1) { # the first leaf is selected based on required class distribution
						if (object$problemType == "classification") {
							leafIdx <- sample(1:length(object$dataGenerator[[t]]$leaves), size = 1, prob = object$dataGenerator[[t]]$classWeights[[selectedClass]], replace=T)
						}
						else {  # for regression or density problems select based on leaf weight distribution
							leafIdx <- sample(1:length(object$dataGenerator[[t]]$leaves), size = 1, prob = object$dataGenerator[[t]]$weights, replace=T)
						}		
					}
					else {  # find the leaf based on already assigned values 
						leafIdx <- findLeafIdx(object$trees[[t]]$tree, dat[i,])
					}
					
					# generate missing 
					for (a in 1:object$noAttr) {
						if (!is.na(dat[i,a])){
							next 
						}
						else if (a == classIdx){
							dat[i,classIdx] <- selectedClass 
						}
						else if (onlyPath && !object$dataGenerator[[t]]$leaves[[leafIdx]]$onPath[a]){
							next 
						}
						else if (! any(class(object$dataGenerator[[t]]$leaves[[leafIdx]]$cdfs[[a]]) %in% c("ecdf","logspline","kde"))) {
							dat[i,a] <- NA
						}
						else if ("factor" %in% object$attrClasses[[a]]) {
							dat[i, a] <- quantile(object$dataGenerator[[t]]$leaves[[leafIdx]]$cdfs[[a]], probs=runif(1, 0, 1), type=3)						
						}
						else if ( "ecdf" %in% class(object$dataGenerator[[t]]$leaves[[leafIdx]]$cdfs[[a]]) ) {
							dat[i, a] <- quantile(object$dataGenerator[[t]]$leaves[[leafIdx]]$cdfs[[a]], probs=runif(1, 0, 1), type=8, ...)
						}
						else if ( "logspline" %in% class(object$dataGenerator[[t]]$leaves[[leafIdx]]$cdfs[[a]]) ){
							dat[i,a] <- rlogspline(1, object$dataGenerator[[t]]$leaves[[leafIdx]]$cdfs[[a]]) 
						}
						else if ( "kde" %in% class(object$dataGenerator[[t]]$leaves[[leafIdx]]$cdfs[[a]]) ) {
							dat[i, a] <- rkde(1, object$dataGenerator[[t]]$leaves[[leafIdx]]$cdfs[[a]])
						}
						else stop("Invalid type of generator detected in the leaf ",class(object$dataGenerator[[t]]$leaves[[leafIdx]]$cdfs[[a]]))			  
					}
				}		
				treeIdx <- treeIdx + 1
				if (treeIdx > object$noTrees && any(is.na(dat[i,]))) { # default generator
					for (j in 1:ncol(dat)) {
						if (is.na(dat[i,j])) {
							if ("factor" %in% object$attrClasses[[j]]) {
								dat[i,j] <- useDefaultGenerator(object$defaultGenerator[[j]], TRUE)
							}
							else {
								dat[i,j] <- useDefaultGenerator(object$defaultGenerator[[j]], FALSE)						      
							}
						}
					}
				}	  
			}
			i <- i+1
			nToGen <- nToGen - 1
			nFromClass[selectedClass] <- nFromClass[selectedClass]-1
		}		
	}
	else { # generators other then in the leaf
		for (i in 1:noInst) {
			treeOrder <- sample(1:object$noTrees, object$noTrees)
			t <- 1
			# while not all the values are filled in
			while (any(is.na(dat[i,])) && t <= object$noTrees) {
				if (object$densityData == "topDown") {
					gen <- fillDataWithTreeTopDown(object$trees[[treeOrder[t]]]$tree, dat[i,])
				}
				else if (object$densityData == "bottomUp") {
					gen <- fillDataWithTreeBottomUp(object$trees[[treeOrder[t]]]$tree, dat[i,])     
				}
				else stop("newdata encountered unrecognized densityData type in the model ", object$densityData)
				dat[i,] <- gen 
				t <- t + 1
			}
			if (any(is.na(dat[i,]))) {
				for (j in 1:ncol(dat))
					if (is.na(dat[i,j])){
						if ("factor" %in% object$attrClasses[[j]]) 
							dat[i,j] <- useDefaultGenerator(object$defaultGenerator[[j]], TRUE)
						else
							dat[i,j] <- useDefaultGenerator(object$defaultGenerator[[j]], FALSE)
					}
			}	  
		}
	}
	dat[dat == -.Machine$double.xmax] <- NA  # fix values set by NAgenerator 
	newdat <- as.data.frame(dat)
	
	names(newdat) <- object$attrNames
	for (i in 1:object$noAttr){
		if ("factor" %in% object$attrClasses[[i]]) {
			newdat[[i]] <- factor(newdat[[i]],levels=seq_along(object$attrLevels[[i]]), labels=object$attrLevels[[i]])
			if (object$attrOrdered[i])
				newdat[[i]] <- as.ordered(newdat[[i]])
		}
		else if ("integer" %in% object$attrClasses[[i]])
			newdat[[i]] <- as.integer(round(newdat[[i]]))
	}
	
	# set class values according to prediction
	if (object$problemType == "classification" && predictClass == TRUE) {
		pred <- predict(object, newdat, type = "class")
		
		nFromClass <- round(size * classProb)
		while (sum(nFromClass) < size) { # correct possible rounding error
			rndIdx <- 1 + floor(runif(n=1, min=0, max=length(nFromClass)))
			nFromClass[rndIdx] <- nFromClass[rndIdx] + 1
		}
		while (sum(nFromClass) > size) { # correct possible rounding error
			rndIdx <- 1 + floor(runif(n=1, min=0, max=length(nFromClass)))
			nFromClass[rndIdx] <- nFromClass[rndIdx] - 1
		}
		returnData <- newdat[NULL,]
		for (cl in 1:length(nFromClass)) {
			if (nFromClass[cl] == 0)
				next ;
			clData <- newdat[as.integer(pred) == cl,]
			if (nFromClass[cl] <= nrow(clData)) { # take all with predicted class cl
				prepData <- clData[1:nFromClass[cl],]
			}
			else { # take all with predicted class cl and the missing ones with sampled class cl 
				prepData <- rbind(clData, newdat[as.integer(newdat[,classIdx])==cl,][1:(nFromClass[cl]-nrow(clData)),])
			}
			# assign the class cl to all of them
			prepData[[classIdx]] <- factor(cl,levels=1:length(object$attrLevels[[classIdx]]), labels=object$attrLevels[[classIdx]]) 
			returnData <- rbind(returnData, prepData)
		}
		returnData <- returnData[sample(1:size,size=size,replace=FALSE),] # shuffle instances
		returnData <- returnData[,object$originalNames] # reorder attributes
		return(returnData)
	}
	else {
		newdat <- newdat[,object$originalNames] # reorder attributes
		return(newdat)
	}			
	
}


treeDataGenerator<-function(ensemble, dat, ...) {
	
	if (ensemble$densityData == "leaf") { 
		ensemble$dataGenerator <- list()
		for (t in 1:ensemble$noTrees) {
			ensemble$dataGenerator[[t]] <- list()
			ensemble$trees[[t]]$tree <- fillWithInstances(ensemble$trees[[t]]$tree, dat[ensemble$trees[[t]]$inBag,], ensemble$trees[[t]]$inBag)
			treeListWithIdx <- fillWithLeafIdx(ensemble$trees[[t]]$tree, 1)
			ensemble$trees[[t]]$tree <- treeListWithIdx$node
			ensemble$dataGenerator[[t]]$leaves <- list()
			ensemble$dataGenerator[[t]]$weights <- c()
			ensemble$dataGenerator[[t]] <- genFromLeaves(ensemble$trees[[t]]$tree, dat, ensemble$cdfEstimation, ...)
		}
	}
	else {
		for (t in 1:ensemble$noTrees) {
			
			ensemble$trees[[t]]$tree <- fillWithInstances(ensemble$trees[[t]]$tree, dat[ensemble$trees[[t]]$inBag,], ensemble$trees[[t]]$inBag)
			
			ensemble$trees[[t]]$tree <- generatorFromTree(ensemble$trees[[t]]$tree, dat, ensemble$densityData, ensemble$cdfEstimation, ...)
		}  
	}
	ensemble$defaultGenerator <- defaultGenerator(dat, ensemble$cdfEstimation, ... )
	
	
	return(ensemble)
}

robustLogspline<-function(x, ...) {
	tc <- tryCatch(model<-logspline(x, ...),error=function(e) e, warning=function(w) w)
	if (is(tc,"warning") || is(tc,"error")) {
		model <- ecdf(x)
		warning(tc, "converting to ecdf")
	}
	model
}

robustKde<-function(x, ...) {
	tc <- tryCatch(model<-kde(x, ...),error=function(e) e, warning=function(w) w)
	if (is(tc,"warning") || is(tc,"error")) {
		model <- ecdf(x)
		warning(tc, "converting to ecdf")
	}
	model
}

defaultGenerator <- function(data, cdfEstimation="ecdf", ...) {
	dgen <- list()
	for (a in 1:ncol(data)) {
		vals <- data[, a]
		if (all(is.na(vals))) { # this should not happen, but if it happens due to unfortunate samplinge, we shall generate NAgenerator
			NAgen <- "NAgenerator"
			class(NAgen) <- "NAgenerator"
			dgen[[a]] <- NAgen
		}
		else if ( cdfEstimation == "ecdf" || is.factor(data[[a]]) )
			dgen[[a]] <- ecdf(vals)
		else if (cdfEstimation == "logspline")                             
			dgen[[a]] <- robustLogspline(vals, ...)
		else if (cdfEstimation == "kde")
			dgen[[a]] <- robustKde(vals, ...)
	}
	names(dgen) <- names(data)
	return(dgen)
}

useDefaultGenerator<-function(dataGenerator, genFactor=FALSE) {
	#if (! any(class(dataGenerator) %in% c("ecdf","logspline","kde","NAgenerator")))
	#	stop("usedefaultGenerator found unexpected type of generator")
	if (  "ecdf" %in% class(dataGenerator) ) {
		if (genFactor)
			return (quantile(dataGenerator, probs=runif(1, 0, 1), type=3))
		else
			return (quantile(dataGenerator, probs=runif(1, 0, 1), type=8))
	}
	else if ( "logspline" %in% class(dataGenerator) )
		return(rlogspline(1, dataGenerator)) 
	else if ( "kde" %in% class(dataGenerator) )
		return(rkde(1, dataGenerator))
	else if ( "NAgenerator" %in% class(dataGenerator) )
		return(-.Machine$double.xmax)
	else stop("Default data generator is of unknown type.")
}

# The function contains three data cleaning submethods, the first two remove instances whose distance to its nearest neighbors are too small 
# or too large. The first checks similarity of instances disregarding class,
# the second checks similarity of instances taking only instances from the same class into account, and
# the third one does not reject the data but only reassigns response variable using the prediction model stored in the teObject.
# The meaning of parameters are as follows.
# - With parameters similarDropP and dissimilarDropP we remove instances in newdat too close to existing instances or
#   too far from existing instances. The values represent the percentage of smaller/larger values computed on the training data
#   contained in the teObject
# - With parameters similarDropPclass and dissimilarDropPclass we remove instances in newdat too close to existing instances of the same class
#   or too distant from existing instances of the same class. The values represent percentages of smaller/larger values taken from 
#   the training set and stored in teObject.
# - The parameter nearestInstK controls distance to how many old instances we take into account when computing 
#   the threshold of too close/far instances to be removed.
# - The parameter reassignResponse controls whether the response column of the cleaned data shall be set anew 
#   using the provided prediction model or taken as it is.
# - cleaningObject stores precomputed data from previous runs of cleaning which saves computation in next runs
cleanData <- function(teObject, newdat, similarDropP=NA, dissimilarDropP=NA, similarDropPclass=NA, dissimilarDropPclass=NA, 
		nearestInstK=1, reassignResponse=FALSE, cleaningObject=NULL) {
	
	if (! (is(teObject, "TreeEnsemble") || is(teObject, "RBFgenerator") ))
		stop("Cleaning new data is only possible possible with objects of class TreeEnsemble or RBFgenerator, yours is of class ",class(teObject))
		
	newdat <- newdat[,names(teObject$dat)]  # reorder to conform to dat
	
	if (is.null(cleaningObject)) 
		# create new cleaningData object to store distributions for consequent runs
		cleaningObject <- list()
	
	# remove instances too close or too far from its nearest neighbors disregarding class
	if (!is.na(similarDropP) && !is.na(dissimilarDropP)) {
				
		# distribution of distances to nearest instances was not computed yet
		if (is.null(cleaningObject$nearestDist)) {
			
			if (is.null(teObject$predictorName))
				dataset <- teObject$dat
			else
				dataset <- teObject$dat[,- ncol(teObject$dat)] # skip predictor variable for computation of distances
			
			# prepare distances of nearest instances,
			# for each instance we find the distance to its nearestInsK nearest instances, disregarding the identical instances
			# as the result we store nearestDist array which contains a distribution of nearest distances  for all training instances
			distMx <- gower.dist(dataset) #from StatMatch package
			distances <- apply(distMx, 1, sort) # results (sorted distances to all other instances) are in columns
			if (!is.matrix(distances))
				distances <- as.matrix(distances)
			firstNonZero <- apply(distances!=0, 2, function(x) which(x)[1])
			# up to how many near instances we take into account
			final <- firstNonZero + nearestInstK
			# distK contains the final distribution of nearest instances
			distK<-c()
			for (i in 1:ncol(distances)) {
				if (is.na(firstNonZero[i]))
					distK[i] <- 0
				else # compute the mean of nearestKInst closest instances			
					distK[i] <- mean(distances[firstNonZero[i]:min(final[i], nrow(distances)), i])
			}
			cleaningObject$nearestDist <- sort(distK)  # store sorted distances to nearest neighbors
		}
		
		# based on nearestDist which ontains the distribution of distances to nearest neighbors 
		#  set thresholds for removal of newdat instances
		noDist <- length(cleaningObject$nearestDist)
		if (similarDropP==0)
			nearDistDrop <- 0
		else
			nearDistDrop <- cleaningObject$nearestDist[max(1, round(similarDropP * noDist))]
		if (dissimilarDropP == 1) 
			farDistDrop=1
		else 
			farDistDrop <- cleaningObject$nearestDist[min(noDist, round(dissimilarDropP*noDist))]
		
		# compute distance of all newdat instances to all training instances stored in teObject, 
		if (teObject$problemType != "density")
			distMx <- gower.dist(data.x = teObject$dat[,-ncol(teObject$dat)], data.y=newdat[,-ncol(newdat)])
		else # density type ensemble does not contain response in dat, so we do not remove it
			distMx <- gower.dist(data.x = teObject$dat, data.y=newdat)
		
		# sort distances for each newdat instance separately
		distances <- apply(distMx, 2, sort) # results are in columns
		if (!is.matrix(distances))
			distances<-as.matrix(distances)
		# find first non-zero distance
		firstNonZero <- apply(distances!=0, 2, function(x) which(x)[1])
		# for computation of distance to nearest neighbors use nearestInstK closest instances
		final <- firstNonZero + nearestInstK
		
		# for each instance compute nearest distance to others, use nearestInstK nearest
		distK <- c()
		for (i in 1:ncol(distances))
			if (is.na(firstNonZero[i]))
				distK[i] <- 0
			else
				distK[i] <- mean(distances[firstNonZero[i]:min(final[i], nrow(distances)), i])
		
		# keep only those instances which are between the two thresholds
		validNew <- distK >= nearDistDrop & distK <= farDistDrop
		newdat <- newdat[validNew,]
	}
	
	
	# remove instances too close or too far from existing nearest neighbors of the same class
	if ((teObject$problemType == "classification") && 
		( (length(similarDropPclass) > 1) || (length(similarDropPclass) == 1 && !is.na(similarDropPclass)) || 
		  (length(dissimilarDropPclass) > 1) || (length(dissimilarDropPclass) == 1 && !is.na(dissimilarDropPclass)) )) {
		
		# distribution of distances to nearest instances was not computed yet
		if (is.null(cleaningObject$nearestDistCl)) {
			
			# prepare distances to nearest instances by class value
			nearestDistCl <-list()
			for (cl in 1:teObject$noClasses) {
				dataCl <- teObject$dat[teObject$dat[,ncol(teObject$dat)]==teObject$class.lev[cl], ]
				if (nrow(dataCl) <= 1) {
					nearestDistCl[[cl]] <- NA
					next
				}
				distMx <- gower.dist(dataCl) 
				distances <- apply(distMx, 1, sort) # results are in columns
				if (!is.matrix(distances))
					distances<-as.matrix(distances)
				firstNonZero <- apply(distances!=0, 2, function(x) which(x)[1])
				final <- firstNonZero + nearestInstK
				distK<-c()
				for (i in 1:ncol(distances)) {
					if (is.na(firstNonZero[i]))
						distK[i] <- 0
					else
						distK[i] <- mean(distances[firstNonZero[i]:min(final[i], nrow(distances)), i])
				}
				nearestDistCl[[cl]] <- sort(distK)
			}
			cleaningObject$nearestDistCl <- nearestDistCl
		}
		
		# prepare data structures for thresholds
		if(length(similarDropPclass) == 1 && is.na(similarDropPclass))
			similarDropPclass <- rep(0, teObject$noClasses) # effectively set to no effect
		else # replicate to proper length if necessary
			similarDropPclass <- rep_len(similarDropPclass, length.out = teObject$noClasses)
		if(length(dissimilarDropPclass) == 1 && is.na(dissimilarDropPclass))
			dissimilarDropPclass <- rep(1, teObject$noClasses) # effectively set to no effect
		else # replicate to proper length if necessary
			dissimilarDropPclass <- rep_len(dissimilarDropPclass, length.out = teObject$noClasses)
		
		# apply thresholds for instances of each class
		validIdxs <- c()
		for (cl in 1:teObject$noClasses) {
			
			if (length(cleaningObject$nearestDistCl[[cl]])==1 && is.na(cleaningObject$nearestDistCl[[cl]]))
				noDist <- 0
			else 
				noDist <- length(cleaningObject$nearestDistCl[[cl]])
			
			# set the thresholds based on input parameters and the distribution of distances to nearest neighbors stored in teObject	
			if (similarDropPclass[cl]==0 || noDist==0)
				nearDistDrop <- 0
			else
				nearDistDrop <- cleaningObject$nearestDistCl[[cl]][max(1, round(similarDropPclass[cl] * noDist))]
			if (dissimilarDropPclass[cl] == 1 || noDist == 0) 
				farDistDrop=1
			else 
				farDistDrop <- cleaningObject$nearestDistCl[[cl]][min(noDist, round(dissimilarDropPclass[cl]*noDist))]
			
			# get index of class value
			classIdxs <- which(newdat[,ncol(newdat)]==teObject$class.lev[cl])
			if (length(classIdxs) == 0)
				next
			
			# compute distances for instances of given class value
			distMx <- gower.dist(data.x = teObject$dat[teObject$dat[,ncol(teObject$dat)]==teObject$class.lev[cl],-ncol(teObject$dat)], 
					data.y=newdat[classIdxs,-ncol(newdat)])
			# sort distances
			distances <- apply(distMx, 2, sort) # results are in columns
			if (!is.matrix(distances))
				distances<-as.matrix(distances)
			# prepare indices for selection of nearestInstK nearest neighbors
			firstNonZero <- apply(distances!=0, 2, function(x) which(x)[1])
			final <- firstNonZero + nearestInstK
			
			# compute the distances to the nearest neighbors
			distK <- c()
			for (i in 1:ncol(distances))
				if (is.na(firstNonZero[i]))
					distK[i] <- 0
				else
					distK[i] <- mean(distances[firstNonZero[i]:min(final[i], nrow(distances)), i])
			
			# select those between near and far threshold
			validNew <- distK >= nearDistDrop & distK <= farDistDrop
			validIdxs <- c(validIdxs, classIdxs[validNew])
		}
		# retain only those obeying the constraints
		newdat <- newdat[sort(validIdxs),]
	}
	
	if (nrow(newdat)==0)
		warning("cleanNewdata has eliminated all new instances - its parameters may be set in a wrong way.")
	
	# reassign response variable
	if (reassignResponse) {
		
		if (teObject$problemType != "density") {
			if (is.null(cleaningObject$postPredictor))  # train a predictor to assign new values
				cleaningObject$postPredictor <- rf(formula=teObject$formula, dataset=teObject$dat, 
						noTrees=max(100,teObject$noTrees), minNodeWeight=5, noSelectedAttr=0, densityData="no")
			
			if (teObject$problemType == "classification") 
				predictedResponse <- predict(cleaningObject$postPredictor, newdat, type="class")
			else 
				predictedResponse <- predict(cleaningObject$postPredictor, newdat)
			
			newdat[[teObject$predictorName]] <- predictedResponse
		}
	}
	
	return(list(cleanData = newdat, cleaningObject=cleaningObject))
}


