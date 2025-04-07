#' @title Classify needs based on machine learning
#' 
#' @description
#' \code{filterTweetsMachineLearning} classifies a list of Tweets as
#' needs based on the random forest machine learning algorithm
#'
#' @details 
#' This function uses a machine learning algorithm (random forest) to
#' classify needs based on their content. It needs a training data set
#' with classified needs (indicated by 0=not a need, 1=a need).
#' This function used code fragments from the archived R packages 
#' maxent and RTextTools. The authors are Timothy P. Jurka, Yoshimasa Tsuruoka, 
#' Loren Collingwood, Amber E. Boydstun, Emiliano Grossman, Wouter van Atteveldt
#'
#' @param dataToClassify a dataframe containing the Tweet messages to classify
#' @param trainingData a dataframe containing Tweets messages with a given classification (0=not a need, 1=a need)
#'
#' @return a dataframe with classified data
#' 
#' @author Dorian Proksch <dorian.proksch@hhl.de>
#' 
#' @importFrom methods new setClass
#' @importClassesFrom SparseM matrix.csr
#' @importFrom SparseM as.matrix.csr as.matrix
#' @importFrom tm Corpus DocumentTermMatrix VectorSource scan_tokenizer removeSparseTerms
#' @importFrom stats predict
#' @importFrom tau textcnt
#' @importFrom randomForest randomForest
#' @importFrom SnowballC wordStem
#'
#' @export
#' 
#' @examples
#' data(NMTrainingData)
#' data(NMdataToClassify)
#' smallNMTrainingData <- rbind(NMTrainingData[1:75,], NMTrainingData[101:175,])
#' smallNMdataToClassify <- rbind(NMdataToClassify[1:10,], NMdataToClassify[101:110,])
#' results <- filterTweetsMachineLearning(smallNMdataToClassify, smallNMTrainingData)
#'


filterTweetsMachineLearning <- function (dataToClassify, trainingData){

	# Code from archived maxnet package
	as.compressed.matrix <- function(DocumentTermMatrix) {
		if (pmatch("matrix.csr",class(DocumentTermMatrix),nomatch=0) > 0) { return(DocumentTermMatrix); }	
		else if (pmatch("TermDocumentMatrix",class(DocumentTermMatrix),nomatch=0) > 0) { DocumentTermMatrix <- t(DocumentTermMatrix); }
		else if (pmatch("DocumentTermMatrix",class(DocumentTermMatrix),nomatch=0) > 0) { flag <- TRUE; }
		else if (pmatch("Matrix",attr(class(DocumentTermMatrix),"package"),nomatch=0) > 0) { return(as.matrix.csr(DocumentTermMatrix)); }
		else if (pmatch("data.frame",class(DocumentTermMatrix),nomatch=0) > 0) { return(as.matrix.csr(as.matrix(DocumentTermMatrix))); }
		else if (pmatch("matrix",class(DocumentTermMatrix),nomatch=0) > 0) { return(as.matrix.csr(DocumentTermMatrix)); }
		else {
		tryCatch(return(as.matrix.csr(as.matrix(DocumentTermMatrix))),error=function(e) stop("Data must be encapsulated using one of the following classes: DocumentTermMatrix or TermDocumentMatrix (package tm), Matrix (package Matrix), matrix.csr (SparseM), data.frame, or matrix"));
		}
		
		ia <- c(1);
		for (n in 1:dim(DocumentTermMatrix)[1]) {
			el <- sum(DocumentTermMatrix$i == n)+ia[length(ia)];
			ia <- append(ia,el);
		}
		
		matrix <- new("matrix.csr",ra=as.numeric(DocumentTermMatrix$v),ja=DocumentTermMatrix$j,ia=as.integer(ia),dimension=dim(DocumentTermMatrix));
		
		return(matrix);
	}


	# Code from archive RTextTools package
	create_container <- function(matrix,labels,trainSize=NULL,testSize=NULL,virgin) {
		if (is.null(trainSize) && is.null(testSize)) stop("You must specify either a trainSize or testSize parameter, or both.")
		if (is.null(trainSize)) trainSize <- testSize
		if (is.null(testSize)) testSize <- trainSize

		totalSize <- sort(unique(append(trainSize,testSize)))
		column_names <- colnames(matrix)
		data_matrix <- as.compressed.matrix(matrix[totalSize,])
		

		matrix_train_predict <- as.compressed.matrix(matrix[trainSize,])
		matrix_test_predict <- as.compressed.matrix(matrix[testSize,])

		train_code <- as.factor(labels[trainSize])
		if (length(unique(is.na(train_code))) > 1) stop("All data in the training set must have corresponding codes.")
		
		test_code <- as.factor(labels[testSize])
		if (virgin == FALSE && length(unique(is.na(test_code))) > 1) stop("The data to be classified does not have corresponding codes. To treat this data set as virgin data, set virgin=TRUE.")
		
		container <- new("matrix_container", training_matrix=matrix_train_predict, classification_matrix=matrix_test_predict, training_codes=train_code, testing_codes=test_code, column_names=column_names, virgin=virgin)
		
		gc()
		return(container)
	}


	if (missing(dataToClassify))
		stop("'dataToClassify' is missing.")

	if (missing(trainingData))
		stop("'trainingData' is missing.")

	merged_data <- as.matrix(rbind(trainingData, dataToClassify))

	# Code adapted from archived RTextTools package
	

	# Parameters for TermDocumentMatrix
	textColumns <- merged_data[,"Tweets"]
	language="english";
	minDocFreq=1;
	maxDocFreq=Inf;
	minWordLength=3;
	maxWordLength=Inf;
	ngramLength=1;
	removeNumbers=TRUE;
	removePunctuation=TRUE;
	removeSparseTerms=0.998;
	removeStopwords=TRUE;
	stemWords=TRUE;
	stripWhitespace=TRUE;
	toLower=TRUE;
	weighting=0
	
    stem_words <- function(x) {
        split <- strsplit(x," ")
        return(wordStem(unlist(split),language=language))
    }
    
    tokenize_ngrams <- function(x, n=ngramLength) return(rownames(as.data.frame(unclass(textcnt(x,method="string",n=n)))))
	
	control <- list(bounds=list(local=c(minDocFreq,maxDocFreq)),language=language,tolower=toLower,removeNumbers=removeNumbers,removePunctuation=removePunctuation,stopwords=removeStopwords,stripWhitespace=stripWhitespace,wordLengths=c(minWordLength,maxWordLength),weighting=weighting)
        
    if (ngramLength > 1) { 
    	control <- append(control,list(tokenize=tokenize_ngrams),after=7)
    } else {
    	control <- append(control,list(tokenize=scan_tokenizer),after=4)
    }
    
    if (stemWords == TRUE && ngramLength == 1) control <- append(control,list(stemming=stem_words),after=7)
    
    trainingColumn <- apply(as.matrix(textColumns),1,paste,collapse=" ")
    trainingColumn <- sapply(as.vector(trainingColumn,mode="character"),iconv,to="UTF8",sub="byte")

	corpus <- Corpus(VectorSource(trainingColumn),readerControl=list(language=language))
	matrix <- DocumentTermMatrix(corpus,control=control);
    if (removeSparseTerms > 0) matrix <- removeSparseTerms(matrix,removeSparseTerms)
	

	matrix <- matrix[,sort(colnames(matrix))]

	doc_matrix <- matrix

	
	container <- create_container(doc_matrix, merged_data[, "isNeed"], trainSize=1:nrow(trainingData), 
	testSize=(nrow(trainingData)+1):(nrow(trainingData)+nrow(dataToClassify)), virgin=FALSE)
		
	
	# Train randomForest
	RF <- randomForest(x=as.matrix(container@training_matrix), y=container@training_codes, ntree=200)
		
	# Classify data	
	extract_maximum_prob <- function(x) return(x[which.max(x)])
    extract_label_from_prob <- function(x) return(which.max(x))
	extract_label_from_prob_names <- function(x) return(rownames(as.matrix(which.max(x))))   
  
    rf_results <- predict(RF,newdata=as.matrix(container@classification_matrix),type="prob")
	rf_pred <- apply(rf_results,1,extract_label_from_prob_names)
    rf_prob <- apply(rf_results,1,extract_maximum_prob)

    RF_CLASSIFY <- data.frame(as.character(rf_pred),rf_prob)
    colnames(RF_CLASSIFY)[1] <- "FORESTS_LABEL"
    colnames(RF_CLASSIFY)[2] <- "FORESTS_PROB"
	
	classification <- as.integer(as.vector(RF_CLASSIFY$FORESTS_LABEL))

	final_results <- cbind(dataToClassify[,"Tweets"], classification)
	colnames(final_results) <- c("Tweets", "isNeed")
	final_results <- as.matrix(final_results)

	return(final_results)	
}