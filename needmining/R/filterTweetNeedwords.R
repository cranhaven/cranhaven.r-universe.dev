#' @title Filter tweets containing need indicating words
#' 
#' @description
#' \code{filterTweetsNeedwords} filters a list of Tweets regarding
#' need indicating words
#'
#' @details 
#' This function filters Tweets regarding a list of need indicating words
#' 
#' @param tweetMessages a dataframe containing the Tweet messages
#' @param needWords a string containing needwords separately by ';'
#'
#' @return a filtered data frame
#' 
#' @author Dorian Proksch <dorian.proksch@hhl.de>
#' 
#' @export
#'
#' @examples
#' data(NMTrainingData)
#' needWordsNeedsOnly <- "need;want;wish;feature;ask;would like;improve;idea;upgrade"
#' needsSimple <- filterTweetsNeedwords(NMTrainingData, needWordsNeedsOnly)
#' needWordsExtended <- "need;want;wish;feature;ask;would like;improve;idea;upgrade;
#'						support;problem;issue;help;fix;complain;fail"
#' needsSimpleExtended <- filterTweetsNeedwords(NMTrainingData, needWordsExtended)

filterTweetsNeedwords <- function (tweetMessages, needWords){

	if (missing(tweetMessages))
		stop("'tweetMessages' is missing.")

	if (missing(needWords))
		stop("'needWords' is missing. Nothing to filter")

	needwordList <- tolower(needWords)
	needwordList <- strsplit(needwordList, ";")
	needwordList <- unlist(needwordList)
	
	for (i in 1:length(needwordList)){
		needwordList[i] = trimws(needwordList[i])
	}
	
	tweetMessagesFiltered <- NULL
	
	for (j in 1:nrow(tweetMessages)){
		filteredOut <- 0
		
		for (k in 1:length(needwordList)){
			if (length(grep(needwordList[k], tolower(tweetMessages[j, "Tweets"]))) > 0){
				filteredOut <- 1
			}
		}
		
		if (filteredOut == 1){
			tweetMessagesFiltered <- rbind(tweetMessagesFiltered, c(tweetMessages[j, "Tweets"], 1))
		}
	}
	if (nrow(tweetMessagesFiltered > 0)){
		colnames(tweetMessagesFiltered) <- c("Tweets", "isNeed")	
	}
	
	return(tweetMessagesFiltered)
}