#' @title Remove Tweets containing stopwords
#' 
#' @description
#' \code{removeTweetsStopwords} removes Tweets containing stopwords
#'
#' @details 
#' This function removes Tweets containing stopwords from a list of Twitter messages.
#' 
#' @param tweetMessages a dataframe containing the Tweet messages
#' @param stopWords a string containing stopwords separated by ';'
#'
#' @return a filtered data frame
#' 
#' @author Dorian Proksch <dorian.proksch@hhl.de>
#' 
#' @export
#' 
#' @examples
#' stopWords <- "review;giveaway;save;deal;win;won;price;launch;news;gift;announce;
#'  			 reveal;sale;http;buy;bought;purchase;sell;sold;invest;discount;
#'				coupon;ship;giving away"
#' data(NMTrainingData)
#' filteredTweets <- removeTweetsStopwords(NMTrainingData, stopWords)

removeTweetsStopwords <- function(tweetMessages, stopWords){
		
	if (missing(tweetMessages))
		stop("'tweetMessages' is missing. Nothing to work on")

	if (missing(stopWords))
		stop("'stopWords' is missing. Nothing to filter")

	stopWords <- tolower(stopWords)
	stopwordList <- strsplit(stopWords, ";")
	stopwordList <- unlist(stopwordList)
	
	for (i in 1:length(stopwordList)){
		stopwordList[i] = trimws(stopwordList[i])
	}
	
	tweetMessagesFiltered <- NULL
	
	for (j in 1:nrow(tweetMessages)){
		filteredOut <- 0
		
		for (k in 1:length(stopwordList)){
			if (length(grep(stopwordList[k], tolower(tweetMessages[j, "Tweets"]))) > 0){
				filteredOut <- 1
			}
		}
		
		if (filteredOut == 0){
			tweetMessagesFiltered <- rbind(tweetMessagesFiltered, c(tweetMessages[j, "Tweets"], tweetMessages[j, "isNeed"]))
		}
	}
	if (nrow(tweetMessagesFiltered > 0)){
		colnames(tweetMessagesFiltered) <- c("Tweets", "isNeed")	
	}
	
	return(tweetMessagesFiltered)
}