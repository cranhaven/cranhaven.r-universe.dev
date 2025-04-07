#' @title Downloading Tweets based on a keyword list
#' 
#' @description
#' \code{downloadTweets} downloads Tweets containing specified keywords from the Twitter API
#'
#' @details 
#' This function downloads Tweets for a specified keyword list, removes line breaks, adds a column isNeed filled with 0
#' 
#' @param search_terms a string containing the search terms in Twitter format (use OR and AND to connect multiple search terms in one search)
#' @param n The number of Tweets downloaded. Please note that this limit is based on your Twitter account
#' @param lang The language of the Tweets. Default is English. Please refer to the Twitter API documentation for language codes
#'
#' @return a data frame containing the tweets as well as an additional column isNeed filled with 0
#' 
#' @author Dorian Proksch <dorian.proksch@hhl.de>
#'
#' @importFrom rtweet search_tweets
#' @importFrom stringr str_replace_all
#' 
#' @export
#'
#' @examples
#' searchterm <- '"smart speaker" OR "homepod" OR "google home mini"'
#' \dontrun{
#' token <- twitterLogin()
#' currentTweets <- downloadTweets(searchterm, n = 180)
#' }

downloadTweets <- function(search_terms, n=100, lang = "en"){

	if (missing(search_terms))
		stop("'search_term' is missing")

	rtweetObject <- search_tweets(search_terms, n, include_rts = FALSE, lang = "en", verbose=TRUE)
	tweetMessages <- rtweetObject$text;
	tweetMessages <- str_replace_all(tweetMessages, "[\r\n]" , " ")
	tweetMessages <- str_replace_all(tweetMessages, "[;]" , " ")
	
	tweetFile <- as.matrix(tweetMessages)	
	tweetFile <- cbind(tweetFile, 0)
	colnames(tweetFile) <- c("Tweets", "isNeed")
	
	return(tweetFile)
 }