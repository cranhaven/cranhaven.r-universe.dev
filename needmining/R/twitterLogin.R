#' @title Login into Twitter API
#' 
#' @description
#' \code{twitterLogin} creates a token for the
#' Twitter API
#'
#' @details 
#' This function creates a Twitter token of the Twitter API. This is necessary to use functions of the Twitter API. The login data has to be stored 
#' in the 'TwitterLoginData.csv' in the current set working directory (please refer to getwd() and setwd()). The file should have the following format:
#' START
#' app;consumer_key;consumer_secret;access_token;access_secret LINEBREAK
#' The name of your app; your consumer_key; your consumer_secret; your access_token; your access_secret
#' END OF FILE
#' 
#' @return a Twitter token
#' 
#' @author Dorian Proksch <dorian.proksch@hhl.de>
#' 
#' @export
#'
#' @importFrom rtweet create_token
#' @importFrom utils read.csv
#'
#' @examples
#' \dontrun{
#' token <- twitterLogin()
#' }

twitterLogin <- function(){
	twitterLoginData <- read.csv(file="TwitterLoginData.csv", header=TRUE, sep=";")

	token <- create_token(
	  app = as.character(twitterLoginData[1,"app"]),
	  consumer_key = as.character(twitterLoginData[1,"consumer_key"]),
	  consumer_secret = as.character(twitterLoginData[1,"consumer_secret"]),
	  access_token = as.character(twitterLoginData[1,"access_token"]),
	  access_secret = as.character(twitterLoginData[1,"access_secret"]))

	return(token)
}