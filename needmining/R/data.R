#' Training dataset regarding the user needs for smart speakers
#'
#' A dataset containing 200 artificially generated messages in
#' the Twitter format for the topic of smart speakers. These messages 
#' are inspired by real Tweets (rephrased, anonymized, all brand names
#' removed). 100 rows contain user needs, 100 rows
#' contain no user needs. The data is coded (0=no need,1=a need).
#' The data can be used to train a classification algorithm.
#'
#' @format A data frame with 200 rows and 2 variables:
#' \describe{
#'   \item{Tweets}{Contains the message}
#'   \item{isNeed}{Is a need described within the message? 0=no, 1=yes}
#' }
#' @usage data(NMTrainingData)
#' 
"NMTrainingData"
#' Test dataset regarding the user needs for smart speakers
#'
#' A dataset containing 200 artificially generated messages in
#' the Twitter format for the topic of smart speakers. These messages 
#' are inspired by real Tweets (rephrased, anonymized, all brand names
#' removed). Furthermore, Tweets containing stopwords were removed. 
#' 100 rows contain user needs, 100 rows
#' contain no user needs. The data is coded (0=no need,1=a need). It can 
#' be used to test a classification algorithm.
#'
#' @format A data frame with 200 rows and 2 variables:
#' \describe{
#'   \item{Tweets}{Contains the message}
#'   \item{isNeed}{Is a need described within the message? 0=no, 1=yes}
#' }
#' @usage data(NMdataToClassify)
#' 
"NMdataToClassify"