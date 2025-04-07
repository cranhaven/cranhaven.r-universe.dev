#' @title Save Tweet file
#' 
#' @description
#' \code{saveNeedminingFile} saves a dataframe
#' created by the needmining package
#' to a file
#'
#' @details 
#' This function saves a dataframe created by the needmining package to a file
#' 
#' @param filename The filename to save to
#' @param tweetMessages An object containing the Twitter messages
#' 
#' @author Dorian Proksch <dorian.proksch@hhl.de>
#'
#' @importFrom utils write.table
#' 
#' @export
#'
#' @examples
#' data(NMTrainingData)
#' saveNeedminingFile(filename=file.path(tempdir(), "NMTrainingData.csv"),
#' NMTrainingData)

saveNeedminingFile <- function(filename, tweetMessages){

	if (missing(filename))
		stop("'filename' is missing")
		
	if (missing(tweetMessages))
		stop("'tweetMessages' is missing. Nothing to save")

	write.table(tweetMessages, file = filename, row.names=FALSE, sep=";", quote=FALSE)
 }