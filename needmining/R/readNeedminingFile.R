#' @title Read Tweet file
#' 
#' @description
#' \code{readNeedminingFile} reads a Needmining file 
#' created by the needmining package
#'
#' @details 
#' This function reads a Needmining file created by the needmining package
#' 
#' @param filename The filename of the file to read
#'
#' @return a data frame containing the content of the file
#' 
#' @author Dorian Proksch <dorian.proksch@hhl.de>
#'
#' @importFrom utils read.csv
#' 
#' @export
#'
#' @examples
#' data(NMTrainingData)
#' saveNeedminingFile(filename=file.path(tempdir(), "NMTrainingData.csv"),
#' NMTrainingData)
#' currentNeedData <- readNeedminingFile(file.path(tempdir(), "NMTrainingData.csv"))

readNeedminingFile <- function(filename){
 
	if (missing(filename))
		stop("'filename' is missing")
 
	tweetFile <- as.matrix(read.csv(file=filename, header=TRUE, sep=";", quote = "", stringsAsFactors = FALSE))
	colnames(tweetFile) <- c("Tweets", "isNeed")

	return(tweetFile)
}