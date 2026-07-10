
##' Create a Chinese term-document matrix or a document-term matrix.
##' 
##' Package "tm" is required.
##' @title Create a Chinese term-document matrix or a document-term matrix.
##' @aliases createTDM
##' @usage
##' createDTM(string, language = c("zh", "en"), tokenize = NULL, removePunctuation = TRUE, 
##'   removeNumbers = TRUE, removeStopwords = TRUE)
##' createTDM(string, language = c("zh", "en"), tokenize = NULL, removePunctuation = TRUE, 
##'   removeNumbers = TRUE, removeStopwords = TRUE)
##' @param string A character vector.
##' @param language The language type, 'zh' means Chinese.
##' @param tokenize A tokenizers function. 
##' @param removePunctuation Whether to remove the punctuations.
##' @param removeNumbers Whether to remove the numbers.
##' @param removeStopwords Whether to remove the stop words.
##' @return An object of class \code{TermDocumentMatrix} or class \code{DocumentTermMatrix}.
##' @author Jian Li <\email{rweibo@@sina.com}>
##' @keywords NLP
##'

createDTM <- function(string, language = c("zh", "en"), tokenize = NULL, 
		removePunctuation = TRUE, removeNumbers = TRUE, removeStopwords = TRUE) {
	
	if (suppressWarnings(!requireNamespace("tm", quietly = TRUE))) {
		stop("Package \"tm\" is required!")
	}
	language <- match.arg(language)
	
	#corpus_m <- tm::Corpus(tm::VectorSource(string), encoding = "UTF-8")
	corpus_m <- tm::VCorpus(tm::VectorSource(string), readerControl = list(language = language))
	
	if (removeStopwords) {
		corpus_m <- tm::tm_map(corpus_m, tm::removeWords, stopwordsCN())
	}
	
	if (is.null(tokenize)) tokenize <- .strsplit_space_tokenizer
	dtm_m <- tm::DocumentTermMatrix(corpus_m, 
			control = list(tokenize = tokenize, wordLengths = c(1, Inf), 
					removePunctuation = removePunctuation, removeNumbers = removeNumbers) )
	#colnames(dtm_m) <- toUTF8(colnames(dtm_m))
	return(dtm_m)
	
}

createTDM <- function(string, language = c("zh", "en"), tokenize = NULL, 
		removePunctuation = TRUE, removeNumbers = TRUE, removeStopwords = TRUE) {
	
	if (suppressWarnings(!requireNamespace("tm", quietly = TRUE))) {
		stop("Package \"tm\" is required!")
	}
	
	corpus_m <- tm::VCorpus(tm::VectorSource(string), readerControl = list(language = language))
	if (removeStopwords) {
		corpus_m <- tm::tm_map(corpus_m, tm::removeWords, c(stopwordsCN()))
	}
	
	if (is.null(tokenize)) tokenize <- .strsplit_space_tokenizer
	tdm_m <- tm::TermDocumentMatrix(corpus_m, 
			control = list(tokenize = tokenize, wordLengths = c(1, Inf), 
					removePunctuation = removePunctuation, removeNumbers = removeNumbers) )
	#rownames(tdm_m) <- toUTF8(rownames(tdm_m))
	return(tdm_m)
	
}
