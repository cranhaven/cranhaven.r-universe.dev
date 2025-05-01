#' Input a Filename and Return a Vector of Stop Words
#'
#' When a filename is provided, the function will return a vector of terms. If nothing is provided, 
#' it will return the stop words used in package \code{jiebaR}. See Details.
#'
#' In a valid text file that saves stop words, each word should occupy a single line. However, 
#' if any line that contains more than one word and these words are separated by blanks, 
#' punctuations, numbers, it is also accepted, for the function will try to split them.
#' Duplicated words will also be automatically removed.
#' The encoding of a stop words file is auto-detected by the function.
#'
#' For stop word list from \code{jiebaR}, see \code{jiebaR::STOPPATH}.  It contains 
#' many words that are often removed in analyzing Chinese text.
#' However, the result returned by \code{make_stoplist} is slightly different.
#'
#' @param x a length 1 character specifying a valid stop word file. 
#' If it is not provided,  or 
#' is "jiebar" (default), "jiebaR" or "auto", it will return part of the stop words used by package 
#' \code{jiebaR}.
#' See Details.
#' @param print \code{TRUE} or \code{FALSE}, whether to print the first 5 words
# after successfully reading stop word file. The default is \code{TRUE}.
#'
#' @return a character vector of words. If no word is obtained, it will return \code{NULL}. 
#'
#' @export
make_stoplist <-
function(x = "jiebar", print = TRUE) {
  infolocale <- localestart2()
  on.exit(localeend2(infolocale))
  if (length(x) > 1) {
    x <- x[1]
    message("x has length > 1, only the 1st is used.")
  }
  x <- whetherencode(x)
  if (!identical(x, "jiebar") & !identical(x, "auto") & !identical(x, "jiebaR")){
    if (!(file.exists(x) & ! dir.exists(x)))
      stop('x must be a valid filename.')
  } 
  if (x == "jiebar" | x == "auto" | x == "jiebaR") {
    ST <- readLines(jiebaR::STOPPATH, encoding = "UTF-8")
    # ST <- readLines(jiebaR::STOPPATH, encoding = Ruchardet::detectFileEncoding(jiebaR::STOPPATH))	
    ST <- ST[-c(1:127, 137, 148:155, 878:882, 1180:1206, 1359, 1526:1534)]
	ST <- ST[!grepl("[a-zA-Z]", ST)]
	ST <- whetherencode(ST)
  }
  else {
    the_enc <- gEtthEEnc(x1 = x, x2 = "auto")
	ST <- scan(x, what = "character", quiet = TRUE, sep = "\n", fileEncoding = the_enc)
	if (the_enc != "UTF-8") ST <- stringi::stri_encode(ST, to="UTF-8")
	ST <- gsub("[[:cntrl:]]\\d", "", ST)
	ST <- gsub("\\n|\\r", " ", ST)	
  }
  ST <- ST[!is.na(ST) & ST != ""]
  ST <- unlist(strsplit(ST, "\\\\n|\\\\t|\\\\r"))
  ST <- unlist(strsplit(ST, "[[:blank:]]+|[[:space:]]+|[[:punct:]]+|\\d+"))
  ST <- ST[ST != "" & ST != "NA"]
  ST <- unique(ST)
  if (length(ST) == 0){
    return(NULL)
  } else {
    if (print == TRUE){
      h <- utils::head(ST, 5)
      cat("Show the first 5 stop words: \n")
      for (i in h)
	    cat(i, "\n")
	}
    return(ST)
  }
}
