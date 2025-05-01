#' Find High Frequency Terms
#'
#' By inputting a matrix, or a document term matrix, or term document matrix, this function counts
#' the sum of each term and output top n terms. The result can be messaged on the screen, so 
#' that you can manually copy them to other places (e. g., Excel).
#'
#' Sometimes you may pick more terms than specified by \code{top}. For example, you specify to 
#' pick up the top 5 terms, and the frequency of the 5th term is 20. But in fact there are 
#' two more terms that 
#' have frequency of 20. As a result, \code{sort_tf} may pick up 7 terms. If you want the 
#' number is exactly 5, set \code{must_exact} to \code{TRUE}.
#'
#' @param x a matrix, or an object created by \code{\link{corp_or_dtm}} or 
#' by \code{tm::DocumentTermMatrix}, or \code{tm::TermDocumentMatrix}.
#' Data frame is not allowed. If it is a matrix, the column names (if \code{type} is "dtm") 
#' or row names (if \code{type} is "tdm") is taken to be terms, see below. If the names 
#' are \code{NULL}, terms are set to "term1", "term2", "term3"...automatically.
#' @param top a length 1 integer. As terms are in the decreasing 
#' order of the term frequency, this argument decides how many top terms should be returned.
#' The default is 10. If the number of terms is smaller than \code{top}, all terms are returned.
#' Sometimes the returned terms are more than \code{top}, see below.
#' @param type should start with "D/d" representing document term matrix, 
#' or "T/t" representing term document matrix.
#' It is only used when \code{x} is a matrix. The default is "dtm".
#' @param todf should be \code{TRUE} or \code{FALSE}. If it is \code{FALSE} (default) 
#' terms and their frequencies will be pasted by "&" and messaged on the screen, nothing is 
#' returned. Otherwise, terms and frequencies will be returned as data frame.
#' @param must_exact should be \code{TRUE} or \code{FALSE} (default). It decides whether 
#' the number of returned words should be equal to that specified by \code{top}. See Details.
#'
#' @return return nothing and message the result, or return a data frame.
#'
#' @export
#' @examples
#' require(tm)
#' x <- c(
#'   "Hello, what do you want to drink?", 
#'   "drink a bottle of milk", 
#'   "drink a cup of coffee", 
#'   "drink some water", 
#'   "hello, drink a cup of coffee")
#' dtm <- corp_or_dtm(x, from = "v", type = "dtm")
#' # Argument top is 5, but more than 5 terms are returned
#' sort_tf(dtm, top = 5)
#' # Set must_exact to TRUE, return exactly 5 terms
#' sort_tf(dtm, top=5, must_exact=TRUE)
#' # Input is a matrix and terms are not specified
#' m=as.matrix(dtm)
#' colnames(m)=NULL
#' mt=t(m)
#' sort_tf(mt, top=5, type="tdm")
sort_tf <- function(x, top = 10, type = "dtm", todf = FALSE, must_exact = FALSE) {
  infolocale <- localestart2()
  on.exit(localeend2(infolocale))
  top <- as.integer(top[1])
  if (!top > 0) 
    stop("Argument top should be a length 1 integer larger than zero.")
  if (!must_exact %in% c(TRUE, FALSE)) 
    stop("must_exact must be TRUE or FALSE.")
  if (class(x)[1] %in% c("matrix")) {
    stopifnot(grepl("^d|^D|^t|^T", type))
    if (grepl("^d|^D", type)) 
      inner_type <- 3
    if (grepl("^t|^T", type)) 
      inner_type <- 4
    check_num <- apply(x, 2, is.numeric)
    if (!all(check_num)) 
      stop("Please make every column of x is numeric.")
  }
  else if ("TermDocumentMatrix" %in% class(x)) {
    inner_type <- 2
  }
  else if ("DocumentTermMatrix" %in% class(x)) {
    inner_type <- 1
  }
  else {
    stop("Class of x should be matrix, DocumentTermMatrix or TermDocumentMatrix.")
  }
  if (inner_type == 4) {
    freq <- rowSums(x)
    word <- rownames(x)
    if (is.null(word)) 
      word <- paste("term", 1:nrow(x), sep = "")
  }
  if (inner_type == 3) {
    freq <- colSums(x)
	word <- colnames(x)
    if (is.null(word)) 
      word <- paste("term", 1:ncol(x), sep = "")
  }
  if (inner_type == 1) {
    freq <- slam::col_sums(x)
	word <- tm::Terms(x)
  } 
  if (inner_type == 2) {
    freq <- slam::row_sums(x)
	word <- tm::Terms(x)
  }
  o <- order(freq, decreasing = TRUE)
  freq <- freq[o]
  word <- word[o]
  nw <- length(word)
  message("In total, you have ", nw, " words.")
  nw <- min(nw, top)
  df_all <- data.frame(word, freq, stringsAsFactors = FALSE)
  df <- df_all[1:nw, ]
  last_freq <- freq[nw]
  extra_pos <- which(freq == last_freq)
  rm(freq, word)
  sub_df <- df_all[extra_pos, ]
  df <- rbind(df, sub_df)
  rm(sub_df)
  df <- unique(df)
  if (must_exact) {
    df <- df[1:nw, ]
  }
  if (todf == TRUE) {
    rownames(df) <- 1:nrow(df)
	df[,1] <- enc2utf8(df[,1])
    return(df)
  }
  else {
    to_paste <- paste(df[, 1], df[, 2], sep = "&")
    for (i in to_paste) cat(i, "\n")
  }
}
