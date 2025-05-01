#' Convert or Write DTM/TDM Object Quickly
#'
#' Given a TermDocumentMatrix or DocumentTermMatrix object, the function converts it 
#' to a matrix or write it into a .csv file, with additional filenames attached to it.
#'
#' @param x an object created by \code{tm::TermDocumentMatrix} 
#' or \code{tm::DocumentTermMatrix}.
#' @param outputfile when it is NULL (default), no file is written and a matrix is returned. 
#' When a filename is provided, it will write the matrix into a file. The filename must end 
#' with ".csv".
#' @param doc_name whether \code{NULL} or a character vector specifying the 
#' names you want to give to texts. If it is not a character vector, the function will try to coerce. 
#' Then the names become the row names of the returned matrix. 
#' Double inversed slashes will be converted to "/" by the function. The 
#' length of the argument must be equal to the number of files. \code{NA} element is not allowed.
#' By default it is \code{NULL}, which means no name is added.
#'
#' @export
#' @examples
#' require(tm)
#' x <- c(
#'   "Hello, what do you want to drink?", 
#'   "drink a bottle of milk", 
#'   "drink a cup of coffee", 
#'   "drink some water")
#' dtm <- corp_or_dtm(x, from = "v", type = "dtm")
#' output_dtm(dtm, doc_name = paste("doc", 1:4))
output_dtm <-
function(x, outputfile = NULL, doc_name = NULL) {
  infolocale <- localestart2()
  on.exit(localeend2(infolocale))
  if (!"simple_triplet_matrix" %in% class(x)) 
    stop("Argument x must be a dtm or a tdm object.")
  if (!is.null(outputfile))
      stopifnot(is.character(outputfile) && grepl("\\.csv$", outputfile[1]))
  if (!is.null(doc_name)) {
    if (any(is.na(doc_name))) 
      stop("doc_name should not contain NA.")
    doc_name <- as.character2(doc_name)
    doc_name <- gsub("\\\\", "/", doc_name)
    if (length(doc_name) != tm::nDocs(x)) 
      stop("Num of doc_name differs from num of files.")
    x$dimnames$Docs <- doc_name
  }
  myfreq <- as.matrix(x)
  if (is.null(outputfile)) {
    return(myfreq)
  }
  else {
    utils::write.csv(myfreq, outputfile[1])
  }
}
