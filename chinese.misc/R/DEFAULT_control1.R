#' A Default Value for corp_or_dtm 1
#'
#' In the previous version, this list object is by default
#' used by \code{corp_or_dtm}. In this version, it is not the default value
#' but it can still be used by the user. See details in \code{\link{corp_or_dtm}}.
#'
#' The object specifies word length from 1 to 
#' 25. The second element, a tokenizer, is temporally deprecated.
#' Also, \code{DEFAULT_control2}
#' sets length from 2 to 25.
#'
#' @export
#' @examples
#' require(tm)
#' x <- c(
#'   "Hello, what do you want to drink?", 
#'   "drink a bottle of milk", 
#'   "drink a cup of coffee", 
#'   "drink some water")
#' dtm <- corp_or_dtm(x, from = "v", type = "dtm", control = DEFAULT_control1)
DEFAULT_control1<- list(wordLengths = c(1, 25), tokenizer = NLP::as.Token_Tokenizer(NLP::Regexp_Tokenizer("\\s", invert = TRUE)))
