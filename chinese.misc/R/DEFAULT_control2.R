#' A Default Value for corp_or_dtm 2
#'
#' The object specifies word length from 2 to 
#' 25. The second element, a tokenizer, is temporally deprecated.
#' Also, \code{DEFAULT_control1}
#' sets length from 1 to 25.
#'
#' @export
#' @examples
#' require(tm)
#' x <- c(
#'   "Hello, what do you want to drink?", 
#'   "drink a bottle of milk", 
#'   "drink a cup of coffee", 
#'   "drink some water")
#' dtm <- corp_or_dtm(x, from = "v", type = "dtm", control = DEFAULT_control2)
DEFAULT_control2<- list(wordLengths = c(2, 25), tokenizer = NLP::as.Token_Tokenizer(NLP::Regexp_Tokenizer("\\s", invert = TRUE)))
