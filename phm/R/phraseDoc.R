#########1#########2#########3#########4#########5#########6#########7#########8
#' phraseDoc Creation
#'
#' Create an object of class phraseDoc. This will hold all principal phrases of
#' a collection of texts that occur a minimum number of times, plus the texts
#' they occur in and their position within those texts.
#'
#' @param text a character vector with each element the text of a document, or 
#' a corpus
#' @param ids a character vector with identifiers for each text
#' @param mn Minimum number of words in a phrase.
#' @param mx Maximum number of words in a phrase.
#' @param ssw A set of words no phrase should start with.
#' @param sew A set of words no phrase should end with.
#' @param sp A set of phrases to be excluded.
#' @param min.freq The minimum frequency of phrases to be included.
#' @param max.phrases Maximum number of phrases to be included.
#' @param shiny TRUE if called from a shiny program. This will allow progress
#' to be recorded on a progress meter; the function uses about 100 progress
#' steps, so it should be created inside a withProgress function with the 
#' argument max set to at least 100.
#' @param silent TRUE if you do not want progress messages.
#' @return Object of class phraseDoc
#' @examples
#' tst=c("This is a test text",
#'       "This is a test text 2",
#'       "This is another test text",
#'       "This is another test text 2",
#'       "This girl will test text that man",
#'       "This boy will test text that man")
#' phraseDoc(tst)
#' @import data.table
#' @importFrom smallstuff isInt
#' @importFrom utils object.size
#' @export
################################################################################
phraseDoc<-function(text,ids=NULL,mn=2,mx=8,ssw=stopStartWords(),
                    sew=stopEndWords(),sp=stopPhrases(),min.freq=2,
                    max.phrases=1500,shiny=FALSE,silent=TRUE) {
  #Error checking the input arguments
  if (inherits(text,"Corpus")) {
    ids=names(text)
    text=unlist(lapply(text,NLP::content))
  } 
  if (!inherits(text,"character")) {
    stop("text must be a character vector or a VCorpus")
  }
  if (!isInt(mn)) stop("mn must be an integer")
  if (!isInt(mx)) stop("mx must be an integer")
  if (!is.null(ids) && !inherits(ids,"character")) stop("ids must be character")
  if (!inherits(ssw,"character")) stop("ssw must be character")
  if (!inherits(sew,"character")) stop("sew must be character")
  if (!inherits(sp,"character")) stop("sp must be character")
  if (!isInt(min.freq)) stop("min.freq must be an integer")
  if (!isInt(max.phrases)) stop("max.phrases must be an integer")
  if (!inherits(shiny,"logical")) stop("shiny must be logical")
  if (!inherits(silent,"logical")) stop("silent must be logical")
  
  sz_kb <- as.numeric(object.size(text) / 1024)
  x=PhraseDoc_cpp(text,mn,mx,ssw,sew,sp,min.freq,max.phrases,shiny,silent,sz_kb)

  if (is.null(ids)||length(ids)!=length(text)) ids=1:length(text)
  x$docs=ids
  class(x) <- "phraseDoc"
  x
}
