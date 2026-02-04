#' List Ignored Words
#'
#' @family Simplified Process
#'
#' @param wordsList List of words to be ignored when grouping user stories
#'     semantically by similarities of word.
#' @param addToExisting If this param is TRUE, passed words will be added to a
#'     predefined set of words. Otherwise, only those in the previous argument
#'     will be ignored.
#'
#' @return Returns an array of words that will be ignored when processing the
#'     semantic groups.
#'
#' @export
#'
#' @examples
#' # Generating default words only
#' createIgnoreWords()
#'
#' # Adding words
#' createIgnoreWords(c("given", "said"))
#'
#' # Replacing words
#' createIgnoreWords(c("given", "said"), FALSE)
#'
createIgnoreWords <- function(wordsList = c(), addToExisting = TRUE ) {
  # Create the list
  wlist <- ignoreWords

  if(addToExisting)
    wlist <- c(wlist, wordsList)
  else
    wlist <- wordsList

  # Return the dataset
  return(data.frame(word = wlist))
}
