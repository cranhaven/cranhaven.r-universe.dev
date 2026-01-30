#########1#########2#########3#########4#########5#########6#########7#########8
#' Create a DFSource object from a data frame
#'
#' This function will create a DFSource object from a data frame that contains
#' at least columns id and text, but may contain several more. VCorpus will
#' use this to read in each row from the data frame into a PlainTextDocument,
#' storing additional variables in its metadata. It will then combine all those 
#' PlainTextDocuments in a VCorpus object.
#'
#' @param x A dataframe with at a minimum a text and id column,
#' and a row for each document to be stored in a corpus.
#' @return A DFSource object containing the encoding set to "", the number
#' of rows (length), the current position (position=0), the type of
#' reader to use (reader=readDF), and the content (\code{x}).
#' @examples
#' (df=data.frame(id=1:3,text=c("First text","Second text","Third text"),
#'                title=c("N1","N2","N3")))
#' DFSource(df)
#' @import tm
#' @export
################################################################################
DFSource<-function (x)
{
  if (!inherits(x,"data.frame")) stop("x must be a data frame type object")
  stopifnot(all(!is.na(match(c("id", "text"),names(x)))))
  SimpleSource(length = nrow(x), reader = readDF, content = x,
               class = "DFSource")
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Create a PlainTextDocument from a row in a data frame
#'
#' Read a row of the content of a DFSource object into a PlainTextDocument.
#'
#' @param elem A list containing the field content containing one
#' row with data from a data frame containing at least the columns id and text,
#' but possibly more.
#' @param language abbreviation of the language used; "en" for English
#' @param id Not used, but needed for VCorpus
#' @return A PlainTextDocument with content equal to the contents of the
#' text field, and meta data containing the information in the remaining
#' fields, including the id field
#' @examples
#' (df=data.frame(id=1:3,text=c("First text","Second text","Third text"),
#'                title=c("N1","N2","N3")))
#' readDF(list(content=df[1,]),"en")
#' @import tm
#' @export
################################################################################
readDF<-function(elem, language, id="1") {
  x=elem$content
  nam=names(x)[!(names(x) %in% c("text","id"))]
  if (length(nam)==0) {
    tx='PlainTextDocument(x[, "text"],id = x[,"id"],language = "en")'
  } else {
    tx=paste('PlainTextDocument(x[, "text"], id = x[,"id"]',
             paste0(', ', nam, ' = x[,"',nam,'"]',collapse=""),
             ', language = "en")')
  }
  eval(parse(text=tx))
}
#########1#########2#########3#########4#########5#########6#########7#########8
#' Obtain the current row of the content of a DFSource
#'
#' Using the position field of x to indicate the index of the current row, we
#' retrieve the current row of the content of a DFSource. This function is
#' mainly used by the VCorpus function.
#'
#' @param x A DFSource object
#' @return A list with the current row in the content of a DFSource object.
#' The current row index is the position in the DFSource object.
#' @examples
#' library(tm)
#' df=data.frame(id=1:3,text=c("First text","Second text","Third text"),
#'               title=c("N1","N2","N3"))
#' getElem(stepNext(DFSource(df)))
#' @import tm
#' @method getElem DFSource
#' @export
################################################################################
getElem.DFSource<-function (x) {
  list(content = x$content[x$position,])
}
