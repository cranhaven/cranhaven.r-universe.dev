\name{cln}
\alias{cln}
\title{
Clean and re-encode a vector, list or dataframe
}
\description{
A function to re-encode Greek (and other) characters and to remove symbols.
}
\usage{
cln(x, level, what, na.rm, case, repl, unlist)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    a vector, list or dataframe
}
  \item{level}{
    optional clean level, either \code{0} for no-clean, default \code{1} to most strict \code{9} (see details)
}
  \item{what}{
    additional characters to clean (optional)
}
  \item{na.rm}{
   remove entries with NA data? (optional and logical)
}
  \item{case}{
   case for text \code{1} for 1st uppercase, code{2} for lowercase, code{3} for uppercase (optional)
}
  \item{repl}{
   data frame with text to replace (optional)
}
  \item{unlist}{
   return a vector? (optional and logical, for vector input)
}
}
\details{
This function is meant to re-encode Greek (and other) characters in the \code{EDH} set given either as list format, 
vector, or a dataframe produced with function \code{\link{edhw}} for example. 

By default, the symbols \code{"?" "*" "+"} placed at the end of each record are removed after the re-encoding. 
However, when level is \code{0} only re-encoding is performed, and level \code{2} is either to force an extra iteration 
in the re-encoding, to remove extra spaces, or what is in \code{what} at the end of a record when clean what is invoked. 
With level \code{9} all content after an opening parenthesis is removed with all the consequences for the input text. 

With \code{repl}, is possible to replace a list of text in two columns, for `text to replace' and for `text that replaces'. 

Disabling option \code{unlist} returns a vector in case that \code{x} is also a vector; otherwise, it returns a list with the two 
versions of the input.
}
\value{
Depending on the input, a vector, list or dataframe.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Antonio Rivero Ostoic
}
%\note{
%%  ~~further notes~~
%}

\section{Warning }{
Encoding more than once the same input requires re-starting the console; otherwise, the re-encoding is not complete.
}

\seealso{
\code{\link{edhw}}, \code{\link{get.edh}}, \code{\link{edhwpd}}, \code{\link{cs}}
}
\examples{
# clean Greek characters
cln("Caesar?*+")
}
\keyword{ utilities }
\keyword{ manip }
