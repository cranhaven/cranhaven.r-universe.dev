\name{cs}
\alias{cs}
\title{
Change case in a text
}
\description{
A function to change letters to uppercase and lowercase in a given text.
}
\usage{
cs(xz, level = 1, case = 1, flgdf = FALSE, na.rm = FALSE)
}
\arguments{
  \item{xz}{
  input text
}
  \item{level}{
    optional clean level, either \code{0} for no-clean, default \code{1} to most strict \code{9} (see \code{\link{cln}})
}
  \item{case}{
  change case: \code{1} first letter uppercase and rest lowercase, \code{2} all letters lowercase, \code{3} all letters uppercase
}
  \item{flgdf}{
  is \code{xz} a data frame?
}
  \item{na.rm}{
   remove entries with NA data? (optional and logical)
}
}
\details{
This is a convenient function to change letters to uppercase and lowercase in a text for
argument \code{'case'} in function \code{\link{cln}}, and it inherits values from this function for  \code{'level'}, 
\code{'flgdf'}, and \code{'na.rm'}.
}
\value{
Input text with defined case.
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

\seealso{
\code{\link{cln}}
}
\examples{
cs("Caesar?*+", level=2, case=3)
}
\keyword{ manip }
\keyword{ utilities }