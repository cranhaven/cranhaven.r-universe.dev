\name{RemNewlineHalfspace}
\alias{RemNewlineHalfspace}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Removes new line characters and fixes half-spaces
}
\description{
Removes new line characters and fixes half-spaces in a string.
}
\usage{
RemNewlineHalfspace(texts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{texts}{A string which its new line characters and half-spaces should be removed or fixed.}
}
\value{
\code{RemNewlineHalfspace} returns a string with new line characters and half-spaces removed or fixed.
}
\author{
Safshekan, Nielsen
}
\examples{
# Create string with Persian string with new line characters and half-spaces 

x <- '\u062F\u0627\u0646\u0634\u06AF\u0627\u0647\u200C\u0647\u0627\u06CC
\u062A\u0647\u0631\u0627\u0646'

# Remove newline characters (eg.\n\r\t\f\v) and fix half-spaces
RemNewlineHalfspace(x) 
}