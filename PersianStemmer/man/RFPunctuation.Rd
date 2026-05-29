\name{RFPunctuation}
\alias{RFPunctuation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove or fix punctuation.
}
\description{
Removes punctuation characters or inserts spaces before and after them so that they can be used in text analysis as separate units. 
}
\usage{
RFPunctuation(texts, NoPunctuation)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{texts}{A string with punctuation which should be removed or fixed.}
  \item{NoPunctuation}{If TRUE, the function removes punctuation. If FALSE, the function inserts spaces before and after punctuation.}
}
\value{
\code{RFPunctuation} returns a string with punctuation removed or fixed for text analysis.
}
\author{
Safshekan, Nielsen
}

\examples{
# Create string with Persian characters and punctuation
x <- '\u062F\u0627\u0646\u0634\u06AF\u0627\u0647\u060C \u062A\u0647\u0631\u0627\u0646\u061F'

# Remove punctuation
RFPunctuation(x, NoPunctuation = TRUE)  

# Fix punctuation
RFPunctuation(x, NoPunctuation = FALSE)  
}