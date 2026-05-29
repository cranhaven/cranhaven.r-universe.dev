\name{RefineChars}
\alias{RefineChars}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Removes all characters that are not Latin, Persian or punctuation, and standardizes Persian characters.
}
\description{
Removes all unicode characters except Latin, Persian or General Punctuation characters and standardizes Persian characters.}
\usage{
RefineChars(texts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{texts}{A string from which all characters that are not Latin, Persian or punctuation should be removed, or in which Persian characters should be standardized.}
}
\value{
\code{RefineChars} returns a string with only Latin, standardized Persian or general punctuation characters.
}
\author{
Safshekan, Nielsen
}

\examples{
# Create string with Latin, Persian, Japanese, non-standardized Persian and punctuation characters.
x <- '\u062F\u0627\u0646\u0634\u06AF\u0627\u0647\u064A \u060C 
\u0641\u06CC\u0632\u06CC\u0643 university 
\u65E5\u672C \u0664\u0665\u0666'

# Remove new line characters and fixe half-spaces from a string.
x <- RemNewlineHalfspace(x)

# Remove all characters that are not Latin, Persian or punctuation, 
# and standardize Persian characters.
RefineChars(x)
}