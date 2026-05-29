\name{RemoveStopwords}
\alias{RemoveStopwords}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove Persian stop-words.
}
\description{
Defines a list of Persian stopwords and removes them from a string.
}
\usage{
RemoveStopwords(texts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{texts}{A string from which Persian stopwords should be removed.}
}
\value{
\code{RemoveStopwords} returns a string with Persian stopwords removed.
}
\author{
Safshekan, Nielsen
}

\examples{
# Create Persian string with stopwords
x <- '\u0627\u0632 
\u062F\u0627\u0646\u0634\u06AF\u0627\u0647 
\u0622\u0645\u062F'

# Remove new line characters and fixe half-spaces from a string.
x <- RemNewlineHalfspace(x)

# Remove all characters that are not Latin, Persian or punctuation, 
# and standardize Persian characters.
x <- RefineChars(x)

# Remove stopwords
RemoveStopwords(x)
}