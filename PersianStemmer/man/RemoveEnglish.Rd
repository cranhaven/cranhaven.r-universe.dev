\name{RemoveEnglish}
\alias{RemoveEnglish}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove English characters
}
\description{
Removes English characters from a string.
}
\usage{
RemoveEnglish(texts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{texts}{A string from which English characters should be removed.
}
}
\value{
\code{RemoveEnglish} returns a string with English characters removed.
}
\author{
Safshekan, Nielsen
}

\examples{
# Create string with English characters
x <- '\u062F\u0627\u0646\u0634\u06AF\u0627\u0647 University'

# Remove English characters
RemoveEnglish(x) 
}