\name{RemoveNumbers}
\alias{RemoveNumbers}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove numerals.
}
\description{
Removes numerals from a string.
}
\usage{
RemoveNumbers(texts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{texts}{A string from which numerals should be removed.
}
}
\value{
\code{RemoveNumbers} returns a string with numerals removed.
}
\author{
Safshekan, Nielsen
}
\examples{
# Create string with Persian characters and number
x <- '\u0633\u0627\u0644 \u06F1\u06F3\u06F9\u06F8'

# Remove Numbers
RemoveNumbers(x)
}