\name{RemovePreSuffix}
\alias{RemovePreSuffix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Remove Persian prefixes and suffixes.
}
\description{
Removes  Persian prefixes and suffixes from a unicode string using the default list of Persian prefixes and suffixes.
}
\usage{
RemovePreSuffix(texts, Context)
}
\arguments{
  \item{texts}{A Persian string in unicode}
  \item{Context}{If TRUE, the function removes prefixes and suffixes of a word only if its stem exists in text. If FALSE, the function removes prefixes and suffixes without considering other words in text.
}
}
\value{
\code{RemovePreSuffix} returns a string with Persian prefixes and suffixes removed.
}
\author{
Safshekan, Nielsen
}
\examples{
# Create string with Persian characters
x <- '\u0627\u0628\u0631\u0642\u062F\u0631\u062A\u0647\u0627\u06CC\u06CC 
\u06A9\u062A\u0627\u0628\u0647\u0627\u06CC\u0645 \u06A9\u062A\u0627\u0628'

# Remove new line characters and fixe half-spaces from a string.
x <- RemNewlineHalfspace(x)

# Remove all characters that are not Latin, Persian or punctuation, 
# and standardize Persian characters.
x <- RefineChars(x)

# Remove Prefixes and Suffixes
RemovePreSuffix(x, Context = TRUE)
RemovePreSuffix(x, Context = FALSE)
}