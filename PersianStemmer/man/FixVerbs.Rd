\name{FixVerbs}
\alias{FixVerbs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Stemms verbs
}
\description{
Stems verbs and returns past and present roots.
}
\usage{
FixVerbs(texts, Context)
}
\arguments{
  \item{texts}{A Persian string in unicode.}
  \item{Context}{If TRUE, the function stems past-root+'he' only if other verbs with the same past-root exist in text. If FALSE, the function stems verbs without considering other words in text.
}
}
\value{
\code{FixVerbs} returns a string with verbs stemmed.
}
\author{
Safshekan, Nielsen
}
\examples{
# Create string with Persian verbs
x <- '\u0646\u0648\u0634\u062A\u0647 \u0634\u062F\u0647 
\u0628\u0648\u062F\u0647 \u0627\u0633\u062A - \u0646\u0648\u0634\u062A\u0645 - 
\u062F\u0627\u0631\u06CC\u0645 \u0645\u06CC\u0631\u0648\u06CC\u0645 - 
\u062E\u0648\u0627\u0646\u062F\u0647 \u0645\u06CC\u0634\u0648\u0646\u062F - 
\u062E\u0648\u0627\u0647\u062F \u06AF\u0641\u062A - 
\u0628\u0631\u062F\u0647 \u0627\u0633\u062A - 
\u0645\u06CC\u06AF\u0648\u06CC\u06CC\u0645'

# Remove new line characters and fixe half-spaces from a string.
x <- RemNewlineHalfspace(x)

# Remove all characters that are not Latin, Persian or punctuation, 
# and standardize Persian characters.
x <- RefineChars(x)

# Stems verbs
y <- FixVerbs(x, Context = TRUE)
z <- FixVerbs(x, Context = FALSE)

# Remove the numeric signifiers which are used in PerStem function.
gsub("0|1|2|3|4|5","",y)
gsub("0|1|2|3|4|5","",z)
}