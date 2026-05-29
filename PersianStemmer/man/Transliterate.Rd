\name{Transliterate}
\alias{Transliterate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transliterate Persian unicode characters into Latin characters
}
\description{
Transliterates Persian unicode characters into Latin characters using a transliteration system developed by Roozbeh Safshekan Rich Nielsen.
}
\usage{
Transliterate(texts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{texts}{A string in Persian characters to be transliterated into Latin characters.
}
}

\value{
\code{Transliterate} returns a string in Latin characters.
}
\author{
Safshekan, Nielsen
}
\examples{
# Create Persian string
x <- '\u062F\u0627\u0646\u0634\u06AF\u0627\u0647 \u062A\u0647\u0631\u0627\u0646'

# Performs transliteration of Persian into Latin characters
Transliterate(x)
}