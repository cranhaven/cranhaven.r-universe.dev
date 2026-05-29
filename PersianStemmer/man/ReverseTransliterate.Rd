\name{ReverseTransliterate}
\alias{ReverseTransliterate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Transliterate Latin characters into Persian unicode characters
}
\description{
Transliterates Latin characters into Persian unicode characters using a transliteration system developed by Roozbeh Safshekan and Rich Nielsen.
}
\usage{
ReverseTransliterate(texts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{texts}{A string in Latin characters to be transliterated into Persian characters.}
}

\value{
\code{ReverseTransliterate} returns a string in Persian characters.
}
\author{
Safshekan, Nielsen
}
\examples{
# Create Latin string 
x <- 'danWGah thran'

# Converts Latin characters into Persian unicode characters
ReverseTransliterate(x)
}