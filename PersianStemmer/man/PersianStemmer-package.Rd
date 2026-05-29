\name{PersianStemmer-package}
\alias{PersianStemmer-package}
\alias{PersianStemmer}
\docType{package}
\title{
A package for stemming Persian for text analysis.
}
\description{
This package is a Persian Stemmer for Text Analysis.
}
\details{
Use the \code{PerStem} function.
}
\author{
Roozbeh Safshekan <rse@mit.edu> and Rich Nielsen <rnielsen@mit.edu>
}

\keyword{ package }
\seealso{ \code{\link{PerStem}}}
\examples{
# Load data
data(UniversityofTehran)

# Stem and transliterate the text
PerStem(UniversityofTehran,NoEnglish=TRUE, NoNumbers= TRUE, 
                    NoStopwords=TRUE, NoPunctuation= TRUE,
                    StemVerbs = TRUE, NoPreSuffix= TRUE, Context = TRUE,
                    StemBrokenPlurals=TRUE,Transliteration= TRUE)

}