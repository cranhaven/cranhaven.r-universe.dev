\name{PerStem}
\alias{PerStem}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Persian Stemmer for Text Analysis
}
\description{
Stems Persian texts for text analysis.
}
\usage{
PerStem(dat, NoEnglish = TRUE, NoNumbers = TRUE, 
	NoStopwords = TRUE, NoPunctuation = TRUE, 
	StemVerbs = TRUE, NoPreSuffix = TRUE, 
	Context = TRUE, StemBrokenPlurals = TRUE, 
	Transliteration = TRUE)
}


\arguments{
  \item{dat}{
The original data.
}
  \item{NoEnglish}{
Removes English characters.
}
  \item{NoNumbers}{
Removes numbers.
}
  \item{NoStopwords}{
Removes stopwords by using the default stopword list.
}
  \item{NoPunctuation}{
If TRUE the function removes punctuation. If FALSE, it fixes punctuation for text analysis.
}
  \item{StemVerbs}{
Performs stemming on verbs and returns past or present root of the verb.
}
  \item{NoPreSuffix}{
Performs stemming by removing prefixes and suffixes.
}
  \item{Context}{
If TRUE, the function performs stemming on a word only if its stem exists in text. If FALSE, the function performs stemming without considering other words in text.
}
  \item{StemBrokenPlurals}{
Performs stemming on Arabic broken plurals and return singulars by using the default Arabic broken plurals list.
}
  \item{Transliteration}{
Transliterates Persian unicode characters into Latin characters using a transliteration system developed by Roozbeh Safshekan and Rich Nielsen.
}
}
\details{
\code{PerStem} prepares texts in Persian for text analysis by stemming.
}
\value{
\code{PerStem} returns the stemmed Persian text.
}
\author{
Roozbeh Safshekan, Richard Nielsen
}
\examples{
# Load data
data(UniversityofTehran)

# Stem and transliterate the text
PerStem(UniversityofTehran,NoEnglish=TRUE, NoNumbers= TRUE, 
                    NoStopwords=TRUE, NoPunctuation= TRUE,
                    StemVerbs = TRUE, NoPreSuffix= TRUE, Context = TRUE,
                    StemBrokenPlurals=TRUE,Transliteration= TRUE)
}