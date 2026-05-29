\name{FixBrokenPlurals}
\alias{FixBrokenPlurals}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Stems Arabic broken plurals
}
\description{
Stems Arabic broken plurals and returns singulars.
}
\usage{
FixBrokenPlurals(texts)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{texts}{
A string with Arabic broken plurals that should be stemmed.}
}
\value{
\code{FixBrokenPlurals} returns a string with Arabic broken plurals stemmed.

}
\author{
Safshekan, Nielsen
}
\examples{
# Create string with Arabic broken plurals
x <- '\u0645\u0635\u0627\u062F\u06CC\u0642 
\u0648\u0632\u0631\u0627 
\u062D\u062F\u0648\u062F'

# Remove new line characters and fixe half-spaces from a string.
x <- RemNewlineHalfspace(x)

# Remove all characters that are not Latin, Persian or punctuation, 
# and standardize Persian characters.
x <- RefineChars(x)

# Stem Arabic broken plurals
FixBrokenPlurals(x)
}