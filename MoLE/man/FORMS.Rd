\name{FORMS}
\alias{FORMS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate forms
}
\description{
Generates set of unique forms for initial generation of speakers.
}
\usage{
FORMS(n, length = world$wordLength, vowels = world$vowels, consonants = world$consonants)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Number of word forms to be generated
}
  \item{length}{
Length (range) within which word forms have to fall.
}
  \item{vowels}{
Vowels that are used in the language
}
  \item{consonants}{
Vowels that are used in the language
}
}
\details{
Allows for CV and VC}
\value{
character vector
}
\author{
Sander Lestrade
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{VERBS}, \code{NOUNS}, \code{FOUND}, \code{FUSE}, \code{PROCREATE}
}
\examples{
world$vowels; world$consonants; world$wordLength
FORMS(10)
}
\keyword{misc}
