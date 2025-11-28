\name{Recode}
\alias{Recode}
\alias{Recode4}
\alias{RecodeR}
\alias{Recode4R}

\title{Basic multiple recoding}
\description{Basic multiple recoding (similar to the 'SQL' left join)}

\usage{
Recode(var, from, to, char=TRUE, recycle=FALSE)
Recode4(var, from, to, missed="", ...)
RecodeR(var, from, to, char=TRUE, recycle=FALSE)
Recode4R(var, from, to, missed="", ...)
}

\arguments{
\item{var}{Variable to recode}
\item{from}{'from' column of the recoding "table"}
\item{to}{'to' column}
\item{char}{If TRUE (default), do not treat 'to' character vectors as factors}
\item{recycle}{If TRUE (not default), recycle 'to' along 'from'}
\item{missed}{Replace missed (not recoded) with something, default is "" (empty charactrer string)}
\item{...}{Further options to Recode() and RecodeR()}
}

\details{Basic multiple recoding is similar to 'SQL' left join.

Inspired from Paul Johnston (Univ. of Kansas) recode() function.

Alternatives are car::recode(), lessR::Recode(), admisc::recode() and 'mgsub' package.
First three are much more complicated, last is much slower and less flexible.

To understand the idea better, please look on the examples.

There are four functions:

1. Recode() -- base function. If starting points ("from") are the same,
only the \emph{last} rule ("from-to" pair) has an effect.
If rules are chained, they still work independently (i.e., chaining has no effect).

2. Recode4() -- considers not recoded (missing). By default, this will replace non-Recode()'d 
entries with empty string ("").

3. RecodeR() -- running recode. If starting points ("from") are the same,
only the \emph{first} rule ("from-to" pair) has an effect.
Chaining is possible.

4. Recode4R() -- running plus considers missing. By default, this will replace non-RecodeR()'ed 
entries with empty string ("").
}

\value{Recoded vector (note that mode will not necessarily be the same, e.g., when recoding numbers with characters).}

% \references{}

\author{Alexey Shipunov}

% \seealso{}

\examples{
## recoding a phrase
phrase <- "The quick brown fox jumps over 123 lazy dogs"
var <- unlist(strsplit(phrase, split=""))
from <- letters[1:20]
to <- rev(from)
Recode.result <- paste(Recode(var, from, to), collapse="")
Recode4.result <- paste(Recode4(var, from, to, missed="-"), collapse="")
RecodeR.result <- paste(RecodeR(var, from, to), collapse="")
Recode4R.result <- paste(Recode4R(var, from, to, missed="-"), collapse="")
from.rule <- paste(from, collapse=" ")
to.rule <- paste(to, collapse=" ")
rbind(from.rule, to.rule, phrase, Recode.result, Recode4.result, RecodeR.result, Recode4R.result)

## reverse complement of DNA sequence
dna <- "GAATTC" # EcoR1 palindromic sequence
paste(Recode(rev(strsplit(dna, NULL)[[1]]),
 c("A", "T", "G", "C"), c("T", "A", "C", "G")), collapse="") # = 'dna', as expected
dna <- "ATTCGGC" # something random
paste(Recode(rev(strsplit(dna, NULL)[[1]]),
 c("A", "T", "G", "C"), c("T", "A", "C", "G")), collapse="")

## Recode4() when value recoded to itself
Recode4(1:5, 1:4, c(2, 1, 3, 3), NA)
Recode4(1:5, 1:4, c(2, 1, 3, 3))

## this is how "char" option works
Recode(1, 1, factor(2), char=FALSE)
Recode(1, 1, factor(2))

## this is how "recycle" option works
Recode(1:3, 1:3, 4)
Recode(1:3, 1:3, 4, recycle=TRUE)

}

\keyword{manip}
