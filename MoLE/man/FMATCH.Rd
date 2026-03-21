\name{FMATCH}
\alias{FMATCH}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compare forms}
\description{
Determine match between a given (perceived) form and a list of forms (i.e., the mental representations).
}
\usage{
FMATCH(target, lexicon)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{target}{
The form whose matching lexeme is to be identified
}
  \item{lexicon}{
The lexicon in which a match is sought.
}
}
\details{
Characters are matched one by one from left to right. Mismatches are weighted according to onset priority: mismatches in the beginning of a word are more important than later ones.
}
\value{
vector of matching scores rescaled to 1-0 range.
}
\author{
Sander Lestrade
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{ANALYZE}
}
\examples{
FOUND()
(lexicon=head(population[[1]]$nouns))
target=lexicon$form[1]
FMATCH(target, lexicon)
}
\keyword{misc}
