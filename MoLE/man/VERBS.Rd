\name{VERBS}
\alias{VERBS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate verbal lexicon
}
\description{
Generate verbal lexicon for founding agents
}
\usage{
VERBS(n = world$nVerbs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Number of verbal lexemes
}
}
\value{
data frame with randomly generated forms, their meaning representations in terms of sets of numeric vectors (for action, actor role, and undergoer role), and frequency counters.
}
\author{
Sander Lestrade
}
\seealso{
\code{NOUNS}, \code{FIRSTSPEAKER}, \code{SEMUPDATE}
}
\examples{
VERBS(10)
}
\keyword{misc}
