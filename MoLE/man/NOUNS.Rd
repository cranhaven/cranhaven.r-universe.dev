\name{NOUNS}
\alias{NOUNS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generate nominal lexicon
}
\description{
Generate nominal lexicon for founding agents
}
\usage{
NOUNS(n = world$nNouns, local = world$local)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{n}{
Number of nominal lexemes
}
  \item{local}{
Should agents have lexemes to refer to speech-act participants (i.e. 'I/me' and 'you')?
}
}
\details{
Minimally 2 entries are necessary to create a dataframe.
}
\value{
data frame with randomly generated forms, their meaning representations in terms of numeric vectors, and frequency counters.
}
\author{
Sander Lestrade
}

\seealso{
\code{VERBS}, \code{FIRSTSPEAKER}, \code{SEMUPDATE}
}
\examples{
NOUNS(10)
}
\keyword{misc}
