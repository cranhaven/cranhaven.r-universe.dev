\name{EROSION}
\alias{EROSION}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Word erosion
}
\description{
If a perceived form differs from the mental representation it is matched with and the form has not been set yet, the hearer agent adjusts its mental representation.
}
\usage{
EROSION(hearerID, interpretation)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hearerID}{
Pointer to the hearer agent whose representations might erode.
}
  \item{interpretation}{
Analysis of the utterance including the actually perceived forms.
}
}
\details{
Forms will only be adjusted if they have not been frequently used (yet). Pronounced forms may differ from their representations because of reduction in pronunciation (cf. \code{REDUCE}).
}
\value{
no actual output; the form representations of the hearer agent are updated.
}
\author{
Sander Lestrade
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{TURN}
}
\examples{
FOUND()
situation=SITUATION(1)
proposition=PROPOSITION(1, situation)
utterance=PRODUCE(1, proposition)
interpretation=INTERPRET(2, utterance, situation)
EROSION(2, interpretation)
}
\keyword{misc}
