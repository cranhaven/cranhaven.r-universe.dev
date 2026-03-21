\name{TYPEMATCH}
\alias{TYPEMATCH}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Determine role qualification
}
\description{
Determine event-role distribution of participants on the basis of role qualification.
}
\usage{
TYPEMATCH(hearerID, analysis)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hearerID}{
Pointer to hearer agent
}
  \item{analysis}{
Analyzed utterance in which verb and participants have been identified
}
}
\details{
Typematch is only necessary if explicit markers and/or grammar are not sufficiently informative
}
\value{
Analysis with event-role assignment (if possible on the basis of role qualifications)
}
\references{
Aristar, A. R. 1997. "Marking and hierarchy. Types and the grammaticalization of case markers". Studies in Language 21:2.313-368.
}
\author{
Sander Lestrade
}

\seealso{
\code{PROTOINTERPRETATION}
}
\examples{
FOUND()
situation=SITUATION(1)
proposition=PROPOSITION(1, situation)
utterance=PRODUCE(1, proposition)
analysis=ANALYZE(2, utterance, situation)
TYPEMATCH(2, analysis)
}
\keyword{misc}
