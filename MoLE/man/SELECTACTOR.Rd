\name{SELECTACTOR}
\alias{SELECTACTOR}
\alias{SELECTUNDERGOER}
\alias{SELECTVERB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Find actor expression
}
\description{
Select best expression for actor/undergoer/verb participant in the event to be described
}
\usage{
SELECTACTOR(speakerID, situation, verb = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{speakerID}{
Pointer to speaker agent
}
  \item{situation}{
Situation in which event to be described is situated
}
  \item{verb}{
Pointer to verb lexeme used in the utterance to be formulated (if present already)
}
}
\details{
Verb is relevant because of collostruction frequencies: some agents are more likely to be mentioned given certain verbs (cf. \code{CANDIDATEORDER}). Works other way around for \code{SELECTVERB}.
}
\value{
A dataframe with the lexical representation of the agent/undergoer/verb.
}
\author{
Sander Lestrade
}

\seealso{
\code{PROPOSITION}
}
\examples{
FOUND()
situation=SITUATION(1)
situation[situation$target==1,]
SELECTACTOR(1, situation)
SELECTVERB(1, situation)
if(!is.na(situation[situation$target==1,]$U1)){
	SELECTUNDERGOER(1, situation)
}
}