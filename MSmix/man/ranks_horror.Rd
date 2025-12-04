\name{ranks_horror}
\alias{ranks_horror}
\docType{data}
\title{Arkham Horror Data (complete rankings)}
\description{The Arkham Horror dataset came up from an on-line survey conducted by Curtis Miller to investigate popularity of different player roles of \emph{Arkham Horror: The Card Game}. A sample of \eqn{N=241} respondents provided their complete rankings of \eqn{n=5} player modes in order of preference. The player roles are: 1 = Survivor, 2 = Rogue, 3 = Guardian, 4 = Seeker, 5 = Mystic. Statistical analyses of these data can be found at the links provided in References.}
\usage{data(ranks_horror)}
\format{
A data frame gathering \eqn{N=421} complete rankings of the \eqn{n=5} player roles in each row (rank 1 = most preferred item). The variables are detailed below:
\describe{
\item{Survivor}{Rank assigned to Survivor role.}
\item{Rogue}{Rank assigned to Rogue role.}
\item{Guardian}{Rank assigned to Guardian role.}
\item{Seeker}{Rank assigned to Seeker role.}
\item{Mystic}{Rank assigned to Mystic role.}
}
}
\references{
Curtis Miller's personal website: \url{https://ntguardian.wordpress.com/2019/02/18/}.

}
\examples{
str(ranks_horror)
head(ranks_horror)
}
\keyword{datasets}
