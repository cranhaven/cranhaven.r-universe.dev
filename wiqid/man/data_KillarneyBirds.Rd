\name{KillarneyBirds}
\alias{KillarneyBirds}
\docType{data}
\title{
Abundance of woodland birds
}
\description{
The number of territories held by breeding males in 9 blocks of woodland habitat in County Killarney, Ireland.
}
\usage{data(KillarneyBirds)}
\format{
  A data frame with counts for 31 species in 9 blocks of habitat. Row names are the English species names.
  \describe{
    \item{\code{Oak1}}{first of 3 oak wood sites}
    \item{\code{Oak2}}{second of 3 oak wood sites}
    \item{\code{Oak3}}{third of 3 oak wood sites}
    \item{\code{Yew}}{a mature yew wood}
    \item{\code{Sitka}}{a Sitka spruce plantation}
    \item{\code{Norway}}{a Norway spruce plantation}
    \item{\code{Mixed}}{a mixed broadleaf wood}
    \item{\code{Patchy}}{a wood with patches of broadleaf and conifer trees}
    \item{\code{Swampy}}{a swampy seasonally-flooded woodland}
  }
}
\source{
Batten L. A. (1976) Bird communities of some Killarney woodlands. \emph{Proceedings of the Royal Irish Academy} 76:285-313.
}
\references{
Magurran (2004) \emph{Measuring Biological Diversity}, p237

Solow (1993) A simple test for change in community structure.	\emph{J Animal Ecology} 62:191-193.
}
\examples{
data(KillarneyBirds)
## number of species in each block of habitat:
colSums(KillarneyBirds > 0)

}
\keyword{datasets}
