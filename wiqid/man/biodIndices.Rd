\name{Diversity indices}
\alias{Diversity indices}
\alias{biodSimpson}
\alias{biodShannon}
\alias{biodBerger}
\alias{biodBrillouin}
\title{
Biodiversity indices
}
\description{
Common indices of biodiversity, expressed as the number of common species.
}
\usage{
biodSimpson(abVec, correct = TRUE)
biodShannon(abVec)
biodBerger(abVec)
biodBrillouin(cntVec)
}
\arguments{
  \item{abVec}{
a vector of measures of abundance, eg. counts of individuals or biomass, one element per species; or a corresponding matrix or data frame, which will be converted to a vector with \code{rowSums}.
}
  \item{cntVec}{
a vector (or matrix or data frame) of counts of individuals, one element per species. Non-integers will be rounded without warning.
}
  \item{correct}{
if TRUE, a small sample correction is applied, and in that case \code{abVec} should have count data (non-integers will be silently rounded).
}
}
\details{
\describe{
  \item{biodSimpson}{
Inverse of Simpson's (1949) index of dominance. If \code{correct = TRUE}, a small-sample correction is applied, giving Hurlbert's (1971) diversity index. Otherwise, the result is equivalent to Hill's (1973) \eqn{N_2}.
}
  \item{biodShannon}{
Exponential form of Shannon's (1948) entropy measure, equivalent to Hill's (1973) \eqn{N_1}.
}
  \item{biodBerger}{
Inverse of Berger & Parker's (1970) index of dominance, equivalent to Hill's (1973) \eqn{N_Inf}.
}
  \item{biodBrillouin}{
Exponential form of Brillouin's index: for small, completely censused populations, Brillouin's index is a more appropriate measure of entropy than Shannon's measure (Maurer & McGill 2011:61).
}
}
}
\value{
The relevant index.
}
\references{
Berger, W H; F L Parker. 1970. Diversity of planktonic Foramenifera in deep sea sediments. \emph{Science} 168:1345-1347. 

Hill, M O. 1973. Diversity and evenness: a unifying notation and its consequences. \emph{Ecology} 54:427-431.

Hurlbert, S H. 1971. The nonconcept of species diversity: A critique and alternative parameters. \emph{Ecology} 52:577-586.

Maurer, B A; B J McGill. 2011. Measurement of species diversity. 55-64 in Magurran, A E, and B J McGill, editors. \emph{Biological diversity: frontiers in measurement and assessment.} Oxford University Press, Oxford, New York NY

Shannon, C E. 1948. A mathematical theory of communication. \emph{Bell System Technical Journal} 27:379-423

Simpson, E H. 1949. Measurement of diversity. \emph{Nature} 163:688.


}
\author{
Mike Meredith
}
\section{Warning}{
It is important that the proportions of each species in the \emph{sample} represent those in the \emph{population} from which it is drawn. This will not be the case if probability of inclusion varies among species, as often occurs when samples are collected in the field.
}
\seealso{
\code{\link{richSobs}} and \link{Species richness estimators} for alternatives to indices.
}
\examples{
data(KillarneyBirds)
apply(KillarneyBirds, 2, biodSimpson)

}
