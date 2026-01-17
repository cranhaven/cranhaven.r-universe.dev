\name{MC_baseline_distribution}
\alias{MC_baseline_distribution}
\concept{Heterocitation Balance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Function to compute baseline heterocitation values for the graph under study with random permutation of corpus attributions}
\description{
This function performs Monte Carlo runs with random permutations of corpus tags in the graph provided and computes the heterocitation balance on the new graphs. Permutation is repeated over several iterations (set through the "rep" argument) and provides a baseline Dx values for the graph topology considered. This can then be compared with the Dx value obtained for the original graph to evaluate whether it could merely be the result of chance (see \code{\link{significance_Dx}}).
}
\usage{
MC_baseline_distribution(gr, labels, infYearLimit, supYearLimit, rep = 20)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gr}{Graph file (created with build_graph)}
  \item{labels}{List of the names of the two corpora studied (e.g. c("Computer Science", "Mathematics")), present in the "Corpus" attribute}
  \item{infYearLimit}{Minimum year considered in this study}
  \item{supYearLimit}{Maximum year considered in this study}
  \item{rep}{Number of Monte Carlo iterations}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
This function currently plots the histograms of distribution of Dx values generated through random permutations of corpus tags among the records.
Returns a list containing:
\item{Dx1}{Dx value for corpus 1 per iteration}
\item{Dx1}{Dx value for corpus 2 per iteration}
\item{DxALL}{Global Dx value per iteration}
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Christian Vincenot (christian@vincenot.biz)
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{significance_Dx}}, \code{\link{heterocitation}}
}
%\examples{}
%\keyword{ ~kwd1 }
