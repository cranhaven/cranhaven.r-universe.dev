\name{BCQuest}
\title{The Questmotif Bicluster algorithm}
\alias{BCQuest}
\alias{BCQuestord}
\alias{BCQuestmet}
\alias{BCQuest-class}
\alias{BCQuestord-class}
\alias{BCQuestmet-class}
\alias{Quest}
\alias{biclust,matrix,BCQuestord-method}
\alias{biclust,matrix,BCQuest-method}
\alias{biclust,matrix,BCQuestmet-method}

%- Also NEED an '\alias' for EACH other topic documented here.
\description{Performs Questmotif Biclustering a Bicluster algorithm for questionairs based on the framework by Murali and Kasif (2003). Searches subgroups of questionairs with same or similar answer to some questions.}
\usage{

\S4method{biclust}{matrix,BCQuest}(x, method=BCQuest(), ns=10, nd=10, sd=5, alpha=0.05, number=100)
\S4method{biclust}{matrix,BCQuestord}(x, method=BCQuestord(), d=1, ns=10, nd=10, sd=5, alpha=0.05, number=100)
\S4method{biclust}{matrix,BCQuestmet}(x, method=BCQuestmet(), quant=0.25, vari=1, ns=10, nd=10, sd=5, 
  alpha=0.05, number=100)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{Data Matrix.}
  \item{method}{Here BCQuest, to perform Questmotif algorithm}
  \item{ns}{Number of questions choosen.}
  \item{nd}{Number of repetitions.}
  \item{sd}{Sample size in repetitions.}
  \item{alpha}{Scaling factor for column result.}
  \item{number}{Number of bicluster to be found.}
  \item{d}{Half margin of intervall question values should be in (Intervall is mean-d,mean+d).}
  \item{quant}{Which quantile to use on metric data}
  \item{vari}{Which varianz to use for metric data}
}

\value{
  Returns an object of class \code{Biclust}.
}

\section{Extends}{
Class \code{"\linkS4class{BiclustMethod}"}, directly.
}

\author{Sebastian Kaiser 
\email{sebastian.kaiser@stat.uni-muenchen.de}
}

\references{
Murali, T. & Kasif, S. 
Extracting Conserved Gene Expression Motifs from Gene Expression Data 
Pacific Symposium on Biocomputing, sullivan.bu.edu, 
2003, 8, 77-88


}

\seealso{ \code{\link{biclust}}, \code{\link{Biclust}}}
%\examples{}

\keyword{cluster}
\keyword{classif}
