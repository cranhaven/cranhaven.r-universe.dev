\name{normalization}

\alias{normalization}

\title{Normalization Methods}

\description{Collection of Different Normalization Methods.}

\usage{
normalization(Xtrain,Xtest=NULL, method = "pqn",ref=NULL)
}

\arguments{
  \item{Xtrain}{a matrix of data (training data set).}
  \item{Xtest}{a matrix of data (test data set).(by default = NULL).}
  \item{method}{the normalization method to be used. Choices are "\code{none}", "\code{pqn}", "\code{sum}", "\code{median}", "\code{sqrt}" (by default = "\code{pqn}"). A partial string sufficient to uniquely identify the choice is permitted.}
  \item{ref}{Reference sample for Probabilistic Quotient Normalization. (by default = NULL).}

}

\details{
	A number of different normalization methods are provided:
  \itemize{
    \item{ "\code{none}": no normalization method is applied.}
    \item{ "\code{pqn}": the Probabilistic Quotient Normalization is computed as described in \emph{Dieterle, et al.} (2006).}
    \item{ "\code{sum}": samples are normalized to the sum of the absolute value of all variables for a given sample.}
    \item{ "\code{median}": samples are normalized to the median value of all variables for a given sample.}
    \item{ "\code{sqrt}": samples are normalized to the root of the sum of the squared value of all variables for a given sample.}
}
}


\value{
    The function returns a list with 2 items or 4 items (if a test data set is present):
    \item{newXtrain}{a normalized matrix (training data set).}
    \item{coeXtrain}{a vector of normalization coefficient of the training data set.}
    \item{newXtest}{a normalized matrix (test data set).}
    \item{coeXtest}{a vector of normalization coefficient of the test data set.}
  }
  
\author{Stefano Cacciatore and Leonardo Tenori}

  \references{
Dieterle F,Ross A, Schlotterbeck G, Senn H.\cr
Probabilistic Quotient Normalization as Robust Method to Account for Diluition of Complex Biological Mixtures. Application in 1H NMR Metabolomics.\cr
\emph{Anal Chem} 2006;78:4281-90.
\cr
\cr
Cacciatore S, Luchinat C, Tenori L \cr
Knowledge discovery by accuracy maximization. \cr
\emph{Proc Natl Acad Sci U S A} 2014;111(14):5117-5122. doi: 10.1073/pnas.1220873111. \doi{10.1073/pnas.1220873111} \cr\cr

Cacciatore S, Tenori L, Luchinat C, Bennett PR, MacIntyre DA \cr
KODAMA: an updated R package for knowledge discovery and data mining.\cr
\emph{Bioinformatics} 2017;33(4):621-623. doi: 10.1093/bioinformatics/btw705. \doi{10.1093/bioinformatics/btw705}
}

  \seealso{\code{\link{scaling}}}

\examples{
data(MetRef)
u=MetRef$data;
u=u[,-which(colSums(u)==0)]
u=normalization(u)$newXtrain
u=scaling(u)$newXtrain
class=as.numeric(as.factor(MetRef$gender))
cc=pca(u)
plot(cc$x,pch=21,bg=class)
}
\keyword{normalization}
