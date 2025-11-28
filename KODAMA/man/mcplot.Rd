\name{mcplot}
\alias{mcplot}
\title{Evaluation of the Monte Carlo accuracy results}
\description{
This function can be used to plot the accuracy values obtained during KODAMA procedure.
}
\usage{
mcplot(model)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{model}{output of KODAMA.}

}
\value{
No return value.
}
\author{Stefano Cacciatore}

\references{
Abdel-Shafy EA, Kassim M, Vignol A, \emph{et al.} \cr
KODAMA enables self-guided weakly supervised learning in spatial transcriptomics. \cr
\emph{bioRxiv} 2025. doi: 10.1101/2025.05.28.656544. \doi{10.1101/2025.05.28.656544} \cr\cr

Cacciatore S, Luchinat C, Tenori L \cr
Knowledge discovery by accuracy maximization. \cr
\emph{Proc Natl Acad Sci U S A} 2014;111(14):5117-5122. doi: 10.1073/pnas.1220873111. \doi{10.1073/pnas.1220873111} \cr\cr

Cacciatore S, Tenori L, Luchinat C, Bennett PR, MacIntyre DA \cr
KODAMA: an updated R package for knowledge discovery and data mining.\cr
\emph{Bioinformatics} 2017;33(4):621-623. doi: 10.1093/bioinformatics/btw705. \doi{10.1093/bioinformatics/btw705}
}

\seealso{\code{\link{KODAMA.matrix}},\code{\link{KODAMA.visualization}}}
\examples{
\donttest{
 data=as.matrix(iris[,-5])
 kk=KODAMA.matrix(data)
 mcplot(kk)
}
}
