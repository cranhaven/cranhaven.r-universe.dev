\name{controlcatpredi}
\alias{controlcatpredi}
\title{
Control function
}
\description{
Function used to set several parameters to control the selection of the optimal cut points in a logistic regression model}
\usage{
controlcatpredi(min.p.cat = 1, grid = 100, B = 50, eps=0.001,
b.method = c("ncoutcome", "coutcome"), print.gen = 0)
}

\arguments{
  \item{min.p.cat}{Set the minimun number of individuals in each category}
  \item{grid}{Grid size for the AddFor and BackAddFor algorithms}
  \item{B}{Number of bootstrap replicates for the AUC bias correction procedure}
  \item{eps}{An argument for the BackAddFor algorithm, indicating whether the improvement between iterations is considered significant}
  \item{b.method}{ Allows to specify whether the bootstrap resampling should be done considering or not the outcome variable. 
  The option "ncoutcome" indicates that the data is resampled without taking into account the response variable, while "coutcome" indicates that the data is
   resampled in regard to the response variable}
  \item{print.gen}{corresponds to the argument print.level of the \code{\link[rgenoud]{genoud}} function of the package \code{rgenoud}.}
}
%%\details{}
\value{
A list with components for each of the possible arguments.

}
\references{
Mebane Jr, W. R., & Sekhon, J. S. (2011). Genetic optimization using derivatives: the rgenoud package for R. \emph{Journal of Statistical Software} 42\bold{11}, 1-26.
}
\author{
Irantzu Barrio, Maria Xose Rodriguez-Alvarez, Inmaculada Arostegui, Javier Roca-Pardinas and Xabier Amutxastegi.
}
%%\note{}

\seealso{
See Also as \code{\link{catpredi}}.
}
%%\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
