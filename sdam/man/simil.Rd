\name{simil}
\alias{simil}
\title{
Similarity between vectors in columns
}
\description{
A function to compute similarity between vectors from columns in a data frame based on common attribute characteristics.
}
\usage{
simil(x, vars, uniq, diag.incl, dichot, rm.isol, k)
}
\arguments{
  \item{x}{
	a list or a data frame object
}
  \item{vars}{
  vector with column(s) in \code{x} representing variable attributes
}
  \item{uniq}{
  unique entries? (optional and logical)
}
  \item{diag.incl}{
  include also entries in matrix diagonal? (optional and logical) 
}
  \item{dichot}{
  dichotomize output? (optional and logical) 
}
  \item{rm.isol}{
  remove isolates in output? (optional and logical) 
}
  \item{k}{
  cut-off for dichotomization (if not specified, \code{max} of output)
}
}
\details{
This is a function to compute the similarity between two or more vectors, which can arise from columns in a data frame and from list entries. 
The similarity of artefacts or other units having common variable attributes specified in \code{vars} is by simple matching, and 
this represents a measure of proximity among the items to compare. Comparison takes an \code{id} column from \code{x}; 
otherwise, the first column is taken provided that there are no duplicated entry names.

Both the dichotomization of the output and the removing isolated items from the system of co-occurrence relations depends on functions 
from package \code{"multiplex"}.
}
\value{
A valued matrix of similarities among items in \code{x}.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
Antonio Rivero Ostoic
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
\code{\link{edhw}}, \code{\link{get.edh}}, \code{\link[multiplex:dichot]{dichot}}, \code{\link[multiplex:rm.isol]{rm.isol}}, \code{\link[multigraph]{multigraph}}.
}
\examples{
\dontrun{
# get inscriptions from a Roman province
arm <- edhw(province="Armenia")

# choose variables to a data frame
armv <- edhw(x=arm, as="df", 
        vars=c("findspot_ancient", "type_of_inscription", "type_of_monument", "language"))

# matrix of similarities of two variables
simil(armv, vars=c("findspot_ancient", "language"))}
}
\keyword{ metrics }
\keyword{ manip }
\keyword{ utilities }