\name{gmap}
\alias{gmap}
\title{Basis Transformation of a Model.}
\description{
Transform the basis for the state by a given invertible matrix.
}
\usage{
    gmap(g, model)
}
\arguments{
    \item{g}{An invertible matrix}
    \item{model}{An object of class TSmodel.}
}
\value{

An equivalent model transformed using g.
}
\details{

If the input model is in state space form g is a change of basis 
for the state. If the input model is in ARMA form then the 
polynomials are premultiplied by g. If g is a scalar it is treated 
as a diagonal matrix.
}
\examples{
data("eg1.DSE.data.diff", package="dse")
model <- toSS(estVARXls(eg1.DSE.data.diff))
gmap(2, model)
}
\concept{DSE}
\keyword{ts}

%%\name{findg}
%%\alias{findg}
%%\title{Find Equivalence Transformation}
%%\description{
%%   Try to find a matrix g which makes two models equivalent.
%%   }
%%\usage{
%%    findg(model1, model2, minf=nlmin)
%%}
%%\arguments{
%%
%%    \item{model1, model2}{Objects of class TSmodel.}
%%    \item{minf}{Algorithm used to minimize the difference.}
%%}
%%\value{A matrix which converts one model to the other.}
%%\details{
%%This is set up as a minimization problem with the objective 
%%to reduce the squared difference between parameters.
%%}
%%\note{WARNING: This program does not work very well.
%%  It is also rather crude and can be very slow.
%%  }
%%\seealso{
%%    \code{\link{gmap}}
%%}
%%\examples{
%%# findg(model1, model2)
%%}
%%\concept{DSE}
%%\keyword{ts}

