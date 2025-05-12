\name{draw.polys}
\alias{draw.polys}
\alias{polys2nb}
\alias{nb2prec}
\alias{polys2polys}
\alias{nb2nb}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Additional supporting functions for random Markov fields
}
\description{
This set of functions were useful in the past to get information and to plot maps but somehow now seem redundant.
}
\usage{
draw.polys(polys, object = NULL, scheme = NULL, 
           swapcolors = FALSE, n.col = 100, ...)
polys2nb(polys)                 
nb2prec(neighbour,x,area=NULL)
polys2polys(object, neighbour.nb)
nb2nb(neighbour.nb)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
 \item{polys}{an object containing the polygon information for the area}
 \item{object}{are either the  values to plot in the \code{draw.polys()} function or a polygons information for a shape file for function \code{polys2polys}}
 \item{scheme}{scheme of colours to use, it can be \code{"heat"}, \code{"rainbow"}, \code{"terrain"}, \code{"topo"}, \code{"cm"} or any colour}
 \item{swapcolors}{to reverse the colours, it just work for \code{"heat"}, \code{"rainbow"}, \code{"terrain"}, \code{"topo"}, \code{"cm"} options}
 \item{n.col}{range for the colours}
  \item{neighbour.nb}{neighbour information for a shape file for function \code{nb2nb}}
  \item{neighbour}{the neighbour information, and if the neighbour is from S4 shape file than use \code{nb2nb} to transfer it to the appropriate neighbour for \code{MRF()}, \code{MRFA()}, \code{mrf()} and \code{mrfa()}.}
  \item{x}{the factor defining the areas}
  \item{area}{all possible areas involved }
 \item{\dots}{for extra options}
 }
\details{

\code{draw.polys()} plots the fitted values of fitted \code{MRF} object.

\code{polys2nb()} gets the neighbour information from the polygons.

\code{nb2prec()} creates the precision matrix from the neighbour information.

\code{polys2polys()} transforms a shape file polygons (S4 object)  to the polygons required form for the functions \code{MRF()} and \code{MRFA()}.

\code{nb2nb()} transforms from a shape file neighbour (S4 object)  to the neighbour required form for functions \code{MRF()}.
}
\value{The \code{draw.polys()} produces a plot while the rest of the functions produce required object for fitting or plotting.
}
\references{
De Bastiani, F. Rigby, R. A., Stasinopoulos, D. M.,  Cysneiros, A. H. M. A. and Uribe-Opazo, M. A. (2016) Gaussian Markov random spatial models in GAMLSS. \emph{Journal of Applied Statistics}, pp 1-19.

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.


Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC. An older version can be found in \url{https://www.gamlss.com/}.


Rue and Held (2005) \emph{Gaussian markov random fields: theory and applications}, Chapman  & Hall, USA. 

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{https://www.jstatsoft.org/v23/i07/}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC.  

(see also \url{https://www.gamlss.com/}).
}

\author{
Fernanda De Bastiani, Mikis Stasinopoulos, Robert Rigby and Vlasios Voudouris

Maintainer: Fernanda <fernandadebastiani@gmail.com>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{MRF}}, \code{\link{MRFA}}
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{regression}
\keyword{spatial}% __ONLY ONE__ keyword per line
