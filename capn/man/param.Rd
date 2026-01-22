\docType{data}
\name{param}
\alias{param}
\title{the parameter vector adopted in GOM dataset}
\format{\strong{param}: a data.frame of parameters
\itemize{
\item \code{r} intrinsic growth rate (=0.3847)
\item \code{k} carrying capacity (=359016000)
\item \code{q} catchability coefficient (=0.00031729344157311126)
\item \code{price} price (=2.70)
\item \code{cost} cost (=153.0)
\item \code{alpha} technology parameter (=0.5436459179063678)
\item \code{gamma} pre-ITQ management parameter (=0.7882)
\item \code{y} system equivalence parameter (=0.15745573410462155)
\item \code{delta} discount rate (=0.02)
\item \code{order} Chebyshev polynomial order (=50)
\item \code{upperK} upper bound of Chebyshev polynomial nodes (=k)
\item \code{lowerK} lower bound of Chebyshev polynomial nodes (=5*10^6)
\item \code{nodes} the number of Chebyshev polynomial nodes (=50)
}}
\usage{
## Load dataset
data("GOM")
}
\description{
The \code{GOM} provides data to replicate the Gulf of Mexico Reef Fish example in \href{http://www.journals.uchicago.edu/doi/abs/10.1086/676034}{Fenichel and Abbott (2014)}.
}
\references{
Fenichel, Eli P. and Joshua K. Abbott. (2014) "\href{http://www.journals.uchicago.edu/doi/abs/10.1086/676034}{Natural Capital: From Metaphor to Measurement}."
             \emph{Journal of the Association of Environmental Economists}. 1(1/2):1-27.
}
\seealso{
\code{\link{GOM}}
}
