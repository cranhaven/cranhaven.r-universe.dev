\docType{data}
\name{GOM}
\alias{GOM}
\title{Reef Fish example: one dimensional stock}
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
}
\strong{functions}: functions for generate simulation data for each nodes
\itemize{
\item \code{effort} effort function
\item \code{catch} catch function
\item \code{profit} profit function (w in Fenichel and Abbott (2014))
\item \code{sdot} evaluated \eqn{\frac{dst}{dt}}
\item \code{dsdotds} evaluated \eqn{\frac{dsdot}{ds}}
\item \code{dsdotdss} evaluated \eqn{\frac{d}{ds}(\frac{dsdot}{ds})}
\item \code{dwds} evaluated \eqn{\frac{dw}{ds}}
\item \code{dwdss} evaluated \eqn{\frac{d}{ds}(\frac{dw}{ds})}
}}
\usage{
## Load dataset
data("GOM")
## Demonstration of example
# demo(GOM, package="capn")
## R-script location
# system.file("demo", "GOM.R", package = "capn")
}
\description{
The \code{GOM} provides data to replicate the Gulf of Mexico Reef Fish example in \href{http://www.journals.uchicago.edu/doi/abs/10.1086/676034}{Fenichel and Abbott (2014)}.
This dataset is consisted of parameters and functions.From Fenichel and Abboott(2014),\cr
\cr
catch effort: \eqn{x(s)=ys^\gamma},\cr
\cr
harvest: \eqn{h(s,x)=q((ys^\gamma)^\alpha)s=q(y^\alpha)(s^{\gamma\alpha})},\cr
\cr
profit: \eqn{w(s,x)=price \cdot h(s,x) - cost \cdot x(s)}, and\cr
\cr
sdot: \eqn{\dot{s} = rs \left( 1 - \frac{s}{k} \right) - q(y^\alpha)(s^{\gamma \alpha + 1})}.\cr
\cr
The parameters in detal are in below.
}
\references{
Fenichel, Eli P. and Joshua K. Abbott. (2014) "\href{http://www.journals.uchicago.edu/doi/abs/10.1086/676034}{Natural Capital: From Metaphor to Measurement}."
             \emph{Journal of the Association of Environmental Economists}. 1(1/2):1-27.
}
