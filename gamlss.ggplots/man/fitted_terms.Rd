\name{fitted_terms}
\alias{fitted_terms}
\alias{pe_terms}

\title{Plotting fitted additive terms}

\description{
The function \code{fitted_terms()} is doing what the function \code{term.plot()} is doing for GAMLSS models but it uses \code{ggplot2} package. 
The function \code{pe_terms()} is synonymous to  \code{fitted_terms()} in the package \pkg{gamlss}.
}
\usage{
fitted_terms(object, 
          what = c("mu", "sigma", "nu", "tau"), 
          parameter = NULL, data = NULL, terms = NULL,
          envir = environment(formula(object)), 
          partial = FALSE, rug = FALSE, rug.sides = "b", 
          rug.col = "gray",  alpha = 0.2, 
          ylim = c("common", "free"), xlabs = NULL, 
          ylabs = NULL, main = NULL, term.col = "darkred", 
          resid.col = "lightblue", resid.alpha = 0.8, 
          resid.size = 1, nrow = NULL, ncol = NULL, 
          plots.per.page = 9, one.by.one = FALSE, 
          surface.gam = FALSE, polys = NULL, 
          polys.scheme = "topo", col.ribbon = "darksalmon",
          col.shaded = "gray", ...)

pe_terms(object, 
          what = c("mu", "sigma", "nu", "tau"), 
          parameter = NULL, data = NULL, terms = NULL,
          envir = environment(formula(object)), 
          partial = FALSE, rug = FALSE, rug.sides = "b", 
          rug.col = "gray",  alpha = 0.2, 
          ylim = c("common", "free"), xlabs = NULL, 
          ylabs = NULL, main = NULL, term.col = "darkred", 
          resid.col = "lightblue", resid.alpha = 0.8, 
          resid.size = 1, nrow = NULL, ncol = NULL, 
          plots.per.page = 9, one.by.one = FALSE, 
          surface.gam = FALSE, polys = NULL, 
          polys.scheme = "topo", col.ribbon = "darksalmon",
          col.shaded = "gray", ...)          
          
}
\arguments{
  \item{object}{a gamlss fitted model}
  \item{what}{which distribution parameters, i.e. "mu"}
  \item{parameter}{which distribution parameters (equivalent to \code{what})}
  \item{data}{data frame in which variables in \code{object} can be found}
  \item{terms}{which terms to plot (default NULL means all terms); a vector passed to lpred(..., type = "terms", terms =*)}
  \item{envir}{environment in which variables in \code{object} can be found}
  \item{partial}{logical; should partial residuals be plotted?}
  \item{rug}{add rug plots to the axes}
  \item{rug.sides}{which side the rug "b"=bottom}
  \item{rug.col}{the colour for the rug}
  \item{alpha}{the alpha for the shade}
  \item{ylim}{there are two options here a) "common" and b) "free". The "common"" option plots all figures with the same ylim range and therefore allows the viewer to check the relative contribution of each terms compate to the rest. In the `free' option the limits are computed for each plot seperatly.}
  \item{xlabs}{the x label}
  \item{ylabs}{the y label}
  \item{main}{title NOT WORKING}
  \item{term.col}{the colour of the line for term}
  \item{resid.col}{the colour of the partial residuals}
  \item{resid.alpha}{The alpha for the partial residuals}
  \item{resid.size}{the size of the partial residuals}
  \item{nrow}{the number or rows in a mupliple plot}
  \item{ncol}{the number of rows in a mupliple plot}
  \item{plots.per.page}{the number of plots per page in multiple plots}
  \item{one.by.one}{whether to plot the terms one by one}
  \item{surface.gam}{whether to use surface plot if a ga() term is fitted}
  \item{polys}{for GRMF models}
  \item{polys.scheme}{The polygone information file for MRF models}
   \item{col.ribbon}{he colour of the ribbon}
  \item{col.shaded}{he colour of the shaded area}
  \item{\dots}{for extra argument}
}
\value{A multiple plot}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{https://www.jstatsoft.org/v23/i07/}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC. 

Stasinopoulos, M.D., Kneib, T., Klein, N., Mayr, A. and Heller, G.Z., (2024). \emph{Generalized Additive Models for Location, Scale and Shape: A Distributional Regression Approach, with Applications} (Vol. \bold{56}). Cambridge University Press.

(see also \url{https://www.gamlss.com/}).
}

\author{Mikis Stasinopoulos}

\seealso{
\code{\link[gamlss]{term.plot}}
}

\examples{
data(aids)
a <- gamlss(y ~ pb(x) + qrt, data = aids, family = NBI)
fitted_terms(a, pages = 1)
}

\keyword{regression}
