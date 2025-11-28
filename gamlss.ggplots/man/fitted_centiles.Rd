\name{fitted_centiles}
\alias{fitted_centiles}
\alias{fitted_centiles_legend}
\alias{model_centiles}

\title{
Plotting centile (growth) curves
}
\description{
The function \code{fitted_centiles()} plots centiles curves for distributions belonging to the GAMLSS family of distributions. The plot is equivalent to the standard plot of \code{gamlss:::centiles()} without a legend.

The function \code{fitted_centiles_legend()} plots centiles curves for distributions belonging to the GAMLSS family of distributions and it is  equivalent to the standard plot of \code{gamlss:::centiles()} with a legend. The function  is slower than \code{fitted_centiles()} since in order to plot the legend the data have to expanded.  

The function \code{model_centiles()} plots centile curves for more than one model.  There is no equivalent  plot in the original GAMLSS centile plots  but it perform the same function as \code{gamlss:::centiles.com()}  which compares centiles from different models.


}
\usage{
fitted_centiles(obj, xvar, 
               cent = c(99.4, 98, 90, 75, 50, 25, 10, 2, 0.4), 
               points = TRUE, point.col = "gray", 
               point.size = 1, line.size = 0.8, 
               line.col = hcl.colors(lc, palette = "Dark 2"), 
               line.type = rep(1, length(cent)),
               xlab = NULL, ylab = NULL, title, ...)
               
fitted_centiles_legend(obj, xvar, 
               cent = c(99.4, 98, 90, 75, 50, 25, 10, 2, 0.4),   
               points = TRUE, point.col = "gray", point.size = 1, 
               line.size = 0.8, line.col = hcl.colors(ncent, 
               palette = "Dark 2"), line.type = rep(1, length(cent)),               
               show.legend = TRUE, save.data = FALSE, title, 
               xlab = NULL, ylab = NULL, ...)               

model_centiles(obj, ..., cent = c(97, 90, 75, 50, 25, 10, 3), 
               xvar, xlab = "age", points = TRUE, 
               point.col = gray(0.8), 
               point.size = 0.05, line.size = 0.7, 
               line.col = hcl.colors(ncent,palette = "Dark 2"), 
               ncol = 2, nrow = ceiling(nnames/ncol),  in.one = FALSE,
               title)               
}

\arguments{
  \item{obj}{a fitted gamlss object}
  \item{xvar}{the (unique) explanatory variable}
  \item{cent}{a vector with elements the \% centile values for which the centile curves have to be evaluated (note that the order is from the highest to the lowest so legend and  the plots are maching)}
  \item{points}{whether to plot the points (\code{TRUE}) of the data or not (\code{FALSE})}
  \item{point.col}{the colour of the points}
  \item{point.size}{the zize of the points}
  \item{line.size}{the sized of the centile lines}
  \item{line.col}{the colour of the  centile lines}
  \item{line.type}{the type of line (different types of lines for each centile are working with \code{fitted_centiles_legend})}
  \item{xlab}{the label of the x-axis variable}
  \item{ylab}{the label of the resposnse variable}
   \item{in.one}{whether the \code{model_centile} plot should be one or multiple}
  \item{title}{the title if need it otherwise a dfault title is pronted}
    \item{show.legend}{whether to show the legend}
  \item{save.data}{whether to save the \code{data.frame} of the plot}
  \item{nrow}{the number of rows in the \code{model_centiles()} plot}
   \item{ncol}{the number of columns in the \code{model_centiles()} plot}
  \item{\dots}{for extra  arguments for \code{fitted_centiles()}, and \code{fitted_centiles.legend()} and extra models for \code{model_centiles()}}
}
\details{
 Centiles are calculated using the fitted values in \code{obj} and \code{xvar} must
correspond exactly to the predictor in \code{obj} to plot correctly.
}
\value{
A plot is created 
}
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
\author{
Mikis Stasinopoulos, Bob Rigby and Fernanda de Bastiani
}
\section{Warning }{This function is appropriate only when one continuous explanatory variable is fitted in the model}  

\seealso{   \code{\link[gamlss]{centiles}} }
\examples{
\donttest{
data(abdom)
h<-gamlss(y~pb(x), sigma.formula=~pb(x), family=BCTo, data=abdom) 
h1 <- gamlss(y~pb(x), sigma.formula=~pb(x), family=LO, data=abdom) 
fitted_centiles(h)
fitted_centiles_legend(h)
model_centiles(h, h1)
}
}

\keyword{regression}
\concept{growth curves}
