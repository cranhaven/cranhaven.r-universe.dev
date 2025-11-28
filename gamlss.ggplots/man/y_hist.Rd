\name{y_hist}
\alias{y_hist}
\alias{y_pacf}
\alias{y_acf}
\alias{y_dots}
\title{Histogram and density plot.}
\description{
The function \code{y_hist()} creates a histogram and a density plot for a continuous variable.

The functions \code{y_acf()} and \code{y_pacf()} plot the autocorrolation and partial autocorrolation functions  for y.

The function \code{y_dots()} is design for long tail right skewed variables. It is  a plot emphasising  the right tail of the distribution for such variables. 

}
\usage{
y_hist(y, data, with.density = TRUE, hist.col = "black",
         hist.fill = "white", dens.fill = "#FF6666", 
         binwidth = (max(y)-min(y))/20,  from, to, title)
         
y_acf(x, data, title)

y_pacf(x, data, title)

y_dots(y, data, value=3, point.size = 2, point.col = "gray", 
          quantile = c(.10, .50, .90),
          line.col = c("black","red", "black"),
          line.type = c("dotted", "solid",  "dotted"),
          line.size = c(1,1,1), x.axis.col = "black", 
          x.axis.line.type = "solid", seed = 123, from, to, title)

}
\arguments{
  \item{y,x}{a continuous variable}
  \item{data}{where to find argument y}
  \item{value}{value to identify outliers i.e. for upper tail an outliers is if it is greater than Q_3+value*IQ} 
  \item{with.density}{whether a density is required, default is \code{TRUE}}
  \item{hist.col}{the colour of lines of the histogram}
  \item{hist.fill}{the colour of the histogram}
  \item{dens.fill}{the color of the density plot}
  \item{binwidth}{the binwidth for the histogram}
  \item{from}{where to start the histogram (you may have to change 
         \code{binwidth})  }
  \item{to}{where to finish the histogram  (you may have to change 
          \code{binwidth}) }
  \item{point.size}{the size of the points in \code{y_dots}}
  \item{point.col}{the colour of the points in \code{y_dots}}
   \item{quantile}{the quantile values to plot in \code{y_dots}, the default is  0.10, .50 and .90 }
  \item{line.col}{the color of the vertical lines  indicating the 0.10, .50 and .90 quantiles in \code{y_dots}}
  \item{line.type}{the type of the verical lines  indicationg the 0.10, .50 and .90 quantiles in \code{y_dots}} 
  \item{line.size}{the size of the verical lines  indication the 0.10, .50 and .90 quantiles in \code{y_dots}} 
  \item{x.axis.col}{the colour of the x-axis}
  \item{x.axis.line.type}{the type of the x-axix}
  \item{seed}{the seed to  jitter  the y}
  \item{title}{use this for a different title}
}

\value{
 A ggplot is returned
}
\references{
Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{https://www.jstatsoft.org/v23/i07/}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC.  

Stasinopoulos, M.D., Kneib, T., Klein, N., Mayr, A. and Heller, G.Z., (2024). \emph{Generalized Additive Models for Location, Scale and Shape: A Distributional Regression Approach, with Applications} (Vol. \bold{56}). Cambridge University Press.

(see also \url{https://www.gamlss.com/}).
}
\author{Mikis Stasinopoulos, Bob Rigby and Fernanda De Bastiani}

\seealso{\code{\link{plot.ecdf}}}
\examples{
library(ggplot2)
y <- rBCT(1000, mu=3, sigma=.1, nu=-1, tau=5)
y_hist(y)
gg <- y_hist(y, with.dens=FALSE)
gg + stat_function(fun = dBCT, args=list(mu=3, sigma=.1,  nu=-1, tau=5), 
                 colour = "black")
gg + stat_function(fun = dBCT, args=list(mu=3, sigma=.1,  nu=-1, tau=5), 
                  geom = "area", alpha=0.5, fill="pink", color="black", n=301)
                  
y_acf(diff(EuStockMarkets[,1]))    

y_dots(rent$R)
}
\keyword{regression}
