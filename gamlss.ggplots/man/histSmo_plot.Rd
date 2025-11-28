\name{histSmo_plot}
\alias{histSmo_plot}

\title{Supporting histSmo()}
\description{
This function helps to plot density estimates created by the \code{histSmo()} function.
}
\usage{
histSmo_plot(x, col_fill_bar = gray(0.5), col_bar = "pink", 
        col_line = "darkblue", width_line = 1, title, xlabel)
}
\arguments{
  \item{x}{a \code{histSmo} object}
  \item{col_fill_bar}{The fill colour of the bars}
  \item{col_bar}{the colour of the border of thebars}
  \item{col_line}{the colour of the lines}
  \item{width_line}{the width of the lines}
  \item{title}{title if needed}
  \item{xlabel}{x axis lable if needed.}
}
\details{
This function supports  \code{histSmo()}.
}
\value{
A plot
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
\author{
Mikis Stasinopulos, Rober Rigby and Fernanda de Bastiani 
}

\seealso{
\code{\link[gamlss]{histSmo}}
}
\examples{
a1 <-histSmo(abdom$y)
gg1 <-histSmo_plot(a1)
gg1
}

\keyword{regression}
