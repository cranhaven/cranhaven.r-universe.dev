\name{pe_pdf}
\alias{pe_pdf}
\alias{pe_pdf_grid}
\title{Partial Effect of a term on the response distribution}
\description{
The function \code{pe_pdf()}  plots the partial effect that a specified term has on the distribution of the response. 

The function \code{pe_pdf_grid()} plot multiple plots on the same page. 
}
\usage{
pe_pdf(obj = NULL, term = NULL,  from = NULL, to = NULL,
          y.grid.points = 100, x.grid.points = 10, x.values,
          data = NULL, scale = NULL, how = c("median", "last"), 
          scenario = list(), size = 0.1, horizontal = TRUE, 
          col.fill = hcl.colors(lqq, palette = "viridis"), 
          alpha = 0.6, xlim = NULL, title)
      
pe_pdf_grid(model, terms, maxcol = 2, maxrow = 3, ...)      
}
\arguments{
  \item{obj, model}{A GAMLSS object}
  \item{term}{The model term}
  \item{terms}{The model terms, more than one for \code{pe_pdf_grid()}.}
  \item{from}{start from}
  \item{to}{end to }
  \item{y.grid.points}{in how many points the pdf should be evaluates}
  \item{x.grid.points}{in how namy points the terms should be plotted}
  \item{x.values}{possible x values}
  \item{data}{The data used for modelling}
  \item{scale}{This is a very importnat value for plotting correctly the fitted distrutions. If the defaul values it is  not working please try different values}
  \item{how}{How to fixed the rest of the variables. For continuous oit takes the median fot factor the level with the highest frequency.}
  \item{scenario}{Alternatively scenatio for fixing the values.}
  \item{size}{the size of the pdf line}
  \item{horizontal}{whether to plot the partial pdf on the x-axis and the x on the y-axix or opposite}
  \item{col.fill}{how to fill the pdf body}
  \item{alpha}{the transparency factor}
   \item{xlim}{the limits for plotting x-axis}
  \item{title}{whether to use a different tittle from the default one}
    \item{maxcol}{maximum of colomns in the grid for \code{pe_pdf_grid()}}
  \item{maxrow}{maximum of rows on the grid for \code{pe_pdf_grid()}}
   \item{\dots}{extra argument to be passed form \code{pe_pdf()} to \code{pe_pdf_grid()}}
}
\details{
The function \code{pe_pdf()} is one of the function design to help the use to interpret the GAMLSS model. Provides the partial effect that one of the continuous terms has on distribution of the response while the rest of the variables in the model are set on specific values or scenarios. Others similar functions are 
\code{pe_param()}, \code{pe_moment()} and \code{pe_quantile()}. 
}
\value{
A plot of the conditional distribution given the term 
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
\code{\link{pe_param}}
}
\examples{
m1 <- gamlss(R~pb(Fl)+pb(A)+loc+H, data=rent, gamily=GA)
pe_pdf(m1, "A")
pe_pdf(m1, "A")
pe_pdf(m1, "A", horizontal=FALSE)
pe_pdf_grid(m1, c("Fl", "A", "H", "loc"))
}

\keyword{"regression"}
