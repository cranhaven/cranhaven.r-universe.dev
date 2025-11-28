\name{fitted_devianceIncr}
\alias{fitted_devianceIncr}
\alias{model_devianceIncr_diff}

\title{Plotting the deviance increment of GAMLSS}
\description{
There are two plotting function here:

i) \code{fitted_devianceIncr()} plots the fitted model deviance components. This is  useful for identifying observations with unusual y-values (given the current fitted  distribution).  


iii) \code{model_devianceIncr_diff} plots the difference of deviance  increments  from two fitted GAMLSS model. This function is useful if the GAIC and the residuals contradict each other. For example the GAIC is better for model 1 but the residuals look lot better for model 2. This can happens if the two distributions are better suited to fit different parts of the response distribution i.e. one model fits the center better but the other fits the tail better.  

}
\usage{

fitted_devianceIncr(obj, plot = TRUE, title, quan.val = 0.99,
         annotate = TRUE, line.col = "steelblue4", 
         point.col = "darkblue", annot.col = "white",
         newdata = NULL)
         

model_devianceIncr_diff(model1, model2, against = "index", 
         tol = 20, data, newdata)

}
\arguments{
 \item{obj}{a GAMLSS fitted object}
 \item{plot}{whether to create just the plot or save also the values with high deviance increment}
 \item{title}{a tittle if needed it.}
 \item{quan.val}{The quantile values of the deviance increment from which the obsrevrvarion should be identify}
 \item{annotate}{whether to plot the quantile values above in the plot.}
   \item{line.col}{the colour of the line}
    \item{point.col}{the colour of the points}
  \item{annot.col}{the colour of the annotation for the deviance increment plot}
  \item{model1}{The first fitted GAMLSS model}
  \item{model2}{The second fitted GAMLSS model}
  \item{against}{you can plot the deviance increment an index, the response or an x-variable}
  \item{tol}{if the absolute value of deviance increment  exceeds the \code{tol} the number of the observation  is plotted}
  \item{data}{The data if can not be found from \code{model1}}
 \item{newdata}{evaluates the function in new data}
}
\details{
The functions are diagnostic tools to check unusual observations in the response.
}  
  
\value{
return a plot
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
Mikis Stasinopulos, Rober Rigby and Fernanda de Bastiani 
}

\seealso{\code{\link{gamlss}}}

\examples{
m1 <- gamlss(R~pb(Fl)+pb(A)+H+loc, data=rent, family=GA )
m2 <- gamlss(R~pb(Fl)+pb(A)+H+loc, data=rent, family=NO )
fitted_devianceIncr(m1)
model_devianceIncr_diff(m1,m2, against="Fl")
}
\keyword{ regression }
