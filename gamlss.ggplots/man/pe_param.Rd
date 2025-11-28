\name{pe_param}
\alias{pe_param}
\alias{pe_1_param}
\alias{pe_2_param}
\alias{pe_param_grid}

\title{
Partial Effect of a term on the parameters and predictors}
\description{
The function \code{pe_param()} is similar to the function \code{getPEF()} of the \pkg{gamlss} package. It plot the partial effect that a particular term has one of the parameters of the distribution or its predictor \code{eta}. 
The function \code{pe2_param()} is build for partial effects from two terms and it is suitable to display first order interactions.
}
\usage{
pe_param(obj = NULL, term = NULL, data = NULL, n.points = 100, 
               parameter = c("mu", "sigma", "nu", "tau"), 
               type = c("parameter", "eta"), scenario = list(),  
               how = c("median", "last", "fixed"),
               col = "darkblue", size = 1.3, name.obj = NULL,
               rug.plot = TRUE, rug.col = "gray", rug.size = 0.5,  
               data.plot = FALSE, data.col = "lightblue", 
               data.size = 0.1, factor.size = 15,
               data.alpha = 0.9, bins = 30, 
               filled = FALSE, ylim = NULL,
                    title) 

pe_1_param(obj = NULL, term = NULL, data = NULL, n.points = 100,  
                parameter = c("mu", "sigma", "nu", "tau"), 
                type = c("parameter", "eta"),
                how = c("median", "last", "fixed"),
                scale.from = c("mean", "median", "none"),
                scenario = list(), col = "darkblue", size = 1.3,
                name.obj = NULL, data.plot = FALSE, 
                data.col = "lightblue",data.size = 0.1,
                data.alpha = 0.9, rug.plot = TRUE, rug.col = "gray",
                rug.size = 0.5, factor.size = 15,
                ylim = NULL, title) 

pe_2_param(obj = NULL, terms = NULL, data = NULL, n.points = 100, 
                parameter = c("mu", "sigma", "nu", "tau"), 
                type = c("parameter", "eta"),
                how = c("median", "last", "fixed"),
                scenario = list(), col = "darkblue",
                size = 1.3, data.plot = TRUE,
                data.col = "lightblue", data.size = 0.1,
                data.alpha = 0.9,bins = 30, 
                filled = FALSE, name.obj = NULL, title) 

pe_param_grid(model, terms, maxcol = 2, maxrow = 3, ylim=NULL, ...)
}
\arguments{
  \item{obj}{a GAMLSS fitted object}
   \item{model}{a GAMLSS fitted model}
  \item{term}{the model term we want to investigate can be one i.e. "Fl" or two 
  c("Fl", "A")}
  \item{terms}{a list of  model terms for example 
  \code{list(c("Fl","A"), "H", "loc" ))} }
  \item{data}{the data frame used otherwise it takes it from the fitted model}
  \item{n.points}{the number of points for the evaluation of the term}
  \item{parameter}{the distribution parameter in which  the term is fitted}
  \item{type}{here you specify or the distribution  parameter i.e        \code{"parameter"} or its  prediction, \code{"eta"}}
  \item{how}{how to set all the other terms in the model}
  \item{scenario}{this can be a list of values for the rest of the terms in the model for the distribution parameter}
  \item{plot}{whether to plot the result}
  \item{col}{the colour of the partial effect of the term}
  \item{size}{the size of the line of partial effect of the term}
  \item{bins}{the number of binds for the contour plot}
  \item{filled}{whether to display the values  in the contour}
  \item{title}{the title if different from the default}
  \item{name.obj}{this is a way to pass the name of the object}
  \item{maxcol}{the maximum columns in the grid plot}
  \item{maxrow}{the maximum rowss in the grid plot}
  \item{data.plot}{whether to plot the data}
  \item{rug.plot}{whether to print the rug bellow the figure}
  \item{rug.size}{the size of the rug}
  \item{rug.col}{the colour of the rug}
  \item{data.col}{the color of the data points}
  \item{data.size}{the size of the data points}
  \item{data.alpha}{the trnsparance constant of the data points}
  \item{factor.size}{the size of the symbol if a factor is plotted}
  \item{ylim}{if a common y limit is required}
  \item{scale.from}{whethet to substact from the mean the median or from zero}
  \item{\dots}{for passing argument from the function \code{pe_param_grid} to the function \code{pe_param}}
  
}
\details{
The functions \code{pe_param()} and \code{pe_param_grid()} can be used to help the use the  interpretation of a GAMLSS model. 
The functions \code{pe_param()} provides the partial effect of one or two   terms  of a specified  parameter of the distribution while the rest of the terms in the model are set on specific values or scenarios. The function \code{pe_param()} calls \code{pe_1param()} if the argument terms is one i.e. "Fl" or the function 
\code{pe_2param()} if the terms are two i.e. c("Fl"","A"). 
The \code{pe_param_grid()} plots multiple plots specified by the list used in the \code{term} argument. 

Similar functions are 
\code{pe_quantile()} and \code{pe_moment()}. 
}
\value{
It is plotting the partial effect or is producing the resulting function
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

\seealso{
\code{\link[gamlss]{getPEF}}
}
\examples{
m1 <- gamlss(R~pb(Fl)+pb(A)+loc+H, data=rent, gamily=GA)
pe_param(m1, "A")
pe_param(m1, c("Fl","A"), filled=TRUE)
pe_param_grid(m1, list(c("Fl","A"), c("H","loc")), filled=TRUE)
# the terms are additive no interaction
}

\keyword{ regression }
