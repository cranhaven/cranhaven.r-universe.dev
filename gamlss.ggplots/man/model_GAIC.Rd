\name{model_GAIC}
\alias{model_GAIC}
\alias{model_GAIC_lollipop}

\title{Plotting GAIC for GAMLSS models}
\description{
The function \code{model_GAIC()} is similar to the function \code{GAIC.scaled()} of the package \pkg{gamlss}. It  produces, [for a given set of different fitted models or for a table produced by \code{chooseDist()}], the scaled Akaike values (see Burnham and Anderson (2002) section 2.9 for a similar concept of the GAIC weights. The plot of the  GAIC's  should not be interpreted as posterior probabilities of models given the data but can be used for model selection purpose since they produce a scaled ranking of the model using their relative importance i.e. from the worst to the best model. 

The function \code{model_GAIC_lollipop()} is almost identical to \code{model_GAIC()}
but the result is a lollipop plot.
}
\usage{
model_GAIC(object, ..., k = 2, c = FALSE, plot = TRUE, 
       which = 1, diff.dev = 1000, text.to.show = NULL, 
       col = "rosybrown", width = 0.9, horiz = TRUE,
       scale = c("[0,1]","[max,min]"), title)

model_GAIC_lollipop(object, ..., k = 2, c = FALSE, plot = TRUE, 
         which = 1, diff.dev = 1000, text.to.show = NULL, 
         col = "skyblue", col.point = "blue", pch.point = 19, 
         width = 0.9, horiz = TRUE, 
         scale = c("[0,1]", "[max,min]"), order.val = TRUE, title)  
}
\arguments{
  \item{object}{
a set of \code{gamlss} fitted model(s) or a matrix table produced by \code{chooseDist()}.
}
  \item{\dots}{
it allows several GAMLSS object to be compared using a GAIC
}
  \item{k}{the penalty with default k=2}
  \item{c}{whether the corrected AIC, i.e. AICc, should be used, note that it applies only when  \code{k=2}}
  \item{plot}{whether to plot with default equal \code{TRUE}}
  \item{which}{which column of GAIC table to plot}
  \item{diff.dev}{this argument applies only a matrix table produced by \code{chooseDist()} and  prevents models with a difference in deviance greater than \code{diff.dev} from the `best' model to be considered (or plotted).}
  \item{text.to.show}{if NULL, \code{model_GAIC()} shows the model names otherwise the character in this list (the length of which should be equal to the length of models)}
  \item{col}{The colour of the bars (or lines. in the lollipop)}
  \item{col.point}{The colour of the points in the lollipop}
   \item{pch.point}{The points character in the lollipop}
  \item{width}{the width of the bars}
  \item{horiz}{whether to plot the bars horizontally (default) or  vertically}
  \item{scale}{the scale of the plot, \code{"[0,1]"} plots the AIC's from the worst to the best models in a scale from  [0,1].  \code{"[max,min]"} plots the AIC's  from the worst model to the best model but in the original scale of the AIC's}
  \item{title}{if different title is needed}
   \item{order.val}{whether to order the models from the best to the worst}
}

\details{The option  allow the AIC to be plotted from worst to best on a 
\eqn{[0,1]}{[0,1]} scale using the formula i.e. \eqn{(AIC_w-AIC_m)/(AIC_w-AIC_b))}{(AIC_w-AIC_m)/(AIC_w-AIC_b))} where the \eqn{AIC_w}{AIC_w} and \eqn{AIC_b}{AIC_b}  are the worst and best AIC, respectively, and \eqn{AIC_m}{AIC_m} is the AIC of the current model.  If the option \code{scale} is set to\code{[max,min]} the  difference \eqn{(AIC_w-AIC_m)}{(AIC_w-AIC_m)} is plotted. 



}

\value{It returns a bar plot using package \pkg{ggplot2}.}

\references{
Burnham K. P.  and  Anderson D. R (2002). \emph{Model Selection and Multimodel Inference
A Practical Information-Theoretic Approach}, Second Edition, Springer-Verlag New York, Inc.

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
\author{Mikis Stasinopoulos, Bob Rigby and Fernanda De Bastiani}


\seealso{ \code{\link[gamlss]{GAIC.scaled}} }

\examples{
data(abdom)
m1 <- gamlss(y~x, family=NO, data=abdom)
m2 <- gamlss(y~x, sigma.fo=~x, family=NO, data=abdom)
m3 <- gamlss(y~pb(x), sigma.fo=~x, family=NO, data=abdom)
m4 <- gamlss(y~pb(x), sigma.fo=~pb(x), family=NO, data=abdom)

model_GAIC(m1,m2, m3, m4)
\donttest{
MT <- chooseDist(m3)
model_GAIC(MT)
model_GAIC(MT, which=2)}
model_GAIC_lollipop(m1,m2, m3, m4)
}

\keyword{regression}
\concept{model selection}
