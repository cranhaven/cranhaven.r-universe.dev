\name{predict.gamcox}
\alias{predict.gamcox}
\title{
Prediction Method for \code{gamcox} Fits}
\description{
Obtains spatial effects predictions and optionally estimates standard errors and confidence intervals of those predictions from a fitted Cox proportional hazard additive model object.}
\usage{
## S3 method for class "gamcox"
\method{predict}{gamcox} (object, newdata = object$data, se.fit = FALSE, type=c("spatial","all"),
        reference = "median", level = 0.05, verbose=FALSE,\dots) 
}
\arguments{
  \item{object}{
a fitted \code{gamcox} object.
}
  \item{newdata}{
a data frame containing the values at which predictions are required. This argument can be missing, in which case predictions are made at the same values used to compute the object. A comprehensive effect (hazard ratio) of the covariates included in \code{newdata} will be predicted.
}
  \item{se.fit}{
if \code{TRUE}, pointwise standard errors and confidence intervals are computed along with the predictions.
}
  \item{type}{
use \code{type="spatial"}(default) to estimate spatial effect, and use \code{type="all"} to estimate the effect of all covariates included in the model.  
}
  \item{reference}{
the type of reference for the estimated effect. If \code{reference = "median"},  the output will be the estimated hazard ratio (or log-ratio) compared to the median effect. If \code{reference = "mean"},the output will be the estimated hazard ratio (or log-ratio) compared to the mean effect.If \code{reference} is a data frame indicating a specific geolocation, the out put will be the estimated hazard ratio (or log ratio) compared to the hazard of the geolocation specified by \code{reference}.
} 
  \item{level}{the confidence level for condifence bands.}
   \item{verbose}{
a logical value indicating whether to print filling values for newdata. The default is \code{verbose = FALSE}.      
}
  \item{\dots}{extra arguments for S3 generic, ignored by \code{predict.gamcox}.}   
}


\value{
\item{pred}{the estimated log hazards ratio compared to the effect specified by \code{reference}.}

\item{se}{
the standard errors along with the predictions.
}
\item{conf.low}{the lower bounds for pointwise (1-\code{level}) confidence intervals.}
\item{conf.high}{the higher bounds for pointwise (1-\code{level}) confidence intervals.}
}

\references{
Hastie TJ, Tibshirani RJ.  Generalized Additive Models.  (Chapman & Hall/CRC Monographs on Statistics & Applied Probability, Boca Raton, Florida, 1990).  
}

\author{
Lu Bai

Send bug reports to \email{sbartell@uci.edu}.
}
 

\seealso{
\code{\link{modgam}}, 
\code{\link{predict.gamcox}}.
}
\examples{
\donttest{
data(CAdata)
data(CAmap)
fit <- gamcox(Surv(time,event)~AGE + factor(INS) + lo(X,Y),data=CAdata,
       span=0.2,loess.trace="approximate")
fit
pred1 = predict(fit)
colormap(list(fit=pred1$pred,grid=data.frame(X=CAdata$X,Y=CAdata$Y)),map=CAmap,
         border.gray=0.5)

data(CAgrid)
pred2 = predict(fit,CAgrid[,c("X","Y")])
colormap(list(fit=pred2$pred,grid=data.frame(X=CAgrid$X,Y=CAgrid$Y)),map=CAmap,
        border.gray=0.5, legend.name="log hazard ratio")

## Circle significant areas based on the confidence intervals specified by conf.low and conf.high
pred3 = predict(fit,CAgrid[,c("X","Y")],se.fit=TRUE)
colormap(list(fit=pred3$pred,conf.low = pred3$conf.low, conf.high = pred3$conf.high,
          grid=data.frame(X=CAgrid$X,Y=CAgrid$Y)),map=CAmap,border.gray = 0.7,
          contours = "interval",legend.name="log hazard ratio")
}
}

\keyword{misc}
\keyword{smooth}
