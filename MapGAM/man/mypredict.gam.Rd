\name{mypredict.gam}
\alias{mypredict.gam}
\title{
Prediction for GAM Fits}
\description{
Obtains spatial effects predictions and optionally estimates standard errors and confidence intervals of those predictions from a fitted generalized additive model object.}
\usage{
mypredict.gam(object, newdata, se.fit = FALSE, type=c("all","spatial"),
              reference = "median", level = 0.05,verbose=FALSE) 
}
\arguments{
  \item{object}{
a fitted \code{gam} object.
}
  \item{newdata}{
a data frame containing the values at which predictions are required. This argument can be missing, in which case predictions are made at the same values used to compute the object. Only two-dimentional predictor need be present by name in newdata.
}
  \item{se.fit}{
if TRUE, pointwise standard errors and confidence intervals are computed along with the predictions.
}
  \item{type}{
use \code{type="spatial"} to estimate spatial effect, and use \code{type="all"}(default) to estimate the effect of all covariates included in the model.  
}
  \item{reference}{
the type of reference for the estimated effect. If \code{reference = "median"},  the output will be the estimated effect difference (or log ratio) compared to the median effect. If \code{reference = "mean"},the output will be the estimated effect difference (or log-ratio) compared to the mean effect.If \code{reference} is a data frame indicating a specific geolocation, the output will be the estimated effect difference (or log-ratio) compared to the effect of the geolocation specified by \code{reference}.
} 
  \item{level}{
the siginificance level used when \code{se.fit=TRUE}.
  }
     \item{verbose}{
a logical value indicating whether to print filling values for newdata. The default is \code{verbose = FALSE}.      
}
}


\value{
\item{pred}{the estimated effect difference or (log ratio) compare to the effect specified by \code{reference}.}

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
data(MAdata)							
data(MAmap)
gamgrid <- predgrid(MAdata, map=MAmap)

fit <- gam(Case~lo(Xcoord,Ycoord,span=0.2)+Smoking,data=MAdata,family=binomial())
pred1 = mypredict.gam(fit)
colormap(list(fit=pred1$pred,grid=data.frame(X=MAdata$X,Y=MAdata$Y)),map=MAmap)

pred2 = mypredict.gam(fit,gamgrid)
colormap(list(fit=pred2$pred,grid=data.frame(X=gamgrid$X,Y=gamgrid$Y)),map=MAmap)

pred3 = mypredict.gam(fit,gamgrid,se.fit=TRUE)
colormap(list(fit=pred3$pred,conf.low = pred3$conf.low, conf.high = pred3$conf.high, 
          grid=data.frame(X=gamgrid$X,Y=gamgrid$Y)),map=MAmap,contours = "interval")


}
}

\keyword{misc}
\keyword{smooth}
