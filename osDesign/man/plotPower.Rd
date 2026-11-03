\name{plotPower}
\Rdversion{1.7}
\alias{plotPower}
\title{
  Plot function for power, based on two-phase and case-control design
}
\description{
 The \code{plotPower} function plots estimates of power obtained from objects returned by either the \code{tpsPower} or \code{ccPower} functions. 
}
\usage{
plotPower(x, coefNum=1, include="All", yAxis=seq(from=0, to=100, by=20),
          xAxis=NULL, main=NULL, legendXY=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    An object in a class \code{tpsPower} or \code{ccPower} obtained as a result of \code{\link{tpsPower}} or \code{\link{ccPower}} functions, respectively.
  }
  \item{coefNum}{
    A numeric vector number specifying the regression coefficient in \code{beta} for the plot.
  }
  \item{include}{
    Character string indicating which estimators from a \code{tpsPower} object are to be printed. The default is "All" in which case all four estimators (two-phase WL, PL, ML and case-control CC) are presented. Other options include "TPS" which solely presents the three two-phase estimators; options "WL", "PL", "ML" and "CC" solely present the corresponding estimators. If the object is of class \code{ccPower} then only the case-control MLE (CC) is presented (i.e., the \code{include} argument is ignored).
}
  \item{yAxis}{
    A scale marking the y-axis for the plot.
  }
  \item{xAxis}{
    A scale marking the x-axis for the plot. If left as the default \code{NULL}, the x-axis scale is taken from \code{nII} in the \code{tpsResults} object.
  }
  \item{main}{
    Title for the plot. 
  }
  \item{legendXY}{
    Optional vector indicating the co-ordinates for the top-left hand corner of the legend box.
  }
}
\details{
Produces a plot of statistical power (to reject a null hypothesis H0: beta = 0), for estimators of a regression coefficient from a logistic regression model, based on a two-phase and/or case-control design.
}
\references{
Haneuse, S. and Saegusa, T. and Lumley, T. (2011) "osDesign: An R Package for the Analysis, Evaluation, and Design of Two-Phase and Case-Control Studies." Journal of Statistical Software, 43(11), 1-29.
}
\author{
  Sebastien Haneuse, Takumi Saegusa
}
\seealso{
  \code{\link{tpsPower}}.
}
\examples{
##
data(Ohio)

##
XM   <- cbind(Int=1, Ohio[,1:3])
fitM <- glm(cbind(Death, N-Death) ~ factor(Age) + Sex + Race, data=Ohio,
            family=binomial)
betaNamesM <- c("Int", "Age1", "Age2", "Sex", "Race")

## Power for the TPS design where phase I stratification is based on Age
##
newBetaM      <- fitM$coef
newBetaM[2:3] <- newBetaM[2:3] / 2
##
\donttest{
powerRaceTPS <- tpsPower(B=10000, betaTruth=fitM$coef, X=XM, N=Ohio$N,
                    strata=4,
                    nII=seq(from=100, to=1000, by=100),
		 betaNames=c("Int", "Age1", "Age2", "Sex", "Race"), monitor=1000)
##
old.par <- par()
par(mfrow=c(2,2))
plotPower(powerRaceTPS, include="TPS", coefNum=2,
          xAxis=seq(from=100, to=1000, by=100),
          main=expression("Age effect (65-74 vs. 55-64 years), " * beta[A1]),
          legendXY=c(800, 65))
plotPower(powerRaceTPS, include="ML", coefNum=2,
          xAxis=seq(from=100, to=1000, by=100),
          main=expression("Age effect (65-74 vs. 55-64 years), " * beta[A1]),
          legendXY=c(800, 65))
plotPower(powerRaceTPS, include="WL", coefNum=2,
          xAxis=seq(from=100, to=1000, by=100),
          main=expression("Age effect (65-74 vs. 55-64 years), " * beta[A1]),
          legendXY=c(800, 65))
plotPower(powerRaceTPS, include="CC", coefNum=2,
          xAxis=seq(from=100, to=1000, by=100),
          main=expression("Age effect (65-74 vs. 55-64 years), " * beta[A1]),
          legendXY=c(800, 65))
##
par(old.par)}

## Power 
##
\donttest{
ccResult <- ccPower(B=1000, betaTruth=newBetaM, X=XM, N=Ohio$N, r=0.5,
                    nCC=seq(from=100, to=500, by=50), betaNames=betaNamesM,
                    monitor=100)
##
par(mfrow=c(2,2))
plotPower(ccResult, coefNum=2, yAxis=seq(from=0, to=100, by=20),
            xAxis=seq(from=100, to=500, by=100),
            main=expression("Age effect (65-74 vs. 55-64 years), " * beta[A1]))
plotPower(ccResult, coefNum=3, yAxis=seq(from=0, to=100, by=20),
            xAxis=seq(from=100, to=500, by=100),
            main=expression("Age effect (75-84 vs. 55-64 years), " * beta[A2]))
plotPower(ccResult, coefNum=4, yAxis=seq(from=0, to=100, by=20),
            xAxis=seq(from=100, to=500, by=100),
            main=expression("Sex effect, " * beta[S]))
plotPower(ccResult, coefNum=5, yAxis=seq(from=0, to=100, by=20),
            xAxis=seq(from=100, to=500, by=100),
            main=expression("Race effect, " * beta[R]))}
}


