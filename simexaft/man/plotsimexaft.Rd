\name{plotsimexaft}
\alias{plotsimexaft}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Plot the Extrapolation Curve
}
\description{
A function to give the plot of the extrapolation curve for any covariables of the AFT model.
}
\usage{
plotsimexaft(obj, var, extrapolation=c("linear","quadratic","both"), ylimit)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obj}{an object returned by the function "simexaft".}
  \item{var}{a character string of any covariate used in the AFT model.}
  \item{extrapolation}{a character string giving the type of the extrapolation 
  method, the default is set to be linear extrapolation.}
  \item{ylimit}{the y limits of the plot.}
}

\details{
 The green points are the average of estimates of B iteration for each labmda. 
 
 The linear extrapolation curve is in blue, the corresponding SIMEX estimate is the solid red circle.
 
 The quadratic extrapolation curve is in red, the corresponding SIMEX estimate is the solid blue circle.

 The "both" option of the extrapolation method gives both linear and quadratic extrapolation curves.  
}


\author{ Juan Xiong, Wenqing He and Grace Y. Yi}

\seealso{
\code{\link[survival]{survreg}}
}

\examples{
###########example for the dataset with known variance.################
library("simexaft")
library("survival")
data("BHS")
dataset <- BHS
dataset$SBP <- log(dataset$SBP-50)

set.seed(120)
formula <- Surv(SURVTIME,DTHCENS)~SBP+CHOL+AGE+BMI+SMOKE1+SMOKE2

ind <- c("SBP", "CHOL")
err.mat <- diag(rep(0.5625, 2))

### fit an AFT model with quadratic extrapolation
out2 <- simexaft(formula = formula, data = dataset, SIMEXvariable = ind, 
        repeated = FALSE, repind = list(), err.mat = err.mat, B = 50, 
        lambda=seq(0, 2, 0.1),extrapolation="quadratic", dist="weibull")

summary(out2)

plotsimexaft(out2,"SBP","both",ylimit=c(-3,1))

}






