\name{WPT}

\docType{data}

\alias{WPT}

\title{Weather Prediction Task Data}

\description{

	This data set contains responses of 11 Parkinsons' patients and 13 age-matched
	controls on the Weather Prediction Task. Both groups were tested twice. The PD
	patients were either on or off dopaminergic medication.
	
}

\usage{data(WPT)}

\format{
  A data.frame with 9600 observations on the following variables.
  \describe{
  \item{\code{id}}{a factor with participant IDs}
  \item{\code{group}}{a factor with group IDs (Parksinson's patient or control)}
  \item{\code{med}}{a factor indicating, for the PD patients, whether they were on dopaminergic medicine or not}
  \item{\code{occ}}{a numeric vector with testing occassions}
  \item{\code{trial}}{a numeric vector with trial numbers}
  \item{\code{c1}}{a numeric (binary) vector indicating whether the first cue was present (1) or not (0)}
  \item{\code{c2}}{a numeric (binary) vector indicating whether the second cue was present (1) or not (0)}
  \item{\code{c3}}{a numeric (binary) vector indicating whether the third cue was present (1) or not (0)}
  \item{\code{c4}}{a numeric (binary) vector indicating whether the fourth cue was present (1) or not (0)}
  \item{\code{y}}{a factor with the actual outcome (Rainy or Fine)}
  \item{\code{r}}{a factor with participants' prediction of the outcome}
  }
}

\source{

Speekenbrink, M., Lagnado, D. A., Wilkinson, L., Jahanshahi, M., & Shanks, D. R. (2010). 
Models of probabilistic category learning in Parkinson's disease: Strategy use and the 
effects of L-dopa. \emph{Journal of Mathematical Psychology}, \emph{54}, 123-136.
	
Corresponding author: m.speekenbrink@ucl.ac.uk
	
}

\examples{
data(WPT)

# set up predictors for the different strategies
WPT$sngl <- 0 # singleton strategy
WPT$sngl[WPT$c1 == 1 & rowSums(WPT[,c("c1","c2","c3","c4")]) == 1] <- -1
WPT$sngl[WPT$c2 == 1 & rowSums(WPT[,c("c1","c2","c3","c4")]) == 1] <- -1
WPT$sngl[WPT$c3 == 1 & rowSums(WPT[,c("c1","c2","c3","c4")]) == 1] <- 1
WPT$sngl[WPT$c4 == 1 & rowSums(WPT[,c("c1","c2","c3","c4")]) == 1] <- 1
WPT$sc1 <- 1 - 2*WPT$c1
WPT$sc2 <- 1 - 2*WPT$c2
WPT$sc3 <- -1 + 2*WPT$c3
WPT$sc4 <- -1 + 2*WPT$c4
WPT$mc <- sign(-WPT$c1 - WPT$c2 + WPT$c3 + WPT$c4)

rModels <- list(
	list(GLMresponse(formula=r~-1,data=WPT,family=binomial())),
	list(GLMresponse(formula=r~sngl-1,data=WPT,family=binomial())),
	list(GLMresponse(formula=r~sc1-1,data=WPT,family=binomial())),
	list(GLMresponse(formula=r~sc2-1,data=WPT,family=binomial())),
	list(GLMresponse(formula=r~sc3-1,data=WPT,family=binomial())),
	list(GLMresponse(formula=r~sc4-1,data=WPT,family=binomial())),
	list(GLMresponse(formula=r~mc-1,data=WPT,family=binomial()))
)

transition <- list()
for(i in 1:7) {
  transition[[i]] <- transInit(~1,nstates=7,family=multinomial(link="identity"))
}

inMod <- transInit(~1,ns=7,data=data.frame(rep(1,48)),family=multinomial("identity"))

mod <- makeDepmix(response=rModels,transition=transition, 
	prior=inMod,ntimes=rep(200,48),stationary=TRUE)

\donttest{fmod <- fit(mod)}

}

\keyword{datasets}
