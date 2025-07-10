\name{AllCurves}
\alias{AllCurves}

\title{
Lactation Curve Parameter Estimation
}

\description{
AllCurves runs multiple lactation curve models and extracts selection criteria (RSE, R2,
log likelihood, AIC, AICC, and BIC) for each model.
}
\usage{
AllCurves(x, trait, dim)
}

\arguments{
  \item{x}{data frame containing londitudinal trait records	and time points of record collection}
  \item{trait}{specifies the column containing longitudinal trait records}
  \item{dim}{specifies the column containing time points}
}

\value{Output
\item{modelnames}{provides names and order number of models}
\item{model}{gives model equation, default starting parameters, and other model specifications}
\item{critall}{gives selection criteria for all models sorted from best to worst accroding to specified criteria}
\item{modeldescrip}{gives RSS, RSD, and F-value for each model}
\item{critbest}{gives all selection	criteria for best model}
\item{bestmodel}{gives model equation for best model for each selection criterion}
\item{Error}{gives a Warning if model failed to converge}
\item{ModelParam}{gives a list of three tables containing the converged model parameters}
\item{summary*}{gives the summary of a particular model. summary1 for example give the summary of the first model by Michaelis and Menten}
}

\author{
Eva M. Strucken
}

\note{
lactcurved requires installation of packages polynom, orthopolynom, and splines.

In general, if a lactation curve model is linear, it can be fitted directly in a test-day (TD)
model. If lactation curve model is non-linear, parameters need to be estimated first and then
fitted in a TD model with other fixed and random effects if required.

In lactcurves, starting parameters (defined in start) have been optimized across the first three
lactations of 1.7 million Holstein Friesian dairy cows. If a lactation curve model fails to
converge with other data, it is recommended to extract the model code and optimize the start values
for the new data.
}

\examples{
## create data set for 3 individuals with milk yield records on 24 days

ID=c(rep("ID123",24),rep("ID456",24),rep("ID789",24))

dim=as.integer(rep(seq(from=5, to=340, by=14),3))

mkg=as.numeric(c(23.4,28.3,30.5,31.3,31.5,31.3,30.9,30.5,30.1,29.6,29.1,28.7,28.2,27.7,27.2,26.7,
26.2,25.7,25.2,24.7,24.2,23.7,23.2,22.8,
21.3,25.7,26.9,27.2,26.9,26.5,26.1,25.6,25.1,24.6,24.1,23.6,23.1,22.6,22.1,21.6,21.1,20.6,20.1,
19.6,19.1,18.6,18.1,17.6,
22.0,26.5,28.1,28.4,28.2,27.9,27.4,26.9,26.4,25.9,25.4,24.9,24.4,23.9,23.4,22.9,22.4,21.9,21.4,
20.9,20.4,19.9,19.4,18.9))

data=cbind.data.frame(ID,dim,mkg)

## run example

library(polynom)
library(orthopolynom)
library(splines)

output=AllCurves(data,mkg,dim)

output$critall
output$modeldescrip
output$critbest
output$bestmodel
output$Error
output$ModelParam
output$summary17b

## plot curve
# set the number of days to consider
dim=c(1:340)

# look up the model and its estimated parameters
output$summary17b

# use model and parameters to plot curve
plot(19.293701+(31.358471-19.293701)*(1-exp(1)^(-0.059874*dim))-0.035495*dim)
}

\section{Citation}{
Strucken, E.M. (2021). lactcurves: Lactation Curve Parameter Estimation. R package version 1.1.0
}


% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }% use one of  RShowDoc("KEYWORDS")
\keyword{ models & regression }% __ONLY ONE__ keyword per line
\keyword{ models & nonlinear }
\keyword{ ts }
