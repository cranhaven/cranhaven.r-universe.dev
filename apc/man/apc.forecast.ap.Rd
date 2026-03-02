\name{apc.forecast.ap}
\alias{apc.forecast.ap}
\title{Forecast for Poisson response model with AP structure.}
\description{Computes forecasts for a model with AP structure.
The data can have any form allowed in, see \code{\link{apc.data.list}}. These are all special cases of
generalised trapezoids. If the "lower triangle" with the
largest (age,coh) values are not observed, they can be forecast using this function.
The function extrapolates the AP model to the lower triangle where
\code{per.zero+per.max < per <= age.max+coh.max-1}.
The estimates of the age parameters can be used for the lower triangle.
The estimates of the period parameters need to be extrapolated for the lower triangle.
Thus, the function extrapolates
\code{per.forecast.J=age.max+coh.max-1-per.zero-per.max}
period values. 
The extrapolation method has to chosen so as not to introduce an identification problem, see
Kuang, Nielsen and Nielsen (2008b,2011).
Two such extrapolation methods are implemented in this function: "I0" and "I1".
The default is to report the linear predictor.

If the \code{model.family="binominal.dose.response"}, that is a logistic model,
then forecasts of dose, response and survival probability are given for lower triangle.
}
\usage{apc.forecast.ap(apc.fit,extrapolation.type="I0",suppress.warning=TRUE)}
\arguments{
  \item{apc.fit}{List. Output from \code{\link{apc.fit.model}}.
  					Note: apc.fit.model should be run with AP structure so that 
					\code{apc.fit$model.design=="AP"}.
					Only point forecasts of the linear predictor are provided.}
  \item{extrapolation.type}{Character.  Choices for extrapolating the differenced period parameter ("Delta.beta_per").
  							Default is "I0".
  							\describe{
  								\item{"I0"}{extrapolates the first out-of-sample differenced period parameter by the average of
								cumulated sums of the in-sample estimated differenced period parameters.
								The subsequent out-of-sample differenced period parameters are zero.}
								\item{"I1"}{extrapolates all out-of-sample differenced period parameters by zero.}
							}
							Both methods are invariant to ad hoc identification of the implied period time effect, by
							following the ideas put forward in
							Kuang, Nielsen and Nielsen (2008b).
							Internally, the extrapolation is done as follows.
							The estimated differenced period parameters are found from
							"apc.fit$coefficients.canonical" using
							\code{\link{apc.identify}} with \code{type="dif"}.
							These imply period time effects by ad hoc identification:
							choose an arbitrary value for the first period time effect
							and add partial sums of the differenced period parameter. Fit a time series model:
							an intercept model with "I0" and a random walk model for "I1".
							Then extrapolate and take differences.  These extrapolation methods are invariant to the
							actual choice of the arbitrary value for the first period time effect.}
	\item{suppress.warning}{Logical. If true, suppresses warnings from \code{\link{apc.data.list.subset}}, which is called internally. Default is "TRUE".}							
}
\value{
  \item{trap.predictors.forecast}{Matrix. Includes estimates and point forecasts of linear predictor. That is design*coefficient.
				Same as the \code{\link{glm.fit}} value \code{linear.predictors} when there is no offset.
			  	Forecasts in lower right triangle. Trapezoid format.}
  \item{index.trap.J}{Matrix. age-coh coordinates for forecast area. Similar structure to
  					\code{index.trap} in \code{apc.index}, see \code{\link{apc.get.index}}.}
  \item{D.xi.per.extrapolated}{Matrix.  Extrapolated parameters.  Dimension \code{per.forecast.J=age.max+coh.max-1-per.zero-per.max} rows, 1 column.} 					
  \item{trap.dose.forecast}{Matrix. Includes data and point forecasts. Forecasts in lower right triangle.        
				Dose in cell age,coh equal to dose in cell age-1,coh minus response in cell age-1,coh.
				Only implemented for \code{model.family="binomial.dose.response"}. See details.}
  \item{trap.response.forecast}{Matrix. Includes data and point forecasts. Forecasts in lower right triangle.      
				Response in cell age,coh equal to dose in cell age,coh times 1 minus probability of surviving in that cell.
				Only implemented for \code{model.family="binomial.dose.response"}. See details.}
  \item{trap.survival.forecast}{Matrix. Point forecasts. Forecasts in lower right triangle 
				Probability of surviving computed from \code{trap.predictors.forecast} using logistic link function.
				Only implemented for \code{model.family="binomial.dose.response"}. See details.}
}
\details{When \code{model.family=binomial.dose.response} forecasts are made by the component method, see Cox (1976).
	It is intended to be used for a population analysis situation where the response equals cohort-decrease of dose. 
	For cell in forecast array with index \code{(age,cohort)} then:
	Survival probability is \code{survival=1/(1+exp(predictor_(a,c)))}.
	Dose is \code{dose_(a,c)=max(0,dose_(a-1,c)-response_(a-1,c))}.
	Response is \code{response_(a,c)=dose_(a,c)*(1-survival_(a,c))}.	
}
\references{
Cox, P.R. (1976) Demography. 5th Edition. Cambridge: Cambridge University Press. (page 324).

Kuang, D., Nielsen, B. and Nielsen, J.P. (2008b) Forecasting with the age-period-cohort model and the extended chain-ladder model. Biometrika 95, 987-991. \emph{Download}: \doi{https://doi.org/10.1093/biomet/asn038}; Earlier version \href{http://www.nuffield.ox.ac.uk/economics/papers/2008/w9/KuangNielsenNielsen_Forecast.pdf}{Nuffield DP}.

Kuang, D., Nielsen B. and Nielsen J.P. (2011) Forecasting in an extended chain-ladder-type model. Journal of Risk and Insurance 78, 345-359. \emph{Download}: \doi{https://doi.org/10.1111/j.1539-6975.2010.01395.x}; Earlier version: \href{http://www.nuffield.ox.ac.uk/economics/papers/2010/w5/Forecast24jun10.pdf}{Nuffield DP}. 
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 2 May 2016 (2 Mar 2016)}