\name{apc.forecast.apc}
\alias{apc.forecast.apc}
\title{Forecast models with APC structure.}
\description{Computes forecasts for a model with APC structure.
Forecasts of the linear predictor are given for all models.
This is done for the triangle which shares age and cohort indices with the data.}
\usage{apc.forecast.apc(apc.fit,extrapolation.type="I0",
suppress.warning=TRUE)}
\arguments{
  \item{apc.fit}{List. Output from \code{\link{apc.fit.model}}.
  					Note: apc.fit.model should be run with APC structure so that 
					\code{apc.fit$model.design=="APC"}.
					Point forecasts of the response are only provided for a Poisson response model where
					\code{apc.fit$model.family=="poisson.response"}
					and for an over-disperse Poisson response model where
					\code{apc.fit$model.family=="od.poisson.response"}.
					For other models only point forecasts of the linear predictor are provided, that is
					the first two values \code{linear.predictors.forecast} and \code{index.trap.J}.}
  \item{extrapolation.type}{Character.  Choices for extrapolating the differenced period parameter ("Delta.beta_per").
  							Default is "I0".
  							\describe{
								\item{"I2"}{Extrapolates future DDbeta by 0.}
								\item{"I1"}{Extrapolates future DDbeta as follows.
								Compute Dbeta=cumsum(DDbeta) for j=3,...,J.  
								This determines Dbeta upto arbitrary level.   
								Compute average mean(Dbeta).                  
								Forecast DDbeta[J+1]=mean(Dbeta)-Dbeta[J].    
								Forecast DDbeta[J+h]=0 for h>1.
								This forecast is invariant to arbitrary level.}
								\item{"I0"}{Extrapolates future DDbeta as follows.
								Compute beta=cumsum(cumsum(DDbeta)) for j=3,...,J.                 
								This determines beta upto arbitrary linear trend.                  
								Regress on 1 and demeaned trend=j-(n+1)/2                          
								giving estimates mu1 and mu2.                                      
								Forecast beta[J+1]=mu1 + mu2*(n+1-(n+1)/2).                        
								Forecast beta[J+2]=mu1 + mu2*(n+2-(n+1)/2).                        
								Forecast DDbeta[J+h]=beta[J+h]-2*beta[J+h-1]+beta[J+h-2] for h=1,2.
								Forecast DDbeta[J+h]=0 for h>2.                                    
								This forecast is invariant to arbitrary linear trend.}             
							}
							All methods are invariant to ad hoc identification of the implied period time effect, by
							following the ideas put forward in
							Kuang, Nielsen and Nielsen (2008b).}
  \item{suppress.warning}{Logical. If true, suppresses warnings from \code{\link{apc.data.list.subset}}, which is called internally. Default is "TRUE".}											
}
\value{
  \item{linear.predictors.forecast}{Vector. Linear predictors for forecast area.}
  \item{index.trap.J}{Matrix. age-coh coordinates for vector. Similar structure to
  					\code{index.trap} in \code{apc.index}, see \code{\link{apc.get.index}}.}
  \item{trap.response.forecast}{Matrix. Includes data and point forecasts. Forecasts in lower right triangle. Trapezoid format.}
  \item{response.forecast.cell}{Matrix. 4 columns.
					1: Point forecasts.
					2: corresponding forecast standard errors
					3: process standard errors
					4: estimation standard errors
					Note that the square of column 2 equals the sums of squares of columns 3 and 4
					Note that \code{index.trap.J} gives the age-coh coordinates for each entry.} 
  \item{response.forecast.age}{Same as \code{response.forecast.cell},
  					but point forecasts by \code{age} cumulated over period/cohort.}
  \item{response.forecast.per}{Same as \code{response.forecast.cell},
  					but point forecasts by \code{per} cumulated over age/cohort.}
  \item{response.forecast.coh}{Same as \code{response.forecast.cell},
  					but point forecasts by \code{coh} cumulated over age/period.}
  \item{response.forecast.all}{Same as \code{response.forecast.cell},
  					but point forecasts cumulated by \code{age} and \code{coh}.}
  \item{xi.per.dd.extrapolated}{The extrapolated double differences.}					
  \item{xi.extrapolated}{The extrapolated parameters.}					
}
\details{The example below is based on the smaller data reserving sets
	\code{\link{data.loss.TA}}.
}
\references{
Kuang, D., Nielsen, B. and Nielsen, J.P. (2008b) Forecasting with the age-period-cohort model and the extended chain-ladder model. Biometrika 95, 987-991. \emph{Download}: \href{https://doi.org/10.1093/biomet/asn038}{Article}; Earlier version \href{http://www.nuffield.ox.ac.uk/economics/papers/2008/w9/KuangNielsenNielsen_Forecast.pdf}{Nuffield DP}.
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 10 Sep 2016}
\seealso{The example below uses Taylor and Ashe reserving see \code{\link{data.loss.TA}}}
\examples{
#####################
#  EXAMPLE with reserving data: data.loss.TA()

data	<- data.loss.TA()
fit.apc	<- apc.fit.model(data,"poisson.response","APC")
forecast	<- apc.forecast.apc(fit.apc)

#	forecasts by "policy-year"
forecast$response.forecast.coh
#	         forecast
#	coh_2    91718.82
#	coh_3   464661.38
#	coh_4   704591.94
#	coh_5  1025337.23
#	coh_6  1503253.81
#	coh_7  2330768.44
#	coh_8  4115906.56
#	coh_9  4257958.30
#	coh_10 4567231.84
#	forecasts of "cash-flow"
forecast$response.forecast.per
#	        forecast
#	per_11 5274762.58
#	per_12 4213526.23
#	per_13 3188451.80
#	per_14 2210649.45
#	per_15 1644203.06
#	per_16 1236495.32
#	per_17  764552.75
#	per_18  444205.71
#	per_19   84581.44
#	forecast  of "total reserve"
forecast$response.forecast.all
#	    forecast
#	all 19061428
}