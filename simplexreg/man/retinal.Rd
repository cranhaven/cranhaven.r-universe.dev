\name{retinal}
\alias{retinal}
\docType{data}
\title{
  Data on recorded decay of intraocular gas in complex retinal surgeries
}
\description{The study recorded the decay of intraocular gas \eqn{C_3F_8} in complex retinal surgeries 
following initial injection in an ophthalmology study, reported in Meyers \emph{et al.} (1992). The outcome
variable was the percent of gas left in the eye. The gas, with three different concentration levels, 15\%, 
20\% and 25\% was injected into the eye before surgery for 31 patients. They were then followed three to eight
(average of five) times over a three-month period, and the volume of gas in the eye at the follow-up times
were recorded as a percentage of the initial gas volume. The primary interest was to investigate whether 
concentration levels of the gas injected in patients' eyes affect the decay rate of the gas. 
}
\usage{data("retinal")}
\format{
   	A data frame with 181 observations on the following 6 variables. 
   	\describe{
   	   	\item{\code{Gas}}{Percentage of the initial gas volume left}
		\item{\code{Time}}{Time covariate of days after the gas injection}
		\item{\code{LogT}}{Logarithm of \code{Time}}
		\item{\code{LogT2}}{Square of \code{LogT}}
		\item{\code{Level}}{Concentration levels of the initial intraocular gas. For each patient, 
			\code{Level = -1} if the gas concentration level is 15\%, \code{Level = 0} if 20\%, or 
			\code{1} if 25\%. 
		}
		\item{\code{ID}}{A factor indicating patients.}
   	}
}
\references{
Meyers, S. M. and Ambler, J. S. and Tan, M. (1992) Variation of Perfluorpropane 
   Disappearance after Vitrectomy. \emph{ Retinal,} \bold{ 12:} 359--363
   
Zhang, P. and Qiu, Z. and Shi, C. (2016) {simplexreg}: An {R} Package for Regression
	Analysis of Proportional Data Using the Simplex Distribution. \emph{ Journal of Statistical Software,}
	\bold{ 71:} 1--21   
}