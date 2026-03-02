\name{apc.hypothesis}
\alias{apc.hypothesis}
\title{Imposing hypotheses on age-period-cohort models.}
\description{\code{apc} has a set of standard hypotheses that can be imposed on the age-period-cohort model.
	A deviance table can be found on 
	\code{\link{apc.fit.table}},
	while fits of restricted models can be found using 
	\code{\link{apc.fit.model}}.

	Other linear hypotheses can be imposed using a little bit of coding, see the vignette
	\href{../doc/NewDesign.pdf}{\code{NewDesign.pdf}},
	\href{../doc/NewDesign.R}{\code{NewDesign.R}}
	on
	\href{../doc/index.html}{\code{Vignettes}}.		

	For over-dispersed Poisson models for responses and no doses the theory is worked out in 
	Harnau and Nielsen (2017).

In general forecasts from age-period-cohort models require extrapolation of the estimated parameters.
	This has to be done without introducing identifications problems, see
	Kuang, Nielsen and Nielsen (2008b,2011).
	There are many different possibilities for extrapolation for the different sub-models.
	The extrapolation results in point forecasts.
	Distribution forecasts should be build on top of these, see
	Martinez Miranda, Nielsen and Nielsen (2015)
	and
	Harnau and Nielsen (2016).
	At present three experimental functions
	\code{\link{apc.forecast.ac}},
	\code{\link{apc.forecast.apc}}
	and
	\code{\link{apc.forecast.ap}}
	are available.
}
\references{
Harnau, J. and Nielsen (2016) Over-dispersed age-period-cohort models. To appear in \emph{Journal of the American Statistical Association}. \emph{Download}: \href{https://www.nuffield.ox.ac.uk/economics/papers/2017/HarnauNielsen2017apcDP.pdf}{Nuffield DP}

Kuang, D., Nielsen, B. and Nielsen, J.P. (2008b) Forecasting with the age-period-cohort model and the extended chain-ladder model. Biometrika 95, 987-991. \emph{Download}: \doi{https://doi.org/10.1093/biomet/asn038}; Earlier version \href{http://www.nuffield.ox.ac.uk/economics/papers/2008/w9/KuangNielsenNielsen_Forecast.pdf}{Nuffield DP}.

Kuang, D., Nielsen B. and Nielsen J.P. (2011) Forecasting in an extended chain-ladder-type model. Journal of Risk and Insurance 78, 345-359. \emph{Download}: \doi{https://doi.org/10.1111/j.1539-6975.2010.01395.x}; Earlier version: \href{http://www.nuffield.ox.ac.uk/economics/papers/2010/w5/Forecast24jun10.pdf}{Nuffield DP}. 

Martinez Miranda, M.D., Nielsen, B. and Nielsen, J.P. (2015) Inference and forecasting in the age-period-cohort model with unknown exposure with an application to mesothelioma mortality. \emph{Journal of the Royal Statistical Society} A 178, 29-55. \emph{Download}: \doi{https://doi.org/10.1111/rssa.12051}, \href{http://www.nuffield.ox.ac.uk/economics/papers/2013/Asbestos8mar13.pdf}{Nuffield DP}. 
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 10 Sep 2016 (1 Feb 2016)}