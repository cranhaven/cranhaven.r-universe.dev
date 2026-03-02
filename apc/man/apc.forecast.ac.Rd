\name{apc.forecast.ac}
\alias{apc.forecast.ac}
\title{Forecast for responses model with AC or CL structure.}
\description{Computes forecasts for a model with AC or Chain Ladder structure.
Forecasts of the linear predictor are given for all models.
Distributions forecasts are provided for a Poisson response model (using Martinez Miranda, Nielsen and Nielsen, 2015),
for an over-dispersed Poisson response model (using Harnau and Nielsen, 2017)
and
for a log normal response model (using Kuang and Nielsen, 2018)
This is done for the triangle which shares age and cohort indices with the data.}
\usage{apc.forecast.ac(apc.fit,sum.per.by.age=NULL,
			sum.per.by.coh=NULL, quantiles=NULL, suppress.warning=TRUE)}
\arguments{
  \item{apc.fit}{List. Output from \code{\link{apc.fit.model}}.
  					Note: apc.fit.model should be run with AC structure so that 
					\code{apc.fit$model.design=="AC"}.
					Distribution forecasts are only provided for a Poisson response model where
					\code{apc.fit$model.family=="poisson.response"}
					for an over-disperse Poisson response model where
					\code{apc.fit$model.family=="poisson.response"}
					and
					for a log normal response model where
					\code{apc.fit$model.family=="log.normal.response"}.
					For other models only point forecasts of the linear predictor are provided, that is
					the first two values \code{linear.predictors.forecast} and \code{index.trap.J}.}
  \item{sum.per.by.age}{\emph{Optional}. Vector.  If not NULL it will generate forecasts by period,
  					where, for each period, the point forecasts are cummulated over certain age groups.
					Indicates which age groups. If \code{sum.per.by.age} is a scalar or vector of length one it
					represents a single age group. Point forecasts are made for the indicated age group.
					If \code{sum.per.by.age} is a vector of length two it represents lower and upper values
					of an range of age groups. Point forecasts are cummulated over the indicated age groups.}
  \item{sum.per.by.coh}{\emph{Optional}. Vector.  Same as \code{sum.per.by.age}, but for cohort instead of age.}
  \item{quantiles}{\emph{Optional}. Vector.  Generates forecast quantiles for indicated quantiles.  Example:
  					\code{quantiles=c(0.05,0.50,0.95)}. Default is NULL.}
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
  \item{response.forecast.per.ic}{Same as response.forecast.cell,
  					but point forecasts cumulated by per and intercept corrected by
					multiplying column 1 of \code{response.forecast.per} by \code{intercept.correction.per}.}
  \item{response.forecast.coh}{Same as \code{response.forecast.cell},
  					but point forecasts by \code{coh} cumulated over age/period.}
  \item{response.forecast.all}{Same as \code{response.forecast.cell},
  					but point forecasts cumulated by \code{age} and \code{coh}.}
  \item{response.forecast.per.by.age}{Only if \code{sum.per.by.age!=NULL}.
  					Same as \code{response.forecast.per},
  					but point forecasts cumulated over ages indicated by \code{sum.per.by.age}.}	
  \item{response.forecast.per.by.age.ic}{Only if \code{sum.per.by.age!=NULL}.
  					Same as \code{response.forecast.per.by.age},
  					but intercept corrected using \code{intercept.correction.per.by.age}.}
  \item{response.forecast.per.by.coh}{Only if \code{sum.per.by.coh!=NULL}.
  					Same as \code{response.forecast.per},
  					but point forecasts cumulated over cohorts indicated by \code{sum.per.by.coh}.}
  \item{response.forecast.per.by.coh.ic}{Only if \code{sum.per.by.coh!=NULL}.
  					Same as \code{response.forecast.per.by.coh},
  					but intercept corrected using \code{intercept.correction.per.by.coh}.}
  \item{intercept.correction.per}{Numeric.
					The intercept correction is constructed as the ratio of
					the sum of data entries for the last period and the sum of the corresponding fitted values.}
 \item{intercept.correction.per.by.age}{Numeric. Only if \code{sum.per.by.age!=NULL}.}
 \item{intercept.correction.per.by.coh}{Numeric. Only if \code{sum.per.by.coh!=NULL}.}
}
\details{The default output only reports standard errors.
	By setting the argument
	\code{quantiles}
	to, for instance,
	\code{quantiles=c(0.05,0.50,0.95)}
	forecast quantiles are reported.

	\emph{Poisson response forecast errors}.
	The asymptotic theory for the Poisson forecast standard errors is presented in
	Martinez Miranda, Nielsen and Nielsen (2015).
	The sampling theory is based on multinomial model, conditional on the total number of outcomes.
	Asymptotically this gives a normal theory.
	There are two independent contributions to the forecast error:
	a process error and an estimation error.
	The empirical example of that paper uses the data
	\code{\link{data.asbestos}}.
	The results of that paper are reproduced in
	the vignette 
	\href{../doc/ReproducingMMNN2015.pdf}{\code{ReproducingMMNN2015.pdf}},
	\href{../doc/ReproducingMMNN2015.R}{\code{ReproducingMMNN2015.R}}
	on
	\href{../doc/index.html}{\code{Vignettes}}.		
	
	\emph{Overdispersed Poisson response forecast errors}.
	The asymptotic theory for the overdispersed Poission forecast standard errors is presented in
	Harnau and Nielsen (2018).
	The sampling theory is based on infinitely devisible distributions, with the
	compound Poisson distribution as a special case.
	This results in scale nuisance parameter, which is estimated by
	the deviance of the AC model divided by the degrees of freedom.
	Asymptotically this gives a t/F theory.
	There are three independent contributions to the forecast error:
	a process error, an estimation error and a sampling error for the overall mean.

	\emph{Generalized log normal forecast errors}.
	Uses the asymptotic theory present in Kuang and Nielsen (2018).
	The sampling theory is based on infinitely devisible distributions,
	using small sigma asymptotics.
	There are two independent contributions to the forecast error:
	a process error and an estimation error.
	
	The examples below are based on the smaller data reserving sets
	\code{\link{data.loss.VNJ}},
	\code{\link{data.loss.TA}}.
	See also
	\code{\link{data.loss.XL}}.	
}
\references{
Harnau, J. and Nielsen (2018) Over-dispersed age-period-cohort models. \emph{Journal of the American Statistical Association} 113, 1722-1732. \emph{Download}: \href{https://www.nuffield.ox.ac.uk/economics/papers/2017/HarnauNielsen2017apcDP.pdf}{Nuffield DP}

Martinez Miranda, M.D., Nielsen, B. and Nielsen, J.P. (2015) Inference and forecasting in the age-period-cohort model with unknown exposure with an application to mesothelioma mortality. \emph{Journal of the Royal Statistical Society} A 178, 29-55. \emph{Download}: \doi{https://doi.org/10.1111/rssa.12051}, \href{http://www.nuffield.ox.ac.uk/economics/papers/2013/Asbestos8mar13.pdf}{Nuffield DP}. 

Martinez Miranda, M.D., Nielsen, B., Nielsen, J.P. and Verrall, R. (2011) Cash flow simulation for a model of outstanding liabilities based on claim amounts and claim numbers. \emph{ASTIN Bulletin} 41, 107-129.

Kuang, D, Nielsen B (2018) Generalized log-normal chain-ladder. mimeo Nuffield Collge.
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 18 November 2019 (2 Mar 2016)}
\seealso{The example below uses Japanese breast cancer data, see \code{\link{data.Japanese.breast.cancer}}}
\examples{
#####################
#  EXAMPLE with reserving data: data.loss.VNJ()
#  Data used in Martinez Miranda, Nielsen, Nielsen and Verrall (2011)
#  Point forecasts are the Chain-Ladder forecasts	
#  *NOTE* Data are over-dispersed,
#		so distribution forecast are *NOT* reliable
#  The same could be done data.asbestos(),
#		which are not over-dispersed
#		see vignette.

data	<- data.loss.VNJ()
fit.ac	<- apc.fit.model(data,"poisson.response","AC")
forecast	<- apc.forecast.ac(fit.ac)

#	forecasts by "policy-year"
forecast$response.forecast.coh
#	          forecast         se    se.proc     se.est
#	coh_2     1684.763   57.69067   41.04586   40.53949
#	coh_3    29379.085  220.53214  171.40328  138.76362
#	coh_4    60637.929  313.33867  246.24770  193.76066
#	coh_5   101157.697  385.69930  318.05298  218.18857
#	coh_6   173801.522  501.42184  416.89510  278.60786
#	coh_7   249348.589  595.21937  499.34816  323.94060
#	coh_8   475991.739  864.06580  689.92155  520.20955
#	coh_9   763918.643 1182.70450  874.02440  796.78810
#	coh_10 1459859.526 2216.80272 1208.24647 1858.58945

#	forecasts of "cash-flow"
forecast$response.forecast.per
#	reproduces Table 6 of MMNNV (2011)
#	         forecast         se    se.proc   se.est
#	per_11 1353858.32 1456.92459 1163.55417 876.7958
#	per_12  754180.12 1017.37629  868.43544 529.9758
#	per_13  488612.42  816.62860  699.00817 422.2202
#	per_14  318043.00  664.36135  563.95302 351.1880
#	per_15  184610.86  508.97704  429.66366 272.8494
#	per_16  115022.56  414.64945  339.14976 238.5615
#	per_17   63145.15  320.93564  251.28700 199.6360
#	per_18   35812.79  255.08766  189.24267 171.0466
#	per_19    2494.27   78.10439   49.94266  60.0502

#	forecast  of "total reserve"
#	reproduces Table 6 of MMNNV (2011)
forecast$response.forecast.all
#	    forecast       se  se.proc   se.est
#	all  3315779 3182.737 1820.928 2610.371

#####################
#	Forecast of cashflows for 7th cohort (policy year)
#	Note a series of warnings are given because 
#		this is done by truncating the data
#		which generates the warnings associated
#		with apc.data.list.subset()
forecast<- apc.forecast.ac(fit.ac,sum.per.by.coh=7)
forecast$response.forecast.per.by.coh
#	         forecast        se   se.proc    se.est
#	per_11 102975.337 355.97444 320.89771 154.08590
#	per_12  58061.306 267.24671 240.95914 115.58329
#	per_13  40466.866 226.40049 201.16378 103.87646
#	per_14  21615.765 170.90637 147.02301  87.13910
#	per_15  24410.927 194.70158 156.23997 116.17994
#	per_16   1818.389  61.09857  42.64257  43.75668
#
#	This can also be intercept corrected
#		Such intercept corrections are useful when
#		analysing data.asbestos().
#		Unclear if they are useful for
#		reserving.
forecast$intercept.correction.per.by.coh
#   > [1] 1.241798
forecast$response.forecast.per.by.coh.ic
#	         forecast        se   se.proc    se.est
#	per_11 127874.573 355.97444 320.89771 154.08590
#	per_12  72100.417 267.24671 240.95914 115.58329
#	per_13  50251.675 226.40049 201.16378 103.87646
#	per_14  26842.415 170.90637 147.02301  87.13910
#	per_15  30313.441 194.70158 156.23997 116.17994
#	per_16   2258.071  61.09857  42.64257  43.75668

#####################
#	Forecast of cashflows cumulated for
#		6th and 7th cohort (policy year)
forecast<- apc.forecast.ac(fit.ac,sum.per.by.coh=c(6,7))
forecast$response.forecast.per.by.coh.ic
#	         forecast        se   se.proc    se.est
#	per_11 226219.380 460.52781 414.62816 200.42295
#	per_12 139628.153 366.48699 325.74697 167.93339
#	per_13  87022.435 295.86605 257.16360 146.29970
#	per_14  66584.160 277.64858 224.94656 162.75067
#	per_15  34962.678 206.77289 163.00324 127.22018
#	per_16   2392.759  61.09857  42.64257  43.75668

#####################
#  EXAMPLE with reserving data: data.loss.TA()
#  Data used in Harnau and Nielsen (2016)
data	<- data.loss.TA()
fit.ac	<- apc.fit.model(data,"od.poisson.response","AC")
forecast	<- apc.forecast.ac(fit.ac,quantiles=c(0.01,0.05,0.5,0.95,0.99))
forecast$response.forecast.all
#	    forecast      se se.proc  se.est  tau.est 
#	all 18680856 2675417 1007826 2474680 134561.2
#	...
#	 t-0.010  t-0.050  t-0.500  t-0.950  t-0.990 
#	12158931 14160544 18680856 23201167 25202781
#	...
#	 G-0.010  G-0.050  G-0.500  G-0.950  G-0.990 
#	12760202 14398564 18553290 23417098 25792423 
forecast$response.forecast.per

#####################
#  EXAMPLE with reserving data: data.loss.XL()
#  see helpfile for data.loss.XL 	


}