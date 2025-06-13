\name{apc-package}
\alias{apc-package}
\alias{apc}
\docType{package}
\title{Age-period-cohort analysis}
\description{The package includes functions for age-period-cohort analysis.  The statistical model is a generalized linear model (GLM)
allowing for age, period and cohort factors, or a sub-set of the factors.
The canonical parametrisation of Kuang, Nielsen and Nielsen (2008a) is used. 
The outline of an analysis is described below.
}
\details{
	\tabular{ll}{
		Package: \tab apc\cr
		Type: \tab Package\cr
		Version: \tab 2.0.0\cr
		Date: \tab 2020-09-28\cr
		License: \tab GPL-3\cr
	}
	The apc package uses the canonical parameters suggested by
	Kuang, Nielsen and Nielsen (2008a)
	and generalized by
	Nielsen (2014). 
	These evolve around the second differences of age, period and cohort factors as well as an three parameters (level and two slopes)
	for a linear plane.  The age, period and cohort factors themselves are not identifiable.  They could be ad hoc identified
	by associating the levels and two slopes to the age, period and cohort factors in a particular way.  This should be done
	with great care as such ad hoc identification easily masks which information is coming from the data and which information
	is coming from the choice of ad hoc identification scheme. An illustration is given below.
	A short description of the package can be found in
	Nielsen (2015).

	A formal analysis of the identification of the age-period-cohort model can be found in
	Nielsen and Nielsen (2014).
	Forecasting is discussed in
	Kuang, Nielsen and Nielsen (2008b, 2011)
	and
	Martinez Miranda, Nielsen and Nielsen (2015).
	Methods for cross section data are introduced in
	Fannon, Monden and Nielsen (2019).
	Methods for panel data are introduced in
	Fannon (2020).	
	For a recent overview see
	Fannon and Nielsen (2019).
	
	
	The package covers age-period-cohort models for three types of data.
	\enumerate{
		\item
			Tables of aggregate data.
		\item
			Repeated cross sectional data.
		\item
			Panel data. 
	}		
	
	The apc package can be used as follows.
	\enumerate{
		\item
			\strong{Aggregate data}.
			For a vignette with an introduction to analysis of aggregate data, see
			see
			\href{../doc/IntroductionAggregateData.pdf}{\code{IntroductionAggregateData.pdf}},
			\href{../doc/IntroductionAggregateData.R}{\code{IntroductionAggregateData.R}}			
			on
			\href{../doc/index.html}{\code{Vignettes}}.		
			\enumerate{
				\item
					Organize the data in as an \code{\link{apc.data.list}}.
					Data are included in matrix format.  Information needs to be given about the original data format.
					Optionally, information can be given about the labels for the time scales.
				\item
					Construct descriptive plots using \code{\link{apc.plot.data.all}}.
					This gives a series of descriptive plots.  The plots can be called individually through
					\enumerate{		
						\item
							Plot data sums using \code{\link{apc.plot.data.sums}}.
							Numerical values can be obtained through \code{\link{apc.data.sums}}.
						\item
						 	Sparsity plots of data using \code{\link{apc.plot.data.sparsity}}.
						\item	
						 	Plot data using all combinations of two time scales using \code{\link{apc.plot.data.within}}.
					}
				\item
					Get an deviance table for the age-period-cohort model through
					\code{\link{apc.fit.table}}.
				\item
					Estimate a particular (sub-model of) age-period-cohort model through
					\code{\link{apc.fit.model}}.
				\item
					Plot probability transforms of observed responses given fit using
					\code{\link{apc.plot.fit.pt}}.
				\item
					Plot estimated parameters through
					\code{\link{apc.plot.fit}}.
					Numerical values of certain transformations of the canonical parameter can be obtained through
					\code{\link{apc.identify}}.
				\item
					Recursive analysis can be done by selecting a subset of the observations through
					\code{\link{apc.data.list.subset}} and then repeating analysis.  This will reveal how sensitive
					the results are to particular age, period and cohort groups.
				\item
					Forecasting.  Some functions have been been added for forecasting in from a Poisson response-only model
					with an age-cohort parametrization
					\code{\link{apc.forecast.ac}}
					and with an age-period parametrization
					\code{\link{apc.forecast.ap}}.
					See also the overview on
					\code{\link{apc.forecast}}
			}
		\item
			\strong{Repeated cross section}
			and 
			\strong{Panel Data}.			
			For a vignette 
			with an introduction to analysis of repeated cross section data and panel data,
			see
			\href{../doc/IntroductionIndividualData.pdf}{\code{IntroductionIndividualData.pdf}},
			\href{../doc/IntroductionIndividualData.R}{\code{IntroductionIndividualData.R}}			
			on
			\href{../doc/index.html}{\code{Vignettes}}		
			Further examples can be found in a second vignette, see 			
			\href{../doc/IntroductionIndividualDataFurtherExamples.pdf}{\code{IntroductionIndividualDataFurtherExample.pdf}},
			\href{../doc/IntroductionIndividualDataFurtherExample.R}{\code{IntroductionIndividualDataFurtherExample.R}}.
	}
	Data examples include
	\enumerate{
		\item
			\strong{Aggregate data}
			\enumerate{
				\item
					\code{\link{data.asbestos}}
					includes counts of deaths from mesothelioma in the UK.
					This dataset has no measure for exposure.
					It can be analysed using a Poisson model with an "APC" or an "AC" design.
					Source: Martinez Miranda, Nielsen and Nielsen (2015).
					Also used in Nielsen (2015).
				\item	
					\code{\link{data.Italian.bladder.cancer}}
					includes counts of deaths from bladder cancer in the Italy.
					This dataset includes a measure for exposure.
					It can be analysed using a Poisson model with an "APC" or an "AC" design.
					Source: Clayton and Schifflers (1987a).
				\item	
					\code{\link{data.Belgian.lung.cancer}}
					includes counts of deaths from lung cancer in the Belgium.
					This dataset includes a measure for exposure.
					It can be analysed using a Poisson model with an "APC", "AC", "AP" or "Ad" design.
					Source: Clayton and Schifflers (1987a).
				\item	
					\code{\link{data.Japanese.breast.cancer}}
					includes counts of deaths from breast cancer in the Japan.
					This dataset includes a measure for exposure.
					It can be analysed using a Poisson model with an "APC" design.
					Source: Clayton and Schifflers (1987b).
			}
			\strong{Repeated cross section data}		
			\enumerate{
				\item
					\code{\link[ISLR]{Wage}} data from the package ISLR
			}
			\strong{Repeated cross section data}		
			\enumerate{
				\item
					\code{\link[AER]{PSID7682}} data from the package AER.
					These are panel data on earnings for 595 individuals for the years 1976-1982.
			}
	}
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 29 Jan 2015 updated 26 Aug 2020.}
\references{
Clayton, D. and Schifflers, E. (1987a) Models for temperoral variation in cancer rates. I: age-period and age-cohort models. \emph{Statistics in Medicine} 6, 449-467.

Clayton, D. and Schifflers, E. (1987b) Models for temperoral variation in cancer rates. II: age-period-cohort models. \emph{Statistics in Medicine} 6, 469-481.

Fannon, Z. (2020). D.Phil. thesis. University of Oxford.

Fannon, Z., Monden, C. and Nielsen, B. (2018) Age-period cohort modelling and covariates, with an application to obesity in England 2001-2014. \emph{Download}: \href{https://www.nuffield.ox.ac.uk/economics/Papers/2018/2018W05_obesity.pdf}{Nuffield DP}. Supplement Code for replication: \href{https://www.nuffield.ox.ac.uk/economics/Papers/2018/2018W05_obesityReplication.zip}{Nuffield DP supplement}. 

Fannon, Z. and Nielsen, B. (2019) Age-period-cohort models. \emph{Oxford Research Encyclopedia of Economics and Finance}. Oxford University Press. \emph{Download}: \code{doi.org/10.1093/acrefore/9780190625979.013.495}; Earlier version \href{https://www.nuffield.ox.ac.uk/economics/Papers/2018/2018W04_age_period_cohort_models.pdf}{Nuffield DP}. 

Kuang, D., Nielsen, B. and Nielsen, J.P. (2008a) Identification of the age-period-cohort model and the extended chain ladder model. Biometrika 95, 979-986. \emph{Download}: \href{https://doi.org/10.1093/biomet/asn026}{Article}; Earlier version \href{http://www.nuffield.ox.ac.uk/economics/papers/2007/w5/KuangNielsenNielsen07.pdf}{Nuffield DP}.

Kuang, D., Nielsen, B. and Nielsen, J.P. (2008b) Forecasting with the age-period-cohort model and the extended chain-ladder model. Biometrika 95, 987-991. \emph{Download}: \href{https://doi.org/10.1093/biomet/asn038}{Article}; Earlier version \href{http://www.nuffield.ox.ac.uk/economics/papers/2008/w9/KuangNielsenNielsen_Forecast.pdf}{Nuffield DP}.

Kuang, D., Nielsen, B. and Nielsen, J.P. (2011) Forecasting in an extended chain-ladder-type model. Journal of Risk and Insurance 78, 345-359. \emph{Download}: \href{http://dx.doi.org/10.1111/j.1539-6975.2010.01395.x}{Article}; Earlier version: \href{http://www.nuffield.ox.ac.uk/economics/papers/2010/w5/Forecast24jun10.pdf}{Nuffield DP}. 

Martinez Miranda, M.D., Nielsen, B. and Nielsen, J.P. (2015) Inference and forecasting in the age-period-cohort model with unknown exposure with an application to mesothelioma mortality. \emph{Journal of the Royal Statistical Society} A 178, 29-55. \emph{Download}: \href{https://doi.org/10.1111/rssa.12051}{Article}, \href{http://www.nuffield.ox.ac.uk/economics/papers/2013/Asbestos8mar13.pdf}{Nuffield DP}. 

Nielsen, B. (2015) apc: An R package for age-period-cohort analysis. \emph{R Journal} 7, 52-64. \emph{Download}: \href{https://journal.r-project.org/archive/2015-2/nielsen.pdf}{Open access}.

Nielsen, B. (2014) Deviance analysis of age-period-cohort models. \emph{Download}: \href{http://www.nuffield.ox.ac.uk/economics/papers/2014/apc_deviance.pdf}{Nuffield DP}.

Nielsen, B. and Nielsen, J.P. (2014) Identification and forecasting in mortality models. The Scientific World Journal. vol. 2014, Article ID 347043, 24 pages. \emph{Download}: \href{http://www.hindawi.com/journals/tswj/2014/347043}{Article}.
}
\seealso{
Vignettes are available on
\href{../doc/index.htm}{\code{Vignettes}}.

Further information, including minor upgrades and a python version can be found on
\href{http://users.ox.ac.uk/~nuff0078/apc/index.htm}{\code{apc development web page}}.
}
\examples{
#	see vignettes
}
\keyword{ package }
\keyword{ models }
\keyword{ regression }
\keyword{ htest }
\keyword{ hplot }
