\name{apc.fit.model}
\alias{apc.fit.model}
\alias{apc.fit.table}
\title{Fits an age period cohort model}
\description{
\code{apc.fit.model} fits the age period cohort model as a Generalized Linear Model using \code{\link{glm.fit}}.
The model is parametrised in terms of the canonical parameter introduced by Kuang, Nielsen and Nielsen (2008),
see also the implementation in Martinez Miranda, Nielsen and Nielsen (2015).
This parametrisation has a number of advantages: it is freely varying, it is the canonical parameter of a
regular exponential family, and it is invariant to extentions of the data matrix.
Other parametrizations can be computed using \code{apc.identify}.

\code{apc.fit.model} can be be used for all three age period cohort factors, or for submodels with fewer of these factors. 

\code{apc.fit.model} can be used either for mortality rates through a dose-response model or for mortality counts through a pure response model without doses/exposures.  

The GLM families include Poisson regressions (with log link) and Normal/Gaussian least squares regressions.  

apc.fit.table produces a deviance table for 15 combinations of the three factors and linear trends:
"APC", "AP", "AC", "PC", "Ad", "Pd", "Cd", "A", "P", "C", "t", "tA", "tP", "tC", "1".
}
\usage{	apc.fit.model(apc.data.list,model.family,model.design,apc.index=NULL,
			replicate.version.1.3.1=FALSE)
		apc.fit.table(apc.data.list,model.family,model.design.reference="APC",
			apc.index=NULL)}
\arguments{
  	\item{apc.data.list}{List. See \code{\link{apc.data.list}} for a description of the format.}
  	\item{model.family}{Character.  The following options are implemented.	These are used internally when
		calling \code{\link{glm.fit}}.		
	  	\describe{
			\item{"poisson.response"}{This sets family=poisson(link="log"). Only responses are used.
				Inference is done in a multinomial model, conditioning on the overall level
				as documented in Martinez Miranda, Nielsen and Nielsen (2015).}
			\item{"od.poisson.response"}{This sets family=quasipoisson(link="log") in the estimation step,
				but then reverts to family=poisson(link="log") when computing standard errors, which are then corrected.
				Only responses are used.
				Inference is done in an over-dispersed Poisson model
				as documented in Harnau and Nielsen (2016).
				Note that limit distributions are t and F not normal and chi2.}
			\item{"poisson.dose.response"}{This sets family=poisson(link="log"). Doses are used as offset.}
			\item{"binomial.dose.response"}{This sets family=binomial(link="logit") and gives a logistic regression.}
			\item{"gaussian.rates"}{This sets family=gaussian(link="identity").
				The dependent variable is the mortality rates, which are computed
				as response/dose.}
			\item{"gaussian.response"}{This sets family=gaussian(link="identity").  Only responses are used.
				The dependent variable is the responses.}
			\item{"log.normal.rates"}{Gaussian regression for log(rates) and with identity link (Least Squares).}	
			\item{"log.normal.response"}{Gaussian regression for log(response) and with identity link (Least Squares).}	
		}}
  \item{model.design}{Character.  This indicates the design choice.  The following options are possible.
	  	\describe{
			\item{"APC"}{Age-period-cohort model.}
			\item{"AP" }{Age-period model. Nested in "APC"}
			\item{"AC" }{Age-cohort model. Nested in "APC"}
			\item{"PC" }{Period-cohort model. Nested in "APC"}
			\item{"Ad" }{Age-trend model, including age effect and two linear trends. Nested in "AP", "AC".}
			\item{"Pd" }{Period-trend model, including period effect and two linear trends. Nested in "AP", "PC".}
			\item{"Cd" }{Cohort-trend model, including cohort effect and two linear trends. Nested in "AC", "PC".}
			\item{"A"  }{Age model. Nested in "Ad".}      
			\item{"P"  }{Period model. Nested in "Pd".}
			\item{"C"  }{Cohort model. Nested in "Cd".}
			\item{"t"  }{Trend model, with two linear trends. Nested in "Ad", "Pd", "Cd".}
			\item{"tA" }{Single trend model in age index. Nested in "A", "t".}
			\item{"tP" }{Single trend model in period index. Nested in "P", "t".}
			\item{"tC" }{Single trend model in cohort index. Nested in "C", "t".}
			\item{"1"  }{Constant model. Nested in "tA", "tP", "tC".}
  		}}
	\item{model.design.reference}{Character.  This indicates the reference design choice for the deviance table. Choices are
			"APC","AP","AC","PC","Ad","Pd","Cd","A","P","C","t". Default is "APC".}	
	\item{apc.index}{\emph{Optional}. List. See \code{\link{apc.get.index}} for a description of the format.
			If not provided this is computed internally.
			If \code{apc.fit.model} is used in a simulation study computational effort can be saved when
			using this option.}
	\item{replicate.version.1.3.1}{\emph{Optional}. Logical. Replicate error in covariance calculation for 
			"poisson.response","od.poisson.response" in versions 1.2.3-1.3.1. Default=FALSE}		
}
\value{
	\emph{apc.fit.table}
	produces a deviance table.  There are 15 rows corresponding to all possible design choices.
	The columns are as follows.
	\item{"-2logL"}{-2 log Likelihood up to some constant.
		If the model family is Poisson or binomial (logistic)
		this is the same as the \code{\link{glm}} deviance: That is the difference in -2 log likelihood value between estimated
		model and the saturated model.
		If the model family is Gaussian it is different from the traditional
		\code{\link{glm}} deviance.  Here the -2 log likelihood value is measured in a model with unknown variance,
		which is the standard in regression analysis, whereas in the \code{\link{glm}} package the deviance
		is the residual sum of squares, which can be interpreted as the 
		-2 log likelihood value in a model with variance set to one.}
	\item{"df.residual"}{Degrees of freedom of residual: nrow x ncol - dim(parameter).
		If the model.family="poisson.response" the degrees of freedom is one lower.}
	\item{"prob(>chi_sq)"}{p-value of the deviance, -2logL. Left out in Gaussian case which has no saturated model}
	\item{"LR vs APC"}{the likelihood ratio statistic against the "APC" model.}
	\item{"df"}{Degrees of freedom against the "APC" model.}
	\item{"prob(>chi_sq)"}{p-value of log likelihood ratio statistic.}
  	\item{"aic"}{Akaike's "An Information Criterion", minus twice the maximized log-likelihood plus twice the
		number of parameters upto a constant.  It is take directly from the
		\code{\link{glm}} function.  
 		For the "poisson.dose.response" and "binomial.dose.response" model families
		the dispersion is fixed at one and the number of parameters is the number of coefficients.
 		The "poisson.response" model is conditional on the level.  The number of parameters should therefore be
		adjusted by subtracting 2 to take this into account to get the proper AIC. However, in practice this does not matter,
		since we are only interested in relative effects. 
		For the "gaussian.response" and "gaussian.dose.response" model families the dispersion is estimated from
		the residual deviance. }
	\item{"F"}{Only for "od.poisson.response". F statistic: Ratio of deviance for submodel divided by degrees of freedom to deviance of apc model divided by degrees of freedom.}
	\item{"prof(>F)"}{Only for "od.poisson.response". F statistic: with degrees of freedom given by differences between sub-model and apc model and between apc model and saturated model.}
	\emph{apc.fit.model}
	returns a list. The entries are as follows.
	\item{fit}{List. Values from \code{\link{glm.fit}}.}
	\item{apc.index}{List. Values from \code{\link{apc.get.index}}.}	  
	\item{coefficients.canonical}{Matrix.  For each coordinate of the canonical parameters is reported coefficient, standard deviation, z-value, which is the ratio of those, and asymptotically normal p-values.
									Note, for "od.poisson.response" the reported standard errors corrected by the deviance and p-values are asymptotically t distributed, see Harnau and Nielsen (2016).
									Other parametrizations can be computed using \code{apc.identify}.}
	\item{covariance.canonical	}{Matrix.  Estimated covariance matrix for canonical parameters.}
	\item{slopes				}{Vector.  Length three.  The design matrix found by \code{\link{apc.get.design.collinear}} has age, period, and cohort linear trends. \code{slopes} indicates which of these are actually used in estimation.}
	\item{difdif				}{Vector.  Length three.  The design matrix found by \code{\link{apc.get.design.collinear}} has age, period, and cohort double differences. \code{slopes} indicates which of these are actually used in estimation.}
	\item{index.age				}{Vector.  Indices for age    double difference parameters within \code{coefficients.canonical}.  NULL if age    double differences are not estimated.}
	\item{index.per				}{Vector.  Indices for period double difference parameters within \code{coefficients.canonical}.  NULL if period double differences are not estimated.}
	\item{index.coh				}{Vector.  Indices for cohort double difference parameters within \code{coefficients.canonical}.  NULL if cohort double differences are not estimated.}
	\item{dates					}{Vector.  Indicates the dates for the double difference parameters within \code{coefficients.canonical}.}
	\item{model.family			}{Character. Argument.}
	\item{model.design			}{Character. Argument.}
	\item{RSS					}{Numeric.  Residual sum of squares.  NULL for non-gaussian families.}
	\item{sigma2				}{Numeric.  Maximum likelihood estimator for variance: RSS/n.  NULL for non-gaussian families.}
	\item{s2					}{Numeric.  Least squares estimator for variance: RSS/df.  NULL for non-gaussian families.}
	\item{n.decimal				}{Numeric.  From \code{\link{apc.data.list}}.}
	\item{predictors			}{Vector. Design*Estimates.                                          
								  Same as the \code{\link{glm.fit}} value \code{linear.predictors} when there is no offset.}
}
\note{
For gaussian families \emph{deviance} is defined differently in \code{apc} and \code{\link{glm}}.
Here it is -2 log likelihood.  In \code{\link{glm}} it is RSS.

The values for \code{apc.fit.model} include the \code{\link{apc.data.list}} and the \code{apc.index} returned by
\code{\link{apc.get.index}}.

For the \code{poisson.response} the inference is conditional on the level, see Martinez Miranda, Nielsen and Nielsen (2015).
The \code{coefficients.canonical} computed by \code{apc} are therefore different from the default \code{coefficients} computed by \code{glm}.

For the \code{od.poisson.response} an asymptotic theory is used that mimics the conditioning for \code{poisson.response}.
The asymptotic distribution are, however, asymptotically t or F distributed, see Harnau and Nielsen (2017).

For the \code{log.normal.response} standard normal theory applies for quantities on the log scale including estimators.
An asymptotic theory for quantities on the original scale is provided in Kuang and Nielsen (2018). 

For \code{coefficients} the 3rd and 4th columns have headings \code{t value} and \code{Pr(>|t|)} for \code{od.poisson.response}
to indicate an asymptotic t theory
and otherwise
\code{z value} and \code{Pr(>|z|)} to indicate an asymptotic normal theory. The labels are inherited from \code{\link{glm.fit}}.
}
\references{
Harnau, J. and Nielsen (2016) Over-dispersed age-period-cohort models. To appear in \emph{Journal of the American Statistical Association}. \emph{Download}: \href{https://www.nuffield.ox.ac.uk/economics/papers/2017/HarnauNielsen2017apcDP.pdf}{Nuffield DP}

Kuang, D, Nielsen B (2018) Generalized log-normal chain-ladder. mimeo Nuffield Collge.

Kuang, D., Nielsen, B. and Nielsen, J.P. (2008a) Identification of the age-period-cohort model and the extended chain ladder model. Biometrika 95, 979-986. \emph{Download}: \href{https://doi.org/10.1093/biomet/asn026}{Article}; Earlier version \href{http://www.nuffield.ox.ac.uk/economics/papers/2007/w5/KuangNielsenNielsen07.pdf}{Nuffield DP}.

Martinez Miranda, M.D., Nielsen, B. and Nielsen, J.P. (2015) Inference and forecasting in the age-period-cohort model with unknown exposure with an application to mesothelioma mortality. \emph{Journal of the Royal Statistical Society} A 178, 29-55. \emph{Download}: \href{https://doi.org/10.1111/rssa.12051}{Article}, \href{http://www.nuffield.ox.ac.uk/economics/papers/2013/Asbestos8mar13.pdf}{Nuffield DP}. 
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 15 Aug 2018 (27 Aug 2014)}
\seealso{
The fit is done using \code{\link{glm.fit}}.

The examples below use Italian bladder cancer data, see \code{\link{data.Italian.bladder.cancer}}
and
Belgian lung cancer data, see \code{\link{data.Belgian.lung.cancer}}.

In example 3 the design matrix is called is called using \code{\link{apc.get.design}}.
}
\examples{
#####################
#	EXAMPLE 1 with Italian bladder cancer data

data.list	<- data.Italian.bladder.cancer()	#	function gives data list
apc.fit.table(data.list,"poisson.dose.response")

#	       -2logL df.residual prob(>chi_sq) LR.vs.APC df.vs.APC prob(>chi_sq)       aic
#	APC    33.179          27         0.191        NA        NA            NA   487.624
#	AP    512.514          40         0.000   479.335        13         0.000   940.958
#	AC     39.390          30         0.117     6.211         3         0.102   487.835
#	PC   1146.649          36         0.000  1113.470         9         0.000  1583.094
#	Ad    518.543          43         0.000   485.364        16         0.000   940.988
#	Pd   4041.373          49         0.000  4008.194        22         0.000  4451.818
#	Cd   1155.629          39         0.000  1122.450        12         0.000  1586.074
#	A    2223.800          44         0.000  2190.621        17         0.000  2644.245
#	P   84323.944          50         0.000 84290.765        23         0.000 84732.389
#	C   23794.205          40         0.000 23761.026        13         0.000 24222.650
#	t    4052.906          52         0.000  4019.727        25         0.000  4457.351
#	tA   5825.158          53         0.000  5791.979        26         0.000  6227.602
#	tP  84325.758          53         0.000 84292.579        26         0.000 84728.203
#	tC  33446.796          53         0.000 33413.617        26         0.000 33849.241
#	1   87313.678          54         0.000 87280.499        27         0.000 87714.123
#
#	Table suggests that "APC" and "AC" fit equally well.  Try both

fit.apc	<- apc.fit.model(data.list,"poisson.dose.response","APC")
fit.ac	<- apc.fit.model(data.list,"poisson.dose.response","AC")

#	Compare the estimates: They are very similar

fit.apc$coefficients.canonical
fit.ac$coefficients.canonical

#####################
#	EXAMPLE 2 with Belgian lung cancer data
#	This example illustrates how to find the linear predictors 

data.list	<- data.Belgian.lung.cancer()

#	Get an APC fit

fit.apc	<- apc.fit.model(data.list,"poisson.dose.response","APC")

#	The linear predictor of the fit is a vector.
#	But, we would like it in the same format as the data.
#	Thus create matrix of same dimension as response data
#	This can be done in two ways

m.lp	<- data.list$response	#	using original information	
m.lp	<- fit.apc$response		# 	using information copied when fitting

#	the fit object index.data is used to fill linear predictor in
#	vector format into matrix format

m.lp[fit.apc$index.data]	<-fit.apc$linear.predictors
exp(m.lp)

#####################
#	EXAMPLE 3 with Belgian lung cancer data
#	This example illustrates how apc.fit.model works.

data.list	<- data.Belgian.lung.cancer()

#	Vectorise data
index		<- apc.get.index(data.list)
v.response	<- data.list$response[index$index.data]
v.dose		<- data.list$dose[index$index.data]

#	Get design
m.design	<- apc.get.design(index,"APC")$design

#	Fit using glm.fit from stats package
fit.apc.glm	<- glm.fit(m.design,v.response,family=poisson(link="log"),offset=log(v.dose))

#	Get canonical coefficients
v.cc		<- fit.apc.glm$coefficients

#	Find linear predictors and express in matrix form
m.fit		<- data.list$response			#	create matrix
m.fit[index$index.data]		<- m.design %*% v.cc 
m.fit.offset		<- m.fit + log(data.list$dose)	#	add offset
exp(m.fit.offset)

#	Compare with linear.predictors from glm.fit
#	difference should be zero
sum(abs(m.fit.offset[index$index.data]-fit.apc.glm$linear.predictors))

#####################
#	EXAMPLE 4 with Taylor-Ashe loss data
#	This example illustrates the over-dispersed poisson response model.

data <- data.loss.TA()
fit.apc.od <- apc.fit.model(data,"od.poisson.response","APC")
fit.apc.od$coefficients.canonical[1:5,]
fit.apc.no.od <- apc.fit.model(data,"poisson.response","APC")
fit.apc.no.od$coefficients.canonical[1:5,]
}
\keyword{ package }
\keyword{ models }
\keyword{ regression }
\keyword{ htest }
