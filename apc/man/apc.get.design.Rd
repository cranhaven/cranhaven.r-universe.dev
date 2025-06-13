\name{apc.get.design}
\alias{apc.get.design}
\alias{apc.get.design.collinear}
\title{Create design matrices}
\description{Functions to create the apc design matrix for the canonical parameters.
Based on Nielsen (2014b), which generalises introduced by Kuang, Nielsen and Nielsen (2008).
In normal use these function are needed for internal use by \code{\link{apc.fit.model}}.

The resulting function design matrix is collinear, so a sub-set of the columns have to be selected.
The columns are: intercept, age/period/cohort slopes, age/period/cohort double differences.
Thus, there are three slopes instead of two. Before use, one has to select which parameters are needed.
This should include at either one/two of age/cohort slopes or period slope or no slope.}
\usage{apc.get.design(apc.index,model.design)
apc.get.design.collinear(apc.index)}
\arguments{
	\item{apc.index}{List. See \code{\link{apc.get.index}} for a description of the format.
						Note, \code{apc.index} can be replace by an apc.fit list. This is extended version
						of \code{apc.index} is the output from \code{\link{apc.fit.model}}. }
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
			\item{NULL}{The function then looks for information on model design in the first argument.}
  		}
			The \code{model.design} argument is not needed if the first argument is of type
			\code{apc.fit}.  If given, the \code{model.design} argument is used.
		}
}
\value{
	\emph{apc.get.design}
	returns a list with
	\item{design}{Matrix. The design matrix. The number of rows is the number of observations, that is \code{apc.index$n.data}.
				  The order of the observations corresponds to the internal choice made in \code{\link{apc.get.index}}.}
	\item{slopes}{Vector. For internal use. Length 3 of logicals,	indicate presence	of age/period/cohort linear slopes
					at most two slopes can be present if neither age/cohort present then period may be presents,
					which is the case for model.design "P","tP"}
	\item{difdif}{Vector. For internal use. Length 3 of logicals}

	\emph{apc.get.design.collinear}
	returns a collinear design matrix for the unrestricted "APC" model.
	It has an extra column.  The columns 2-4 are linear trends in age, period and cohort directions.  At most
	two of these should be used.  They are selected by \code{slopes}.
}
\references{
Kuang, D., Nielsen, B. and Nielsen, J.P. (2008a) Identification of the age-period-cohort model and the extended chain ladder model. Biometrika 95, 979-986. \emph{Download}: \href{https://doi.org/10.1093/biomet/asn026}{Article}; Earlier version \href{http://www.nuffield.ox.ac.uk/economics/papers/2007/w5/KuangNielsenNielsen07.pdf}{Nuffield DP}.

Nielsen, B. (2014b) Deviance analysis of age-period-cohort models.
}
\seealso{
The vignette
	\href{../doc/NewDesign.pdf}{\code{NewDesign.pdf}},
	\href{../doc/NewDesign.R}{\code{NewDesign.R}}
	on
	\href{../doc/index.html}{\code{Vignettes}}.		
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> 1 Mar 2015}
\examples{
#####################
#	EXAMPLE 1 with Belgian lung cancer data
#	This example illustrates how apc.fit.model works.

data.list	<- data.Belgian.lung.cancer()

#	Vectorise data
index		<- apc.get.index(data.list)
v.response	<- data.list$response[index$index.data]
v.dose		<- data.list$dose[index$index.data]

#	Get design
m.design.apc	<- apc.get.design(index,"APC")$design

#	Fit using glm.fit from stats package
fit.apc.glm	<- glm.fit(m.design.apc,v.response,family=poisson(link="log"),offset=log(v.dose))
fit.apc.glm$deviance

#	Compare with standard output from apc.fit.model
apc.fit.model(data.list,"poisson.dose.response","APC")$deviance

#####################
#	EXAMPLE 2 with Belgian lung cancer data
#	The age-drift model gives a good fit.
#	This fit can be refined to a cubic or quadratic age effect.
#	The latter is not precoded so one will have to work directly with the design matrix.
#	SEE ALSO VIGNETTE

data.list	<- data.Belgian.lung.cancer()

#	Vectorise data
index		<- apc.get.index(data.list)
v.response	<- data.list$response[index$index.data]
v.dose		<- data.list$dose[index$index.data]

#	Get design matrix for "Ad"
m.design.ad	<- apc.get.design(index,"Ad")$design

#	Modify design matrix for cubic or quadratic age effect
#	Note this implies a linear or constant double difference
#	Quadractic age effect: restrict double differences to be equal
p	<- ncol(m.design.ad)
m.rest.q	<- matrix(data=0,nrow=p,ncol=4)
m.rest.q[1,1]	<- 1
m.rest.q[2,2]	<- 1
m.rest.q[3,3]	<- 1
m.rest.q[4:p,4]	<- 1
m.design.adq	<- m.design.ad \%*\% m.rest.q
#	Cubic age effect: restrict double differences to be linear
m.rest.c	<- matrix(data=0,nrow=p,ncol=5)
m.rest.c[1,1]	<- 1
m.rest.c[2,2]	<- 1
m.rest.c[3,3]	<- 1
m.rest.c[4:p,4]	<- 1
m.rest.c[4:p,5]	<- seq(1,p-3)
m.design.adc	<- m.design.ad \%*\% m.rest.c

#	Poisson regression for dose-response and with log link
fit.ad	<- glm.fit(m.design.ad,v.response,family=poisson(link="log"),offset=log(v.dose))
fit.adc	<- glm.fit(m.design.adc,v.response,family=poisson(link="log"),offset=log(v.dose))
fit.adq	<- glm.fit(m.design.adq,v.response,family=poisson(link="log"),offset=log(v.dose))

#	Deviance tests
fit.adc$deviance - fit.ad$deviance 
fit.adq$deviance - fit.ad$deviance 
#	Degrees of freedom
ncol(m.design.ad) - ncol(m.design.adc)
ncol(m.design.ad) - ncol(m.design.adq)

}
