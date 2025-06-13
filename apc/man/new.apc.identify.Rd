\name{new.apc.identify}
\alias{new.apc.identify}
\title{Identification of time effects}
\description{Computes ad hoc identified time effects.}
\usage{new.apc.identify(apc.fit.model)}
\arguments{
  \item{apc.fit.model}{List. See \code{\link{apc.fit.model}} for a description of the format.}
}
\details{Forms ad hoc identified time effects from the canonical parameter.
These are used either indirectly by \code{\link{apc.plot.fit}}
or they are computed directly with this command.

The ad hoc identifications are based on	Nielsen (2014b). For details see also the vignette
	\href{../doc/Identification.pdf}{\code{Identification.pdf}},
	\href{../doc/Identification.R}{\code{Identification.R}}
	on
	\href{../doc/index.html}{\code{Vignettes}}		
or in the notes below.

For model designs of any type two ad hoc identified time effects.

(1)
The type "sum.sum" (same as "ss.dd") gives double sums anchored in the middle of
the first period diagonal. 

(2)
The type "detrend" gives double sums that start in zero and end in zero.

For model designs with only two time effects, that is "AC", "AP", "PC" there is
a further ad hoc identification.

(3)
The type "demean" gives single sums of single differences. Derived from "detrend" where the
linear trends are attributed to the double sums of double differences. Level unchanged.

(4)
The type "dif" gives the single differences derived from "demean".  Could also have been chosen
as canonical parametrisation for these models.
}
\value{
	\item{index.age.max		}{Vector.  Indices for age    parameters when using coefficients.ssdd or coefficients.detrend.  The length is two longer that that of \code{apc.model.fit$index.age} if \code{model.design} is "APC.  NULL if age    double differences are not estimated.}
	\item{index.per.max		}{Vector.  Indices for period parameters when using coefficients.ssdd or coefficients.detrend.  The length is two longer that that of \code{apc.model.fit$index.per} if \code{model.design} is "APC.  NULL if age    double differences are not estimated.}
	\item{index.coh.max		}{Vector.  Indices for cohort parameters when using coefficients.ssdd or coefficients.detrend.  The length is two longer that that of \code{apc.model.fit$index.coh} if \code{model.design} is "APC.  NULL if age    double differences are not estimated.}
	\item{dates.max			}{Vector.  Indicates the dates for the parameters when using coefficients.ssdd or coefficients.detrend.  The length is six longer that that of \code{apc.model.fit$index.coh} if \code{model.design} is "APC.}
	\item{index.age.sub		}{* Vector.  Indices for age    parameters when using coefficients.demean.  The length is two longer that that of \code{apc.model.fit$index.age} if \code{model.design} is "APC.  NULL if age    double differences are not estimated.}
	\item{index.per.sub		}{* Vector.  Indices for period parameters when using coefficients.demean.  The length is two longer that that of \code{apc.model.fit$index.per} if \code{model.design} is "APC.  NULL if age    double differences are not estimated.}
	\item{index.coh.sub		}{* Vector.  Indices for cohort parameters when using coefficients.demean.  The length is two longer that that of \code{apc.model.fit$index.coh} if \code{model.design} is "APC.  NULL if age    double differences are not estimated.}
	\item{dates.sub			}{* Vector.  Indicates the dates for the parameters when using coefficients.demean.  The length is six longer that that of \code{apc.model.fit$index.coh} if \code{model.design} is "APC.}
	\item{index.age.dif		}{* Vector.  Indices for age    parameters when using coefficients.dif.  The length is one longer that that of \code{apc.model.fit$index.age} if \code{model.design} is "APC.  NULL if age    double differences are not estimated.}
	\item{index.per.dif		}{* Vector.  Indices for period parameters when using coefficients.dif.  The length is one  longer that that of \code{apc.model.fit$index.per} if \code{model.design} is "APC.  NULL if age    double differences are not estimated.}
	\item{index.coh.dif		}{* Vector.  Indices for cohort parameters when using coefficients.dif.  The length is one  longer that that of \code{apc.model.fit$index.coh} if \code{model.design} is "APC.  NULL if age    double differences are not estimated.}
	\item{dates.dif			}{* Vector.  Indicates the dates for the parameters when using coefficients.dif.  The length is three longer that that of \code{apc.model.fit$index.coh} if \code{model.design} is "APC.}
	\item{coefficients.ssdd		}{Matrix.  Coefficients of the double sum of double differences.  Normalised to be zero at two values chosen so age=cohort and period is at the minimal value.  For each parameter is reported coefficient, standard deviation, z-value, which is the ratio of those, and p-value.}
	\item{covariance.ssdd		}{Matrix.  Estimated covariance matrix for double sums.}
	\item{coefficients.detrend	}{Matrix.  Coefficients of the double sum of double differences.  Normalised to be zero for first and last value.  For each parameter is reported coefficient, standard deviation, z-value, which is the ratio of those, and p-value.}
	\item{covariance.detrend	}{Matrix.  Estimated covariance matrix for detrended double sums.}
	\item{coefficients.demean	}{* Matrix.  Coefficients of the sum of differences.  Normalised to be zero for first value.  Does not apply is design is "APC" For each parameter is reported coefficient, standard deviation, z-value, which is the ratio of those, and p-value.}
	\item{covariance.demean		}{* Matrix.  Estimated covariance matrix for demeaned sums.}
	\item{coefficients.dif		}{* Matrix.  Coefficients of the differences.  Does not apply is design is "APC" For each parameter is reported coefficient, standard deviation, z-value, which is the ratio of those, and p-value.}
	\item{covariance.dif		}{* Matrix.  Estimated covariance matrix for differences.}
}
\note{* indicates that values only implemented for designs "AC", "AP", "PC".

The differences are not identified for design "APC".  An arbitrary level can be moved between differences for
age, period and cohort.  

The differences are not identified for designs "Ad", "Pd", "Cd".  These models have two linear trends and one set of
double differences.  In the model "Ad", as an example, one linear trend will be associated with age, but it is
arbitrary whether the second linear trend should be associated with period or cohort.  The slope of the age trend
will depend on that arbitrary choice.  In turn the level of the age differences will be arbitrary.  

(1)
The type "sum.sum" (same as "ss.dd") gives double sums anchored 
to be zero in the three points where
\code{age=cohort=U},
\code{age=U+1,cohort=U}
\code{age=U,cohort=U+1}
with
\code{apc.fit.model$U}
and where
\code{U}
is the integer value of
\code{(per.zero+3)/2}
This corresponds to the representation in
Nielsen (2014b).
The linear plane is parametrised in terms of 
a level, which is the value of the predictor at
\code{age=cohort=U};
an age slope, which is the difference of the values of the predictor at
\code{age=U+1,cohort=U}
and 
\code{age=cohort=U};
an cohort slope, which is the difference of the values of the predictor at
\code{age=U,cohort=U+1}
and 
\code{age=cohort=U}.

(2)
The type "detrend" gives double sums that start in zero and end in zero.
The linear plane is parametrised in terms of
a level, which is the value of the predictor at 
\code{age=cohort=1}, which is usually outside the index set for the data;
while age and cohort slopes are adjusted for the ad hoc identification of the time effects.

(3)
Subsumes \code{var.apc.identify} from \code{apc.indiv} (25 Sep 2020)
}
\references{
Kuang, D., Nielsen, B. and Nielsen, J.P. (2008a) Identification of the age-period-cohort model and the extended chain ladder model. Biometrika 95, 979-986. \emph{Download}: \href{https://doi.org/10.1093/biomet/asn026}{Article}; Earlier version \href{http://www.nuffield.ox.ac.uk/economics/papers/2007/w5/KuangNielsenNielsen07.pdf}{Nuffield DP}.

Nielsen, B. (2014b) Deviance analysis of age-period-cohort models. Work in progress.
}
\author{Bent Nielsen <bent.nielsen@nuffield.ox.ac.uk> & Zoe Fannon 25 Sep 2020 (12 Apr 2015)}
\seealso{The \href{http://users.ox.ac.uk/~nuff0078/apc/index.htm}{vignette} \href{http://users.ox.ac.uk/~nuff0078/apc/vignettes/Identification.pdf}{Identification.pdf}.}
\examples{
########################
#	Belgian lung cancer
# 	first an example with APC design, note that demean and dif not defined.

data.list	<- data.Belgian.lung.cancer()

fit.apc	<- apc.fit.model(data.list,"poisson.dose.response","APC")
fit.apc$coefficients.canonical
id.apc	<- apc.identify(fit.apc)
id.apc$coefficients.ssdd
id.apc$coefficients.detrend
id.apc$coefficients.demean
id.apc$coefficients.dif

fit.ap	<- apc.fit.model(data.list,"poisson.dose.response","AP")
fit.ap$coefficients.canonical
id.ap	<- apc.identify(fit.ap)
id.ap$coefficients.ssdd
id.ap$coefficients.detrend
id.ap$coefficients.demean
id.ap$coefficients.dif

}