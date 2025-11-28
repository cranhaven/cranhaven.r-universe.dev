\name{MKMplot}
\alias{MKMplot}

\title{
Multivariate Kaplan-Meier survival curve plot
}
\description{
Function MKMplot plots the outcome of multivariate survival analysis of patients 
in a cohort. Independent variable is specified as a gene or a signature. 
Covariates are sex, age, stage, genotype etc. If user needs one or multiple 
covariates, for example,smoking, tumor stage, etc., user can use X to specify 
the covariates.
}
 \usage{
MKMplot(data, mol, X, HR="hazard risk",time = "month", status = "status", 
sml="hv",quant=c("No",-0.2,0.2),plotmethod="plot",adjx)
}

\arguments{
  \item{data}{
survival data including clinical data such as status, month, age, sex and/or
smoking and expression data of genes or signatures.
}
  \item{mol}{
char value specified a gene symbol or a gene id or a signature existed in data 
or int value for a column number of a specified gene or a signature in data.
}
  \item{X}{
a string or string vector for one or multiple specified covariates. For example,
X="smoking" or X=c("age","sex",smoking","genotype"). 10 covariates are limited 
in the current version. All covariates should be listed in the data.
}
  \item{HR}{
a string to specify hazard risk or hazard rate. If HR="Hazard risk" or 
"Hazard_risk", then plot would show coefficient, which has negative, zero, or
positive value. The domain is (-\eqn{\infty},\eqn{\infty}). The Negative value 
indicates reduction of hazard risk while the positive value suggests
increment of risk. Zero suggests no change of hazard risk. If HR = "hazard rate"
or "hazard_rate", then plot would show exp(coefficient), which the value is 
from 0 to \eqn{\infty} and HR < 1 indicates that hazard risk is reduced and 
HR > 1 means that hazard risk is increased. The default is "hazard risk". HR is 
only used in plot.
}
  \item{time}{
string for survival time and may be one of c("day", "month", "year"), depending 
on the clinical data. The default value is "month".
}
  \item{status}{
string to specify status name in the input survival data. For example,
status may be "death", "relapse", or "status", depending on user's data.
The default value is "status".
}
 \item{sml}{
 surv.median.line: character vector for drawing a horizontal/vertical line at 
 median survival. Allowed values include one of c("none", "hv", "h", "v"). v: 
 vertical, h:horizontal. The value of sml is used in only ggsurvplot. The default
 is "hv".
 }
 
 \item{quant}{
 vector for quantile, low and high values. the low and high values are used to
 define or classify low and high expression groups. If quantile is "yes" or 
 "YES", then the low and high are non-negative precent values, for example, 
 quant=c("yes",0.25,0.75). If quantile is "no", then the low and high values 
 are z-scores, the low value may be negative. For example, quant=c("no",-0.2,
 0.2). The quantile = "yes" or = "No" may produce different results of survival
 analysis. User should carefully choose qunatile or no quantile according to
 the data. The default values are c("no",-0.2,0.2).
 }
 \item{plotmethod}{
string value: choose a method to plot Kaplan-Meier survival curves. In current 
version, we have two methods for chose: plot and ggsurvplot. The defauld is
plot method. In plot method, we show p-value for result of Ward t-test but in 
ggsurvplot, we show p-value for the result of log-rank test.
}
  \item{adjx}{
numeric value used to adjust x-axis position of p-value and HR in only plot. 
}
}
\details{
survival data contain clinical data such as status, month, age, sex and/or
smoking and expression of gene or protein. The status may be death, recurrence 
or relapse and must be a binary variable: 1 for an event (such as death) or 0 for 
no event (such as alive). The month, age, and gene are numeric continuous 
variables but sex is a binary variable. The other covariates may be 
numeric continuous or binary variables. They are listed in column and patients or
observations are listed in row.
}
\value{
output a survival curve plot of multivariate survival analysis.
}

\author{
Yuan-De Tan\cr
\email{yxt477@med.miami.edu}\cr
\email{tanyuande@gmail.com}\cr
Yuguang Ban \cr
\email{Yuguang.ban@med.miami.edu}
}

\note{
To plot outcome of univariate survival, user can use UKMplot.
All inputting parametes are not sensitive to upper or lower. That is, user can
input upper or lower string or letter. For example, both time ="MONTH" or time =
"month" work.
}

\seealso{
\code{\link{UKMplot},\link{MUKMplot},\link{MMKMplot}}
}
\examples{
require(survival)
require(ggplot2)
data(GSE50081)
MKMplot(data=GSE50081,mol=56,X=c("t.stage","n.stage",	"m.stage"),time="month",
status="status1",sml="none",quant=c("No",-0.2,0.2),plotmethod="ggsurvplot",
adjx = 5)

}

\keyword{ multivariate }
\keyword{ survival analysis }

