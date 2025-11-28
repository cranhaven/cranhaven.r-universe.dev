\name{UKMplot}
\alias{UKMplot}

\title{
Univariate Kaplan-Meier survival curve plot
}
\description{
Function UKMplot is used to plot outcome of survival analysis of patients in a 
cohort using a specified gene or a signature.
}
\usage{
UKMplot(data,mol,HR="hazard risk", time="month",status="status", sml="hv",
      quant=c("No",-0.2,0.2), plotmethod="plot",adjx)
}

\arguments{
  \item{data}{
survival data in which columns contain patient survival data such as survival
status, survival month, age, sex,cell type and genes and rows list all patients.
Status and sex are binary data. In status, 1 is defined as an event occurrece
(such as death) and 0 as no event occurrence (such as alive). Month, age, and
genes, signature are numeric and continuous data.
}
\item{mol}{
char value specified a gene symbol or a gene id or a signature existed in data 
or int value for a column number of a specified gene or signature in data.
}
\item{HR}{
a string for specifying hazard risk or hazard rate. If HR="Hazard risk" or 
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
string for survival status of patients, may be "relapse" or "recurrence" or
"death". User can set status="death", "relapse" or "recurrence","status"
depending on user's survival data. The default value is "status".
}
 \item{sml}{
 surv.median.line: character vector for drawing a horizontal/vertical line at 
 median survival. Allowed values include one of c("none", "hv", "h", "v"). v: 
 vertical, h:horizontal.The value of sml is used in ggsurvplot, not in plot. 
 The default is "hv".
 }
 \item{quant}{
vector for quantile, low and high values. the low and high values are used to
 define or classify low and high expression groups. If quantile is "yes" or 
 "YES", then the low and high are non-negative precent values, for example, 
 quant=c("yes",0.25,0.75). If quantile is "no", then the low and high values 
 are z-scores, the low value may be negative. For example, quant=c("no",-0.2,
 0.2). The quantile = "yes" or = "No" may produce different results of survival
 analysis. User should carefully choose quantile or no quantile according to
 the data. The default values are c("no",-0.2,0.2).
 }
\item{plotmethod}{
string value: choose a method to plot Kaplan-Meier survival curves. In current 
version, we have two methods for chose: plot and ggsurvplot. The defauld is
plot method
}
\item{adjx}{
a numeric value used to adjust x-axis position of p-value and HR in plot.
}
}
\details{
UKMplot is a function invoked by MKMplot. By invoking UMKplot, MKMplot can make
a set of survival plots for a set of genes or a set of signatures.
}
\value{
outputs a survival curve plot of a specified gene.
}

\author{
Yuan-De Tan\cr
\email{yxt477@med.miami.edu}\cr
\email{tanyuande@gmail.com}\cr
Yuguang Ban \cr
\email{Yuguang.ban@med.miami.edu}
}
\note{
For a multivariate survival analysis, user can use MVKMplot to plot survival
outcome. 

All inputting parameters are not sensitive to upper or lower. That is, user can
input upper or lower string or letter. For example, both time ="MONTH" or time =
"month" work.
}

\seealso{
\code{\link{MKMplot},\link{MMKMplot},\link{MUKMplot}}
}
\examples{
require(survival)
require(ggplot2)
data(TCGA_survivalData)
UKMplot(data=TCGA_survivalData,mol=78,time="month", status="status",
quant=c("No",-0.2,0.2),adjx=100)
UKMplot(data=TCGA_survivalData,mol="RPS28",time="month", status="status",adjx=100)
}

\keyword{ survival analysis }
\keyword{ gene or signature }

