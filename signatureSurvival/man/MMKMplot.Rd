\name{MMKMplot}
\alias{MMKMplot}

\title{
Multiple multivariate Kaplan-Meier survival curve plots
}
\description{
Function MMKMplot plots outcomes of multivariate survival analyses of patients 
with covariates age and sex in a cohort with a specified set of genes. Each gene
has a plot and each plot takes a page.
}
\usage{
MMKMplot(sdata, stn, gn, X, HR="hazard risk", status = "status", 
time = "month", sml="hv",quant=c("No",-0.2,0.2),plotmethod="plot",adjx,
outdir, file)
}

\arguments{
  \item{sdata}{
survival data including clinical data such as status, month, age, sex, and
expressions of genes or expressions of proteins. Status such as death,
recurrence or relapse must be 1 for event (such as death) or 0 for no event
(such as alive), month is numeric data, age is numeric data, sex is binary data
and genes are numeric data. They are listed in columns and patients are listed
in rows.
}
  \item{stn}{
character value specifying gene symbol or gene id existed in sdata or int value
specifying column number for the first gene in survival data.
}
  \item{gn}{
character value specifying gene symbol or gene id existed in sdata or int value
specifying column number for the last gene in survival data.
}

  \item{X}{
string vector for covariate(s), an option with inputting names of one, two, three
or more covariates. For example, X="smoking", X=c("age","sex","smoking","stage_n"
). The number of covariates is limited to 10. All covariates should be listed
in the data.
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
  \item{status}{
string for a specifying status name. For example, status may be death, relapse
or recurrence. User can set status="death", "relapse" or "recurrence" or
"status", depending on the data.
}
  \item{time}{
string for survival time and may be one of c("day", "month", "year"), depending 
on the clinical data. The default value is "month".
}
 \item{sml}{
 surv.median.line: character vector for drawing a horizontal/vertical line at 
 median survival. Allowed values include one of c("none", "hv", "h", "v"). v: 
 vertical, h:horizontal.The value of sml is used in only ggsurvplot. The default
 is "hv".
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
version, we have two methods for choice: plot and ggsurvplot. The defauld is
plot method. In plot method, we show p-value for result of Ward t-test but in 
ggsurvplot, we show p-value for the result of log-rank-sum test.
}
  \item{adjx}{
numeric value used to adjust x-axis position of p-value and HR in only plot. 
}
  \item{outdir}{
string, a path to save a file. If user uses setwd to set a dir for saving file,
the outdir can be set "NULL", MMKMplot can automatically save pdf file in this
dir folder in user local computer.
}
  \item{file}{
string for file name. If user sets a path with setwd or a path to outdir, then
file just sets file name. The file is a pdf file containing all plot pages.  
}
}

\value{
output multiple pdf pages for multiple survival curve plots and saved in a
outputting pdf file. If file ="NULL", MUKMplot would output the last survival 
plot page.
}

\author{
Yuan-De Tan\cr
\email{yxt477@med.miami.edu}\cr
\email{tanyuande@gmail.com}\cr
Yuguang Ban \cr
\email{Yuguang.ban@med.miami.edu}
}
\note{
User may not perform this function if unnecessary.

All inputting parameters are not sensitive to upper or lower. That is, user can
input upper or lower string or letter. For example, both time ="MONTH" or time =
"month" work.
}

\seealso{
\code{\link{UKMplot},\link{MKMplot},\link{MUKMplot}}
}
\examples{

data(GSE50081)
XX=c("age","sex","t.stage","n.stage","m.stage")
hr="hazard rate"

MMKMplot(sdata=GSE50081,stn="X232578_at", gn="down",X=XX,HR=hr,status="status1", 
time="month", quant=c("yes",0.25,0.75),plotmethod="plot",adjx = 3.5,file =
"NULL")

}

\keyword{ multivariate survival analysis }
\keyword{ multiple survival curve plots }
