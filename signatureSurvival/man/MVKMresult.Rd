\name{MVKMresult}
\alias{MVKMresult}

\title{
Multivariate survival analysis with multiple specified independent variables
}
\description{
Function MVKMresult performs multivariate Cox proportional hazard survival analysis with
a set of patients and an independent variable (a specified gene or a
feature) and a set of covariates (age, sex and/or smoking or stage) and outputs
hazard risks (HR), z-scores, and p-values of the gene and these covariates.
}
\usage{
MVKMresult(data,X,mol, status="status",time="month",quant=c("No",-0.2,0.2))
}

\arguments{
  \item{data}{
survival data containing p patients in row, s survival variables such as
survival status for death, recurrence or relapse, survival time such as day,
month,or year, covariates age, sex and/or smoking, etc. and a set of genes in
column.
}
\item{X}{
string for covariate(s), number of covariates is limited to 10. If X=NULL, 
then MVKMresult would be stopped.
}
\item{mol}{
int value for column number of the first gene specified.
}
\item{status}{
string for survival status of patients, may be "relapse" or "recurrence" or
"death". User can set status="death", "relapse" or "recurrence","status"
depending on user's survival data. The default value is "status".
}
\item{time}{
string for survival time and may be one of c("day", "month", "year"), depending 
on the clinical data. The default value is "month".
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
}
\details{
Function MVKMresult performs multivariate Cox proportional hazard survival 
analyses with p patients and a specified gene and a set of covariates specified 
and outputs hazard risk (HR),z-score and p-value for the specified gene or
signature and covariates.
}
\value{
output a matrix with one row and multiple columns for gene name, Hazard risk, 
hazard rate, standard error, z-value, p-value of gene and covariates.
}

\author{
Yuan-De Tan\cr
\email{yxt477@med.miami.edu}\cr
\email{tanyuande@gmail.com}\cr
Yuguang Ban \cr
\email{Yuguang.ban@med.miami.edu}
}
\note{
User can also use SKMCresult to screen a prognostic signature and a weight
vector without covariates.

All inputting parameters are not sensitive to upper or lower. That is, user can
input upper or lower string or letter. For example, both time ="MONTH" or time =
"month" work.
}

\seealso{
\code{\link{SKMCresult}},\code{\link{musvtest}},\code{\link{mvstest}}
}
\examples{

data(GSE50081)
res<-MVKMresult(data=GSE50081,X=c("t.stage","n.stage","m.stage"),mol=3500,
status="status",time="month",quant=c("no",-0.2,0.2))
}

\keyword{ multivariates }
\keyword{ multiple survival analyses }

