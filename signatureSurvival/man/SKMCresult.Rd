\name{SKMCresult}
\alias{SKMCresult}

\title{
Univariate Cox proportional hazard survival analysis with a specified
independent variable
}
\description{
Function SKMCresult performs univariate Cox proportional hazard survival analysis 
of patients with a specified independent variable or a gene without covariates
and outputs hazard risk (HR), z-score, and p-value of this specified gene.
}
\usage{
SKMCresult(data,mol, time="month", status="status",quant=c("No",-0.2,0.2))
}

\arguments{
  \item{data}{
survival and gene-expression data containing patients in row, survival status
for death, recurrence or relapse, survival time and a set of genes in column.
}
\item{mol}{
char value specified a gene symbol or a gene id or a signature existed in data 
or int value for a column number of a specified gene or a signature in data.
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
Patient survival status is binary variable with 1 for event (such as death)
and 0 for no event(such as alive). Genes have expression values(numeric values),
which are used to calculate z-scores for classifying patients into two groups:
high-expression patients and low-expression patients. SKMCresult performs
univariate Cox proportional hazard survival analyses of patients with
expression values of a specified gene and outputs hazard risk (HR), z-score
and p-value of this specified gene.
}
\value{
output a matrix with one row and 6 columns for gene name, Hazard risk, hazard
rate, standard error, z-value, p-value of the specified gene.
}

\author{
Yuan-De Tan\cr
\email{yxt477@med.miami.edu}\cr
\email{tanyuande@gmail.com}\cr
Yuguang Ban \cr
\email{Yuguang.ban@med.miami.edu}
}
\note{
All inputting parameters are not sensitive to upper or lower. That is, user can
input upper or lower string or letter. For example, both time ="MONTH" or time =
"month" work.
}

\seealso{
\code{\link{MVKMresult}},\code{\link{musvtest}},\code{\link{mvstest}}
}
\examples{
data(GSE50081)
res<-SKMCresult(data=GSE50081,mol=350, time="month", status="status")

}
 \keyword{ univariate survival }
 \keyword{ Cox hazard proportional model}
