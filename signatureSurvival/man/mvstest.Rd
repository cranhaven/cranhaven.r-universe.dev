\name{mvstest}
\alias{mvstest}

\title{
Multivariate Cox proportional hazard survival analyses with multiple genes
}
\description{
Function mvstest performs MVKMresult by iteration from the first gene to the 
last gene. The output result of musvtest is used to build a vector of weights for
screening genes as prognostic signature or doing signature survival analysis.
}
\usage{
mvstest(sdata,X,stn,gn, status,time,quant=c("No",-0.2,0.2))
}

\arguments{
  \item{sdata}{
survival data containing p patients in row, survival status for death or
recurrence or relapse, survival time such as day, month, or year, and
covariates age, sex and/or smoking, tumor stage, or chemotherapy, and a set of
genes in column.
}
\item{X}{
string specifying one or multiple covariates such as age, sex, smoking,
 or tumor stage or genotype. The number of covariates in X is limited to 10.
}

\item{stn}{
character value specifying gene symbol or gene id existed in sdata or int value
specifying column number for the first gene in survival data.
}
  \item{gn}{
character value specifying gene symbol or gene id existed in sdata or int value
specifying column number for the last gene in survival data.
}

\item{status}{
string for survival status which must be binary variable: 1 for event
occurrence and 0 for no event occurrence. status may be "death", "relapse"
or "recurrence", depending on clinical data.
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
Patient survival status is binary variable with 1 for event (such as death)
and 0 for no event (such as alive). Genes have expression values(numeric values),
which are used to calculate z-scores for classifying patients into two groups:
high-expression patients and low-expression patients. SKMCresult performs
univariate Cox proportional hazard survival analyses of patients with
expression values of a specified gene and outputs hazard risk (HR), z-score
and p-value of this specified gene. At first, user can run this function by
performing msvtest to screen genes for prognostic signature by using HRs,
z-scores, and p-values. Once getting a set of genes for signature, user can
perform this function to build a weight vector using
\deqn{w_i=\frac{log_{10}(p_i)}{\sum_{i=1}^g{log_{10}(p_i)}}}
where \eqn{p_i} is p-value for Ward-test of gene i. For a patient, the signature
score or expression value is given by weighting expression values of g genes in
the signature:
\deqn{y_j=\sum_{i}^g{w_ix_{ij}}}

where \eqn{x_{ij}} is expression of gene i in patient j.

}
\value{
output a matrix with n rows for n genes, m+1 columns for Hazard risk, hazard
rate, standard error, z-value, p-value of gene and m covariates.

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
\code{\link{MVKMresult}}
}
\examples{

data(GSE50081)

res<-mvstest(sdata=GSE50081,X=c("t.stage","n.stage"),stn=3550,gn=3555,
status="status",time="month",quant=c("No",-0.2,0.2))


}

\keyword{multivariate survival}
\keyword{multiple genes}
