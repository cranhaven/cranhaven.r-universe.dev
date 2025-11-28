\name{musvtest}
\alias{musvtest}

\title{
Multiple univariate suvival tests with a set of genes
}
\description{
Function musvtest performs SKMCresult by an iteration from the specified first 
gene to the specified last gene. The output of musvtest is used to build a 
weight vector for signature survival analysis.

}
\usage{
musvtest(sdata,stn,gn,time="month",status="status",quant=c("No",-0.2,0.2))
}

\arguments{
  \item{sdata}{
survival and gene-expression data containing patients in row, survival status
for death or recurrence or relapse, survival time such as month, day or year
and a set of genes in columns.
}
  \item{stn}{
character value specifying the first gene symbol or gene id existed in sdata or
int value specifying column number for the first gene in survival data.
}
  \item{gn}{
character value specifying the last gene symbol or gene id existed in sdata or 
int value specifying column number for the last gene in survival data.
}

\item{time}{
string for survival time and may be one of c("day", "month", "year"), depending 
on the clinical data. The default value is "month".
}
\item{status}{
string for survival status which is binary variable: 1 for event
occurrence and 0 for no event occurrence. status may be "death","relapse"
or "recurrence", depending on clinical data.
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
Patient survival status is a binary variable with 1 for an event (such as death)
and 0 for no event(such as alive). Genes have expression values(numeric values),
which are used to calculate z-scores for classifying patients into two groups:
high-expression patients and low-expression patients. SKMCresult performs
univariate Cox proportional hazard survival analyses of patients with
expression values of a specified gene and outputs hazard risk (HR), z-score
and p-value of this specified gene. At first, user can run this function by
performing musvtest to screen genes for prognostic signature by using HRs,
z-scores, and p-values. Once getting a set of genes for signature, user can
perform this function to build a weight vector using
\deqn{w_i=\frac{log_{10}(p_i)}{\sum_{i=1}^g{log_{10}(p_i)}}}
where \eqn{p_i} is p-value for Ward-test of gene i. For a patient,
the signature score or expression value is given by weighting expression values
of genes in the signature:
\deqn{y_j=\sum_{i}^g{w_ix_{ij}}}

where \eqn{x_{ij}} is expression of gene i in patient j.
}
\value{
output a matrix with n rows for gene name and hazard risk, hazard rate, standard
error, z-value and p-value of each gene.
}


\author{
Yuan-De Tan\cr
\email{yxt477@med.miami.edu}\cr
\email{tanyuande@gmail.com}
Yuguang Ban \cr
\email{Yuguang.ban@med.miami.edu}
}
\note{
All inputting parameters are not sensitive to upper or lower. That is, user can
input upper or lower string or letter. For example, both time ="MONTH" or time =
"month" work.
}

\seealso{
\code{\link{SKMCresult}}
}
\examples{
data(GSE50081)
res<-musvtest(sdata=GSE50081,stn=3500,gn=3506,time="month",status="status")
#res
#              Gene Hazad risk hazard rate standard error    z-value     p-value
#3500  X209170_s_at -0.6510414   0.5215024      0.3133664 -2.0775721 0.037748792
#3501   X1556325_at -0.6041918   0.5465159      0.3507459 -1.7225913 0.084962455
#3502    X228915_at -0.4992865   0.6069636      0.3394520 -1.4708606 0.141328818
#3503 X1555216_a_at -0.5465844   0.5789238      0.3143044 -1.7390290 0.082029656
#3504  X203548_s_at -0.2004345   0.8183751      0.3018504 -0.6640193 0.506677970
#3505    X205433_at -1.3528063   0.2585138      0.4134465 -3.2720229 0.001067809
#3506    X209614_at -0.8389441   0.4321666      0.3905470 -2.1481262 0.031703733
}

\keyword{ univariate survival }
\keyword{multiple genes}

