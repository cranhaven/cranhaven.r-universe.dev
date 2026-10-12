\name{sensspec}
\alias{sensspec}
\title{Sensitivity and Specificity}
\description{Compute sensitivity and specificity}
\usage{
sensspec(X,etime,status,u=NULL,tt,s,vtimes,cutoff=0,fc=NULL)
}
\arguments{
  \item{X}{n by S matrix of longitudinal score/biomarker for i-th
  subject at j-th occasion (NA if unmeasured)}
   \item{etime}{n vector with follow-up times}
   \item{status}{n vector with event indicators}
   \item{u}{Lower limit for evaluation of sensitivity and
   specificity. Defaults to \code{vtimes[s]} (see below)}
   \item{tt}{Upper limit (time-horizon) for evaluation of sensitivity
and specificity.}
\item{s}{Scalar number of measurements/visits to use for each subject. s<=S}
\item{vtimes}{S vector with visit times}
\item{cutoff}{cutoff for definining events. Defaults to \code{0}}
\item{fc}{Events are defined as fc = 1. Defaults to $I(cup X(t_j)>cutoff)$}
}

\details{
Sensitivity and specificities for a time-dependent multiply-measured
marker are defined as 

Se(t,c,s,u) = Pr(f_c(X(t_1),X(t_2),...,X(t_{s_i}))| u <= T <= t),

and

Sp(t,c,s,u) = 1-Pr(f_c(X(t_1),X(t_2),...,X(t_{s_i})) | T > t) 

for some fixed f_c, where c is a cutoff. 
The default for f_c is that a positive diagnosis is given as soon as
any measurement among the s considered is above the threshold. 
}
\value{
  A vector with the following elements: 
	\tabular{ll}{
	\code{sens} \tab Sensitivity at the cutoff \cr
	\code{spec} \tab Specificity at the cutoff \cr
	}
}
\references{
Barbati, G. and Farcomeni, A. (2017) Prognostic assessment of
repeatedly measured time-dependent biomarkers, with application to
dilated cardiomuopathy, \emph{Statistical Methods \& Applications}, in
press 
}
\author{Alessio Farcomeni \email{alessio.farcomeni@uniroma1.it}}
\seealso{
  \cite{\link{roc}}, \cite{\link{auc}}, \cite{\link{butstrap}}, \cite{\link{maxauc}}
}


