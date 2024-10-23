\name{elrm}
\alias{elrm}
\title{elrm: exact-like inference in logistic regression models}
\description{\code{elrm} implements a modification of the Markov Chain Monte Carlo algorithm proposed by Forster et al. (2003) to approximate exact conditional inference for logistic regression models. The modifications can handle larger datasets than the original algorithm (Zamar 2006). Exact conditional inference is based on the distribution of the sufficient statistics for the parameters of interest given the sufficient statistics for the remaining nuisance parameters. Using model formula notation, users specify a logistic model and model terms of interest for exact inference.}
\usage{
elrm(formula, interest, r = 4, iter = 1000, dataset, burnIn = 0, alpha = 0.05)
}
\arguments{
  \item{formula}{a formula object that contains a symbolic description of the logistic regression model of interest in the usual R formula format. One exception is that the binomial response should be specified as \var{success/trials}, where \var{success} gives the number of successes and \var{trials} gives the number of binomial trials for each row of \var{dataset}.}
  \item{interest}{a formula object that contains a symbolic description of the model terms for which exact conditional inference is of interest.}
  \item{r}{a parameter of the MCMC algorithm that influences how the Markov chain moves around the state space. Small values of \var{r} cause the chain to take small, relatively frequent steps through the state space; larger values cause larger, less frequent steps. The value of \var{r} must be an even integer less than or equal to the length of the response vector. Typical values are 4, 6 or 8; default=4.}
  \item{iter}{an integer representing the number of Markov chain iterations to make (must be larger than or equal to 1000); default=1000.}
  \item{dataset}{a data.frame object where the data are stored.}
  \item{burnIn}{the burn-in period to use when conducting inference. Values of the Markov chain in the burn-in period are discarded; default=0.}
  \item{alpha}{determines the level used for confidence intervals; default=0.05.}
}
\details{
The function \command{summary()} (i.e., \code{\link{summary.elrm}}) can be used to obtain or print a summary of the results. \cr \cr 
Each estimated exact p-value is based on the conditional probabilities test. \cr \cr 
The Monte Carlo standard error of each p-value is computed by the batch-means method (Geyer C.J. 1992). \cr \cr 
Inference on each parameter must be based on a Markov chain of at least 1000 iterations, otherwise \code{NA} is returned. \cr \cr 
If the observed value of the sufficient statistic for a parameter is either the maximum or the minimum value sampled, the MUE of the parameter is given instead of the CMLE. In such cases, the resulting confidence interval is open-ended on one side. \cr \cr

Apart from the documentation files accompanying this package, the \pkg{elrm} package vignette may be downloaded from https://www.jstatsoft.org/article/view/v021i03. The vignette is also distributed with the code.
}
\value{
  \item{coeffs}{a vector containing the parameter estimates.}
  \item{coeffs.ci}{a list containing (1-\var{alpha})*100\% confidence intervals for each parameter of interest.}
  \item{p.values}{a vector containing the estimated p-value for jointly testing that the parameters of interest are simultaneously equal to zero, and the full conditional p-values from separately testing each parameter equal to zero.}
  \item{p.values.se}{a vector containing the Monte Carlo standard errors of the estimated p-values of each term of interest.}
  \item{mc}{an \code{\link[coda]{mcmc}} object containing the Markov chain of sampled values of the sufficient statistics for the parameters of interest. Columns correspond to parameters; rows to Monte Carlo iterations.}
  \item{mc.size}{a vector containing the lengths of the extracted Markov chains used in testing each parameter. The length of the Markov chain used for the joint test (i.e., \var{iter}) is also included as the first element.}
  \item{obs.suff.stat}{a vector containing the observed value of the sufficient statistic for each parameter of interest.}
  \item{distribution}{a list containing distribution tables for the sampled values of the sufficient statistic of the parameters of interest conditional on all the rest.}
  \item{call.history}{a list composed of the matched call and the history of calls to \command{update()}.}
  \item{dataset}{the data.frame object that was passed to \command{elrm()} as an argument.}
  \item{last}{the last response vector sampled by the Markov chain.}
  \item{r}{the value of \var{r} passed to \command{elrm()} as an argument.}
  \item{ci.level}{the level used when constructing the confidence intervals for the parameters of interest. The level is calculated as (1-\var{alpha})*100\%.}
}
\section{Warning}{The labels of the terms in the in the \var{interest} model should match those found in the \var{formula} model. Thus, the \var{term.labels} attribute of \code{terms.formula(\var{interest)}} should match those found in \code{terms.formula(\var{formula})}. Please see the Examples section for more details.}
\references{
Zamar, D., McNeney, B., & Graham, J. (2007). elrm: Software Implementing Exact-Like Inference for Logistic Regression Models. Journal of Statistical Software, 21(3), 1-18.

Zamar, D., Monte Carlo Markov Chain Exact Inference for Binomial Regression Models. Master's thesis, Statistics and Actuarial Sciences, Simon Fraser University, 2006

Forster, J.J., McDonald, J.W. & Smith, P.W.F. Markov chain Monte Carlo exact inference for binomial and multinomial logistic regression models. Statistics and Computing 13, 169-177 (2003).

Geyer, C.J. Practical Markov chain Monte Carlo. Statistical Science, 7:473-511, 1992
}
\author{David Zamar, Jinko Graham, Brad McNeney}
\seealso{\code{\link{update.elrm}}, \code{\link{summary.elrm}}, \code{\link{plot.elrm}}.}
\examples{
# Drug dataset example with sex and treatment as the variables of interest
data(drugDat);
drug.elrm = elrm(formula=recovered/n~sex+treatment, interest=~sex+treatment, 
	r=4,iter=40000, burnIn=1000, dataset=drugDat);

\donttest{
# crash dataset example where the terms of interest are age and 
# the interaction of age and velocity.
data(crashDat);
crash.elrm = elrm(formula=y/n~vel+age+vel:age, interest=~vel:age, r=4, iter=20000, 
	dataset=crashDat, burnIn=100);

# Urinary tract dataset example with dia as the variable of interest
data(utiDat); 
uti.elrm = elrm(uti/n~age+current+dia+oc+pastyr+vi+vic+vicl+vis, interest=~dia,r=4, 
	iter=20000,burnIn=1000, dataset=utiDat);}
}
\keyword{methods}
