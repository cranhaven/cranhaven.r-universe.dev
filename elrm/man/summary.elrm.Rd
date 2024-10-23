\name{summary.elrm}
\alias{summary.elrm}
\title{Summarize an elrm Object}
\description{Summary method for class \code{elrm} that formats and prints out the results of an \code{elrm} object.}
\usage{\method{summary}{elrm}(object, ...)}
\arguments{
  \item{object}{an object of class \code{elrm}, resulting from a call to \command{elrm()} or a previous call to \command{update()}.}
  \item{\dots}{additional arguments to the summary function (currently unused).}
}
\value{No return value. Results are printed to the screen.}
\details{The following results are formatted and printed to the screen: the matched call, coefficient estimates and confidence intervals for each model term of interest, estimated p-value for jointly testing that the parameters of interest are simultaneously equal to zero, full conditional p-values from separately testing each parameter equal to zero, length of the Markov chain that inference was based on, and the Monte Carlo standard error of each reported p-value.}
\references{
Zamar, D., McNeney, B., & Graham, J. (2007). elrm: Software Implementing Exact-Like Inference for Logistic Regression Models. Journal of Statistical Software, 21(3), 1-18.

Zamar, D., Monte Carlo Markov Chain Exact Inference for Binomial Regression Models. Master's thesis, Statistics and Actuarial Sciences, Simon Fraser University, 2006

Forster, J.J., McDonald, J.W. & Smith, P.W.F. Markov chain Monte Carlo exact inference for binomial and multinomial logistic regression models. Statistics and Computing 13, 169-177 (2003).

Geyer, C.J. Practical Markov chain Monte Carlo. Statistical Science, 7:473-511, 1992
}
\author{David Zamar, Jinko Graham, Brad McNeney}
\seealso{\code{\link{update.elrm}}, \code{\link{plot.elrm}}, \code{\link{elrm}}.}
\examples{
# Drug dataset example with sex as the variable of interest
data(drugDat);
drug.elrm = elrm(formula=recovered/n~sex+treatment, interest=~sex, r=4, 
	iter=2000, burnIn=100, dataset=drugDat); 

# Summarize the results:
summary(drug.elrm);

\donttest{
# Urinary tract dataset example with dia as the variable of interst
data(utiDat);
uti.elrm = elrm(uti/n~age+current+dia+oc+pastyr+vi+vic+vicl+vis, interest=~dia, r=4, 
	iter=30000, burnIn=100, dataset=utiDat);

# Summarize the results:
summary(uti.elrm);
}
}
\keyword{methods}


