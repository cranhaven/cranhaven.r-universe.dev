\name{update.elrm}
\alias{update.elrm}
\title{Update Method for Objects of Class elrm.}
\description{An update method for objects created by \command{elrm()}. Extends the Markov chain of an \code{elrm} object by a specified number of iterations.}
\usage{\method{update}{elrm}(object, iter, burnIn = 0, alpha = 0.05, ...)}
\arguments{
  \item{object}{an object of class \code{elrm}, resulting from a call to \command{elrm()} or a previous call to \command{update()}.}
  \item{iter}{an integer representing the number of Markov chain iterations to make.}
  \item{burnIn}{the burn-in period to use when conducting inference. Values of the Markov chain in the burn-in period are discarded; default=0.}
  \item{alpha}{determines the level used for confidence intervals; default=0.05.}
  \item{\dots}{additional arguments to the update function (currently unused).}
}
\details{Extends the Markov chain of an \code{elrm} object by creating a new Markov chain of the specified length using the last sampled value as the starting point. The newly created chain is then appended to the original. Subsequent inference is based on the extended Markov chain.}
\value{An object of class \code{elrm}.}
\references{
Zamar, D., McNeney, B., & Graham, J. (2007). elrm: Software Implementing Exact-Like Inference for Logistic Regression Models. Journal of Statistical Software, 21(3), 1-18.

Zamar, D., Monte Carlo Markov Chain Exact Inference for Binomial Regression Models. Master's thesis, Statistics and Actuarial Sciences, Simon Fraser University, 2006

Forster, J.J., McDonald, J.W. & Smith, P.W.F. Markov chain Monte Carlo exact inference for binomial and multinomial logistic regression models. Statistics and Computing 13, 169-177 (2003).

Geyer, C.J. Practical Markov chain Monte Carlo. Statistical Science, 7:473-511, 1992
}
\author{David Zamar, Jinko Graham, Brad McNeney}
\seealso{\code{\link{summary.elrm}}, \code{\link{plot.elrm}}, \code{\link{elrm}}.}
\examples{
# Drug dataset example with sex and treatment as the variables of interest
data(drugDat);
drug.elrm = elrm(formula=recovered/n~sex+treatment, interest=~treatment, r=4, 
	iter=2000, burnIn=0, dataset=drugDat); 

# Summarize the results
summary(drug.elrm);

# Call update and extend the chain by 15000 iterations and set the burn-in 
# period to 100 iterations
drug.elrm = update(drug.elrm, iter=3000, burnIn=100);

# Summarize the results
summary(drug.elrm);

# Now change the burn-in to 500
drug.elrm = update(drug.elrm, iter=0, burnIn=500);

# Summarize the results
summary(drug.elrm);
}
\keyword{methods}
