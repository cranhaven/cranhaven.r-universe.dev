\name{plot.elrm}
\alias{plot.elrm}
\title{Plot Diagnostics for an elrm Object}
\description{Produces both a trace plot and histogram of the sampled values of each sufficient statistic of interest. Sampled values within the burn-in period are also plotted.}
\usage{\method{plot}{elrm}(x, p = 1, breaks = "Sturges", ask=FALSE, ...)}
\arguments{
\item{x}{an object of class \code{elrm}, resulting from a call to \command{elrm()} or a previous call to \command{update()}.}
\item{p}{the sampling fraction of points to be plotted. A random sample consisting of \var{p}*100\% of all the observations in the Markov chain is plotted; default=1.}
\item{breaks}{a vector giving the number of cells to use for the histogram of each sufficient statistic of interest or a single number giving the number of cells for each histogram or the character string naming an algorithm to compute the number of cells.}
\item{ask}{the graphics parameter \var{ask}: see \code{\link{par}} for details. If set to TRUE will ask between plots corresponding to each sufficient statistic; default=FALSE.}
\item{\dots}{additional arguments to the plot function (currently unused).}
}
\value{No return value. Creates a plot of the sampled sufficent statistics for each parameter of interest.}
\details{The default for \var{breaks} is "Sturges": see \code{\link[grDevices]{nclass.Sturges}}. Other names for which algorithms are supplied are "Scott" and "FD".}
\references{
Zamar, D., McNeney, B., & Graham, J. (2007). elrm: Software Implementing Exact-Like Inference for Logistic Regression Models. Journal of Statistical Software, 21(3), 1-18.

Zamar, D., Monte Carlo Markov Chain Exact Inference for Binomial Regression Models. Master's thesis, Statistics and Actuarial Sciences, Simon Fraser University, 2006

Forster, J.J., McDonald, J.W. & Smith, P.W.F. Markov chain Monte Carlo exact inference for binomial and multinomial logistic regression models. Statistics and Computing 13, 169-177 (2003).

Geyer, C.J. Practical Markov chain Monte Carlo. Statistical Science, 7:473-511, 1992
}
\author{David Zamar, Jinko Graham, Brad McNeney}
\seealso{\code{\link{update.elrm}}, \code{\link{summary.elrm}}, \code{\link{elrm}}.}
\examples{
# Drug dataset example with treatment as the variables of interest
data(drugDat);
drug.elrm = elrm(formula=recovered/n~sex+treatment, interest=~treatment, r=4, 
	iter=2000, burnIn=100, dataset=drugDat);

# Plot the sampled values of the sufficient statistic for the parameter(s) of 
# interest
plot(drug.elrm,p=0.10,ask=TRUE);
}
\keyword{methods}

