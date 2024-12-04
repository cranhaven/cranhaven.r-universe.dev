\name{pfa}
\alias{pfa.test}
\alias{pfa.gwas}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ 
%%  ~~function to do ... ~~
Estimates False Discovery Proportion Under Arbitrary Covariance Dependence
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This package contains functions for performing multiple testing and estimating the false discovery proportion (FDP). \code{pfa.test(X,...)} finds the false nulls
in p hypotheses; \code{pfa.test(X,Y,...)} tests the difference of two multiple-dimensional population means; \code{pfa.gwas(X,Y,...)} performs the genome-wise association study(GWAS).
}
\usage{
pfa.test(X,Y,tval,Sigma,reg="L2",K,e=0.05,gamma,mat_est="poet",plot="-log")
pfa.gwas(X,Y,tval,v,reg="L1",e=0.05,gamma,K,plot="-log")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X,Y}{In \code{pfa.test}, either X and Y are data matrices of two different samples, or X is a vector of test statistics and Y is missing. In \code{pfa.gwas}, X is the design matrix and Y contains observations of the response variable.
%%
}
  \item{Sigma}{the covariance matrix.
%%
}
  \item{v}{standard deviation of noises. By default, it is estimated by refitted cross-validation.
%% 	
  }
  \item{tval}{ a sequence of thresholding level for p-values. By default, \code{tval} is chosen automatically.
%%     
}
  \item{reg}{method used to estimate factors. If reg="L1", the method is least absolute value regression; if reg="L2", the method is least-squares (with large outliers filtered out). Default is "L1".
%%     
}
  \item{K}{ number of factors. By default, given the covariance matrix, K is the smallest integer such that the sum of squares of the smallest (p-K) eigenvalues is no larger than e=0.01 times its trace.
%%     
}
  \item{e}{ a parameter used to choose the number of factors in PFA. Default value is 0.01.
%%     
}
  \item{gamma}{ a parameter used to estimate the true Null proportion: pi0 = (percentage of (p-values > gamma))  /(1- gamma). By default, it is chosen automatically.
%%     
}
  \item{mat_est}{method used to estimate the covariance matrix. If mat_est="sample", the estimate is the (pooled) sample covariance matrix; if mat_est="poet", the estimate comes from the \code{poet} package. Default is "poet".
}
%%
\item{plot}{ plotting mode. If plot="-log", in the FDP plot, the x axis is -log(t); if plot="log", the x axis is log(t); if plot="linear", the x axis is t; if plot="none", no graph is generated. Default is "-log".
 %% 	
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
\code{pfa.test(X,Sigma=Sigma,...)}: X is a vector of test statistics. Suppose it has a multivariate Gaussian distribution \code{N(mu, Sigma)}. 
We would like to test: \code{mu(i)=0, i=1,...,p}. Given a threshold \code{t}, we reject hypothesis \code{i} if and only if \code{P(i)=X(i)/sqrt(Sigma(i,i))<t},\code{i=1,...,p}. We apply the PFA method [Fan, Han and Gu (2012)] to estimate the false discovery proportion (FDP) for arbitrary thresholds.
In this case, the covariance matrix \code{Sigma} is required. 


\code{pfa.test(X,...)}: X is an n-by-p matrix containing i.i.d. samples of the multivariate Gaussian distrubtion \code{N(mu, Sigma)}. Again, we would like to test: \code{mu(i)=0}, i=1,...,p. 
The test statistics is the vector of sample mean. When \code{Sigma} is unknown, we instead use the sample covariance matrix or the estimate from POET-PFA method [Fan and Han (2016)]. The number of factors determined for POET and for PFA is based on the eigenvalue ratio test in Ahn and Horenstein (2013). 

\code{pfa.test(X,Y,...)}: X and Y are i.i.d. samples from the distributions \code{N(mu1, Sigma)} and \code{N(mu2, Sigma)}. We would like to test: \code{mu1(i)=mu2(i)}, i=1,...,p. The test statistics
is the vector of sample mean difference. When \code{Sigma} is unknown, we instead use the pooled sample covariance matrix or the estimate from POET-PFA method.

\code{pfa.gwas(X,Y,...)}: X is the data matrix of \code{p} covariates (e.g. SNP measurements) and Y is the data vector of response variables (e.g. indicator of a trait).
We would like to test: whether covariate \code{i} is marginally associated with the response, \code{i=1,...,p}. The test statistics is the maginal regression coefficients. 
We suppose \code{Y=X_1*beta_1+...+X_p*beta_p+epsilon}, where \code{epsilon}'s are i.i.d. samples from \code{N(0,sigma^2)}. Then the 
covariane matrix of the test statistics is nothing but the sample covariance matrix of X. 
}

\value{ 
	Four graphs will be generated for each of the functions: one histogram of p-values; number of total rejections, number of false rejections, and FDPs all indexed by t. 

It returns an object of class \code{PDFresults}, which is a list containing the following components:
\item{Pvalue}{Sorted p-values.}
\item{adjPvalue}{Sorted adjusted p-values.}
\item{FDP}{Estimated FDPs.}
\item{pi0}{Estimated true null proportion.}
\item{K}{Number of factors used in the PFA method.}
\item{sigma}{Estimated standard deviation of noises. This component is NULL except in the return of \code{pfaRegress}.}
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
Fan, Han and Gu (2012) "Estimating False Discovery Proportion Under Arbitrary Covariance Dependence" (with discussion) JASA.

Fan and Han (2016) "Estimation of False Discovery Proportion with Unknown Dependence", Manuscript.
}
\author{Jianqing Fan, Tracy Ke, Sydney Li and Lucy Xia.
	
	Maintainer: Tracy Ke <zke@galton.uchicago.edu>, Lucy Xia <lucyxia@stanford.edu>.
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
1. The estimated FDP does not necessarily decrease as the threshold \code{t} increases (although the number of total rejections and estimated false rejections both decrease as \code{t} increases). 
As a result, the estimated FDP curve is sometimes zigzag. Moreover, two values of \code{t} can yield to different estimated FDP values even if they give exactly the same rejections.

2. In \code{pfa.gwas}, when the standard deviation of noises, \code{v}, is not provided, we apply refitted cross validation [Fan,Guo and Hao (2012)] 
to estimate \code{v}. This may take some time (especially when the dimension p is large), and the results can be different at each running, due to random data splits.
An input of \code{v} is suggested in this case.

3. Sometimes people want to use the sequence of obtained p-values as the sequence of thresholds. This can be implemented by setting \code{tval}="pval", see Example 4 below.

4. It is generally better to use the factor-adjusted p-values, but there is no universal conclusion on which is better. One way is to plot both histograms and see, if ignoring 
a neighborhood of 0, which one is closer to the uniform distribution. 

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
Ahn and Horestein (2013) "Eigenvalue Ratio Test for the Number of Factors", Econometrica. 	
	
Fan, Guo and Hao (2012)"Variance Estimation Using Refitted Cross-Validation in Ultrahigh Dimensional Regression" JRSSB.

Fan, Liao and Mincheva (2013) "Large Covariance Estimation by Thresholding Principal Orthogonal Complements", JRSSB .
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
# Example 1: multiple testing with known covariance

require(MASS)
p <- 100
Sigma <- matrix(0.4,p,p)
diag(Sigma)<- 1
mu <- as.vector(c(rep(3,5), rep(0, p-5)))
Z <- mvrnorm(1, mu, Sigma)
RE1 <- pfa.test(Z, Sigma=Sigma,reg="L1")
summary(RE1)

# Example 2: multiple testing with unknown covariance 

n <- 200
p <- 300
K <- 3
mu <- as.vector(c(rep(2,10),rep(0,p-10)))
B <- matrix(runif(K*p, min=-1, max=1), nrow=K)
f <- matrix(rnorm(K*n), nrow=n)
Bf <- f \%*\% B
X <- matrix(rep(0, n*p), nrow=n)
for (i in 1:n) 
   X[i,] <- mu + Bf[i,] + rnorm(p)
\dontrun{RE2 <- pfa.test(X, tval="pval")}
\dontrun{summary(RE2)}

# Example 3: testing the marginal regression coefficients

n <- 100
p <- 300
beta <- as.matrix(c(rep(2, 10), rep(0, p-10)))
X <- matrix(rep(0, n*p), nrow=n)
X[,1:10] <- matrix(rnorm(n*10), nrow=n)
z <- as.matrix(rnorm(n))
y <- as.matrix(rnorm(n))
X[,11:p] <- as.matrix(rnorm(n*(p-10)), nrow=n)
for (i in 11:p) {
    rho1 <- runif(1,min=-0.2,max=0.2)
    rho2 <- runif(1,min=-0.2,max=0.2)
    X[,i] <- X[,i] + z*rho1 + y*rho2 
}
eps <- as.matrix(rnorm(n))
Y <- X \%*\% beta + eps
\dontrun{RE3 <- pfa.gwas(X,Y)}
\dontrun{summary(RE3)}

# Example 4: GWAS on the CCT8 gene

data(CEU)
\dontrun{RE4 <- pfa.gwas(CEU$x, CEU$y, t=exp(-seq(1.8,3.6,0.1)), reg="L2")}
\dontrun{summary(RE4)}

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
