\name{mined}
\alias{mined}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Minimum Energy Design
%%  ~~function to do ... ~~
}
\description{
Generate MinED samples from an unnormalized density function.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mined(initial, logf, K_iter = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{initial}{
  An n-by-p matrix containing the initial uniform samples from \code{[0,1]^p}. 
%%     ~~Describe \code{ExploreDesign} here~~
}
  \item{logf}{
  An R function to compute the logarithm of unnormalized density function. The input region should be scaled in \code{[0,1]^p}.
%%     ~~Describe \code{logFunc} here~~
}
  \item{K_iter}{
  The number of iteration steps for annealed version of the unnormalized posterior density. Optional, default is \code{0}. If \code{0}, \code{K_iter = ceiling(4 * sqrt(p))} is used.
  %%  ~~Describe \code{K_iter} here~~
  }
}
\details{
%%  ~~ If necessary, more details than the description above ~~
This is the main function of the package, which is used for generating the MinED samples. The MinED sample can be viewed as a deterministic sample from the probability density specified in the mined function. Since only the unnormalized density is needed to generate the MinED samples, this method could be used in Bayesian computation to approximate the posterior. The method uses few evaluations of the unnormalized posterior compared to random sampling-based methods and therefore, it will be useful when the evaluations are expensive or time consuming.

There are many parameters that control the performance of the algorithm, which are fixed at some reasonable values as specified in Joseph et al. (2019). The only thing user need to choose is the region for scaling the variables in [0,1]^p. Ideally it should be the highercube containing the highest posterior density region with good coverage. However, the algorithm is robust to this choice to some extend as it can shrink or expand from the intial region. Therefore, it can be chosen based on user's guessed range of each variable.
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
The value returned from the function is a list containing the following components:
\item{points}{A matrix containing \code{n} MinED samples.}
\item{logf}{Log-unnormalized density function values of MinED samples.}
\item{cand}{Full set of samples used in the algorithm.}
\item{candlf}{Log-unormalized density function values of the samples in \code{cand}.}
}
\references{
Joseph, V. R., Wang, D., Gu, L., Lv, S., and Tuo, R. (2019). "Deterministic Sampling of Expensive Posteriors Using Minimum Energy Designs", \emph{Technometrics}, 61, 297-308, arXiv:1712.08929, DOI:10.1080/00401706.2018.1552203.
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Dianpeng Wang <wdp@bit.edu.cn> and V. Roshan Joseph <roshan@gatech.edu>
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
require(mined)
p <- 2
n <- 109 # largest prime number less than 100+5p
initial <- Lattice(n, p)

# suppose x1 is in [-40,40] and x2 in [-25,10]
logf <- function(para)
{
  l1 <- -40
  u1 <- 40
  l2 <- -25
  u2 <- 10
  x1 <- l1 + (u1 - l1) * para[1]
  x2 <- l2 + (u2 - l2) * para[2]
  val <- -.5 * (x1 ^2 / 100 + (x2+ .03 * x1^2 -3)^2)
  return(val)
}

res <- mined::mined(initial, logf, K_iter = 8)
dim(res$points)
dim(res$cand)

x1 <- seq(0, 1, length.out = 200)
x2 <- seq(0, 1, length.out = 200)
y <- matrix(0.0, 200, 200)
for(i in 1:200)
{
  for(j in 1:200)
  {
    y[i, j] = logf(c(x1[i], x2[j]))
  }
}
image(x1, x2, exp(y), col = cm.colors(5), xlab = expression(x[1]), ylab = expression(x[2]))
points(res$cand[, 1], res$cand[, 2], pch = 11, col = rgb(red = 0, green = 0, blue = 1, 
       alpha = 0.35), cex = .25)
points(res$points[, 1], res$points[, 2], pch = 17, col = 'black', cex = .75)
legend("bottom", c('Candidates points', 'MinED samples'), pch = c(11, 17), 
        col = c(rgb(red = 0, green = 0, blue = 1, alpha = 0.35), 'black'), 
        inset = .02, bg = 'transparent', bty = 'n')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{Bayesian Computation}% use one of  RShowDoc("KEYWORDS")
\keyword{Quasi-Monte Carlo}% __ONLY ONE__ keyword per line
\keyword{Experimental Design}
