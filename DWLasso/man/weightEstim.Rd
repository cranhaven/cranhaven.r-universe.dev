\name{weightEstim}
\alias{weightEstim}
\title{
Estimating weights from the degree of the inferred network
}
\description{
This function estimates weigths from the degree of the inferred network using iterative procedure. This function is called from the main functon DWLasso.R 
}
\usage{
weightEstim(dat, lam=0.4, a=1, tol=1e-6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
An input matrix. The columns represent variables and the rows indicate observations.
}
\item{lam}{
A penalty parameter that controls degree sparsity of the inferred network
}
  \item{a}{
A parameter of the update equation that controls the convergence of weights
}
  \item{tol}{
Tolerance
}

}
%%\details{
%%This function estimates weigths from the degrees of the inferred networks using iterative procedure. The algorithm stops when the estimated weights converge to steady state. 
%%}
\value{
\item{w.dat}{
Estimated weight vector from the last iteration at which the algorithm converges}
}
\references{
Nurgazy Sulaimanov, Sunil Kumar, Frederic Burdet, Mark Ibberson, Marco Pagni, Heinz Koeppl. Inferring hub networks using weighted degree Lasso. http://arxiv.org/abs/1710.01912.
}
\author{
Nurgazy Sulaimanov, Sunil Kumar and Heinz Koeppl
}
%%\note{
%%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%%}

\examples{
library(DWLasso)
library(glmnet)
library(hglasso)


# Generate inverse covariance matrix with 3 hubs
# 20 % of the elements within a hub are zero
# 97 % of the elements that are not within hub nodes are zero
p <- 60 # Number of variables
n <- 40 # Number of samples

hub_number = 3  # Number of hubs

# Generate the adjacency matrix
Theta <- HubNetwork(p,0.97,hub_number,0.2)$Theta

# Generate a data matrix
out <- rmvnorm(n,rep(0,p),solve(Theta))

# Standardize the data
dat <- scale(out)

# Estimate weights from the degrees of the inferred network
w.est <- weightEstim(dat, lam=0.4, a=1, tol=1e-6)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
