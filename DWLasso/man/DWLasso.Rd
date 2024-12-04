\name{DWLasso}
\alias{DWLasso}
\alias{DWLasso}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Degree weighted lasso
}
\description{
Infers undirected networks with hubs using weighted nodewise regression approach. The method contains two parameters that control hub and overall sparsity, respectively.
}
\usage{
DWLasso(X, lambda1 = 0.4, lambda2 = 2, a = 1, tol = 1e-05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
An input matrix. The columns represent variables and the rows indicate observations.
}
  \item{lambda1}{
A penalty parameter that controls degree sparsity of the inferred network
}
  \item{lambda2}{
A penalty parameter that controls overall sparsity of the inferred network
}
  \item{a}{
A parameter of the update equation that controls the convergence of weights
}
  \item{tol}{
Tolerance
}
}
\details{
This implements weighted degree lasso using coordinate descent algorithm (implemented in Glmnet package) described in Sulaimanov et al.. The method is based on the weighted nodewise regression approach and infers large undirected networks with hubs in iterative manner in the setting more variables than samples (p>n). Given p variables, the network is inferred by regressing each variable against the remaining (p-1) variables.
  The penalty parameter, lambda1 controls the degree sparsity of the network, whereas the penalty parameter, lambda2 controls the overall sparsity.The method uses a fast Lasso solver Glmnet (Friedman et al. (2010)) with default settings.

}

\value{
\item{mat}{
The estimated matrix corresponding to the inferred network. The diagonal elements of the matrix are zero}
\item{weights}{
The estimated weights used to estimate the network. These weights are computed from the degree of estimated networks}
\item{lambda1}{
The value of the penalty parameter controlling degree sparsity of the inferred network.
}
  \item{lambda2}{
The value of the penalty parameter controlling the overall sparsity}
}



\references{
1. Nurgazy Sulaimanov, Sunil Kumar, Frederic Burdet, Mark Ibberson, Marco Pagni, Heinz Koeppl. Inferring hub networks using weighted degree Lasso. http://arxiv.org/abs/1710.01912.

2. Jerome Friedman, Trevor Hastie, Robert Tibshirani (2010). Regularization Paths for Generalized Linear Models via Coordinate Descent. Journal of Statistical Software, 33(1), 1-22. URL http://www.jstatsoft.org/v33/i01/.

3. Tan, KM., London, P., Mohan, K., Lee, S-I., Fazel, M., and Witten, D. (2014). Learning graphical models with hubs. Journal of Machine Learning Research. 5.1 (2014): 3297-3331.

4. Meinshausen, Nicolai, and Peter Bühlmann. "High-dimensional graphs and variable selection with the lasso." The annals of statistics (2006): 1436-1462.
}


\author{
Nurgazy Sulaimanov, Sunil Kumar, Frederic Burdet, Mark Ibberson, Marco Pagni, Heinz Koeppl.

Maintainer: Nurgazy Sulaimanov, nurgazy.sulaimanov@bcs.tu-darmstadt.de
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

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

# Run DWLasso
out.p <- DWLasso(dat, lambda1 = 0.6, lambda2 = 10)

# print out a summary of the output
summary(out.p)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
