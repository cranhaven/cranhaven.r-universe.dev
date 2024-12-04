\name{weightComp}
\alias{weightComp}
\title{
  Computing  weights from the degree of the inferred network
}
\description{
  This function computes weights from the degree of estimated network using the weighted Lasso approach}
\usage{
  weightComp(dat,lam=0.4,w.mb)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dat}{
    An input matrix. The columns represent variables and the rows indicate observations.
}
  \item{lam}{
A penalty parameter of the weighted Lasso that controls the sparsity of the inferred network.
}
  \item{w.mb}{
An unput weight vector which is computed from the degree of the inferred network. 
}
}

%%\details{
%%  This function computes weights from the degree of estimated network
%%}
\value{
\item{d.mb}{
    Weight vector computed from degree of the inferred network}
}


%%\references{
%%  Nurgazy Sulaimanov, Sunil Kumar, Frederic Burdet, Mark Ibberson, Marco Pagni, Heinz Koeppl. Inferring hub networks using weighted degree Lasso.
%%}

%%\author{
%%  Nurgazy Sulaimanov, Sunil Kumar and Heinz Koeppl
%%}
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

# Compute weights from the inferred network
w.mb <- rep(1,p)
w.Mat <- weightComp(dat,lam=0.4,w.mb)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%%\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
%%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
