\name{dependence}
\alias{dependence}
\title{
Dependence Measures
}
\description{
Calculation of pairwise monotone and supremum dependence, monotone/supremum dependence ratio, and proportion of pairwise NAs.
}
\usage{
dependence(mat, cont = NULL, sc.order = 0)
}
\arguments{
  \item{mat}{
    \eqn{k}-column matrix with \eqn{n} observations of a \eqn{k}-dimensional random vector (NA values are allowed).
}
  \item{cont}{
    vector of column numbers to consider/coerce as continuous random variables (optional).
}
  \item{sc.order}{
    order of subcopula approximation (continuous random variables). If \eqn{0} (default) then maximum order \eqn{m = n} is used. Often \eqn{m = 50} is a good recommended value, higher values demand more computing time.
}
}
\details{
  Each of the random variables in the \eqn{k}-dimensional random vector under consideration may be of any kind (discrete, continuous, or mixed). NA values are allowed.
}
\value{
A 3-dimensional array \eqn{k\times k\times 4} with pairwise monotone and supremum dependence, monotone/supremum dependence ratio, and proportion of pairwise NAs.
}
\references{
Erdely, A. (2017) \emph{A subcopula based dependence measure}. Kybernetika 53(2), 231-243. DOI: 10.14736/kyb-2017-2-0231

Nelsen, R.B. (2006) \emph{An Introduction to Copulas}. Springer, New York.
}
\author{
  Arturo Erdely \url{https://sites.google.com/site/arturoerdely}
}
\note{
  NA values are allowed.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{subcopem}}, \code{\link{subcopemc}}
}
\examples{
V <- runif(300)  # Continuous Uniform(0,1)
W <- V*(1-V)     # Continuous transform of V
# X given V=v as continuous Uniform(0,v)
X <- mapply(runif, rep(1, length(V)), rep(0, length(V)), V)
Y <- 1*(0.2 < X)*(X < 0.6)  # Discrete transform of X
Z <- X*(0.1 < X)*(X < 0.9) + 1*(X >= 0.9)  # Mixed transform of X
V[1:10] <- NA  # Introducing some NAs
W[3:12] <- NA  # Introducing some NAs
Y[5:25] <- NA  # Introducing some NAs
vector5D <- cbind(V, W, X, Y, Z)  # Matrix of 5-variate observations
# Monotone and supremum dependence, ratio and proportion of NAs:
(deparray <- dependence(vector5D, cont = c(1, 2, 3), 30))
# Pearson's correlations:
cor(vector5D, method = "pearson", use = "pairwise.complete.obs")
# Spearman's correlations:
cor(vector5D, method = "spearman", use = "pairwise.complete.obs") 
# Kendall's correlations:
cor(vector5D, method = "kendall", use = "pairwise.complete.obs")   
pairs(vector5D)  # Matrix of pairwise scatterplots
}
