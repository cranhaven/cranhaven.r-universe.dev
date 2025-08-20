\name{networkInferenceGlassoBIC}
\alias{networkInferenceGlassoBIC}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Network inference using the glasso algorithm
%%  ~~function to do ... ~~
}
\description{
This function performs network inference using the glasso algorithm for several regularization parameters and selects a network based on the BIC of the model.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
networkInferenceGlassoBIC(dataNet, nb.rho = 100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataNet}{
    matrix of data 
%%     ~~Describe \code{dataNet} here~~
}
  \item{nb.rho}{
  number of regularization parameters to test in the glasso algorithm
%%     ~~Describe \code{nb.rho} here~~
}
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
  \item{A}{selected adjacency matrix based on BIC}
  \item{Theta}{selected precision matrix based on BIC}
    \item{Sigma}{selected covariance matrix based on BIC}
        \item{penaltieslist}{list of regularization parameters}
  \item{pathA}{list of adjacency matrices for each regularization parameter}
  \item{pathTheta}{list of precision matrices for each regularization parameter}
\item{pathSigma}{list of covariance matrices for each regularization parameter}
\item{pathBIC}{list of BIC values for each regularization parameter}

}
\references{
\url{https://cran.r-project.org/web/packages/glasso/glasso.pdf}
%% ~put references to the literature/web site here ~
}

\examples{


## load data to test 
data(dataTest)

## perform network inference 
resNet <- networkInferenceGlassoBIC(dataTest)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~network }
%\keyword{ ~i }% __ONLY ONE__ keyword per line
