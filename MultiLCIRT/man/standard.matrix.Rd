\name{standard.matrix}
\alias{standard.matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Standardization of a matrix of support points on the basis of a vector of probabilities}
\description{Given a matrix of support points X and a corresponding vector of probabilities piv
it computes the mean for each dimension, the variance covariance matrix, the correlation matrix, 
Spearman correlation matrix, and the standarized matrix Y}
\usage{standard.matrix(X,piv)}

%- maybe also 'usage' for other objects documented here.
\arguments{
\item{X}{matrix of support points for the distribution included row by row}
\item{piv}{vector of probabilities with the same number of elements as the rows of \code{X}}
}

%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}

\value{
\item{mu}{vector of the means}
\item{V}{variance-covariance matrix}
\item{si2}{vector of the variances}
\item{si}{vector of standard deviations}
\item{Cor}{Braives-Pearson correlation matrix}
\item{Sper}{Spearman correlation matrix}
\item{Y}{matrix of standardized support points}
}

\author{Francesco Bartolucci, Silvia Bacci, Michela Gnaldi - University of Perugia (IT)}

%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{
## Example of standardization of a randomly generated distribution
X = matrix(rnorm(100),20,5)
piv = runif(20); piv = piv/sum(piv)
out = standard.matrix(X,piv)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate statistics}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
