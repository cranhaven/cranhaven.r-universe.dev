\name{test_dim}
\alias{test_dim}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Likelihood ratio testing between nested multidimensional LC IRT models}
\description{The function tests a certain multidimensional model (restricted model) 
against a larger multidimensional model based on a higher number of dimensions. A
typical example is testing a unidimensional model (and then the hypothesis of unidimensionality)
against a bidimensional model. Both models are estimated by \code{est_multi_poly}.}

\usage{test_dim(S, yv, k, link = 1, disc = 0, difl = 0, multi0 = 1:J,
         multi1, tol = 10^-10, disp = FALSE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S}{matrix of all response sequences observed at least once in the sample and listed row-by-row
           (use 999 for missing response)}
  \item{yv}{vector of the frequencies of every response configuration in \code{S}}
  \item{k}{number of ability levels (or latent classes)}
  \item{link}{type of link function (1 = global logits, 2 = local logits);
            with global logits the Graded Response model
            results; with local logits the  Partial Credit results (with dichotomous responses, global logits
            is the same as using local logits resulting in the Rasch or the 2PL model depending on the value
            assigned to disc)}  \item{disc}{indicator of constraints on the discriminating indices 
            (0 = all equal to one, 1 = free)}
  \item{difl}{indicator of constraints on the difficulty levels (0 = free, 1 = rating scale parametrization)}
  \item{multi0}{matrix specifying the multidimensional structure of the restricted model}
  \item{multi1}{matrix specifying the multidimensional structure of the larger model}
  \item{tol}{tolerance level for checking convergence of the algorithm as relative difference between 
  	         consecutive log-likelihoods}
  \item{disp}{to display intermediate output}
}

%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}

\value{
	\item{out0}{output for the restricted model obtained from \code{est_multi_poly}}
	\item{out1}{output for the larger model obtained from \code{est_multi_poly}}
	\item{dev}{likelihood ratio statistic}
	\item{df}{number of degrees of freedom of the test}
	\item{pv}{\emph{p}-value for the test}
	\item{call}{command used to call the function}  
}

\references{
Bartolucci, F. (2007), A class of multidimensional IRT models for testing unidimensionality and clustering
items, \emph{Psychometrika}, \bold{72}, 141-157.

Bacci, S., Bartolucci, F. and Gnaldi, M. (2012), A class of Multidimensional Latent Class IRT models for 
ordinal polytomous item responses, \emph{Technical report}, http://arxiv.org/abs/1201.4667.}

\author{Francesco Bartolucci, Silvia Bacci, Michela Gnaldi - University of Perugia (IT)}

%\note{
%%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%% ~~objects to See Also as \code{\link{help}}, ~~~
%}

\examples{
## Computation of the LR statistic testing unidimensionality on HADS data
# Aggregate data
data(hads)
X = as.matrix(hads)
out = aggr_data(X)
S = out$data_dis
yv = out$freq
# Define matrix to allocate each item on one dimension 
multi1 = rbind(c(2,6,7,8,10,11,12),c(1,3,4,5,9,13,14))
# Compare unidimensional vs bidimensional Graded Response models with free
# discrimination and free difficulty parameters
# with less severe tollerance level (to be increased)
out = test_dim(S,yv,k=3,link=1,disc=1,multi1=multi1,tol=5*10^-4)
}

%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
\keyword{likelihood ratio testing}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
