\name{search.model}
\alias{search.model}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Search for the global maximum of the log-likelihood}
\description{It search for the global maximum of the log-likelihood given a vector of possible number
	of classes to try for.}
\usage{search.model(S, yv = rep(1,ns), kv, X = NULL, link = 0, disc = 0,
             difl = 0, multi = 1:J, fort = FALSE, tol = 10^-10,
             nrep = 2, glob = FALSE, disp=FALSE)}

%- maybe also 'usage' for other objects documented here.
\arguments{
\item{S}{matrix of all response sequences observed at least once in the sample and listed row-by-row
         (use 999 for missing response)}
\item{yv}{vector of the frequencies of every response configuration in \code{S}}
\item{kv}{vector of the possible numbers of latent classes}
\item{X}{matrix of covariates that affects the weights}
\item{link}{type of link function (1 = global logits, 2 = local logits);
            with global logits the Graded Response model
            results; with local logits the  Partial Credit results (with dichotomous responses, global logits
            is the same as using local logits resulting in the Rasch or the 2PL model depending on the value
            assigned to disc)}
\item{disc}{indicator of constraints on the discriminating indices (0 = all equal to one, 1 = free)}
\item{difl}{indicator of constraints on the difficulty levels (0 = free, 1 = rating scale parametrization)}
\item{multi}{matrix with a number of rows equal to the number of dimensions and elements in each row
               equal to the indices of the items measuring the dimension corresponding to that row}
\item{fort}{to use fortran routines when possible}
\item{tol}{tolerance level for checking convergence of the algorithm as relative difference between 
  	         consecutive log-likelihoods}
\item{nrep}{number of repetitions of each random initialization}
\item{glob}{to use global logits in the covariates}
\item{disp}{to dispaly partial output}  	         
}

%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}

\value{
\item{out.single}{output of each single model (as from est_multi_poly) for each k in kv}
\item{bicv}{value of BIC index for each k in kv}
\item{lkv}{value of log-likelihood for each k in kv}
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
\dontrun{
## Search Multidimensional LC IRT models for binary responses
# Aggregate data
data(naep)
X = as.matrix(naep)
out = aggr_data(X)
S = out$data_dis
yv = out$freq
# Define matrix to allocate each item on one dimension 
multi1 = rbind(c(1,2,9,10),c(3,5,8,11),c(4,6,7,12))
out2 = search.model(S, yv = yv, kv=c(1:4),multi=multi1)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{model selection}
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
