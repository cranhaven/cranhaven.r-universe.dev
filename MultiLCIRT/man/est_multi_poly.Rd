\name{est_multi_poly}
\alias{est_multi_poly}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Estimate multidimensional LC IRT model for dichotomous and polytomous responses}

\description{The function performs maximum likelihood estimation of the parameters of the IRT models
assuming a discrete distribution for the ability. Every ability level corresponds to a latent
class of subjects in the reference population. Maximum likelihood estimation is based on Expectation-
Maximization algorithm.}

\usage{est_multi_poly(S, yv = rep(1,ns), k, X = NULL, start = 0, link = 0,
               disc = 0, difl = 0, multi = NULL, piv = NULL,
               Phi = NULL, gac = NULL, De = NULL, fort = FALSE,
               tol = 10^-10, disp = FALSE, output = FALSE,
               out_se = FALSE, glob = FALSE)}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{S}{matrix of all response sequences observed at least once in the sample and listed row-by-row
           (use NA for missing response)}
  \item{yv}{vector of the frequencies of every response configuration in \code{S}}
  \item{k}{number of ability levels (or latent classes)}
  \item{X}{matrix of covariates that affects the weights}
  \item{start}{method of initialization of the algorithm (0 = deterministic, 1 = random, 2 = arguments given as
               input)}
  \item{link}{type of link function (0 = no link function, 1 = global logits, 2 = local logits);
              with no link function the Latent Class model results; with global logits the Graded Response model
              results; with local logits the  Partial Credit results (with dichotomous responses, global logits
              is the same as using local logits resulting in the Rasch or the 2PL model depending on the value
              assigned to disc)}
  \item{disc}{indicator of constraints on the discriminating indices (0 = all equal to one, 1 = free)}
  \item{difl}{indicator of constraints on the difficulty levels (0 = free, 1 = rating scale parameterization)}
  \item{multi}{matrix with a number of rows equal to the number of dimensions and elements in each row
               equal to the indices of the items measuring the dimension corresponding to that row}
  \item{piv}{initial value of the vector of weights of the latent classes (if start=2)}
  \item{Phi}{initial value of the matrix of the conditional response probabilities (if start=2)}
  \item{gac}{initial value of the complete vector of discriminating indices (if start=2)}
  \item{De}{initial value of regression coefficients for the covariates (if start=2)}
  \item{fort}{to use fortran routines when possible}
  \item{tol}{tolerance level for checking convergence of the algorithm as relative difference between 
  	         consecutive log-likelihoods}
  \item{disp}{to display the likelihood evolution step by step}
  \item{output}{to return additional outputs (Phi,Pp,Piv)}
  \item{out_se}{to return standard errors}
  \item{glob}{to use global logits in the covariates}  
}

%\details{
%%%  ~~ If necessary, more details than the description above ~~
%}

\value{
\item{piv}{estimated vector of weights of the latent classes (average of the weights in case of model with covariates)}
\item{Th}{estimated matrix of ability levels for each dimension and latent class}
\item{Bec}{estimated vector of difficulty levels for every item (split in two vectors if difl=1)}
\item{gac}{estimated vector of discriminating indices for every item (with all elements equal to 1
	       with Rasch parametrization)}
\item{fv}{vector indicating the reference item chosen for each latent dimension}
\item{Phi}{array of the conditional response probabilities for every item and latent class}
\item{De}{matrix of regression coefficients for the multinomial logit model on the class weights}
\item{Piv}{matrix of the weights for every response configuration (if output=TRUE)}
\item{Pp}{matrix of the posterior probabilities for each response configuration and latent class
	      (if output=TRUE)}
\item{lk}{log-likelhood at convergence of the EM algorithm}
\item{np}{number of free parameters}
\item{aic}{Akaike Information Criterion index}
\item{bic}{Bayesian Information Criterion index}
\item{ent}{Etropy index to measure the separation of classes}
\item{lkv}{Vector to trace the log-likelihood evolution across iterations (if output=TRUE)}
\item{seDe}{Standard errors for De (if output=TRUE)}
\item{separ}{Standard errors for vector of parameters containing Th and Be (if out_se=TRUE)}
\item{sega}{Standard errors for vector of discrimination indices (if out_se=TRUE)}
\item{Vn}{Estimated variance-covariance matrix for all parameter estimates (if output=TRUE)}
}

\references{
Bartolucci, F. (2007), A class of multidimensional IRT models for testing unidimensionality and clustering
items, \emph{Psychometrika}, \bold{72}, 141-157.

Bacci, S., Bartolucci, F. and Gnaldi, M. (2014), A class of Multidimensional Latent Class IRT models for 
ordinal polytomous item responses, \emph{Communication in Statistics - Theory and Methods}, \bold{43}, 787-800.
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
## Estimation of different Multidimensional LC IRT models with binary
# responses
# Aggregate data
data(naep)
X = as.matrix(naep)
out = aggr_data(X)
S = out$data_dis
yv = out$freq
# Define matrix to allocate each item to one dimension 
multi1 = rbind(c(1,2,9,10),c(3,5,8,11),c(4,6,7,12))
# Three-dimensional Rasch model with 3 latent classes
# the tolerance level has been rise to increase the speed (to be reported
# to a smaller value)
out1 = est_multi_poly(S,yv,k=3,start=0,link=1,multi=multi1,tol=10^-6)
\dontrun{
# Three-dimensional 2PL model with 3 latent classes
out2 = est_multi_poly(S,yv,k=3,start=0,link=1,disc=1,multi=multi1)
}

\dontrun{
## Estimation of different Multidimensional LC IRT models with ordinal
# responses
# Aggregate data
data(hads)
X = as.matrix(hads)
out = aggr_data(X)
S = out$data_dis
yv = out$freq
# Define matrix to allocate each item to one dimension 
multi1 = rbind(c(2,6,7,8,10,11,12),c(1,3,4,5,9,13,14))
# Bidimensional LC Graded Response Model with 3 latent classes 
# (free discriminating and free difficulty parameters)
out1 = est_multi_poly(S,yv,k=3,start=0,link=1,disc=1,multi=multi1)
# Bidimensional LC Partial Credit Model with 3 latent classes 
# (constrained discrimination and free difficulty parameters)
out2 = est_multi_poly(S,yv,k=3,start=0,link=2,multi=multi1)
# Bidimensional LC Rating Scale Model with 3 latent classes 
# (constrained discrimination and constrained difficulty parameters)
out3 = est_multi_poly(S,yv,k=3,start=0,link=2,difl=1,multi=multi1)
}

\dontrun{
## Estimation of LC model with covariates
# gerate covariates
be = c(0,1,-1)
X = matrix(rnorm(2000),1000,2)
u = cbind(1,X)%*%be
p = exp(u)/(1+exp(u))
c = 1+(runif(1000)<p)
Y = matrix(0,1000,5)
la = c(0.3,0.7)
for(i in 1:1000) Y[i,] = runif(5)<la[c[i]]
# fit the model with k=2 and k=3 classes
out1 = est_multi_poly(Y,k=2,X=X)
out2 = est_multi_poly(Y,k=3,X=X)
# fit model with k=2 and k=3 classes in fortran
out3 = est_multi_poly(Y,k=2,X=X,fort=TRUE)
out4 = est_multi_poly(Y,k=3,X=X,fort=TRUE)
}
}

%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
\keyword{maximum likelihood estimation}
\keyword{Expectation-Maximization algorithm}% __ONLY ONE__ keyword per line
