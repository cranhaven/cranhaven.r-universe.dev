\name{est_multi_poly_clust}
\alias{est_multi_poly_clust}
%- Also NEED an '\alias' for EACH other topic documented here.

\title{Estimate multidimensional and multilevel LC IRT model for dichotomous and polytomous responses}

\description{The function performs maximum likelihood estimation of the parameters of the IRT models
assuming a discrete distribution for the ability and a discrete distribution for the latent variable
at cluster level. Every ability level corresponds to a latent
class of subjects in the reference population. Maximum likelihood estimation is based on Expectation-
Maximization algorithm.}

\usage{est_multi_poly_clust(S, kU, kV, W = NULL, X = NULL, clust,
                     start = 0, link = 0,  disc = 0, difl = 0,
                     multi = 1:J, piv = NULL, Phi = NULL,
                     gac = NULL, DeU = NULL, DeV = NULL,
                     fort = FALSE, tol = 10^-10, disp = FALSE,
                     output = FALSE)}
%- maybe also 'usage' for other objects documented here.

\arguments{
  \item{S}{matrix of all response sequences observed at least once in the sample and listed row-by-row
           (use NA for missing response)}
  \item{kU}{number of support points (or latent classes at cluster level)}
  \item{kV}{number of ability levels (or latent classes at individual level)}
  \item{W}{matrix of covariates that affects the weights at cluster level}
  \item{X}{matrix of covariates that affects the weights at individual level}
  \item{clust}{vector of cluster indicator for each unit}
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
  \item{DeU}{initial value of regression coefficients for the covariates in W (if start=2)}
  \item{DeV}{initial value of regression coefficients for the covariates in X (if start=2)}
  \item{fort}{to use fortran routines when possible}
  \item{tol}{tolerance level for checking convergence of the algorithm as relative difference between 
  	         consecutive log-likelihoods}
  \item{disp}{to display the likelihood evolution step by step}
  \item{output}{to return additional outputs (Phi,Pp,Piv)}
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
\item{separ}{Standard errors for vector of parameters containing Th and Be (if output=TRUE)}
\item{sega}{Standard errors for vector of discrimination indices (if output=TRUE)}
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

\dontrun{
# generate covariate at cluster level
nclust = 200
W = matrix(round(rnorm(nclust)*2,0)/2,nclust,1)
la = exp(W)/(1+exp(W))
U = 1+1*(runif(nclust)<la)
clust = NULL
for(h in 1:nclust){
	nh = round(runif(1,5,20))
	clust = c(clust,h*rep(1,nh))	
} 
n = length(clust)

# generate covariates
DeV = rbind(c(1.75,1.5),c(-0.25,-1.5),c(-0.5,-1),c(0.5,1))
X = matrix(round(rnorm(2*n)*2,0)/2,n,2)
Piv = cbind(0,cbind(U[clust]==1,U[clust]==2,X)\%*\%DeV)
Piv = exp(Piv)*(1/rowSums(exp(Piv)))
V = rep(0,n)
for(i in 1:n) V[i] = which(rmultinom(1,1,Piv[i,])==1)

# generate responses
la = c(0.2,0.5,0.8)
Y = matrix(0,n,10)
for(i in 1:n) Y[i,] = runif(10)<la[V[i]]

# fit the model with k1=3 and k2=2 classes
out1 = est_multi_poly_clust(Y,kU=2,kV=3,W=W,X=X,clust=clust)
out2 = est_multi_poly_clust(Y,kU=2,kV=3,W=W,X=X,clust=clust,disp=TRUE,
                            output=TRUE)
out3 = est_multi_poly_clust(Y,kU=2,kV=3,W=W,X=X,clust=clust,disp=TRUE,
                            output=TRUE,start=2,Phi=out2$Phi,gac=out2$gac,
                            DeU=out2$DeU,DeV=out2$DeV)
# Rasch                            
out4 = est_multi_poly_clust(Y,kU=2,kV=3,W=W,X=X,clust=clust,link=1,
                            disp=TRUE,output=TRUE)
out5 = est_multi_poly_clust(Y,kU=2,kV=3,W=W,X=X,clust=clust,link=1,
                            disc=1,disp=TRUE,output=TRUE)
}

}

%% Add one or more standard keywords, see file 'KEYWORDS' in the
%% R documentation directory.
\keyword{maximum likelihood estimation}
\keyword{Expectation-Maximization algorithm}% __ONLY ONE__ keyword per line
