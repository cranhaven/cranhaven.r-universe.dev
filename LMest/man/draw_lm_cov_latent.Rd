\name{draw_lm_cov_latent}
\alias{draw_lm_cov_latent}

\title{Draw samples from LM model with covariaates in the latent model}

\description{Function that draws samples from the LM model with individual covariates with specific parameters.  \cr \cr \bold{The function is no longer maintained. Please look at} \code{\link{draw.LMlatent}} \bold{function}.}

\usage{
draw_lm_cov_latent(X1, X2, param = "multilogit", Psi, Be, Ga, fort = TRUE)
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X1 }{desing matrix for the covariates on the initial probabilities (n x nc1)}
  \item{X2 }{desing matrix for the covariates on the transition probabilities (n x TT-1 x nc2)}
  \item{param }{type of parametrization for the transition probabilities ("multilogit" = standard multinomial logit for every row of the transition matrix, "difflogit" = multinomial logit based on the difference between two sets of parameters)}
  \item{Psi }{array of conditional response probabilities (mb x k x r)}
  \item{Be }{parameters affecting the logit for the initial probabilities}
  \item{Ga }{parametes affecting the logit for the transition probabilities}
  \item{fort}{to use fortran routine when possible (FALSE for not use fortran)}}

\value{
  \item{Y }{matrix of response configurations unit by unit (n x TT x r)}
  \item{U }{matrix containing the sequence of latent states (n x TT)}
 }

\author{Francesco Bartolucci, Silvia Pandolfi, University of Perugia (IT), http://www.stat.unipg.it/bartolucci}

\examples{
\dontrun{

# draw a sample for 1000 units, 10 response variable and 2 covariates
n <- 1000
TT <- 5
k <- 2
nc <- 2 #number of covariates
r <- 10 #number of response variables
mb <- 2 #maximum number of response categories
fort <- TRUE


Psi <- matrix(c(0.9,0.1,0.1,0.9), mb, k)
Psi <- array(Psi, c(mb, k, r))
Ga <- matrix(c(-log(0.9/0.1),0.5,1), (nc+1)*(k-1), k)
Be <- array(c(0,0.5,1), (nc+1)*(k-1))
#Simulate covariates
X1 <- matrix(0, n, nc)
for(j in 1:nc) X1[,j] <- rnorm(n)
X2 <- array(0,c(n, TT-1, nc))
for (t in 1:(TT-1)) for(j in 1:nc){
	if(t==1){
		X2[,t,j] <- 0.5*X1[,j] + rnorm(n)
	}else{
		X2[,t,j] <- 0.5 *X2[,t-1,j] + rnorm(n)
	}
}

out <- draw_lm_cov_latent(X1, X2, Psi = Psi, Be = Be, Ga = Ga, fort = fort)
}
}
