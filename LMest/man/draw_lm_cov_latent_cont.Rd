\name{draw_lm_cov_latent_cont}

\alias{draw_lm_cov_latent_cont}

\title{Draw samples from LM model for continuous outcomes with covariaates in the latent model}

\description{Function that draws samples from the LM model for continuous outcomes with individual covariates with specific parameters.  \cr \cr \bold{The function is no longer maintained. Please look at} \code{\link{draw.LMlatentcont}} \bold{function}.}

\usage{
draw_lm_cov_latent_cont(X1, X2, param = "multilogit", Mu, Si, Be, Ga, fort = TRUE)
}

\arguments{
  \item{X1 }{desing matrix for the covariates on the initial probabilities (n x nc1)}
  \item{X2 }{desing matrix for the covariates on the transition probabilities (n x TT-1 x nc2)}
  \item{param }{type of parametrization for the transition probabilities ("multilogit" = standard multinomial logit for every row of the transition matrix, "difflogit" = multinomial logit based on the difference between two sets of parameters)}
  \item{Mu }{array of conditional means for the response variables (r x k)}
  \item{Si }{var-cov matrix common to all states (r x r)}
  \item{Be }{parameters affecting the logit for the initial probabilities}
  \item{Ga }{parametes affecting the logit for the transition probabilities}
  \item{fort}{to use fortran routine when possible (FALSE for not use fortran)}}

\value{
  \item{Y }{array of continuous outcomes (n x TT x r)}
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
r <- 3 #number of response variables
fort <- TRUE

Mu <- matrix(c(-2,-2,0,0,2,2), r, k)
Si <- diag(r)
Ga <- matrix(c(-log(0.9/0.1),0.5,1), (nc+1)*(k-1), k)
Be <- array(c(0,0.5,1), (nc+1)*(k-1))

#Simulate covariates
X1 <- matrix(0, n, nc)
for(j in 1:nc) X1[,j] <- rnorm(n)
X2 <- array(0, c(n,TT-1,nc))
for (t in 1:(TT-1)) for(j in 1:nc){
	if(t==1){
		X2[,t,j] <- 0.5*X1[,j] + rnorm(n)
	}else{
		X2[,t,j] <- 0.5*X2[,t-1,j] + rnorm(n)
	}
}

out <- draw_lm_cov_latent_cont(X1, X2, param = "multilogit", Mu, Si, Be, Ga, fort = fort)
}
}
