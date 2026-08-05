\name{clustering_probabilities_and_deviance}
\alias{clustering_probabilities_and_deviance}
\title{
Evaluate Clustering Probabilities and Deviance of Output of \code{clustGLMM}
}
\description{
To evaluate the clustering probabilities, all latent variables need to be integrated out. No analytic closed form solution is available for the integral in case of joint random effects for mixed-type outcomes. Numerical approximations are obtained using Laplace's approximation or Adaptive Gaussian Quadrature (AGQ). The samples of the model parameters from \code{\link{clustGLMM}} are sent to C for faster evaluation.
}
\usage{
clustering_probabilities_and_deviance(mcmc, id, data, dynamic_prob = FALSE, 
  start = 1, end = mcmc$iter, thin = 1, chains = mcmc$chains,
  howsave = mcmc$howsave,                                      
  tolerance = mcmc$tuning$double$tolerance, 
  maxiter = mcmc$tuning$integer$maxiter, 
  maxnrep = mcmc$tuning$integer$maxnrep, 
  NGQ = 1)
}
\arguments{
  \item{mcmc}{
an object of class \code{"clustglmm"} (as returned by function \code{\link{clustGLMM}}).
}
  \item{id}{
a string, name of the column containing the unit ID. New units not used in MCMC sampling may be used here.
}
  \item{data}{
a data.frame containing outcomes and regressors including the unit ID variable \code{id}.
}
  \item{dynamic_prob}{
a Boolean; indicates if the clustering probabilities are computed dynamically. That is, if unit \code{i} is observed \code{ni} times, then \code{ni} different probabilities are computed, each based on the first \code{j} observations for \code{j in 1:ni}.
}
  \item{start}{
an iteration number where to start.
}
  \item{end}{
an iteration number where to end.
}
  \item{thin}{
a thinning that should be used between \code{start} and \code{end}; e.g., if \code{thin = 10} only every 10th iteration is approximated; default is 1. 
}
  \item{chains}{
chain number(s) for which the approximation should be computed. 
}
  \item{howsave}{
a string declaring how the sampled chains should be returned as \code{draws}. One of:
\describe{
\item{\code{"list"}}{\code{draws} is a list indexed by chain numbers, parameters will be saved in a structured list in which it is easy to orient,}
\item{\code{"matrix"}}{\code{draws} is a large matrix, where one row corresponds to one iteration of one specific chain and columns are all model parameters,}
\item{\code{"cmcmc"}}{\code{draws} is a list indexed by chain numbers and contains the condensed output of the inner C function (not recommended!).}
}}
  \item{tolerance}{
tolerance used when computing the norm of the shift within Newton-Raphson.
}
  \item{maxiter}{
maximum number of iterations allowed during Newton-Raphson.
}
  \item{maxnrep}{
maximum number of repetitions of Newton-Raphson from different starting points.
}
  \item{NGQ}{
number of points for Adaptive Gaussian Quadrature; if \code{NGQ = 1}, then simple Laplace's approximation is used. Do not use values higher than 7, since there are \code{NGQ^totnran} summands with \code{totnran} denoting the dimension of the random effects.
}
}
\details{
An object of class \code{"clustglmm"} can be used regardless of the format, i.e.,  \code{howsave = "list"} or \code{howsave = "matrix"}.

The data.frame \code{data} containing the outcomes and regressors does not have to be the same as used for MCMC sampling. Even completely new units are allowed here.  

The indexing of probabilities is more complicated when \code{dynamic_prob = TRUE}. In this case, for one unit there will be as many probabilities as observations belong to this unit. If unit \code{i} is observed 3 times, there will be 3 probabilities: one where only the first observation is used, a second where the first two observations are used and a third where all observations are used. With more available data the classification towards one of the clusters should be more pronounced. In the end, there will be as many probabilities as rows of \code{data}. However, it does not mean that the rows correspond to each other, e.g., when \code{data} is not ordered by unit IDs. The reason is that we use a \code{for} cycle that goes through all units and then for a specific unit through all its observations. 

The code can handle the case where \code{data} is not sorted by ID. However, for one specific unit the data are assumed to be ordered in time. This is internally used as ordering for the dynamic probability calculation. However, we recommend to insert data also sorted by ID so that pairing rows with the probabilities obtained with \code{dynamic_prob = TRUE} is easier. 

Computations are usually expensive and a division of the workload into several jobs where different chains or even subsamples are processed simultaneously might be useful. This can be achieved by setting \code{start}, \code{end} and \code{thin} differently for different jobs. However, you need to divide this on your own als also gather the results back together. To do this properly keep in mind that the used iteration numbers are created by \code{seq(start, end, by = thin)}.

To approximate the \code{totnran}-dimensional integral over the random effects, we use Adaptive Gaussian Quadrature. First, the Newton-Raphson method is used to find the Taylor expansion of the log-pdf as a function of random effects. If \code{NGQ = 1} then the integral is approximated by evaluation only in the central point found by Newton-Raphson, which corresponds to simple Laplace's approximation. If \code{NGQ = 2, 3, ..., 7} then \code{hermite.he.quadrature.rules(n = NGQ, normalized = FALSE)} from package \pkg{gaussquad} is used to find the roots and weights of the Hermite quadrature rules. These are combined for every dimension of random effects, which yields \code{NGQ^totnran} points at which the log-pdf needs to be evaluated. So, think carefully whether higher approximation precision with high \code{NGQ} is worth the expensive evaluation.
}
\value{
  An object of class \code{"clustglmm"} is returned so that already existing methods are applicable. Most of the outputs are taken from the given \code{mcmc} object.
  
  Depending on the choice of \code{howsave = "list"} or \code{howsave = "matrix"}, the \code{$draws} element contains a list of outputs for different chains (from \code{chains}) organized in either \code{list} or \code{matrix} format. Each chain then contains the following quantities:
  \describe{
    \item{\code{dev_i}}{a vector of individual unit contribution to the model deviance,}
    \item{\code{deviance}}{a scalar containing the model deviance (sum of all individual contributions),}
    \item{\code{pUig_int}}{a group-specific vector of length equal to number of unique \code{id} labels containing clustering probabilities towards one of \code{mcmc$G} clusters. The mapping between ids and numbers is given via the item \code{$numbered_unique_ids}.}
  }
  
  Outputs \code{$clustering} and \code{$draws} are derived from \code{pUig_int} by averaging and evaluating, which cluster has the highest posterior probability.
}
\references{
Pinheiro, J.C., Chao, E.C. Efficient Laplacian and adaptive Gaussian quadrature algorithms for multilevel generalized linear mixed models. J. Comput. Graph. Stat. 15(1), 58–81 (2006). https://doi.org/10.1198/106186006x96962

Vávra, J., Komárek, A., Grün, B., Malsiner-Walli, G. Clusterwise multivariate regression of mixed-type panel data. Stat Comput 34, 46 (2024). https://doi.org/10.1007/s11222-023-10304-5
}
\author{
Jan Vávra
}
\seealso{
\code{\link{clustGLMM}}
}
\examples{

### Get some longitudinal mixed type data
### here generated by generate_longitudinal_mixed_type_data()
data("longitudinal_mixed_type_data")

# Examples here demonstrate the basic use of the available functions on a toy dataset.
# For more thorough analyses see the vignettes.


###-----------------------------------------------------------------------------
###     Prepare inputs for clustGLMM()
###-----------------------------------------------------------------------------
id <- "i"

family <- c("num", "poi", "bin", "ord", "cat")
names(family) <- Ys <- c("ynum", "ypoi", "ybin", "yord", "ycat")

## Create list of formulae for each outcome
formula <- list()
for(y in Ys){
  formula[[y]] <- list()
  formula[[y]]$fixed <- ~ f
  formula[[y]]$group <- ~ x
  formula[[y]]$random <- ~ 1
  formula[[y]]$offset <- ""
}
# intercept term will be group-specific 
# since it appears in $group which has a preference over $fixed

## All other inputs need to be prepared,
## unless the default values are desired (varying, save, howsave, param, tuning).
## See the vignette for how these can be adjusted.

###-----------------------------------------------------------------------------
###     Calling clustGLMM()
###-----------------------------------------------------------------------------

Gmax <- 5     # maximal number of mixture components to be considered
nchains <- 1  # number of chains to be sampled 

## No inits specified
set.seed(123456789)
mcmc <- clustGLMM(formula = formula, id = id, family = family, 
                  data = longitudinal_mixed_type_data, 
                  G = Gmax, iter = 100, nchains = nchains)
                  
## Extract the last state
inits <- mcmc$last

## Initialize with inits = Continue in sampling from the last state
mcmc <- clustGLMM(formula = formula, id = id, family = family, 
                  data = longitudinal_mixed_type_data,
                  inits = inits, 
                  G = Gmax, iter = 100, nchains = nchains)
                       
## Note that sampling takes time, here we generate too short chains.
## Sample longer chains for more faithful posterior approximation.
## Parallelization for different chains is recommended.
## See the vignette for more examples. 

###-----------------------------------------------------------------------------
###     Sample completely new data
###-----------------------------------------------------------------------------
nnew <- 100
n_i <- 4       
G <- 2         
timepar <- "mix" 
catpar <- "no"  
catsubjfix <- TRUE 
xin1 <- 1        
Kord <- 5        
Kcat <- 4        
kspec_bi_cat <- FALSE
corr <- matrix(c(1, -0.5, -0.5, -0.4, -0.4,
                 -0.5, 1, 0.3, 0.4, 0.5,
                 -0.5, 0.3, 1, 0.2, 0.3,
                 -0.4, 0.4, 0.2, 1, 0.1,
                 -0.4, 0.5, 0.3, 0.1, 1), byrow = TRUE, ncol = 5)
sdSigmas <- c(0.5, 0.5, 0.5, 0.5, 0.5)
Sigma <- diag(sdSigmas) %*% corr %*% diag(sdSigmas)

set.seed(27818)
ldatanew <- generate_longitudinal_mixed_type_data(n = nnew, n_i = 4,
                     G = 2, timepar = "cross", catpar = "fixed", 
                     catsubjfix = TRUE,
                     xin1 = 1, Sigma = Sigma,
                     Kord = 5, Kcat = 4, kspec_bi_cat = FALSE)
newdata <- ldatanew$data

###-----------------------------------------------------------------------------
###     Evaluate the clustering probabilities for newdata
###-----------------------------------------------------------------------------

probs <- clustering_probabilities_and_deviance(mcmc, id, newdata,
                                               dynamic_prob = FALSE,
                                               start = 1,
                                               end = 100,
                                               thin = 2,
                                               chains = 1:nchains,
                                               tolerance = 1e-7, 
                                               maxiter = 100,
                                               maxnrep = 20, 
                                               NGQ = 1)

###-----------------------------------------------------------------------------
###     Final clustering
###-----------------------------------------------------------------------------
certainty <- probs$certainty
clustering <- probs$clustering
# To be classified in the cluster with maximal clustering probability,
# it has to overcome the given threshold, otherwise remains unclassified.
threshold <- 0.8
clustering[certainty < threshold] <- 0

ch <- 1
newdata$clustering <- clustering[,ch]

newdata1 <- newdata[newdata$j==1,]
table(newdata1$clustering, newdata1$g)
# row 0: unclassified

}

