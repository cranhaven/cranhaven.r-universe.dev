\name{as_coda}
\alias{as_coda}
\alias{from_C_to_list}
\alias{from_C_to_matrix}
\alias{from_list_to_C}
\alias{from_list_to_coda}
\alias{from_list_to_matrix}
\alias{from_matrix_to_C}
\alias{from_matrix_to_coda}
\alias{from_matrix_to_list}
\title{
Change the Output Formats of \code{clustGLMM}
}
\description{
These functions are used to process or rearrange data between different formats. The main function \code{\link{as_coda}} converts the \code{draws} item of the \code{clustglmm} class into a \code{\link[coda]{mcmc.list}} object compatible with functions from package \pkg{coda}. Other functions (usually internally used) allow us to change between different saving formats (\code{mcnc$howsave = "list"} or \code{mcmc$howsave = "data.frame"}).
}
\usage{
as_coda(mcmc, burnin=0, thin=1)
from_C_to_list(values, p, settings, yspecd1=NULL, yspecd2=NULL, d2spec = NULL, family)
from_C_to_matrix(values, p, settings, yspecd1=NULL, yspecd2=NULL, d2spec = NULL, family)
from_list_to_C(draws, iterations, p, settings, yspecd1=NULL, yspecd2=NULL,
               d2spec = NULL, family)
from_list_to_coda(mcmc, burnin=0, thin=1)
from_list_to_matrix(mcmc)
from_matrix_to_C(draws, iterations, p, settings)
from_matrix_to_coda(mcmc, burnin=0, thin=1)
from_matrix_to_list(mcmc)
}
\arguments{
  \item{mcmc}{
an object of class \code{clustglmm}.
}
  \item{values}{
a vector of values to be stored.
}
  \item{p}{
a string containing the name of the parameter. 
}
  \item{settings}{
a matrix containing the information about model parameters.
}
  \item{yspecd1}{
a list of outcome-specific sizes along the first dimension.
}
  \item{yspecd2}{
a list of outcome-specific sizes along the second dimension.
}
  \item{d2spec}{
a numeric vector containing the dimension sizes if the parameter has different second dimension for every element of the first dimension (applicable only for dynamic clustering probabilities). 
}  
  \item{family}{
a vector of distributional families named by the outcome name in \code{data}.
}
  \item{iterations}{
what iterations from a given chain to include.
}
  \item{draws}{
either a list or a matrix; e.g., an element of \code{clustGLMM$draws} for a certain chain.
}
  \item{burnin}{
the length of burn-in period (default 0), i.e., the number of initial samples to omit.
}
  \item{thin}{
thinning rate (default 1); only every \code{thin}th value is kept (must be a positive integer). 
}
}
\details{
Functions with \code{C} in the name are used internally in \code{\link{clustGLMM}} or \code{\link{clustering_probabilities_and_deviance}} to process data coming from C functions. 
These functions only convert only draws from specific chain for a specific parameter, which explains the arguments \code{values}, \code{p} or \code{draws}.
They do not convert the whole \code{clustglmm} object.

The functions that change between \code{howsave = "list"}, \code{howsave = "data.frame"} and \code{coda} output go through every chain and every saved parameter, thus, convert the whole \code{clustglmm} object.

Function \code{as_coda} is a wrapper function that triggers the respective function \code{from_..._to_coda} depending on the current value of \code{mcmc$howsave}.
}
\value{

For functions \code{...to_list}, the output is a carefully structured list. 
Each chain \code{ch in 1:mcmc$chains} is saved in \code{mcmc$draws[[ch]]}. 
Then one can select a specific parameter which may have even other sublists. 
For example, group-specific effects for a numeric outcome would be accessed by \code{mcmc$draws[[ch]]$beta_num[[g]][[mcmc$Nums[1]]]} where \code{g} is the group.  

For functions \code{...to_matrix}, the output is a separate list for each chain. 
Each element of this list is a matrix with carefully named columns forming a \code{data.frame}. 
The first column is \code{m} and contains the iteration number to which the row belongs to. 
Other columns are model parameters. 
For example, the first effect specific for group 2 for numeric outcome called "ynum" would be named \code{"beta_num_ynum(2)[1]"}. 
Note that the name of the variable has been pasted after the parameter name, then follows a group number in parentheses (only if parameter group-specific) and it ends with the dimensional coordinates in square brackets.

For functions \code{...to_C}, the output is a long vector where all the parameter values are stacked together in the order assumed in the C functions. 

For functions \code{...to_coda}, the output is a \code{"mcmc.list"} object from \pkg{coda}. 
Then you can use this class and related methods for monitoring, investigating, plotting. 
However, a minor warning: the number of parameters is quite huge and can get overwhelming. 
Therefore, a set of plotting functions designed for the different parameter blocks is provided with this package, see \code{\link{plot.clustglmm}}.
}
\references{
Vávra, J., Komárek, A., Grün, B., Malsiner-Walli, G. Clusterwise multivariate regression of mixed-type panel data. Stat Comput 34, 46 (2024). https://doi.org/10.1007/s11222-023-10304-5
}
\author{
Jan Vávra
}
\seealso{
\code{\link{clustGLMM}}, \code{\link{plot.clustglmm}}, \code{\link{clustering_probabilities_and_deviance}}
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
formula
# intercept term will be group-specific 
# since it appears in $group which has a preference over $fixed

## All other inputs need to be prepared,
## unless the default values are desired (varying, save, howsave, param, tuning).
## See the vignette for how these can be adjusted.

###-----------------------------------------------------------------------------
###     Calling clustGLMM()
###-----------------------------------------------------------------------------

Gmax <- 5     # maximal number of mixture components to be considered
nchains <- 2  # number of chains to be sampled 

## No inits specified
set.seed(123456789)
mcmc <- clustGLMM(formula = formula, id = id, family = family, 
                  data = longitudinal_mixed_type_data, 
                  G = Gmax, iter = 100, nchains = nchains)
                       
###-----------------------------------------------------------------------------
###     Changing formats
###-----------------------------------------------------------------------------

## How to change from "data.frame"="matrix" to "list":
mcmc <- from_matrix_to_list(mcmc)

## How to change from "list" to "data.frame"="matrix":
mcmc <- from_list_to_matrix(mcmc)

## How to change from "data.frame"="matrix" to mcmc from 'coda' package
codamcmc <- from_matrix_to_coda(mcmc, burnin = 10, thin = 1)

## How to change from any howsave type to mcmc from 'coda' package
codamcmc <- as_coda(mcmc, burnin = 10, thin = 1)
}
