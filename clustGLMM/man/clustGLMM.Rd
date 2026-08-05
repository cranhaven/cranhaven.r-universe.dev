\name{clustGLMM}
\alias{clustGLMM}
\title{
Sample MCMC for a Mixture of GLMMs of Mixed-type
}
\description{
First, initial values are set based on separate univariate GLMMs for each individual outcome. Then, an inner \code{C} function is called to do the hard work of MCMC sampling (Metropolis within Gibbs sampling). Outputs are processed and returned in required format (either in nicely organized lists or one huge data.frame).
}
\usage{
clustGLMM(formula, id, family, data, G,  
          varying, save, param, tuning, inits,
          iter = 1000, nchains = 1, standardize = TRUE,
          howsave = c("data.frame", "list", "cmcmc"))
}
\arguments{
  \item{formula}{
a structured list containing formulae for all modelled outcomes. Let \code{y} be a modelled outcome then \code{formula[[y]]} should be a list containing
\describe{
\item{\code{$fixed}}{\code{formula} for effects common to all clusters,}
\item{\code{$group}}{\code{formula} for cluster-specific effects,}
\item{\code{$random}}{\code{formula} for \code{id}-specific effects,}
\item{\code{$offset}}{a column name from \code{data} to be used as an offset; by default it is set to an empty string, i.e., "", which means no offset.}
}}
  \item{id}{
a string, the name of the column containing the unit ID.
}
  \item{family}{
a vector of distributional families named by the outcome name in \code{data}; see below for implemented options.
}
  \item{data}{
a data.frame containing the outcomes, regressors, and the unit ID variable indicated in \code{id}. 
}
  \item{G}{
an integer, the number of mixture components (maximal number of clusters). 
Can be a vector of integers (of length \code{nchains}) to provide a different value for each chain.
}
  \item{varying}{
a named vector of Boolean values; declares which parameters should be group-specific (\code{TRUE}) with the following names: \code{"prec_num"}, \code{"c_ord"}, \code{"InvSigma"}, \code{"InvQ"}, \code{"naY"}.
}
  \item{save}{
a named vector of Boolean values; declares which model parameters should be returned (\code{TRUE}) and which should be dropped (\code{FALSE}). See also 'Details'.
}
  \item{param}{
a named list of hyperparameters for prior distributions. See also 'Details'.  
}
  \item{tuning}{
a list of integer valued and double-valued tuning parameters. See also 'Details'.
}
  \item{inits}{
a structured list of initial values; ideally the last generated values from a previous run. If \code{missing}, units are randomly assigned to components and parameters are initiated using values obtained based on separately estimated univariate models. See also 'Details'.
}
  \item{iter}{
length of each sampled chain.
}
  \item{nchains}{
number of chains to be sampled.
}
  \item{standardize}{
a Boolean whether the numeric outcome and regressors should be scaled.
}
  \item{howsave}{
a string declaring how the sampled chains should be returned as \code{draws}. One of:
\describe{
\item{\code{"list"}}{\code{draws} is a list indexed by chain numbers, parameters will be saved in a structured list in which it is easy to orient,}
\item{\code{"data.frame"}}{\code{draws} is a large data.frame, where one row corresponds to one iteration of one specific chain and columns are all model parameters,}
\item{\code{"cmcmc"}}{\code{draws} is a list indexed by chain numbers and contains the condensed output of the inner C function (not recommended!).}
}}
}
\details{
Column \code{id} in the \code{data} data.frame contains different IDs. For simplicity, these IDs are tied with numbers from 1 to total number of unique IDs. Then, unit-specific quantities are referred to by these integers instead of full IDs. 

Outcomes in \code{data} are expected to belong to one of the following families:
\describe{
  \item{numeric}{\code{"num"} or \code{"gaussian"}; double values without any restriction,}
  \item{count}{\code{"poi"} or \code{"poisson"}; integer values 0, 1, 2, \ldots,}
  \item{binary}{\code{"bin"} or \code{"bernoulli"}; factor of two levels, first will be baseline (0), other (1),}
  \item{ordinal}{\code{"ord"} or \code{"cumulative"}; (ordered) factor of at least 3 levels, if not apriori ordered, ordering will correspond to the one by \code{factor(, ordered = TRUE)},}
  \item{categorical}{\code{"cat"} or \code{"categorical"}; factor of at least 3 levels, first category will be baseline.}
}
Only these 5 types of outcome families are supported. The link functions cannot be changed.
\code{family} is internally relabeled to contain only \code{"num"}, \code{"poi"}, \code{"bin"}, \code{"ord"}, \code{"cat"} and reordered to follow this order of types. The same ordering of outcomes is applied to the covariance matrix \code{Sigma} of random effects.     

A named vector \code{varying} contains a Boolean for each model parameter that may be considered cluster-specific. The defaults are set by \code{\link{default_varying}}.

A named vector \code{save} contains a Boolean for each model parameter including some transformed parameters. For an overview on all model parameters including the default values used, see \code{\link{default_save}}.
 
A named list \code{param} contains the values of the hyperparameters needed to set up the priors. The default values are set up by \code{\link{default_param}}. By adjusting the values of \code{ae} and \code{be} different levels of sparsity can be achieved. The lower \code{ae/be} is, the sparser the results expected.  

The list of initial values \code{inits} is convenient to supply in case one is intereseted in continuing the sampling using the last sampled values as initialization, e.g., if the chains have not yet converged. If \code{inits} is missing, then a completely new set of initial values for each of the sampled chains is generated in the following way:

1. Units are randomly (uniformly) distributed to \code{G} components and \code{w} is initiated with \code{1/G} for all components.

2. For each numeric outcome, a regression model with combined \code{fixed} and \code{group} formula is fit to set up initial values for fixed parameters \code{beta_num_fixed}. Then, the model is fitted  again (if possible) for each component separately to determine the initial values for group-specific effects \code{beta_num}. Depending on the choice of group-specificity for \code{prec_num} the initial values are chosen as the inverse of the error variance estimate of the corresponding model. 

3. Effects for count outcomes are initialized in a similar way using \code{glm(, family = poisson(link = "log"))} instead. 

4. Effects for binary outcomes are initialized in a similar way using \code{glm(, family = binomial(link = "logit"))} instead. 

5. Effects and ordered intercepts for ordinal outcomes are initialized in a similar way using \code{polr(, Hess = TRUE)} from \pkg{MASS} instead. 

6. Effects for general categorical outcomes are initialized in a similar way using \code{multinom()} from \pkg{nnet} instead.

7. Matrix \code{InvSigma} is initialized by diagonal matrices with gamma distributed (\code{rate = 1/init_sd_b^2}) diagonal. 

8. Matrix \code{InvQ} is initialized as inverse of (averaged and scaled) \code{InvSigma}.

9. Random effects are sampled randomly from a centered normal distribution with \code{sd = param$init_sd_b}.

The algorithm also requires a set of tuning parameters in \code{tuning} that tune the sampling. For clarity, the parameters are split into two sublists depending on the expected type (\code{integer} or \code{double}). If \code{tuning} is missing, then the default values are set by \code{\link{default_tuning}}.
}
\value{
 An object of class \code{"clustglmm"} where the most important elements contained are:
\item{draws}{a list indexed by chain with generated samples saved either in a large data.frame (\code{howsave = "data.frame"}) or a carefully structured list (\code{howsave = "list"}).}
\item{param_names}{individual parameter (column) names in a structured list by general parameter name.}

In addition the object contains the inputs given to the function: \code{iter}, \code{nchains}, \code{family} (sorted by type), \code{formula}, \code{G} (a vector of Gs for all chains), \code{varying}, \code{save}, \code{howsave}, \code{param}, \code{tuning}, \code{standardize}.

Furthermore elements consisting of newly defined quantities are:
\item{iterations}{a list per each chain containing iterations available,}
\item{modeGplus}{a vector of the most frequent number of non-empty components per chain,}
\item{sameclusters}{a Boolean declaring whether the cluster labels in all sampled chains are the same (useful for plotting),}
\item{clusters}{a list of ordered non-empty components for each sampled chain (\code{modeGplus} most frequently appearing cluster labels),}
\item{clustering}{a matrix containing estimated cluster labels based on all sampled allocation indicators for each unit (row) and sampled chain (column),}
\item{certainty}{a matrix containing the probability of assignment into   the cluster specified in \code{clustering} based on all sampled allocation indicators for each unit (row) and sampled chain (column),}
\item{inits}{a structured list of parameter values used as initial values,}
\item{last}{a structured list in the same format as \code{inits} to be used as initial values for additional sampling,}
\item{InitType}{string describing whether the initial values were given or created,} 
\item{call}{summary of what has been fitted,}
\item{iter}{length of each sampled chain,}
\item{n}{total number of units (unique IDs),}
\item{numbered_unique_ids}{numbers \code{1:n} named by unique values from column \code{id},}
\item{Nums}{names of numerical outcomes,}
\item{Pois}{names of count outcomes,}
\item{Bins}{names of binary outcomes,}
\item{Ords}{names of ordinal outcomes,}
\item{Cats}{names of categorical outcomes,}
\item{nfix}{a vector named by outcomes that declares the dimension size of fixed effects common to all clusters,}
\item{ngrp}{a vector named by outcomes that declares the dimension size of group-specific effects,}
\item{nran}{a vector named by outcomes that declares the dimension size of random effects specific to each unit,}
\item{noff}{a vector named by outcomes that declares whether an offset is included in formula,}
\item{totnran}{total dimension of random effects, \code{totnran = sum(nran)},}
\item{lfixnames}{a list of names of fixed effects common to all clusters for each modelled outcome,}
\item{lgrpnames}{a list of names of group-specific effects for each modelled outcome,}
\item{lrannames}{a list of names of random effects specific to each unit for each modelled outcome,}
\item{loffnames}{a list of names of offset variables for each modelled outcomes,}
\item{fixnames}{unlisted \code{lfixnames},}
\item{grpnames}{unlisted \code{lgrpnames},}
\item{rannames}{unlisted \code{lrannames},}
\item{offnames}{unlisted \code{loffnames},}
\item{nY}{number of outcomes per type,}
\item{Kord}{number of categories for each ordinal outcome,}
\item{Kcat}{number of categories for each general categorical outcome,}
\item{settings}{a list (for each chain) of matrices where each row belongs to one model parameter and describes its properties such as: 1. is it saved?, 2. is it group-specific?, 3. is any dimension size dependent on outcome?, 4. on which type of outcome it depends?, 5. is it point, vector or matrix? 5. what are the dimension sizes?, 6. is the matrix symmetric?, 7. does the matrix contain diagonal?, 8. what is the total size of the parameter (including group-specificity)?; very useful matrix for organizing the samples and writing automated plotting functions, }
\item{yspecd1}{a list of model parameters, which have their first dimension size dependent on outcomes and how large it is, e,g, fixed-effects \code{beta_num_fix} are vectors that can be of different size among outcomes under different formulae,}
\item{yspecd2}{a list of matrix-valued model parameters, which have their second dimension size dependent on outcomes and how large it is, e.g. fixed-effects \code{beta_cat_fix} for categorical outcome \code{y in Cats} are matrices (\code{Kcat[y]} * \code{nfix[y]}) with both dimensions dependent on the outcome \code{y},}
\item{isYna}{a vector of Booleans that determine which outcome values are not observed, outcomes are ordered by \code{Ys = c(Nums, Pois, Bins, Ords, Cats)}.}
\item{centers}{a named vector of centers of numeric outcomes and regressors.}
\item{scales}{a named vector of scales of numeric outcomes and regressors.}
}
\references{
Vávra, J., Komárek, A., Grün, B., Malsiner-Walli, G. Clusterwise multivariate regression of mixed-type panel data. Stat Comput 34, 46 (2024). https://doi.org/10.1007/s11222-023-10304-5
}
\author{
Jan Vávra
}
\seealso{
\code{\link{generate_longitudinal_mixed_type_data}}, \code{\link{plot.clustglmm}}, \code{\link{default_varying}}, \code{\link{default_save}}, \code{\link{default_param}}, \code{\link{default_tuning}}
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
}
