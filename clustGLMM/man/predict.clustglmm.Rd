\name{predict.clustglmm}
\alias{predict.clustglmm}
\title{
Predict Method for Class \code{clustglmm} 
}
\description{
Predicts the linear combinations of fixed and group part of the regression of \code{clustGLMM} output. 
Transforms linear predictors to response level. 
Explores the posterior predictive distribution of the response.
}
\usage{
\method{predict}{clustglmm}(object, newdata, y = names(object$family)[1],
    type = c("link", "response", "posterior_predictive_response"), 
    method = c("samples", "plugin"),
    what = c("fg", "f", "g"), 
    posterior_predictive_by_cluster=TRUE,
    fun_posterior = mean, level = 0.95, interval = c("ET", "HPD"), 
    chain = 1, burnin = 0, thin = 1,
    gs = object$clusters[[chain]], ...)
}
\arguments{
  \item{object}{
    an object of class \code{"clustglmm"}, an output of \code{clustGLMM} function.
}
  \item{newdata}{
    a \code{data.frame} containing all columns needed to construct model matrices.
}
  \item{y}{
    a string; outcome to be predicted.
}
  \item{type}{
    what should be predicted; one of \code{"link"} (default), \code{"response"} or
    \code{"posterior_predictive_response"}, see Details below.
}
  \item{method}{
    the method of predicting; one of \code{"samples"} (default) or \code{"plugin"}, see Details below.
}
  \item{what}{
    more detailed clarification to the selected \code{type = "link"}.
}
  \item{posterior_predictive_by_cluster}{
    should under the posterior predictive distribution for response be explored for each cluster separately?
}
  \item{fun_posterior}{
    a function summarizing samples into an estimator of location parameter of posterior distribution; e.g. \code{mean} or \code{median}.
}
  \item{level}{
    the credibility level of credible intervals; 0.95 by default.
}
  \item{interval}{
    the method of constructing credible intervals; one of \code{"ET"} (equal-tailed by default) or \code{"HPD"} (highest-posterior density), see Details below.
}
  \item{chain}{
    chain number to be used to do the prediction, the default is the first \code{chain = 1}.
}
  \item{burnin}{
    the length of burn-in period (default 0), i.e., number of initial samples to ignore for inference.
}
  \item{thin}{
    thinning rate (default 1); only every \code{thin}th value is used for inference (must be a positive integer). 
}
  \item{gs}{
    cluster labels, for which prediction of cluster-specific should be made; all non-empty clusters by default.
}
  \item{...}{
    other arguments.
}
}
\details{
Depending on \code{type} the following is computed:
\describe{
  \item{link}{reconstructs the linear predictor for given outcome
    \code{y}. You can specify by \code{what} which part
    of the predictor should be reconstructed: 
    "f", "g", "fg", which part
    of the predictor should be reconstructed: the fixed part invariant
    to all clusters, the group-specific part for chosen clusters
    \code{gs} or sum of both (default).}
  \item{response}{linear predictor "fg" transformed depending on the outcome type. 
  Not transformed for \code{num}.
  Transformed by \code{exp} for \code{poi}.
  Category probabilities for \code{bin}, \code{ord} and \code{cat}.}
  \item{posterior_predictive_response}{explores the posterior predictive distribution.
  Depending on \code{posterior_predictive_by_cluster}, 
  either for each cluster separately or you learn a mixture over given clusters \code{gs}.
  For each iteration in \code{object$draws[[chain]]}, 
  samples latent parameters (allocation and random effects) and then an outcome value.
  It is important to know that only clusters in \code{gs} are considered here, other clusters are ignored. 
  }
}

Argument \code{what} only specifies option \code{type="link"}:
\describe{
  \item{f}{the fixed part of linear predictor invariant to all clusters,}
  \item{g}{the group-specific part of linear predictor for chosen clusters \code{gs},}
  \item{fg}{sum of the two above.}
}

As we approximate the posterior distribution with samples, there may be
several ways on obtaining the posterior estimate of desired quantity
(\code{type}). Depending on \code{method} we proceed in the following
way:
\describe{
  \item{samples}{treat the desired quantity as a parametric function,
    evaluate it with all (closely specified) samples in
    \code{object$draws} and estimate it by \code{fun_posterior}. This
    option allows for construction of credible intervals.}
  \item{plugin}{estimate the model parameters with \code{fun_posterior}
    first and then plug them into the function defining the
    quantity. Does not provide credible intervals and \code{type="posterior_predictive_response"}.}
}

The computation of credible intervals under \code{method="samples"} is given by \code{interval}:
\describe{
  \item{ET}{equal-tailed credible interval determined out of samples
    from posterior distribution with \code{quantile} function with
    \code{probs = c((1-level)/2, 1 - (1-level)/2}.}
  \item{HPD}{highest-posterior density credible interval determined out
    of samples from posterior distribution with \code{\link[HDInterval]{hdi}}
    function from \pkg{HDInterval}.}
}
For \code{method="plugin"} the credible intervals are not implemented.
}
\value{
For given outcome \code{y} returns a list with
\describe{
  \item{$fit}{the estimate of the posterior location obtained via function \code{fun_posterior} over samples,}
  \item{$lwr}{the lower bound of credible interval, if \code{method="samples"},}
  \item{$upr}{the upper bound of credible interval, if \code{method="samples"}.}
}

Moreover, \code{type="posterior_predictive_response"} provides
\describe{
  \item{$samples}{the non-aggregated samples of the response,}
  \item{$aux}{the auxiliary quantities used to each generate outcome. 
  Linear predictor for numeric, 
  exponential of linear predictor for count,
  probabilities for binary, ordinal and categorical outcomes.}
}

The output is an array of up to 4 dimensions (dropped if only size 1) in this exact order:
\item{1.}{the size of \code{newdata},}
\item{2.}{the number of clusters \code{length(gs)}, unless just 1 (then dropped) if not group-specific (\code{what="f"}),}
\item{3.}{the dimension of samples, if not reduced by \code{fun_posterior} (only \code{$samples} and \code{$aux}).}
\item{4.}{the number of different outcome levels: 1 for numeric and count (then dropped), K (probabilities) or K-1 (predictors) for categorical outcome of K levels}
}
\references{
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
###     predict.clustglmm()
###-----------------------------------------------------------------------------

# By default, predicts the first outcome in mcmc$family
predict(mcmc, newdata = subset(longitudinal_mixed_type_data, i \%in\% 1:3), type = "link")
# result is list of fit, lwr, upr matrices

# Predict the evolution in time for covariate f=0
newdata <- data.frame("x" = seq(0, 1, length.out=100), "f"=factor(0, levels=c(0,1)))

## Numeric outcome
# both fixed and group-specific part of the predictor
p1 <- predict(mcmc, newdata, y="ynum", type="link", what="fg")
# posterior predictive distribution of the response
p2 <- predict(mcmc, newdata, y="ynum", type="posterior_predictive_response")

matplot(newdata$x, p1$fit, type = "l", lty = 1, lwd = 2)
matplot(newdata$x, p1$lwr, type = "l", lty = 2, add = TRUE)
matplot(newdata$x, p1$upr, type = "l", lty = 2, add = TRUE)
matplot(newdata$x, p2$lwr, type = "l", lty = 3, add = TRUE) # needs longer chains
matplot(newdata$x, p2$upr, type = "l", lty = 3, add = TRUE) # needs longer chains

## Count outcome - conditional mean 
# predictor
pl <- predict(mcmc, newdata, y="ypoi", type="link")
# conditional mean
p1 <- predict(mcmc, newdata, y="ypoi", type="response")
# posterior predictive distribution
p2 <- predict(mcmc, newdata, y="ypoi", type="posterior_predictive_response")

matplot(newdata$x, p1$fit, type = "l", lty = 1, lwd = 2)
matplot(newdata$x, p1$lwr, type = "l", lty = 2, add = TRUE)
matplot(newdata$x, p1$upr, type = "l", lty = 2, add = TRUE)
matplot(newdata$x, p2$lwr, type = "l", lty = 3, add = TRUE) # needs longer chains
matplot(newdata$x, p2$upr, type = "l", lty = 3, add = TRUE) # needs longer chains

## Categorical outcome - list of fit, lwr, upr 3D arrays (category dimension added)
predict(mcmc, newdata[1:5,], y="ycat", type="link", what="fg")

}
