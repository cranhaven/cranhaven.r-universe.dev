\name{plot_diagnostics}
\alias{plot_diagnostics}
\alias{plot_ACF}
\alias{plot_ECDF}
\alias{plot_kerneldensity}
\alias{plot_traceplots}
\alias{plot_ng_trace_chain_split}
\alias{plot_ACF_param}
\alias{plot_ECDF_param}
\alias{plot_kerneldensity_param}
\alias{plot_traceplots_param}
\alias{plot_clusters}
\alias{plot_clusters_ECDF_param}
\alias{plot_clusters_kerneldensity_param}
\title{
Plot MCMC Samples from \code{clustGLMM}
}
\description{
There are 4 main plots to be created:
\describe{
  \item{\code{ACF}}{auto-correlation function,}
  \item{\code{ECDF}}{empirical cumulative distribution function,}
  \item{\code{kerneldensity}}{one-dimensional kernel density estimate,}
  \item{\code{traceplots}}{parameter samples against the iteration number.}
}
All of these for one specific parameter can be plotted by \code{plot_diagnostics}. 
These functions above work only for a univariate parameter. 
Plots for all elements of a multivariate parameter can be created using \code{..._param} functions. 
A special function is \code{plot_clusters} which allows to compare densities or ECDFs of univariate group-specific parameter across non-empty clusters.  
}
\usage{
plot_ACF(mcmc, scalar_samples, what, gspec, dimspec, yspec,
         burnin = 0, thin = 1, lag.max = 30, iterations, chains, 
         COL, move.width = 0.3, labcex = 1, whatLAB)
plot_ECDF(mcmc, scalar_samples, what, gspec, dimspec, yspec,
          burnin = 0, thin = 1, iterations, chains, 
          COL, labcex = 1, whatLAB)
plot_kerneldensity(mcmc, scalar_samples, what, gspec, dimspec, yspec, 
                   burnin = 0, thin = 1, iterations, chains, 
                   dolegend = FALSE, COL, labcex = 1, whatLAB)
plot_traceplots(mcmc, scalar_samples, what, gspec, dimspec, yspec,
                burnin = 0, thin = 1, iterations, chains, 
                COL, labcex = 1, whatLAB)
plot_ng_trace_chain_split(mcmc, 
                          burnin = 0, thin = 1, iterations, chains,
                          COL, labcex = 1, setparmfrow = TRUE)
plot_diagnostics(mcmc, scalar_samples, what, gspec, dimspec, yspec,
                 burnin = 0, thin = 1, lag.max = 30, iterations, chains,
                 trueval, setparmfrow = TRUE, COL, move.width = 0.3, whatLAB)
plot_ACF_param(mcmc, what = "ng", yspec, burnin = 0, thin = 1, iterations,
               lag.max = 30, chains, setparmfrow = TRUE, allclusters = FALSE)
plot_ECDF_param(mcmc, what, yspec, burnin = 0, thin = 1, iterations, chains, 
                setparmfrow = TRUE, allclusters = FALSE)
plot_kerneldensity_param(mcmc, what, yspec, burnin = 0, thin = 1, iterations,
                         chains, setparmfrow = TRUE, allclusters = FALSE)
plot_traceplots_param(mcmc, what, yspec, burnin = 0, thin = 1, iterations,
                      chains, setparmfrow = TRUE, allclusters = FALSE)
plot_clusters(mcmc, what, dimspec, yspec, whichg,
              burnin = 0, thin = 1, iterations, chains,
              setparmfrow = TRUE, doKern = TRUE, doECDF = TRUE,
              COL, labcex = 0.8, whatLAB, wherelegend = "topright")
plot_clusters_kerneldensity_param(mcmc, what, whichg, yspec, burnin = 0,
                                  thin = 1, iterations, chains,
                                  setparmfrow = TRUE)
plot_clusters_ECDF_param(mcmc, what, whichg, yspec, burnin = 0, thin = 1,
                         iterations, chains, setparmfrow = TRUE)
}
\arguments{
  \item{mcmc}{
an output of \code{\link{clustGLMM}} accepted in both formats (list or matrix).
}
  \item{scalar_samples}{
an output of \code{get_scalar_samples}, overrides any given \code{burnin} or \code{thin}.
}  
  \item{what}{
a name of the parameter to be extracted; see \code{names(mcmc$save)} for possible options. 
}
  \item{gspec}{
a group specifier (values in \code{1:mcmc$G}); if not group-specific parameter this \emph{should be missing}.
}
  \item{dimspec}{
a dimension specifier; not specifying this is possible only when the parameter is scalar.
}
  \item{yspec}{
an outcome specifier; if the parameter is not outcome-dependent then this \emph{should be missing}.
}
  \item{burnin}{
the length of burn-in period (default 0), i.e. number of initial samples to ignore for inference.
}
  \item{thin}{
thinning rate (default 1); only every \code{thin}th value is used for inference (must be a positive integer). 
}
  \item{lag.max}{
maximal lag value to be plotted in ACF.
}
  \item{iterations}{
a vector of iterations numbers to extract, overrides any given \code{burnin} or \code{thin}. 
}
  \item{chains}{
a vector of chain numbers to be plotted.
}
  \item{trueval}{
a true value of the parameter to be plotted by vertical/horizontal line.
}
  \item{setparmfrow}{
whether \code{par(mfrow = ..)} should be set automatically before creating all plots.
}
  \item{allclusters}{
whether for group-specific parameters plot all the clusters (\code{TRUE}) or just those in \code{mcmc$clusters} (\code{FALSE}).
}
  \item{COL}{
colors for different chains; default values are set by \code{diverge_hcl(mcmc$nchains, c = 80, l = 70)}.
}
  \item{move.width}{
ACF for different chains are jittered around the actual lag by \code{seq(-move.width, move.width, length.out = length(chains)+1)}
}
  \item{whatLAB}{
the label for the parameter; default corresponds to the label for that parameter that would be used in \emph{matrix} format of the output. 
}
  \item{labcex}{
\code{cex} value for label of plots. 
}
  \item{dolegend}{
a Boolean declaring whether legend should be added to the plot.  
}
  \item{whichg}{
a set of cluster labels to be compared in \code{plot_clusters}. By default \code{mcmc$clusters} is taken under consistency among selected chains. Can be a list in \code{plot_clusters_kerneldensity_param} and \code{plot_clusters_ECDF_param}.  
}
  \item{doKern}{
a Boolean declaring whether kernel density estimates should be plotted for comparing different clusters.
}
  \item{doECDF}{
a Boolean declaring whether ECDFs should be plotted for comparing different clusters.
}
  \item{wherelegend}{
  a string that declares where the legend should be placed.
}
}
\details{
  \code{scalar_samples} is a list (for each element of \code{chains}) containing \code{data.frame} of three columns: \code{chain} declaring the chain number, \code{m} declaring the iterations numbers and \code{y} containing chosen samples. 
  This could be created using \code{\link{get_scalar_samples}}, see its help file for more details. If missing, \code{\link{get_scalar_samples}} is called anyway with the given specifications.
  If \code{iterations} is missing, first, only iterations with \code{m > burnin} are selected, then, from the remaining values only \code{seq(1, total_remaining_count, by=thin)} are selected.
  Parameter is selected by filling/missing \code{what}, \code{gspec}, \code{yspec}, \code{dimspec}. More details here: \code{\link{get_scalar_samples}}.
  
  Description of the plots:
  \describe{
    \item{\code{ACF}}{Autocorrelation function plots have the lag on x-axis [0, \code{lag.max}] and auto-correlations in interval (-1, 1) on y-axis. By default, \code{lag.max = min(30, mcmc$iter-1)}. Different chains have different colors and are slightly jittered for better visuality, see \code{move.width}. }
    \item{\code{ECDF}}{Cumulative distribution function plots have the range of sampled values on x-axis and the cumulative probabilities in [0, 1] on y-axis. Different colors depict different chains of non-decreasing ECDFs.}
    \item{\code{kerneldensity}}{Kernel density estimator plots have the range of sampled values on x-axis and the density value in [0, max(density)] on y-axis. Different colors depict different chains.}
    \item{\code{traceplot}}{Traceplots have the range of the considered iteration numbers on x-axis and the sampled values of the parameter on y-axis. Different colors depict different chains. Stationarity of these plots is desired for reliable posterior approximation, if the plots are not stationary then consider continuation in sampling. }
  }
  You can plot all 4 of these by \code{plot_diagnostics}. 
  By default they are produced in 2 by 2 layout, but if you would prefer some different arrangement you can do that before calling the function and supressing \code{setparmfrow = FALSE}. 
  
  Using the functions above you can only plot a scalar model parameter which is specified by filling/missing \code{what}, \code{gspec}, \code{yspec}, \code{dimspec}. 
  If you would like to plot results for all parameters that are associated with parameter \code{what} then \code{..._param} functions can do this for you automatically. 
  By default, only cluster-specific parameters for the (non-empty) clusters saved in \code{mcmc$clusters} are arranged.
  If the labels across chains do not coincide, then chains are plotted separately.
  The exception is made for \code{w} and \code{ng} for which always all clusters are plotted. 
  Moreover, there is special function \code{plot_ng_trace_chain_split} for plotting traces of \code{ng} for all clusters into one plot per chain (default by \code{\link{plot.clustglmm}}). 
  Using function \code{\link{nice_nrow_ncol}} plots are arranged into a most effective grid. 
  Moreover, parameters which are symmetric matrices (with or without diagonal) are arranged as expected and only the upper triangle is filled with plots. 
  See examples below.
  
  Special function in this collection is \code{plot_clusters} which allows to plot kernel density estimates or empirical cumulative distribution function of the same parameters for different cluster labels (\code{whichg}) into one plot. 
  There are also \code{plot_clusters_kerneldensity_param} and \code{plot_clusters_ECDF_param} functions that create the plots automatically for a set of given parameter \code{what}, however, the plotted cluster labels only include those in \code{mcmc$clusters} of corresponding chain. 
  
}
\value{
The functions do not return anything but produce one or more plots.
}
\references{
Vávra, J., Komárek, A., Grün, B., Malsiner-Walli, G. Clusterwise multivariate regression of mixed-type panel data. Stat Comput 34, 46 (2024). https://doi.org/10.1007/s11222-023-10304-5
}
\author{
Jan Vávra
}
\seealso{
\code{\link{clustGLMM}}, \code{\link{get_scalar_samples}}, \code{\link{nice_nrow_ncol}}, \code{\link{post_processing}}.
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
plot(mcmc)
plot(mcmc, ng_trace_chain_split=FALSE)
                       
###-----------------------------------------------------------------------------
###     Creating plots
###-----------------------------------------------------------------------------

## Traceplots of number of non-empty clusters
# Plotting with defaults values:
# thin = 1, burnin = 0, chains = 1:mcmc$nchains
plot_traceplots(mcmc = mcmc, what = "Gplus")

## Traceplots of number of observations in cluster 1
plot_traceplots(mcmc = mcmc, what = "ng", gspec = 1)

# Omitting the burn-in-period
plot_traceplots(mcmc = mcmc, what = "ng", gspec = 1, burnin = 20)

# Thinned plots
plot_traceplots(mcmc = mcmc, what = "ng", gspec = 1, burnin = 20, thin = 5)

## First fixed beta coefficient for the first numerical outcome
plot_ACF(mcmc = mcmc, what = "beta_num_fix", yspec = mcmc$Nums[1], dimspec = 1)
plot_ECDF(mcmc = mcmc, what = "beta_num_fix", yspec = mcmc$Nums[1], dimspec = 1)
plot_kerneldensity(mcmc = mcmc, what = "beta_num_fix", yspec = mcmc$Nums[1],
                   dimspec = 1, dolegend = TRUE)
plot_traceplots(mcmc = mcmc, what = "beta_num_fix", yspec = mcmc$Nums[1],
                dimspec = 1)
plot_diagnostics(mcmc = mcmc, what = "beta_num_fix", yspec = mcmc$Nums[1], dimspec = 1)

## Correlation matrix between the random effects
# True correlation matrix used for generating the data
corr <- matrix(c(1, -0.5, -0.5, -0.4, -0.4,
                 -0.5, 1, 0.3, 0.4, 0.5,
                 -0.5, 0.3, 1, 0.2, 0.3,
                 -0.4, 0.4, 0.2, 1, 0.1,
                 -0.4, 0.5, 0.3, 0.1, 1), byrow = TRUE, ncol = 5)
                 
par(mfrow = c(mcmc$totnran-1, mcmc$totnran-1), mar = c(4, 4, 1, 1))
for(i in 1:(mcmc$totnran-1)){
  for(j in 2:(mcmc$totnran)){
    if(i < j){
      plot_traceplots(mcmc = mcmc, what = "corSigma", 
                      dimspec = c(i, j),
                      labcex = 0.6)
      abline(h = corr[i,j], col = "black", lty = 2)
    }else{
      plot(x = c(0, 1), y = c(0, 1), type = "n", xlab = "", ylab = "",
           xaxt = "n", yaxt = "n", bty = "n")
    }
  }
}
# or simply use
plot_traceplots_param(mcmc, "corSigma")
# but no true value displayed...

## Possible what arguments:
# "_param" - plots everything tied to the given what parameter
plot_traceplots_param(mcmc, "Gplus", thin = 5)
plot_traceplots_param(mcmc, "e0", thin = 5)
plot_traceplots_param(mcmc, "ng", thin = 5)
plot_traceplots_param(mcmc, "w", thin = 5)
plot_traceplots_param(mcmc, "sdSigma", thin = 5)
plot_traceplots_param(mcmc, "corSigma", thin = 5)
plot_traceplots_param(mcmc, "Sigma", thin = 5)
plot_traceplots_param(mcmc, "InvSigma", thin = 5)
plot_traceplots_param(mcmc, "prec_num", thin = 5)
plot_traceplots_param(mcmc, "beta_num_fix", thin = 5)
plot_traceplots_param(mcmc, "beta_num", thin = 5)
plot_traceplots_param(mcmc, "beta_poi_fix", thin = 5)
plot_traceplots_param(mcmc, "beta_poi", thin = 5)
plot_traceplots_param(mcmc, "beta_bin_fix", thin = 5)
plot_traceplots_param(mcmc, "beta_bin", thin = 5)
plot_traceplots_param(mcmc, "beta_ord_fix", thin = 5)
plot_traceplots_param(mcmc, "beta_ord", thin = 5)
plot_traceplots_param(mcmc, "c_ord", thin = 5)
plot_traceplots_param(mcmc, "beta_cat_fix", thin = 5)
plot_traceplots_param(mcmc, "beta_cat", thin = 5)
plot_traceplots_param(mcmc, "loglik", thin = 5)

## Other "_param" versions for different types of plots
plot_kerneldensity_param(mcmc, "beta_num")
plot_ECDF_param(mcmc, "prec_num")
plot_ACF_param(mcmc, "w")

## Comparing different cluster-specific parameters
## We recommend to do it for chains separately
ch <- 1
TAB <- table(mcmc$last[[ch]]$U)
nonempty <- as.numeric(names(TAB)[which(TAB>0)]) 
# non-empty clusters decided based on last sampled values
# by default, mcmc$clusters (based on all iterations) is taken

neff <- mcmc$ngrp[[mcmc$Nums[1]]]
oldpar <- par(mfrow = c(length(mcmc$Nums), neff), mar = c(4, 4, 1, 1))

for(y in mcmc$Nums){
  for(i in 1:neff){
    plot_clusters(mcmc = mcmc, what = "beta_num", 
                  yspec = y, dimspec = i, whichg = nonempty,
                  setparmfrow = FALSE, chains = ch,
                  doECDF = FALSE)
    # Now you can add anything to the plot
    # e.g. true values, etc.
  }
}
par(oldpar)

# or simply use
plot_clusters_kerneldensity_param(mcmc = mcmc, what = "beta_num", chains = c(ch))
# where cluster labels are taken from mcmc$clusters

}
