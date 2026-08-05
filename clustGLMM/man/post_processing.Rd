\name{post_processing}
\alias{post_processing}
\alias{permute_cluster_labels}
\title{
Post-process the Chains Sampled with \code{clustGLMM}
}
\description{
Sampling MCMC chains via \code{\link{clustGLMM}} assumes underlying \code{Gmax} clusters and allows for learning the true number of underlying clusters. 
Many clusters are eventually empty and only some cluster labels survive. 
Given random initialization, different chains may end up with different labels of non-empty clusters including their total count. 
The function \code{post_processing} performs the post-processing suggested by Frühwirth-Schnatter (2011) and Malsiner-Walli et al (2016).
}
\usage{
post_processing(mcmc, cols, iter.max = 200, nstart = 30)
permute_cluster_labels(mcmc, perm)
}
\arguments{
  \item{mcmc}{
an output of \code{\link{clustGLMM}} accepted in both formats (list or data.frame).
}
  \item{cols}{
a vector of column names of cluster-specific variables to be used for k-means clustering. 
If missing, all cluster-specific parameters are used. 
}
  \item{iter.max}{
the maximum number of iterations of \code{\link[stats]{kmeans}} algorithm allowed. 
}
  \item{nstart}{
how many random sets for \code{\link[stats]{kmeans}} should be chosen? 
}
  \item{perm}{
a list of permutation of cluster labels, one permutation for each chain. 
If missing, the chains are aligned to chain 1 based on the Hungarian algorithm, see package \pkg{RcppHungarian}.
}
}
\details{
\code{post_processing} follows the post-processing suggested by Frühwirth-Schnatter (2011) and Malsiner-Walli et al. (2016) to identify the number of underlying cluster and to resolve possible issues with label-switching.
All post-processing is done separately for each chain, keep in mind that the results among chains may differ due to different initial conditions. 
The procedure can be divided into several steps:

0) The procedure works with \code{mcmc$draws} only under \code{howsave = "data.frame"}, so if \code{howsave = "list"} then \code{mcmc} is converted to the \code{"matrix"} format, post-processed and converted back to "list".  

1) Take the most frequent \code{Gplus} value - \code{modeGplus} - the number of non-empty clusters across the sampled chain and find the posterior draws with \code{Gplus = modeGplus}. 
The indices are stored in \code{inds} and their length is \code{iters}. 

2) Target all cluster-specific parameters. 
To clarify these are parameters with \code{(g)} in their column name, parameters \code{w}, \code{U}, \code{pUig} do not count. 
If you prefer to use just a subset of those parameters, you have to specify the column names by \code{cols} input variable.

3) Create submatrices of sampled cluster-specific parameters. 
Index by index in \code{inds} gather the cluster-specific parameters from only non-empty clusters. 
Due to label-switching it may happen that different cluster labels correspond to the non-empty clusters at each index. 
After these operations it may happen that values in one column correspond to different clusters.

4) Prepare the data for k-means clustering to resolve the label-switching. 
Gather values for different clusters below each other. 
That is, if there are \code{L} different cluster-specific parameters and \code{iters} samples, then the matrix is rearranged from \code{iters * (L * modeGplus)} to \code{(iters * modeGplus) * L}. 

5) K-means clustering \code{\link[stats]{kmeans}} using \code{centers=modeGplus, iter.max = iter.max, nstart = nstart} is applied to matrix from 5). 
Ideally, rows corresponding to iteration \code{m} would be each clustered to a different mode. 
In such case, we would obtain a permutation for each row that aligns the meaning of the cluster labels across all iterations. 
Unfortunately, it may happen that some iterations do not result into clear permutation. 
In such case, this iteration has to be eliminated (replaced with \code{NA}), so the length of the chain used for inference is reduced from \code{iters} to possibly even lower number \code{Mrhos}. 

6) Regroup the matrix of cluster-specific data back to the format in 3) while also correcting for the permutations found in 5). 
This should result in a matrix where all the values in one column  correspond to the same cluster. 
This output matrix is called \code{clusterspec}.

7) Create the matrix of posterior draws by relabelling and permuting clustering-related parameters.

8) Update all other items saved in \code{mcmc}, see \code{\link{clustGLMM}}.
Especially, 
\code{clustering}, 
\code{certainty}, 
\code{G} (now \code{modeGplus}, possibly different among chains!), 
\code{iterations} (omit iterations with NA values created by not leading to permutation in step 5)), 
\code{settings} (changed dimensions for cluster-specific parameters), 
\code{param_names}, 
\code{last} (list of last sampled values - now from the last iterations with \code{modeGplus} clusters, properly permuted) 
and \code{call} (summary of the post-processing). 
Many items remain the same as in \code{mcmc}, e.g., even the initial values.

\code{permute_cluster_labels} simply takes the list of permutations \code{perm} and reshuffles the cluster labels as requested. 
Useful when you want to align the meaning of cluster labels across different chains. 
For example, an element of the \code{perm} list should be a vector \code{c(2, 3, 4, 1)}, if

cluster 2 is to become cluster 1,

cluster 3 is to become cluster 2,

cluster 4 is to become cluster 3,

cluster 1 is to become cluster 4.

If \code{perm} is missing, then labels within the first chain are fixed. 
For remaining chains we take the posterior means of group-specific parameters and compute the root mean square errors among all couples of cluster labels (between the first and currect chain).
The resulting cost matrix is sent to \code{\link[RcppHungarian]{HungarianSolver}} and second column of \code{$pairs} output is used as the permuation for the current chain.
This automatic choice of \code{perm} is slowed down by the call of \code{\link{summary}} to \code{mcmc}. 
}
\value{
Both functions return back list of class \code{\link{clustGLMM}}, the same structure as \code{mcmc}, but with modified items.
}
\references{
Frühwirth-Schnatter, S. (2011) Dealing with Label Switching under Model Uncertainty, John Wiley & Sons, chap 10, pp 213–239

Malsiner-Walli, G., Frühwirth-Schnatter, S., Grün, B.(2016) Model-based clustering based on sparse finite Gaussian mixtures. Statistics and Computing 26:303–324. https://doi.org/10.1007/s11222-014-9500-2

Vávra, J., Komárek, A., Grün, B., Malsiner-Walli, G. Clusterwise multivariate regression of mixed-type panel data. Stat Comput 34, 46 (2024). https://doi.org/10.1007/s11222-023-10304-5
}
\author{
Jan Vávra
}
\seealso{
\code{\link{clustGLMM}}, \code{\link{plot.clustglmm}}, \code{\link{nice_nrow_ncol}}, \code{\link{post_processing}}, \code{\link[stats]{kmeans}}.
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
                  G = Gmax, iter = 200, nchains = nchains)

###-----------------------------------------------------------------------------
###     Post-processing the sampled chains
###-----------------------------------------------------------------------------

### First run the function post_processing
# takes some time with iter large....
set.seed(12345)
mcmc <- post_processing(mcmc)
mcmc
summary(mcmc)

# All plotting functions should work even with these post-processed draws 
plot(mcmc, "beta_num", "clusters_kerneldensity", chains=1:nchains)

### Additionally permute so that the meaning of the number of clusters
### is kept the same across all chains

## first we have to recognize which chain has which meaning
neff <- mcmc$ngrp[["ynum"]]
oldpar <- par(mfrow = c(nchains,neff), mar = c(4,4,1,1))
for(ch in 1:nchains){
  TAB <- table(mcmc$last[[ch]]$U)
  nonempty <- as.numeric(names(TAB)[which(TAB>0)]) 
  
  for(i in 1:neff){
    plot_clusters(mcmc = mcmc, what = "beta_num", 
                  yspec = "ynum", dimspec = i, whichg = nonempty,
                  setparmfrow = FALSE, chains = ch,
                  doECDF = FALSE)
  }
}
par(oldpar)

## confusion matrices with the true labelling
for(ch in 1:mcmc$nchains){
  longitudinal_mixed_type_data[,paste0("clustering",ch)] <- 
    mcmc$clustering[longitudinal_mixed_type_data[,id],ch]
}

data1 <- longitudinal_mixed_type_data[longitudinal_mixed_type_data$j==1,]
table(data1$clustering1, data1$g)
table(data1$clustering2, data1$g)


### Example how to change the permutation so that results across chains align
### + align also with the true labelling within the given data
perm <- list()
perm[[1]] <- c(2, 1) # change if needed
perm[[2]] <- c(1, 2) # change if needed

# function to permute meaning of cluster labels in mcmc according to perm
mcmc1 <- permute_cluster_labels(mcmc, perm)

# If perm is not specified,
# Hungarian algorithm matches the labelling of other chains to chain number 1
mcmc2 <- permute_cluster_labels(mcmc)
# which in this case is different from the true labelling in data



###-----------------------------------------------------------------------------
###     Plotting post-processed chains
###-----------------------------------------------------------------------------
# Notice that now there are only modeGplus clusters and not original G

plot(mcmc1, "w", "traceplots", thin = 10)
plot(mcmc1, "w", "kerneldensity")
plot(mcmc1, "beta_num", "clusters_kerneldensity")
plot(mcmc1, "beta_poi", "clusters_kerneldensity")
plot(mcmc1, "beta_bin", "clusters_kerneldensity")
plot(mcmc1, "beta_ord", "clusters_kerneldensity")
plot(mcmc1, "beta_cat", "clusters_kerneldensity")

# Compare
plot(mcmc1, "beta_num", "clusters_kerneldensity")
plot(mcmc2, "beta_num", "clusters_kerneldensity")

}
