## ----setup, set.seed = 18, include=FALSE---------------------------------
knitr::opts_chunk$set(echo = TRUE)
set.seed(18)
library(corrcoverage)

## ------------------------------------------------------------------------
set.seed(18)
library(corrcoverage)
# library(simGWAS)

data <- system.file("extdata", "", package="corrcoverage")

#  Simulate reference haplotypes
nsnps <- 200
nhaps <- 1000
lag <- 30  # genotypes are correlated between neighbouring variants
maf <- runif(nsnps + lag, 0.05, 0.5)  # common SNPs
laghaps <- do.call("cbind", lapply(maf, function(f) rbinom(nhaps, 1, f)))
haps <- laghaps[, 1:nsnps]
for (j in 1:lag) haps <- haps + laghaps[, (1:nsnps) + j]
haps <- round(haps/matrix(apply(haps, 2, max), nhaps, nsnps, byrow = TRUE))
snps <- colnames(haps) <- paste0("s", 1:nsnps)
freq <- as.data.frame(haps + 1)
freq$Probability <- 1/nrow(freq)
sum(freq$Probability)
MAF <- colMeans(freq[, snps] - 1)  # minor allele frequencies
CV <- sample(snps[which(colMeans(haps) > 0.1)], 1)
iCV <- sub("s", "", CV)  # index of cv
LD <- cor2(haps) # correlation between SNPs

## ------------------------------------------------------------------------
OR <- 1.1 # odds ratios
N0 <- 10000 # number of controls
N1 <- 10000 # number of cases
  
#z0 <- simulated_z_score(N0 = N0, # number of controls
#                        N1 = N1, # number of cases
#                        snps = snps, # column names in freq
#                        W = CV, # causal variants, subset of snps
#                        gamma.W = log(OR), # log odds ratios
#                        freq = freq) # reference haplotypes

z0 <- readRDS(paste0(data,"/z0_2.RDS"))

## ------------------------------------------------------------------------
varbeta <- Var.data.cc(f = MAF, N = N1+N0, s = N1/(N0+N1)) # variance of 
                                                       # estimated effect size

## ------------------------------------------------------------------------
postprobs <- ppfunc(z = z0, V = varbeta) 

## ------------------------------------------------------------------------
muhat <- est_mu(z0, MAF, N0, N1)
muhat

## ------------------------------------------------------------------------
thr = 0.9
corrcov <- corrected_cov(pp0 = postprobs, mu = muhat, V = varbeta, 
                         Sigma = LD, thr = thr, nrep = 1000)
cs <- credset(pp = postprobs, thr = thr)
data.frame(claimed.cov = cs$claimed.cov, corr.cov =  corrcov, nvar = cs$nvar)

## ------------------------------------------------------------------------
# z0.tmp <- simulated_z_score(N0 = N0, # number of controls
#                            N1 = N1, # number of cases
#                            snps = snps, # column names in freq
#                            W = CV, # causal variants, subset of snps
#                            gamma.W = log(OR), # log odds ratios
#                            freq = freq, # reference haplotypes
#                            nrep = 200) # increase nrep to increase accuracy of estimate

z0.tmp <- readRDS(paste0(data,"/z0.tmp_2.RDS"))

pps <- ppfunc.mat(zstar = z0.tmp, V = varbeta)  # find pps 
cs.cov <- apply(pps, 1, function(x) credset(x, CV = iCV, thr = thr)$cov)
true.cov.est <- mean(cs.cov)
data.frame(claimed.cov = cs$claimed.cov, corr.cov =  corrcov, 
           true.cov = true.cov.est, nvar = cs$nvar)

## ------------------------------------------------------------------------
res <- corrected_cs(z = z0, f = MAF, N0, N1, 
                    Sigma = LD, lower = 0.5, upper = 1, desired.cov = 0.9)
res

## ------------------------------------------------------------------------
new.cs.sims <- apply(pps, 1, function(x) credset(x, CV = iCV, thr = res$req.thr)$cov)
true.cov.est2 <- mean(new.cs.sims)

## ------------------------------------------------------------------------
df1 <- data.frame(claimed.cov = round(cs$claimed.cov, 3), corr.cov =  round(corrcov, 3), true.cov = round(true.cov.est, 3), nvar = cs$nvar)
print(df1, row.names = FALSE)

## ------------------------------------------------------------------------
df2 <- data.frame(claimed.cov = round(res$size, 3), corr.cov = round(res$corr.cov, 3), true.cov = round(true.cov.est2, 3), nvar = length(res$credset))
print(df2, row.names = FALSE)

