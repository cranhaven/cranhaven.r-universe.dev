## ----message=FALSE, echo=FALSE------------------------------------------------
library("gbm3")

## -----------------------------------------------------------------------------
# Create strata
strats <- sample(1:5, 100, replace=TRUE)

# Create CoxPH dist object
cox_dist <- gbm_dist(name="CoxPH", ties="breslow", 
                     strata=strats, prior_node_coeff_var=100)


## -----------------------------------------------------------------------------
# Create pairwise grouped data
# create query groups, with an average size of 25 items each
N <- 1000
num.queries <- floor(N/25)
query <- sample(1:num.queries, N, replace=TRUE)

# X1 is a variable determined by query group only
query.level <- runif(num.queries)
X1 <- query.level[query]

# X2 varies with each item
X2 <- runif(N)

# X3 is uncorrelated with target
X3 <- runif(N)

# The target
Y <- X1 + X2

# Add some random noise to X2 that is correlated with
# queries, but uncorrelated with items

X2 <- X2 + scale(runif(num.queries))[query]

# Add some random noise to target
SNR <- 5 # signal-to-noise ratio
sigma <- sqrt(var(Y)/SNR)
Y <- Y + runif(N, 0, sigma)

data <- data.frame(Y, query=query, X1, X2, X3)

# Create appropriate Pairwise object
pair_dist <- gbm_dist(name="Pairwise", group="query", max_rank=1, metric="ndcg")

## -----------------------------------------------------------------------------
# Create a QuantileGBMDist object
quant_dist <- gbm_dist(name="Quantile", alpha=0.1)

## -----------------------------------------------------------------------------
# Creat a t-distribution object with 7 degrees of freedom
t_dist <- gbm_dist(name="TDist", df=7)

## -----------------------------------------------------------------------------
# Create a TweedieGBMDist object with a power of 2 - equivalent to a Gamma distribution
tweedie_dist <- gbm_dist(name="Tweedie", power=2)

