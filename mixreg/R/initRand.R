initRand <- function(fmla,data,K,seed=NULL) {
#
# Function initRand.  To form starting values for the EM
# algorithm, randomly.
#

tmp <- lm(fmla,data=data)
ccc <- coef(tmp)
ncc <- length(ccc)
sdc <- 0.3*(abs(ccc))
vvv <- summary(tmp)$sigma**2
lll <- 1/K

if(is.null(seed)) seed <- sample(1:1e5,1)
set.seed(seed)
rslt <- list()
for(j in 1:K) {
	cft <- rnorm(ncc,ccc,sdc)
	rslt[[j]] <- list(beta=cft,sigsq=vvv,lambda=lll)
}
attr(rslt,"seed") <- seed
rslt
}
