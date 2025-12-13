set.seed(123)

n <- 160
trt <- factor(rep(0:1, each = n/2))
y <- 4 + (trt == 1) + rnorm(n)
z <- matrix(rnorm(n * 2), ncol = 2)

dat <- data.frame(y, trt, z)

mod <- lm(y ~ trt, data = dat)

## Note that ntree should usually be higher
frst <- pmforest(mod, ntree = 20) 
pmods <- pmodel(frst, fun = identity)

## Note that B should be at least 100
## The low B is just for demonstration 
## purposes.
tst <- test_heterogeneity(forest = frst, 
                          pmodels = pmods, 
                          B = 10) 
tst$pvalue
plot(tst)
