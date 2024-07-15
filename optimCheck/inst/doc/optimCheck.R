## ---- eval = FALSE, echo = FALSE-----------------------------------------
#  
#  rmarkdown::render("optimCheck-quicktut.Rmd") # R code to render vignette
#  

## ---- echo = -1----------------------------------------------------------
set.seed(2608) # set seed to fix output
d <- 12 # dimension of optimization problem

# create the objective function: Q(x) = x'Ax - 2b'x
A <- crossprod(matrix(rnorm(d^2), d, d)) # positive definite matrix
b <- rnorm(d)
objfun <- function(x) crossprod(x, A %*% x)[1] - 2 * crossprod(b, x)[1]

xhat <- solve(A, b) # analytic solution

# numerical mode-finding using optim
xfit <- optim(fn = objfun,                    # objective function
              par = xhat * 5,                 # initial value is far from the solution
              control = list(maxit = 1e5))    # very large max. number of iterations


## ------------------------------------------------------------------------
# any value other than 0 means optim failed to converge
xfit$convergence 

## ---- fig.width = 10, fig.height = 6, out.width = "97%"------------------
require(optimCheck) # load package

# projection plots
xnames <- parse(text = paste0("x[", 1:d, "]")) # variable names
oproj <- optim_proj(fun = objfun,              # objective function
                    xsol = xfit$par,           # potential solution
                    maximize = FALSE,          # indicates that a local minimum is sought
                    xrng = .5,                 # range of projection plot: x_i +/- .5*|x_i|
                    xnames = xnames)


## ------------------------------------------------------------------------
sapply(oproj, function(x) dim(as.matrix(x)))

## ------------------------------------------------------------------------
oproj # same print method as summary(oproj)

## ------------------------------------------------------------------------
diff(oproj) # equivalent to summary(oproj)$xdiff

# here's exactly what these are
xsol <- summary(oproj)$xsol # candidate solution
xopt <- summary(oproj)$xopt # optimal solution in each projection plot
xdiff <- cbind(abs = xopt-xsol, rel = (xopt-xsol)/abs(xsol))
range(xdiff - diff(oproj))

## ----eval = FALSE--------------------------------------------------------
#  testthat::test_package("optimCheck", reporter = "progress")

## ------------------------------------------------------------------------
orefit <- optim_refit(fun = objfun,        # objective function
                      xsol = xfit$par,     # potential solution
                      maximize = FALSE)    # indicates that a local minimum is sought
summary(orefit) # same print method as orefit

## ---- fig.width = 10, fig.height = 6, out.width = "97%"------------------
# projection plots with refined solution
optim_proj(xsol = orefit$xopt, fun = objfun,
           xrng = .5, maximize = FALSE)

## ------------------------------------------------------------------------
# gradient of the objective function
objgrad <- function(x) 2 * drop(A %*% x - b)

# mode-finding using quasi-Newton method
xfit2 <- optim(fn = objfun,                    # objective function
               gr = objgrad,                   # gradient
               par = xfit$par,                 # initial value (first optim fit)
               method = "BFGS")

# external refit test with optimizer of choice
orefit2 <- optim_refit(fun = objfun,
                       xsol = xfit$par,        # initial value (first optim fit)
                       xopt = xfit2$par,       # refit value (2nd fit with quasi-Newton method
                       maximize = FALSE)

# project plot test on refit solution
optim_proj(xsol = orefit2$xopt, fun = objfun,
           xrng = .5, maximize = FALSE, plot = FALSE)

