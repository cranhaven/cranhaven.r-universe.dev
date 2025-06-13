controlLCA <- function(maxiter = 1e05, tol = 1e-04, nrep = 5)
  # see '?poLCA'
{
  list( maxiter = maxiter, tol = tol, nrep = nrep )
}



controlReg <- function(maxiter = 5000, tol = 1e-05 )
{
  list( maxiter = maxiter, tol = tol )
}



controlGA <- function(popSize = 20, maxiter = 100, run = maxiter/2,
                      pcrossover = 0.8, pmutation = 0.2,
                      elitism = base::max(1, round(popSize*0.05)))
{
  list(popSize = popSize, maxiter = maxiter, run = run,
       pcrossover = pcrossover, pmutation = pmutation,
       elitism = elitism)
}
