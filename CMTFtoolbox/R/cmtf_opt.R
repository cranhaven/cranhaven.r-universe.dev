#' Coupled matrix and tensor factorizations
#'
#' @param Z Combined dataset and mode object as produced by [setupCMTFdata()].
#' @param numComponents Number of components
#' @param initialization Initialization, either "random" (default) or "nvec" for numComponents components of the concatenated data using svd.
#' @param method Optimization method to use (default = "CG", the conjugate gradient). See [mize::mize()] for other options.
#' @param cg_update Update method for the conjugate gradient algorithm, see [mize::mize()] for the options (default="HS", Hestenes-Steifel).
#' @param line_search Line search algorithm to use, see [mize::mize()] for the options (default="MT", More-Thuente).
#' @param max_iter Maximum number of iterations.
#' @param max_fn Maximum number of function evaluations.
#' @param abs_tol Function tolerance criterion for convergence.
#' @param rel_tol Relative function tolerance criterion for convergence.
#' @param grad_tol Absolute tolerence for the l2-norm of the gradient vector.
#' @param nstart Number of models to produce (default 1). If set higher than one, the package will return the best fitted model.
#' @param numCores Number of cores to use (default 1). If set higher than one, the package will attempt to run in parallel.
#' @param sortComponents Sort the components in the output by descending order of variation explained.
#' @param allOutput Return all created models. Ignored if nstart=1.
#'
#' @return List object, similar to [mize::mize()] output. Includes a Fac object of the model, which is a list of components per mode. Also includes an init object giving the initialized input vectors.
#' @export
#' @importFrom foreach %dopar%
#'
#' @examples
#' A = array(rnorm(108*2), c(108, 2))
#' B = array(rnorm(100*2), c(100, 2))
#' C = array(rnorm(10*2), c(10, 2))
#' D = array(rnorm(100*2), c(100,2))
#' E = array(rnorm(10*2), c(10,2))
#'
#' df1 = reinflateTensor(A, B, C)
#' df2 = reinflateTensor(A, D, E)
#' datasets = list(df1, df2)
#' modes = list(c(1,2,3), c(1,4,5))
#' Z = setupCMTFdata(datasets, modes, normalize=FALSE)
#'
#' model = cmtf_opt(Z, 1, rel_tol=1e-4) # quick convergence for example only
cmtf_opt = function(Z, numComponents, initialization="random", method="CG", cg_update="HS", line_search="MT", max_iter=10000, max_fn=10000, abs_tol=1e-8, rel_tol=1e-8, grad_tol=1e-8, nstart=1, numCores=1, sortComponents=TRUE, allOutput=FALSE){
  numModes = max(unlist(Z$modes))
  numDatasets = length(Z$object)

  # Prepare initialization outside of the main loop to allow it as output
  inits = list()
  for(i in 1:nstart){
    inits[[i]] = initializeCMTF(Z, numComponents, initialization, output="vect")
  }

  # Create nstart models, in parallel if requested
  if((numCores > 1) & (nstart > 1)){
    cl = parallel::makeCluster(numCores)
    doParallel::registerDoParallel(cl)

    models = foreach::foreach(i=1:nstart) %dopar% {
      fg = list("fn"=function(x){return(CMTFtoolbox::cmtf_fun(x,Z))}, "gr"=function(x){return(CMTFtoolbox::cmtf_gradient(x,Z))}, "fg"=function(x){return(CMTFtoolbox::cmtf_fg(x,Z))})
      message(dim(Z$object[[1]])) # somehow this line fixes a bug where parallel gives an error about "dims cannot be of length 0"
      model = mize::mize(par=inits[[i]], fg=fg, max_iter=max_iter, max_fn=max_fn, abs_tol=abs_tol, rel_tol=rel_tol, grad_tol=grad_tol, method=method, cg_update=cg_update, line_search=line_search, store_progress=TRUE)
    }
    parallel::stopCluster(cl)

  } else{
    models = list()
    for(i in 1:nstart){
      fg = list("fn"=function(x){return(CMTFtoolbox::cmtf_fun(x,Z))}, "gr"=function(x){return(CMTFtoolbox::cmtf_gradient(x,Z))}, "fg"=function(x){return(CMTFtoolbox::cmtf_fg(x,Z))})
      models[[i]] = mize::mize(par=inits[[i]], fg=fg, max_iter=max_iter, max_fn=max_fn, abs_tol=abs_tol, rel_tol=rel_tol, grad_tol=grad_tol, method=method, cg_update=cg_update, line_search=line_search, store_progress=TRUE)
    }
  }

  # Attach extra model info
  for(i in 1:nstart){
    models[[i]]$Fac = vect_to_fac(models[[i]]$par, Z, sortComponents=sortComponents)
    models[[i]]$init = vect_to_fac(inits[[i]], Z, sortComponents=sortComponents)
    models[[i]]$varExp = calculateVarExp(models[[i]]$Fac, Z) * 100
    models[[i]]$varExpPerComponent = calcVarExpPerComponent(models[[i]]$Fac, Z) * 100
    models[[i]]$f_per_block = cmtf_fun(models[[i]]$par, Z, manual=TRUE)
  }

  # Return all models if specified, otherwise return only the best model
  if(allOutput == TRUE){
    return(models)
  }
  else{
    bestModel = 0
    bestObjective = Inf
    for(i in 1:nstart){
      model = models[[i]]
      if(model$f <= bestObjective){
        bestModel = model
        bestObjective = model$f
      }
    }
    return(bestModel)
  }
}
