#' Running model on a grid of parameters/initial states
#' 
#' This function is useful for running a large scan of parameter combinations 
#' for the same model. Typical use cases are probing stability of an attractor, 
#' effect of certain parameters on the system, etc.
#' 
#' Grid is a data frame whose columns are model parameters. 
#' See `model$contents()` for tunable parameters. 
#'
#' @param model_gen Odin model generator, see `getOdinGen()`.
#' @param grid Data frame of the parameter grid.
#' @param apply.fn Function to apply before return (e.g., some summary).
#' @param n.core Number of cores for parallel computing.
#' @param custom.export Names of additional variables used by apply.fn.
#' @param ... Model `$run(...)` parameters.
#'
#' @returns List of model run results.
#' @export
#'
#' @examples
#' vignette("grid-scan", "clockSim")
grid_scan <- function(model_gen,
                      grid,
                      apply.fn = identity,
                      n.core = 2,
                      custom.export = NULL,
                      ...) {
  # Wrap ellipsis
  elli <- list(...)
  
  if (n.core > 1) {
    # Create cluster
    cluster <- parallel::makeCluster(n.core)
    on.exit(parallel::stopCluster(cluster))
    
    # Setup workers - initialize model in each worker
    #   Unfortunately, grid is copied to each worker unless fork.
    parallel::clusterExport(cluster,
                            c("model_gen", "grid", "apply.fn", "elli"),
                            envir = environment())
    if (!is.null(custom.export))
      parallel::clusterExport(cluster, custom.export)
    parallel::clusterEvalQ(cluster, library(clockSim))
    parallel::clusterEvalQ(cluster, model <- model_gen$new())
    
    # Run parallel
    res <- parallel::parLapply(cluster, 1:nrow(grid), \(idx) {
      model$set_user(user = grid[idx, ,drop=FALSE] |> as.list())
      res <- do.call(model$run, elli)
      return(apply.fn(res))
    })
    return(res)
    
  } else{
    # Serial lapply
    model <- model_gen$new()
    lapply(1:nrow(grid), \(idx) {
      model$set_user(user = grid[idx, ,drop=FALSE] |> as.list())
      res <- do.call(model$run, elli)
      return(apply.fn(res))
    })
  }
}
