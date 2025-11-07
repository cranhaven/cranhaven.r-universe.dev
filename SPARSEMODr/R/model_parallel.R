##' Run separate realizations of the model in parallel
##'
##' The future package is used for parallelization, so the default is
##' sequential execution. For parallel execution you need to declare a
##' future plan.
##' @title Parallel sparse model
##' @param ... passed to model_interface
##' @param input_realz_seeds integer vector of random seeds to
##'   parallelize over.
##' @return data table of model results
##' @author Toby Dylan Hocking
##' @examples
##' if(interactive() && require(future)){
##'   future::plan("multiprocess")
##' }
##' ex_dir <- system.file(
##'   "extdata", "sparsemodr_example.Rdata", package="SPARSEMODr", mustWork=TRUE)
##' load(ex_dir)
##' n_pop <- length(dat_list[["pop_N"]])
##' model_dt <- with(dat_list, SPARSEMODr::covid19_model_parallel(
##'   census_area, E_pops, dist_vec, pop_N))

model_parallel <- function(...,
                           input_realz_seeds=1:2,
                           control=NULL)
{
    ## we need to store the ... args here (future can not access the
    ## ... args of model_parallel from within one_seed).
    arg.list <- list(...)
    #print(names(arg.list))

    if (is.null(control))
    {
        stop("Error, please supply a control list.")
    }

    ## per-realz function
    one_seed <- function(seed)
    {
        arg.list[["input_realz_seeds"]] <- seed
        one.seed.list <- model_interface(control, arg.list)
        do.call(data.table, one.seed.list)
    }

    ## future_lapply
    dt.list <- future.apply::future_lapply(input_realz_seeds, one_seed)
    do.call(rbind, dt.list)
}
