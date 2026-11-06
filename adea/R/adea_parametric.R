#' Selection of an optimal subset of variables for DEA analysis
#'
#' The function returns a list of DEA models, progressively removing at least one variable at each step.
#' This process results in a sequence of models with increasing values ADEA loads.
#'
#' The models are sorted by increasing values of loads, starting from initially given model and progressing to the one with one input and one output variable.
#' Note that the load value for the model with one input and one output is 1.
#' In each step, at least one variable is dropped, but more than one can be dropped if necessary.
#'
#' See example for more details.
#' 
#' @inheritParams adea
#' @inheritParams adea_stepwise
#' @return The function returns an adeaparametric class object with the following named members:
#' \itemize{
#' \item name: A label of the model.
#' \item orientation: DEA model orientation ('input' or 'output').
#' \item load.orientation: load DEA model orientation ('input', 'output', or 'inoutput').
#' \item loads: Load of each model.
#' \item models: List of all ADEA models.
#' \item ninputs: Number of input variables in each model.
#' \item noutputs: Number of output variables in each model.
#' \item nvariables: Number of variables in each model.
#' \item inputnames: Names of input variables in each model.
#' \item outputnames: Names of output variables in each model.
#' \item out: Variable, or variables, dropped in each step.
#' \item solver: The solver used for the resolution of the optimization problem.
#' }
#' @seealso \code{\link{adea_hierarchical}}
#' @examples
#' # Read data
#' data('cardealers4')
#' input <- cardealers4[, c('Employees', 'Depreciation')]
#' output <- cardealers4[, c('CarsSold', 'WorkOrders')]
#' 
#' # Compute all dea models in parametric way and store in sol.ap
#' sol.ap <- adea_parametric(input, output)
#' # Show result
#' sol.ap
#' #       Loads nEfficients nVariables nInputs nOutputs                  Inputs              Outputs
#' # 4 0.6666667           2          4       2        2 Employees, Depreciation CarsSold, WorkOrders
#' # 3 0.9575672           2          3       1        2            Depreciation CarsSold, WorkOrders
#' # 2 1.0000000           1          2       1        1            Depreciation             CarsSold
#' 
#' # Summary the model with 3 variables
#' summary(sol.ap$models[[3]])
#' # Model name                                 
#' # Orientation                           input
#' # Load orientation                   inoutput
#' # Model load                0.957567163474156
#' # Input load.Depreciation                   1
#' # Output load.CarsSold       1.04243283652584
#' # Output load.WorkOrders    0.957567163474156
#' # Inputs                         Depreciation
#' # Outputs                 CarsSold WorkOrders
#' # nInputs                                   1
#' # nOutputs                                  2
#' # nVariables                                3
#' # nEfficients                               2
#' # Eff. Mean                  0.90022318389575
#' # Eff. sd                   0.135194867030839
#' # Eff. Min.                 0.651504424778761
#' # Eff. 1st Qu.              0.872252747252747
#' # Eff. Median               0.942225031605563
#' # Eff. 3rd Qu.              0.997898230088496
#' # Eff. Max.                                 1
#' # Get efficiencies for the model with 3 variables
#' sol.ap$models[[3]]$eff
#' #  Dealer A  Dealer B  Dealer C  Dealer D  Dealer E  Dealer F
#' # 0.9915929 1.0000000 0.8928571 0.8653846 1.0000000 0.6515044
#' @export
adea_parametric <- function(input, output, orientation = c('input', 'output'), load.orientation = c('inoutput', 'input', 'output'), name = '', direction = c('backward', 'backward/input', 'backward/output'), solver = 'auto', verbose = 0) {

    ## Match args
    orientation <- match.arg(orientation)
    load.orientation <- match.arg(load.orientation)
    
    ## Check input and output
    err <- adea_check(input = input, output = output)
    if (!isTRUE(err)) stop(err)
    
    ## Canonize input and output format
    output <- adea_setup(input, output)
    input <- output$input
    output <- output$output    
    
    ## Call low level function
    .adea <- stepwise(input = input, output = output, orientation = orientation, load.orientation = load.orientation, name = name, direction = direction, verbose = verbose, load.critical = 1, solver = solver)
    
    ## Drop non useful information
    .adea$load.critical <- NULL
    .adea$lp <- NULL
    .adea$steps <- NULL
    .adea$stop.criterion <- NULL
    
    ## Drop decreasing load models
    models <- list()
    .load <- .adea$models[[length(.adea$models)]]$loads$load
    if (length(.adea$models) >= 3) {
        models[[length(.adea$models)]] <- .adea$models[[length(.adea$models)]]
        for (i in length(.adea$models):3) {
            if (!is.null(.adea$models[[i-1]]$loads$load)) {
                if (.load < .adea$models[[i-1]]$loads$load) {
                    .load <- .adea$models[[i-1]]$loads$load
                    models[[i-1]] <- .adea$models[[i-1]]
                } else {
                    .adea$inputnames[i-1] <- ''
                    .adea$outputnames[i-1] <- ''
                    .adea$ninputs[i-1] <- 0
                    .adea$noutputs[i-1] <- 0
                    .adea$nvariables[i-1] <- 0
                    .adea$loads[i-1] <- 0
                    .adea$out[i] <- paste0(.adea$out[i], ', ', .adea$out[i-1])
                    .adea$out[i-1] <- ''
                }
            }
        }
        ## Replace models with new list
        .adea$models <- models
    }

    ## Setup list
    .adea <- list(
        name = name,
        orientation = orientation,
        load.orientation = load.orientation,
        loads = .adea$loads,
        models = .adea$models,
        ninputs = .adea$ninputs,
        noutputs = .adea$noutputs,
        nvariables = .adea$nvariables,
        inputnames = .adea$inputnames,
        outputnames = .adea$outputnames,
        out = .adea$out,
        solver = .adea$solver
    )
    
    ## Set class
    class(.adea) <- 'adeaparametric'

    .adea
}
