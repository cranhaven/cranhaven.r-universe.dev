#' Selection of an optimal subset of variables for DEA analysis
#'
#' This function returns a list of DEA models by systematically eliminating one variable at a time, following ADEA methodology.
#'
#' This procedure provides a list of all DEA models for all nested sets of variables.
#' In each model, the variable with lowest load is dropped.
#' It's important to note that the load of the new model can be lower than that of the previous one.
#' For more details, please refer to the examples section.
#' 
#' @importFrom graphics lines
#' @importFrom graphics plot
#' @inheritParams adea
#' @inheritParams adea_stepwise
#' 
#' @return The function returns an object of the adeaparametric class with the following named members:
#' \itemize{
#' \item name: A label of the model.
#' \item orientation: DEA model orientation ('input' or 'output').
#' \item load.orientation: load DEA model orientation ('input', 'output', or 'inoutput').
#' \item loads: Load of each model.
#' \item models: List of all adea models.
#' \item ninputs: Number of input variables in each model.
#' \item noutputs: Number of output variables in each model.
#' \item nvariables: Number of variables in each model.
#' \item inputnames: Names of input variables in each model.
#' \item outputnames: Names of output variables in each model.
#' \item out: Variable dropped at each step.
#' \item solver: The solver used for resolving the optimization problem.
#' }
#' @seealso \code{\link{adea_parametric}}
#' @examples
#' # Load data
#' data('cardealers4')
#'
#' # Define input and output
#' input <- cardealers4[, c('Employees', 'Depreciation')]
#' output <- cardealers4[, c('CarsSold', 'WorkOrders')]
#' 
#' # Compute all dea models in hierarchical manner
#' sol.ah <- adea_hierarchical(input, output)
#'
#' # Print the result
#' sol.ah
#' #       Loads nEfficients nVariables nInputs nOutputs                  Inputs              Outputs
#' # 4 0.6666667           2          4       2        2 Employees, Depreciation CarsSold, WorkOrders
#' # 3 0.9575672           2          3       1        2            Depreciation CarsSold, WorkOrders
#' # 2 1.0000000           1          2       1        1            Depreciation             CarsSold 
#' 
#' # Summary the model with 3 variables
#' summary(sol.ah$models[[3]])
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
#' 
#' # Get efficiencies for the model with 3 variables
#' sol.ah$models[[3]]$eff
#' # Dealer A  Dealer B  Dealer C  Dealer D  Dealer E  Dealer F
#' # 0.9915929 1.0000000 0.8928571 0.8653846 1.0000000 0.6515044 
#' @export
adea_hierarchical <- function(input, output, orientation = c('input', 'output'), load.orientation = c('inoutput', 'input', 'output'), name = '', direction = c('backward', 'backward/input', 'backward/output'), solver = 'auto', verbose = 0)
{
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
    .adea <- stepwise(input = input, output = output, orientation = orientation, load.orientation = load.orientation, name = name, direction = direction, load.critical = 1, solver = solver, verbose = verbose)
    
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
    class(.adea) <- 'adeahierarchical'
    .adea
}
