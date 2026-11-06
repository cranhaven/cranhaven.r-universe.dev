#' Feature Selection in Data Envelopment Analysis with Mathematical Programming
#'
#' Data Envelopment Analysis (DEA) calculates a relative efficiency score for a set of Decision Making Units (DMUs) by comparing one unit with others.
#'
#' Each DMU transforms inputs into outputs.
#' The set of inputs and outputs is the same for all the DMUs, but not their quantities.
#' 
#' One of the fundamental steps in the application of data envelopment analysis is the choice of variables to include in the model.
#' One of the methods proposed for this is what is known as the feature selection method.
#' This method constructs a linear programming problem to maximize some objective function related to the dmu efficiencies.
#' This function implements the feature selection method in the article [Benitez-Pena, S., Bogetoft, P. and Romero Morales, D. *Feature Selection in Data Envelopment Analysis: A Mathematical Optimization approach* Omega, Elsevier BV, **2020**, Vol. 96, pp. 102068](http://www.sciencedirect.com/science/article/pii/S0305048318312131)
#'
#' This function, in the case of input orientation, maximize the sum of all efficiencies, while in the output orientation case, the goal is to minimize this sum. 
#' Once the relevant variables are selected, the function calculates the relative efficiency scores for each Decision Making Unit (DMU) and determines the weights for all input and output variables within the model.
#'
#' @name fsdea
#' @aliases fsdea
#' @concept Data Envelopment Analysis
#' @concept Feature selection
#' @concept Variable selection
#' @inheritParams adea
#' @param ninputs Number of input features (variables) to be selected.
#' Default is the number of input variables.
#' @param noutputs Number of output features (variables) to be selected.
#' Default is the number of output variables.
#' @param nvariables Number of total features (variables) to be selected, only applicable when both ninputs and noutputs are omitted.
#' Default is the number of input plus output variables.
#' @param solver The solver to be used by ROI to solve the DEA optimization problem.
#' The solver must be installed and capable of solving mixed integer linear programming problems.
#' Default is "auto."
#' Use `ROI_installed_solvers()` to list available solvers.
#' @return This function return a fsdea class object with the following named members:
#' \itemize{
#' \item orientation: DEA model orientation.
#' \item name: A label of the model.
#' \item ninputs: Number of inputs to be selected.
#' \item noutputs: Number of outputs to be selected.
#' \item nvariables: Number of total variables to be selected.
#' \item inputnames: Names of input variables.
#' \item outputnames: Names of output variables.
#' \item eff: A vector with DMU scores.
#' \item ux: A set of weights for input variables.
#' \item vy: A set of weights for output variables.
#' \item obj: Optimal value of the objective function in the optimization problem.
#' \item iselected: A vector of zeros and ones indicating the selected input variables.
#' \item oselected: A vector of zeros and ones indicating the selected output variables.
#' \item niselected: Number of input selected variables.
#' \item noselected: Number of output selected variables.
#' \item nvselected: Number of selected variables.
#' \item vinput: Standardized virtual input divided by the sum of the weights, see [Costa2006] in \code{\link{adea-package}}.
#' \item voutput: Standardized virtual output divided by the sum of the weights, see [Costa2006] in \code{\link{adea-package}}.
#' \item solver: The solver used for the resolution of the optimization problem.
#' }
#' @seealso \code{\link{adea-package}}.
#' @examples
#' data('cardealers4')
#' input <- cardealers4[, c('Employees', 'Depreciation')]
#' output <- cardealers4[, c('CarsSold', 'WorkOrders')]
#' 
#' # Compute DEA model selecting at most 1 output
#' model1o <- fsdea(input, output, noutputs = 1)
#' model1o
#' #   Dealer A  Dealer B  Dealer C  Dealer D  Dealer E  Dealer F 
#' #  0.7875000 0.7500000 0.3000000 0.8653846 1.0000000 0.5400000 
#' #  Selected inputs : Depreciation
#' #  Selected outputs: CarsSold
#'
#' # Compute DEA model selecting at most 1 input
#' model1i <- fsdea(input, output, ninputs = 1)
#' model1i
#' # Dealer A  Dealer B  Dealer C  Dealer D  Dealer E  Dealer F
#' # 0.9915929 1.0000000 0.8928571 0.8653846 1.0000000 0.6515044
#' # Selected inputs : Depreciation
#' # Selected outputs: CarsSold, WorkOrders
#' 
#' # Compute DEA model selecting at most 3 variables
#' model3v <- fsdea(input, output, nvariables = 3)
#' model3v
#' # Dealer A  Dealer B  Dealer C  Dealer D  Dealer E  Dealer F
#' # 0.9915929 1.0000000 0.8928571 0.8653846 1.0000000 0.6515044
#' # Selected inputs : Depreciation
#' # Selected outputs: CarsSold, WorkOrders
#' 
#' @export
fsdea <- function(input, output, orientation = c('input', 'output'), name = '', ninputs = ncol(input), noutputs = ncol(output), nvariables = ncol(input) + ncol(output), solver = 'auto')
{
    ## Check input and output
    err <- adea_check(input = input, output = output, ux = NULL, vy = NULL, eff = NULL)
    if (!isTRUE(err)) stop(err)

    ## Check orientation
    orientation <- match.arg(orientation)

    ## Canonize input and output format
    output <- adea_setup(input, output)
    input <- output$input
    output <- output$output

    ## Initialize values
    ni <- ncol(input)
    no <- ncol(output)
    nio <- ni + no

    ## If not missed check ninputs values
    if (!missing(ninputs)) {
        if (is.na(as.integer(ninputs))) stop('ninputs is not numeric')
        if (as.integer(ninputs) != as.numeric(ninputs)) stop('ninputs is not an integer')
        ninputs <- as.integer(ninputs)
        if (ninputs < 1) stop('ninputs must be at least 1')
        if (ninputs > ni) stop('ninputs must be at most the number of input variables')
    }
    ## If not missed check noutputs values
    if (!missing(noutputs)) {
        if (is.na(as.integer(noutputs))) stop('noutputs is not numeric')
        if (as.integer(noutputs) != as.numeric(noutputs)) stop('noutputs is not an integer')
        noutputs <- as.integer(noutputs)
        if (noutputs < 1) stop('noutputs must be at least 1')
        if (noutputs > no) stop('noutputs must be at most the number of output variables')
    }
    ## If not missed check nvariables values
    if (missing(ninputs) && missing(noutputs) && !missing(nvariables)) {
        if (is.na(as.integer(nvariables))) stop('nvariables is not numeric')
        if (as.integer(nvariables) != as.numeric(nvariables)) stop('nvariables is not an integer')
        nvariables <- as.integer(nvariables)
        if (nvariables < 2) stop('nvariables must be at least 2')
        if (nvariables > nio) stop('nvariables must be at most the total number of variables')
    }
    ## Analyze the coherence of the values
    if (missing(ninputs) && !missing(noutputs) && !missing(nvariables)) {
        ninputs < nvariables - noutputs
        if (ninputs < 1) stop('nvariables must be at least ninputs plus 1')
        if (ninputs > ni) stop('nvariables must be at most ninputs plus the noutputs')
        }
    if (!missing(ninputs) && missing(noutputs) && !missing(nvariables)) {
        noutputs < nvariables - ninputs
        if (noutputs < 1) stop('nvariables must be at least noutputs plus 1')
        if (noutputs > ni) stop('nvariables must be at most noutputs plus the ninputs')
    }
    if ((!missing(ninputs) || !missing(noutputs)) && missing(nvariables)) {
        nvariables <- ninputs + noutputs
    }

    ## Canonize input and output format
    data <- adea_setup(input, output)
    input <- data$input
    output <- data$output
    
    ## Build fsdea problem
    lp <- build_fsdea(input = input, output = output, orientation = orientation, ninputs = ninputs, noutputs = noutputs, nvariables = nvariables)

    ## Solve fsdea model
    .fsdea <- solve_fsdea(input = input, output = output, orientation = orientation, ninputs = ninputs, noutputs = noutputs, nvariables = nvariables, solver = solver, lp = lp)
    
    ## Compute scores
    eff <- eff_dea(input = input, output = output, orientation = orientation, ux = .fsdea$ux, vy = .fsdea$vy)

    ## Numerical inestability check comparing objval with efficiencies
    if (abs(sum(eff) - .fsdea$obj) > 1.e-6) stop('Objetive function and efficiencies does not match this can be due to a numerical issue.')

    ## Compute and store virtual input and output as described in @references A new approach to the bi-dimensional representation of the DEA efficient frontier with multiple inputs and outputs
    virtuals <- virtual_dea(input = input, output = output, orientation = orientation, ux = .fsdea$ux, vy = .fsdea$vy)

    ## Setup object to return
    .fsdea <- list(
        'name' = name,
        'orientation' = orientation,
        'ninputs' = ninputs,
        'noutputs' = noutputs,
        'nvariables' = nvariables,
        'inputnames' = colnames(input),
        'outputnames' = colnames(output),
        'eff' = eff,
        'ux' = .fsdea$ux,
        'vy' = .fsdea$vy,
        'obj' = .fsdea$obj,
        'iselected' = .fsdea$iselected,
        'oselected' = .fsdea$oselected,
        'niselected' = sum(.fsdea$iselected),
        'noselected' = sum(.fsdea$oselected),
        'nvselected' = sum(.fsdea$iselected) + sum(.fsdea$oselected),
        'vinput' = virtuals$vinput,
        'voutput' = virtuals$voutput,
        'solver' = .fsdea$solver
    )
    ## Change the class of the object
    class(.fsdea) <- 'fsdea'
    .fsdea
}
