#' Compute the variables loads for DEA analysis with specified weights
#'
#' The adea_loads function calculates variable loads for Data Envelopment Analysis (DEA) with user-specified weights for input and output variables.
#'
#' In DEA analysis, even when the efficiency scores remain constants, there is a significant degree of freedom in selecting the sets of weights for input and output variables.
#'
#' Not all sets of weights assign the same importance to the variables.
#' This function allows you to compute the load of each variable based on the provided weights.
#' It also computes load.levels, which represents the minimum values of such loads.
#' 
#' It's important to note that different sets of weights result in different ways to model efficiency.
#' This function does not solve any model.
#' It provides the loads for the specified weights, as described in the theoretical ADEA model.
#' 
#' This function is primarily intended for research and internal use.
#'
#' @inheritParams adea
#' @param ux A matrix of weights for DMUs and input variables.
#' @param vy A matrix of weights for DMUs and output variables.
#' @examples
#' # Load data
#' data('cardealers4')
#' # Define input and output
#' input <- cardealers4[, c('Employees', 'Depreciation')]
#' output <- cardealers4[, c('CarsSold', 'WorkOrders')]
#' # Make dea analysis
#' model <- dea(input, output)
#' # Show results
#' model
#' #   Dealer A  Dealer B  Dealer C  Dealer D  Dealer E  Dealer F
#' # 0.9915929 1.0000000 0.8928571 0.8653846 1.0000000 0.6515044 
#' # Compute loads for such weights
#' adea_loads(input, output, model$ux, model$vy)
#' # $load
#' # [1] 0
#' # $input
#' #    Employees Depreciation 
#' #            0            2 
#' # $iinput
#' # Employees 
#' #         1 
#' # $output
#' #   CarsSold WorkOrders 
#' #  1.1025075  0.8974925 
#' # $ioutput
#' # WorkOrders 
#' 
#' @return Loads for model, input and output variables
#' @export
adea_loads <- function(input, output, ux, vy, load.orientation = c('inoutput', 'input', 'output'))
{
    ## Check input and output
    err <- adea_check(input = input, output = output, ux = ux, vy = vy, eff = NULL)
    if (!isTRUE(err)) stop('adea_loads:', err)
    
    ## Canonize input and output format
    vy <- adea_setup(input = input, output = output, ux = ux, vy = vy)
    input <- vy$input
    output <- vy$output
    ux <- vy$ux
    vy <- vy$vy

    ## Check load.orientation
    load.orientation <- match.arg(load.orientation)

    ## Compute loads: normalised contribution of an input inside all inputs (or output)
    loads <- list()

    loads$load <- Inf
    
    if ((load.orientation == 'input') || (load.orientation == 'inoutput')) {
        loads$input <- ncol(input) * colSums(ux * input) / sum(ux * input)
        names(loads$input) <- colnames(input)
        loads$iinput <- which.min(loads$input)
        loads$load <- loads$input[loads$iinput]
    }
    if ((load.orientation == 'output') || (load.orientation == 'inoutput')) {    
        loads$output <- ncol(output) * colSums(vy * output) / sum(vy * output)
        names(loads$output) <- colnames(output)
        loads$ioutput <- which.min(loads$output)
        loads$load <- min(loads$load, loads$output[loads$ioutput])
    }

    ## Return
    loads
}
