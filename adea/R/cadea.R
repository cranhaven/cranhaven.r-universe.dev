#' Make a constrained ADEA analysis
#'
#' This function calculates an efficiency score for each DMU and computes a load for each variable within the current model.
#' However, it's essential to note that the loads or contributions of input or output variables are subject to constraints within specified values.
#' As a result, the efficiencies of DMUs may deviate from those obtained in regular DEA or ADEA models.
#'
#' A variable load is a numerical value between 0 and 1, with 0 signifying that the variable's contribution to the efficiency calculations is negligible.
#' In an ideal scenario, each input or output variable would have a load of 1.
#' This model enforces input and output weights in a manner that ensures the final variable loads fall within specified values.
#'
#' In a raw variable selection procedure, it's possible to inadvertently remove a variable from a DEA model, leading to a non-natural model.
#' In other cases, there may be political or tactical reasons for certain variables to be considered.
#' In a standard DEA model, the weights associated with these variables might be reduced to very small values, effectively rendering their contributions nearly negligible.
#'
#' The constraints for variable loads in these models prevent such scenarios by ensuring that the contributions of variables reach the desired levels.
#' This maintains the integrity and significance of variables in the DEA model.
#'
#' For more information about loads or ADEA model see \code{\link{adea}}
#' 
#' @note If the given limits are too narrow, then the model is infeasible, which will result in an error.
#'
#' @inheritParams adea
#' @param load.min A numeric value or vector giving minimum values for loads.
#' Values for \code{load.min} must belongs to [0, 1).
#' @param load.max A numeric value or vector giving maximum values for loads.
#' Values for \code{load.max} must be greater than 1.
#' If \code{load.min} or \code{load.max} are vectors then its length must be the same as the number of loads to compute.
#' This means, number of inputs when \code{load.orientation} is input, number of outputs when \code{load.orientation} is output, and the sum of both when \code{load.orientation} is inoutput.
#' @return The function returns a cadea object with the follosing named members:
#' \itemize{
#' \item name: A label of the model
#' \item orientation: DEA model orientation 'input' or 'output'
#' \item load.orientation: load DEA model orientation 'input', 'output', or 'inoutput'
#' \item inputnames: Variable input names
#' \item outputnames: Variable output names
#' \item eff: is a vector with DMU's scores
#' \item loads: A list with all information about loads:
#'   \itemize{
#'     \item load: The lowest load, which is the load of the ADEA model
#'     \item input: A vector with loads of input variables
#'     \item iinput: Index of input variable that reach the load of the model
#'     \item output: A vector with loads of output variables
#'     \item ioutput: Index of output variable that reach the load of the model
#'     \item load.min: Miniminum allowed value for variable loads when load.orientation is ioutput, for input variable loads when load.orientation is input, and for output variable loads when load.orientation is output.
#'     \item load.max: Mamixum allowed value for all, input or output variable loads as for load.min.
#'   }
#' \item ux: A set of weights for inputs
#' \item vy: A set of weights for output
#' \item iterations: Number of iterations to reach the stop rule
#' \item vinput: Standardized virtual input dividing by the sum of the weights, see [Costa2006] in \code{\link{adea-package}}.
#' \item voutput: Standardized virtual output dividing by the sum of the weights, see [Costa2006] in \code{\link{adea-package}}
#' efficiency scores, one set of weights for inputs and other for outputs, number of efficient units, variable loads and model load.
#' \item solver: The solver used for the resolution of the optimization problem
#' }
#' @seealso \code{\link{adea-package}}.
#' @examples
#' data('cardealers4')
#' input <- cardealers4[, c('Employees', 'Depreciation')]
#' output <- cardealers4[, c('CarsSold', 'WorkOrders')]
#' 
#' # Compute adea model
#' model <- adea(input, output)
#' model
#' # Dealer A  Dealer B  Dealer C  Dealer D  Dealer E  Dealer F
#' # 0.9915929 1.0000000 0.8928571 0.8653846 1.0000000 0.6515044
#' 
#' # Get input variable loads
#' model$loads$input
#' # Employees Depreciation
#' # 0.6666667    1.3333333
#' # Get output variable loads
#' model$loads$output
#' #  CarsSold WorkOrders
#' # 1.2663476  0.7336524 
#' 
#' # Compute a constrained adea model to force load between .8 and 1.5
#' cmodel <- cadea(input, output, load.min = .8, load.max = 1.5)
#' cmodel
#' #  Dealer A  Dealer B  Dealer C  Dealer D  Dealer E  Dealer F 
#' # 0.9915929 1.0000000 0.8928571 0.8653846 1.0000000 0.5920826 
#' 
#' # Get loads
#' cmodel$loads
#' # $load
#' # [1] 0.8
#' # $input
#' # Employees Depreciation
#' #       0.8          1.2
#' # $iinput
#' # Employees
#' # 1 
#' # $output
#' # CarsSold WorkOrders
#' #      1.2        0.8
#' # $ioutput
#' # WorkOrders
#' # 2
#' # $load.min
#' # [1] 0.8 0.8 0.8 0.8
#' # $load.max
#' # [1] 1.5 1.5 1.5 1.5
#' # See differences of efficiencies in both models
#' model$eff - cmodel$eff
#' #      Dealer A      Dealer B      Dealer C      Dealer D      Dealer E      Dealer F
#' # -2.220446e-16 -1.332268e-15 -1.110223e-16  2.220446e-16 -1.110223e-16  5.942183e-02 
#' @export
cadea <- function(input, output, orientation = c('input', 'output'), load.orientation = c('inoutput', 'input', 'output'), name = '', load.min, load.max, solver = 'lpsolve')
{
    ## Check input and output
    err <- adea_check(input = input, output = output, ux = NULL, vy = NULL, eff = NULL)
    if (!isTRUE(err)) stop(err)

    ## Check other parameters
    orientation <- match.arg(orientation)
    load.orientation <- match.arg(load.orientation)
    
    ## Canonize input and output format
    dat <- adea_setup(input, output)
    input <- dat$input
    output <- dat$output

    ## Compute total length of load.min and load.max vectors
    n <- switch(load.orientation,
                input = ncol(input),
                output = ncol(output),
                inoutput = ncol(input) + ncol(output)
                )
    
    ## Check load.min and load.max
    ## Check load.min and expand it if necessary
    if (!is.numeric(load.min)) stop(paste('cadea:', gettext('load.min value is not numeric')))
    if ((length(load.min) == 1) && (n > 1)) load.min <- rep(load.min, n)
    if (length(load.min) != n) stop(paste('cadea:', gettext('The length of load.min does not match')))
    ## Check load.max
    if (!is.numeric(load.max)) stop(paste('cadea:', gettext('load.max value is not numeric')))

    if ((length(load.max) == 1) && (n > 1)) load.max <- rep(load.max, n)
    if(length(load.max) != n) stop(paste('cadea:', gettext('The length of load.max does not match')))
    ## Check load.min values
    if (any(load.min < 0) || any(load.min >= 1)) stop(paste("cadea: load.min", gettext('is not a numeric value or vector in [0, 1)')))
    ## Check load.max values
    if (any(load.max < 1)) stop(paste("cadea: load.max", gettext('is not a numeric value or vector greater or equal to 1')))
    ## Compareload.min and load.max
    if (any(load.max - load.min < 0)) stop(paste("cadea:", gettext('load.max values must be greater than or equal load.min values')))
    
    ## Call to low level function
    cadea_(input = input, output = output, orientation = orientation, load.orientation = load.orientation, name = name, load.min = load.min, load.max = load.max, solver = solver) 
}

## Same as cadea but without exhaustive input check, for internal use only
cadea_ <- function(input, output, orientation, load.orientation, name, load.min, load.max, solver)
{
    ## Do an standard adea
    .adea <- adea_(input = input, output = output, orientation = orientation, load.orientation = load.orientation, name = name, solver = solver)

    ## Add bounds to adea program and solve again
    .cadea <- lp_solve_cadea(input = input, output = output, eff = .adea$eff, orientation = orientation, load.orientation = load.orientation, load.min = load.min, load.max = load.max, solve = TRUE, lp = .adea$lp)

    ## Compute loads
    loads <- adea_loads(input = input, output = output, ux = .cadea$ux, vy = .cadea$vy, load.orientation = load.orientation)

    ## Store load ranges
    loads$load.min <- load.min
    loads$load.max <- load.max
    
    ## Compute and store virtual input and output as described in @references A new approach to the bi-dimensional representation of the DEA efficient frontier with multiple inputs and outputs
    virtuals <- virtual_dea(input = input, output = output, orientation = orientation, ux = .cadea$ux, vy = .cadea$vy)
    ## Set up object to return
    .cadea <- list(
        'name' = name,
        'orientation' = orientation,
        'load.orientation' = load.orientation,
        'inputnames' = colnames(input),
        'outputnames' = colnames(output),
        'eff' = .cadea$eff,
        'loads' = loads,
        'ux' = .cadea$ux,
        'vy' = .cadea$vy,
        'iterations' = .cadea$iterations,
        'vinput' = virtuals$vinput,
        'voutput' = virtuals$voutput,
        'solver' = .cadea$solver)

    ## Change the class of the object
    class(.cadea) <- 'cadea'
    .cadea
}
