#' DEA - Data Envelopment Analysis
#'
#' Data Envelopment Analysis, DEA, computes, for a set of Decision Making Units, DMU, a relative efficiency score, comparing one unit with the others.
#'
#' Each DMU transforms inputs into outputs.
#' The set of inputs and outputs is the same for all the DMUs, but not their quantities.
#'
#' This function computes a relative efficiency score and weights for each input and output variable in the model.
#' All these for each DMU.
#'
#' @name dea
#' @aliases dea
#' @keywords DEA
#' @concept Data Envelopment Analysis
#' @inheritParams adea
#' @return This function return a dea class object with the following named members:
#' \itemize{
#' \item name: A label of the model
#' \item orientation: DEA model orientation 'input' or 'output'
#' \item inputnames: Variable input names
#' \item outputnames: Variable output names
#' \item eff: is a vector with DMU's scores
#' \item ux: A set of weights for inputs
#' \item vy: A set of weights for output
#' \item vinput: Standardized virtual input dividing by the sum of the weights, see [Costa2006] in \code{\link{adea-package}}.
#' \item voutput: Standardized virtual output dividing by the sum of the weights, see [Costa2006] in \code{\link{adea-package}}
#' \item solver: The solver used for the resolution of the optimization problem
#' }
#' @seealso \code{\link{adea-package}}.
#' @examples
#' # Load data
#' data('cardealers4')
#'
#' # Define input and output
#' input <- cardealers4[, c('Employees', 'Depreciation')]
#' output <- cardealers4[, c('CarsSold', 'WorkOrders')]
#'
#' # Compute dea model
#' model <- dea(input, output, name = 'DEA for cardealers4 dataset')
#'
#' # Print DMU efficiencies
#' model
#' # Dealer A  Dealer B  Dealer C  Dealer D  Dealer E  Dealer F
#' # 0.9915929 1.0000000 0.8928571 0.8653846 1.0000000 0.6515044
#'
#' # Summarize the model and print aditional information
#' summary(model)
#' # Model name   DEA for cardealers4 dataset
#' # Orientation                        input
#' # Inputs            Employees Depreciation
#' # Outputs              CarsSold WorkOrders
#' # nInputs                                2
#' # nOutputs                               2
#' # nVariables                             4
#' # nEfficients                            2
#' # Eff. Mean               0.90022318389575
#' # Eff. sd                0.135194867030839
#' # Eff. Min.              0.651504424778761
#' # Eff. 1st Qu.           0.872252747252747
#' # Eff. Median            0.942225031605562
#' # Eff. 3rd Qu.           0.997898230088496
#' # Eff. Max.                              1
#' 
#' @export
dea <- function(input, output, orientation = c('input', 'output'), name = '', solver = 'auto')
{
    ## Check input and output
    err <- adea_check(input = input, output = output, ux = NULL, vy = NULL, eff = NULL)
    if (!isTRUE(err)) stop(err)

    ## Check other parameters
    orientation <- match.arg(orientation)

    ## Canonize input and output format
    output <- adea_setup(input, output)
    input <- output$input
    output <- output$output

    ## Solve dea model
    .dea <- roi_dea(input = input, output = output, orientation = orientation, solver = solver)

    ## Compute and store virtual input and output as described in @references A new approach to the bi-dimensional representation of the DEA efficient frontier with multiple inputs and outputs
    virtuals <- virtual_dea(input = input, output = output, orientation = orientation, ux = .dea$ux, vy = .dea$vy)
    
    ## Setup return object
    .dea <- list(
        name = name,
        orientation = orientation,
        inputnames = colnames(input),
        outputnames = colnames(output),
        eff = .dea$eff,
        ux = .dea$ux,
        vy = .dea$vy,
        vinput = virtuals$vinput,
        voutput = virtuals$voutput,
        solver = .dea$solver
    )
    
    ## Change the class of the object
    class(.dea) <- 'dea'
    .dea
}
