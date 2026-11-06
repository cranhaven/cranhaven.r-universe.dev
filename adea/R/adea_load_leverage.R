#' Search for leverage units (DMU's) with a greater impact on load levels in DEA analysis
#'
#' Search for leverage units (DMU's) with a greater impact on load levels in DEA analysis.
#'
#' A leverage unit is a DMU that significantly alters the results of the current procedure, in this case, a DMU that produce a large change in variable loads.
#' 
#' @inheritParams adea
#' @param load.diff Minimum difference in load to consider a subset of DMUs as a leverage one
#' @param ndel Maximum number of units to drop out in each try.
#' @param nmax Maximum number of DMU sets to include in results. 0 for no limit.
#' @return The function returns a list with the following named members:
#' \itemize{
#' \item loads: Load of each model after removing the corresponding DMUs
#' \item loads.diff: For each model the difference between its load and the original one
#' \item dmu.indexs: Index of DMUs removed in each model
#' }
#' @examples
#' data('cardealers4')
#' input <- cardealers4[, c('Employees', 'Depreciation')]
#' output <- cardealers4[, c('CarsSold', 'WorkOrders')]
#' adea_load_leverage(input, output, ndel = 2)
#' #         load  load.diff DMUs
#' # 1  1.0000000 0.33333333 1, 6
#' # 2  1.0000000 0.33333333 3, 4
#' # 3  1.0000000 0.33333333 2, 3
#' # 4  1.0000000 0.33333333 2, 5
#' # 5  1.0000000 0.33333333 4, 6
#' # 6  1.0000000 0.33333333    2
#' # 7  1.0000000 0.33333333 1, 4
#' # 8  1.0000000 0.33333333 2, 6
#' # 9  1.0000000 0.33333333 1, 2
#' # 10 0.9635628 0.29689609 2, 4
#' # 11 0.8743243 0.20765766 5, 6
#' # 12 0.8479940 0.18132736 1, 3
#' # 13 0.8420551 0.17538843 3, 6
#' # 14 0.8243243 0.15765766 1, 5
#' # 15 0.8000000 0.13333333    6
#' # 16 0.8000000 0.13333333    4
#' # 17 0.8000000 0.13333333    1
#' # 18 0.8000000 0.13333333    3
#' # 19 0.7461771 0.07951041 3, 5
#' # 20 0.7358231 0.06915643    5
#' 
#' @note This function has to solve a large number of large linear programs that grows with DMUs. So computation time required may be very large, be patient.
#' @importFrom combinat combn
#' @importFrom combinat permn
#' @importFrom parallel parApply
#' @importFrom parallel stopCluster
#' @importFrom parallelly availableCores
#' @importFrom parallelly makeClusterPSOCK
#' @export
adea_load_leverage <- function(input, output, orientation = c('input', 'output'), load.orientation = c('inoutput', 'input', 'output'), load.diff = .05, ndel = 1, nmax = 0, solver = 'auto')
{
    ## .output <- function() {
    ##    cat('n:', next.index, ' ')
    ##    cat(paste0(gettext('loads'), ':'), .loads[next.index])
    ##    cat(paste0(gettext('loads.diff'), ':'), .loads.diff[next.index])
    ##    cat(paste0(gettext('index'), ': {'), paste0(dmu.indexs[next.index, 1:k], collapse = ', '), '} ')
    ##    cat(paste0(gettext('names'), ': {'), paste0(rownames(input)[dmu.indexs[next.index, 1:k]], collapse = ', '), '}')
    ##    cat('\n')
    ##}

    ## Check input
    orientation <- match.arg(orientation)
    load.orientation <- match.arg(load.orientation)
    if (!is.numeric(nmax) || nmax < 0) stop(gettext('adea_load_leverage: nmax must be numeric >= 0'))

    ## Check input and output
    err <- adea_check(input = input, output = output)
    if (!isTRUE(err)) stop(err)

    ## Check ndel
    ndel <- as.numeric(ndel)
    if (is.na(ndel) || ndel < 1) stop('The number of units to drop (ndel) must be an integer number at least 1. ')

    ## Normalise input
    input <- adea_setup(input, output)
    output <- input$output
    input <- input$input

    ##
    ## Compute initial model
    ## This instruction has been replace by the following block
    ## iload.level <- adea_(input, output, orientation = orientation, load.orientation = load.orientation, solver = solver, name = '')$loads$load
    ## cat("debug:adea_load_leverage.R:84: iload.level: ", iload.level, "\n")

    ## Compute initial model
    .adea <- roi_solve_adea(input, output, orientation = orientation, load.orientation = load.orientation, solver = solver)

    ## Compute initial load level
    iload.level <- adea_loads(input, output, ux = .adea$ux, vy = .adea$vy, load.orientation = load.orientation)$load
    ## cat("debug:adea_load_leverage.R:81: iload.level: ",  iload.level, "\n")

    ## Intialize variables
    loads <- numeric(0)
    loads.diff <- numeric(0)
    dmu.indexs <- matrix(ncol = 0, nrow = ndel)

    ## Create a cluster
    supportsMulticore <- parallelly::supportsMulticore()
    if (supportsMulticore) cl <- parallel::makeCluster(availableCores(omit = 1), type = "FORK")

    ## Main loop in size
    for(k in 1:ndel) {
        ## Compute combinations
        .combn <- combn(1:nrow(input), k)
        ## Compute loads
        if (supportsMulticore) {
            .loads <- parApply(cl = cl, .combn, MARGIN = 2, FUN = adea_load_leverage_i, input = input, output = output, orientation = orientation, load.orientation = load.orientation, solver = solver)
       } else {
           .loads <- apply(.combn, MARGIN = 2, FUN = adea_load_leverage_i, input = input, output = output, orientation = orientation, load.orientation = load.orientation, solver = solver)
        }
        ## Compute differences in loads
        .loads.diff <- abs(iload.level - .loads)
        ## Compute index
        .index.diff <- (.loads.diff > load.diff)
        ## Filter and append the results
        if (sum(.index.diff) > 0) {
            loads <- c(loads, .loads[.index.diff])
            loads.diff <- c(loads.diff, .loads.diff[.index.diff])
            .dmu.indexs <- matrix(NA, ncol = sum(.index.diff), nrow = ndel)
            .dmu.indexs[1:k, ] <- .combn[, .index.diff]
            dmu.indexs <- cbind(dmu.indexs, .dmu.indexs)
        }
    }

    ## Stop the cluster
    if (supportsMulticore) stopCluster(cl)

    ## Return results
    if (length(loads.diff) > 1) {
        index <- sort(loads.diff, decreasing = TRUE, index.return = TRUE)
        index <- index$ix
        if (nmax > 1 && nmax < length(index)) index <- index[1:nmax]
        loads <- loads[index]
        loads.diff <- loads.diff[index]
        dmu.indexs <- dmu.indexs[, index, drop = FALSE]
    }
    dmu.indexs <- t(dmu.indexs)
    if (length(loads.diff) == 0) dmu.indexs <- numeric(0)
    
    ## Build return list
    result <- list(loads = loads, loads.diff = loads.diff, dmu.indexs = dmu.indexs)
    class(result) <- 'adealoadleverage'
    result
}

## Auxiliary function for adea_load_leverage
## eindex = Set of DMU to exclude
adea_load_leverage_i <- function(eindex, input, output, orientation, load.orientation, solver)
{
    inputi <- input[-eindex,, drop = FALSE]
    outputi <- output[-eindex,, drop = FALSE]
    .adea <- roi_solve_adea(inputi, outputi, orientation = orientation, load.orientation = load.orientation, solver = solver)
    adea_loads(inputi, outputi, ux = .adea$ux, vy = .adea$vy, load.orientation = load.orientation)$load
}
