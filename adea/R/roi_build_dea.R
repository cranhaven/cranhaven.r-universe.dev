#' Build (standard) DEA problem using ROI infraestructure
#'
#' For the given input and output, construct the corresponding instance of the DEA problem.
#'
#' Unlike the dea function, this function expects input and output to be arrays or data.frames, not vectors.
#'
#' Note: As this function is mainly for internal use, to avoid unnecessary overload, it does not do an extensive check of the input parameters. Use the higher level dea function instead.
#' 
#' @name roi_build_dea
#' @aliases roi_build_dea
#' @keywords internal
#' @inheritParams adea
#' @return lp
roi_build_dea <- function(input, output, orientation)
{
    ## Check input and output
    ## err <- adea_check(input, output)
    ## if (err != TRUE) stop(err)
    orientation <- match.arg(orientation, c('input', 'output'))
    ## Canonize input and output format
    ## data <- adea_setup(input, output)
    ## input <- data$input
    ## output <- data$output

    ## Initialize values
    ndmu <- nrow(input)
    ni <- ncol(input)
    no <- ncol(output)
    nio <- ni + no
    nrow <- (ndmu + 1) * ndmu
    ncol <- nio * ndmu

    ## Debug
    ## cat('roi_solve_dea:roi_solve_dea.R:34: ni:  ', ni, '\n')
    ## cat('roi_solve_dea:roi_solve_dea.R:35: no:  ', no, '\n')
    ## cat('roi_solve_dea:roi_solve_dea.R:36: ndmu:', ndmu, '\n')
    ## cat('roi_solve_dea:roi_solve_dea.R:37: nrow:', nrow, '\n')
    ## cat('roi_solve_dea:roi_solve_dea.R:38: ncol:', ncol, '\n')
    ## Build lp_matrix as sparse matrix as in package slam (Matrix seems not work here)
    L <- simple_triplet_zero_matrix(nrow = nrow, ncol = ncol)
    ## Columns 
    ## * Weights for outputs of first DMU
    ## * Weights for inputs of second DMU
    ## * Same for following DMU's
    ## Build constraints:
    ## * First block normalization constraints
    ## * Second block efficiencies
    ## RHS
    rhs <- rep(0, nrow)
    rhs[1:ndmu] <- 1
    ## Constraint type
    dir <- rep('<=', nrow)
    dir[1:ndmu] <- '=='
    ## Build efficiency conditions
    ## Build output, vy, part matrix, i.e. the first ndmu x no columns
    for (u in 1:ndmu)
        for (j in 1:no)
            L[seq(ndmu + u, ndmu * (ndmu + 1), by = ndmu), (u - 1) * nio + j] <- output[, j]
    ## Build input, ux, part matrix, i.e. the last ndmu x ni columns
    for (u in 1:ndmu)
        for (i in 1:ni)
            L[seq(ndmu + u, ndmu * (ndmu + 1), by = ndmu), (u - 1) * nio + i + no] <- -input[, i]
    ## Build normalisations
    if (orientation == 'input') {
        for (u in 1:ndmu)
            for (i in 1:ni)
                L[u, (u - 1) * nio + i + no] <- input[u, i]
    }
    if (orientation == 'output') {
        for (u in 1:ndmu)
            for (j in 1:no)
                L[u, (u - 1) * nio + j] <- output[u, j]
    }
    ## Build constraints
    varnames <- c()
    for(i in 1:ndmu) varnames <- c(varnames, paste0('v_', i, '_', 1:no), paste0('u_', i, '_', 1:ni))
    constraints <- L_constraint(L = L, dir = dir, rhs = rhs, names = varnames)

    ## Build objective (Standard DEA)
    if (orientation == 'input') {
        objective <- -c(rbind(t(output), matrix(0, nrow = ni, ncol = ndmu)))
    } else if  (orientation == 'output') {
        objective <- c(rbind(matrix(0, nrow = no, ncol = ndmu), t(input)))
    } else {
        stop('orientation:', orientation, 'not implemented yet.\n')
    }

    ## Build problem
    lp <- OP(objective = objective, constraints = constraints, maximum = FALSE)
    attr(lp, 'adea')$lp <- 'roi_dea'
    
    ## Write model to file
    ## ROI_write(lp, file = '/tmp/roi_solve_dea.mod', type = 'lp_cplex')

    ## Return
    lp
}
