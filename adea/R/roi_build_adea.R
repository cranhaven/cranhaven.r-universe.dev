#' Build adea problem
#'
#' For the given input and output build the adea problem and return it.
#'
#' Note: As this function is mainly for internal use, to avoid unnecessary overload, it does not do an extensive check of the input parameters. Use the higher level adea function instead.
#' 
#' @name roi_build_adea
#' @aliases roi_build_adea
#' @keywords internal
#' @inheritParams adea
#' @return lp
#' @importFrom slam simple_triplet_zero_matrix
roi_build_adea <- function(input, output, eff = NULL, orientation = c('input', 'output'), load.orientation = c('inoutput', 'input', 'output'), solver = 'auto', lp = NULL)
{
    ## Check input and output
    ## err <- adea_check(input, output)
    ## if (err != TRUE) stop(err)
    orientation <- match.arg(orientation)
    load.orientation <- match.arg(load.orientation)
    
    ## Initialize values
    ndmu <- nrow(input)
    ni <- ncol(input)
    no <- ncol(output)
    nio <- ni + no

    ## Check if lp is provided if not, build as first stage
    if (missing(lp) || is.null(lp)) {
        lp <- roi_build_dea(input = input, output = output, orientation = orientation)
    } else {
        ## Paranoid check
        ## cat("roi_build_adea:roi_build_adea.R:40: debug: test result", attr(lp, 'adea', exact = TRUE)$lp == 'roi_dea', '\n')
    }

    ## Check if eff vector is provided, else build it
    if (missing(eff) || is.null(eff)) {
        .dea <- roi_solve_dea(input = input, output = output, orientation = orientation, solver = solver, lp = lp)
        eff <- eff_dea(input = input, output = output, orientation = orientation, ux = .dea$ux, vy = .dea$vy)
    }
    
    ##
    ## Resize lp problem
    ##
    ## Standard DEA
    ## ncol <- nio * ndmu
    ## ADEA
    ncol <- nio * ndmu + nio + 1
    ## Update lp
    lp$n_of_variables <- ncol
    ## Update objective
    lp$objective$L$ncol <- ncol
    ## Update constraint matrix
    lp$constraints$L$ncol <- ncol
    lp$constraints$names <- c(lp$constraints$names, paste0('l_O_', 1:no), paste0('l_I_', 1:ni), 'l')
    ## Debug
    ## cat('debug:roi_build_adea:roi_build_adea.R:55: ni:   ', ni, '\n')
    ## cat('debug:roi_build_adea:roi_build_adea.R:56: no:   ', no, '\n')
    ## cat('debug:roi_build_adea:roi_build_adea.R:57: ndmu: ', ndmu, '\n')
    ## cat('debug:roi_build_adea:roi_build_adea.R:58: ncol: ', ncol, '\n')
    ## cat('debug:roi_build_adea:roi_build_adea.R:59: o:    ', orientation, '\n')
    ## cat('debug:roi_build_adea:roi_build_adea.R:60: lo:   ', load.orientation, '\n')
    
    ## Columns 
    ## * Weights for outputs of first DMU
    ## * Weights for inputs of second DMU
    ## * ...
    ## * Aditional variables, in this case variable loads and model load
    ## Build constraints:
    ## * First block normalization constraints (from Standard DEA)
    ## * Second block efficiencies (from Standard DEA)
    ## * Third alpha, model load, constraints
    ## * Fourth alphas, load, definition constraints
    
    ## Set ADEA objective function
    lp$objective$L[1,] <- 0
    lp$objective$L[1, ncol] <- -1

    ##
    ## Add constraints to keep the score
    ##
    ## Build lp_matrix as sparse matrix as in package slam, only new rows
    nrow <- ndmu
    L <- simple_triplet_zero_matrix(nrow = nrow, ncol = ncol)
    ## Initialize new rows at rhs
    rhs <- eff
    ## Initialize dir
    dir <- rep('==', nrow)
    ## Add rows for scores constraints
    if (orientation == 'input') {
        for (d in 1:ndmu) {
            ## The following line does not work
            ## L[d, (1:no) + (d-1) * nio + no] <- output[d, ]
            for(o in 1:no) L[d, o + (d-1) * nio] <- output[d, o]
            ## add.constraint(lp, output[d, ], type = '=', rhs = eff[d], ((d-1) * nio + 1):((d -1) * nio + no))
        }
    }
    if (orientation == 'output') {
        for (d in 1:ndmu) {
            ## The following line does not work
            ## L[d, 1:ni + (d-1) * nio] <- output[d, ]
            for(i in 1:ni) L[d, i + (d-1) * nio + no] <- input[d, i]
            ## add.constraint(lp, input[i, ], type = '=', rhs = eff[i], ((i-1) * nio + 1 + no):((i -1) * nio + ni + no))
        }
    }
    ## Add new set of constraints
    constraints(lp) <- rbind(constraints(lp), L_constraint(L = L, dir = dir, rhs = rhs))

    ##
    ## Add load definition constraints
    ##
    ## Build lp_matrix
    nrow <- nio
    L <- simple_triplet_zero_matrix(nrow = nrow, ncol = ncol)
    rhs <- rep(0, nio)
    dir <- rep("==", nio)
    ## Add load constraints for outputs
    if ((load.orientation == 'output') || (load.orientation == 'inoutput')) {
        for (i in 1:no)
        {
            xt <- switch(orientation,
                         input = c(output[, i] * no / sum(eff), -1),
                         output = c(output[, i] * no / ndmu, -1)
                         )
            indices <- seq(i, ndmu * nio, by = nio)
            indices <- c(indices, ndmu * nio + i)
            L[i, indices] <- xt
            ##add.constraint(lp, xt, type = '=', rhs = 0, indices = indices)
        }
    }
    ## Add load constraints for inputs
    if ((load.orientation == 'input') || (load.orientation == 'inoutput')) {
        for (i in 1:ni)
        {
            xt <- switch(orientation,
                         input = c(input[, i] * ni / ndmu, -1),
                         output = c(input[, i] * ni / sum(eff), -1)
                         )
            indices <- seq(no + i, ndmu * nio, by = nio)
            indices <- c(indices, ndmu * nio + no + i)
            L[no + i, indices] <- xt
            ##add.constraint(lp, xt, type = '=', rhs = 0, indices = indices)
        }
    }
    ## Debug
    ## m <- as.matrix(L)
    ## colnames(m) <- lp$constraints$names
    ## print(m)
    ## Add new set of constraints
    constraints(lp) <- rbind(constraints(lp), L_constraint(L = L, dir = dir, rhs = rhs))

    ##
    ## Add load constraints (l_O^o <= l, l_I^i <= l)
    ##
    ## Build lp_matrix
    nrow <- nio
    L <- simple_triplet_zero_matrix(nrow = nrow, ncol = ncol)
    rhs <- rep(0, nrow)
    dir <- rep('<=', nrow)

    if ((load.orientation == 'output') || (load.orientation == 'inoutput')) {
        L[1:no, ncol] <- 1
        L[1:no, ndmu * nio + 1:no] <- -diag(no)
    }
    if ((load.orientation == 'input') || (load.orientation == 'inoutput')) {
        L[(no +1):nio, ncol] <- 1
        L[(no + 1):nio, ndmu * nio + no + 1:ni] <- -diag(ni)
    }
    ## Add new set of constraints
    constraints(lp) <- rbind(constraints(lp), L_constraint(L = L, dir = dir, rhs = rhs))

    ## Set attr
    attr(lp, 'adea')$lp <- 'roi_adea'
    
    ## Return
    lp
}
