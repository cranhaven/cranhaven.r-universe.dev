#' Build fsdea problem
#'
#' For the given input and output build the feature selection problem and return it.
#'
#' This function implement the feature selection method in the article [Benitez-Pena, S., Bogetoft, P. and Romero Morales, D. *Feature Selection in Data Envelopment Analysis: A Mathematical Optimization approach* Omega, Elsevier BV, **2020**, Vol. 96, pp. 102068](http://www.sciencedirect.com/science/article/pii/S0305048318312131)
#'
#' Note: As this function is mainly for internal use, to avoid unnecessary overload, it does not do an extensive check of the input parameters. Use the higher level fsdea function instead.
#' 
#' @name solve_fsdea
#' @aliases solve_fsdea
#' @keywords internal
#' @inheritParams adea
#' @param ninputs a number which is the number input features, variables, to be selected. Its default value is the number of input variables.
#' @param noutputs a number which is the number output features, variables, to be selected. Its default value is the number of output variables.
#' @param nvariables a number of total features, variables, to be selected. Only if both ninputs and noutputs are omitted. In other case it is ignored.
#' @param solver Any of the available solver that will be used by ROI to solve the optimization problem. Use <code>ROI_installed_solvers()</code> to list them.
#' @return list(status, obj, solver, iselected, oselected, ux, vy)
solve_fsdea <- function(input, output, orientation = c('input', 'output'), ninputs = ncol(input), noutputs = ncol(output), nvariables = ncol(input) + ncol(output), solver = 'auto', lp)
{
    ## Check input and output
    ## err <- adea_check(input, output)
    ## if (err != TRUE) stop(err)
    orientation <- match.arg(orientation)
    
    ## Paranoid check
    ## cat("solve_fsdea:solve_fsdea.R:26: debug: test result", attr(lp, 'adea', exact = TRUE)$lp == 'fsdea', '\n')
    
    ## Initialize values
    ndmu <- nrow(input)
    ni <- ncol(input)
    no <- ncol(output)
    nio <- ni + no
    ncol <- dim(constraints(lp))[2]

    ## Call to solver
    ROIsol <- ROI_solve(lp, solver = solver)
    ##print(unclass(ROIsol))
    status <- ROIsol$status$code
    solver <- attr(ROIsol, "meta")$solver
    ## Check return status
    if (status != 0)
        stop(paste0(gettext('Unable to solve fsdea model. Solver status is '), status, ' (', ROIsol$status$msg$message, ')'))
    ## Get u and v values
    sol <- ROIsol$solution
    sol <- matrix(sol[seq(1, ndmu * nio)], nrow = ndmu, ncol = nio, byrow = TRUE)
    ## debug
    ## rownames(sol) <- rownames(input)
    ## l <- 1
    ## for(u in 1:ndmu)
    ##    for(i in 1:nio) {
    ##        cat(L[l, l]$v , '*', names(ROIsol$solution)[l])
    ##        cat(L[l, i + ndmu * nio]$v, '*', names(ROIsol$solution)[i + ndmu * nio], '<= 0\n')
    ##        cat(names(ROIsol$solution)[l], '=', ROIsol$solution[l], '<= ')
    ##        cat(names(ROIsol$solution)[i + ndmu * nio], '=', ROIsol$solution[i + ndmu * nio])
    ##        cat('\n')
    ##        l <- l +1
    ##    }
    ## Get weights for outputs, named vy for compatibility with Benchmarking package
    vy <- sol[, 1:no, drop = FALSE]
    colnames(vy) <- colnames(output)
    rownames(vy) <- rownames(output)
    ## Get weights for inputs, named ux for compatibility with Benchmarking package
    ux = sol[, (no+1):nio, drop = FALSE]
    colnames(ux) <- colnames(input)
    rownames(ux) <- rownames(input)
    
    ## Get feature selection variables
    oselected <- ROIsol$solution[seq(ncol - nio + 1, ncol - ni)]
    iselected <- ROIsol$solution[seq(ncol - ni + 1, ncol)]

    ## Force weight of nonselected variables to 0
    ux[, iselected < .1] <- 0
    vy[, oselected < .1] <- 0
    
    ## Return the list values
    names(iselected) <- colnames(input)
    names(oselected) <- colnames(output)
    list(status = status, solver = solver, obj = -ROIsol$objval, iselected = iselected, oselected = oselected, ux = ux, vy = vy)
}
