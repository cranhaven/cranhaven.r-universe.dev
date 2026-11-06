#' Build adea problem
#'
#' For the given input and output build the adea problem and return it.
#'
#' Note: As this function is mainly for internal use, to avoid unnecessary overload, it does not do an extensive check of the input parameters. Use the higher level adea function instead.
#' 
#' @name roi_solve_adea
#' @aliases roi_solve_adea
#' @keywords internal
#' @inheritParams adea
#' @param solver Any of the available solver that will be used by ROI to solve the optimization problem. Use <code>ROI_installed_solvers()</code> to list them.
#' @param lp Can be a DEA lp or ADEA lp
#' @return list(lp, status, solver, selected, ux, vy, eff)
roi_solve_adea <- function(input, output, eff = NULL, orientation = c('input', 'output'), load.orientation = c('inoutput', 'input', 'output'), solver = 'auto', lp = NULL)
{
    ## Check input and output
    ## err <- adea_check(input, output)
    ## if (err != TRUE) stop(err)
    orientation <- match.arg(orientation)
    ## Initialize values
    ## status <- NULL
    ## ux <- NULL
    ## vy <- NULL
    ## eff <- NULL
    ndmu <- nrow(input)
    ni <- ncol(input)
    no <- ncol(output)
    nio <- ni + no
    
    ## Number of alpha variables
    ## nalpha <- switch(load.orientation,
    ##                 input = ni,
    ##                 output = no,
    ##                 inoutput = nio)

    ## Check if lp is provided if not, build it as first stage
    if (missing(lp) || is.null(lp)) {
        lp <- roi_build_adea(input = input, output = output, orientation = orientation)
    } else {
        ## Paranoid check
        ## cat("roi_solve_adea:roi_solve_adea.R:42: debug: test result ", attr(lp, 'adea', exact = TRUE)$lp == 'roi_dea' || attr(lp, 'adea', exact = TRUE)$lp == 'roi_adea', '\n')
    }

    ## Check if eff vector is provided, else build it
    if (missing(eff) || is.null(eff)) {
        .dea <- roi_solve_dea(input = input, output = output, orientation = orientation, solver = solver, lp = lp)
        eff <- eff_dea(input = input, output = output, orientation = orientation, ux = .dea$ux, vy = .dea$vy)
    }

    ## Check type of given lp
    if (attr(lp, 'adea', exact = TRUE)$lp == 'roi_dea') lp <- roi_build_adea(input = input, output = output, eff = eff, orientation = orientation, load.orientation = load.orientation)

    ##
    ## Solve the model
    ##
    ## Call to solver
    ROIsol <- ROI_solve(lp, solver = solver)
    status <- ROIsol$status$code
    solver <- attr(ROIsol, "meta")$solver
    ## Check return status
    if (status != 0) stop(paste0(gettext('Unable to solve adea model. Solver status is '), status, ' (', ROIsol$status$msg$message, ')'))

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

    list(solver = solver, ux = ux, vy = vy)
}
