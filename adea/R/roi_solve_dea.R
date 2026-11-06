#' Build (standard) DEA problem using ROI infraestructure
#'
#' For the given input and output build and solve standard DEA problem.
#'
#' Unlike the dea function, this function expects input and output to be arrays or data.frames, not vectors.
#'
#' Note: As this function is mainly for internal use, to avoid unnecessary overload, it does not do an extensive check of the input parameters. Use the higher level dea function instead.
#' 
#' @name roi_solve_dea
#' @aliases roi_solve_dea
#' @keywords internal
#' @inheritParams adea
#' @param solver a solver to be used by ROI to solve the DEA optimization problem. Use <code>ROI_installed_solvers()</code> to list them.
#' @return list(lp, solver, ux, vy)
roi_solve_dea <- function(input, output, orientation = c('input', 'output'), solver = 'auto', lp)
{
    ## Check input and output
    ## err <- adea_check(input, output)
    ## if (err != TRUE) stop(err)
    orientation <- match.arg(orientation)
    ## Canonize input and output format
    ## data <- adea_setup(input, output)
    ## input <- data$input
    ## output <- data$output
    
    ## Paranoid check
    ## cat("roi_solve_dea:roi_solve_dea.R:27: debug: test result", attr(lp, 'adea', exact = TRUE)$lp == 'roi_dea', '\n')
    
    ## Check for solver
    if (solver != 'auto') {
        ROIsolver <- paste0('ROI.plugin.', solver)
        if (!(ROIsolver %in% ROI_available_solvers()[, 1])) stop(solver, ' (', ROIsolver, ')', gettext(' is not available.'))
        if (!(ROIsolver %in% ROI_installed_solvers())) stop(solver, ' (', ROIsolver, ')', gettext(' is not installed.'))
    } else {
        ## To ensure that there is at least one solver available
        ROIsolver <- 'ROI.plugin.glpk'
    }
    require(ROIsolver, character.only = TRUE)
    

    ## Initialize values
    ndmu <- nrow(input)
    ni <- ncol(input)
    no <- ncol(output)
    nio <- ni + no

    ## Solve the model
    ## Check for solver
    ## if ((solver != 'auto') && !(ROIsolver %in% ROI_installed_solvers())) stop(solver, gettext(' is not installed.'))
    if (length(ROI_applicable_solvers(lp)) == 0) stop(gettext('There is no an applicable solver installed to solve the DEA problem.'))
    if ((solver != 'auto') && !(solver %in% ROI_applicable_solvers(lp))) stop(solver, gettext(' is not in applicable solver list for DEA problem. Applicable and available solvers are: '), ROI_applicable_solvers(lp))
    ## Call to solver
    ROIsol <- ROI_solve(lp, solver = solver)
    status <- ROI_plugin_solution_status_code(ROIsol)
    solver <- attr(ROIsol, "meta")$solver
    ## Check return status
    if (status != 0) stop(paste0(gettext('Unable to solve DEA model. Solver status is '), status, ' (', ROIsol$status$msg$message, ')'))
    ## Get u and v values
    sol <- ROIsol$solution
    sol <- matrix(sol[seq(1, ndmu * nio)], nrow = ndmu, ncol = nio, byrow = TRUE)
    rownames(sol) <- rownames(input)
    ## Get weights for outputs, named vy for compatibility with Benchmarking package
    vy <- sol[, 1:no, drop = FALSE]
    colnames(vy) <- colnames(output)
    rownames(vy) <- rownames(output)
    ## Get weights for inputs, named ux for compatibility with Benchmarking package
    ux = sol[, (no+1):nio, drop = FALSE]
    colnames(ux) <- colnames(input)
    rownames(ux) <- rownames(input)

    ## Return the list values
    list(solver = solver, ux = ux, vy = vy)
}
