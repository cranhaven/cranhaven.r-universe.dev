#' Build dea problem
#'
#' For the given input and output build the lp_solve problem and return it.
#'
#' @name lp_solve_adea
#' @aliases lp_solve_adea
#' @keywords internal
#' @inheritParams adea
#' @param solve If TRUE then solve dea model
#' @param eff The efficiency scores from dea analysis.
#' @param lp The problem returned from lp_solve.dea or NULL
#' @param max.iterations Maximum number of iterations before stop
#' @return lp adea problem for the given input, output and scores
lp_solve_cadea <- function(input, output, eff = NULL, orientation, load.orientation, load.min, load.max, max.iterations = 25, solve = TRUE, lp = NULL)
{
    ## Setup internal parameter
    load.tolerance <- 1e-6
    err.tolerance <- 1e-6
    
    ## Check input and output
    err <- adea_check(input = input, output = output, eff = eff)
    if (err != TRUE) stop(err)

    ## Check other input parameters
    orientation <- match.arg(orientation, c('input', 'output'))
    load.orientation <- match.arg(load.orientation, c('inoutput', 'input', 'output'))
    
    ## Standardise input and output
    dat <- adea_setup(input, output)
    input <- dat$input
    output <- dat$output

    ## Check if lp is provided if not, build as first stage
    if (missing(lp) || is.null(lp)  || missing(eff) || is.null(eff)) {
        .lp_solve_dea <- suppressWarnings(lp_solve_adea(input = input, output = output, eff = eff, orientation = orientation, load.orientation = load.orientation))
        lp <- .lp_solve_dea$lp
    }

    ## Avoid scaling
    lpSolveAPI::lp.control(lp, scaling = c("none"))

    ## Check load.min and load.max
    if (!is.numeric(load.min)) stop(paste('cadea:', gettext('load.min value is not numeric')))
    if ( (length(load.min) != 1) && (
        ((length(load.min) != ncol(input)) && (load.orientation == 'input')) ||
        ((length(load.min) != ncol(output)) && (load.orientation == 'output')) ||
        ((length(load.min) != ncol(input) + ncol(output)) && (load.orientation == 'inoutput'))
    )) stop(paste('cadea:', gettext('The length of load.min does not match')))
    if (any(load.min < 0) || any(load.min >= 1)) stop(paste("cadea: load.min", gettext('is not a numeric value or vector in [0, 1)')))
    if (!is.numeric(load.max)) stop(paste('cadea:', gettext('load.max value is not numeric')))
    if ( (length(load.max) != 1) && (
        ((length(load.max) != ncol(input)) && (load.orientation == 'input')) ||
        ((length(load.max) != ncol(output)) && (load.orientation == 'output')) ||
        ((length(load.max) != ncol(input) + ncol(output)) && (load.orientation == 'inoutput'))
    )) stop(paste('cadea:', gettext('The length of load.max does not match')))
    if (any(load.max < 1)) stop(paste("cadea: load.max", gettext('is not a numeric value or vector greater or equal to 1')))

    ## if needed, then compute scores
    if (missing(eff) || is.null(eff)) eff <- .lp_solve_dea$eff

    ## Initialise values
    status <- NULL
    ux <- NULL
    vy <- NULL
    ndmu <- .adea_check(input)
    ni <- ncol(input)
    no <- ncol(output)
    nio <- ni + no
    
    ## Number of alpha variables
    nalpha <- switch(load.orientation,
                     input = ni,
                     output = no,
                     inoutput = nio)
    
    ## Compute the size of new problem
    ## The variables are: weights for each DMU + alpha + level
    ncol <- ndmu * nio + nalpha + 1

    ## The rows are: (ndmu + ndmu^2) + (ndmu + nio)
    nrow <- (ndmu + 2) * ndmu + nio


    ## Set dea objective function
    objective <- switch(orientation,
                        input = -c(rbind(t(output), matrix(0, nrow = ni, ncol = ndmu))),
                        output = c(rbind(matrix(0, nrow = no, ncol = ndmu), t(input)))
                        )
    ## All alpha columns has been keep
    objective <- c(objective, rep(0, nio + 1))
    lpSolveAPI::set.objfn(lp, objective)

    ## Remove score constraints
    lpSolveAPI::delete.constraint(lp, (ndmu + 1) * ndmu + 1:ndmu)
    
    ## Remove alpha score constraints
    lpSolveAPI::delete.constraint(lp, (ndmu + 1) * ndmu + 1:nalpha)

    ## Add updated alpha score constraints
    lp_solve_add_alpha_constraints(lp = lp, input = input, output = output, eff = eff, orientation = orientation, load.orientation = load.orientation)
    
    ## Add load constraints as variable bounds
    if (length(load.min) == 1) load.min <- rep(load.min, nio)
    if (length(load.max) == 1) load.max <- rep(load.max, nio)
    if (load.orientation == 'input') lpSolveAPI::set.bounds(lp, lower = load.min[1:ni], upper = load.max[1:ni], columns = ndmu * nio + 1:ni)
    if (load.orientation == 'output') lpSolveAPI::set.bounds(lp, lower = load.min[1:no], upper = load.max[1:no], columns = ndmu * nio + ni + 1:no)
    if (load.orientation == 'inoutput') lpSolveAPI::set.bounds(lp, lower = load.min[1:nio], upper = load.max[1:nio], columns = ndmu * nio + 1:nio)

    ## Solve the model
    if (solve) {

        .continue <- TRUE
        .iteration <- 1
        while(.continue && (.iteration <= max.iterations)) {
            
            ## Store current values of eff
            eff.old <- eff

            ## Store current values of loads
            ## if (exists('.cadea.loads')) .cadea.loads.old <- .cadea.loads
            
            
            ## Update model with efficiencies
            ## Remove old score alpha constraints
            lpSolveAPI::delete.constraint(lp, (ndmu + 1) * ndmu + nalpha + 1:nalpha)
            ## Add new score alpha constraints
            lp_solve_add_alpha_constraints(lp = lp, input = input, output = output, eff = eff, orientation = orientation, load.orientation = load.orientation)

            ## Call to solver
            status = solve(lp)

            ## Check return status (drop alpha values)
            if (status != 0) stop(paste0(gettext('Unable to solve cadea model. Solver status is '), status, '. (', lpsolve.status.txt[status + 1], '.)'))

            ## Get u and v values
            sol <- lpSolveAPI::get.variables(lp)[1:(ndmu * nio)]
            sol <- matrix(sol, nrow = ndmu, ncol = nio, byrow = TRUE)

            ## Get weights for outputs, named vy for compatibility with Benchmarking package
            vy <- sol[, 1:no, drop = FALSE]
            rownames(vy) <- rownames(output)
            colnames(vy) <- colnames(output)

            ## Get weights for inputs, named ux for compatibility with Benchmarking package
            ux = sol[, (no+1):nio, drop = FALSE]
            rownames(ux) <- rownames(input)
            colnames(ux) <- colnames(input)

            ## Compute scores
            eff <- switch(orientation,
                          input = rowSums(vy * output),
                          output = rowSums(ux * input)
                          )
            names(eff) <- rownames(input)

            ## Compute new loads and update .continue
            .cadea.loads <- adea_loads(input = input, output = output, ux = ux, vy = vy, load.orientation = load.orientation)
            
            ## debug
            ## cat("lp_solve_cadea: Iteration:", .iteration, "\n")
            ## print(.cadea.loads)
            
            ## Increase .iteration
            .iteration <- .iteration + 1
            
            ## Update continue
            .continue <- switch(load.orientation,
                                input = any(.cadea.loads$ratios$input < load.min - load.tolerance) || any(.cadea.loads$ratios$input > load.max + load.tolerance),
                                output = any(.cadea.loads$ratios$output < load.min - load.tolerance) || any(.cadea.loads$ratios$output > load.max + load.tolerance),
                                inoutput = any(.cadea.loads$ratios$input < load.min[1:ni] - load.tolerance) || any(.cadea.loads$ratios$input > load.max[1:ni] + load.tolerance) || any(.cadea.loads$ratios$output < load.min[(ni+1):nio] - load.tolerance) || any(.cadea.loads$ratios$output > load.max[(ni+1):nio] + load.tolerance)
                                )

            ## debug
            ## cat("continue (loads):", .continue, "\n")
            ## if (exists('.cadea.loads.old')) cat("loads.diff:", unlist(.cadea.loads) - unlist(.cadea.loads.old), "\n")

            ## Compute efficiencies error and loop if necessary
            .err <- sum((eff - eff.old)^2)
            .continue <- .continue || (.err > err.tolerance)

            ## debug
            ## cat("error:", .err, "\n")
            ## cat("continue (error)", .continue, "\n")
            ## cat("eff.diff:", eff - eff.old, "\n")
            ## cat(.cadea.loads$ratios$input < load.min[1:ni] - load.tolerance, '\n')
            ## cat(.cadea.loads$ratios$input, load.max[1:ni] + load.tolerance, .cadea.loads$ratios$input > load.max[1:ni] - load.tolerance, '\n')
            ## cat(.cadea.loads$ratios$output < load.min[(ni+1):nio] - load.tolerance, '\n')
            ## cat(.cadea.loads$ratios$output > load.max[(ni+1):nio] + load.tolerance, '\n')
        }
    }
    ## debug
    ## cat("lp_solve_cadea: Final loads:\n")
    ## print(.cadea.loads)
    
    ## Setup names
    rownames(ux) <- rownames(input)
    colnames(ux) <- colnames(input)
    rownames(vy) <- rownames(output)
    colnames(vy) <- colnames(output)
    
    ## Return the list values
    l <- list(lp = lp, status = status, ux = ux, vy = vy, iterations = .iteration, eff = eff, solver = 'lpsolve')
    class(l) <- 'cadea'
    l
}


## Definition of the lpsolve status text messages
lpsolve.status.txt <- c(gettext("Optimal solution found"),
                        gettext("The model is sub-optimal"),
                        gettext("The model is infeasible"),
                        gettext("The model is unbounded"),
                        gettext("The model is degenerate"),
                        gettext("Numerical failure encountered"),
                        gettext("Process aborted"),
                        gettext("Timeout"),
                        gettext("The model was solved by presolve"),
                        gettext("The branch and bound routine failed"),
                        gettext("The branch and bound was stopped because of a break-at-first or break-at-value"),
                        gettext("A feasible branch and bound solution was found"),
                        gettext("No feasible branch and bound solution was found"))
