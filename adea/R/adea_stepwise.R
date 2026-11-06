#' Select an optimal subset of variables for DEA analysis
#'
#' Stepwise procedure for variable selection in DEA models.
#'
#' This function serves as a backend utility for variable selection in DEA models and is not intended for end-user use.
#' It is used as part of the adea_hierarchical and adea_parametric functions.
#'
#' This function performs a stepwise procedure for variable selection within DEA models.
#' 
#' @inheritParams adea
#' @inheritParams adea_parametric
#' @param direction The direction in which the variables enter or exit the model.
#' Currently, only the "backward" option is implemented.
#' @param load.critical Minimum values for loads to consider that a variable should be considered in the model. It can be also a vector with two values, the first value input loads and the second for output loads.
#' @param max.steps The maximum number of steps allowed.
#' @param verbose Use 0 for minimal output, displaying only the final model.
#' Use 1 or higher values to get detailed information for each step.
#' The default is 0.
#' This option affects only the printed output and not the result.
#' @return The function returns a DEA model with optimised set of variables.
adea_stepwise <- function(input, output, orientation = c('input', 'output'), load.orientation = c('inoutput', 'input', 'output'), name = '', direction = c('backward', 'backward/input', 'backward/output'), load.critical = .5, max.steps = ncol(input) + ncol(output) - 2, solver = 'auto', verbose = 0) {
    .adea <- stepwise(input = input, output = output, orientation = orientation, load.orientation = load.orientation, name = name, direction = direction, load.critical = load.critical, max.steps = max.steps, solver = 'auto', verbose = verbose)
    .adea$adea <- .adea$models[[length(.adea$models) - .adea$steps]]
    .adea$models <- NULL
    class(.adea) <- 'adeastepwise'
    .adea
}

stepwise <- function(input, output, orientation, load.orientation, name = '', direction, load.critical, max.steps = ncol(input) + ncol(output) - 2, solver, verbose = 0) {

    ## Check orientation
    orientation <- match.arg(orientation, c('input', 'output'))

    ## Check direction
    direction <- match.arg(direction, c('backward', 'backward/input', 'backward/output') )

    ## Check load.orientation
    load.orientation <- match.arg(load.orientation, c('inoutput', 'input', 'output'))
    
    ## Check load.critical value
    if (load.critical < 0 || load.critical > 1) stop(gettext('load.critical value should be a positive number less than 1.'))

    ## Compute initial adea model
    .adea <- switch(direction,
                    'backward' = stepwisebackward(input = input, output = output, orientation = orientation, load.orientation = load.orientation, name = name, load.critical = load.critical, max.steps = max.steps, index.input = 1:ncol(input), index.output = 1:ncol(output), solver = solver, verbose = verbose),
                    'backward/input' = stepwisebackward(input = input, output = output, orientation = orientation, load.orientation = load.orientation, name = name, load.critical = load.critical, max.steps = max.steps, index.input = 1:ncol(input), index.output = NULL, solver = solver, verbose = verbose),
                    'backward/output' = stepwisebackward(input = input, output = output, orientation = orientation, load.orientation = load.orientation, name = name, load.critical = load.critical, max.steps = max.steps, index.input = NULL, index.output = ncol(input) + 1:ncol(output), solver = solver, verbose = verbose)
                    )

    ## Return object with results
    .adea
}

### Currently the only step-by-step procedure implemented
stepwisebackward <- function(input, output, orientation, load.orientation, name = '', load.critical, max.steps = ncol(input) + ncol(output), index.input = 1:ncol(input), index.output = 1:ncol(output), solver, verbose)
{
    ## Match args
    orientation <- match.arg(orientation, c('input', 'output'))
    load.orientation <- match.arg(load.orientation, c('inoutput', 'input', 'output'))
    
    ## Setup initial values
    .step <- 1
    .nvariables <- ncol(input) + ncol(output)
    i <- .nvariables
    iter <- 0

    dropping <- character(.nvariables)
    loads <- numeric(.nvariables)
    loads[i] <- 0
    models <- list()
    ninputs <- numeric(.nvariables)
    inputnames <- character(.nvariables)
    noutputs <- numeric(.nvariables)
    outputnames <- character(.nvariables)
    nvariables <- 1:.nvariables
    out <- character(.nvariables)

    ## Compute initial adea model
    .adea <- adea_(input = input, output = output, orientation = orientation, load.orientation = load.orientation, name = name, solver = solver)
    
    ## Setup .index
    .index <- list(input = index.input, output = index.output)

    ## Store data log
    loads[i] <- .adea$loads$load
    ninputs[i] <- length(.index$input)
    noutputs[i] <- length(.index$output)
    nvariables[i] <- ninputs[i] + noutputs[i]
    inputnames[i] <- paste(colnames(input)[.index$input], collapse = ", ")
    outputnames[i] <- paste(colnames(output)[.index$output], collapse = ", ")

    ## Start output log
    if (verbose > 0) {
        cat(gettext('\nStarting adea_stepwise using backward selection\n'))
        cat(gettext('load.critical value is'), load.critical, '\n')
    }

    ## Main stepwise loop
    while (.adea$loads$load < load.critical && .step <= max.steps && i > 2) {

        ## Check if current model has only one variable
        if ((load.orientation == 'input') && (length(.index$input) == 1)) break;
        if ((load.orientation == 'output') && (length(.index$output) == 1)) break;
        if ((load.orientation == 'inoutput') && (length(.index$input) == 1) && (length(.index$output) == 1)) break;

        ## Store data log
        loads[i] <- .adea$loads$load
        models[[i]] <- .adea
        ninputs[i] <- length(.index$input)
        noutputs[i] <- length(.index$output)
        nvariables[i] <- ninputs[i] + noutputs[i]
        inputnames[i] <- paste(colnames(input)[.index$input], collapse = ", ")
        outputnames[i] <- paste(colnames(output)[.index$output], collapse = ", ")
        
        ## Compute the going out variable
        ## Next drop is input or output?
        .inout <- switch(load.orientation,
                         input = 'input',
                         output = 'output',
                         inoutput = ifelse(min(.adea$loads$input) < min(.adea$loads$output) , 'input', 'output')
                         )

        ## print("***")
        ## print(load.orientation)
        ## print(.inout)
        ## Compute the outgoing out variable
        .out <- switch(load.orientation,
                       input = which.min(.adea$loads$input),
                       output = which.min(.adea$loads$output),
                       inoutput = ifelse(min(.adea$loads$input) < min(.adea$loads$output), which.min(.adea$loads$input), which.min(.adea$loads$output))
                       )
        
        ## Store data log and output
        out[i] <- colnames(get(.inout))[.index[[.inout]][.out]]

        if (verbose > 0) {
            cat(sprintf(gettext('Step %d dropping %s %s with load equal to %f\n'), .step, .inout, colnames(get(.inout))[.index[[.inout]][.out]], .adea$loads[[.inout]][.out]))
        }
        
        ## Output load values
        if (verbose > 0) {
            cat('\n', gettext('Load values for current model'), ':\n')
            print(.adea$loads$input)
            print(.adea$loads$output)
        }
        
        ## Drop variable
        .index[[.inout]] <- .index[[.inout]][-.out]
        
        ## Compute new model
        .adea <- adea_(input = input[, .index$input, drop = FALSE], output = output[, .index$output, drop = FALSE], orientation = orientation, load.orientation = load.orientation, name = name, solver = solver)
        
        ## Next step
        .step <- .step + 1
        i <- i - 1
    }
    
    ## Store data log for last step
    loads[i] <- .adea$loads$load
    models[[i]] <- .adea
    ninputs[i] <- length(.index$input)
    noutputs[i] <- length(.index$output)
    nvariables[i] <- ninputs[i] + noutputs[i]
    inputnames[i] <- paste(colnames(input)[.index$input], collapse = ", ")
    outputnames[i] <- paste(colnames(output)[.index$output], collapse = ", ")

    ## Output verbose information
    if (verbose > 0) {
        cat('\n', gettext('Load values for final model'), ':\n')
        print(.adea$loads$inputs)
        print(.adea$loads$outputs)
        cat(gettext("The reason for stop was that the"), ifelse(.step > max.steps, gettext('maximum number of steps has been reached.\n'), gettext('load level given has been reached.\n')))
    }

    ## Prepare to return
    .adeasw <- list(
        inputnames = inputnames,
        loads = loads,
        load.critical = load.critical,
        load.orientation = load.orientation,
        models = models,
        name = name,
        ninputs = ninputs,
        noutputs = noutputs,
        nvariables = nvariables,
        orientation = orientation,
        out = out,
        outputnames = outputnames,
        solver = .adea$solver,
        steps = .step -1,
        stop.criterion = ifelse(.step > max.steps, 'Steps', 'Level')
    )

    ## Set class of returning object
    class(.adeasw) <- 'adeastepwise'
    .adeasw
}
