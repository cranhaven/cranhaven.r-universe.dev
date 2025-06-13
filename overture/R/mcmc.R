CreateSampsMat <- function(samps, var, var.name, n.save, backing.path) {
    samps[[var.name]] <- bigmemory::big.matrix(
        nrow=n.save,
        ncol=length(var),
        init=NA,
        backingpath=backing.path,
        backingfile=var.name,
        descriptorfile=paste0(var.name, ".desc")
    )

    return(samps)
}

InitExistingSampMat <- function(backing.file.path, desc.file.path, samps, var,
                                var.name, n.save, backing.path) {
        old.bigmat <- bigmemory::attach.big.matrix(desc.file.path)
        if(ncol(old.bigmat) == length(var)) { # Current mat is correct size
            old.bigmat[,] <- NA
            samps[[var.name]] <- old.bigmat
        }
        else { # Try removing the old big.matrix, might not work on Windows
            if(file.remove(backing.file.path)) {
                samps <- CreateSampsMat(samps, var, var.name, n.save,
                                        backing.path)
            }
            else {
                stop(paste0("Failed to remove ", backing.file.path,
                            ".  Try removing the file manually."))
            }
        }

        return(samps)
}

InitSampMat <- function(samps, var, var.name, n.save, backing.path, overwrite) {
    if (is.na(backing.path)) { # In-memory
        samps[[var.name]] <- matrix(nrow = n.save, ncol = length(var))
    }
    else { # File backed
        backing.file.path <- file.path(backing.path, var.name)
        if(file.exists(backing.file.path)) {
            if(!overwrite) { # Stop right away if overwrite is false
                stop(paste0("Backing file already exists in backing.path. ",
                          "Use overwrite=TRUE to replace."))
            }

            desc.file.path <- file.path(backing.path, paste0(var.name, ".desc"))
            if(!file.exists(desc.file.path)) {
                stop(paste0("Tried to overwrite ", var.name,
                            " but couldn't find ", desc.file.path,
                            ". Try removing the old results manually."))
            }

            samps <- InitExistingSampMat(backing.file.path, desc.file.path,
                                         samps, var, var.name, n.save,
                                         backing.path)
        }
        else {
            samps <- CreateSampsMat(samps, var, var.name, n.save, backing.path)
        }
    }

    return(samps)
}

InitSampMats <- function(envir, samps, n.save, backing.path, exclude,
                         overwrite) {
    for(var.name in ls(envir)) {
        if(ShouldSave(var.name, exclude, envir)) {
            # If it hasn't been done already, allocate storage for samples
            if(is.null(samps[[var.name]])) {
                samps <- InitSampMat(samps, envir[[var.name]], var.name, n.save,
                                     backing.path, overwrite)
            }
        }
    }

    return(samps)
}

ShouldSave <- function(var.name, exclude, envir) {
    if(is.null(exclude)) {
        is.numeric(envir[[var.name]])
    }
    else {
        is.numeric(envir[[var.name]]) && !(var.name %in% exclude)
    }
}

IsPositiveInteger <- function(x) {
    is.numeric(x) && (length(x)==1) && (x %% 1 == 0) && (x > 0)
}

FlushMats <- function(samps) {
    flush.failed.warning.msg <-
        "Failed to flush big.matrix, some samples may not be saved on-disk"
    lapply(samps, function(x) {
        if(bigmemory::is.big.matrix(x) && bigmemory::is.filebacked(x)) {
            flushed <- bigmemory::flush(x)
            if(!flushed) warning(flush.failed.warning.msg)
        }
    })
}

RunMcmc <- function(samps, b.start, expr_q, env, n.save, backing.path, thin,
                    exclude, overwrite) {
    # If file-backed, save the call so the MCMC can be resumed
    if(!is.na(backing.path)) {
        call.lst <- as.list(environment())
        call.lst[["env"]] <- NULL
        saveRDS(call.lst, file.path(backing.path, "call_lst.rds"))
    }

    env.len <- -1
    for(b in b.start:n.save) {
        for(t in 1:thin) {
            eval(expr_q, envir=env)
        }
        if(env.len != length(env)) {
            samps <- InitSampMats(env, samps, n.save, backing.path, exclude,
                                  overwrite)
            env.len <- length(env)
        }
        for(var.name in names(samps)) {
            samps[[var.name]][b, ] <- c(env[[var.name]])
        }
    }
    FlushMats(samps)

    return(samps)
}

#' Initialize a Markov chain Monte Carlo run
#'
#' Eliminates much of the "boilerplate" code needed for MCMC implementations by
#' looping through the samplers and saving the resulting draws automatically.
#'
#' \code{InitMcmc} returns a function that takes an R expression.  The returned
#' function automatically loops through the R expression and saves any numeric
#' assignments, typically MCMC samples, that are made within it. \code{exclude}
#' specifies assignments that should not be saved.  When \code{exclude} is
#' \code{NULL}, all the numeric assignments (scalar, vector, matrix, or array)
#' are saved.  The dimensions of matrix and array assignments are not preserved;
#' they are flattened into vectors before saving.  Non-numeric assignments are
#' not saved.
#'
#' The number of iterations for the MCMC chain is determined by \code{n.save}
#' and \code{thin}.  The desired number of samples to be saved from the target
#' distribution is set by \code{n.save}, and the chain is thinned according to
#' the interval set by \code{thin}.  The MCMC chain will run for \code{n.save}
#' \eqn{x} \code{thin} iterations.
#'
#' The MCMC samples can be saved either in-memory or on-disk.  Unlike saving
#' in-memory, saving on-disk is not constrained by available RAM.  Saving
#' on-disk can be used in high-dimensional settings where running multiple MCMC
#' chains in parallel and saving the results in-memory would use up all
#' available RAM.  File-backed saving uses \code{\link{big.matrix}}, and the
#' behaviors of that implementation apply when saving on-disk.  In particular,
#' \code{\link{big.matrix}} has call-by-reference rather than call-by-value
#' behavior, so care must be taken not to introduce unintended side-effects when
#' modifying these objects.  In-memory saving is implemented via
#' \code{\link{matrix}} and has standard R behavior.
#'
#' When \code{backing.path} is \code{NA}, samples will be saved in-memory.  To
#' save samples on-disk, \code{backing.path} should specify the path to the
#' directory where the MCMC samples should be saved.  The
#' \code{\link{big.matrix}} \code{backingfile}s will be saved in that directory,
#' with filenames corresponding to the variable assignment names made in the R
#' expression.  Consequently, the assignment names in the R expression must be
#' chosen in such a way that they are compatible as filenames on the operating
#' system.  The \code{\link{big.matrix}} \code{descriptorfile}s are also named
#' according to the variable assignment names made in the R expression, but with
#' a ".desc" suffix.
#'
#' By default, \code{InitMcmc} will not overwrite the results from a previous
#' file-backed MCMC.  This behavior can be overridden by specifying
#' \code{overwrite=TRUE} in \code{InitMcmc}, or as the second argument to the
#' function returned by \code{InitMcmc}.  See the examples for an illustration.
#' \code{overwrite} is ignored for in-memory MCMC.
#'
#' @param n.save number of samples to take.  If \code{thin}=1, the number of
#'   iterations to run the MCMC chain
#' @param backing.path \code{NA} to save the samples in-memory, otherwise
#'   directory path where MCMC samples will be saved
#' @param thin thinning interval
#' @param overwrite TRUE/FALSE indicating whether previous MCMC results should
#'   be overwritten
#' @param exclude character vector specifying variables that should not be saved
#' @return A function that returns a list of either \code{\link{matrix}} or
#'   \code{\link{big.matrix}} with the MCMC samples.  Each row in the matrices
#'   corresponds to one sample from the MCMC chain.
#' @example examples/example-InitMcmc.R
#' @export
#' @seealso \code{\link{bigmemory}}
InitMcmc <- function(n.save, backing.path=NA, thin=1, exclude=NULL,
                     overwrite=FALSE) {
    if(!IsPositiveInteger(n.save)) stop("'n.save' must be an integer > 0")
    if(!IsPositiveInteger(thin)) stop("'thin' must be an integer > 0")
    if(!is.null(exclude) && !is.character(exclude)) {
        warning("'exclude' will be ignored, expected character vector")
        exclude <- NULL
    }
    if(!is.na(backing.path) && !requireNamespace("bigmemory", quietly=TRUE)) {
        stop("Package 'bigmemory' required for saving on-disk")
    }

    over.write <- overwrite # To avoid recursive default argument reference
    function(expr, overwrite=over.write) {
        expr_q <- substitute(expr)
        env <- new.env(parent=parent.frame(1))
        samps <- list()
        RunMcmc(samps, 1, expr_q, env, n.save, backing.path, thin, exclude, overwrite)
    }
}

GetDescFileNames <- function(backing.path) {
    list.files(path=backing.path, pattern = "\\.desc$")
}

AttachSampleMat <- function(backing.path, desc.name) {
    bigmemory::attach.big.matrix(file.path(backing.path, desc.name))
}

#' Load samples from a file-backed MCMC run
#'
#' \code{LoadMcmc} loads the samples from a file-backed MCMC run initiated by
#' \code{InitMcmc}.  The result is a list of \code{\link{big.matrix}} with all
#' of the parameters that were saved in the MCMC run.  Alternatively, the
#' samples for individual parameters can be loaded by using
#' \code{\link{attach.big.matrix}} to load the corresponding \code{descriptor}
#' file, "ParameterName.desc," in the MCMC's \code{backing.path} directory.
#'
#' @param backing.path directory path where MCMC samples were saved
#' @return list of \code{\link{big.matrix}} with the MCMC samples
#' @example examples/example-LoadMcmc.R
#' @export
#' @seealso \code{\link{ToMemory}}, \code{\link{Peek}},
#'   \code{\link{attach.big.matrix}}
LoadMcmc <- function(backing.path) {
    desc.names <- GetDescFileNames(backing.path)
    if(identical(desc.names, character(0))) {
        stop(paste0("No '.desc' files found in ", backing.path))
    }
    sample.names <- gsub("\\.desc$", "", desc.names)

    samps <- list()
    for(i in 1:length(desc.names)) {
        samps[[sample.names[i]]] <- AttachSampleMat(backing.path, desc.names[i])
    }

    return(samps)
}

FirstMissIdxs <- function(samps) {
# Get a vector with the row index of the first MCMC sample with incomplete
# samples, i.e. the first row that has some NA values for the parameter
    first.miss.idxs <- vector(length=length(samps))
    for(i in 1:length(samps)) {
        param.curr <- samps[[i]]
        last.col.idx <- ncol(param.curr)
        na.ind <- is.na(param.curr[, last.col.idx])

        if(all(!na.ind)) { # No values are missing for this parameter
            first.miss.idxs[i] <- NA
        }
        else {
            first.miss.idxs[i] <- which.max(is.na(param.curr[, last.col.idx]))
        }
    }

    return(first.miss.idxs)
}

LastSampIdx <- function(first.miss.idxs) {
# Get row index of the last sample that was fully completed for all parameters
    min(first.miss.idxs, na.rm=TRUE) - 1
}

RemoveMissingDraws <- function(samps) {
    first.miss.idxs <- FirstMissIdxs(samps)
    if(all(is.na(first.miss.idxs))) { # No missing values, MCMC is finished
        samps.subsetted <- samps
    }
    else {
        last.samp.idx <- LastSampIdx(first.miss.idxs)
        samps.subsetted <- lapply(samps, function(x) {
            bigmemory::sub.big.matrix(x, lastRow=last.samp.idx)
        })
    }

    return(samps.subsetted)
}

#' Load samples from a partial MCMC run
#'
#' \code{Peek} allows the samples from a file-backed MCMC to be loaded in
#' another R session while the MCMC is still in progress.  By using \code{Peek},
#' the chain's convergence can be monitored before the MCMC chain has finished
#' running.
#'
#' @param backing.path directory path of an in-progress MCMC
#' @return list of \link{big.matrix} with samples from the partial MCMC run
#' @example examples/example-Peek.R
#' @export
#' @seealso \code{\link{InitMcmc}}, \code{\link{LoadMcmc}},
#'   \code{\link{big.matrix}}
Peek <- function(backing.path) {
    samps <- LoadMcmc(backing.path)
    samps.subsetted <- RemoveMissingDraws(samps)

    return(samps.subsetted)
}

#' Converts matrices in a file-backed MCMC to R matrix objects
#'
#' \code{ToMemory} is a convenience method to load the samples from a
#' file-backed MCMC run into memory.  Given a list of \code{\link{big.matrix}}
#' objects, it will convert them to standard R matrix objects.
#'
#' @param samples list of \code{\link{big.matrix}} objects, typically coming
#'   from \code{\link{InitMcmc}}
#' @return list of R \code{\link{matrix}} objects
#' @example examples/example-ToMemory.R
#' @export
#' @seealso \code{\link{InitMcmc}}, \code{\link{big.matrix}}
ToMemory <- function(samples) {
    lapply(samples, function(x) {
        x.dim <- dim(x)
        in.mem.mat <- bigmemory::as.matrix(x)
        dim(in.mem.mat) <- x.dim
        in.mem.mat
    })
}

#' Resumes an interrupted file-backed MCMC
#'
#' \code{Resume} will finish a file-backed MCMC that was interrupted.  To resume
#' an MCMC run, specify the MCMC's backing path and the sampling will continue
#' from the last completed sample in the chain.  Note, however, that the random
#' number generator state from when the MCMC was interrupted is \emph{not}
#' restored, so the resulting chain my not be reproducible, even if a seed was
#' specified before the sampling was interrupted.
#'
#' @param backing.path directory path where the (partially completed) MCMC
#'   samples were saved
#' @return A list of either \code{\link{matrix}} or \code{\link{big.matrix}}
#'   with the MCMC samples.  Each row in the matrices corresponds to one sample
#'   from the MCMC chain.
#' @example examples/example-Resume.R
#' @export
#' @seealso \code{\link{InitMcmc}}
Resume <- function(backing.path) {
    samps <- LoadMcmc(backing.path)
    first.miss.idxs <- FirstMissIdxs(samps)

    # Run the rest of the MCMC if the MCMC isn't already complete
    if(!all(is.na(first.miss.idxs))) {
        env <- new.env(parent=parent.frame(1))

        # Re-assign the last completed parameter samples to the env
        last.samp.idx <- LastSampIdx(first.miss.idxs)
        for(param.name in names(samps)) {
            assign(param.name, samps[[param.name]][last.samp.idx,], pos=env)
        }

        call.lst <- readRDS(file.path(backing.path, "call_lst.rds"))
        call.lst[["b.start"]] <- last.samp.idx + 1
        call.lst[["env"]] <- env
        call.lst[["samps"]] <- samps

        samps <- do.call("RunMcmc", call.lst, quote=TRUE)
    }

    return(samps)
}
