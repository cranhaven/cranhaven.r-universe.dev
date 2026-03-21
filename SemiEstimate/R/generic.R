

arg_filter <- function(args, fn) {
        args[names(formals(fn))]
}

easy_call <- function(fn, args) {
        do.call(fn, arg_filter(args, fn))
}

update <- function(iterspace) {
        UseMethod("update")
}

small_update <- function(vec, tol) {
        return(all(abs(vec) < tol))
}

update.iterspace.ITAT <- function(iterspace) {
        args <- rlang::dots_list(!!!iterspace$parameters, !!!iterspace$initials, .homonyms = "first")
        Phi <- easy_call(iterspace$eqfns$Phi_fn, args)
        Phi_der_theta <- easy_call(iterspace$jac_like$Phi_der_theta_fn, args)
        iterspace$update_delta$theta <- -1 * solve(Phi_der_theta, Phi)

        args$theta <- args$theta + iterspace$update_delta$theta

        Psi <- easy_call(iterspace$eqfns$Psi_fn, args)
        Psi_der_lambda <- easy_call(iterspace$jac_like$Psi_der_lambda_fn, args)
        iterspace$update_delta$lambda <- -1 * solve(Psi_der_lambda, Psi)

        iterspace$iter_over <- small_update(iterspace$update_delta$theta, iterspace$tol) && small_update(iterspace$update_delta$lambda, iterspace$tol)
        if (!iterspace$iter_over) {
                iterspace$parameters$theta <- iterspace$parameters$theta + iterspace$update_delta$theta
                iterspace$parameters$lambda <- iterspace$parameters$lambda + iterspace$update_delta$lambda
        }
        return(iterspace)
}

update.iterspace.ITHM <- function(iterspace) {
        update.iterspace.IPHM(iterspace)
}

update.iterspace.IPAT <- function(iterspace) {
        args <- rlang::dots_list(!!!iterspace$parameters, !!!iterspace$initials, .homonyms = "first")
        Phi <- easy_call(iterspace$eqfns$Phi_fn, args)
        Phi_der_theta <- easy_call(iterspace$jac_like$Phi_der_theta_fn, args)
        Phi_der_lambda <- easy_call(iterspace$jac_like$Phi_der_lambda_fn, args)
        Psi_der_theta <- easy_call(iterspace$jac_like$Psi_der_theta_fn, args)
        Psi_der_lambda <- easy_call(iterspace$jac_like$Psi_der_lambda_fn, args)
        iterspace$update_delta$theta <- -1 * solve(Phi_der_theta - Phi_der_lambda * solve(Psi_der_lambda, Psi_der_theta), Phi)

        args$theta <- args$theta + iterspace$update_delta$theta

        Psi <- easy_call(iterspace$eqfns$Psi_fn, args)
        Psi_der_lambda <- easy_call(iterspace$jac_like$Psi_der_lambda_fn, args)
        iterspace$update_delta$lambda <- -1 * solve(Psi_der_lambda, Psi)

        iterspace$iter_over <- small_update(iterspace$update_delta$theta, iterspace$tol) && small_update(iterspace$update_delta$lambda, iterspace$tol)
        if (!iterspace$iter_over) {
                iterspace$parameters$theta <- iterspace$parameters$theta + iterspace$update_delta$theta
                iterspace$parameters$lambda <- iterspace$parameters$lambda + iterspace$update_delta$lambda
        }
        return(iterspace)
}

update.iterspace.IPHM <- function(iterspace) {
        fn_args <- iterspace$jac_like$fn_args
        ordered_fn <- iterspace$jac_like$ordered_fn
        intermediates <- iterspace$jac_like$intermediates
        fn_args$intermediates <- intermediates

        walk_fn <- function(x) {
                intermediates <<- easy_call(ordered_fn[[x]], fn_args)
                fn_args$intermediates <<- intermediates
        }
        purrr::walk(names(ordered_fn), walk_fn)
        iterspace$update_delta$theta <- intermediates$theta_delta
        iterspace$update_delta$lambda <- intermediates$lambda_delta
        fn_args$theta <- fn_args$theta + iterspace$update_delta$theta
        fn_args$lambda <- fn_args$lambda + iterspace$update_delta$lambda
        iterspace$iter_over <- small_update(iterspace$update_delta$theta, iterspace$tol) && small_update(iterspace$update_delta$lambda, iterspace$tol)
        if (!iterspace$iter_over) {
                iterspace$parameters$theta <- fn_args$theta
                iterspace$parameters$lambda <- fn_args$lambda
                iterspace$jac_like$fn_args <- fn_args
                iterspace$jac_like$intermediates <- intermediates
        }
        return(iterspace)
}

update.default <- function(iterspace) {
        stop("must be one of the iterspace type")
}

savestats <- function(savespace, iterspace, step) {
        iterspace$step <- step
        savespace$step <- step
        savespace$iterspace <- iterspace
        savespace$theta <- iterspace$parameters$theta
        savespace$lambda <- iterspace$parameters$lambda
        if (savespace$save.path) {
                savespace$res_path[[step]] <- iterspace
        }
        savespace
}