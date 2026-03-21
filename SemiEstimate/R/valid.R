validate_fn <- function(fn) {
        arg_names <- names(formals(fn))
        if ("lambda" %in% arg_names && "theta" %in% arg_names) {
                TRUE
        } else {
                FALSE
        }
}

validate_eqfns <- function(x) {
        values <- unclass(x)
        stopifnot("Phi_fn" %in% names(values))
        stopifnot("Psi_fn" %in% names(values))
        stopifnot(validate_fn(values$Phi_fn))
        stopifnot(validate_fn(values$Psi_fn))
}

validate_jac <- function(x) {
        values <- unclass(x)
        stopifnot("Phi_der_theta_fn" %in% names(values))
        stopifnot("Phi_der_lambda_fn" %in% names(values))
        stopifnot("Psi_der_theta_fn" %in% names(values))
        stopifnot("Psi_der_lambda_fn" %in% names(values))
        stopifnot(validate_fn(values$Phi_der_theta_fn))
        stopifnot(validate_fn(values$Phi_der_lambda_fn))
        stopifnot(validate_fn(values$Psi_der_theta_fn))
        stopifnot(validate_fn(values$Psi_der_lambda_fn))
}

validate_quasijac <- function(x) {
        values <- unclass(x)
        stopifnot("Phi_der_theta_fn" %in% names(values))
        stopifnot("Phi_der_lambda_fn" %in% names(values))
        stopifnot("Psi_der_theta_fn" %in% names(values))
        stopifnot("Psi_der_lambda_fn" %in% names(values))
        stopifnot(validate_fn(values$Phi_der_theta_fn))
        stopifnot(validate_fn(values$Phi_der_lambda_fn))
        stopifnot(validate_fn(values$Psi_der_theta_fn))
        stopifnot(validate_fn(values$Psi_der_lambda_fn))
}

validate_semijac <- function(x) {
        values <- unclass(x)
        stopifnot("Phi_der_theta_fn" %in% names(values))
        stopifnot("Phi_der_lambda_fn" %in% names(values))
        stopifnot("Psi_der_theta_fn" %in% names(values))
        stopifnot("Psi_der_lambda_fn" %in% names(values))
        stopifnot(validate_fn(values$Phi_der_theta_fn))
        stopifnot(validate_fn(values$Phi_der_lambda_fn))
        stopifnot(validate_fn(values$Psi_der_theta_fn))
        stopifnot(validate_fn(values$Psi_der_lambda_fn))
}


validate_diyjac <- function(x) {
        values <- unclass(x)
        stopifnot("theta_delta" %in% names(values))
        stopifnot("lambda_delta" %in% names(values))
}

validate_diyfn <- function(fn) {
        arg_names <- names(formals(fn))
        if ("intermediates" %in% arg_names) {
                TRUE
        } else {
                FALSE
        }
}

validate_method <- function(x) {
        stopifnot(x %in% c("implicit", "iterative"))
}

validate_control <- function(x) {
        stopifnot("max_iter" %in% names(x))
        stopifnot("tol" %in% names(x))
        stopifnot(is.double(x$max_iter))
        stopifnot(is.double(x$tol))
}

validate_save <- function(x) {
        stopifnot("time" %in% names(x))
        stopifnot("path" %in% names(x))
        stopifnot(is.logical(x$time))
        stopifnot(is.logical(x$path))
}