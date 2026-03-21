


new_eqfns <- function(Phi_fn = function(theta, lambda, ...) NULL,
                      Psi_fn = function(theta, lambda, ...) NULL) {
        stopifnot(is.function(Phi_fn))
        stopifnot(is.function(Psi_fn))
        data <- list(Phi_fn = Phi_fn, Psi_fn = Psi_fn)
        eqfns <- structure(data, class = "eqfns")
        validate_eqfns(eqfns)
        eqfns
}

new_jac <- function(Phi_der_theta_fn = function(theta, lambda, ...) NULL,
                    Phi_der_lambda_fn = function(theta, lambda, ...) NULL,
                    Psi_der_theta_fn = function(theta, lambda, ...) NULL,
                    Psi_der_lambda_fn = function(theta, lambda, ...) NULL) {
        stopifnot(is.function(Phi_der_theta_fn))
        stopifnot(is.function(Phi_der_lambda_fn))
        stopifnot(is.function(Psi_der_theta_fn))
        stopifnot(is.function(Psi_der_lambda_fn))
        data <- list(
                Phi_der_theta_fn = Phi_der_theta_fn,
                Phi_der_lambda_fn = Phi_der_lambda_fn,
                Psi_der_theta_fn = Psi_der_theta_fn,
                Psi_der_lambda_fn = Psi_der_lambda_fn
        )
        jac <- structure(data,
                class = "jac"
        )
        validate_jac(jac)
        jac
}


new_quasijac <- function(Phi_fn = function(theta, lambda, ...) NULL,
                         Psi_fn = function(theta, lambda, ...) NULL, ...) {
        stopifnot(is.function(Phi_fn))
        stopifnot(is.function(Psi_fn))
        data_args <- list(...)
        Phi_der_theta_fn <- function(theta, lambda) numDeriv::jacobian(func = function(x) easy_call(Phi_fn, rlang::dots_list(theta = x, lambda = lambda, !!!data_args, .homonyms = "first")), x = theta)
        Phi_der_lambda_fn <- function(theta, lambda) numDeriv::jacobian(func = function(x) easy_call(Phi_fn, rlang::dots_list(theta = theta, lambda = x, !!!data_args, .homonyms = "first")), x = lambda)
        Psi_der_theta_fn <- function(theta, lambda) numDeriv::jacobian(func = function(x) easy_call(Psi_fn, rlang::dots_list(theta = x, lambda = lambda, !!!data_args, .homonyms = "first")), x = theta)
        Psi_der_lambda_fn <- function(theta, lambda) numDeriv::jacobian(func = function(x) easy_call(Psi_fn, rlang::dots_list(theta = theta, lambda = x, !!!data_args, .homonyms = "first")), x = lambda)
        data <- list(
                Phi_der_theta_fn = Phi_der_theta_fn,
                Phi_der_lambda_fn = Phi_der_lambda_fn,
                Psi_der_theta_fn = Psi_der_theta_fn,
                Psi_der_lambda_fn = Psi_der_lambda_fn
        )
        quasijac <- structure(
                data,
                class = "quasijac"
        )
        validate_quasijac(quasijac)
        quasijac
}

new_semijac <- function(Phi_fn = function(theta, lambda, ...) NULL, Psi_fn = function(theta, lambda, ...) NULL, ...) {
        stopifnot(is.function(Phi_fn))
        stopifnot(is.function(Psi_fn))
        quasijac <- new_quasijac(Phi_fn = Phi_fn, Psi_fn = Psi_fn, ...)
        der_name_list <- c("Phi_der_theta_fn", "Phi_der_lambda_fn", "Psi_der_theta_fn", "Psi_der_lambda_fn")
        args <- list(...)
        quasi_names <- c()
        purrr::map(der_name_list, function(x) if (!(x %in% names(args))) eval(parse(text = paste("args$", x, "<<-", "quasijac$", x, ";quasi_names<<-c(quasi_names,'", x, "')"))))
        data <- list(
                Phi_der_theta_fn = args$Phi_der_theta_fn,
                Phi_der_lambda_fn = args$Phi_der_lambda_fn,
                Psi_der_theta_fn = args$Psi_der_theta_fn,
                Psi_der_lambda_fn = args$Psi_der_lambda_fn
        )
        if (length(quasi_names) == 4) {
                quasijac
        } else {
                semijac <- structure(
                        data,
                        quasi_names = quasi_names, class = "semijac"
                )
                validate_semijac(semijac)
                semijac
        }
}

check_names <- function(names, var_list) {
        purrr::walk(names, function(x) stopifnot(x %in% names(var_list)))
}

# use key map to wrap it
new_diyjac <- function(intermediates, theta, lambda, method, ...) {
        args <- list(...)
        ordered_fn <- list()
        fn_args <- list()
        class(intermediates) <- "intermediates"
        args$intermediates <- intermediates
        map_fn <- function(x) {
                if (is.function(args[[x]])) {
                        ordered_fn[[x]] <<- args[[x]]
                } else {
                        fn_args[[x]] <<- args[[x]]
                }
        }
        purrr::walk(names(args), map_fn)
        fn_args$method <- method
        fn_args$theta <- theta
        fn_args$lambda <- lambda
        if (method == "implicit") {
                return_fn <- function(itermediates) list(Phi_der_theta = intermediates$Phi_der_theta, Phi_der_lambda = intermediates$Phi_der_lambda, Psi_der_theta = intermediates$Psi_der_theta, Psi_der_lambda = intermediates$Psi_der_lambda, Psi_der_lambda2 = intermediates$Psi_der_lambda2, Phi = intermediates$Phi, Psi = intermediates$Psi)
        } else {
                return_fn <- function(itermediates) list(Phi_der_theta = intermediates$Phi_der_theta, Psi_der_lambda = intermediates$Psi_der_lambda, Phi = intermediates$Phi, Psi = intermediates$Psi)
        }
        data <- list(ordered_fn = ordered_fn, intermediates = intermediates, fn_args = fn_args, return_fn = return_fn)
        diyjac <- structure(
                data,
                class = "diyjac"
        )
        validate_diyjac(diyjac)
        diyjac
}

get_subclass <- function(method, jac_class) {
        first <- ifelse(method == "iterative", "IT", "IP")
        second <- ifelse(jac_class == "diyjac", "HM", "AT")
        paste(first, second, sep = "")
}

new_iterspace <- function(initials = list(), Phi_fn, Psi_fn, jac_like, control) {
        stopifnot("theta" %in% names(initials))
        stopifnot("lambda" %in% names(initials))
        stopifnot("method" %in% names(initials))
        sub_class_name <- get_subclass(initials$method, class(jac_like))
        eqfns <- new_eqfns(Phi_fn = Phi_fn, Psi_fn = Psi_fn)
        data <- list(initials = initials, eqfns = eqfns, jac_like = jac_like, iter_step = 0, iter_over = FALSE, update_delta = list(theta = NULL, lambda = NULL), parameters = list(lambda = initials$lambda, theta = initials$theta), control = control)
        structure(data, class = paste("iterspace.", sub_class_name, sep = ""))
}

new_savespace <- function(save.path = FALSE, save.time = FALSE, ...) {
        data <- list(save.path = save.path, save.time = save.time, cache = list(...), res_path = list())
        structure(data, class = "savespace")
}