.cimehr_spline <- new.env(parent = .cimehr_common)
evalq({
    .cimehr_spline_predict <- function(object, newdata, times, type = c("population", "observed"), kappa = NULL, 
        se.fit = TRUE, level = 0.95, ...) {
        type <- match.arg(type)
        z <- .cimehr_spline_newdata_design(object, newdata, times, type, kappa)
        fit <- .cimehr_stable_matvec(z$D, object$coefficients)
        ans <- data.frame(profile = z$profile, time = z$time, fit = if (length(z$profile) > nrow(newdata)) 
            unname(fit)
        else fit)
        if (isTRUE(se.fit)) {
            V <- .cimehr_spline_vcov(object)
            variance <- if (is.null(object$covariance_factor))
                rowSums((z$D %*% V) * z$D) else
                .cimehr_factor_variance(z$D, object$covariance_factor)
            se <- .cimehr_checked_se(variance)
            q <- stats::qnorm(1 - (1 - level)/2)
            ans$se <- se
            ans$lower <- fit - q * se
            ans$upper <- fit + q * se
        }
        attr(ans, "trajectory_type") <- type
        attr(ans, "uncertainty_scope") <- if (type == "observed") {
            "conditional on the supplied kappa values"
        }
        else {
            "population trajectory from the fitted coefficient covariance"
        }
        ans
    }

    .check_covars <- function(vars, data, arg_name) {
        if (is.null(vars) || length(vars) == 0L) 
            return(character(0))
        if (!is.character(vars)) 
            stop(sprintf("`%s` must be character or NULL.", arg_name), call. = FALSE)
        bad <- setdiff(vars, names(data))
        if (length(bad)) 
            stop(sprintf("`%s` has columns absent from data: %s", arg_name, paste(bad, collapse = ", ")), 
                call. = FALSE)
        vars
    }

    .cimehr_spline_basis <- function(x, df = NULL, knots = NULL, boundary_knots, type = c("natural", 
        "bspline"), intercept = TRUE, degree = 3L, spec = NULL) {
        type <- match.arg(type)
        x <- as.numeric(x)
        if (any(!is.finite(x))) 
            stop("Spline times must be finite.", call. = FALSE)
        if (!is.null(spec)) {
            type <- spec$type
            if (type == "natural") {
                ans <- splines::ns(x, knots = spec$knots, Boundary.knots = spec$boundary_knots, intercept = spec$intercept)
            }
            else {
                ans <- splines::bs(x, knots = spec$knots, Boundary.knots = spec$boundary_knots, intercept = spec$intercept, 
                  degree = spec$degree)
            }
            colnames(ans) <- spec$column_names
            return(as.matrix(ans))
        }
        boundary_knots <- sort(as.numeric(boundary_knots))
        if (length(boundary_knots) != 2L || any(!is.finite(boundary_knots)) || diff(boundary_knots) <= 
            0) {
            stop("`boundary_knots` must contain two increasing finite values.", call. = FALSE)
        }
        if (!is.null(knots)) {
            knots <- sort(unique(as.numeric(knots)))
            if (any(!is.finite(knots)) || any(knots <= boundary_knots[1L] | knots >= boundary_knots[2L])) {
                stop("Every internal spline knot must lie strictly inside the boundaries.", call. = FALSE)
            }
            df <- NULL
        }
        else {
            if (length(df) != 1L || !is.finite(df) || df < 2L || df != as.integer(df)) {
                stop("Supply integer `df >= 2` or explicit internal knots.", call. = FALSE)
            }
            df <- as.integer(df)
        }
        if (type == "natural") {
            ans <- splines::ns(x, df = df, knots = knots, Boundary.knots = boundary_knots, intercept = intercept)
        }
        else {
            degree <- as.integer(degree)
            if (length(degree) != 1L || is.na(degree) || degree < 1L) {
                stop("`degree` must be a positive integer.", call. = FALSE)
            }
            ans <- splines::bs(x, df = df, knots = knots, Boundary.knots = boundary_knots, intercept = intercept, 
                degree = degree)
        }
        column_names <- paste0(if (intercept) 
            "S0."
        else "Sc.", seq_len(ncol(ans)))
        colnames(ans) <- column_names
        spec <- list(type = type, degree = if (type == "bspline") degree else 3L, knots = attr(ans, "knots"), 
            boundary_knots = attr(ans, "Boundary.knots"), intercept = intercept, column_names = column_names, 
            dimension = ncol(ans), df_argument = df, knots_were_prespecified = !is.null(knots))
        list(matrix = as.matrix(ans), spec = spec)
    }

    .cimehr_spline_spec <- function(x, df = NULL, knots = NULL, boundary_knots, type = c("natural", 
        "bspline"), intercept = TRUE, degree = 3L) {
        type <- match.arg(type)
        full <- function() .cimehr_spline_basis(x = x, df = df, knots = knots, boundary_knots = boundary_knots, 
            type = type, intercept = intercept, degree = degree)$spec
        plain <- is.numeric(x) && length(x) > 0L && all(is.finite(x)) && is.numeric(boundary_knots) && 
            length(boundary_knots) == 2L && all(is.finite(boundary_knots)) && (isTRUE(intercept) || 
            isFALSE(intercept))
        if (!plain) 
            return(full())
        x <- as.numeric(x)
        bk <- sort(as.numeric(boundary_knots))
        if (diff(bk) <= 0) 
            return(full())
        if (!is.null(knots)) {
            if (!is.numeric(knots) || any(!is.finite(knots))) 
                return(full())
            knots_used <- sort(unique(as.numeric(knots)))
            if (any(knots_used <= bk[1L] | knots_used >= bk[2L])) 
                return(full())
            df_used <- NULL
        }
        else {
            if (!is.numeric(df) || length(df) != 1L || !is.finite(df) || df < 2 || df > 1e+06 || 
                df != as.integer(df)) 
                return(full())
            df_used <- as.integer(df)
        }
        if (type == "bspline") {
            if (!is.numeric(degree) || length(degree) != 1L || !is.finite(degree) || degree < 1 || 
                degree > 1e+06) 
                return(full())
            degree <- as.integer(degree)
            ord <- 1L + degree
        }
        outside <- (x < bk[1L]) | (x > bk[2L])
        if (type == "bspline" && any(outside)) 
            return(full())
        if (is.null(knots)) {
            nIknots <- if (type == "natural") 
                df_used - 1L - intercept
            else df_used - ord + (1L - intercept)
            if (nIknots < 0L || all(outside)) 
                return(full())
            knots_used <- if (nIknots > 0L) {
                probs <- seq.int(from = 0, to = 1, length.out = nIknots + 2L)[-c(1L, nIknots + 2L)]
                stats::quantile(x[!outside], probs, names = FALSE)
            }
            if (length(knots_used) && any(range(knots_used) %in% bk)) 
                return(full())
        }
        dimension <- if (type == "natural") 
            length(knots_used) + 2L - !intercept
        else length(knots_used) + ord - !intercept
        column_names <- paste0(if (intercept) 
            "S0."
        else "Sc.", seq_len(dimension))
        list(type = type, degree = if (type == "bspline") degree else 3L, knots = if (is.null(knots_used)) 
            numeric()
        else knots_used, boundary_knots = bk, intercept = intercept, column_names = column_names, 
            dimension = dimension, df_argument = df_used, knots_were_prespecified = !is.null(knots))
    }

    .cimehr_spline_stage3 <- function(y, r, time, visit_mask, subject, XY_subject, G_subject, trajectory_names, 
        ZY_subject, use_time, kappa, include_theta, baseline_spec, trajectory_spec, reference_time, time_scale, 
        chunk_size = 100000L, zero_spec = NULL) {
        n <- nrow(XY_subject)
        eo <- which(visit_mask & r == 1L & is.finite(y))
        if (!length(eo)) 
            stop("No observed outcomes are available.", call. = FALSE)
        chunk_size <- as.integer(chunk_size)
        if (length(chunk_size) != 1L || is.na(chunk_size) || chunk_size < 1L) {
            stop("`stage3_chunk_size` must be a positive integer.", call. = FALSE)
        }
        baseline_names <- paste0("baseline.", baseline_spec$column_names)
        fixed_names <- if (ncol(XY_subject)) {
            paste0("beta.", colnames(XY_subject))
        }
        else character(0)
        interaction_names <- if (length(trajectory_names)) {
            unlist(lapply(trajectory_names, function(v) {
                paste0("trajectory.", v, ".", trajectory_spec$column_names)
            }), use.names = FALSE)
        }
        else character(0)
        random_names <- if (include_theta) {
            z_names <- colnames(.cimehr_random_parts(ZY_subject, use_time)$M)
            paste0(if (!is.null(zero_spec) && identical(zero_spec$kind, "quotient"))
                "compensation." else "theta.", z_names)
        }
        else character(0)
        coef_names <- c(baseline_names, fixed_names, interaction_names, random_names)
        p <- length(coef_names)
        idx_baseline <- seq_along(baseline_names)
        cursor <- length(idx_baseline)
        idx_fixed <- if (length(fixed_names)) 
            cursor + seq_along(fixed_names)
        else integer(0)
        cursor <- cursor + length(fixed_names)
        idx_interaction <- if (length(interaction_names)) {
            cursor + seq_along(interaction_names)
        }
        else integer(0)
        cursor <- cursor + length(interaction_names)
        idx_theta <- if (length(random_names)) 
            cursor + seq_along(random_names)
        else integer(0)
        build_design <- function(take) {
            rows <- eo[take]
            si <- subject[rows]
            tt <- time[rows]
            B0 <- .cimehr_spline_eval(tt, baseline_spec)
            colnames(B0) <- baseline_names
            X <- .cimehr_fixed_rows(XY_subject, si, tt)
            colnames(X) <- fixed_names
            GI <- matrix(0, length(rows), 0L)
            if (length(trajectory_names)) {
                Bc <- .cimehr_spline_centered_eval(tt, trajectory_spec, reference_time)
                GI <- .cimehr_spline_interactions(Bc, G_subject[si, , drop = FALSE], trajectory_names)
            }
            Theta <- matrix(0, length(rows), 0L)
            W <- matrix(0, length(rows), 0L)
            if (include_theta) {
                W <- .cimehr_spline_random_design(si, tt, ZY_subject, use_time, reference_time, time_scale)
                Theta <- kappa[si] * W
                colnames(Theta) <- random_names
            }
            D <- cbind(B0, X, GI, Theta)
            colnames(D) <- coef_names
            list(D = D, W = W, subject = si, rows = rows)
        }
        starts <- seq.int(1L, length(eo), by = chunk_size)
        single <- length(starts) == 1L
        bread <- matrix(0, p, p, dimnames = list(coef_names, coef_names))
        rhs <- setNames(numeric(p), coef_names)
        for (first in starts) {
            take <- first:min(first + chunk_size - 1L, length(eo))
            z <- build_design(take)
            bread <- bread + crossprod(z$D)
            rhs <- rhs + drop(crossprod(z$D, y[z$rows]))
        }
        numerical_solver <- NULL
        inverse_bread <- NULL
        solve_state <- NULL
        diagnostic_tolerance <- if (is.null(zero_spec)) 1e-7 else zero_spec$tolerance
        ordinary <- tryCatch({
            scale_zero <- sqrt(pmax(diag(bread), 0))
            scaled_zero <- bread / outer(pmax(scale_zero, .Machine$double.eps),
                pmax(scale_zero, .Machine$double.eps))
            if (any(!is.finite(scaled_zero)) || qr(scaled_zero, tol = 1e-7)$rank < p)
                stop("The normal-equation conditioning screen requests an original-design solve.")
            list(coefficients = drop(.cimehr_safe_solve(bread, rhs, "Spline Stage-3 estimating matrix")),
                inverse = .cimehr_safe_solve(bread, name = "Spline Stage-3 bread"))
        }, error = function(e) e)
        if (inherits(ordinary, "error")) {
            ordinary <- tryCatch({
                compact_D <- matrix(0, 0L, p)
                compact_y <- numeric()
                for (first in starts) {
                    take <- first:min(first + chunk_size - 1L, length(eo))
                    z <- build_design(take)
                    block <- rbind(compact_D, z$D)
                    block_y <- c(compact_y, y[z$rows])
                    factor <- qr(block, LAPACK = TRUE)
                    keep <- seq_len(min(nrow(block), p))
                    compact_D <- qr.R(factor)[keep, order(factor$pivot), drop = FALSE]
                    compact_y <- qr.qty(factor, block_y)[keep]
                }
                rescued <- .cimehr_updated$.cimehr_stage3_qr_solve(compact_D, compact_y,
                    diagnostic_tolerance = diagnostic_tolerance,
                    roundoff_dimension = length(eo))
                rescued$diagnostics$method <- "chunked original-design QR"
                rescued
            }, error = function(e) {
                if (!is.null(zero_spec))
                    .cimehr_updated$.lu_zero_stop(zero_spec$posterior,
                        "compensation_not_separable_from_outcome", "outcome",
                        detail = paste0(conditionMessage(e),
                            " This numerical check does not establish population nonidentification."))
                stop(paste0("Spline Stage-3 original-design solve failed: ",
                    conditionMessage(e)), call. = FALSE)
            })
            numerical_solver <- ordinary$diagnostics
            solve_state <- ordinary$solve_state
        }
        coef <- ordinary$coefficients
        inverse_bread <- ordinary$inverse
        names(coef) <- coef_names
        direct_score <- matrix(0, n, p, dimnames = list(NULL, coef_names))
        A_kappa <- matrix(0, n, p, dimnames = list(NULL, coef_names))
        sse <- 0
        max_abs_residual <- 0
        for (first in starts) {
            take <- first:min(first + chunk_size - 1L, length(eo))
            if (!single) 
                z <- build_design(take)
            residual <- y[z$rows] - drop(z$D %*% coef)
            sse <- sse + sum(residual^2)
            max_abs_residual <- max(max_abs_residual, abs(residual))
            sc <- z$D * residual
            rs <- rowsum(sc, z$subject, reorder = FALSE)
            direct_score[as.integer(rownames(rs)), ] <- direct_score[as.integer(rownames(rs)), , drop = FALSE] + 
                rs
            if (include_theta) {
                G <- matrix(0, nrow(z$D), p)
                G[, idx_theta] <- z$W
                psi_G <- drop(z$W %*% coef[idx_theta])
                ak <- G * residual - z$D * psi_G
                rk <- rowsum(ak, z$subject, reorder = FALSE)
                A_kappa[as.integer(rownames(rk)), ] <- A_kappa[as.integer(rownames(rk)), , drop = FALSE] + 
                  rk
            }
        }
        if (is.null(inverse_bread))
            inverse_bread <- .cimehr_safe_solve(bread, name = "Spline Stage-3 bread")
        bread_scale <- sqrt(pmax(diag(bread), .Machine$double.eps))
        scaled_bread <- bread/outer(bread_scale, bread_scale)
        scaled_bread <- (scaled_bread + t(scaled_bread))/2
        bread_eigenvalues <- eigen(scaled_bread, symmetric = TRUE, only.values = TRUE)$values
        bread_largest <- max(abs(bread_eigenvalues))
        bread_smallest <- min(abs(bread_eigenvalues))
        relative_minimum_eigenvalue <- bread_smallest/max(bread_largest, .Machine$double.eps)
        scaled_condition_number <- bread_largest/max(bread_smallest, .Machine$double.eps)
        conditional_influence <- .cimehr_updated$.cimehr_stage3_apply_inverse(
            direct_score, inverse_bread, solve_state)
        colnames(conditional_influence) <- coef_names
        vcov_conditional <- crossprod(conditional_influence)
        dimnames(vcov_conditional) <- list(coef_names, coef_names)
        conditional_covariance_factor <- .cimehr_covariance_factor(conditional_influence)
        attr(conditional_covariance_factor, "scope") <- "Stage-3 patient influence; nuisance quantities held fixed"
        ans <- list(coef = coef, baseline = coef[idx_baseline], beta = coef[idx_fixed], trajectory = coef[idx_interaction], 
            theta = coef[idx_theta], index = list(baseline = idx_baseline, fixed = idx_fixed, trajectory = idx_interaction, 
                theta = idx_theta), bread = bread, inverse_bread = inverse_bread, direct_score = direct_score, 
            A_kappa = A_kappa, vcov_conditional = vcov_conditional,
            conditional_covariance_factor = conditional_covariance_factor,
            n_outcomes = length(eo), residual_sse = sse, 
            residual_df = length(eo) - p, residual_scale = sqrt(sse/max(1, length(eo) - p)), max_abs_residual = max_abs_residual, 
            design_rank = if (is.null(numerical_solver)) qr(scaled_bread)$rank else numerical_solver$design_rank,
            relative_minimum_eigenvalue = relative_minimum_eigenvalue,
            scaled_condition_number = scaled_condition_number, outcome_rows = eo, coefficient_names = coef_names)
        if (!is.null(numerical_solver)) ans$numerical_solver <- numerical_solver
        if (!is.null(solve_state)) ans$solve_state <- solve_state
        ans
    }

    .cimehr_random_modifiers <- function(covars, time_col, structure) {
        if (is.null(structure)) 
            return(setdiff(covars, time_col))
        if (!is.null(structure$columns)) 
            return(.cimehr_fixed_modifiers(covars, structure))
        setdiff(unique(c(structure$static_covs, structure$time_covs)), "(Intercept)")
    }

    .cimehr_spline_newdata_design <- function(object, newdata, times, type = c("population", "observed"), 
        kappa = NULL) {
        type <- match.arg(type)
        if (!is.data.frame(newdata) || !nrow(newdata)) {
            stop("`newdata` must be a nonempty data.frame.", call. = FALSE)
        }
        times <- as.numeric(times)
        if (!length(times) || any(!is.finite(times))) {
            stop("`times` must contain finite values.", call. = FALSE)
        }
        spec <- object$spline_spec
        fixed <- spec$fixed_covars
        missing <- setdiff(c(.cimehr_fixed_modifiers(fixed, spec$fixed_structure), spec$trajectory_covars, 
            if (type != "population") spec$random_covars else character()), names(newdata))
        if (length(missing)) {
            stop("`newdata` is missing: ", paste(missing, collapse = ", "), call. = FALSE)
        }
        X <- .cimehr_fixed_setup(newdata, fixed, spec$fixed_structure)
        training <- spec$training_XY_columns
        absent <- setdiff(training, colnames(X))
        if (length(absent)) {
            X <- cbind(X, matrix(0, nrow(X), length(absent), dimnames = list(NULL, absent)))
        }
        extra <- setdiff(colnames(X), training)
        if (length(extra)) {
            stop("`newdata` generated unexpected fixed-effect columns: ", paste(extra, collapse = ", "), 
                call. = FALSE)
        }
        if (!identical(colnames(X), training)) 
            X <- X[, training, drop = FALSE]
        profile <- rep(seq_len(nrow(newdata)), each = length(times))
        tt <- rep(times, times = nrow(newdata))
        tu <- unique(times)
        within <- function(b) all(tu >= b$boundary_knots[1L] & tu <= b$boundary_knots[2L])
        if (!any(times == 0 & 1/times < 0) && within(spec$baseline) && (!length(spec$trajectory_covars) || 
            within(spec$trajectory)) && .trajectory_reference_blas()) {
            at <- rep(match(times, tu), times = nrow(newdata))
            basis <- function(b) b[at, , drop = FALSE]
            x <- tu
        }
        else {
            basis <- identity
            x <- tt
        }
        B0 <- basis(.cimehr_spline_eval(x, spec$baseline))
        colnames(B0) <- paste0("baseline.", spec$baseline$column_names)
        Xlong <- .cimehr_fixed_rows(X, profile, tt)
        colnames(Xlong) <- if (ncol(Xlong)) {
            paste0("beta.", colnames(Xlong))
        }
        else character(0)
        GI <- matrix(0, length(tt), 0L)
        if (length(spec$trajectory_covars)) {
            Bc <- basis(.cimehr_spline_centered_eval(x, spec$trajectory, spec$reference_time))
            G <- as.matrix(newdata[, spec$trajectory_covars, drop = FALSE])[profile, , drop = FALSE]
            GI <- .cimehr_spline_interactions(Bc, G, spec$trajectory_covars)
        }
        D <- cbind(B0, Xlong, GI)
        link_coef <- object$compensation %||% object$theta
        if (length(link_coef)) {
            if (type == "population") {
                Theta <- matrix(0, length(tt), length(link_coef))
            }
            else {
                covars <- if (is.null(spec$random_structure$columns)) 
                  spec$random_covars
                else setdiff(names(spec$random_structure$columns), "(Intercept)")
                rs <- .cimehr_random_setup(newdata, covars, ".__unused_time__", spec$random_structure, 
                  spec$random_intercept %||% TRUE, validate_rank = FALSE)
                W0 <- rs$ZY
                if (!identical(colnames(W0), spec$training_ZY_columns)) 
                  stop("Newdata random-link coefficient columns differ from training data.", call. = FALSE)
                if (!is.null(spec$zero_compensation) &&
                    identical(spec$zero_compensation$kind, "quotient")) {
                    absent_zero <- setdiff(spec$zero_observation_covars, names(newdata))
                    if (length(absent_zero))
                        stop("`newdata` is missing observation-design covariates: ",
                            paste(absent_zero, collapse = ", "), call. = FALSE)
                    ZOnew <- .cimehr_exact_design(newdata, spec$zero_observation_covars, TRUE)
                    rz <- .cimehr_spline_zero_random(rs, spec$zero_compensation,
                        spec$reference_time, spec$time_scale, ZO = ZOnew)
                    W <- .cimehr_spline_random_design(profile, tt, rz$ZY, rz$use_time,
                        spec$reference_time, spec$time_scale)
                } else W <- .cimehr_spline_random_design(profile, tt, W0, rs$use_time, spec$reference_time,
                  spec$time_scale)
                if (is.null(kappa) || length(kappa) != nrow(newdata) || any(!is.finite(kappa))) 
                  stop("Observed-conditional prediction requires one finite kappa per newdata row.", 
                    call. = FALSE)
                Theta <- kappa[profile] * W
            }
            colnames(Theta) <- names(link_coef)
            D <- cbind(D, Theta)
        }
        if (anyDuplicated(colnames(D)) || !identical(colnames(D), names(object$coefficients))) 
            D <- D[, names(object$coefficients), drop = FALSE]
        list(D = D, profile = profile, time = tt)
    }

    .cimehr_spline_vcov <- function(object, ...) {
        if (is.null(object$vcov)) {
            stop("No covariance was requested; refit with `inference = 'stacked'` or `'conditional'`.", 
                call. = FALSE)
        }
        object$vcov
    }

    .cimehr_fixed_modifiers <- function(covars, structure) {
        if (is.null(structure)) 
            return(covars)
        setdiff(unique(unlist(lapply(structure$columns, function(z) as.character(z$modifier)), use.names = FALSE)), 
            "(Intercept)")
    }

    .cimehr_exact_design <- function(data, vars, intercept = TRUE) {
        if (length(vars) == 0L) {
            if (intercept) {
                return(matrix(1, nrow(data), 1L, dimnames = list(NULL, "(Intercept)")))
            }
            return(matrix(0, nrow(data), 0L))
        }
        f <- stats::reformulate(vars, intercept = intercept)
        stats::model.matrix(f, data = data, na.action = stats::na.pass)
    }

    .cimehr_fixed_setup <- function(subject_data, covars, structure = NULL) {
        if (is.null(structure)) 
            return(.cimehr_exact_design(subject_data, covars, intercept = FALSE))
        if (!is.list(structure$columns) || !identical(names(structure$columns), covars)) 
            stop("Fixed-path column names/order must match outcome fixed covariates.", call. = FALSE)
        origin <- structure$time_origin
        scale <- structure$time_scale
        if (length(origin) != 1L || !is.finite(origin) || length(scale) != 1L || !is.finite(scale) || 
            scale <= 0) 
            stop("Fixed-path clock requires a finite origin and positive scale.", call. = FALSE)
        A <- B <- matrix(0, nrow(subject_data), length(covars), dimnames = list(NULL, covars))
        for (j in seq_along(covars)) {
            z <- structure$columns[[j]]
            if (!is.data.frame(z) || !all(c("modifier", "power") %in% names(z)) || !nrow(z) || anyNA(z[, 
                c("modifier", "power")]) || any(!z$power %in% c(0, 1))) 
                stop("Each fixed-path column requires modifier/power terms with powers 0 or 1.", call. = FALSE)
            for (k in seq_len(nrow(z))) {
                v <- as.character(z$modifier[k])
                if (v == "(Intercept)") 
                  x <- rep(1, nrow(subject_data))
                else {
                  if (!v %in% names(subject_data) || !is.numeric(subject_data[[v]])) 
                    stop("Missing/non-numeric fixed-path modifier: ", v, call. = FALSE)
                  x <- subject_data[[v]]
                }
                if (any(!is.finite(x))) 
                  stop("Nonfinite fixed-path modifier: ", v, call. = FALSE)
                if (z$power[k] == 0) 
                  A[, j] <- A[, j] + x
                else B[, j] <- B[, j] + x
            }
        }
        attr(A, "cimehr_fixed_time") <- list(M = B, origin = origin, scale = scale)
        A
    }

    .cimehr_fixed_validate_rows <- function(XY, data, subject, time, covars) {
        at <- which(is.finite(time))
        if (!length(at)) 
            return(invisible(TRUE))
        expected <- .cimehr_fixed_rows(XY, subject[at], time[at])
        actual <- as.matrix(data[at, covars, drop = FALSE])
        if (any(!is.finite(actual)) || max(abs(actual - expected)) > 1e-08 * max(1, max(abs(expected)))) 
            stop("Observed outcome covariates disagree with the declared predictable paths.", call. = FALSE)
        invisible(TRUE)
    }

    .cimehr_random_setup <- function(subject_data, covars, time_col, structure = NULL, intercept = TRUE, 
        validate_rank = TRUE) {
        if (is.null(structure)) 
            return(list(ZY = .cimehr_exact_design(subject_data, setdiff(covars, time_col), intercept = intercept), 
                use_time = time_col %in% covars))
        if (!is.null(structure$columns)) {
            cols <- structure$columns
            if (isTRUE(intercept) && !"(Intercept)" %in% names(cols)) 
                cols <- c(list(`(Intercept)` = data.frame(modifier = "(Intercept)", power = 0)), cols)
            if (!isTRUE(intercept)) 
                cols <- cols[setdiff(names(cols), "(Intercept)")]
            expected <- c(if (isTRUE(intercept)) "(Intercept)", setdiff(covars, "(Intercept)"))
            if (!setequal(names(cols), expected) || anyDuplicated(names(cols))) 
                stop("Random-path columns must match the declared random covariates and intercept.", 
                  call. = FALSE)
            structure$columns <- cols[expected]
            A <- .cimehr_fixed_setup(subject_data, expected, structure)
            part <- attr(A, "cimehr_fixed_time")
            attr(A, "cimehr_fixed_time") <- NULL
            return(list(ZY = A, use_time = list(power = rep(0L, ncol(A)), slope = part$M, origin = part$origin, 
                scale = part$scale)))
        }
        a <- unique(as.character(structure$static_covs))
        m <- unique(as.character(structure$time_covs))
        if (isTRUE(intercept)) 
            a <- unique(c("(Intercept)", a))
        if (!isTRUE(intercept)) 
            a <- setdiff(a, "(Intercept)")
        origin <- structure$time_origin
        scale <- structure$time_scale
        if (length(origin) != 1L || !is.finite(origin) || length(scale) != 1L || !is.finite(scale) || 
            scale <= 0) 
            stop("Random-effect clock requires finite origin and positive scale.", call. = FALSE)
        make <- function(v) .cimehr_exact_design(subject_data, setdiff(v, "(Intercept)"), intercept = "(Intercept)" %in% 
            v)
        A <- make(a)
        M <- make(m)
        mn <- colnames(M)
        mn <- ifelse(mn %in% c("Intercept", "(Intercept)"), "time01", paste0(mn, "_time01"))
        colnames(M) <- mn
        ZY <- cbind(A, M)
        if (!ncol(ZY)) 
            stop("At least one outcome random-link column is required.", call. = FALSE)
        if (any(!is.finite(ZY))) 
            stop("Nonfinite random-link modifiers.", call. = FALSE)
        for (B in list(A, M)) if (isTRUE(validate_rank) && ncol(B) && qr(B)$rank < ncol(B)) 
            stop("Random-link modifier block is rank deficient.", call. = FALSE)
        list(ZY = ZY, use_time = list(power = c(rep(0L, ncol(A)), rep(1L, ncol(M))), origin = origin, 
            scale = scale))
    }

    .cimehr_random_validate_rows <- function(setup, data, subject, time, structure) {
        if (is.null(structure$columns)) 
            return(invisible(TRUE))
        rp <- .cimehr_random_parts(setup$ZY, setup$use_time)
        at <- which(is.finite(time))
        covs <- setdiff(colnames(setup$ZY), "(Intercept)")
        if (!length(at) || !length(covs)) 
            return(invisible(TRUE))
        actual <- as.matrix(data[at, covs, drop = FALSE])
        expected <- .cimehr_random_rows(rp, subject[at], time[at])[, covs, drop = FALSE]
        if (any(!is.finite(actual)) || max(abs(actual - expected)) > 1e-08 * max(1, max(abs(expected)))) 
            stop("Observed random covariates disagree with the declared predictable paths.", call. = FALSE)
        invisible(TRUE)
    }

    .cimehr_random_parts <- function(ZY, use_time, origin = 0, scale = 1) {
        if (is.list(use_time)) 
            return(list(M = ZY, power = use_time$power, slope = use_time$slope, origin = use_time$origin, 
                scale = use_time$scale))
        if (isTRUE(use_time)) {
            p <- ncol(ZY)
            return(list(M = cbind(ZY, time = rep(1, nrow(ZY))), power = c(rep(0L, p), 1L), origin = origin, 
                scale = scale))
        }
        list(M = ZY, power = rep(0L, ncol(ZY)), origin = origin, scale = scale)
    }

    .cimehr_spline_eval <- function(x, spec) {
        .cimehr_spline_basis(x = x, boundary_knots = spec$boundary_knots, type = spec$type, intercept = spec$intercept, 
            degree = spec$degree, spec = spec)
    }

    .cimehr_fixed_rows <- function(XY, subject, time) {
        D <- XY[subject, , drop = FALSE]
        part <- attr(XY, "cimehr_fixed_time")
        if (!is.null(part)) 
            D <- D + part$M[subject, , drop = FALSE] * ((time - part$origin)/part$scale)
        D
    }

    .cimehr_spline_centered_eval <- function(x, spec, reference_time) {
        B <- .cimehr_spline_eval(x, spec)
        b0 <- .cimehr_spline_eval(reference_time, spec)
        sweep(B, 2L, b0[1L, ], "-")
    }

    .cimehr_spline_interactions <- function(B, G, covariate_names) {
        B <- as.matrix(B)
        G <- as.matrix(G)
        if (!ncol(G)) 
            return(matrix(0, nrow(B), 0L))
        if (nrow(B) != nrow(G)) 
            stop("Spline interaction rows disagree.", call. = FALSE)
        out <- vector("list", ncol(G))
        for (j in seq_len(ncol(G))) {
            out[[j]] <- B * G[, j]
            colnames(out[[j]]) <- paste0("trajectory.", covariate_names[j], ".", colnames(B))
        }
        do.call(cbind, out)
    }

    .cimehr_spline_random_design <- function(subject, time, ZY_subject, use_time, reference_time, time_scale) {
        rp <- .cimehr_random_parts(ZY_subject, use_time, reference_time, time_scale)
        .cimehr_random_rows(rp, subject, time)
    }

    .cimehr_random_rows <- function(parts, subject, time) {
        W <- parts$M[subject, , drop = FALSE]
        j <- which(parts$power == 1L)
        if (length(j)) 
            W[, j] <- W[, j, drop = FALSE] * ((time - parts$origin)/parts$scale)
        if (!is.null(parts$slope)) 
            W <- W + parts$slope[subject, , drop = FALSE] * ((time - parts$origin)/parts$scale)
        W
    }

.cimehr_spline_zero_random <- function(rs, spec, reference_time, time_scale, ZO = NULL) {
  rp <- .cimehr_random_parts(rs$ZY, rs$use_time, reference_time, time_scale)
  W0 <- rp$M
  W1 <- matrix(0, nrow(W0), ncol(W0), dimnames=dimnames(W0))
  slopes <- which(rp$power == 1L)
  if(length(slopes)) {
    W1[,slopes] <- rp$M[,slopes,drop=FALSE]/rp$scale
    W0[,slopes] <- -rp$origin*W1[,slopes,drop=FALSE]
  }
  if(!is.null(rp$slope)) {
    W1 <- W1 + rp$slope/rp$scale
    W0 <- W0 - rp$origin*rp$slope/rp$scale
  }
  out <- .cimehr_updated$.lu_zero_compensation(spec,W0,W1,ZO=ZO)
  list(ZY=out$W0,use_time=list(power=rep(0L,ncol(out$W0)),
    slope=out$W1,origin=0,scale=1),zero_spec=out$spec)
}

.cimehr_spline_nuisance_if <- function(nuisance, derivative_step=2e-5) {
    stage1<-nuisance$stage1;pcl<-nuisance$pcl;XV<-nuisance$XV;XO<-nuisance$XO;ZO<-nuisance$ZO
    posterior<-nuisance$posterior;include_delta<-nuisance$include_delta
    .lu_inverse_information<-.cimehr_updated$.lu_inverse_information
    .lu_laplace<-.cimehr_updated$.lu_laplace
    .lu_functionals<-.cimehr_updated$.lu_functionals
    .cimehr_shared_engine<-.cimehr_common
        if (length(derivative_step) != 1L || !is.finite(derivative_step) || derivative_step <= 0 || derivative_step >= 
            0.1) {
            stop("derivative_step must be between zero and 0.1.", call. = FALSE)
        }
        n <- length(stage1$mi)
        par <- pcl$par
        p <- length(par)
        if (!isTRUE(pcl$identified)) {
            stop("Stacked inference requires an identified fitted Stage-2 free face.", call. = FALSE)
        }
        free <- pcl$free
        if (is.null(free)) 
            free <- pcl$free_indices
        if (is.logical(free)) {
            if (length(free) != p || anyNA(free)) 
                stop("Invalid free-parameter mask.", call. = FALSE)
            free <- which(free)
        }
        if (is.character(free)) 
            free <- match(free, names(par))
        if (is.null(free)) {
            free <- which(!(startsWith(names(par), "sigma2_q.") & par == 0))
        }
        if (anyNA(free) || anyDuplicated(free) || any(free < 1L | free > p)) {
            stop("Invalid Stage-2 free-parameter indices.", call. = FALSE)
        }
        free <- as.integer(free)
        fixed <- setdiff(seq_len(p), free)
        if (!isTRUE(pcl$zero_quotient) && length(fixed) &&
            any(!startsWith(names(par)[fixed], "sigma2_q.") | par[fixed] != 0)) {
            stop("Only zero variance components may be fixed on the inference face.", call. = FALSE)
        }
        p_free <- length(free)
        eta_names <- names(par)[free]
        H <- as.matrix(pcl$hessian)
        if (all(dim(H) == c(p, p))) 
            H <- H[free, free, drop = FALSE]
        if (!all(dim(H) == c(p_free, p_free))) {
            stop("Stage-2 Hessian does not match the free-parameter face.", call. = FALSE)
        }
        Hinv <- .lu_inverse_information(H, "Stage-2 negative-loglikelihood Hessian")
        dimnames(Hinv) <- list(eta_names, eta_names)
        score_at <- function(post) {
            result <- pcl$eval(par, mu = post$mu, s2 = post$s2, scores = TRUE)
            score <- as.matrix(result$subject_score)
            if (!all(dim(score) == c(n, p)) || any(!is.finite(score)) || length(result$value) != 1L || 
                !is.finite(result$value)) {
                stop("Stage-2 evaluator did not return finite n-by-p positive loglikelihood scores.", 
                  call. = FALSE)
            }
            score <- score[, free, drop = FALSE]
            colnames(score) <- eta_names
            score
        }
        base_score <- score_at(posterior)
        f0 <- .lu_functionals(par, XO, ZO, posterior, include_delta)
        s1 <- .cimehr_shared_engine$.cimehr_stage1_case_linearization(stage1, XV)
        a <- stage1$sigma2_zeta
        B <- sum(stage1$nu^2)
        A <- sum(stage1$mi * (stage1$mi - 1))
        if (!is.finite(B) || B <= 0 || !is.finite(A)) {
            stop("Frailty moment sums are not finite and estimable.", call. = FALSE)
        }
        frailty_regular <- is.finite(a) && a > 0 && A > B
        if (frailty_regular) {
            ea <- exp(a)
            bread_sigma2 <- ea * B
            h <- stage1$mi * (stage1$mi - 1) - ea * stage1$nu^2
            moment_nu <- -2 * ea * stage1$nu
            moment_nu_chain <- s1$apply_nu(matrix(moment_nu, ncol = 1L))[, 1L]
            sigma_effective <- h + moment_nu_chain
            if_sigma <- sigma_effective/bread_sigma2
        }
        else {
            h <- stage1$mi * (stage1$mi - 1) - stage1$nu^2
            moment_nu <- moment_nu_chain <- sigma_effective <- if_sigma <- numeric(n)
            bread_sigma2 <- NA_real_
        }
        D_nu <- matrix(0, n, p_free, dimnames = dimnames(base_score))
        D_sigma <- D_nu
        fd <- list(omega_nu = numeric(n), kappa_nu = numeric(n), omega_tau = numeric(n), kappa_tau = numeric(n), 
            omega_eta = matrix(0, n, p_free, dimnames = list(NULL, eta_names)), kappa_eta = matrix(0, 
                n, p_free, dimnames = list(NULL, eta_names)))
        if (frailty_regular) {
            nu <- stage1$nu
            plus_nu <- nu * exp(derivative_step)
            minus_nu <- nu * exp(-derivative_step)
            zero_nu <- nu == 0
            plus_nu[zero_nu] <- derivative_step
            minus_nu[zero_nu] <- 0
            denom_nu <- plus_nu - minus_nu
            if (any(!is.finite(denom_nu)) || any(denom_nu <= 0)) {
                stop("Exposure derivative perturbations cannot be represented numerically.", call. = FALSE)
            }
            post_plus <- .lu_laplace(stage1$mi, plus_nu, a)
            post_minus <- .lu_laplace(stage1$mi, minus_nu, a)
            D_nu <- (score_at(post_plus) - score_at(post_minus))/denom_nu
            fp <- .lu_functionals(par, XO, ZO, post_plus, include_delta)
            fm <- .lu_functionals(par, XO, ZO, post_minus, include_delta)
            fd$omega_nu <- (fp$omega - fm$omega)/denom_nu
            fd$kappa_nu <- (fp$kappa - fm$kappa)/denom_nu
            plus_a <- a * exp(derivative_step)
            minus_a <- a * exp(-derivative_step)
            denom_a <- plus_a - minus_a
            if (!is.finite(denom_a) || denom_a <= 0) {
                stop("Frailty derivative perturbations cannot be represented numerically.", call. = FALSE)
            }
            post_plus <- .lu_laplace(stage1$mi, nu, plus_a)
            post_minus <- .lu_laplace(stage1$mi, nu, minus_a)
            D_sigma <- (score_at(post_plus) - score_at(post_minus))/denom_a
            fp <- .lu_functionals(par, XO, ZO, post_plus, include_delta)
            fm <- .lu_functionals(par, XO, ZO, post_minus, include_delta)
            fd$omega_tau <- (fp$omega - fm$omega)/denom_a
            fd$kappa_tau <- (fp$kappa - fm$kappa)/denom_a
        }
        for (j in seq_along(free)) {
            k <- free[j]
            step <- derivative_step * max(1, abs(par[k]))
            if (startsWith(names(par)[k], "sigma2_q.")) 
                step <- min(step, par[k]/2)
            if (!is.finite(step) || step <= 0 || par[k] + step == par[k] || par[k] - step == par[k]) {
                stop("Observation-functional derivative step is not representable.", call. = FALSE)
            }
            plus_par <- minus_par <- par
            plus_par[k] <- par[k] + step
            minus_par[k] <- par[k] - step
            fp <- .lu_functionals(plus_par, XO, ZO, posterior, include_delta)
            fm <- .lu_functionals(minus_par, XO, ZO, posterior, include_delta)
            fd$omega_eta[, j] <- (fp$omega - fm$omega)/(2 * step)
            fd$kappa_eta[, j] <- (fp$kappa - fm$kappa)/(2 * step)
        }
        derivative_values <- c(D_nu, D_sigma, unlist(fd, use.names = FALSE))
        if (any(!is.finite(derivative_values)) || any(!is.finite(if_sigma))) {
            stop("A downstream derivative or frailty influence is nonfinite.", call. = FALSE)
        }
        s2_nu_chain <- s1$apply_nu(D_nu)
        s2_sigma_chain <- outer(if_sigma, colSums(D_sigma))
        s2_effective <- base_score + s2_nu_chain + s2_sigma_chain
        if_eta <- s2_effective %*% t(Hinv)
        colnames(if_eta) <- eta_names

    list(s1=s1,if_sigma=if_sigma,if_eta=if_eta,fd=fd,free=free,fixed=fixed,
      frailty_regular=frailty_regular,base_score=base_score,s2_effective=s2_effective,
      derivative_nu=D_nu,derivative_sigma2=D_sigma,Hinv=Hinv)
}

.cimehr_spline_nuisance_fit <- function(data, id_col, time_col, y_col, r_col, censor_col,
    covars_visit_XV, covars_obs_fixed_XO, covars_obs_random_link_ZO,
    time_precision = 0, pairwise_control = list(), entry_col = NULL, check = NULL) {
  up <- .cimehr_updated
  if(is.null(check)) check <- up$.lu_validate_data(data,id_col,time_col,y_col,r_col,censor_col,
    unique(c(covars_visit_XV,covars_obs_fixed_XO,covars_obs_random_link_ZO)),entry_col=entry_col)
  sf <- check$sf; subject <- check$subject; visit <- check$visit
  XV <- up$.lu_design(data,sf,covars_visit_XV)
  XO <- up$.lu_design(data,sf,covars_obs_fixed_XO,TRUE)
  ZO <- up$.lu_design(data,sf,covars_obs_random_link_ZO,TRUE)
  if(ncol(XV)&&qr(cbind(1,XV))$rank!=ncol(XV)+1L)
    stop('Visiting design is rank deficient or contains a baseline-confounded intercept.',call.=FALSE)
  stage1 <- .cimehr_fit_visiting(data[[time_col]],visit,subject,data[[censor_col]][sf],
    XV,time_precision=time_precision,score_tol=1e-10,Ei_subj=check$Ei)
  if(!isTRUE(stage1$converged)) stop('Visiting regression did not converge.',call.=FALSE)
  stage1$frailty_method <- 'factorial-moment estimator'
  posterior <- up$.lu_laplace(stage1$mi,stage1$nu,stage1$sigma2_zeta)
  include_delta <- TRUE
  s <- tabulate(subject[visit & data[[r_col]]==1],length(sf))
  original_ZO <- ZO
  zero_spec <- NULL
  if(isTRUE(stage1$sigma2_zeta==0)) {
    zero <- up$.lu_zero_observation(XO,ZO,stage1$mi,s,posterior,pairwise_control)
    pcl <- zero$pcl;ZO <- zero$ZO;include_delta <- zero$include_delta
    zero_spec <- zero$zero_spec
  } else pcl <- up$.lu_fit_pairwise(XO,ZO,stage1$mi,s,posterior,include_delta=include_delta,control=pairwise_control)
  if(!isTRUE(pcl$converged)) stop('Pairwise observation fit did not converge.',call.=FALSE)
  if(!isTRUE(pcl$identified)) stop('Pairwise observation parameters fail local identification/information checks.',call.=FALSE)
  functionals <- up$.lu_functionals(pcl$par,XO,ZO,posterior,include_delta)
  if(any(functionals$omega<=0)||any(functionals$omega>1))
    stop('Stage-3 observation probabilities must satisfy 0 < omega <= 1.',call.=FALSE)
  contract <- c(list(id=data[[id_col]],time=data[[time_col]],R=data[[r_col]],C=data[[censor_col]],
    XV=XV,XO=XO,ZO=original_ZO,time_precision=time_precision,pairwise_control=pairwise_control),
    if(!is.null(entry_col))list(E=data[[entry_col]]))
  out <- list(stage1=stage1,posterior=posterior,pcl=pcl,functionals=functionals,
    XV=XV,XO=XO,ZO=ZO,subject=subject,first=sf,visit_mask=visit,ids=check$ids,
    include_delta=include_delta,contract=contract,
    model_info=list(n_subjects=length(sf),fit_shared_loading=include_delta,
      frailty_estimator='method of moments',laplace_posterior=TRUE,time_precision=time_precision,
      observation_covariance='diagonal conditional covariance of q given U',
      nuisance_algorithm='CIMEHR MOM/Laplace Stages 1 and 2'))
  if(!is.null(zero_spec)) {
    out$zero_spec <- zero_spec
    out$model_info$fit_shared_loading <- !identical(zero_spec$kind,'quotient')
    out$model_info$compensation_parameterization <- zero_spec$kind
    if(identical(zero_spec$kind,'quotient'))
      out$model_info$observation_covariance <- 'total observation random-effect variance in identified coordinates'
  }
  class(out) <- 'cimehr_laplace_nuisance'
  out
}

.cimehr_spline_nuisance_summary <- function(fit,stacked=NULL) {
  s<-fit$stage1;p<-fit$pcl;ids<-fit$ids
  Vg<-if(is.null(stacked))matrix(NA_real_,ncol(fit$XV),ncol(fit$XV),
    dimnames=list(colnames(fit$XV),colnames(fit$XV))) else stacked$component_vcov$gamma
  dimnames(Vg)<-list(colnames(fit$XV),colnames(fit$XV))
  Vo<-if(is.null(stacked))matrix(NA_real_,length(p$par),length(p$par),
    dimnames=list(names(p$par),names(p$par))) else stacked$component_vcov$observation
  seg<-.cimehr_checked_se(Vg);seo<-.cimehr_checked_se(Vo)
  va<-if(is.null(stacked))NA_real_ else stacked$component_vcov$sigma2_zeta[1,1]
  list(visiting_process=c(list(gamma_V=s$gamma,se_gamma=setNames(seg,names(s$gamma)),vcov_gamma=Vg,
    sigma2_zeta=s$sigma2_zeta,se_sigma2_zeta=.cimehr_checked_se(va),
    sigma2_zeta_mom=s$sigma2_zeta,frailty_variance_method='factorial-moment estimator',
    frailty_boundary=s$sigma2_zeta==0,convergence=if(isTRUE(s$converged))0L else 1L,
    max_abs_score=s$max_abs_score,mi=setNames(s$mi,ids),Ci=setNames(s$Ci,ids),
    Lambda0_hat=setNames(s$dLambda,s$unique_times),
    mu_Ui=setNames(fit$posterior$mu,ids),sigma2_Ui=setNames(fit$posterior$s2,ids)),
    if(!is.null(fit$contract$E))list(Ei=setNames(s$Ei,ids))),
    observation_process=list(alpha_fixed=p$alpha,delta_latent_random_link=p$delta,Sigma_q=p$sigma2_q,
      se_alpha=setNames(seo[paste0('alpha.',names(p$alpha))],names(p$alpha)),
      se_delta=setNames(seo[paste0('delta.',names(p$delta))],names(p$delta)),
      se_Sigma_q=setNames(seo[paste0('sigma2_q.',names(p$sigma2_q))],names(p$sigma2_q)),
      vcov_stacked=Vo,vcov_obs=Vo,objective=p$diagnostics$objective,
      convergence=if(isTRUE(p$converged))0L else 1L,optimization_converged=p$converged,
      observation_variance_boundary=any(p$boundary),active_variance=!p$boundary,
      boundary_variance=p$boundary,boundary_components=names(p$boundary)[p$boundary],
      identification=list(identified=p$identified),pair_weight=p$diagnostics$pair_weight,
      pairwise_diagnostics=p$diagnostics),model_info=fit$model_info,
    numerical_diagnostics=list(laplace_posterior=TRUE,
      maximum_mode_residual=fit$posterior$maximum_mode_residual))
}

.cimehr_spline_stacked <- function(nuisance,stage3,derivative_step=2e-5,retain_influence=FALSE) {
  z <- .cimehr_spline_nuisance_if(nuisance,derivative_step)
  A <- stage3$A_kappa;fd <- z$fd
  Dnu <- A*fd$kappa_nu
  Da <- colSums(A*fd$kappa_tau)
  Deta <- t(crossprod(fd$kappa_eta,A))
  nu_chain <- z$s1$apply_nu(Dnu)
  sigma_chain <- outer(z$if_sigma,Da)
  eta_chain <- z$if_eta%*%t(Deta)
  effective <- stage3$direct_score+nu_chain+sigma_chain+eta_chain
  influence <- .cimehr_updated$.cimehr_stage3_apply_inverse(
    effective,stage3$inverse_bread,stage3$solve_state)
  colnames(influence)<-names(stage3$coef)
  scaled_sum<-function(M) {
    M<-as.matrix(M);if(!length(M))return(0)
    max(abs(colSums(M)))/max(1,sqrt(sum(M^2)))
  }
  raw_sums<-c(gamma=scaled_sum(z$s1$influence_gamma),sigma2_zeta=scaled_sum(z$if_sigma),
    observation=scaled_sum(z$if_eta),outcome=scaled_sum(influence))
  if(any(!is.finite(c(influence,z$if_eta,z$s1$influence_gamma,raw_sums)))||
    max(raw_sums)>max(1e-4,100*derivative_step))
    stop('Spline stacked influence fails the uniform-case-weight invariance check.',call.=FALSE)
  center<-function(M){M<-as.matrix(M);if(ncol(M))sweep(M,2L,colMeans(M),'-') else M}
  ig<-center(z$s1$influence_gamma)
  if(ncol(ig))colnames(ig)<-paste0('gamma.',colnames(nuisance$XV))
  ia<-as.numeric(center(z$if_sigma));ie<-center(z$if_eta);iy<-center(influence)
  io<-matrix(0,nrow(iy),length(nuisance$pcl$par),dimnames=list(NULL,names(nuisance$pcl$par)))
  io[,z$free]<-ie
  joint<-cbind(ig,sigma2_zeta=ia,io,iy)
  if(anyDuplicated(colnames(joint)))stop('Joint parameter names must be unique.',call.=FALSE)
  unavailable<-c(if(!z$frailty_regular)'sigma2_zeta',names(nuisance$pcl$par)[z$fixed])
  V<-crossprod(iy);Vjoint<-crossprod(joint);Vo<-crossprod(io)
  if(length(unavailable)){Vjoint[unavailable,]<-NA_real_;Vjoint[,unavailable]<-NA_real_}
  if(length(z$fixed)){Vo[z$fixed,]<-NA_real_;Vo[,z$fixed]<-NA_real_}
  Va<-matrix(if(z$frailty_regular)sum(ia^2) else NA_real_,1L,1L,
    dimnames=list('sigma2_zeta','sigma2_zeta'))
  frailty_statistic<-if(z$frailty_regular)nuisance$stage1$sigma2_zeta/sqrt(sum(ia^2)) else NA_real_
  frailty_weak<-isTRUE(frailty_statistic<=sqrt(log(nrow(iy))))
  regular<-z$frailty_regular&&nuisance$include_delta&&!length(z$fixed)
  inference_scope<-if(regular)'all-stage MOM/Laplace spline sandwich for the interior estimator' else
    'all-stage MOM/Laplace spline sandwich conditional on the selected frailty/variance face; boundary variance Wald inference unavailable'
  if(frailty_weak)inference_scope<-paste0(inference_scope,
    '; the positive frailty variance has a weak standardized signal relative to zero (descriptive diagnostic); computed covariance, SEs and intervals are retained')
  covariance_factor<-.cimehr_covariance_factor(iy)
  attr(covariance_factor,'scope')<-inference_scope
  ans<-list(regular=regular,frailty_parameter_regular=z$frailty_regular,
    observation_variance_regular=!length(z$fixed),frailty_weak_identification=frailty_weak,
    frailty_identification=c(statistic=frailty_statistic,threshold=sqrt(log(nrow(iy)))),
    conditional_active_face_inference_available=TRUE,
    scope=inference_scope,
    approximation_scope='Gaussian/Laplace approximation bias is not removed by this sandwich',
    parameterization='direct raw variance coordinates, including sigma2_zeta',vcov=V,vcov_joint=Vjoint,
    covariance_factor=covariance_factor,
    unavailable_wald_parameters=unavailable,
    diagnostics=list(derivative_step=derivative_step,raw_influence_case_sums=raw_sums,
      frailty_regular=z$frailty_regular,observation_variance_regular=!length(z$fixed),
      conditional_active_face_inference_available=TRUE,centered_influences=TRUE,
      functional_tau_coordinate='raw sigma2_zeta'),
    component_vcov=list(gamma=crossprod(ig),sigma2_zeta=Va,observation=Vo))
  if(retain_influence) {
    ans$influence_joint_active_face<-joint
    if(length(unavailable))joint[,unavailable]<-NA_real_
    ans$influence_joint<-joint;ans$influence_outcome<-iy;ans$effective_score_outcome<-effective
    ans$chains<-list(nu=nu_chain,sigma2=sigma_chain,observation=eta_chain)
    ans$components<-list(stage1=z$s1,frailty=list(influence_sigma2=ia),
      stage2=list(influence_eta=ie,free=z$free),functionals=fd,
      stage3=list(derivative_nu=Dnu,derivative_sigma2=Da,derivative_eta=Deta))
  }
  ans
}

.cimehr_spline_native <- function(data,id_col='id',time_col='time',y_col='Y',r_col='R',
    censor_col='C',covars_visit_XV=NULL,covars_outcome_fixed_XY=NULL,outcome_fixed_structure=NULL,
    covars_outcome_random_link_ZY=NULL,outcome_random_structure=NULL,outcome_random_intercept=TRUE,
    covars_obs_fixed_XO=NULL,covars_obs_random_link_ZO=NULL,trajectory_covars=NULL,
    spline_type=c('natural','bspline'),baseline_df=5L,trajectory_df=3L,baseline_knots=NULL,
    trajectory_knots=NULL,boundary_knots=NULL,reference_time=NULL,spline_degree=3L,
    stage3_chunk_size=100000L,inference=c('stacked','conditional','none'),nuisance_fit=NULL,
    retain_nuisance_fit=FALSE,retain_influence=FALSE,entry_col=NULL,...) {
  call<-match.call();spline_type<-match.arg(spline_type);inference<-match.arg(inference)
  dots<-list(...)
  allowed<-c('time_precision','pairwise_control','derivative_step')
  if(length(dots)&&(is.null(names(dots))||any(!nzchar(names(dots)))||anyDuplicated(names(dots))))
    stop('Additional spline controls must have unique explicit names.',call.=FALSE)
  bad<-setdiff(names(dots),allowed)
  if(length(bad))stop('Unsupported MOM/Laplace spline control(s): ',paste(bad,collapse=', '),
    '. Supported controls: time_precision, pairwise_control, derivative_step.',call.=FALSE)
  time_precision<-dots$time_precision %||% 0
  pairwise_control<-dots$pairwise_control %||% list()
  derivative_step<-dots$derivative_step %||% 2e-5
  if(length(time_precision)!=1L||!is.finite(time_precision)||time_precision<0)
    stop('time_precision must be a finite nonnegative scalar.',call.=FALSE)
  if(length(derivative_step)!=1L||!is.finite(derivative_step)||derivative_step<=0||derivative_step>=.1)
    stop('derivative_step must be finite and between 0 and 0.1.',call.=FALSE)
  for(flag in list(outcome_random_intercept,retain_nuisance_fit,retain_influence))
    if(!is.logical(flag)||length(flag)!=1L||is.na(flag))stop('Logical controls must be one nonmissing logical.',call.=FALSE)
  data<-as.data.frame(data)
  covars_visit_XV<-.check_covars(covars_visit_XV,data,'covars_visit_XV')
  covars_outcome_fixed_XY<-.check_covars(covars_outcome_fixed_XY,data,'covars_outcome_fixed_XY')
  covars_outcome_random_link_ZY<-.check_covars(covars_outcome_random_link_ZY,data,'covars_outcome_random_link_ZY')
  covars_obs_fixed_XO<-.check_covars(covars_obs_fixed_XO,data,'covars_obs_fixed_XO')
  covars_obs_random_link_ZO<-.check_covars(covars_obs_random_link_ZO,data,'covars_obs_random_link_ZO')
  trajectory_covars<-.check_covars(trajectory_covars,data,'trajectory_covars')
  if(length(setdiff(trajectory_covars,covars_outcome_fixed_XY)))
    stop('trajectory_covars must be outcome fixed covariates.',call.=FALSE)
  if(!is.null(outcome_fixed_structure)&&length(trajectory_covars)) {
    dynamic<-names(Filter(function(x)any(x$power==1),outcome_fixed_structure$columns))
    if(length(intersect(dynamic,trajectory_covars)))stop('Time-varying fixed paths cannot also be spline trajectory modifiers.',call.=FALSE)
  }
  static<-unique(c(covars_visit_XV,covars_obs_fixed_XO,covars_obs_random_link_ZO,
    .cimehr_fixed_modifiers(covars_outcome_fixed_XY,outcome_fixed_structure),
    .cimehr_random_modifiers(covars_outcome_random_link_ZY,time_col,outcome_random_structure)))
  check<-.cimehr_updated$.lu_validate_data(data,id_col,time_col,y_col,r_col,censor_col,static,entry_col=entry_col)
  subject_data<-data[check$sf,,drop=FALSE];n<-length(check$sf)
  XY<-.cimehr_fixed_setup(subject_data,covars_outcome_fixed_XY,outcome_fixed_structure)
  if(length(covars_outcome_fixed_XY)&&(!is.null(outcome_fixed_structure)||!identical(colnames(XY),covars_outcome_fixed_XY)||
      nrow(XY)!=n||!isTRUE(all(XY==as.matrix(subject_data[,covars_outcome_fixed_XY,drop=FALSE])))))
    .cimehr_fixed_validate_rows(XY,data,check$subject,data[[time_col]],covars_outcome_fixed_XY)
  rs<-.cimehr_random_setup(subject_data,covars_outcome_random_link_ZY,time_col,outcome_random_structure,outcome_random_intercept)
  .cimehr_random_validate_rows(rs,data,check$subject,data[[time_col]],outcome_random_structure)
  started<-proc.time()[['elapsed']];reused<-!is.null(nuisance_fit)
  if(reused) {
    if(!inherits(nuisance_fit,'cimehr_laplace_nuisance'))stop('nuisance_fit must be a retained MOM/Laplace spline nuisance fit.',call.=FALSE)
    up<-.cimehr_updated
    contract<-c(list(id=data[[id_col]],time=data[[time_col]],R=data[[r_col]],C=data[[censor_col]],
      XV=up$.lu_design(data,check$sf,covars_visit_XV),XO=up$.lu_design(data,check$sf,covars_obs_fixed_XO,TRUE),
      ZO=up$.lu_design(data,check$sf,covars_obs_random_link_ZO,TRUE),time_precision=time_precision,pairwise_control=pairwise_control),
      if(!is.null(entry_col))list(E=data[[entry_col]]))
    if(!identical(contract,nuisance_fit$contract))stop('nuisance_fit data, designs or nuisance controls differ.',call.=FALSE)
    if(isTRUE(nuisance_fit$stage1$sigma2_zeta==0)) {
      zero <- nuisance_fit$zero_spec
      valid_zero <- !is.null(zero) && identical(zero$schema,3L) &&
        zero$kind %in% c('quotient','raw_unique') &&
        isTRUE(nuisance_fit$posterior$sigma2_zeta==0) &&
        length(nuisance_fit$posterior$mu)==n && all(nuisance_fit$posterior$mu==0) &&
        length(nuisance_fit$posterior$s2)==n && all(nuisance_fit$posterior$s2==1) &&
        length(nuisance_fit$pcl$par)>0L && all(is.finite(nuisance_fit$pcl$par))
      quotient_marker <- !identical(zero$kind,'quotient') ||
        isTRUE(attr(nuisance_fit$pcl$par,'zero_compensation'))
      if(!isTRUE(valid_zero) || !isTRUE(quotient_marker))
        up$.lu_zero_frailty_stop(nuisance_fit$posterior,'incompatible_zero_frailty_nuisance_refit_required')
      if(!isTRUE(nuisance_fit$pcl$identified))
        up$.lu_zero_frailty_stop(nuisance_fit$posterior,'stage2_sensitivity_not_identified')
    }
  } else nuisance_fit<-.cimehr_spline_nuisance_fit(data,id_col,time_col,y_col,r_col,censor_col,
    covars_visit_XV,covars_obs_fixed_XO,covars_obs_random_link_ZO,time_precision,pairwise_control,entry_col=entry_col,check=check)
  nuisance_elapsed<-proc.time()[['elapsed']]-started
  boundary_given<-!is.null(boundary_knots);baseline_given<-!is.null(baseline_knots)
  trajectory_given<-!is.null(trajectory_knots)
  all_times<-data[[time_col]][check$visit]
  if(is.null(boundary_knots))boundary_knots<-range(c(0,all_times,data[[censor_col]][check$sf]),finite=TRUE)
  if(is.null(reference_time))reference_time<-boundary_knots[1]
  if(length(reference_time)!=1L||!is.finite(reference_time)||reference_time<min(boundary_knots)||reference_time>max(boundary_knots))
    stop('reference_time must lie inside boundary_knots.',call.=FALSE)
  baseline_spec<-.cimehr_spline_spec(all_times,df=baseline_df,knots=baseline_knots,
    boundary_knots=boundary_knots,type=spline_type,intercept=TRUE,degree=spline_degree)
  trajectory_spec<-if(length(trajectory_covars)).cimehr_spline_spec(all_times,df=trajectory_df,
    knots=trajectory_knots,boundary_knots=boundary_knots,type=spline_type,intercept=FALSE,degree=spline_degree) else NULL
  G<-if(length(trajectory_covars))as.matrix(subject_data[,trajectory_covars,drop=FALSE]) else matrix(0,n,0L)
  time_scale<-diff(sort(boundary_knots));fully_fixed<-boundary_given&&baseline_given&&(!length(trajectory_covars)||trajectory_given)
  started<-proc.time()[['elapsed']]
  fit_rs <- rs
  zero_spec <- nuisance_fit$zero_spec
  if(!is.null(zero_spec)) {
    fixed_time <- attr(XY, 'cimehr_fixed_time')
    outcome_modifiers <- cbind(Intercept=1, XY,
      if(!is.null(fixed_time)) fixed_time$M else matrix(0,n,0L), G)
    zero_spec <- .cimehr_updated$.lu_zero_compensation_rule(zero_spec, nuisance_fit$XO,
      outcome_modifiers, nuisance_fit$pcl, nuisance_fit$posterior,
      nuisance_fit$stage1$mi>0, n)
    fit_rs <- .cimehr_spline_zero_random(rs,zero_spec,reference_time,time_scale)
    zero_spec <- fit_rs$zero_spec
  }
  stage3<-.cimehr_spline_stage3(data[[y_col]],data[[r_col]],data[[time_col]],check$visit,check$subject,
    XY,G,trajectory_covars,fit_rs$ZY,fit_rs$use_time,nuisance_fit$functionals$kappa,
    if(is.null(zero_spec))nuisance_fit$include_delta else ncol(fit_rs$ZY)>0L,
    baseline_spec,trajectory_spec,reference_time,time_scale,stage3_chunk_size,zero_spec=zero_spec)
  stage3_elapsed<-proc.time()[['elapsed']]-started
  started<-proc.time()[['elapsed']]
  stacked<-if(inference=='stacked').cimehr_spline_stacked(nuisance_fit,stage3,derivative_step,retain_influence) else NULL
  stacked_elapsed<-proc.time()[['elapsed']]-started
  if(!is.null(stacked)&&!fully_fixed)stacked$scope<-paste0(stacked$scope,'; conditional on visit-time-selected spline knots/boundaries')
  if(inference=='stacked'&&!isTRUE(stacked$regular))warning('Spline covariance is conditional on the selected frailty/variance face.',call.=FALSE)
  if(inference=='conditional')warning('Conditional spline standard errors hold MOM/Laplace Stages 1 and 2 fixed.',call.=FALSE)
  V<-switch(inference,stacked=stacked$vcov,conditional=stage3$vcov_conditional,none=NULL)
  covariance_factor<-switch(inference,stacked=stacked$covariance_factor,
    conditional=stage3$conditional_covariance_factor,none=NULL)
  spec<-list(spline_type=spline_type,baseline=baseline_spec,trajectory=trajectory_spec,
    trajectory_covars=trajectory_covars,boundary_knots=boundary_knots,reference_time=reference_time,time_scale=time_scale,
    fixed_covars=covars_outcome_fixed_XY,fixed_structure=outcome_fixed_structure,
    random_structure=outcome_random_structure,random_intercept=outcome_random_intercept,
    random_covars=.cimehr_random_modifiers(covars_outcome_random_link_ZY,time_col,outcome_random_structure),
    random_time=if(is.list(rs$use_time))rs$use_time[setdiff(names(rs$use_time),'slope')] else rs$use_time,
    training_XY_columns=colnames(XY),training_ZY_columns=colnames(rs$ZY),fully_prespecified=fully_fixed,
    boundary_knots_were_prespecified=boundary_given,baseline_knots_were_prespecified=baseline_given,
    trajectory_knots_were_prespecified=!length(trajectory_covars)||trajectory_given)
  if(!is.null(zero_spec)) {
    spec$zero_compensation <- zero_spec
    spec$zero_observation_covars <- covars_obs_random_link_ZO
  }
  summary<-.cimehr_spline_nuisance_summary(nuisance_fit,stacked)
  result<-list(baseline_spline=stage3$baseline,beta=stage3$beta,trajectory=stage3$trajectory,
    theta=stage3$theta,coefficients=stage3$coef,vcov=V,vcov_conditional=stage3$vcov_conditional,
    covariance_factor=covariance_factor,
    conditional_covariance_factor=stage3$conditional_covariance_factor,
    stacked_sandwich=stacked,spline_spec=spec,
    diagnostics=list(n_subjects=n,n_visits=sum(check$visit),n_outcomes=stage3$n_outcomes,
      stage3_dimension=length(stage3$coef),stage3_design_rank=stage3$design_rank,
      stage3_relative_minimum_eigenvalue=stage3$relative_minimum_eigenvalue,
      stage3_scaled_condition_number=stage3$scaled_condition_number,residual_scale=stage3$residual_scale,
      nuisance_fit_seconds=nuisance_elapsed,nuisance_fit_reused=reused,
      spline_stage3_seconds=stage3_elapsed,spline_stacked_seconds=stacked_elapsed,
      total_seconds=nuisance_elapsed+stage3_elapsed+stacked_elapsed,
      nuisance_converged=nuisance_fit$stage1$converged&&nuisance_fit$pcl$converged,
      stage2_scale_identified=nuisance_fit$pcl$identified,laplace_posterior=TRUE,
      frailty_estimator='method of moments',derivative_step=derivative_step,
      approximation_target='Laplace-defined spline estimating-equation target; finite-sample unbiasedness is not guaranteed'),
    model_info=list(inference=inference,baseline_model='fixed-dimensional regression spline',
      basis_inference=if(fully_fixed)'fixed prespecified knots and boundaries' else 'conditional on visit-time-selected spline knots/boundaries',
      fit_shared_loading=nuisance_fit$include_delta,nuisance_algorithm='CIMEHR MOM/Laplace Stages 1 and 2',
      laplace_posterior=TRUE,frailty_estimator='method of moments',time_precision=time_precision),
    nuisance_summary=summary,nuisance_fit=if(retain_nuisance_fit)nuisance_fit else NULL,
    outcome_functionals=list(omega=setNames(nuisance_fit$functionals$omega,check$ids),
      kappa=setNames(nuisance_fit$functionals$kappa,check$ids)),call=call)
  if(!is.null(stage3$numerical_solver))
    result$diagnostics$stage3_numerical_solver <- stage3$numerical_solver
  if(!is.null(zero_spec)) {
    result$model_info$zero_frailty_compensation <- zero_spec$kind
    result$diagnostics$compensation_identification <- if(identical(zero_spec$kind,'raw_unique'))
      paste0(zero_spec$identification_scope,
        '; combined spline outcome design full rank') else
      'original attainable compensation set represented in identified coordinates; combined spline outcome design full rank'
    result$diagnostics$identification_scope <- result$diagnostics$compensation_identification
    if(!is.null(result$stacked_sandwich))
      result$stacked_sandwich$scope <- paste0(result$stacked_sandwich$scope,
        '; all identifiable nuisance-stage uncertainty and cross-stage covariance retained; numerical identification checks do not establish true-stratum regularity or post-selection coverage')
    if(identical(zero_spec$kind,'quotient')) {
      result$compensation <- result$theta
      result$theta <- setNames(numeric(0),character(0))
      result$outcome_functionals$compensation_multiplier <- result$outcome_functionals$kappa
      result$outcome_functionals$kappa <- NULL
      result$model_info$fit_shared_loading <- FALSE
      obs <- result$nuisance_summary$observation_process
      if(isTRUE(zero_spec$raw_chart)) {
        obs$fitted_total_variance <- setNames(rowSums(
          (zero_spec$original_ZO %*% zero_spec$Omega)*zero_spec$original_ZO),check$ids)
        if(isTRUE(zero_spec$covariance_identified))
          obs$total_covariance <- zero_spec$Omega
        obs$delta_latent_random_link <- obs$se_delta <- NULL
      } else {
        obs$total_variance <- obs$Sigma_q
        obs$se_total_variance <- obs$se_Sigma_q
      }
      obs$Sigma_q <- obs$se_Sigma_q <- NULL
      obs$parameterization <- if(isTRUE(zero_spec$raw_chart) &&
        !isTRUE(zero_spec$covariance_identified))
        'identified fitted total observation variances; the covariance decomposition, delta and conditional Sigma_q are not separately reported' else
        'identified total observation covariance; delta and conditional Sigma_q are not separately reported'
      rename <- function(x) if(isTRUE(zero_spec$raw_chart))x else sub('^sigma2_q[.]','tau2.',x)
      rename_matrix <- function(x) {
        if(is.matrix(x)) {
          if(!is.null(rownames(x)))rownames(x)<-rename(rownames(x))
          if(!is.null(colnames(x)))colnames(x)<-rename(colnames(x))
        }
        x
      }
      obs$vcov_stacked <- rename_matrix(obs$vcov_stacked)
      obs$vcov_obs <- rename_matrix(obs$vcov_obs)
      if(isTRUE(zero_spec$raw_chart)) {
        ai <- grep('^alpha[.]',rownames(obs$vcov_stacked))
        obs$vcov_stacked <- obs$vcov_stacked[ai,ai,drop=FALSE]
        obs$vcov_obs <- obs$vcov_obs[ai,ai,drop=FALSE]
      }
      result$nuisance_summary$observation_process <- obs
      if(!is.null(result$stacked_sandwich)) {
        st <- result$stacked_sandwich
        st$vcov_joint <- rename_matrix(st$vcov_joint)
        st$component_vcov$observation <- rename_matrix(st$component_vcov$observation)
        st$unavailable_wald_parameters <- rename(st$unavailable_wald_parameters)
        if(!is.null(st$influence_joint))st$influence_joint<-rename_matrix(st$influence_joint)
        if(!is.null(st$influence_joint_active_face))
          st$influence_joint_active_face<-rename_matrix(st$influence_joint_active_face)
        st$parameterization <- 'identified total observation variance and compensation coordinates'
        if(isTRUE(zero_spec$raw_chart)) {
          nuisance_raw <- grep('^(delta[.]|sigma2_q[.])',rownames(st$vcov_joint),value=TRUE)
          st$unavailable_wald_parameters <- union(st$unavailable_wald_parameters,nuisance_raw)
          st$vcov_joint[nuisance_raw,] <- NA_real_
          st$vcov_joint[,nuisance_raw] <- NA_real_
          if(!is.null(st$influence_joint))st$influence_joint[,nuisance_raw]<-NA_real_
          ai <- grep('^alpha[.]',rownames(st$component_vcov$observation))
          st$component_vcov$observation <- st$component_vcov$observation[ai,ai,drop=FALSE]
          st$parameterization <- 'identified compensation coordinates; raw Stage-2 chart is internal and its decomposition has no reported Wald inference'
        }
        result$stacked_sandwich <- st
      }
    }
  }
  if(!is.null(zero_spec)) {
    result$diagnostics$zero_frailty_compensation <- zero_spec$compensation_rule
    id_diagnostics <- .cimehr_identification_diagnostics(
      nuisance_fit$XO, outcome_modifiers, nuisance_fit$pcl$alpha,
      if(is.null(stacked)) NULL else stacked$component_vcov$observation,
      nuisance_fit$stage1$mi>0, n, stage3$bread, stage3$index$theta,
      zero_spec, nuisance_fit$pcl$delta,
      gram_scope='recorded-outcome spline design including baseline, fixed effects and trajectory terms',
      compensation_rule=zero_spec$compensation_rule)
    result <- .cimehr_attach_identification_diagnostics(result,id_diagnostics)
  }
  result <- .cimehr_attach_numerical_diagnostics(result,
    .cimehr_numerical_diagnostics(relative_minimum=stage3$relative_minimum_eigenvalue,
      solver=stage3$numerical_solver,spline=TRUE))
  result$prediction_spec <- .trajectory_cimehr_prediction_spec(
    data, check$sf, covars_outcome_fixed_XY, outcome_fixed_structure,
    time_col, .trajectory_cimehr_followup_range(
      all_times, data[[censor_col]][check$sf], check$Ei, boundary_knots),
    additional_covars=trajectory_covars, subject_index=check$subject, checked=static)
  if(!is.null(result$stacked_sandwich$covariance_factor)) {
    attr(result$stacked_sandwich$covariance_factor,'scope')<-result$stacked_sandwich$scope
    if(identical(inference,'stacked'))
      attr(result$covariance_factor,'scope')<-result$stacked_sandwich$scope
  }
  if(retain_influence)result$stage3<-stage3
  class(result)<-c('CIMEHR_laplace_spline','CIMEHR_spline')
  result
}

}, envir = .cimehr_spline)
.cimehr_spline_fit <- function(...) {
    fit <- .cimehr_spline$.cimehr_spline_native(...)
    se <- if (is.null(fit$vcov)) 
        NULL
    else {
        .cimehr_checked_se(fit$vcov)
    }
    inference <- fit$model_info$inference
    ok <- all(is.finite(fit$coefficients)) && isTRUE(fit$diagnostics$stage3_design_rank == fit$diagnostics$stage3_dimension)
    ans <- list(method_key = "cimehr_spline", method = "CIMEHR spline", 
        estimand = "Laplace-defined spline association target at reference time", coefficients = fit$coefficients, 
        se = se, vcov = fit$vcov, covariance_factor = fit$covariance_factor,
        vcov_all = fit$stacked_sandwich$vcov_joint, vcov_type = switch(inference, 
            stacked = paste0("all-stage MOM/Laplace patient stacked spline sandwich; ", fit$stacked_sandwich$scope), conditional = "conditional spline Stage-3 patient sandwich", 
            none = "not requested"), beta = fit$beta, trajectory = fit$trajectory, baseline_spline = fit$baseline_spline, 
        theta = fit$theta, fit = fit, diagnostics = c(list(ok = ok, extension_only = TRUE, nonlinear_baseline = TRUE, 
            group_time_trajectory = length(fit$spline_spec$trajectory_covars) > 0L), fit$diagnostics), 
        assumptions = c("Same MOM visiting frailty, Gaussian/Laplace posterior and pairwise probit nuisance as CIMEHR", 
            "Fixed-dimensional prespecified spline baseline and trajectory contrasts", "Stacked inference is branch-conditional at variance-component boundaries"))
    if(!is.null(fit$compensation)) ans$compensation <- fit$compensation
    ans$diagnostics$converged <- ok && isTRUE(fit$diagnostics$nuisance_converged)
    ans$diagnostics$identified <- ok && isTRUE(fit$diagnostics$stage2_scale_identified)
    ans$diagnostics$full_stage_regular <- isTRUE(fit$stacked_sandwich$regular)
    ans$diagnostics$regular <- inference == "stacked" && isTRUE(fit$stacked_sandwich$regular)
    ans$diagnostics$conditional_inference_valid <- inference == "conditional" || isTRUE(fit$stacked_sandwich$diagnostics$conditional_active_face_inference_available)
    ans$diagnostics$conditional_inference_available <- ans$diagnostics$conditional_inference_valid
    if (isTRUE(ans$diagnostics$identification_diagnostics$zero_frailty))
        ans$diagnostics$conditional_inference_valid <-
            ans$diagnostics$conditional_inference_available &&
            !isTRUE(ans$diagnostics$covariance_failed) &&
            .cimehr_identification_assessment(ans$diagnostics$identification_diagnostics, stage = "outcome")$valid
    ans$diagnostics$frailty_boundary <- isTRUE(fit$nuisance_summary$visiting_process$frailty_boundary)
    ans$diagnostics$frailty_weak_identification <- isTRUE(fit$stacked_sandwich$frailty_weak_identification)
    ans$diagnostics$observation_variance_boundary <- isTRUE(fit$nuisance_summary$observation_process$observation_variance_boundary)
    ans$diagnostics$variance_boundary <- ans$diagnostics$frailty_boundary || ans$diagnostics$observation_variance_boundary
    ans$diagnostics$inference_scope <- if (inference == "stacked") 
        fit$stacked_sandwich$scope
    else if (inference == "conditional") 
        "conditional spline Stage-3 patient sandwich"
    else "not computed"
    ans
}
.cimehr_predict_spline <- function(object, ...) {
    .cimehr_spline$.cimehr_spline_predict(object, ...)
}
.cimehr_spline_adapter <- function(d, id = "id", time = "time", outcome = "Y", obs_indicator = "R", visit_covs = NULL, 
    cimehr_visit_covs = NULL, obs_fixed_covs = NULL, long_covs = NULL, random_covs = NULL, outcome_random_covs = NULL, 
    obs_random_covs = NULL, censor_col = "C", outcome_fixed_structure = NULL, outcome_random_structure = NULL, 
    outcome_random_intercept = TRUE, trajectory_covars = NULL, entry_col = NULL, ...) {
    if (is.null(cimehr_visit_covs)) 
        cimehr_visit_covs <- visit_covs
    if (is.null(obs_fixed_covs)) 
        obs_fixed_covs <- cimehr_visit_covs
    if (is.null(outcome_random_covs)) 
        outcome_random_covs <- random_covs
    if (is.null(obs_random_covs)) 
        obs_random_covs <- random_covs
    .cimehr_spline_fit(data = d, id_col = id, time_col = time, y_col = outcome, r_col = obs_indicator, 
        censor_col = censor_col, covars_visit_XV = cimehr_visit_covs, covars_outcome_fixed_XY = long_covs, 
        covars_outcome_random_link_ZY = outcome_random_covs, covars_obs_fixed_XO = obs_fixed_covs, covars_obs_random_link_ZO = obs_random_covs, 
        outcome_fixed_structure = outcome_fixed_structure, outcome_random_structure = outcome_random_structure, 
        outcome_random_intercept = outcome_random_intercept, trajectory_covars = trajectory_covars,
        entry_col = entry_col, ...)
}

CIMEHR_spline <- function(
    data, id_col = "id", time_col = "time", y_col = "Y", r_col = "R",
    censor_col = "C", covars_visit_XV = NULL, covars_outcome_fixed_XY = NULL,
    outcome_fixed_structure = NULL, covars_outcome_random_link_ZY = NULL,
    outcome_random_structure = NULL, outcome_random_intercept = TRUE,
    covars_obs_fixed_XO = NULL, covars_obs_random_link_ZO = NULL,
    trajectory_covars = NULL, spline_type = c("natural", "bspline"),
    baseline_df = 5L, trajectory_df = 3L, baseline_knots = NULL,
    trajectory_knots = NULL, boundary_knots = NULL, reference_time = NULL,
    spline_degree = 3L, stage3_chunk_size = 100000L,
    inference = c("stacked", "conditional", "none"), nuisance_fit = NULL,
    retain_nuisance_fit = FALSE, retain_influence = FALSE, entry_col = NULL, ...) {
  spline_type <- match.arg(spline_type)
  inference <- match.arg(inference)
  data <- as.data.frame(data)
  .cimehr_check_numeric_columns(data, c(covars_visit_XV, covars_outcome_fixed_XY,
    covars_outcome_random_link_ZY, covars_obs_fixed_XO, covars_obs_random_link_ZO,
    trajectory_covars))
  .validate_data(data, id_col, time_col, y_col, r_col,
    unique(c(covars_visit_XV, covars_outcome_fixed_XY, covars_outcome_random_link_ZY,
      covars_obs_fixed_XO, covars_obs_random_link_ZO, trajectory_covars)), entry_col, censor_col)
  curve_refit_data <- data
  masked <- .mask_entry_visits(data, id_col, time_col, y_col, r_col, entry_col)
  data <- masked$data
  engine <- .cimehr_engine()
  warnings <- character()
  started <- proc.time()[["elapsed"]]
  ans <- withCallingHandlers(engine$.cimehr_spline_fit(
    data = data, id_col = id_col, time_col = time_col, y_col = y_col, r_col = r_col,
    censor_col = censor_col, entry_col = entry_col, covars_visit_XV = covars_visit_XV,
    covars_outcome_fixed_XY = covars_outcome_fixed_XY,
    outcome_fixed_structure = outcome_fixed_structure,
    covars_outcome_random_link_ZY = covars_outcome_random_link_ZY,
    outcome_random_structure = outcome_random_structure,
    outcome_random_intercept = outcome_random_intercept,
    covars_obs_fixed_XO = covars_obs_fixed_XO,
    covars_obs_random_link_ZO = covars_obs_random_link_ZO,
    trajectory_covars = trajectory_covars, spline_type = spline_type,
    baseline_df = baseline_df, trajectory_df = trajectory_df,
    baseline_knots = baseline_knots, trajectory_knots = trajectory_knots,
    boundary_knots = boundary_knots, reference_time = reference_time,
    spline_degree = spline_degree, stage3_chunk_size = stage3_chunk_size,
    inference = inference, nuisance_fit = nuisance_fit,
    retain_nuisance_fit = retain_nuisance_fit, retain_influence = retain_influence, ...),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w)); invokeRestart("muffleWarning")
    })
  elapsed <- proc.time()[["elapsed"]] - started
  ans <- .cimehr_tag_fit(ans,'cimehr_spline')
  ans <- .normalize_fit(ans, 'cimehr_spline', elapsed, warnings)
  ans$diagnostics$n_visits_at_entry_excluded <- masked$n
  ans$call <- match.call()
  ans$parameter_table <- .outcome_table(ans)
  class(ans) <- c("cimehr_spline_fit", "cimehr_fit", "list")
  ans$refit <- .curve_capture_refit(curve_refit_data, list(
    method = "cimehr_spline", id = id_col, time = time_col, outcome = y_col,
    obs_indicator = r_col, censor_col = censor_col, entry_col = entry_col,
    visit_covs = covars_visit_XV, cimehr_visit_covs = covars_visit_XV,
    long_covs = covars_outcome_fixed_XY,
    outcome_random_covs = covars_outcome_random_link_ZY,
    obs_random_covs = if(is.null(covars_obs_random_link_ZO))character() else covars_obs_random_link_ZO,
    obs_fixed_covs = covars_obs_fixed_XO,
    outcome_fixed_structure = outcome_fixed_structure,
    outcome_random_structure = outcome_random_structure,
    outcome_random_intercept = outcome_random_intercept,
    focal = if(length(covars_outcome_fixed_XY))covars_outcome_fixed_XY[1L] else NULL,
    seed = 1L,
    controls = c(list(trajectory_covars = trajectory_covars, spline_type = spline_type,
      baseline_df = baseline_df, trajectory_df = trajectory_df,
      baseline_knots = baseline_knots, trajectory_knots = trajectory_knots,
      boundary_knots = boundary_knots, reference_time = reference_time,
      spline_degree = spline_degree, stage3_chunk_size = stage3_chunk_size,
      inference = inference, nuisance_fit = nuisance_fit,
      retain_nuisance_fit = retain_nuisance_fit, retain_influence = retain_influence),
      list(...))))
  ans$diagnostics$object_bytes <- as.numeric(object.size(ans))
  .cimehr_signal_identification_warning(ans)
  ans
}

predict.cimehr_spline_fit <- function(object, newdata = NULL, times = NULL,
    type = c("population", "observed"), kappa = NULL, se.fit = TRUE,
    level = .95, aggregate = FALSE, weights = NULL, bandwidth = NULL, ...) {
  type <- match.arg(type)
  if (type == "population") {
    if (length(list(...))) stop("Unknown prediction arguments.", call. = FALSE)
    return(population_trajectory(object, newdata, times, bandwidth,
      aggregate, weights, se.fit, level))
  }
  if (!inherits(object, "cimehr_spline_fit") || is.null(object$fit$spline_spec))
    stop("object must be a fitted CIMEHR_spline model.", call. = FALSE)
  if (!is.numeric(level) || length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1)
    stop("level must be one finite number strictly between 0 and 1.", call. = FALSE)
  if (!is.data.frame(newdata)) stop("newdata must be a data.frame.", call. = FALSE)
  spec <- object$fit$spline_spec
  .cimehr_check_numeric_columns(newdata,
    c(spec$fixed_covars, spec$trajectory_covars, spec$random_covars))
  ans <- .cimehr_object_engine(object)$.cimehr_predict_spline(object$fit, newdata = newdata,
    times = times, type = type, kappa = kappa, se.fit = se.fit, level = level, ...)
  scope <- object$diagnostics$inference_scope
  attr(ans, "inference_scope") <- scope
  attr(ans, "regular") <- object$diagnostics$regular
  ans <- .report_curve_inference_metadata(ans, object)
  assessed <- attr(ans, "inference_scope")
  if (!identical(assessed, scope)) {
    attr(ans, "inference_assessment_scope") <- assessed
    attr(ans, "inference_scope") <- scope
  }
  ans
}
