.cimehr_updated <- new.env(parent = .cimehr_common)
evalq({
    .cimehr_shared_engine <- parent.env(environment())
    .cimehr_affine_engine <- environment()
    .cimehr_affine_outcome_design <- function(data, subject_first, subject_index, time, fixed_covs, random_covs, 
        fixed_structure, random_structure, random_intercept) {
        n <- length(subject_first)
        constant <- function(v) {
            if (v %in% c("(Intercept)", "Intercept")) 
                return(rep(1, n))
            if (!v %in% names(data) || !is.numeric(data[[v]]) || any(!is.finite(data[[v]]))) 
                stop("Outcome design modifiers must be finite numeric subject-level columns: ", v, call. = FALSE)
            z <- data[[v]][subject_first]
            if (any(abs(data[[v]] - z[subject_index]) > 1e-10 * pmax(1, abs(data[[v]])))) 
                stop("Outcome design modifier varies within subject: ", v, call. = FALSE)
            z
        }
        clock <- function(z) {
            origin <- if (is.null(z$time_origin)) 
                0
            else z$time_origin
            scale <- if (is.null(z$time_scale)) 
                1
            else z$time_scale
            if (length(origin) != 1L || !is.finite(origin) || length(scale) != 1L || !is.finite(scale) || 
                scale <= 0) 
                stop("Outcome design time origin/scale must be finite with positive scale.", call. = FALSE)
            c(origin = origin, scale = scale)
        }
        p <- length(fixed_covs)
        X0 <- X1 <- matrix(0, n, p, dimnames = list(NULL, fixed_covs))
        if (!is.null(fixed_structure) && !is.list(fixed_structure)) 
            stop("outcome_fixed_structure must be a list.", call. = FALSE)
        fx <- if (is.null(fixed_structure)) 
            list()
        else fixed_structure
        fc <- clock(fx)
        extra <- setdiff(names(fx$columns), fixed_covs)
        if (length(extra)) 
            stop("Declared fixed columns are not fitted: ", paste(extra, collapse = ", "), call. = FALSE)
        for (j in seq_along(fixed_covs)) {
            name <- fixed_covs[j]
            terms <- fx$columns[[name]]
            if (is.null(terms)) 
                terms <- data.frame(modifier = name, power = 0)
            if (!is.data.frame(terms) || !all(c("modifier", "power") %in% names(terms)) || !nrow(terms) || 
                anyNA(terms$modifier) || anyNA(terms$power) || any(!terms$power %in% c(0, 1))) 
                stop("Each fixed column requires modifier/power rows with powers 0 or 1: ", name, call. = FALSE)
            for (k in seq_len(nrow(terms))) {
                value <- constant(as.character(terms$modifier[k]))
                if (terms$power[k] == 0) 
                  X0[, j] <- X0[, j] + value
                else {
                  X0[, j] <- X0[, j] - fc["origin"]/fc["scale"] * value
                  X1[, j] <- X1[, j] + value/fc["scale"]
                }
            }
            valid <- is.finite(time)
            reconstructed <- X0[subject_index[valid], j] + X1[subject_index[valid], j] * time[valid]
            actual <- data[[name]][valid]
            if (any(!is.finite(actual)) || any(abs(actual - reconstructed) > 1e-08 * pmax(1, abs(actual)))) 
                stop("Declared trajectory does not reproduce observed fixed covariate: ", name, call. = FALSE)
        }
        if (!is.logical(random_intercept) || length(random_intercept) != 1L || is.na(random_intercept)) 
            stop("outcome_random_intercept must be TRUE or FALSE.", call. = FALSE)
        rr <- random_structure
        if (is.null(rr)) 
            rr <- list(static_covs = setdiff(random_covs, attr(time, "column")), time_covs = if (attr(time, 
                "column") %in% random_covs) "(Intercept)" else character())
        if (!is.list(rr)) 
            stop("outcome_random_structure must be a list.", call. = FALSE)
        if (!is.null(rr$columns)) {
            columns <- rr$columns
            if (!is.list(columns) || (length(columns) && is.null(names(columns))) || any(!nzchar(names(columns))) || 
                anyDuplicated(names(columns))) 
                stop("Random columns must be a uniquely named list of modifier/power tables.", call. = FALSE)
            if (random_intercept && !any(names(columns) %in% c("Intercept", "(Intercept)"))) 
                columns <- c(list(Intercept = data.frame(modifier = "(Intercept)", power = 0)), columns)
            if (!random_intercept && any(names(columns) %in% c("Intercept", "(Intercept)"))) 
                stop("An intercept column requires outcome_random_intercept=TRUE.", call. = FALSE)
            rc <- clock(rr)
            W0 <- W1 <- matrix(0, n, length(columns), dimnames = list(NULL, names(columns)))
            if (!length(columns)) 
                stop("The outcome random design must contain a coefficient.", call. = FALSE)
            for (j in seq_along(columns)) {
                tt <- columns[[j]]
                name <- names(columns)[j]
                if (!is.data.frame(tt) || !all(c("modifier", "power") %in% names(tt)) || !nrow(tt) || 
                  anyNA(tt$modifier) || anyNA(tt$power) || any(!tt$power %in% c(0, 1))) 
                  stop("Each random column requires modifier/power rows with powers 0 or 1: ", name, 
                    call. = FALSE)
                for (k in seq_len(nrow(tt))) {
                  value <- constant(as.character(tt$modifier[k]))
                  if (tt$power[k] == 0) 
                    W0[, j] <- W0[, j] + value
                  else {
                    W0[, j] <- W0[, j] - rc["origin"]/rc["scale"] * value
                    W1[, j] <- W1[, j] + value/rc["scale"]
                  }
                }
                valid <- is.finite(time)
                reconstructed <- W0[subject_index[valid], j] + W1[subject_index[valid], j] * time[valid]
                if (name %in% c("Intercept", "(Intercept)") && any(abs(reconstructed - 1) > 1e-10)) 
                  stop("The declared random intercept must equal one.", call. = FALSE)
                if (name %in% names(data)) {
                  actual <- data[[name]][valid]
                  if (any(!is.finite(actual)) || any(abs(actual - reconstructed) > 1e-08 * pmax(1, abs(actual)))) 
                    stop("Declared trajectory does not reproduce observed random covariate: ", name, 
                      call. = FALSE)
                }
            }
            return(list(X0 = X0, X1 = X1, W0 = W0, W1 = W1))
        }
        st <- unique(as.character(rr$static_covs))
        tv <- unique(as.character(rr$time_covs))
        st <- setdiff(st, c("(Intercept)", "Intercept"))
        if (random_intercept) 
            st <- c("(Intercept)", st)
        rc <- clock(rr)
        nm <- c(ifelse(st == "(Intercept)", "Intercept", st), ifelse(tv %in% c("(Intercept)", "Intercept"), 
            "time01", paste0(tv, "_time01")))
        if (anyDuplicated(nm)) 
            stop("Outcome random design has duplicated coefficient names.", call. = FALSE)
        W0 <- W1 <- matrix(0, n, length(nm), dimnames = list(NULL, nm))
        for (j in seq_along(st)) W0[, j] <- constant(st[j])
        for (j in seq_along(tv)) {
            value <- constant(tv[j])
            k <- length(st) + j
            W0[, k] <- -rc["origin"]/rc["scale"] * value
            W1[, k] <- value/rc["scale"]
        }
        list(X0 = X0, X1 = X1, W0 = W0, W1 = W1)
    }
    .cimehr_stage3_qr_factor <- function(M, diagnostic_tolerance = 1e-7,
        roundoff_dimension = nrow(M), cancellation_bound = NULL) {
        M <- as.matrix(M)
        p <- ncol(M)
        scale <- sqrt(colSums(M^2))
        machine_tolerance <- max(nrow(M), p, roundoff_dimension) * .Machine$double.eps
        if (!p || nrow(M) < p || any(!is.finite(M)) || any(!is.finite(scale)) || any(scale <= 0) ||
            (!is.null(cancellation_bound) && any(scale <= cancellation_bound)))
            stop("Stage-3 design rank is numerically unresolved at floating-point precision.", call. = FALSE)
        normalized <- sweep(M, 2L, scale, "/")
        singular <- svd(normalized, nu = 0L, nv = 0L)$d
        if (min(singular) <= machine_tolerance * max(singular))
            stop("Stage-3 design rank is numerically unresolved at floating-point precision.", call. = FALSE)
        factor <- qr(normalized, LAPACK = TRUE)
        Q <- qr.Q(factor, complete = FALSE)[, seq_len(p), drop = FALSE]
        R <- qr.R(factor)[seq_len(p), seq_len(p), drop = FALSE]
        map <- matrix(0, p, p)
        map[factor$pivot, ] <- backsolve(R, diag(p))
        map <- sweep(map, 1L, scale, "/")
        list(Q = Q, map = map, R = R, pivot = factor$pivot, scale = scale,
            rank = p, machine_tolerance = machine_tolerance,
            diagnostic_rank = qr(normalized, tol = diagnostic_tolerance, LAPACK = FALSE)$rank,
            relative_minimum_singular_value = min(singular)/max(singular))
    }
    .cimehr_stage3_qr_solve <- function(D, y, H = NULL, case_weight = rep(1, nrow(D)),
        diagnostic_tolerance = 1e-7, roundoff_dimension = nrow(D), center = NULL) {
        weighted_D <- D * sqrt(case_weight)
        weighted_y <- y * sqrt(case_weight)
        dx <- .cimehr_stage3_qr_factor(weighted_D, diagnostic_tolerance, roundoff_dimension)
        original_coefficients <- function(value) {
            result <- numeric(ncol(D))
            result[dx$pivot] <- backsolve(dx$R, value)
            result/dx$scale
        }
        if (is.null(H)) {
            rhs <- matrix(cimehr_stable_matvec_cpp(t(dx$Q), weighted_y), ncol = 1L)
            coefficients <- original_coefficients(rhs)
            inverse <- cimehr_stable_right_product_cpp(dx$map, t(dx$map))
            rank_info <- list(design_rank = dx$rank, diagnostic_design_rank = dx$diagnostic_rank,
                design_relative_minimum_singular_value = dx$relative_minimum_singular_value)
            solved <- backsolve(dx$R, rhs)
            factor_error <- max(abs(dx$R %*% solved - rhs))/
                max(norm(dx$R, "I") * max(abs(solved)) + max(abs(rhs)), .Machine$double.xmin)
            factor_errors <- factor_error
            if (!is.finite(factor_error) || factor_error > 100 * dx$machine_tolerance)
                stop("Stage-3 factored solve has an unresolved numerical residual.", call. = FALSE)
        } else {
            weighted_H <- H * sqrt(case_weight)
            cancellation_bound <- if (is.null(center)) NULL else
                8 * .Machine$double.eps * max(roundoff_dimension, ncol(D)) *
                (sqrt(colSums(weighted_D^2)) + sqrt(colSums((center * sqrt(case_weight))^2)))
            hx <- .cimehr_stage3_qr_factor(weighted_H, diagnostic_tolerance,
                roundoff_dimension, cancellation_bound)
            coupling <- cimehr_stable_right_product_cpp(t(hx$Q), dx$Q)
            coupling_singular <- svd(coupling, nu = 0L, nv = 0L)$d
            numerical_tolerance <- max(dx$machine_tolerance, hx$machine_tolerance)
            if (min(coupling_singular) <= numerical_tolerance * max(coupling_singular))
                stop("Stage-3 moment matrix rank is numerically unresolved at floating-point precision.", call. = FALSE)
            qcoupling <- qr(coupling, LAPACK = TRUE)
            rhs <- matrix(cimehr_stable_matvec_cpp(t(hx$Q), weighted_y), ncol = 1L)
            solved <- qr.coef(qcoupling, cbind(rhs, t(hx$map)))
            coefficients <- original_coefficients(solved[, 1L])
            inverse <- cimehr_stable_right_product_cpp(dx$map, solved[, -1L, drop = FALSE])
            original_rhs <- cbind(rhs, t(hx$map))
            equation_error <- cimehr_stable_right_product_cpp(coupling, solved) - original_rhs
            error_scales <- norm(coupling, "I") * apply(abs(solved), 2L, max) +
                apply(abs(original_rhs), 2L, max)
            factor_errors <- apply(abs(equation_error), 2L, max)/pmax(error_scales, .Machine$double.xmin)
            factor_error <- max(factor_errors)
            if (!is.finite(factor_error) || factor_error > 100 * numerical_tolerance)
                stop("Stage-3 factored solve has an unresolved numerical residual.", call. = FALSE)
            rank_info <- list(design_rank = dx$rank, centered_design_rank = hx$rank,
                diagnostic_design_rank = dx$diagnostic_rank, diagnostic_centered_design_rank = hx$diagnostic_rank,
                design_relative_minimum_singular_value = dx$relative_minimum_singular_value,
                centered_design_relative_minimum_singular_value = hx$relative_minimum_singular_value,
                coupling_relative_minimum_singular_value = min(coupling_singular)/max(coupling_singular))
        }
        if (any(!is.finite(coefficients)) || any(!is.finite(inverse)))
            stop("Stage-3 factored solve did not produce finite coefficients and inverse.", call. = FALSE)
        dimnames(inverse) <- list(colnames(D), if (is.null(H)) colnames(D) else colnames(H))
        state <- list(design_map = dx$map, center_map = if (is.null(H)) NULL else hx$map,
            coupling = if (is.null(H)) NULL else coupling)
        list(coefficients = coefficients, inverse = inverse, solve_state = state, diagnostics = c(list(
            method = if (is.null(H)) "original-design QR" else "two-design QR for original H' W D equation",
            numerical_rescue = TRUE, machine_tolerance = dx$machine_tolerance,
            diagnostic_tolerance = diagnostic_tolerance, factored_backward_error = factor_error,
            factored_backward_error_by_rhs = factor_errors,
            scope = "numerical computability of the original estimating equation; not a proof of population identification or interval coverage"), rank_info))
    }
    .cimehr_stage3_apply_inverse <- function(score, inverse, state = NULL) {
        if (is.null(state)) return(score %*% t(inverse))
        if (is.null(state$coupling)) {
            transformed <- cimehr_stable_right_product_cpp(score, state$design_map)
        } else {
            transformed <- cimehr_stable_right_product_cpp(score, state$center_map)
            transformed <- t(qr.coef(qr(state$coupling, LAPACK = TRUE), t(transformed)))
        }
        cimehr_stable_right_product_cpp(transformed, t(state$design_map))
    }
    .cimehr_affine_stage3 <- function(D0, D1, y, subject, event_grid, event_time, risk_weight, last_risk, 
        case_weight = rep(1, length(risk_weight)), first_risk, shared = FALSE,
        zero_rank_tolerance = NULL) {
        D0 <- as.matrix(D0)
        D1 <- as.matrix(D1)
        n <- nrow(D0)
        p <- ncol(D0)
        K <- max(c(last_risk, event_grid, 0L))
        if (!identical(dim(D0), dim(D1)) || length(case_weight) != n || any(!is.finite(case_weight)) || 
            any(case_weight <= 0))
            stop("Invalid affine outcome design/case weights.", call. = FALSE)
        w <- risk_weight * case_weight
        stable_center <- cimehr_stage3_center_cpp(D0, D1, risk_weight, case_weight,
            as.integer(last_risk), as.integer(first_risk), as.integer(subject),
            as.integer(event_grid), event_time, K)
        den <- stable_center$den
        if (any(!is.finite(den)) || any(den[event_grid] <= 0))
            stop("Affine outcome centering requires positive risk mass.", call. = FALSE)
        inv <- ifelse(den > 0, 1/den, 0)
        D <- stable_center$D; H <- stable_center$H; center <- stable_center$center
        center0 <- stable_center$center0; center1 <- stable_center$center1
        colnames(D) <- colnames(H) <- colnames(center) <- colnames(D0)
        colnames(center0) <- colnames(center1) <- colnames(D0)
        cw <- case_weight[subject]
        bread <- crossprod(H, cw * D)
        numerical_solver <- NULL
        inverse <- NULL
        solve_state <- NULL
        diagnostic_tolerance <- if (is.null(zero_rank_tolerance)) 1e-7 else zero_rank_tolerance
        ordinary <- tryCatch({
            if (.lu_zero_rank(bread, diagnostic_tolerance) < p)
                stop("The normal-equation conditioning screen requests an original-design solve.")
            list(coefficients = drop(solve(bread, crossprod(H, cw * y))), inverse = solve(bread))
        }, error = function(e) e)
        if (inherits(ordinary, "error")) {
            ii <- which(last_risk > 0L & last_risk >= first_risk)
            ordinary_risk <- cimehr_window_risk_sum(cbind(w[ii], w[ii] * D0[ii, , drop = FALSE],
                w[ii] * D1[ii, , drop = FALSE]), last_risk[ii], first_risk[ii], K)
            ordinary_inverse <- ifelse(ordinary_risk[, 1L] > 0, 1/ordinary_risk[, 1L], 0)
            ordinary_center <- (ordinary_risk[event_grid, 1L + seq_len(p), drop = FALSE] +
                ordinary_risk[event_grid, 1L + p + seq_len(p), drop = FALSE] * event_time) *
                ordinary_inverse[event_grid]
            ordinary_H <- D0[subject, , drop = FALSE] + D1[subject, , drop = FALSE] * event_time - ordinary_center
            centering_change <- max(abs(H - ordinary_H))
            bread <- cimehr_stable_right_product_cpp(t(H), cw * D)
            dimnames(bread) <- list(colnames(D0), colnames(D0))
            ordinary <- .cimehr_stage3_qr_solve(D, y, H, cw, diagnostic_tolerance,
                roundoff_dimension = max(n, K, p), center = center)
            numerical_solver <- ordinary$diagnostics
            numerical_solver$centering_method <- "shifted risk-set sums with compensated products and accumulation"
            numerical_solver$centering_max_abs_change <- centering_change
            numerical_solver$centering_scope <- "difference from the ordinary arithmetic, not an independent high-precision accuracy guarantee"
            solve_state <- ordinary$solve_state
        }
        coefficients <- ordinary$coefficients
        inverse <- ordinary$inverse
        original_check <- cimehr_stage3_residual_cpp(D, H, y, coefficients, cw)
        residual <- original_check$residual
        original_relative <- original_check$relative
        if (!is.null(numerical_solver)) {
            numerical_solver$original_equation_relative_residual <- original_relative
            numerical_solver$original_equation_scope <- "compensated row-wise H' W (y - D beta), scaled separately for each estimating equation"
        }
        stable_score <- cimehr_stage3_accumulate_cpp(D0, D1, H, residual, cw, w,
            as.integer(subject), as.integer(event_grid), event_time, as.integer(last_risk),
            as.integer(first_risk), den, stable_center$shift0, stable_center$shift1,
            stable_center$anchor0, stable_center$anchor1, stable_center$den_low,
            stable_center$shift0_low, stable_center$shift1_low)
        score <- stable_score$score; event_score <- stable_score$event_score
        risk_integral <- stable_score$risk_integral
        dA0 <- stable_score$dA0; dA1 <- stable_score$dA1
        dimnames(score) <- dimnames(event_score) <- dimnames(risk_integral) <- list(NULL, colnames(D0))
        if (is.null(inverse)) inverse <- solve(bread)
        conditional_influence <- .cimehr_stage3_apply_inverse(score, inverse, solve_state)
        colnames(conditional_influence) <- colnames(D0)
        covariance <- crossprod(conditional_influence)
        dimnames(covariance) <- list(colnames(D0), colnames(D0))
        ans <- list(coefficients = coefficients, covariance = covariance, subject_score = score, bread = bread, 
            risk_integral = risk_integral, baseline_increments = data.frame(event_grid = seq_len(K), 
                dA0 = dA0, dA1 = dA1), scope = "variance (plug-in): empirical outcome-center influence; earlier visiting/recording nuisance held fixed", 
            cache = list(D0 = D0, D1 = D1, y = y, subject = subject, event_grid = event_grid, event_time = event_time, 
                risk_weight = risk_weight, last_risk = last_risk, first_risk = first_risk),
            covariance_factor = .cimehr_covariance_factor(conditional_influence),
            original_equation_relative_residual = original_relative,
            centering_method = "shifted risk-set sums with compensated products and accumulation")
        if (!is.null(solve_state)) ans$solve_state <- solve_state
        if (!is.null(numerical_solver)) ans$numerical_solver <- numerical_solver
        if (shared) 
            ans$shared <- list(S0 = den, center0 = center0, center1 = center1, D = D, center = center, 
                H = H, residual = residual, event_score = event_score, inverse = inverse)
        ans
    }
    .cimehr_updated_native <- function(data, id_col = "id", time_col = "time", y_col = "Y", r_col = "R", 
        censor_col = "C", covars_visit_XV = c("Z", "X"), covars_outcome_fixed_XY = c("Z", "X"), covars_outcome_random_link_ZY = "Z", 
        covars_obs_fixed_XO = c("Z", "X"), covars_obs_random_link_ZO = "Z", time_precision = 0, outcome_fixed_structure = NULL, 
        outcome_random_structure = NULL, outcome_random_intercept = TRUE, pairwise_control = list(), 
        inference = c("stacked", "none"), derivative_step = 2e-05, retain_details = FALSE, entry_col = NULL) {
        call <- match.call()
        inference <- match.arg(inference)
        data <- as.data.frame(data)
        if (length(time_precision) != 1L || !is.finite(time_precision) || time_precision < 0) 
            stop("time_precision must be a finite nonnegative scalar.", call. = FALSE)
        if (length(derivative_step) != 1L || !is.finite(derivative_step) || derivative_step <= 0) 
            stop("derivative_step must be finite and positive.", call. = FALSE)
        if (!length(covars_outcome_fixed_XY)) 
            stop("Specify at least one outcome fixed covariate.", call. = FALSE)
        check <- .lu_validate_data(data, id_col, time_col, y_col, r_col, censor_col, unique(c(covars_visit_XV, 
            covars_obs_fixed_XO, covars_obs_random_link_ZO)), entry_col = entry_col)
        sf <- check$sf
        subject <- check$subject
        visit <- check$visit
        XV <- .lu_design(data, sf, covars_visit_XV)
        XO <- .lu_design(data, sf, covars_obs_fixed_XO, TRUE)
        ZO <- .lu_design(data, sf, covars_obs_random_link_ZO, TRUE)
        if (ncol(XV) && qr(cbind(1, XV))$rank != ncol(XV) + 1L) 
            stop("Visiting design is rank deficient or contains a baseline-confounded intercept.", call. = FALSE)
        stage1 <- .cimehr_shared_engine$.cimehr_fit_visiting(data[[time_col]], visit, subject, data[[censor_col]][sf], 
            XV, time_precision = time_precision, score_tol = 1e-10, Ei_subj = check$Ei)
        if (!isTRUE(stage1$converged)) 
            stop("Visiting regression did not converge.", call. = FALSE)
        stage1$frailty_method <- "factorial-moment estimator"
        posterior <- .lu_laplace(stage1$mi, stage1$nu, stage1$sigma2_zeta)
        include_delta <- TRUE
        s <- tabulate(subject[visit & data[[r_col]] == 1], length(sf))
        zero_spec <- NULL
        if (isTRUE(stage1$sigma2_zeta == 0)) {
            zfit <- .lu_zero_observation(XO, ZO, stage1$mi, s, posterior, pairwise_control)
            pcl <- zfit$pcl; ZO <- zfit$ZO; include_delta <- zfit$include_delta
            zero_spec <- zfit$zero_spec
        } else pcl <- .lu_fit_pairwise(XO, ZO, stage1$mi, s, posterior, include_delta = include_delta, control = pairwise_control)
        if (!isTRUE(pcl$converged)) 
            stop("Pairwise observation fit did not converge; inspect design/separation or increase pairwise_control limits.", 
                call. = FALSE)
        if (!isTRUE(pcl$identified)) 
            stop("Pairwise observation parameters fail local identification/information checks.", call. = FALSE)
        functional <- .lu_functionals(pcl$par, XO, ZO, posterior, include_delta)
        if (!is.null(zero_spec)) {
            design_time <- data[[time_col]]
            attr(design_time, "column") <- time_col
            outcome_design <- .cimehr_affine_outcome_design(data, sf, subject, design_time,
                covars_outcome_fixed_XY, covars_outcome_random_link_ZY, outcome_fixed_structure,
                outcome_random_structure, outcome_random_intercept)
            zero_spec <- .lu_zero_compensation_rule(zero_spec, XO,
                cbind(Intercept = 1, outcome_design$X0, outcome_design$X1), pcl, posterior,
                stage1$mi > 0, length(sf))
        }
        stage3 <- .lu_stage3(data, sf, subject, data[[time_col]], data[[y_col]], data[[r_col]], visit, 
            stage1, covars_outcome_fixed_XY, covars_outcome_random_link_ZY, time_col, outcome_fixed_structure, 
            outcome_random_structure, outcome_random_intercept, functional$omega, functional$kappa, include_theta = include_delta || !is.null(zero_spec),
            zero_spec = zero_spec)
        if (!is.null(zero_spec)) zero_spec <- stage3$zero_spec
        stacked <- if (inference == "stacked") 
            .lu_stacked(stage1, pcl, stage3, XV = XV, XY = matrix(0, length(sf), length(stage3$beta)), 
                ZO = ZO, XO = XO, posterior = posterior, include_delta = include_delta, derivative_step = derivative_step, 
                retain_details = retain_details)
        else NULL
        alpha_names <- paste0("alpha.", colnames(XO))
        delta_names <- if (include_delta) 
            paste0("delta.", colnames(ZO))
        else character()
        variance_names <- paste0("sigma2_q.", colnames(ZO))
        alpha <- setNames(unname(pcl$par[alpha_names]), colnames(XO))
        delta <- setNames(unname(pcl$par[delta_names]), if (include_delta) 
            colnames(ZO)
        else character())
        variances <- setNames(unname(pcl$par[variance_names]), colnames(ZO))
        out_names <- c(paste0("beta.", names(stage3$beta)), if (length(stage3$theta))
            if (isTRUE(pcl$zero_quotient)) names(stage3$theta) else paste0("theta.",
              sub("^theta\\.", "", names(stage3$theta))) else character())
        gamma_names <- if (length(stage1$gamma)) 
            paste0("gamma.", names(stage1$gamma))
        else character()
        Vout <- if (is.null(stacked)) 
            matrix(NA_real_, length(out_names), length(out_names))
        else stacked$vcov_outcome
        dimnames(Vout) <- list(out_names, out_names)
        Vobs <- if (is.null(stacked)) 
            matrix(NA_real_, length(pcl$par), length(pcl$par), dimnames = list(names(pcl$par), names(pcl$par)))
        else stacked$vcov_observation
        seobs <- .cimehr_checked_se(Vobs)
        vcg <- if (is.null(stacked)) 
            matrix(NA_real_, ncol(XV), ncol(XV))
        else stacked$vcov_gamma
        dimnames(vcg) <- list(colnames(XV), colnames(XV))
        coefficients <- c(setNames(unname(stage3$coef), out_names), setNames(unname(stage1$gamma), gamma_names), 
            pcl$par, sigma2_zeta = unname(stage1$sigma2_zeta))
        sigma_v <- if (is.null(stacked)) 
            NA_real_
        else stacked$vcov_sigma2_zeta
        se <- c(setNames(.cimehr_checked_se(Vout), out_names), setNames(.cimehr_checked_se(vcg), gamma_names),
            seobs, sigma2_zeta = .cimehr_checked_se(as.numeric(sigma_v)[1L]))
        se <- se[names(coefficients)]
        diagnostics <- list(ok = all(is.finite(coefficients)), converged = TRUE, stage2_scale_identified = isTRUE(pcl$identified), 
            identification_scope = "design rank and local active-face information; not a proof of global identification", 
            laplace_posterior = TRUE, frailty_estimator = "method of moments", observation_covariance = "diagonal conditional covariance of q given U", 
            full_stage_regular = !is.null(stacked) && isTRUE(stacked$regular), inference_scope = if (is.null(stacked)) "not computed" else stacked$scope, 
            approximation_target = "Laplace-defined estimating-equation target; finite-sample unbiasedness is not guaranteed", 
            n_subjects = length(sf), n_visits = sum(stage1$mi), n_outcomes = sum(s), visiting = list(converged = stage1$converged, 
                max_abs_score = stage1$max_abs_score), pairwise = pcl$diagnostics, stacked = if (is.null(stacked)) NULL else stacked$diagnostics)
        if (!is.null(stage3$conditional_profile_inference$numerical_solver))
            diagnostics$stage3_numerical_solver <- stage3$conditional_profile_inference$numerical_solver
        covariance_factor <- if (is.null(stacked)) NULL else stacked$covariance_factor
        if (!is.null(covariance_factor)) colnames(covariance_factor) <- out_names
        fit <- list(call = call, coefficients = coefficients, se = se, vcov = Vout,
            covariance_factor = covariance_factor, vcov_all = if (is.null(stacked)) NULL else stacked$vcov_joint[names(coefficients),
            names(coefficients), drop = FALSE], visiting_process = list(gamma_V = stage1$gamma, se_gamma = .cimehr_checked_se(vcg),
            vcov_gamma = vcg, Lambda0_hat = setNames(stage1$dLambda, stage1$unique_times), sigma2_zeta = stage1$sigma2_zeta,
            se_sigma2_zeta = se["sigma2_zeta"], mu_Ui = setNames(posterior$mu, check$ids), sigma2_Ui = setNames(posterior$s2, 
                check$ids), Ci = setNames(stage1$Ci, check$ids), mi = setNames(stage1$mi, check$ids)), 
            observation_process = list(alpha_fixed = alpha, delta_latent_random_link = delta, Sigma_q = variances, 
                Sigma_q_matrix = diag(variances, nrow = length(variances), ncol = length(variances)), 
                vcov_obs = Vobs, se_alpha = setNames(seobs[alpha_names], names(alpha)), se_delta = setNames(seobs[delta_names], 
                  names(delta)), se_Sigma_q = setNames(seobs[variance_names], names(variances)), pairwise_diagnostics = pcl$diagnostics), 
            longitudinal_outcome = list(beta_Y = stage3$beta, theta_Y = stage3$theta, se_beta = setNames(.cimehr_checked_se(diag(Vout)[seq_along(stage3$beta)]),
                names(stage3$beta)), vcov_joint = Vout, vcov_conditional = stage3$vcov_conditional,
                omega = setNames(functional$omega, check$ids), kappa = setNames(functional$kappa, check$ids), 
                baseline = data.frame(time = stage1$unique_times, dA = unname(stage3$dA), A = unname(stage3$A), 
                  mean = unname(stage3$baseline_mean))), inference = stacked, diagnostics = diagnostics)
        fit$prediction_spec <- .trajectory_cimehr_prediction_spec(
            data, sf, covars_outcome_fixed_XY, outcome_fixed_structure,
            time_col, .trajectory_cimehr_followup_range(
                stage1$unique_times, stage1$Ci, stage1$Ei), subject_index=subject)
        dimnames(fit$observation_process$Sigma_q_matrix) <- list(names(variances), names(variances))
        if (!is.null(entry_col))
            fit$visiting_process$Ei <- setNames(stage1$Ei, check$ids)
        if (retain_details) 
            fit$details <- list(stage1 = stage1, posterior = posterior, pairwise = pcl, stage3 = stage3, 
                XO = XO, ZO = ZO, XV = XV, subject = subject)
        if (!is.null(zero_spec)) {
            fit <- .lu_zero_relabel_fit(fit, zero_spec)
            fit$diagnostics$zero_frailty_compensation <- zero_spec$compensation_rule
            id_diagnostics <- .cimehr_identification_diagnostics(
                XO, cbind(Intercept = 1, stage3$design$X0, stage3$design$X1),
                alpha, Vobs, stage1$mi > 0, length(sf), crossprod(stage3$H),
                if (length(stage3$theta)) length(stage3$beta) + seq_along(stage3$theta) else integer(),
                zero_spec, delta,
                gram_scope = 'recorded-outcome design after the existing risk-set baseline centering',
                compensation_rule = zero_spec$compensation_rule)
            fit <- .cimehr_attach_identification_diagnostics(fit, id_diagnostics)
        }
        fit <- .cimehr_attach_numerical_diagnostics(fit,
            .cimehr_numerical_diagnostics(bread = stage3$bread,
                solver = stage3$conditional_profile_inference$numerical_solver))
        class(fit) <- "cimehr_core_fit"
        fit
    }
    .lu_validate_data <- function(data, id_col, time_col, y_col, r_col, censor_col, static_columns, entry_col = NULL) {
        needed <- unique(c(id_col, time_col, y_col, r_col, censor_col, static_columns))
        missing <- setdiff(needed, names(data))
        if (length(missing)) 
            stop("Missing columns: ", paste(missing, collapse = ", "), call. = FALSE)
        if (!nrow(data) || anyNA(data[[id_col]])) 
            stop("Nonempty data and nonmissing subject IDs are required.", call. = FALSE)
        sf <- which(!duplicated(data[[id_col]]))
        subject <- match(data[[id_col]], data[[id_col]][sf])
        if (length(sf) < 3L) 
            stop("At least three independent subjects are required.", call. = FALSE)
        time <- data[[time_col]]
        C <- data[[censor_col]]
        if (!is.numeric(time) || !is.numeric(C) || any(!is.finite(C))) 
            stop("Time and censoring must be numeric; censoring must be finite.", call. = FALSE)
        E <- if (is.null(entry_col)) 
            rep(-Inf, nrow(data))
        else data[[entry_col]]
        timed <- !is.na(time)
        if (any(!is.finite(time[timed])) || any(time[timed] > C[timed]))
            stop("Every visit must have finite time no later than C.", call. = FALSE)
        visit <- timed & time > E
        R <- data[[r_col]]
        Y <- data[[y_col]]
        if (!is.numeric(R) && !is.logical(R)) 
            stop("Recording indicator must be numeric/logical 0 or 1.", call. = FALSE)
        if (anyNA(R[timed]) || any(!R[timed] %in% c(0, 1))) 
            stop("Every visit needs a known binary recording indicator; R=NA is not silently omitted.", 
                call. = FALSE)
        if (!is.numeric(Y) || any(!is.finite(Y[timed & !is.na(R) & R == 1]))) 
            stop("Every recorded outcome must be finite; unrecorded outcomes may be NA.", call. = FALSE)
        if (any(!timed & !is.na(R) & R == 1)) 
            stop("A no-visit placeholder cannot have a recorded outcome.", call. = FALSE)
        for (v in unique(c(censor_col, static_columns))) {
            x <- data[[v]]
            if (!is.numeric(x) || any(!is.finite(x))) 
                stop("Subject covariate must be finite numeric: ", v, call. = FALSE)
            if (any(abs(x - x[sf][subject]) > 1e-10 * pmax(1, abs(x)))) 
                stop("This version requires subject-constant visiting/observation covariates: ", v, call. = FALSE)
        }
        if (any(tabulate(subject[!timed], length(sf)) > 1L) || any(tabulate(subject[!timed], length(sf)) > 
            0L & tabulate(subject[timed], length(sf)) > 0L)) 
            stop("Use exactly one placeholder row for each zero-visit subject, and none for visited subjects.", 
                call. = FALSE)
        list(sf = sf, subject = subject, visit = visit, Ei = E[sf], ids = data[[id_col]][sf])
    }
    .lu_design <- function(data, rows, vars, intercept = FALSE) {
        if (!is.character(vars) && !is.null(vars)) 
            stop("Covariate names must be character vectors.", call. = FALSE)
        if (anyDuplicated(vars)) 
            stop("Covariate names must not be duplicated.", call. = FALSE)
        out <- if (length(vars)) 
            as.matrix(data[rows, vars, drop = FALSE])
        else matrix(0, length(rows), 0L)
        if (intercept) 
            out <- cbind(`(Intercept)` = 1, out)
        storage.mode(out) <- "double"
        out
    }
    .lu_laplace <- function(m, nu, sigma2_zeta) {
        a <- as.numeric(sigma2_zeta)
        if (length(a) != 1L || !is.finite(a) || a < 0) {
            stop("sigma2_zeta must be one finite nonnegative number.", call. = FALSE)
        }
        checked <- .cimehr_shared_engine$.cimehr_frailty_validate(m, nu)
        n <- length(checked$m)
        if (a == 0) {
            return(list(mu = numeric(n), s2 = rep(1, n), sigma2_zeta = 0, mode = numeric(n), curvature = rep(1, 
                n), approximation = "Gaussian/Laplace; zero shared-frailty branch"))
        }
        mode <- .cimehr_shared_engine$.cimehr_frailty_mode_curvature(checked$m, checked$nu, a)
        list(mu = mode$mode_u, s2 = mode$scale2, sigma2_zeta = a, mode = mode$mode_u, curvature = mode$curvature, 
            mode_log_frailty = mode$mode_x, maximum_mode_residual = max(mode$relative_score_residual), 
            approximation = "Gaussian/Laplace mode and inverse curvature")
    }
    .lu_fit_pairwise <- function(XO, ZO, m, s, posterior, include_delta = TRUE, control = list(), case_weights = rep(1, 
        length(m)), start = NULL) {
        XO <- as.matrix(XO)
        ZO <- as.matrix(ZO)
        storage.mode(XO) <- storage.mode(ZO) <- "double"
        rownames(XO) <- rownames(ZO) <- NULL
        n <- nrow(XO)
        pa <- ncol(XO)
        q <- ncol(ZO)
        m <- as.numeric(m)
        s <- as.numeric(s)
        case_weights <- as.numeric(case_weights)
        mu0 <- as.numeric(posterior$mu)
        s20 <- as.numeric(posterior$s2)
        if (!is.list(control) || length(include_delta) != 1L || is.na(include_delta)) 
            stop("Invalid control or include_delta.", call. = FALSE)
        include_delta <- isTRUE(include_delta)
        if (!n || !pa || !q || nrow(ZO) != n || any(vapply(list(m, s, case_weights, mu0, s20), length, 
            integer(1)) != n) || any(!is.finite(c(XO, ZO, m, s, case_weights, mu0, s20))) || any(m < 
            0 | s < 0 | s > m | s20 < 0 | case_weights < 0) || any(abs(c(m, s) - round(c(m, s))) > 1e-08)) 
            stop("Invalid subject designs, counts, posterior moments, or case weights.", call. = FALSE)
        observed <- m > 0 & case_weights > 0
        paired <- m >= 2 & case_weights > 0
        if (!any(observed) || qr(XO[observed, , drop = FALSE])$rank < pa) 
            stop("Observation fixed-effect design has deficient rank.", call. = FALSE)
        if (!any(paired) || (!isTRUE(posterior$sigma2_zeta == 0) && qr(ZO[paired, , drop = FALSE]^2)$rank < q)) 
            stop("Squared observation random-effect design among repeated visits has deficient rank.", 
                call. = FALSE)
        if (is.null(colnames(XO))) 
            colnames(XO) <- paste0("x", seq_len(pa))
        if (is.null(colnames(ZO))) 
            colnames(ZO) <- paste0("z", seq_len(q))
        ia <- seq_len(pa)
        id <- if (include_delta) 
            pa + seq_len(q)
        else integer()
        iv <- pa + length(id) + seq_len(q)
        np <- pa + length(id) + q
        nm <- c(paste0("alpha.", colnames(XO)), if (include_delta) paste0("delta.", colnames(ZO)), paste0("sigma2_q.", 
            colnames(ZO)))
        if (anyDuplicated(nm)) 
            stop("Design column names must be unique.", call. = FALSE)
        z2 <- ZO^2
        pair_weight <- ifelse(m >= 2, case_weights/pmax(m - 1, 1), 0)
        counts <- cbind(s * pmax(s - 1, 0)/2, (m - s) * pmax(m - s - 1, 0)/2, s * (m - s))
        counts <- counts * pair_weight
        colnames(counts) <- c("11", "00", "10")
        singleton <- m == 1 & case_weights > 0
        total_weight <- sum(counts) + sum(case_weights[singleton])
        score_tol <- if (is.null(control$score_tol)) 
            1e-10
        else control$score_tol
        boundary_tol <- if (is.null(control$boundary_tol)) 
            1e-07
        else control$boundary_tol
        rank_tol <- if (is.null(control$identification_tol)) 
            1e-07
        else control$identification_tol
        fd_step <- if (is.null(control$hessian_step)) 
            2e-05
        else control$hessian_step
        if (any(!is.finite(c(score_tol, boundary_tol, rank_tol, fd_step))) || score_tol <= 0 || boundary_tol < 
            0 || rank_tol <= 0 || fd_step <= 0) 
            stop("Invalid numerical tolerances.", call. = FALSE)
        scalar_bvn <- function(a, b, rho) {
            if (rho == 0) 
                return(stats::pnorm(a) * stats::pnorm(b))
            if (a > b) {
                swap <- a
                a <- b
                b <- swap
            }
            log_mass <- stats::pnorm(a, log.p = TRUE)
            mass <- exp(log_mass)
            if (mass == 0) 
                return(0)
            root <- sqrt((1 - rho) * (1 + rho))
            f <- function(u) {
                x <- stats::qnorm(log_mass + log(u), log.p = TRUE)
                stats::pnorm((b - rho * x)/root)
            }
            ans <- tryCatch(stats::integrate(f, 0, 1, subdivisions = 300L, rel.tol = 1e-09, abs.tol = 0, 
                stop.on.error = FALSE), error = function(e) NULL)
            if (is.null(ans) || !identical(ans$message, "OK") || !is.finite(ans$value) || !is.finite(ans$abs.error) || 
                ans$value < 0 || ans$abs.error > 1e-07 * abs(ans$value)) 
                return(NA_real_)
            mass * ans$value
        }
        evaluate <- function(par, mu = mu0, s2 = s20, scores = TRUE) {
            par <- as.numeric(par)
            mu <- as.numeric(mu)
            s2 <- as.numeric(s2)
            invalid <- function() list(value = Inf, gradient = setNames(rep(NA_real_, np), nm), subject_score = if (scores) matrix(NA_real_, 
                n, np, dimnames = list(NULL, nm)) else NULL)
            if (length(par) != np || any(!is.finite(par)) || any(par[iv] < 0) || length(mu) != n || length(s2) != 
                n || any(!is.finite(c(mu, s2))) || any(s2 < 0)) 
                return(invalid())
            ans <- cimehr_pairwise_evaluate(par, mu, s2, XO, ZO, counts, singleton, case_weights, s, include_delta, 
                scores, scalar_bvn, nm)
            if (is.null(ans)) 
                return(invalid())
            ans
        }
        invalid_fit <- list(value = Inf, gradient = setNames(rep(NA_real_, np), nm), subject_score = NULL)
        evaluate_fit <- function(par) {
            par <- as.numeric(par)
            if (length(par) != np || any(!is.finite(par)) || any(par[iv] < 0)) 
                return(invalid_fit)
            ans <- cimehr_pairwise_evaluate(par, mu0, s20, XO, ZO, counts, singleton, case_weights, s, include_delta, 
                FALSE, scalar_bvn, nm)
            if (is.null(ans)) 
                return(invalid_fit)
            ans
        }
        lower <- rep(-Inf, np)
        lower[iv] <- 0
        opt_control <- list(iter.max = 250L, eval.max = 600L, rel.tol = 1e-10, x.tol = 1e-09)
        supplied <- intersect(names(control), names(opt_control))
        opt_control[supplied] <- control[supplied]
        if (!is.null(control$optim_control)) 
            opt_control <- utils::modifyList(opt_control, control$optim_control)
        run_fit <- function(initial, free = seq_len(np), known = NULL) {
            base <- initial
            cache_par <- NULL
            cache_value <- NULL
            previous_par <- NULL
            previous_value <- NULL
            best_par <- if (!is.null(known)) 
                initial[free]
            best_value <- known
            at <- function(x) {
                if (!identical(x, cache_par)) {
                  value <- if (identical(x, previous_par, num.eq = FALSE)) 
                    previous_value
                  else if (identical(x, best_par, num.eq = FALSE)) 
                    best_value
                  else {
                    full <- base
                    full[free] <- x
                    evaluate_fit(full)
                  }
                  previous_par <<- cache_par
                  previous_value <<- cache_value
                  cache_par <<- x
                  cache_value <<- value
                  if (is.null(best_value) || isTRUE(value$value < best_value$value)) {
                    best_par <<- x
                    best_value <<- value
                  }
                }
                cache_value
            }
            fit <- tryCatch(stats::nlminb(initial[free], function(x) at(x)$value, function(x) at(x)$gradient[free], 
                lower = lower[free], control = opt_control), error = function(e) list(par = initial[free], 
                objective = Inf, convergence = 99L, message = conditionMessage(e)))
            evaluation <- if (identical(fit$par, cache_par, num.eq = FALSE)) 
                cache_value
            else if (identical(fit$par, best_par, num.eq = FALSE)) 
                best_value
            else if (identical(fit$par, previous_par, num.eq = FALSE)) 
                previous_value
            full <- base
            full[free] <- fit$par
            fit$par <- setNames(full, nm)
            fit$evaluation <- evaluation
            fit
        }
        fit_evaluation <- function(fit) if (is.null(fit$evaluation)) 
            evaluate_fit(fit$par)
        else fit$evaluation
        if (!is.null(start)) {
            initial <- if (is.list(start)) 
                start$par
            else start
            if (!is.null(names(initial)) && all(nm %in% names(initial))) 
                initial <- initial[nm]
            initial <- as.numeric(initial)
            if (length(initial) != np || any(!is.finite(initial)) || any(initial[iv] < 0)) 
                stop("Invalid pairwise start values.", call. = FALSE)
            starts <- list(initial)
        }
        else {
            fit0 <- suppressWarnings(tryCatch(stats::glm.fit(x = XO[observed, , drop = FALSE], y = s[observed]/m[observed], 
                weights = m[observed] * case_weights[observed], family = stats::binomial("probit")), 
                error = function(e) NULL))
            a0 <- if (is.null(fit0)) 
                numeric(pa)
            else fit0$coefficients
            a0[!is.finite(a0)] <- 0
            a0 <- pmax(pmin(a0, 5), -5)
            variance_starts <- c(0, 0.15, 0.7, 0.2, 0.2)
            delta_starts <- c(0, 0, 0, 0.2, -0.2)
            n_starts <- if (is.null(control$n_starts)) 
                5L
            else as.integer(control$n_starts)
            if (length(n_starts) != 1L || is.na(n_starts) || n_starts < 1L) 
                stop("n_starts must be positive.", call. = FALSE)
            n_starts <- min(n_starts, length(variance_starts))
            starts <- lapply(seq_len(n_starts), function(k) {
                par <- numeric(np)
                par[iv] <- variance_starts[k]
                par[ia] <- a0 * sqrt(1 + mean(drop(z2[observed, , drop = FALSE] %*% par[iv])))
                if (include_delta) 
                  par[id[1L]] <- delta_starts[k]
                par
            })
        }
        reuse_starts <- is.null(control$optim_control) && !length(supplied)
        fits <- list()
        for (k in seq_along(starts)) {
            repeated <- if (reuse_starts) 
                Position(function(previous) identical(previous, starts[[k]], num.eq = FALSE), starts[seq_len(k - 1L)])
            else NA
            fits[[k]] <- if (is.na(repeated)) 
                run_fit(starts[[k]])
            else fits[[repeated]]
        }
        objectives <- vapply(fits, function(f) f$objective, numeric(1))
        if (!any(is.finite(objectives))) 
            stop("All pairwise optimization starts failed.", call. = FALSE)
        best <- fits[[which.min(objectives)]]
        par <- best$par
        for (iteration in seq_len(3L)) {
            boundary <- par[iv] <= boundary_tol
            if (!any(boundary)) 
                break
            initial <- par
            initial[iv[boundary]] <- 0
            candidate <- run_fit(initial, setdiff(seq_len(np), iv[boundary]))
            if (!is.finite(candidate$objective) || candidate$objective > best$objective + 1e-07 * max(1, 
                abs(best$objective))) 
                break
            best <- candidate
            par <- candidate$par
            g <- fit_evaluation(candidate)$gradient/total_weight
            release <- iv[boundary][g[iv[boundary]] < -score_tol]
            if (!length(release)) 
                break
            initial <- par
            initial[release] <- max(10 * boundary_tol, 1e-05)
            best <- run_fit(initial)
            par <- best$par
        }
        at_opt <- fit_evaluation(best)
        boundary <- par[iv] == 0
        free <- setdiff(seq_len(np), iv[boundary])
        g <- at_opt$gradient/total_weight
        projected <- g
        projected[iv[boundary]] <- pmin(g[iv[boundary]], 0)
        kkt <- max(abs(projected))
        if (is.finite(at_opt$value) && is.finite(kkt) && kkt > score_tol) {
            previous_control <- opt_control
            opt_control$iter.max <- max(opt_control$iter.max, 10000L)
            opt_control$eval.max <- max(opt_control$eval.max, 30000L)
            opt_control$rel.tol <- min(opt_control$rel.tol, 1e-15)
            opt_control$x.tol <- min(opt_control$x.tol, 1e-14)
            candidate <- run_fit(par, known = at_opt)
            opt_control <- previous_control
            candidate_eval <- fit_evaluation(candidate)
            candidate_boundary <- candidate$par[iv] == 0
            candidate_score <- candidate_eval$gradient/total_weight
            candidate_projected <- candidate_score
            candidate_projected[iv[candidate_boundary]] <- pmin(candidate_score[iv[candidate_boundary]], 0)
            candidate_kkt <- max(abs(candidate_projected))
            if (is.finite(candidate_eval$value) && is.finite(candidate_kkt) &&
                candidate_kkt < kkt && candidate_eval$value <= at_opt$value +
                1e-12 * max(1, abs(at_opt$value))) {
                best <- candidate
                par <- candidate$par
                at_opt <- candidate_eval
                boundary <- candidate_boundary
                free <- setdiff(seq_len(np), iv[boundary])
                g <- candidate_score
                projected <- candidate_projected
                kkt <- candidate_kkt
            }
        }
        newton_polish_steps <- 0L
        projected_gradient_polish_steps <- 0L
        roundoff_polish_steps <- 0L
        if (is.finite(at_opt$value) && is.finite(kkt) && kkt > score_tol) {
            polish_trial <- function(cand) {
                for (trial in trials) if (identical(trial$par, cand, num.eq = FALSE))
                  return(trial)
                value <- evaluate(cand, scores = FALSE)
                trial_score <- value$gradient/total_weight
                trial_boundary <- cand[iv] == 0
                trial_projected <- trial_score
                trial_projected[iv[trial_boundary]] <- pmin(trial_score[iv[trial_boundary]], 0)
                trial <- list(par = cand, value = value$value, gradient = value$gradient,
                  boundary = trial_boundary, score = trial_score, projected = trial_projected,
                  kkt = max(abs(trial_projected)), precision_exception = FALSE)
                trials[[length(trials) + 1L]] <<- trial
                trial
            }
            previous_rule <- function(cand) {
                trial <- polish_trial(cand)
                if (is.finite(trial$value) && is.finite(trial$kkt) && trial$kkt < kkt &&
                  trial$value <= at_opt$value + 1e-12 * max(1, abs(at_opt$value)))
                  trial
                else NULL
            }
            assess_polish_trial <- function(cand) {
                if (any(!is.finite(cand)))
                  return(NULL)
                displacement <- cand - par
                slope <- sum(g_now * displacement)
                if (!is.finite(slope) || slope >= 0)
                  return(NULL)
                trial <- polish_trial(cand)
                if (!is.finite(trial$value) || any(!is.finite(trial$gradient)))
                  return(NULL)
                trial_kkt <- trial$kkt
                if (!is.finite(trial_kkt))
                  return(NULL)
                current_value <- at_opt$value/total_weight
                trial_value <- trial$value/total_weight
                predicted_decrease <- -slope
                required_decrease <- 1e-4 * predicted_decrease
                sufficient_decrease <- required_decrease > 0 &&
                  current_value - trial_value >= required_decrease
                precision_exception <- FALSE
                if (!sufficient_decrease) {
                  roundoff <- 8 * .Machine$double.eps *
                    max(1, abs(current_value), abs(trial_value))
                  precision_exception <- predicted_decrease <= roundoff &&
                    trial_value - current_value <= roundoff && trial_kkt < kkt
                }
                if (!sufficient_decrease && !precision_exception)
                  return(NULL)
                trial$precision_exception <- precision_exception
                trial
            }
            newton_search <- function(accept, descent_only) {
                accepted <- NULL
                repeat {
                  nfree <- setdiff(seq_len(np), held)
                  hf <- hn[nfree, match(nfree, cols), drop = FALSE]
                  hf <- (hf + t(hf))/2
                  step <- if (all(is.finite(hf)))
                    tryCatch(solve(hf, at_opt$gradient[nfree]), error = function(e) NULL)
                  else NULL
                  if (is.null(step) || any(!is.finite(step))) {
                    if (!length(released))
                      break
                    held <- c(held, released)
                    released <- integer()
                    next
                  }
                  down <- released[step[match(released, nfree)] >= 0]
                  if (length(down)) {
                    held <- c(held, down)
                    released <- setdiff(released, down)
                    next
                  }
                  if (!descent_only || sum(at_opt$gradient[nfree] * step) > 0)
                    for (halving in 0:30) {
                      cand <- par
                      cand[nfree] <- par[nfree] - step/2^halving
                      cand[iv] <- pmax(cand[iv], 0)
                      accepted <- accept(cand)
                      if (!is.null(accepted))
                        break
                    }
                  if (!is.null(accepted) || !length(released))
                    break
                  held <- c(held, released)
                  released <- integer()
                }
                accepted
            }
            for (newton_iter in seq_len(50L)) {
                g_now <- at_opt$gradient/total_weight
                held <- iv[par[iv] == 0 & g_now[iv] >= -score_tol]
                released <- iv[par[iv] == 0 & g_now[iv] < -score_tol]
                cols <- setdiff(seq_len(np), held)
                hn <- matrix(NA_real_, np, length(cols))
                for (k in seq_along(cols)) {
                  j <- cols[k]
                  h <- fd_step * max(1, abs(par[j]))
                  plus <- par
                  plus[j] <- par[j] + h
                  if (j %in% iv && par[j] < h) {
                    plus2 <- par
                    plus2[j] <- par[j] + 2 * h
                    hn[, k] <- (-3 * at_opt$gradient + 4 * evaluate(plus, scores = FALSE)$gradient -
                      evaluate(plus2, scores = FALSE)$gradient)/(2 * h)
                  }
                  else {
                    minus <- par
                    minus[j] <- par[j] - h
                    hn[, k] <- (evaluate(plus, scores = FALSE)$gradient - evaluate(minus, scores = FALSE)$gradient)/(2 *
                      h)
                  }
                }
                trials <- list()
                accepted <- newton_search(previous_rule, descent_only = TRUE)
                if (is.null(accepted))
                  accepted <- newton_search(assess_polish_trial, descent_only = FALSE)
                used_projected_gradient <- FALSE
                if (is.null(accepted)) {
                  for (halving in 0:30) {
                    cand <- par - g_now/2^halving
                    cand[iv] <- pmax(cand[iv], 0)
                    accepted <- assess_polish_trial(cand)
                    if (!is.null(accepted)) {
                      used_projected_gradient <- TRUE
                      break
                    }
                  }
                }
                if (is.null(accepted))
                  break
                par <- accepted$par
                at_opt <- evaluate(par)
                boundary <- accepted$boundary
                free <- setdiff(seq_len(np), iv[boundary])
                g <- accepted$score
                projected <- accepted$projected
                kkt <- accepted$kkt
                if (used_projected_gradient)
                  projected_gradient_polish_steps <- projected_gradient_polish_steps + 1L
                else newton_polish_steps <- newton_polish_steps + 1L
                if (accepted$precision_exception)
                  roundoff_polish_steps <- roundoff_polish_steps + 1L
                if (kkt <= score_tol)
                  break
            }
        }
        converged <- is.finite(at_opt$value) && is.finite(kkt) && kkt <= score_tol
        hessian <- matrix(NA_real_, np, np, dimnames = list(nm, nm))
        for (j in seq_len(np)) {
            h <- fd_step * max(1, abs(par[j]))
            plus <- par
            plus[j] <- par[j] + h
            if (j %in% iv && par[j] < h) {
                plus2 <- par
                plus2[j] <- par[j] + 2 * h
                hessian[, j] <- (-3 * at_opt$gradient + 4 * evaluate_fit(plus)$gradient - 
                  evaluate_fit(plus2)$gradient)/(2 * h)
            }
            else {
                minus <- par
                minus[j] <- par[j] - h
                hessian[, j] <- (evaluate_fit(plus)$gradient - evaluate_fit(minus)$gradient)/(2 * 
                  h)
            }
        }
        asymmetry <- if (all(is.finite(hessian))) 
            max(abs(hessian - t(hessian)))
        else Inf
        hessian <- (hessian + t(hessian))/2
        eigenvalues <- rep(NA_real_, length(free))
        if (all(is.finite(hessian[free, free, drop = FALSE]))) {
            scale <- pmax(1, abs(par[free]))
            eigenvalues <- eigen(hessian[free, free, drop = FALSE] * outer(scale, scale), symmetric = TRUE, 
                only.values = TRUE)$values
        }
        max_eigen <- max(abs(eigenvalues), .Machine$double.eps, na.rm = TRUE)
        numerical_rank <- sum(is.finite(eigenvalues) & eigenvalues > rank_tol * max_eigen)
        identified <- all(is.finite(eigenvalues)) && numerical_rank == length(free)
        names(boundary) <- colnames(ZO)
        list(par = setNames(par, nm), alpha = setNames(par[ia], colnames(XO)), delta = if (include_delta) setNames(par[id], 
            colnames(ZO)) else setNames(numeric(), character()), sigma2_q = setNames(par[iv], colnames(ZO)), 
            free = free, hessian = hessian, eval = evaluate, converged = converged, identified = identified, 
            boundary = boundary, diagnostics = list(objective = at_opt$value, optimizer_code = best$convergence, 
                optimizer_message = best$message, newton_polish_steps = newton_polish_steps,
                projected_gradient_polish_steps = projected_gradient_polish_steps,
                roundoff_polish_steps = roundoff_polish_steps,
                scaled_kkt_error = kkt, score_tolerance = score_tol, 
                total_composite_weight = total_weight, starts = length(starts), start_objectives = objectives, 
                hessian_eigenvalues = eigenvalues, numerical_rank = numerical_rank, n_free = length(free), 
                hessian_asymmetry = asymmetry, identification_note = "Numerical local sensitivity rank on the selected variance face; not a proof of global identifiability.", 
                inference_note = if (any(boundary)) "Variance boundary: regular Wald inference excludes zero variance coordinates." else "No covariance is fabricated; use the returned scores and sensitivity for inference.", 
                singleton_subjects = sum(singleton), paired_subjects = sum(paired), pair_weight = "1/(m-1); singleton weight 1"))
    }
    .lu_functionals <- function(par, XO, ZO, posterior, include_delta = TRUE) {
        XO <- as.matrix(XO)
        ZO <- as.matrix(ZO)
        n <- nrow(XO)
        if (nrow(ZO) != n || !all(is.finite(XO)) || !all(is.finite(ZO))) {
            stop("Observation designs must be finite matrices with matching rows.", call. = FALSE)
        }
        if (is.null(colnames(XO)) || is.null(colnames(ZO))) {
            stop("Observation designs require column names.", call. = FALSE)
        }
        required <- c(paste0("alpha.", colnames(XO)), if (include_delta) paste0("delta.", colnames(ZO)), 
            paste0("sigma2_q.", colnames(ZO)))
        if (is.null(names(par)) || anyDuplicated(names(par)) || !setequal(names(par), required) || any(!is.finite(par))) {
            stop("Observation parameters do not match the named raw-parameter contract.", call. = FALSE)
        }
        mu <- as.numeric(posterior$mu)
        s2 <- as.numeric(posterior$s2)
        if (length(mu) != n || length(s2) != n || !all(is.finite(mu), is.finite(s2)) || any(s2 < 0)) {
            stop("Laplace posterior moments are invalid.", call. = FALSE)
        }
        alpha <- par[paste0("alpha.", colnames(XO))]
        delta <- if (include_delta) 
            par[paste0("delta.", colnames(ZO))]
        else numeric(ncol(ZO))
        variance <- par[paste0("sigma2_q.", colnames(ZO))]
        if (any(variance < 0)) {
            stop("Observation variance components must be nonnegative.", call. = FALSE)
        }
        d <- drop(ZO %*% delta)
        v <- drop((ZO^2) %*% variance)
        total <- 1 + v + d^2 * s2
        k <- (drop(XO %*% alpha) + d * mu)/sqrt(total)
        log_omega <- stats::pnorm(k, log.p = TRUE)
        omega <- exp(log_omega)
        mills <- exp(stats::dnorm(k, log = TRUE) - log_omega)
        kappa <- if (isTRUE(attr(par, "zero_compensation"))) mills/sqrt(total) else mu + d * s2/sqrt(total) * mills
        if (!all(is.finite(total), is.finite(k), is.finite(omega), is.finite(kappa)) || any(total <= 0)) {
            stop("Gaussian observation functionals are nonfinite; rescale the model.", call. = FALSE)
        }
        list(omega = omega, kappa = kappa, log_omega = log_omega, k = k, total_variance = total, observation_variance = v, 
            shared_loading = d)
    }
    .lu_stage3 <- function(data, sf, subj_index, time_vec, y_vec, r_vec, visit_mask, stage1, covars_outcome_fixed_XY, 
        covars_outcome_random_link_ZY, time_col, outcome_fixed_structure = NULL, outcome_random_structure = NULL, 
        outcome_random_intercept = TRUE, omega, kappa, include_theta = TRUE, zero_spec = NULL) {
        n <- length(sf)
        if (!is.environment(.cimehr_affine_engine) || !is.function(.cimehr_affine_engine$.cimehr_affine_outcome_design) || 
            !is.function(.cimehr_affine_engine$.cimehr_affine_stage3)) {
            stop("The private Stage-3 routines have not been loaded.", call. = FALSE)
        }
        if (length(omega) != n || length(kappa) != n || any(!is.finite(omega)) || any(omega <= 0) || 
            any(omega > 1) || any(!is.finite(kappa))) {
            stop("Stage-3 omega/kappa must be finite subject-level vectors with 0 < omega <= 1.", call. = FALSE)
        }
        if (!is.logical(include_theta) || length(include_theta) != 1L || is.na(include_theta)) {
            stop("include_theta must be TRUE or FALSE.", call. = FALSE)
        }
        if (length(stage1$mi) != n || length(stage1$Lambda_C) != n || length(stage1$last_risk_idx) != 
            n) {
            stop("Stage-1 subject dimensions do not match the Stage-3 design.", call. = FALSE)
        }
        design_time <- time_vec
        attr(design_time, "column") <- time_col
        design <- .cimehr_affine_engine$.cimehr_affine_outcome_design(data, sf, subj_index, design_time, 
            covars_outcome_fixed_XY, covars_outcome_random_link_ZY, outcome_fixed_structure, outcome_random_structure, 
            outcome_random_intercept)
        prefix <- "theta."
        if (!is.null(zero_spec)) {
            zdesign <- .lu_zero_compensation(zero_spec, design$W0, design$W1)
            design$W0 <- zdesign$W0; design$W1 <- zdesign$W1
            zero_spec <- zdesign$spec; prefix <- zdesign$prefix
        }
        include_theta <- include_theta && ncol(design$W0) > 0L
        p_beta <- ncol(design$X0)
        D0 <- design$X0
        D1 <- design$X1
        if (include_theta) {
            B0 <- kappa * design$W0
            B1 <- kappa * design$W1
            colnames(B0) <- colnames(B1) <- paste0(prefix, colnames(design$W0))
            D0 <- cbind(D0, B0)
            D1 <- cbind(D1, B1)
        }
        if (!ncol(D0)) 
            stop("Stage 3 has no identified outcome coefficients.", call. = FALSE)
        w <- omega * stage1$mi/pmax(stage1$Lambda_C, 1e-30)
        w[stage1$mi == 0L | stage1$Lambda_C <= 0] <- 0
        eo_idx <- which(visit_mask & !is.na(r_vec) & r_vec == 1L & is.finite(y_vec))
        if (!length(eo_idx)) 
            stop("No observed outcome rows.", call. = FALSE)
        eo_subject <- subj_index[eo_idx]
        visit_position <- rep(NA_integer_, length(visit_mask))
        visit_position[stage1$visit_rows] <- seq_along(stage1$visit_rows)
        time_k <- stage1$event_time_idx[visit_position[eo_idx]]
        if (anyNA(time_k)) 
            stop("An outcome row is absent from the Stage-1 visit grid.", call. = FALSE)
        event_time <- time_vec[eo_idx]
        last_risk <- stage1$last_risk_idx
        first_risk <- stage1$first_risk_idx
        profile <- tryCatch(.cimehr_affine_engine$.cimehr_affine_stage3(D0, D1, y_vec[eo_idx], eo_subject, time_k,
            event_time, w, last_risk, first_risk = first_risk, shared = TRUE,
            zero_rank_tolerance = if (is.null(zero_spec)) NULL else zero_spec$tolerance), error = function(e) {
              if (!is.null(zero_spec) && grepl("singular|rank|numerical", conditionMessage(e), ignore.case=TRUE))
                .lu_zero_stop(zero_spec$posterior, "compensation_not_separable_from_outcome", "outcome",
                    detail = paste0(conditionMessage(e), " This numerical check does not establish population nonidentification."))
              stop(e)
            })
        shared <- profile$shared
        profile$shared <- NULL
        coef <- stats::setNames(profile$coefficients, colnames(D0))
        K <- nrow(profile$baseline_increments)
        S0 <- shared$S0
        center0 <- shared$center0
        center1 <- shared$center1
        D <- shared$D
        Dbar_event <- shared$center
        H <- shared$H
        residual <- shared$residual
        event_score <- shared$event_score
        dA0 <- profile$baseline_increments$dA0
        dA1 <- profile$baseline_increments$dA1
        A0 <- c(0, cumsum(dA0))
        A1 <- c(0, cumsum(dA1))
        random_rows <- if (include_theta) {
            design$W0[eo_subject, , drop = FALSE] + design$W1[eo_subject, , drop = FALSE] * event_time
        }
        else matrix(0, length(eo_idx), 0L)
        random_integral <- if (include_theta)
            cimehr_stage3_random_integral_cpp(design$W0, design$W1, dA0, dA1,
                as.integer(last_risk), as.integer(first_risk))
        else matrix(0, n, 0L)
        baseline_mean <- rep(NA_real_, K)
        if (length(stage1$dLambda) == K) {
            positive <- is.finite(stage1$dLambda) & stage1$dLambda > 0
            baseline_mean[positive] <- dA0[positive]/stage1$dLambda[positive]
        }
        grid_time <- stage1$unique_times
        if (length(grid_time) == K) {
            names(dA0) <- names(dA1) <- names(baseline_mean) <- grid_time
        }
        A <- cumsum(dA0)
        list(beta = coef[seq_len(p_beta)], theta = if (include_theta) coef[p_beta + seq_len(ncol(design$W0))] else stats::setNames(numeric(0), 
            character(0)), coef = coef, vcov_conditional = profile$covariance, subject_score = profile$subject_score, 
            event_score = event_score, bread = profile$bread, inverse_bread = shared$inverse, risk_weight = w, 
            risk_integral = profile$risk_integral, D = D, H = H, Dbar_event = Dbar_event, center0 = center0, 
            center1 = center1, residual = residual, eo_idx = eo_idx, eo_subject = eo_subject, time_k = time_k, 
            event_time = event_time, S0 = S0, n_outcomes = length(eo_idx), dA = dA0, dA_time = dA1, A = A, 
            baseline_mean = baseline_mean, baseline_increments = profile$baseline_increments, random_rows = random_rows, 
            random_integral = random_integral, XY = design$X0, ZY = design$W0, design = design, include_theta = include_theta, 
            conditional_profile_inference = profile, solve_state = profile$solve_state,
            covariance_factor = profile$covariance_factor, zero_spec = zero_spec)
    }
    .lu_stacked <- function(stage1, pcl, stage3, XV, XY, ZO, XO, posterior, include_delta = TRUE, derivative_step = 5e-05, 
        retain_details = FALSE) {
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
        if (!isTRUE(pcl$zero_quotient) && length(fixed) && any(!startsWith(names(par)[fixed], "sigma2_q.") | par[fixed] != 0)) {
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
        XO <- as.matrix(XO)
        ZO <- as.matrix(ZO)
        alpha_names <- paste0("alpha.", colnames(XO))
        delta_names <- paste0("delta.", colnames(ZO))
        variance_names <- paste0("sigma2_q.", colnames(ZO))
        ZO2 <- ZO^2
        mu0 <- as.numeric(posterior$mu)
        s20 <- as.numeric(posterior$s2)
        linear0 <- drop(XO %*% par[alpha_names])
        functionals_at <- function(mu = mu0, s2 = s20, d = f0$shared_loading, v = f0$observation_variance, 
            linear = linear0) {
            total <- 1 + v + d^2 * s2
            k <- (linear + d * mu)/sqrt(total)
            log_omega <- stats::pnorm(k, log.p = TRUE)
            omega <- exp(log_omega)
            mills <- exp(stats::dnorm(k, log = TRUE) - log_omega)
            kappa <- if (isTRUE(pcl$zero_quotient)) mills/sqrt(total) else mu + d * s2/sqrt(total) * mills
            if (!all(is.finite(total), is.finite(k), is.finite(omega), is.finite(kappa)) || any(total <= 0)) {
                stop("Gaussian observation functionals are nonfinite; rescale the model.", call. = FALSE)
            }
            list(omega = omega, kappa = kappa)
        }
        posterior_functionals <- function(post) {
            mu <- as.numeric(post$mu)
            s2 <- as.numeric(post$s2)
            if (length(mu) != nrow(XO) || length(s2) != nrow(XO) || !all(is.finite(mu), is.finite(s2)) || 
                any(s2 < 0)) {
                stop("Laplace posterior moments are invalid.", call. = FALSE)
            }
            functionals_at(mu, s2)
        }
        parameter_functionals <- function(par, k) {
            if (!is.finite(par[k])) {
                stop("Observation parameters do not match the named raw-parameter contract.", call. = FALSE)
            }
            if (names(par)[k] %in% alpha_names) 
                return(functionals_at(linear = drop(XO %*% par[alpha_names])))
            if (names(par)[k] %in% delta_names) 
                return(functionals_at(d = drop(ZO %*% par[delta_names])))
            variance <- par[variance_names]
            if (any(variance < 0)) {
                stop("Observation variance components must be nonnegative.", call. = FALSE)
            }
            functionals_at(v = drop(ZO2 %*% variance))
        }
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
            fp <- posterior_functionals(post_plus)
            fm <- posterior_functionals(post_minus)
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
            fp <- posterior_functionals(post_plus)
            fm <- posterior_functionals(post_minus)
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
            fp <- parameter_functionals(plus_par, k)
            fm <- parameter_functionals(minus_par, k)
            fd$omega_eta[, j] <- (fp$omega - fm$omega)/(2 * step)
            fd$kappa_eta[, j] <- (fp$kappa - fm$kappa)/(2 * step)
        }
        if (!all(is.finite(D_nu), is.finite(D_sigma), vapply(fd, function(x) all(is.finite(x)), logical(1))) || 
            any(!is.finite(if_sigma))) {
            stop("A downstream derivative or frailty influence is nonfinite.", call. = FALSE)
        }
        s2_nu_chain <- s1$apply_nu(D_nu)
        s2_sigma_chain <- outer(if_sigma, colSums(D_sigma))
        s2_effective <- base_score + s2_nu_chain + s2_sigma_chain
        if_eta <- s2_effective %*% t(Hinv)
        colnames(if_eta) <- eta_names
        s3 <- .cimehr_shared_engine$.cimehr_stage3_case_linearization(stage3, stage1, XV, XY, matrix(0, 
            n, 0L), FALSE, fd)
        nu_chain <- s1$apply_nu(s3$derivative_nu)
        stable_arithmetic <- !is.null(stage3$solve_state)
        gamma_chain <- if (ncol(s1$influence_gamma)) {
            if (stable_arithmetic) cimehr_stable_right_product_cpp(s1$influence_gamma, t(s3$derivative_gamma))
            else s1$influence_gamma %*% t(s3$derivative_gamma)
        }
        else matrix(0, n, length(stage3$coef))
        sigma_chain <- outer(if_sigma, s3$derivative_tau)
        eta_chain <- if (stable_arithmetic) cimehr_stable_right_product_cpp(if_eta, t(s3$derivative_eta))
            else if_eta %*% t(s3$derivative_eta)
        effective_outcome <- if (stable_arithmetic) cimehr_stable_matrix_sum_cpp(
            list(s3$direct_score, nu_chain, gamma_chain, sigma_chain, eta_chain)) else
            s3$direct_score + nu_chain + gamma_chain + sigma_chain + eta_chain
        if_outcome <- .cimehr_stage3_apply_inverse(effective_outcome, stage3$inverse_bread, stage3$solve_state)
        colnames(if_outcome) <- names(stage3$coef)
        scaled_sum <- function(M) {
            M <- as.matrix(M)
            if (!length(M)) 
                return(0)
            max(abs(colSums(M)))/max(1, sqrt(sum(M^2)))
        }
        raw_case_sums <- c(gamma = scaled_sum(s1$influence_gamma), sigma2_zeta = scaled_sum(if_sigma), 
            observation = scaled_sum(if_eta), outcome = scaled_sum(if_outcome))
        if (!all(is.finite(if_outcome), is.finite(if_eta), is.finite(s1$influence_gamma), is.finite(raw_case_sums))) {
            stop("Stacked case influences are nonfinite.", call. = FALSE)
        }
        if (max(raw_case_sums) > max(1e-04, 100 * derivative_step)) {
            stop("Stacked influence fails the uniform-case-weight invariance check.", call. = FALSE)
        }
        center <- function(M) {
            M <- as.matrix(M)
            if (ncol(M)) 
                sweep(M, 2L, colMeans(M), "-")
            else M
        }
        if_gamma <- center(s1$influence_gamma)
        if_sigma <- as.numeric(center(if_sigma))
        if_eta <- center(if_eta)
        if_outcome <- center(if_outcome)
        if_obs_face <- matrix(0, n, p, dimnames = list(NULL, names(par)))
        if_obs_face[, free] <- if_eta
        vcov_obs <- crossprod(if_obs_face)
        if (length(fixed)) {
            vcov_obs[fixed, ] <- NA_real_
            vcov_obs[, fixed] <- NA_real_
        }
        vcov_outcome <- crossprod(if_outcome)
        vcov_sigma2 <- matrix(if (frailty_regular) 
            sum(if_sigma^2)
        else NA_real_, 1L, 1L, dimnames = list("sigma2_zeta", "sigma2_zeta"))
        if (ncol(if_gamma)) 
            colnames(if_gamma) <- paste0("gamma.", colnames(XV))
        outcome_joint_names <- names(stage3$coef)
        p_beta <- length(stage3$beta)
        if (p_beta) 
            outcome_joint_names[seq_len(p_beta)] <- paste0("beta.", names(stage3$beta))
        if (length(outcome_joint_names) > p_beta) {
            theta_at <- seq.int(p_beta + 1L, length(outcome_joint_names))
            no_prefix <- !startsWith(outcome_joint_names[theta_at], "theta.") & !startsWith(outcome_joint_names[theta_at], "compensation.")
            outcome_joint_names[theta_at[no_prefix]] <- paste0("theta.", outcome_joint_names[theta_at[no_prefix]])
        }
        outcome_joint <- if_outcome
        colnames(outcome_joint) <- outcome_joint_names
        joint_face <- cbind(if_gamma, sigma2_zeta = if_sigma, if_obs_face, outcome_joint)
        if (anyDuplicated(colnames(joint_face))) {
            stop("Joint raw-parameter names are not unique.", call. = FALSE)
        }
        unavailable <- c(if (!frailty_regular) "sigma2_zeta", names(par)[fixed])
        vcov_joint <- crossprod(joint_face)
        if (length(unavailable)) {
            vcov_joint[unavailable, ] <- NA_real_
            vcov_joint[, unavailable] <- NA_real_
        }
        frailty_statistic <- if (frailty_regular) a/sqrt(sum(if_sigma^2)) else NA_real_
        frailty_weak <- isTRUE(frailty_statistic <= sqrt(log(n)))
        regular <- frailty_regular && include_delta && !length(fixed)
        inference_scope <- if (regular) "all-stage stacked sandwich for the Laplace-defined interior estimator" else
            "all-stage stacked sandwich conditional on the selected frailty/variance face; boundary variance Wald inference unavailable"
        if (frailty_weak) inference_scope <- paste0(inference_scope,
            "; the positive frailty variance has a weak standardized signal relative to zero (descriptive diagnostic); computed covariance, SEs and intervals are retained")
        answer <- list(regular = regular, frailty_parameter_regular = frailty_regular, observation_variance_regular = !length(fixed),
            frailty_weak_identification = frailty_weak, frailty_identification = c(statistic = frailty_statistic,
                threshold = sqrt(log(n))), conditional_active_face_inference_available = TRUE,
            scope = inference_scope,
            approximation_scope = "Gaussian/Laplace approximation bias is not removed by this sandwich", 
            parameterization = "direct raw variance coordinates, including sigma2_zeta", vcov_outcome = vcov_outcome,
            covariance_factor = .cimehr_covariance_factor(if_outcome),
            vcov_beta = vcov_outcome[names(stage3$beta), names(stage3$beta), drop = FALSE], vcov_observation = vcov_obs, 
            vcov_gamma = crossprod(if_gamma), vcov_sigma2_zeta = vcov_sigma2, vcov_sigma2 = vcov_sigma2, 
            vcov_joint = vcov_joint, unavailable_wald_parameters = unavailable, diagnostics = list(derivative_step = derivative_step, 
                raw_influence_case_sums = raw_case_sums, stage2_relative_minimum_eigenvalue = attr(Hinv, 
                  "relative_minimum_eigenvalue"), centered_influences = TRUE))
        if (isTRUE(retain_details)) {
            influence_joint <- joint_face
            influence_observation <- if_obs_face
            if (length(unavailable)) 
                influence_joint[, unavailable] <- NA_real_
            if (length(fixed)) 
                influence_observation[, fixed] <- NA_real_
            answer$influence_joint <- influence_joint
            answer$influence_joint_active_face <- joint_face
            answer$influence_outcome <- if_outcome
            answer$influence_observation <- influence_observation
            answer$influence_gamma <- if_gamma
            answer$influence_sigma2_zeta <- if (frailty_regular) 
                if_sigma
            else rep(NA_real_, n)
            answer$effective_score_outcome <- effective_outcome
            answer$effective_score_observation <- s2_effective
            answer$components <- list(stage1 = s1, frailty = list(regular = frailty_regular, direct_moment = h, 
                derivative_nu = moment_nu, nu_chain = moment_nu_chain, effective_score = sigma_effective, 
                bread = bread_sigma2, influence_sigma2 = if_sigma), stage2 = list(score_subject = base_score, 
                derivative_nu = D_nu, derivative_sigma2_subject = D_sigma, nu_chain = s2_nu_chain, sigma2_chain = s2_sigma_chain, 
                influence_eta = if_eta, free = free, inverse_bread = Hinv), stage3 = s3, functionals = fd, 
                fitted_functionals = f0, outcome_chains = list(nu = nu_chain, gamma = gamma_chain, sigma2 = sigma_chain, 
                  observation = eta_chain))
        }
        answer
    }
    .lu_inverse_information <- function(H, label) {
        H <- as.matrix(H)
        if (nrow(H) != ncol(H) || any(!is.finite(H))) {
            stop(paste(label, "must be a finite square matrix."), call. = FALSE)
        }
        if (!nrow(H)) 
            return(H)
        asymmetry <- max(abs(H - t(H)))/max(1, max(abs(H)))
        if (asymmetry > 1e-06) {
            stop(paste(label, "is not numerically symmetric."), call. = FALSE)
        }
        H <- (H + t(H))/2
        if (any(diag(H) <= 0)) {
            stop(paste(label, "has a nonpositive diagonal."), call. = FALSE)
        }
        scale <- sqrt(diag(H))
        scaled <- H/outer(scale, scale)
        eigenvalues <- eigen(scaled, symmetric = TRUE, only.values = TRUE)$values
        relative_minimum <- min(eigenvalues)/max(eigenvalues)
        if (!is.finite(relative_minimum) || relative_minimum <= 1e-10) {
            stop(paste(label, "is not positive and full rank on the fitted free face."), call. = FALSE)
        }
        ans <- solve(H)
        attr(ans, "relative_minimum_eigenvalue") <- relative_minimum
        ans
    }
}, envir = .cimehr_updated)
.cimehr_updated_fit <- function(d, id = "id", time = "time", outcome = "Y", obs_indicator = "R", visit_covs = c("Z", 
    "X"), cimehr_visit_covs = NULL, obs_fixed_covs = NULL, long_covs = c("Z", "X"), random_covs = "Z", 
    outcome_random_covs = NULL, obs_random_covs = NULL, censor_col = "C", outcome_fixed_structure = NULL, 
    outcome_random_structure = NULL, outcome_random_intercept = TRUE, entry_col = NULL, ...) {
    if (is.null(cimehr_visit_covs)) 
        cimehr_visit_covs <- visit_covs
    if (is.null(obs_fixed_covs)) 
        obs_fixed_covs <- cimehr_visit_covs
    if (is.null(outcome_random_covs)) 
        outcome_random_covs <- random_covs
    if (is.null(obs_random_covs)) 
        obs_random_covs <- random_covs
    fit <- .cimehr_updated$.cimehr_updated_native(data = d, id_col = id, time_col = time, y_col = outcome, 
        r_col = obs_indicator, censor_col = censor_col, covars_visit_XV = cimehr_visit_covs, covars_outcome_fixed_XY = long_covs, 
        covars_outcome_random_link_ZY = outcome_random_covs, covars_obs_fixed_XO = obs_fixed_covs, covars_obs_random_link_ZO = obs_random_covs, 
        outcome_fixed_structure = outcome_fixed_structure, outcome_random_structure = outcome_random_structure, 
        outcome_random_intercept = outcome_random_intercept, entry_col = entry_col, ...)
    ans <- list(method_key = "cimehr", method = "CIMEHR", estimand = "Laplace-defined three-stage CIMEHR target", 
        coefficients = fit$coefficients, se = fit$se, vcov = fit$vcov, covariance_factor = fit$covariance_factor,
        vcov_all = fit$vcov_all, vcov_type = fit$diagnostics$inference_scope,
        fit = fit, diagnostics = fit$diagnostics)
    inf <- fit$inference
    ans$diagnostics$identified <- isTRUE(ans$diagnostics$stage2_scale_identified)
    ans$diagnostics$regular <- isTRUE(inf$regular)
    ans$diagnostics$conditional_inference_valid <- isTRUE(inf$conditional_active_face_inference_available)
    ans$diagnostics$conditional_inference_available <- ans$diagnostics$conditional_inference_valid
    if (isTRUE(ans$diagnostics$identification_diagnostics$zero_frailty))
        ans$diagnostics$conditional_inference_valid <-
            ans$diagnostics$conditional_inference_available &&
            !isTRUE(ans$diagnostics$covariance_failed) &&
            .cimehr_identification_assessment(ans$diagnostics$identification_diagnostics, stage = "outcome")$valid
    ans$diagnostics$frailty_boundary <- fit$visiting_process$sigma2_zeta == 0
    ans$diagnostics$frailty_weak_identification <- isTRUE(inf$frailty_weak_identification)
    ans$diagnostics$observation_variance_boundary <- any((fit$observation_process$Sigma_q %||% fit$observation_process$total_variance) == 0)
    ans$diagnostics$variance_boundary <- ans$diagnostics$frailty_boundary || ans$diagnostics$observation_variance_boundary
    ans$diagnostics$message <- paste(ans$diagnostics$inference_scope, "Laplace approximation bias is not removed by the sandwich.")
    ans$assumptions <- c("MOM visiting frailty and Gaussian/Laplace posterior", "Within-patient pairwise probit observation likelihood with diagonal conditional variances", 
        "Centered outcome equation; all-stage patient sandwich", "Inference targets the Laplace-defined estimator; variance boundaries condition on the selected face")
    ans
}

CIMEHR <- function(
    data, id_col = "id", time_col = "time", y_col = "Y", r_col = "R",
    censor_col = "C", covars_visit_XV = NULL, covars_outcome_fixed_XY = NULL,
    covars_outcome_random_link_ZY = NULL, covars_obs_fixed_XO = NULL,
    covars_obs_random_link_ZO = NULL, time_precision = 0,
    outcome_fixed_structure = NULL, outcome_random_structure = NULL,
    outcome_random_intercept = TRUE, pairwise_control = list(),
    inference = c("stacked", "none"), derivative_step = 2e-5,
    retain_details = FALSE, entry_col = NULL) {
  inference <- match.arg(inference)
  data <- as.data.frame(data)
  .cimehr_check_numeric_columns(data, c(covars_visit_XV, covars_outcome_fixed_XY,
    covars_outcome_random_link_ZY, covars_obs_fixed_XO, covars_obs_random_link_ZO))
  ans <- fit_method('cimehr', data = data,
    id = id_col, time = time_col, outcome = y_col, obs_indicator = r_col,
    censor_col = censor_col, entry_col = entry_col, visit_covs = covars_visit_XV,
    cimehr_visit_covs = covars_visit_XV,
    obs_fixed_covs = if (is.null(covars_obs_fixed_XO)) character() else covars_obs_fixed_XO,
    long_covs = covars_outcome_fixed_XY,
    outcome_random_covs = if (is.null(covars_outcome_random_link_ZY)) character() else covars_outcome_random_link_ZY,
    obs_random_covs = if (is.null(covars_obs_random_link_ZO)) character() else covars_obs_random_link_ZO,
    outcome_fixed_structure = outcome_fixed_structure,
    outcome_random_structure = outcome_random_structure,
    outcome_random_intercept = outcome_random_intercept,
    controls = list(time_precision = time_precision, pairwise_control = pairwise_control,
      inference = inference, derivative_step = derivative_step, retain_details = retain_details))
  ans$call <- match.call()
  class(ans) <- c("cimehr_fit", "list")
  ans
}
