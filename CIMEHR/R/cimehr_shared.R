.cimehr_common <- new.env(parent = environment())
evalq({
`%||%` <- function(x, y) if (is.null(x)) y else x
`.cimehr_fit_visiting` <- function(time_vec, visit_mask, subj_index, Ci_subj, XV_subj, time_precision = 0, score_tol = 3e-07, maxit = 60L, Ei_subj) {
    n_subj <- nrow(XV_subj)
    p_V <- ncol(XV_subj)
    visit_rows <- which(visit_mask)
    if (length(visit_rows) == 0L) 
        stop("No visited rows.", call. = FALSE)
    Ci_original <- Ci_subj
    Ei_original <- as.numeric(Ei_subj)
    raw_times <- time_vec[visit_rows]
    if (!is.null(time_precision) && time_precision > 0) {
        raw_times <- round(raw_times/time_precision) * time_precision
        Ci_disc <- round(Ci_subj/time_precision) * time_precision
        Ei_disc <- round(Ei_original/time_precision) * time_precision
    }
    else {
        Ci_disc <- Ci_subj
        Ei_disc <- Ei_original
    }
    event_subj_idx <- subj_index[visit_rows]
    if (is.object(raw_times)) {
        unique_times <- sort(unique(raw_times))
        K <- length(unique_times)
        event_time_idx <- match(raw_times, unique_times)
        visit_key <- paste(event_subj_idx, format(raw_times, digits = 17), sep = "\r")
    }
    else {
        time_order <- order(raw_times, method = "radix")
        sorted_times <- raw_times[time_order]
        new_time <- c(TRUE, sorted_times[-1L] != sorted_times[-length(sorted_times)])
        unique_times <- as.vector(sorted_times[new_time])
        K <- length(unique_times)
        event_time_idx <- integer(length(raw_times))
        event_time_idx[time_order] <- cumsum(new_time)
        visit_key <- event_subj_idx * (K + 1) + event_time_idx
    }
    if (anyDuplicated(visit_key)) {
        stop("`time_precision` collapses two visits from the same subject to one time; use a finer precision.", 
            call. = FALSE)
    }
    dN <- tabulate(event_time_idx, nbins = K)
    mi <- tabulate(event_subj_idx, nbins = n_subj)
    if (any(raw_times <= Ei_disc[event_subj_idx]))
        stop("Every Stage-1 visit must lie in its subject's risk window (entry, C]: visits at the entry time must be masked out before Stage 1, and a time_precision that rounds a visit onto its entry time is too coarse.", 
            call. = FALSE)
    last_risk_idx <- findInterval(Ci_disc, unique_times)
    first_risk_idx <- findInterval(Ei_disc, unique_times) + 1L
    valid <- which(last_risk_idx >= first_risk_idx)
    lri <- last_risk_idx[valid]
    fri <- first_risk_idx[valid]
    risk_scalar <- function(w) {
        out <- numeric(K)
        late <- numeric(K + 1L)
        if (length(valid)) {
            wv <- w[valid]
            if (is.double(wv) && !anyNA(wv)) 
                return(cimehr_window_risk_sum(matrix(wv), lri, fri, K)[, 1L])
            z <- rowsum(w[valid], lri, reorder = FALSE)
            out[as.integer(rownames(z))] <- z[, 1L]
            z <- rowsum(w[valid], fri, reorder = FALSE)
            late[as.integer(rownames(z))] <- z[, 1L]
        }
        rev(cumsum(rev(out))) - rev(cumsum(rev(late)))[-1L]
    }
    risk_matrix <- function(w, M) {
        out <- matrix(0, K, ncol(M))
        if (ncol(M) == 0L) 
            return(out)
        late <- matrix(0, K + 1L, ncol(M))
        if (length(valid)) {
            wM <- w[valid] * M[valid, , drop = FALSE]
            if (is.matrix(wM) && is.double(wM) && !anyNA(wM)) 
                return(cimehr_window_risk_sum(wM, lri, fri, K))
            z <- rowsum(wM, lri, reorder = FALSE)
            out[as.integer(rownames(z)), ] <- as.matrix(z)
            z <- rowsum(wM, fri, reorder = FALSE)
            late[as.integer(rownames(z)), ] <- as.matrix(z)
        }
        for (j in seq_len(ncol(out))) out[, j] <- rev(cumsum(rev(out[, j])))
        for (j in seq_len(ncol(late))) late[, j] <- rev(cumsum(rev(late[, j])))
        out - late[-1L, , drop = FALSE]
    }
    Xe <- XV_subj[event_subj_idx, , drop = FALSE]
    score_hess <- function(gamma) {
        rs <- if (p_V) 
            drop(exp(XV_subj %*% gamma))
        else rep(1, n_subj)
        sums <- if (p_V)
            cimehr_s1_score_hess(rs, XV_subj, valid, lri, fri, K, dN, event_time_idx, event_subj_idx)
        else list(S0 = risk_scalar(rs))
        S0 <- sums$S0
        if (any(S0[event_time_idx] <= 0)) {
            stop("Empty visiting-process risk set at an event time.", call. = FALSE)
        }
        if (!p_V) {
            return(list(score = numeric(0), hessian = matrix(0, 0, 0), objective = -sum(dN * log(S0)), 
                S0 = S0, Xbar = matrix(0, K, 0L), rs = rs))
        }
        score <- sums$score
        names(score) <- colnames(Xe)
        objective <- sum(Xe %*% gamma) - sum(dN * log(S0))
        list(score = score, hessian = sums$hessian, objective = objective, S0 = S0, Xbar = sums$Xbar, rs = rs)
    }
    objective_increment <- function(current, delta) {
        delta_lp <- drop(XV_subj %*% delta)
        relative_change <- risk_scalar(current$rs * expm1(delta_lp))/current$S0
        if (any(!is.finite(relative_change)) || any(relative_change <= -1)) {
            return(-Inf)
        }
        sum(mi * delta_lp) - sum(dN * log1p(relative_change))
    }
    gamma <- numeric(p_V)
    converged <- !p_V
    used_fallback <- FALSE
    score_limit <- score_tol * max(1, length(visit_rows))
    cur <- NULL
    if (p_V) {
        for (iter in seq_len(maxit)) {
            if (is.null(cur))
                cur <- score_hess(gamma)
            if (max(abs(cur$score)) <= score_limit) {
                converged <- TRUE
                break
            }
            step <- tryCatch(solve(cur$hessian, cur$score), error = function(e) NULL)
            if (is.null(step) || any(!is.finite(step))) {
                stop("Stage-1 visiting information matrix is singular.", call. = FALSE)
            }
            candidate_step <- step
            accepted <- FALSE
            for (ls in 0:20) {
                candidate <- gamma - candidate_step
                nxt <- score_hess(candidate)
                gain <- objective_increment(cur, -candidate_step)
                if (is.finite(nxt$objective) && is.finite(gain) && gain >= -1e-10) {
                  gamma <- candidate
                  accepted <- TRUE
                  break
                }
                candidate_step <- candidate_step/2
            }
            if (!accepted) {
                used_fallback <- TRUE
                break
            }
            cur <- nxt
        }
    }
    final <- if (is.null(cur)) 
        score_hess(gamma)
    else cur
    if (p_V && max(abs(final$score)) > score_limit) {
        used_fallback <- TRUE
        fallback_origin <- gamma
        fallback_current <- final
        fallback <- stats::nlminb(start = gamma, objective = function(g) -objective_increment(fallback_current, 
            g - fallback_origin), gradient = function(g) -score_hess(g)$score, lower = rep(-20, p_V), 
            upper = rep(20, p_V), control = list(iter.max = 200L, eval.max = 500L, rel.tol = 1e-12))
        gamma <- fallback$par
        final <- score_hess(gamma)
    }
    if (p_V && max(abs(final$score)) <= score_limit) 
        converged <- TRUE
    if (p_V) 
        names(gamma) <- colnames(XV_subj)
    dLambda <- dN/final$S0
    cumLambda <- cumsum(dLambda)
    Lambda_C <- numeric(n_subj)
    Lambda_C[valid] <- cumLambda[lri] - c(0, cumLambda)[fri]
    nu <- final$rs * Lambda_C
    mm_den <- sum(nu^2)
    var_eta <- if (mm_den > 0) {
        max((sum(mi * (mi - 1L)) - mm_den)/mm_den, 0)
    }
    else 0
    sigma2_zeta_mom <- if (var_eta > 0) 
        log1p(var_eta)
    else 0
    if (p_V) {
        info <- -final$hessian
        inv_info <- .cimehr_safe_solve(info, name = "Stage-1 information")
        parts <- cimehr_s1_subject_parts(XV_subj, final$Xbar, dLambda, valid, lri, fri, event_time_idx, 
            event_subj_idx)
        score_i <- parts$evt_full - final$rs * (XV_subj * Lambda_C - parts$A_C)
        vcov_gamma <- inv_info %*% crossprod(score_i) %*% t(inv_info)
    }
    else {
        info <- matrix(0, 0, 0)
        score_i <- matrix(0, n_subj, 0L)
        vcov_gamma <- matrix(0, 0, 0)
    }
    list(gamma = gamma, vcov_gamma = vcov_gamma, dLambda = dLambda, unique_times = unique_times, event_time_idx = event_time_idx, 
        event_subj_idx = event_subj_idx, visit_rows = visit_rows, mi = mi, Lambda_C = Lambda_C, nu = nu, 
        sigma2_zeta = sigma2_zeta_mom, sigma2_zeta_mom = sigma2_zeta_mom, Ci = Ci_original, Ci_grid = Ci_disc, 
        Ei = Ei_original, first_risk_idx = first_risk_idx, fri = fri, 
        last_risk_idx = last_risk_idx, valid = valid, lri = lri, risk_scalar = risk_scalar, risk_matrix = risk_matrix, 
        S0 = final$S0, Xbar = final$Xbar, rs = final$rs, information_gamma = info, score_gamma_subject = score_i, 
        converged = converged, used_fallback = used_fallback, max_abs_score = if (p_V) max(abs(final$score)) else 0, 
        score_tolerance = score_limit)
}

`.cimehr_frailty_validate` <- function(m, nu) {
    m <- as.numeric(m)
    nu <- as.numeric(nu)
    if (!length(m) || length(nu) != length(m)) {
        stop("`m` and `nu` must be nonempty vectors of equal length.", call. = FALSE)
    }
    if (any(!is.finite(m)) || any(m < 0) || any(abs(m - round(m)) > 1e-08)) {
        stop("`m` must contain finite nonnegative counts.", call. = FALSE)
    }
    if (any(!is.finite(nu)) || any(nu < 0)) {
        stop("`nu` must contain finite nonnegative exposures.", call. = FALSE)
    }
    list(m = m, nu = nu)
}

`.cimehr_frailty_mode_curvature` <- function(m, nu, sigma2_zeta, tol = 2e-14, maxit = 80L) {
    dat <- .cimehr_frailty_validate(m, nu)
    m <- dat$m
    nu <- dat$nu
    a <- as.numeric(sigma2_zeta)
    if (length(a) != 1L || !is.finite(a) || a < 0) {
        stop("`sigma2_zeta` must be one finite nonnegative number.", call. = FALSE)
    }
    n <- length(m)
    if (a == 0) {
        return(list(mode_u = numeric(n), mode_x = numeric(n), log_omega = rep(-Inf, n), omega = numeric(n), 
            curvature = rep(1, n), scale2 = rep(1, n), relative_score_residual = numeric(n), sigma2_zeta = 0))
    }
    sqrt_a <- sqrt(a)
    mode_u <- mode_x <- numeric(n)
    log_omega <- rep(-Inf, n)
    omega <- numeric(n)
    positive <- nu > 0
    if (any(positive)) {
        log_A <- log(a) + log(nu[positive])
        B <- a * (m[positive] - 0.5)
        z <- log_A + B
        if (any(!is.finite(z))) {
            stop("The exact frailty-mode equation overflowed; rescale counts/exposures.", call. = FALSE)
        }
        log_w <- .cimehr_log_wright_omega(z, tol = tol, maxit = maxit)
        w <- exp(log_w)
        x <- log_w - log_A
        u <- (x + a/2)/sqrt_a
        log_omega[positive] <- log_w
        omega[positive] <- w
        mode_x[positive] <- x
        mode_u[positive] <- u
    }
    if (any(!positive)) {
        mode_u[!positive] <- sqrt_a * m[!positive]
        mode_x[!positive] <- a * (m[!positive] - 0.5)
    }
    curvature <- 1 + omega
    scale2 <- 1/curvature
    if (any(!is.finite(mode_u)) || any(!is.finite(mode_x)) || any(!is.finite(curvature)) || any(scale2 <= 
        0)) {
        stop("The exact frailty mode or curvature is outside floating-point range.", call. = FALSE)
    }
    raw_residual <- -mode_u + sqrt_a * m - omega/sqrt_a
    residual_scale <- 1 + abs(mode_u) + abs(sqrt_a * m) + abs(omega/sqrt_a)
    list(mode_u = mode_u, mode_x = mode_x, log_omega = log_omega, omega = omega, curvature = curvature, 
        scale2 = scale2, relative_score_residual = abs(raw_residual)/residual_scale, sigma2_zeta = a)
}

`.cimehr_stage1_case_linearization` <- function(stage1, XV) {
    XV <- as.matrix(XV)
    n <- length(stage1$mi)
    p <- ncol(XV)
    if (nrow(XV) != n) 
        stop("Stage-1 IF design has the wrong number of rows.", call. = FALSE)
    score_gamma <- stage1$score_gamma_subject
    if (is.null(score_gamma)) {
        stop("Stage-1 subject scores were not retained.", call. = FALSE)
    }
    if (p) {
        inv_info <- .cimehr_safe_solve(stage1$information_gamma, name = "Stage-1 stacked information")
        if_gamma <- score_gamma %*% t(inv_info)
    }
    else {
        inv_info <- matrix(0, 0, 0)
        if_gamma <- matrix(0, n, 0L)
    }
    G <- NULL
    apply_nu <- function(D) {
        D <- as.matrix(D)
        if (nrow(D) != n || any(!is.finite(D))) {
            stop("The nu-chain derivative must be a finite n-row matrix.", call. = FALSE)
        }
        q <- ncol(D)
        if (!q) 
            return(matrix(0, n, 0L))
        if (is.double(D)) {
            chained <- cimehr_s1_apply_nu(stage1$rs, D, stage1$valid, stage1$lri, stage1$fri, stage1$S0, 
                stage1$dLambda, stage1$event_time_idx, stage1$event_subj_idx)
        }
        else {
            K <- length(stage1$unique_times)
            W <- stage1$risk_matrix(stage1$rs, D)
            W_over_S0 <- W/stage1$S0
            event_part <- matrix(0, n, q)
            z <- rowsum(W_over_S0[stage1$event_time_idx, , drop = FALSE], stage1$event_subj_idx, reorder = FALSE)
            event_part[as.integer(rownames(z)), ] <- z
            prefix <- apply(W_over_S0 * stage1$dLambda, 2L, cumsum)
            if (!is.matrix(prefix)) 
                prefix <- matrix(prefix, nrow = K, ncol = q)
            risk_part <- matrix(0, n, q)
            if (length(stage1$valid)) {
                window <- prefix[stage1$lri, , drop = FALSE] - rbind(0, prefix)[stage1$fri, , drop = FALSE]
                risk_part[stage1$valid, ] <- stage1$rs[stage1$valid] * window
            }
            chained <- event_part - risk_part
        }
        if (p) {
            if (is.null(G)) {
                cum_Xbar <- apply(stage1$Xbar * stage1$dLambda, 2L, cumsum)
                if (!is.matrix(cum_Xbar)) 
                    cum_Xbar <- matrix(cum_Xbar, ncol = p)
                A_C <- matrix(0, n, p)
                if (length(stage1$valid)) {
                    A_C[stage1$valid, ] <- cum_Xbar[stage1$lri, , drop = FALSE] - rbind(0, cum_Xbar)[stage1$fri, , drop = FALSE]
                }
                G <<- stage1$rs * (XV * stage1$Lambda_C - A_C)
            }
            C <- crossprod(D, G)
            gamma_part <- if_gamma %*% t(C)
        }
        else {
            gamma_part <- matrix(0, n, q)
        }
        ans <- chained + gamma_part
        colnames(ans) <- colnames(D)
        ans
    }
    list(score_gamma = score_gamma, inverse_information_gamma = inv_info, influence_gamma = if_gamma, 
        vcov_gamma = crossprod(if_gamma), apply_nu = apply_nu)
}

`.cimehr_stage3_case_linearization` <- function(stage3, stage1, XV, XY, ZY, use_time, functional_derivatives) {
    XV <- as.matrix(XV)
    XY <- as.matrix(XY)
    ZY <- as.matrix(ZY)
    n <- nrow(XY)
    p_beta <- ncol(XY)
    p_psi <- length(stage3$coef)
    p_eta <- ncol(functional_derivatives$omega_eta)
    include_theta <- p_psi > p_beta
    stable_arithmetic <- !is.null(stage3$solve_state)
    A_weight <- -stage3$risk_integral
    A_kappa <- matrix(0, n, p_psi, dimnames = list(NULL, names(stage3$coef)))
    if (include_theta) {
        eo_subject <- stage3$eo_subject
        n_eo <- length(eo_subject)
        G <- matrix(0, n_eo, p_psi)
        theta_cols <- p_beta + seq_len(ncol(stage3$random_rows))
        G[, theta_cols] <- stage3$random_rows
        psi_G <- drop(G %*% stage3$coef)
        obs <- G * stage3$residual - stage3$H * psi_G
        obs_subject <- matrix(0, n, p_psi)
        z <- rowsum(obs, eo_subject, reorder = FALSE)
        obs_subject[as.integer(rownames(z)), ] <- z
        G_integral <- matrix(0, n, p_psi)
        G_integral[, theta_cols] <- stage3$random_integral
        A_kappa <- if (stable_arithmetic)
            cimehr_stage3_kappa_score_cpp(G, stage3$H, stage3$residual, stage3$coef,
                as.integer(eo_subject), stage3$risk_weight, G_integral)
        else obs_subject - stage3$risk_weight * G_integral
    }
    rs <- stage1$rs
    scale <- numeric(n)
    active <- stage1$mi > 0 & stage1$nu > 0
    scale[active] <- stage1$mi[active] * rs[active]/stage1$nu[active]
    w_nu <- numeric(n)
    w_nu[active] <- scale[active] * functional_derivatives$omega_nu[active] - stage3$risk_weight[active]/stage1$nu[active]
    w_tau <- scale * functional_derivatives$omega_tau
    w_eta <- scale * functional_derivatives$omega_eta
    D_nu <- A_kappa * functional_derivatives$kappa_nu + A_weight * w_nu
    D_tau_subject <- A_kappa * functional_derivatives$kappa_tau + A_weight * w_tau
    D_tau <- colSums(D_tau_subject)
    D_eta <- t(crossprod(functional_derivatives$kappa_eta, A_kappa) + crossprod(w_eta, A_weight))
    if (stable_arithmetic) {
        D_nu <- cimehr_stable_scale_add_cpp(A_kappa, functional_derivatives$kappa_nu, A_weight, w_nu)
        D_tau_subject <- cimehr_stable_scale_add_cpp(A_kappa, functional_derivatives$kappa_tau, A_weight, w_tau)
        D_tau <- cimehr_stable_matvec_cpp(t(D_tau_subject), rep(1, n))
        D_eta <- t(cimehr_stable_matrix_sum_cpp(list(
            cimehr_stable_right_product_cpp(t(functional_derivatives$kappa_eta), A_kappa),
            cimehr_stable_right_product_cpp(t(w_eta), A_weight))))
    }
    if (ncol(XV)) {
        D_gamma <- if (stable_arithmetic)
            cimehr_stable_right_product_cpp(t(A_weight * stage3$risk_weight), XV)
        else crossprod(A_weight * stage3$risk_weight, XV)
    }
    else {
        D_gamma <- matrix(0, p_psi, 0L)
    }
    list(direct_score = stage3$subject_score, A_weight = A_weight, A_kappa = A_kappa, derivative_nu = D_nu, 
        derivative_tau_subject = D_tau_subject, derivative_tau = D_tau, derivative_eta = D_eta, derivative_gamma = D_gamma)
}

`.cimehr_safe_solve` <- function(A, b = NULL, name = "matrix", tol = 1e-10) {
    q <- qr(A, tol = tol)
    if (q$rank < min(dim(A))) {
        stop(sprintf("%s is rank deficient (rank %d of %d). This is an identification/design failure, not an optimizer problem.", 
            name, q$rank, min(dim(A))), call. = FALSE)
    }
    if (is.null(b)) 
        solve(A)
    else solve(A, b)
}

`.cimehr_log_wright_omega` <- function(z, tol = 2e-14, maxit = 80L, diagnostics = FALSE) {
    z <- as.numeric(z)
    if (!length(z) || any(!is.finite(z))) {
        stop("`z` must contain finite numbers.", call. = FALSE)
    }
    if (length(tol) != 1L || !is.finite(tol) || tol <= 0) {
        stop("`tol` must be one positive finite number.", call. = FALSE)
    }
    maxit <- as.integer(maxit)
    if (length(maxit) != 1L || is.na(maxit) || maxit < 1L) {
        stop("`maxit` must be one positive integer.", call. = FALSE)
    }
    r <- numeric(length(z))
    large <- z > 1
    r[large] <- log(z[large])
    r[!large] <- z[!large] - exp(z[!large])
    converged <- rep(FALSE, length(z))
    iterations <- integer(length(z))
    for (iter in seq_len(maxit)) {
        active <- which(!converged)
        if (!length(active)) 
            break
        er <- exp(r[active])
        residual <- r[active] + er - z[active]
        step <- residual/(1 + er)
        candidate <- r[active] - step
        if (any(!is.finite(candidate))) {
            stop("The log-Wright-omega Newton iteration became non-finite.", call. = FALSE)
        }
        r[active] <- candidate
        new_residual <- candidate + exp(candidate) - z[active]
        relative_residual <- abs(new_residual)/pmax(1, abs(z[active]))
        done <- abs(step) <= tol * (1 + abs(candidate)) | relative_residual <= tol
        if (any(done)) {
            converged[active[done]] <- TRUE
            iterations[active[done]] <- iter
        }
    }
    iterations[!converged] <- maxit
    final_residual <- r + exp(r) - z
    relative_residual <- abs(final_residual)/pmax(1, abs(z))
    converged <- converged | relative_residual <= 10 * tol
    if (any(!converged)) {
        stop(sprintf("Log-Wright-omega failed to converge for %d value(s); maximum relative residual %.3g.", 
            sum(!converged), max(relative_residual[!converged])), call. = FALSE)
    }
    if (!isTRUE(diagnostics)) 
        return(r)
    list(log_omega = r, omega = exp(r), iterations = iterations, converged = converged, relative_residual = relative_residual)
}

}, envir = .cimehr_common)
