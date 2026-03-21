
require(splines2)
require("BB")

# ===========================
# ====== Back Fitting =======
# ===========================

# 估计sigma
series_cal <- function(y, init, sigma_1) {
        N <- length(y)
        sigma <- vector(length = N)
        sigma[1] <- sigma_1
        for (i in 2:N) {
                sigma[i] <- init[1] + init[2] * y[i - 1]^2 + init[3] * sigma[i - 1]
        }
        return(sigma)
}

# 计算样条矩阵（不需要，使用了spline包）
spline_matrix <- function(sigma, knots, n_partitions) {
        m <- cbind(sigma, sigma^2)
        for (i in 1:n_partitions) {
                k <- sigma - knots[i]
                k[which(k < 0)] <- 0
                m <- cbind(m, k^2)
        }
        return(m)
}

# 计算QML
M <- function(init_est, y, epsilon, sigma_1) {
        sigma <- series_cal(y, init_est, sigma_1)

        k1 <- -1 / 2 * sum(log(sigma))
        k2 <- -1 / 2 * sum(epsilon^2 / sigma)
        return(-(k1 + k2))
}

# Backfitting 实现
bf <- function(y, init = rep(1, 3),
               sigma_1 = var(y), tol = 1e-5, maxiter = 20, lower = 1e-3, upper = 1,
               judge_k = F) {
        key <- init
        t1 <- Sys.time()
        iter <- 0
        step <- 1
        N <- length(y)
        n_partitions <- floor(N^(3 / 20))

        judge <- TRUE # 控制循环的变量
        judge_covergence <- TRUE # ！！！判断是否收敛的变量

        while (judge) {
                sigma <- series_cal(y, init, sigma_1)

                # if(any(sigma <= 0)) warning("sigma <= 0")

                k <- range(sigma)
                knots <- seq(k[1], k[2], length.out = n_partitions + 2)
                knots <- knots[c(-1, -length(knots))]
                # sigma_m = spline_matrix(sigma, knots = knots, n_partitions)
                sigma_m <- bSpline(sigma, knots = knots, degree = 2)
                eta_out <- lm(y ~ sigma_m)
                eta <- predict(eta_out)

                epsilon <- y - eta

                # sigma_try = series_cal(eta, init, sigma_1)

                # if(step <= 1){
                #   lm_out = lm(sigma[2:N]~y[1:(N-1)]^2 + sigma[1:(N-1)])
                #   init = lm_out$coefficients
                # }

                init_out <- BBoptim(init, M,
                        y = y, sigma_1 = sigma_1, epsilon = epsilon, lower = lower,
                        upper = upper,
                        control = list(maxit = 1500, gtol = tol, ftol = tol^2)
                )

                if (init_out$convergence > 0) judge_covergence <- FALSE
                if (judge_k & (init_out$iter > 500)) {
                        judge_covergence <- FALSE
                }


                if (max(abs(init_out$par - init)) < tol) judge <- FALSE

                cat(step, init - init_out$par, init_out$convergence, "\n")
                # if(any(init_out$x <= 0)) {
                #   k = init_out$x
                #   k[which(k <= 0)] = init[which(k <= 0)]
                #   init = k
                # }else{
                #   init = init_out$x
                # }

                # if(init)
                init <- init_out$par
                iter <- iter + init_out$iter
                step <- step + 1
                if (step > maxiter) judge <- FALSE
        }
        if (step > maxiter) judge_covergence <- FALSE
        sigma <- series_cal(y, init, sigma_1)
        run.time <- Sys.time() - t1
        result <- list()
        result$beta <- init
        result$eta <- eta
        result$sigma <- sigma
        result$run.time <- run.time
        result$step <- step
        result$iter <- iter
        result$judge_covergence <- judge_covergence

        return(result)
}

# =================================
# =========== SP-MBP ==============
# =================================

# 估计sigma（和backfitting相同，可以删掉）
series_cal <- function(y, init, sigma_1) {
        N <- length(y)
        sigma <- vector(length = N)
        sigma[1] <- sigma_1
        for (i in 2:N) {
                sigma[i] <- init[1] + init[2] * y[i - 1]^2 + init[3] * sigma[i - 1]
        }
        return(sigma)
}

# 计算样条矩阵（使用了包可以删掉）
spline_matrix <- function(sigma, knots, n_partitions) {
        m <- cbind(sigma, sigma^2)
        for (i in 1:n_partitions) {
                k <- sigma - knots[i]
                k[which(k < 0)] <- 0
                m <- cbind(m, k^2)
        }
        return(m)
}


# 计算QML
M_sp <- function(init_est, y, epsilon, sigma_1, Psi2, n_partitions) {
        sigma <- series_cal(y, init_est, sigma_1)
        k1 <- -1 / 2 * sum(log(sigma))
        k2 <- -1 / 2 * sum(epsilon^2 / sigma)

        k <- range(sigma)
        knots <- seq(k[1], k[2], length.out = n_partitions + 2)
        knots <- knots[c(-1, -length(knots))]
        sigma_m <- bSpline(sigma, knots = knots, degree = 2)

        eta_out <- lm(y ~ sigma_m)
        eta <- predict(eta_out)

        k3 <- Psi2 %*% init_est

        return(-(k1 + k2 + k3))
}

# 计算Psi_2
Psi_2_B <- function(y, init, sigma, epsilon, knots) {
        init_dsigma <- rep(0, 3)
        dsigma <- matrix(0, nrow = length(y), ncol = 3)
        dsigma[1, ] <- init_dsigma

        for (i in 2:length(sigma)) {
                dsigma[i, ] <- c(1, y[i - 1]^2, sigma[i - 1]) + init[3] * dsigma[(i - 1), ]
        }

        sigma_d <- dbs(sigma, knots = knots, degree = 2)
        eta_d <- lm(y ~ sigma_d)
        eta_d <- predict(eta_d)
        eta_d <- eta_d * dsigma

        output <- apply(epsilon / sigma * eta_d, 2, sum)

        return(output)
}

# SPMBP实现
spmbp_B <- function(y, init = rep(1, 3),
                    sigma_1 = var(y), tol = 1e-5, maxiter = 20, lower = 1e-3, upper = 1,
                    judge_k = F) {
        key <- init
        t1 <- Sys.time()
        iter <- 0
        step <- 1
        N <- length(y)
        n_partitions <- floor(N^(3 / 20))

        judge <- TRUE
        judge_covergence <- TRUE

        while (judge) {
                sigma <- series_cal(y, init, sigma_1)

                # if(any(sigma <= 0)) warning("sigma <= 0")

                k <- range(sigma)
                knots <- seq(k[1], k[2], length.out = n_partitions + 2)
                knots <- knots[c(-1, -length(knots))]
                sigma_m <- bSpline(sigma, knots = knots, degree = 2)

                eta_out <- lm(y ~ sigma_m)
                eta <- predict(eta_out)

                epsilon <- y - eta

                # sigma_try = series_cal(eta, init, sigma_1)
                Psi2 <- Psi_2_B(y, init, sigma, epsilon, knots)
                # if(step <= 1){
                #   lm_out = lm(sigma[2:N]~y[1:(N-1)]^2 + sigma[1:(N-1)])
                #   init = lm_out$coefficients
                # }


                init_out <- BBoptim(init, M_sp,
                        y = y, sigma_1 = sigma_1, epsilon = epsilon,
                        n_partitions = n_partitions,
                        Psi2 = Psi2, lower = 1e-3, upper = upper,
                        control = list(maxit = 1500, gtol = tol, ftol = tol^2)
                )

                if (init_out$convergence > 0) judge_covergence <- FALSE
                if (judge_k & (init_out$iter > 500)) {
                        judge_covergence <- FALSE
                }

                if (max(abs(init_out$par - init)) < tol) judge <- FALSE

                cat(step, init - init_out$par, init_out$convergence, "\n")
                init <- init_out$par

                step <- step + 1
                iter <- iter + init_out$iter
                if (step > maxiter) judge <- FALSE
        }
        if (step > maxiter) judge_covergence <- FALSE

        sigma <- series_cal(y, init, sigma_1)
        run.time <- Sys.time() - t1
        result <- list()
        result$beta <- init
        result$eta <- eta
        result$sigma <- sigma
        result$run.time <- run.time
        result$step <- step
        result$iter <- iter
        result$judge_covergence <- judge_covergence

        return(result)
}


# ===========================
# ====== IP-GARCH ===========
# ===========================

# 计算QML
M_ip_BB <- function(init_est, y, sigma_1, n_partitions) {
        sigma <- series_cal(y, init_est, sigma_1)
        k <- range(sigma)
        knots <- seq(k[1], k[2], length.out = n_partitions + 2)
        knots <- knots[c(-1, -length(knots))]
        sigma_m <- bSpline(sigma, knots = knots, degree = 2)

        eta_out <- lm(y ~ sigma_m)
        eta <- predict(eta_out)

        epsilon <- y - eta

        k1 <- -1 / 2 * sum(log(sigma))
        k2 <- -1 / 2 * sum(epsilon^2 / sigma)

        return(-(k1 + k2))
}

IP_GARCH_BB <- function(y, init = rep(1, 3),
                        sigma_1 = var(y), tol = 1e-5, maxiter = 20, lower = 1e-3, upper = 1,
                        judge_k = F) {
        key <- init
        t1 <- Sys.time()
        iter <- 0
        step <- 1
        N <- length(y)
        n_partitions <- floor(N^(3 / 20))

        judge <- TRUE
        judge_covergence <- TRUE

        # while(judge){
        init_out <- BBoptim(init, M_ip_BB,
                y = y, sigma_1 = sigma_1,
                n_partitions = n_partitions, lower = 1e-3, upper = upper,
                control = list(
                        maxit = 1500, ftol = tol^2,
                        gtol = tol
                )
        )



        if (init_out$convergence > 0) judge_covergence <- FALSE
        if (judge_k & init_out$iter > 500) {
                judge_covergence <- FALSE
        }

        if (sum((init_out$par - init)^2) < tol) judge <- FALSE

        cat(step, init - init_out$par, init_out$convergence, "\n")
        init <- init_out$par

        step <- step + 1
        iter <- iter + init_out$iter
        if (step > maxiter) judge <- FALSE
        # }

        if (step > maxiter) judge_covergence <- FALSE

        sigma <- series_cal(y, init, sigma_1)
        run.time <- Sys.time() - t1
        result <- list()
        result$beta <- init
        # result$eta = eta
        result$sigma <- sigma
        result$run.time <- run.time
        result$step <- step
        result$iter <- iter
        result$judge_covergence <- judge_covergence

        return(result)
}



IP_GARCH_BB <- function(intermediates, data, theta) {
        tol <- 1e-5
        y <- data
        init <- theta
        sigma_1 <- var(y)
        upper <- 1
        intermediates$theta_1 <- init
        # 估计sigma
        series_cal <- function(y, init, sigma_1) {
                N <- length(y)
                sigma <- vector(length = N)
                sigma[1] <- sigma_1
                for (i in 2:N) {
                        sigma[i] <- init[1] + init[2] * y[i - 1]^2 + init[3] * sigma[i - 1]
                }
                return(sigma)
        }

        # 计算样条矩阵
        spline_matrix <- function(sigma, knots, n_partitions) {
                m <- cbind(sigma, sigma^2)
                for (i in 1:n_partitions) {
                        k <- sigma - knots[i]
                        k[which(k < 0)] <- 0
                        m <- cbind(m, k^2)
                }
                return(m)
        }

        # 计算Psi
        M <- function(init_est, y, epsilon, sigma_1) {
                sigma <- series_cal(y, init_est, sigma_1)
                k1 <- -1 / 2 * sum(log(sigma))
                k2 <- -1 / 2 * sum(epsilon^2 / sigma)
                return(-(k1 + k2))
        }

        M_ip_BB <- function(init_est, y, sigma_1, n_partitions) {
                sigma <- series_cal(y, init_est, sigma_1)
                k <- range(sigma)
                knots <- seq(k[1], k[2], length.out = n_partitions + 2)
                knots <- knots[c(-1, -length(knots))]
                sigma_m <- splines2::bSpline(sigma, knots = knots, degree = 2)

                eta_out <- lm(y ~ sigma_m)
                eta <- predict(eta_out)

                epsilon <- y - eta

                k1 <- -1 / 2 * sum(log(sigma))
                k2 <- -1 / 2 * sum(epsilon^2 / sigma)

                return(-(k1 + k2))
        }
        N <- length(y)
        n_partitions <- floor(N^(3 / 20))
        print("---init----")
        print(init)

        init_out <- BB::BBoptim(init, M_ip_BB,
                y = y, sigma_1 = sigma_1,
                n_partitions = n_partitions, lower = 1e-3, upper = upper,
                control = list(
                        maxit = 1500, ftol = tol^2,
                        gtol = tol
                )
        )

        intermediates$theta <- init_out$par

        sigma <- series_cal(y, init, sigma_1)
        intermediates$sigma_delta <- sigma - intermediates$sigma

        ## run.time <- Sys.time() - t1
        ## result <- list()
        ## result$beta <- init
        ## # result$eta = eta
        ## result$sigma <- sigma
        ## result$run.time <- run.time
        ## result$step <- step
        ## result$judge_covergence <- judge_covergence
        intermediates$iter <- init_out$iter
        intermediates
}

get_result_from_raw <- function(raw_fit) {
        result <- list()
        result$beta <- raw_fit$theta
        result$sigma <- raw_fit$parameters$lambda
        result$run.time <- raw_fit$run.time
        result$iter <- ip_raw$iterspace$jac_like$intermediates$iter
        result$judge_covergence <- result$iter < 500
        result
}

theta_delta <- function(intermediates) {
        intermediates$theta_delta <- intermediates$theta - intermediates$theta_1
        intermediates
}

lambda_delta <- function(intermediates) {
        intermediates$lambda_delta <- intermediates$sigma_delta
        intermediates
}


# A数据生成
series_gen <- function(N, y1, init, sigma1) {
        y <- vector(length = N)
        sigma <- vector(length = N)
        y[1] <- y1
        sigma[1] <- sigma1

        for (i in 2:N) {
                sigma[i] <- init[1] + init[2] * y[i - 1]^2 + init[3] * sigma[i - 1]
                y[i] <- sigma[i] + 0.5 * sin(10 * sigma[i]) + sqrt(sigma[i]) * rnorm(1)
        }

        return(y)
}

# B数据生成
series_gen2 <- function(N, y1, init, sigma1) {
        y <- vector(length = N)
        sigma <- vector(length = N)
        y[1] <- y1
        sigma[1] <- sigma1

        for (i in 2:N) {
                sigma[i] <- init[1] + init[2] * y[i - 1]^2 + init[3] * sigma[i - 1]
                y[i] <- 0.5 * sigma[i] + 0.1 * sin(0.5 + 20 * sigma[i]) + sqrt(sigma[i]) * rnorm(1)
        }

        return(y)
}

Phi_fn <- function(theta, lambda) NULL

Psi_fn <- function(theta, lambda) NULL
data <- series_gen(100, 1, c(0, 0, 0), 1)
theta0 <- c(0.1, 0.1, 0.1)
lambda0 <- rep(0, 100)
res <- semislv(theta = theta0, lambda = lambda0, Phi_fn = Phi_fn, Psi_fn = Psi_fn, method = "implicit", diy = TRUE, data = data, IP_GARCH_BB = IP_GARCH_BB, theta_delta = theta_delta, lambda_delta = lambda_delta, control = list(max_iter = 2, tol = 1e-5))


N <- 500
init <- c(0.01, 0.1, 0.68)
sigma_1 <- 0.1

result1 <- matrix(nrow = 3, ncol = 1000)
time1 <- vector(length = 1000)
iter1 <- vector(length = 1000)
judge_test <- NULL
result2 <- matrix(nrow = 3, ncol = 1000)
time2 <- vector(length = 1000)
iter2 <- vector(length = N)
result3 <- matrix(nrow = 3, ncol = 1000)
time3 <- vector(length = 1000)
iter3 <- vector(length = 1000)
i <- 1
while (i <= 1000) {
        judge <- TRUE
        while (judge) {
                y <- series_gen(N, 0, init, 0.1)
                if (all(!is.na(y))) judge <- FALSE
        }
        bf.fit <- bf(y, init = init, sigma_1 = 0.1, judge_k = T)
        spmbp.fit <- spmbp_B(y, init = init, sigma_1 = 0.1, judge_k = T)
        ##   ip.fit = IP_GARCH_BB(y, init = init, sigma_1 = 0.1, judge_k = T)
        theta0 <- c(0, 0, 0)
        lambda0 <- rep(0, N)
        ip_raw <- try(semislv(theta = init, lambda = lambda0, Phi_fn = Phi_fn, Psi_fn = Psi_fn, method = "implicit", diy = TRUE, data = y, IP_GARCH_BB = IP_GARCH_BB, theta_delta = theta_delta, lambda_delta = lambda_delta, control = list(max_iter = 1, tol = 1e-5)))
        if (class(ip_raw) != "try-error" & bf.fit$judge_covergence & spmbp.fit$judge_covergence) {
                ip.fit <- get_result_from_raw(ip_raw)
                result1[, i] <- bf.fit$beta
                time1[i] <- bf.fit$run.time
                iter1[i] <- bf.fit$iter
                result2[, i] <- spmbp.fit$beta
                time2[i] <- spmbp.fit$run.time
                iter2[i] <- spmbp.fit$iter
                result3[, i] <- ip.fit$beta
                time3[i] <- ip.fit$run.time
                iter3[i] <- ip.fit$iter
                i <- i + 1
                cat("Time", i, "\n")
        }
}
result1 <- result1[, which(!is.na(result1[1, ]))]
result2 <- result2[, which(!is.na(result2[1, ]))]
result3 <- result3[, which(!is.na(result3[1, ]))]

tb_it <- matrix(nrow = 4, ncol = 3)
tb_it[1, ] <- apply(result1 - init, 1, mean)
tb_it[2, ] <- apply(result1, 1, sd)
tb_it[3, ] <- apply(abs(result1 - init), 1, mean)
tb_it[4, ] <- sqrt(apply((result1 - init)^2, 1, mean))

rownames(tb_it) <- c("BIAS", "SD", "MAE", "RMSE")
print(tb_it)

tb_spmbp <- matrix(nrow = 4, ncol = 3)
tb_spmbp[1, ] <- apply(result2 - init, 1, mean)
tb_spmbp[2, ] <- apply(result2, 1, sd)
tb_spmbp[3, ] <- apply(abs(result2 - init), 1, mean)
tb_spmbp[4, ] <- sqrt(apply((result2 - init)^2, 1, mean))

rownames(tb_spmbp) <- c("BIAS", "SD", "MAE", "RMSE")
print(tb_spmbp)

tb_ip <- matrix(nrow = 4, ncol = 3)
tb_ip[1, ] <- apply(result3 - init, 1, mean)
tb_ip[2, ] <- apply(result3, 1, sd)
tb_ip[3, ] <- apply(abs(result3 - init), 1, mean)
tb_ip[4, ] <- sqrt(apply((result3 - init)^2, 1, mean))

rownames(tb_ip) <- c("BIAS", "SD", "MAE", "RMSE")
print(tb_ip)

print(mean(time1))
print(mean(time2))
print(mean(time3)) # 按顺序为it, spmbp与ip
print(mean(iter1))
print(mean(iter2))
print(mean(iter3))