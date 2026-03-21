library(MASS)
library(nleqslv)

# 数据生成
sim.gen.STM <- function(n, p, beta0, sigmaZ, Nperturb = 0) {
        Z <- mvrnorm(n, rep(0, p), sigmaZ^2 * (0.2 + 0.8 * diag(1, p)))
        u <- runif(n)
        T <- exp((log(u / (1 - u)) - Z %*% beta0) / 3) * 4 # h^-1 (g^-1(u) - beta'Z)
        C <- runif(n, 0, 12)
        delta <- (T <= C)

        return(list(
                delta = delta, C = C,
                Z = Z
        ))
}

# Style 1
pi <- function(x) 1 / (1 + exp(-x))

Psi_fn <- function(theta, lambda, delta, Z, KC, N, p) {
        line <- Z %*% beta
        reg <- outer(lambda, line, "+")[, , 1]
        reg_pi <- pi(reg)
        dif <- as.vector(delta) - t(reg_pi)
        apply(Z * diag(dif), 2, sum)
}

Phi_fn <- function(theta, lambda, delta, Z, KC, N, p) {
        line <- Z %*% beta
        reg <- outer(lambda, line, "+")[, , 1]
        reg_pi <- pi(reg)
        dif <- as.vector(delta) - t(reg_pi)
        apply(KC * dif, 2, sum)
}

dat <- sim.gen.STM(N, p, beta0, sigmaZ)
h <- sd(dat$C) / (sum(dat$delta))^0.25
KC <- dnorm(as.matrix(dist(dat$C / h, diag = T, upper = T))) / h
KCd <- drop(KC %*% dat$delta)
theta0 <- rep(0, ncol(dat$Z))
n <- nrow(dat$Z)
lambda0 <- rep(0, n)
intermediates <- list(
        hC = lambda0,
        beta = theta0,
)
hC <- function(intermediates, lambda) {
        intermediates$hC <- lambda
        intermediates
}
beta <- function(intermediates, theta) {
        intermediates$beta <- theta
        intermediates
}

Psi_der_lambda <- function(intermediates, Z, KC) {
        expit <- function(d) {
                return(1 / (1 + exp(-d)))
        }
        intermediates$lp <- drop(Z %*% intermediates$beta)
        intermediates$gij <- expit(outer(intermediates$hC, intermediates$lp, "+"))
        intermediates$tmp <- KC * intermediates$gij
        intermediates$wZbar <- intermediates$tmp * (1 - intermediates$gij)
        intermediates$hHess <- apply(intermediates$wZbar, 1, sum)
        intermediates$Psi_der_lambda <- intermediates$hHess # is that right
        return(intermediates)
}
Psi_der_theta <- function(intermediates, Z) {
        intermediates$Zbar_up <- intermediates$wZbar %*% Z
        intermediates$Zbar <- intermediates$Zbar_up / intermediates$hHess
        intermediates$Psi_der_theta <- intermediates$Zbar # is that right we need to
        return(intermediates)
}
Phi <- function(intermediates, Z, delta) {
        intermediates$gi <- expit(intermediates$hC + intermediates$lp)
        intermediates$bscore <- drop(t(Z) %*% (delta - intermediates$gi))
        ## print(intermediates$gi)
        intermediates$Phi <- intermediates$bscore
        return(intermediates)
}
Phi_der_theta <- function(intermediates, Z) {
        intermediates$part1 <- t(intermediates$gi * (1 - intermediates$gi) * Z)
        intermediates$Phi_der_theta <- intermediates$part1
        intermediates
}
Phi_der_lambda <- function(intermediates, Z) {
        intermediates$part2 <- intermediates$Zbar - Z
        intermediates$Phi_der_lambda <- intermediates$part2
        intermediates
}
theta_Hess <- function(intermediates, Z) {
        intermediates$theta_Hess <- intermediates$part1 %*% intermediates$part2
        return(intermediates)
}
theta_delta <- function(intermediates) {
        intermediates$theta_delta <- -1 * solve(intermediates$theta_Hess, intermediates$bscore)
        intermediates$beta <- intermediates$beta + intermediates$theta_delta
        intermediates
}
Psi <- function(intermediates, Z, KC, KCd) {
        expit <- function(d) {
                return(1 / (1 + exp(-d)))
        }
        intermediates$lp <- drop(Z %*% intermediates$beta)
        intermediates$gij <- expit(outer(intermediates$hC, intermediates$lp, "+"))
        intermediates$tmp <- KC * intermediates$gij
        intermediates$hscore <- apply(intermediates$tmp, 1, sum) - KCd
        intermediates$Psi <- intermediates$hscore
        return(intermediates)
}

Psi_der_lambda2 <- function(intermediates) {
        intermediates$wZbar <- intermediates$tmp * (1 - intermediates$gij)
        intermediates$hHess <- apply(intermediates$wZbar, 1, sum)
        intermediates$Psi_der_lambda2 <- intermediates$hHess
        return(intermediates)
}

lambda_delta <- function(intermediates) {
        intermediates$lambda_delta <- unlist(-1 * intermediates$hscore / intermediates$hHess)
        intermediates
}

res <- semislv(theta = theta0, lambda = lambda0, Phi_fn = Phi_fn, Psi_fn = Psi_fn, method = "implicit", diy = TRUE, Z = dat$Z, delta = dat$delta, KC = KC, n = n, KCd = KCd, hC = hC, beta = beta, Psi_der_lambda = Psi_der_lambda, Psi_der_theta = Psi_der_theta, Phi = Phi, Phi_der_theta = Phi_der_theta, Phi_der_lambda = Phi_der_lambda, theta_Hess = theta_Hess, theta_delta = theta_delta, Psi = Psi, Psi_der_lambda2 = Psi_der_lambda2, lambda_delta = lambda_delta, intermediates = intermediates)

# Style 2 IP

run_time.ip <- function(intermediates, theta, lambda, delta, Z, KC, Zd, KCd) {
        beta <- theta
        hC <- lambda
        expit <- function(d) {
                return(1 / (1 + exp(-d)))
        }
        lp <- drop(Z %*% beta)
        # print(hC[hC.flag])
        gij <- expit(outer(hC, lp, "+"))
        tmp <- KC * gij
        wZbar <- tmp * (1 - gij)
        hscore <- apply(tmp, 1, sum) - KCd
        hHess <- apply(wZbar, 1, sum)

        dhC <- hscore / hHess
        dhC <- sign(dhC) * pmin(abs(dhC), 1)
        intermediates$dhC <- dhC
        hC <- hC - dhC
        Zbar <- (wZbar %*% Z) / hHess

        gi <- expit(hC + lp)
        bscore <- drop(t(Z) %*% (delta - gi))

        bHess <- t(gi * (1 - gi) * Z) %*% (Zbar - Z)
        dinit <- solve(bHess, bscore)

        intermediates$dinit <- dinit
        intermediates
}

theta_delta.ip <- function(intermediates) {
        intermediates$theta_delta <- -intermediates$dinit
        intermediates
}

lambda_delta.ip <- function(intermediates) {
        intermediates$lambda_delta <- -intermediates$dhC
        intermediates
}
intermediates.ip <- list(
        hC = lambda0,
        beta = theta0
)
res <- semislv(theta = theta0, lambda = lambda0, Phi_fn = Phi_fn, Psi_fn = Psi_fn, method = "implicit", diy = TRUE, Z = dat$Z, delta = dat$delta, KC = KC, n = n, KCd = KCd, Zd = Zd, run_time = run_time.ip, theta_delta = theta_delta.ip, lambda_delta = lambda_delta.ip, intermediates = intermediates.ip)


# Style 2


h <- sd(dat$C) / (sum(dat$delta))^0.25
KC <- dnorm(as.matrix(dist(dat$C / h, diag = T, upper = T))) / h
KCd <- drop(KC %*% dat$delta)
theta0 <- rep(0, ncol(dat$Z))
n <- nrow(dat$Z)
Zd <- drop(t(dat$Z) %*% dat$delta)
lambda0 <- rep(0, n)
intermediates.it <- list(
        hC = lambda0,
        beta = theta0,
        dif = NULL,
        hCd = NULL,
        lp = NULL,
        gij = NULL,
        wZbar = NULL,
        tmp = NULL,
        hscore = NULL,
        hHess = NULL,
        Zbar_up = NULL,
        Z_bar = NULL,
        gi = NULL,
        bscore = NULL,
        part1 = NULL,
        part2 = NULL,
        theta_Hess = NULL
)
run_time.it <- function(intermediates, lambda, theta, Z, KC, Zd, KCd) {
        beta <- theta
        hC <- lambda
        expit <- function(d) {
                return(1 / (1 + exp(-d)))
        }

        f_beta <- function(beta, hC, gi) {
                temp_pi <- t(Z) %*% gi
                return(Zd - temp_pi)
        }

        jacf_beta <- function(beta, hC, gi) {
                bHess <- t(gi * (1 - gi) * Z) %*% Z
                return(-bHess)
        }

        f_hC <- function(hC, beta, gij, temp) {
                return(apply(temp, 1, sum) - KCd)
        }

        jacf_hC <- function(hC, beta, gij, temp) {
                wZbar <- temp * (1 - gij)
                hscore <- apply(temp, 1, sum) - KCd
                hHess <- apply(wZbar, 1, sum)
                hHess <- diag(hHess)
                return(hHess)
        }
        intermediates$gi <- expit(hC + drop(Z %*% beta))
        intermediates$temp_beta <- nleqslv::nleqslv(beta, f_beta,
                jac = jacf_beta,
                hC = hC, gi = intermediates$gi, method = "Newton",
                global = "none", control = list(maxit = 1)
        )
        intermediates$gij <- matrix(0, nrow = n, ncol = n)
        intermediates$gij[1:n, ] <- expit(outer(hC, Z %*% intermediates$temp_beta$x, "+"))
        intermediates$temp <- KC * intermediates$gij
        intermediates$temp_hC <- nleqslv::nleqslv(hC, f_hC,
                jac = jacf_hC,
                beta = temp_beta$x, gij = intermediates$gij, temp = intermediates$temp, method = "Newton",
                global = "none", control = list(maxit = 1)
        )
        intermediates
}
theta_delta.it <- function(intermediates, theta) {
        intermediates$theta_delta <- intermediates$temp_beta$x - theta
        intermediates
}


lambda_delta.it <- function(intermediates, lambda) {
        intermediates$lambda_delta <- intermediates$temp_hC$x - lambda
        intermediates
}
res <- semislv(theta = theta0, lambda = lambda0, Phi_fn = Phi_fn, Psi_fn = Psi_fn, method = "iterative", diy = TRUE, Z = dat$Z, delta = dat$delta, KC = KC, n = n, KCd = KCd, Zd = Zd, run_time = run_time, theta_delta = theta_delta.it, lambda_delta = lambda_delta.it, intermediates = intermediates.it)

global <- function(init, f, delta, Z, KC, N, p) {
        t0 <- Sys.time()
        out <- nleqslv::nleqslv(init, f,
                delta = delta, Z = Z, KC = KC,
                N = N, p = p, method = "Newton", global = "none"
        )
        run.time <- Sys.time() - t0
        return(list(Model = out, run.time = run.time))
}

# simulation
nrep <- 100 # 100
N <- 1000
p <- 10
beta0 <- c(0.7, 0.7, 0.7, -0.5, -0.5, -0.5, 0.3, 0.3, 0.3, 0)
sigmaZ <- 1
compare <- list()
time <- matrix(nrow = nrep, ncol = 2)
step <- matrix(nrow = nrep, ncol = 2)
mse_global <- 0
mse_IP <- 0
mse_IT <- 0
for (i in 1:nrep) {
        dat <- sim.gen.STM(N, p, beta0, sigmaZ) ## don't change
        h <- sd(dat$C) / (sum(dat$delta))^0.25
        KC <- dnorm(as.matrix(dist(dat$C / h, diag = T, upper = T))) / h
        KCd <- drop(KC %*% dat$delta)
        theta0 <- rep(0, ncol(dat$Z))
        n <- nrow(dat$Z)
        Zd <- drop(t(dat$Z) %*% dat$delta)
        lambda0 <- rep(0, n)
        ## place my initial value
        ## out_global <- global(rep(0, N + p), f, dat$delta, dat$Z, KC, N, p)
        ## return beta, runtime, step
        ## out_ip <- implicit.profile(dat$delta, dat$Z, KC, h.loop = F)
        intermediates.ip <- list(hC = lambda0, beta = theta0)
        ## out_it <- iterative(dat$delta, dat$Z, KC)
        intermediates.it <- list(hC = lambda0, beta = theta0)
        # out_ip <- semislv(theta = theta0, lambda = lambda0, Phi_fn = Phi_fn, Psi_fn = Psi_fn, method = "implicit", diy = TRUE, Z = dat$Z, delta = dat$delta, KC = KC, n = n, KCd = KCd, hC = hC, beta = beta, Psi_der_lambda = Psi_der_lambda, Psi_der_theta = Psi_der_theta, Phi = Phi, Phi_der_theta = Phi_der_theta, Phi_der_lambda = Phi_der_lambda, theta_Hess = theta_Hess, theta_delta = theta_delta, Psi = Psi, Psi_der_lambda2 = Psi_der_lambda2, lambda_delta = lambda_delta, intermediates = intermediates)
        out_ip <- semislv(theta = theta0, lambda = lambda0, Phi_fn = Phi_fn, Psi_fn = Psi_fn, method = "implicit", diy = TRUE, Z = dat$Z, delta = dat$delta, KC = KC, n = n, KCd = KCd, Zd = Zd, run_time = run_time.ip, theta_delta = theta_delta.ip, lambda_delta = lambda_delta.ip, intermediates = intermediates.ip, control = list(max_iter = 100, tol = 1e-7))
        # style 1 -> style 2
        out_it <- semislv(theta = theta0, lambda = lambda0, Phi_fn = Phi_fn, Psi_fn = Psi_fn, method = "iterative", diy = TRUE, Z = dat$Z, delta = dat$delta, KC = KC, n = n, KCd = KCd, Zd = Zd, run_time = run_time.it, theta_delta = theta_delta.it, lambda_delta = lambda_delta.it, intermediates = intermediates.it, control = list(max_iter = 100, tol = 1e-7))
        # style 2
        ## mse_global <- mse_global + sum((out_global$Model$x[1:p] - beta0)^2)
        mse_IP <- mse_IP + sum((out_ip$theta - beta0)^2)
        mse_IT <- mse_IT + sum((out_it$theta - beta0)^2)
        time[i, ] <- c(out_ip$run.time, out_it$run.time) # out_global$run.time,
        step[i, ] <- c(out_ip$step, out_it$step) # out_global$Model$iter,
        compare[[i]] <- rbind(out_ip$theta, out_it$theta) # out_global$Model$x[1:p],
        ## cat(
        ##         "step", i, sum(abs(out_global$Model$x[1:p] - out_ip$theta)),
        ##         sum(abs(out_global$Model$x[1:p] - out_it$theta)), "\n"
        ## )
}
apply(time, 2, mean)
apply(step, 2, mean)
## (mse_global <- mse_global / nrep)
(mse_IP <- mse_IP / nrep)
(mse_IT <- mse_IT / nrep)


# > apply(time, 2, mean)
# [1]  0.839407 14.227783
# > apply(step, 2, mean)
# [1]  9.25 17.77
# > ## (mse_global <- mse_global / nrep)
# > (mse_IP <- mse_IP / nrep)
# [1] 0.1110877
# > (mse_IT <- mse_IT / nrep)
# [1] 0.1110877


# 计算Psi和Phi
f <- function(parameters, delta, Z, KC, N, p) {
        # parameters contain beta and h
        beta <- parameters[1:p]
        h <- parameters[-(1:p)]
        pi <- function(x) {
                return(1 / (1 + exp(-x)))
        }

        line <- Z %*% beta
        # line is a N*1 vector

        reg <- outer(h, line, "+")[, , 1]
        reg_pi <- pi(reg)
        # reg is a N*N matrix

        dif <- as.vector(delta) - t(reg_pi)
        f1 <- Z * diag(dif)
        f1 <- apply(f1, 2, sum)
        f2 <- KC * dif
        f2 <- apply(f2, 2, sum)

        return(c(f1, f2))
}




jac <- list()

# IP实现
implicit.profile <- function(delta, Z, KC, init = rep(0, ncol(Z)), tol = 1e-7,
                             maxit = 100, min.factor = 0.75,
                             ls.factor = 0.75, max.move = 1,
                             h.loop = T) {
        start <- Sys.time()
        n <- nrow(Z)
        KCd <- drop(KC %*% delta)
        hC <- rep(0, n)
        oldscore <- NULL

        #
        # f_path =NULL

        for (k in 1:maxit)
        {
                lp <- drop(Z %*% init)
                hC.flag <- rep(TRUE, n)
                gij <- wZbar <- matrix(0, n, n)
                hHess <- rep(0, n)
                for (kk in 1:ifelse(h.loop, maxit, 1))
                {
                        gij[hC.flag, ] <- expit(outer(hC[hC.flag], lp, "+"))
                        tmp <- KC[hC.flag, ] * gij[hC.flag, ]
                        wZbar[hC.flag, ] <- tmp * (1 - gij[hC.flag, ])
                        if (sum(hC.flag) >= 2) {
                                hscore <- apply(tmp, 1, sum) - KCd[hC.flag]
                                hHess[hC.flag] <- apply(wZbar[hC.flag, ], 1, sum)
                        } else {
                                hscore <- sum(tmp) - KCd[hC.flag]
                                hHess[hC.flag] <- sum(wZbar[hC.flag, ])
                        }

                        dhC <- hscore / hHess[hC.flag]
                        dhC <- sign(dhC) * pmin(abs(dhC), max.move)
                        kk.flag <- abs(hscore) > tol
                        if (!any(kk.flag)) {
                                break
                        }
                        hC[hC.flag][kk.flag] <- hC[hC.flag][kk.flag] - dhC[kk.flag]
                        hC.flag[hC.flag] <- kk.flag
                }
                if (kk >= maxit) {
                        stop("Numerical error when computing h0(Ci)")
                }
                Zbar <- (wZbar %*% Z) / hHess

                gi <- expit(hC + lp)
                bscore <- drop(t(Z) %*% (delta - gi))


                #
                # f_path = cbind(f_path, abs(bscore))

                if (!is.null(oldscore)) {
                        if (((sum(oldscore^2) * min.factor) <= sum(bscore^2))) {
                                init <- init + dinit
                                dinit <- dinit * ls.factor
                                if (max(abs(dinit)) < tol) {
                                        if (max(abs(oldscore)) > 1e-6) {
                                                warning(paste("Algorithm stops in line-search. Target tol: ",
                                                        tol, ". Current tol: ", max(abs(oldscore)),
                                                        ". ",
                                                        sep = ""
                                                ))
                                        }
                                        break
                                }
                                init <- init - dinit
                                next
                        }
                }
                oldscore <- bscore
                bHess <- t(gi * (1 - gi) * Z) %*% (Zbar - Z)
                dinit <- solve(bHess, bscore)
                if (all(abs(bscore) < tol)) {
                        break
                }
                # print(rbind(init,bscore,dinit))
                init <- init - dinit
        }
        if (k >= maxit) {
                stop("Numerical error when computing beta_delta")
        }

        run.time <- Sys.time() - start
        return(list(
                beta = init,
                run.time = run.time,
                step = k
        ))
}

# It实现
iterative <- function(delta, Z, KC, tol = 1e-7,
                      maxit = 100, h.maxit = 1) {
        start <- Sys.time()
        beta <- rep(0, ncol(Z))
        n <- nrow(Z)
        KCd <- drop(KC %*% delta)
        Zd <- drop(t(Z) %*% delta)
        hC <- rep(0, n)
        expit <- function(d) {
                return(1 / (1 + exp(-d)))
        }

        #
        # f_path = NULL

        f_beta <- function(beta, hC, gi) {
                temp_pi <- t(Z) %*% gi
                return(Zd - temp_pi)
        }

        jacf_beta <- function(beta, hC, gi) {
                bHess <- t(gi * (1 - gi) * Z) %*% Z
                return(-bHess)
        }

        f_hC <- function(hC, beta, gij, temp) {
                return(apply(temp, 1, sum) - KCd)
        }

        jacf_hC <- function(hC, beta, gij, temp) {
                wZbar <- temp * (1 - gij)
                hscore <- apply(temp, 1, sum) - KCd
                hHess <- apply(wZbar, 1, sum)
                hHess <- diag(hHess)
                return(hHess)
        }

        for (k in 1:maxit) {
                gi <- expit(hC + drop(Z %*% beta))
                temp_beta <- nleqslv(beta, f_beta,
                        jac = jacf_beta,
                        hC = hC, gi = gi, method = "Newton",
                        global = "none", control = list(maxit = 1)
                )

                gij <- matrix(0, nrow = n, ncol = n)
                gij[1:n, ] <- expit(outer(hC, Z %*% temp_beta$x, "+"))
                temp <- KC * gij
                temp_hC <- nleqslv(hC, f_hC,
                        jac = jacf_hC,
                        beta = temp_beta$x, gij = gij, temp = temp, method = "Newton",
                        global = "none", control = list(maxit = h.maxit)
                )
                # print(k)
                if (all(abs(temp_beta$fvec) < tol)) {
                        break
                }

                #
                # f_path = cbind(f_path, abs(temp_beta$fvec))

                beta <- temp_beta$x
                hC <- temp_hC$x
        }

        runtime <- Sys.time() - start
        return(list(
                beta = beta,
                run.time = runtime,
                step = k
        ))
}

# 牛顿法实现
global <- function(init, f, delta, Z, KC, N, p) {
        t0 <- Sys.time()
        out <- nleqslv(init, f,
                delta = delta, Z = Z, KC = KC,
                N = N, p = p, method = "Newton", global = "none"
        )
        run.time <- Sys.time() - t0
        return(list(Model = out, run.time = run.time))
}