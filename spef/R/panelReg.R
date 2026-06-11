## Internal functions :
##   doPanelFit.AEE
##   doPanelFit.AEEX
##   doPanelFit.HWZ
##   doPanelFit.MPL
##   doPanelFit.MPLs
##   doPanelFit.MLs
##   doPanelFit.AMM
##   doPanelFit.EE.HSWc
##   doPanelFit.EE.HSWm
##   doPanelFit.EE.SWa
##   doPanelFit.EE.SWb
##   doPanelFit.EE.SWc
##   doPanelFit.Engine.Bootstrap
##   doPanelFit.AE.Impute
##   doPanelFit.AEEX.Impute
##   doPanelFit.AEE.Sandwich
##   doPanelFit.AEEX.Sandwich
##   doPanelFit.AMM.smBoot
## Export functions :
##   panelReg

##############################################################################
## Augmented Estimating Equations (AEE)
##############################################################################
doPanelFit.AEE <- function(DF, panelMatrix, timeGrid, X, engine, stdErr) {
    N <- nrow(panelMatrix)
    K <- ncol(panelMatrix)
    eStep <- function(lambda) {
        e <- matrix(0, N, K)
        for (i in 1:N) {
            end <- which(!is.na(panelMatrix[i, ]))
            start <- c(1, head(end, -1) + 1)
            for (j in which(panelMatrix[i, end] > 0)) {
                sq <- seq(start[j], end[j])
                e[i, sq] <- panelMatrix[i, end[j]] * lambda[sq] / sum(lambda[sq])
            }
        }
        e
    }
    # ncol(X) dimensional nonlinear equation
    f <- function(beta, e) {
        lambda <- c(colSums(e)) / c(t(r) %*% exp(X %*% beta))
        c(t(X) %*% (rowSums(e) - c(exp(X %*% beta)) * c(r %*% lambda)))
    }
    sStep <- function(f, beta, e) {
        if (ncol(X) == 1) {
            beta <- uniroot(f, engine@interval, e=e)$root
        } else {
            beta <- nleqslv(beta, function(x) f(x, e))$x
        }
        lambda <- colSums(e) / c(t(r) %*% exp(X %*% beta))
        list(beta=beta,
             lambda=lambda)
    }
    e <- r <- matrix(0, N, K)
    for (i in 1:N) {
        set <- which(!is.na(panelMatrix[i, ]))
        mi <- tail(set, 1)
        dset <- diff(c(0, set))
        e[i, 1:mi] <- rep(panelMatrix[i, set] / dset, dset)
        r[i, 1:mi] <- 1
    }
    convergence <- 1
    sRes <- sStep(f, engine@betaInit, e)
    for (i in 2:engine@maxIter) {
        e <- eStep(sRes$lambda)
        betaPre <- sRes$beta
        sRes <- sStep(f, sRes$beta, e)
        s <- sRes$beta - betaPre
        if (max(abs(s)) < engine@absTol | max(abs(s / betaPre)) < engine@relTol) {
            convergence <- 0
            break
        }
    }
    iter <- i
    list(beta=sRes$beta,
         baseline=stepfun(timeGrid, cumsum(c(0, sRes$lambda))),
         timeGrid=timeGrid,
         lambda=sRes$lambda,
         convergence=convergence,
         iter=iter)
}

##############################################################################
## Extension of Augmented Estimating Equations (AEEX)
##############################################################################
doPanelFit.AEEX <- function(DF, panelMatrix, timeGrid, X, engine, stdErr) {
    N <- nrow(panelMatrix)
    K <- ncol(panelMatrix)
    eStep <- function(lambda, a) {
        e <- matrix(0, N, K)
        for (i in 1:N) {
            end <- which(!is.na(panelMatrix[i, ]))
            start <- c(1, head(end, -1) + 1)
            for (j in which(panelMatrix[i, end] > 0)) {
                sq <- seq(start[j], end[j])
                e[i, sq] <- panelMatrix[i, end[j]] * lambda[sq] / sum(lambda[sq])
            }
            if (tail(end, 1) < K) {
                sq <- seq(tail(end, 1) + 1, K)
                e[i, sq] <- (sum(panelMatrix[i, end]) + a) * lambda[sq] /
                    (sum(lambda[-sq]) + a)
            }
        }
        e
    }
    ## ncol(X) dimensional nonlinear equation
    f <- function(beta, e) {
        lambda <- c(colSums(e)) / sum(exp(X %*% beta))
        c(t(X) %*% (rowSums(e) - c(exp(X %*% beta)) * sum(lambda)))
    }
    sStep <- function(f, beta, e) {
        if (ncol(X) == 1) {
            beta <- uniroot(f, engine@interval, e=e)$root
        } else {
            beta <- nleqslv(beta, function(x) f(x, e))$x
        }
        lambda <- colSums(e) / sum(exp(X %*% beta))
        list(beta=beta,
             lambda=lambda)
    }
    ## ############################
    ## Initialize e matrix
    e <- matrix(0, N, K)
    for (i in 1:N) {
        sq <- which(!is.na(panelMatrix[i, ]))
        mi <- tail(sq, 1)
        dsq <- diff(c(0, sq))
        e[i, 1:mi] <- rep(panelMatrix[i, sq] / dsq, dsq)
        if (mi < K) {
            e[i, (mi + 1):K] <- sum(panelMatrix[i, sq]) / mi
        }
    }
    a <- engine@a
    ## Iteration
    convergence <- 1
    sRes <- sStep(f, engine@betaInit, e)
    for (i in 2:engine@maxIter) {
        e <- eStep(sRes$lambda, a)
        betaPre <- sRes$beta
        sRes <- sStep(f, sRes$beta, e)
        s <- sRes$beta - betaPre
        if (max(abs(s)) < engine@absTol | max(abs(s / betaPre)) < engine@relTol) {
            convergence <- 0
            break
        }
    }
    iter <- i
    list(beta=sRes$beta,
         baseline=stepfun(timeGrid, cumsum(c(0, sRes$lambda))),
         timeGrid=timeGrid,
         lambda=sRes$lambda,
         convergence=convergence,
         iter=iter)
}

##############################################################################
## Huang, Wang and Zhang's method, Biometrika (2006)
##############################################################################
doPanelFit.HWZ <- function(DF, panelMatrix, timeGrid, X, engine, stdErr) {
    N <- nrow(panelMatrix)
    K <- ncol(panelMatrix)
    NPEM <- function(p) {
        e <- matrix(0, N, K)
        for (i in 1:N) {
            end <- which(!is.na(panelMatrix[i, ]))
            start <- c(1, head(end, -1) + 1)
            for (j in which(panelMatrix[i, end] > 0)) {
                sq <- seq(start[j], end[j])
                e[i, sq] <- panelMatrix[i, end[j]] * p[sq] / sum(p[sq])
            }
            yi <- tail(end, 1)
            if (yi < K) {
                sq1 <- seq(1, yi)
                sq2 <- seq(yi + 1, K)
                e[i, sq2] <- sum(panelMatrix[i, end]) / sum(p[sq1]) * p[sq2]
            }
        }
        d <- colSums(e)
        p <- d / sum(d)
    }
    ## ############################
    convergence <- 1
    p <- rep(1/K, K)
    for (i in 1:engine@maxIter) {
        pPre <- p
        p <- NPEM(p)
        if (max(abs(pPre - p)) < engine@absTol) {
            convergence <- 0
            break
        }
    }
    iter <- i
    m <- rowSums(panelMatrix, na.rm=TRUE)  # N*1
    y <- apply(panelMatrix, 1, function(x) tail(which(!is.na(x)), 1))
    F <- cumsum(p)
    n <- m / F[y]
    f <- function(beta, w) {
        totalBase <- sum(w * n) / sum(w * c(exp(X %*% beta)))
        c((w * t(X)) %*% (n - totalBase * c(exp(X %*% beta))))
    }
    sStep <- function(f, beta, w) {
        if (ncol(X) == 1) {
            beta <- uniroot(f, engine@interval, w=w)$root
        } else {
            beta <- nleqslv(beta, function(x) f(x, w))$x
        }
        totalBase <- sum(w * n) / sum(w * c(exp(X %*% beta)))
        list(beta=beta,
             totalBase=totalBase)
    }
    w <- rep(1, N)
    if (engine@adjust == "W")
        w[which(F[y] < 0.005)] <- 0
    if (engine@adjust == "H")
        n[which(F[y] < 1e-4)] <- 0
    sRes <- sStep(f, engine@betaInit, w)
    if (!engine@unitWeight) {
        w <- sRes$totalBase * c(exp(X %*% sRes$beta)) /
            mean((n - sRes$totalBase * c(exp(X %*% sRes$beta)))^2)
        if (engine@adjust == "W")
            w[which(F[y] < 0.005)] <- 0
        if (engine@adjust == "H")
            n[which(F[y] < 1e-4)] <- 0
        w <- w / sum(w)
        sRes <- sStep(f, sRes$beta, w)
    }
    list(beta=sRes$beta,
         baseline=stepfun(timeGrid, c(0, F * sRes$totalBase)),
         timeGrid=timeGrid,
         convergence=convergence,
         iter=iter)
}

##############################################################################
## Zhang's pseudo likelihood method, Biometrika (2002)
##############################################################################

doPanelFit.MPL <- function(DF, panelMatrix, timeGrid, X, engine, stdErr) {
    N <- nrow(panelMatrix)
    K <- ncol(panelMatrix)
    obsMatrix <- !is.na(panelMatrix)
    NMatrix <- matrix(0, N, K)
    for (i in 1:N) {
        sq <- !is.na(panelMatrix[i, ])
        NMatrix[i, sq] <- cumsum(panelMatrix[i, sq])
    }
    wNbar <- colSums(NMatrix)
    ## Cumulative sum upper triangle matrix, [i, j] element = sum_{s=i}^j(x_s)
    cumSumMatrix <- function(x) {
        m <- outer(rep(1, length(x)), x)
        m[row(m) > col(m)] <- 0
        t(apply(m, 1, cumsum))
    }
    cumSumMatrix.wNbar <- cumSumMatrix(wNbar)
    ## Isotonic regression estimates of Lambda, use max-min formula
    ## Same as pava(wNbar / wAbar, wAbar) in 'Iso' package
    iso <- function(beta) {
        wAbar <- colSums(obsMatrix * c(exp(X %*% beta)))
        m <- cumSumMatrix.wNbar / cumSumMatrix(wAbar)
        m[row(m) > col(m)] <- 0
        for (i in 1:(K - 1))
            m[, K - i] <- pmin(m[, K - i], m[, K - i + 1])
        apply(m, 2, max)
    }
    f <- function(beta, Lambda) {
        c(t(X) %*% rowSums(NMatrix - outer(c(exp(X %*% beta)), Lambda) * obsMatrix))
    }
    beta <- engine@betaInit
    convergence <- 1
    for (i in 1:engine@maxIter) {
        Lambda <- iso(beta)
        betaPre <- beta
        if (ncol(X) == 1) {
            beta <- uniroot(f, engine@interval, Lambda)$root
        } else {
            beta <- nleqslv(beta, function(x) f(x, Lambda))$x
        }
        s <- beta - betaPre
        if (max(abs(s)) < engine@absTol | max(abs(s / betaPre)) < engine@relTol) {
            convergence <- 0
            break
        }
    }
    iter <- i
    list(beta=beta,
         baseline=stepfun(timeGrid, c(0, Lambda)),
         timeGrid=timeGrid,
         convergence=convergence,
         iter=iter)
}

##############################################################################
## Adapt from Lu, Zhang and Huang, Biometrika (2007)
## Maximum pseudolikehood using I-Spline
##############################################################################

doPanelFit.MPLs <- function(DF, panelMatrix, timeGrid, X, engine, stdErr) {
    N <- nrow(panelMatrix)
    K <- ncol(panelMatrix)
    ## ##############################
    ## Preparation for solving Lambda
    rawX <- as.matrix(DF[, -c(1:3)])
    DF <- ddply(DF, "ID", transform, cumCount=cumsum(count))
    ## number of interior knots
    nKnots <- ceiling(K^{1/3}) + 1
    tau <- max(timeGrid)
    bspBasis <- list(df=nKnots+3, knots=seq(0, tau, length=nKnots+2)[2:(nKnots + 1)],
                     intercept=TRUE, Boundary.knots=c(0, tau))
    ispMat <- ispline(timeGrid, bspBasis)
    rawIspMat <- ispline(DF$time, bspBasis)
    ## initial value of I-Spline coefficient
    alpha <- rep(1, bspBasis$df)
    lower <- rep(0, bspBasis$df)
    ## log pseudolikelihood, rawXB = c(rawX %*% beta)
    logPseudolike <- function(alpha, rawXB) {
        rawL <- c(rawIspMat %*% alpha)
        rawL <- ifelse(rawL <= 0, 1e-16, rawL)
        sum(DF$cumCount * (log(rawL) + rawXB) - exp(rawXB) * rawL)
    }
    ## ##############################
    ## Preparation for solving beta
    obsMatrix <- !is.na(panelMatrix)
    NMatrix <- matrix(0, N, K)
    for (i in 1:N) {
        sq <- !is.na(panelMatrix[i, ])
        NMatrix[i, sq] <- cumsum(panelMatrix[i, sq])
    }
    ## Lambda is the baseline evaluated at timeGrid
    f <- function(beta, Lambda) {
        c(t(X) %*% rowSums(NMatrix - outer(c(exp(X %*% beta)), Lambda) * obsMatrix))
    }
    ## ##############################
    ## Solve beta and Lambda in turn
    beta <- doPanelFit(DF, panelMatrix, timeGrid, X,
                       engine=new("MPL", betaInit=engine@betaInit), stdErr=NULL)$beta
    convergence <- 1
    for (i in 1:engine@maxIter) {
        rawXB <- c(rawX %*% beta)
        alpha <- optim(alpha, logPseudolike, rawXB=rawXB,
                       method = "L-BFGS-B", lower=lower,
                       control = list(fnscale=-1))$par
        Lambda <- c(ispMat %*% alpha)
        betaPre <- beta
        if (ncol(X) == 1) {
            beta <- uniroot(f, engine@interval, Lambda)$root
        } else {
            beta <- nleqslv(beta, function(x) f(x, Lambda))$x
        }
        s <- beta - betaPre
        if (max(abs(s)) < engine@absTol | max(abs(s / betaPre)) < engine@relTol) {
            convergence <- 0
            break
        }
    }
    iter <- i
    list(beta=beta,
         baseline=isplineFun(alpha, bspBasis),
         timeGrid=timeGrid,
         convergence=convergence,
         iter=iter)
}

##############################################################################
## Adapt from Lu, Zhang and Huang, Biometrika (2007),
## Maximum likehood using I-Spline
##############################################################################

doPanelFit.MLs <- function(DF, panelMatrix, timeGrid, X, engine, stdErr) {
    N <- nrow(panelMatrix)
    K <- ncol(panelMatrix)
    ## ##############################
    ## Preparation for solving baseline
    rawX <- as.matrix(DF[, -c(1:3)])
    ## number of interior knots
    nKnots <- ceiling(K^{1/3}) + 1
    tau <- max(timeGrid)
    bspBasis <- list(df=nKnots+3, knots=seq(0, tau, length=nKnots+2)[2:(nKnots + 1)],
                     intercept=TRUE, Boundary.knots=c(0, tau))
    ispMat <- ispline(timeGrid, bspBasis)
    rawIspMat <- ispline(DF$time, bspBasis)
    tempDF <- ddply(data.frame(ID=DF$ID, rawIspMat), "ID",
                    function(m) data.frame(diff(as.matrix(rbind(0, m[, -1])))))
    dRawIspMat <- as.matrix(tempDF[, -1])
    ## initial value of I-Spline coefficient
    alpha <- rep(1, bspBasis$df)
    lower <- rep(0, bspBasis$df)
    ## log likelihood
    logLike <- function(alpha, rawXB) {
        rawDL <- c(dRawIspMat %*% alpha)
        rawDL <- ifelse(rawDL <= 0, 1e-16, rawDL)
        sum(DF$count * (log(rawDL) + rawXB) - exp(rawXB) * rawDL)
    }
    ## ##############################
    ## Preparation for solving beta
    ## Total number of events for each subject
    totCount <- rowSums(panelMatrix, na.rm=TRUE)
    csrTime <- ddply(DF, "ID", tail, n=1)$time
    f <- function(beta, totBase) {
        c(t(X) %*% (totCount - totBase * c(exp(X %*% beta))))
    }
    ## ##############################
    ## Solve beta and baseline in turn
    beta <- doPanelFit(DF, panelMatrix, timeGrid, X,
                       engine=new("MPL", betaInit=engine@betaInit), stdErr=NULL)$beta
    convergence <- 1
    for (i in 1:engine@maxIter) {
        rawXB <- c(rawX %*% beta)
        alpha <- optim(alpha, logLike, rawXB=rawXB,
                       method="L-BFGS-B", lower=lower,
                       control=list(fnscale=-1))$par
        baseline <- isplineFun(alpha, bspBasis)
        totBase <- baseline(csrTime)
        betaPre <- beta
        if (ncol(X) == 1) {
            beta <- uniroot(f, engine@interval, totBase)$root
        } else {
            beta <- nleqslv(beta, function(x) f(x, totBase))$x
        }
        s <- beta - betaPre
        if (max(abs(s)) < engine@absTol | max(abs(s / betaPre)) < engine@relTol) {
            convergence <- 0
            break
        }
    }
    iter <- i
    list(beta=beta,
         baseline=baseline,
         timeGrid=timeGrid,
         convergence=convergence,
         iter=iter)
}

##############################################################################
## Scale-Change model (AMM), informative censoring
##############################################################################

doPanelFit.AMM <- function(DF, panelMatrix, timeGrid, X, engine, stdErr) {
    id <- DF[,1]
    X <- as.matrix(DF[, -(1:3)])
    tij <- DF[,2]
    mt <- DF[,3]
    cluster <- as.numeric(unlist(lapply(split(id, id), function(x) 1:length(x))))
    fit <- dfsane(engine@betaInit, pcEq, X = X, tij = tij, mt = mt,
                  cluster = cluster, id = id, conv.tol = engine@absTol, adjust = engine@adjust,
                  alertConvergence = FALSE, quiet = TRUE,
                  control = list(NM = FALSE, M = 100, noimp = 50,
                                 tol = engine@absTol, trace = FALSE))
    beta <- fit$par
    if (beta %*% beta > 1e4) {
        fit <- BBoptim(engine@betaInit, fn = function(x)
            sum(pcEq(x, X = X, tij = tij, mt = mt, cluster = cluster, id = id,
                     conv.tol = engine@absTol)^2), quiet = TRUE,
            control = list(trace = FALSE))
        beta <- fit$par
        }
    iter <- fit$iter
    convergence <- ifelse(beta %*% beta > 1e4, 1, 0)
    ## baseline lambda estimator
    sT <- log(tij) + X %*% beta
    if (max(abs(exp(sT))) == Inf || all(exp(sT) == 0))
        Ft <- getLam(sT, cluster, id, mt, conv.tol = engine@absTol)
    else Ft <- getLam(exp(sT), cluster, id, mt, conv.tol = engine@absTol)
    if (max(Ft) > 1e2)
        Ft <- getLam(sT, cluster, id, mt, conv.tol = .01)
    mi <- unlist(lapply(split(mt, id), sum))
    Ly <- mean((mi + engine@adjust) / (Ft[cumsum(unlist(lapply(split(tij, id), which.max)))] + engine@adjust), na.rm = TRUE)
    yi <- unlist(lapply(split(exp(sT), id), max))
    lamSm <- sm.regression(exp(sT), Ft, method = "cv", eval.points = exp(sT), display = "none")
    ## baseline <- with(lamSm, approx(eval.points, estimate, timeGrid, "constant", 50, 0, 1)$y)
    list(beta = beta, baseline = with(lamSm, stepfun(eval.points[order(eval.points)],
                                                     c(0, estimate[order(eval.points)]))),
         timeGrid = sort(lamSm$eval.points), Ly = Ly, lamSm = lamSm,
         convergence = convergence, iter = iter)
}

## smooth bootstrap for scale change model only
doPanelFit.AMM.smBoot <- function(DF, panelMatrix, timeGrid, X, engine, stdErr) {
    res <- doPanelFit(DF, panelMatrix, timeGrid, X, engine, NULL)
    id <- DF[,1]
    X <- as.matrix(DF[, -(1:3)])
    tij <- DF[,2]
    mt <- DF[,3]
    cluster <- as.numeric(unlist(lapply(split(id, id), function(x) 1:length(x))))
    bt <- getBtInf(X, tij, mt, cluster, id, res$beta, stdErr@R, res$lamSm$data, method = "smooth", adjust = engine@adjust)
    ## converged <- -which(rowSums(bt$lami) == 0 |rowSums(bt$lami) > 1e5)
    sT <- exp(tij * exp(X %*% res$beta))
    bt$lami <- bt$lami[,order(sT)]
    converged <- ifelse(rowSums(bt$lami) == 0 |rowSums(bt$lami) > 1e5, FALSE, TRUE)
    converged <- converged & (apply(bt$covmat, 1, function(x) sum(x^2)) < sum(res$beta^2) * 100)
    ## converged <- apply(bt$covmat, 1, function(x) (as.numeric(x %*% x) < 1e4))
    ## converged2 <- apply(bt$lami, 
    betaVar <- as.matrix(var(bt$covmat[converged, ], na.rm = TRUE))
    betaSE <- sqrt(diag(betaVar))
    baselineSE <- apply(bt$lami[converged,], 2, function(x) sd(x, na.rm = TRUE))
    baselineQT <- apply(bt$lami[converged,], 2, quantile, probs = c(.025, .975), na.rm = T, names = F)
    c(res, list(betaSE = betaSE, betaVar = betaVar, betaSEmat = bt$covmat, baselineMat = bt$lami,
                baselineSE = baselineSE, baselineQT = baselineQT, 
                R = length(converged)))
}

################################################################################
## Hu, Sun and Wei's method (HSWc), independent observation and censoring times
## Implemented from Sun's book "U_{II}^C
################################################################################
doPanelFit.EE.HSWc <- function(DF, panelMatrix, timeGrid, X, engine, stdErr) {
    N <- nrow(panelMatrix)
    K <- ncol(panelMatrix)
    yi <- aggregate(time ~ ID, data = DF, max)$time
    tij <- DF$time
    XX <- as.matrix(DF[,-c(1:3)])
    XX <- XX - outer(rep(1, nrow(XX)), colMeans(X))
    X <- X - outer(rep(1, nrow(X)), colMeans(X))
    txy <- outer(tij, yi, "<=")
    hi <- sapply(1:N, function(x) tij %in% with(DF, split(time, ID))[[x]])
    Nij <- aggregate(count ~ ID, data = DF, sum)$count
    f <- function(beta) {
        s1 <- t(txy * hi) %*% (c(exp(XX %*% beta)) * XX)
        s0 <- t(txy * hi) %*% (c(exp(XX %*% beta)) * matrix(1, nrow(XX), ncol(XX)))
        colSums(Nij * (X - s1 / s0))
    }
    if (ncol(X) == 1) {
        beta <- uniroot(f, engine@interval)$root
    } else {
        beta <- nleqslv(engine@betaInit, f)$x
    }
    list(beta=beta,
         baseline = function(x) rep(NA, length(x)),
         timeGrid=timeGrid,
         convergence=0)
}

################################################################################
## Hu, Sun and Wei's method (HSWm), dependent observation and censoring times
## Implemented from Sun's book "U_{II}^M
################################################################################
doPanelFit.EE.HSWm <- function(DF, panelMatrix, timeGrid, X, engine, stdErr) {
    N <- nrow(panelMatrix)
    K <- ncol(panelMatrix)
    yi <- aggregate(time ~ ID, data = DF, max)$time
    tij <- DF$time
    XX <- as.matrix(DF[,-c(1:3)])
    XX <- XX - outer(rep(1, nrow(XX)), colMeans(X))
    txy <- outer(tij, yi, "<=")
    hi <- sapply(1:N, function(x) tij %in% with(DF, split(time, ID))[[x]])
    Nij <- aggregate(count ~ ID, data = DF, sum)$count
    X <- X - outer(rep(1, nrow(X)), colMeans(X))
    nm <- names(DF)[-c(1:3)]
    ## Model observation times
    obsTimeDF <- ddply(DF, "ID", transform, start=c(0, head(time, -1)), end=time, event=1)
    obsTimeFml <- as.formula(paste("Surv(start, end, event) ~ ", paste(nm, collapse="+"),
                                   "+ cluster(ID)"))
    obsTimeFit <- coxph(obsTimeFml, obsTimeDF)    
    f <- function(beta) {
        s1 <- t(txy) %*% (c(exp(XX %*% beta)) * XX)
        s0 <- t(txy) %*% (c(exp(XX %*% beta)) * matrix(1, nrow(XX), ncol(XX)))
        colSums(Nij * (X - s1 / s0))
    }
    if (ncol(X) == 1) {
        beta <- uniroot(f, engine@interval)$root
    } else {
        beta <- nleqslv(engine@betaInit, f)$x
    }
    beta <- beta - obsTimeFit$coefficients
    list(beta=beta,
         baseline=function(x) rep(NA, length(x)),
         timeGrid=timeGrid,
         convergence=0)
}

##############################################################################
## Sun and Wei's method (SWa), independent observation and censoring times
##############################################################################
doPanelFit.EE.SWa <- function(DF, panelMatrix, timeGrid, X, engine, stdErr) {
    N <- nrow(panelMatrix)
    K <- ncol(panelMatrix)
    X <- X - outer(rep(1, nrow(X)), colMeans(X))
    Nbar <- apply(panelMatrix, 1, function(x) sum(cumsum(x[!is.na(x)])))
    f <- function(beta) {
        c(t(X) %*% c(exp(- X %*% beta) * Nbar))
    }
    if (ncol(X) == 1) {
        beta <- uniroot(f, engine@interval)$root
    } else {
        beta <- nleqslv(engine@betaInit, f)$x
    }
    list(beta=beta,
         baseline=function(x) rep(NA, length(x)),
         timeGrid=timeGrid,
         convergence=0)
}

##############################################################################
# Sandwich variance estimator for SWa
##############################################################################
## doPanelFit.SWa.Sandwich <- function(DF, panelMatrix, timeGrid, X, engine, stdErr) {
##     N <- nrow(panelMatrix)
##     K <- ncol(panelMatrix)
##     X <- X - outer(rep(1, nrow(X)), colMeans(X))
##     Nbar <- apply(panelMatrix, 1, function(x) sum(cumsum(x[!is.na(x)])))
##     f <- function(beta) {
##         c(t(X) %*% c(exp(- X %*% beta) * Nbar))
##     }
##     if (ncol(X) == 1) {
##         beta <- uniroot(f, engine@interval)$root
##     } else {
##         beta <- nleqslv(engine@betaInit, f)$x
##     }
##     A <- t(X) %*% (exp(- c(X %*% beta)) * Nbar * X)
##     Tau <- t(X) %*% (exp(- c(2 * X %*% beta)) * Nbar^2 * X)
##     invA <- solve(A)
##     betaVar <- invA %*% Tau %*% t(invA)
##     betaSE <- sqrt(diag(as.matrix(betaVar)))
##     list(beta=beta, baseline=function(x) rep(NA, length(x)),
##          convergence=0, betaVar=betaVar, betaSE=betaSE)
## }

##############################################################################
# Sun and Wei's method (SWb),
# conditional independent observation times and independent censoring times
##############################################################################

doPanelFit.EE.SWb <- function(DF, panelMatrix, timeGrid, X, engine, stdErr) {
    N <- nrow(panelMatrix)
    K <- ncol(panelMatrix)
    X <- X - outer(rep(1, nrow(X)), colMeans(X))
    nm <- names(DF)[-c(1:3)]
    ## Model observation times
    obsTimeDF <- ddply(DF, "ID", transform,
                       start=c(0, head(time, -1)), end=time, event=1)
    obsTimeFml <- as.formula(paste("Surv(start, end, event) ~ ",
                            paste(nm, collapse="+"),
                            "+ cluster(ID)"))
    obsTimeFit <- coxph(obsTimeFml, obsTimeDF)
    ## Estimating equation
    Nbar <- apply(panelMatrix, 1, function(x) sum(cumsum(x[!is.na(x)])))
    f <- function(beta) {
        c(t(X) %*% c(exp(- X %*% beta) * Nbar))
    }
    if (ncol(X) == 1) {
        beta <- uniroot(f, engine@interval)$root
    } else {
        beta <- nleqslv(engine@betaInit, f)$x
    }
    beta <- beta - obsTimeFit$coefficients
    list(beta=beta,
         baseline=function(x) rep(NA, length(x)),
         timeGrid=timeGrid,
         convergence=0)
}

##############################################################################
# Sun and Wei's method (SWc),
# conditional independent observation and censoring times
##############################################################################

doPanelFit.EE.SWc <- function(DF, panelMatrix, timeGrid, X, engine, stdErr) {
    N <- nrow(panelMatrix)
    K <- ncol(panelMatrix)
    X <- X - outer(rep(1, nrow(X)), colMeans(X))
    nm <- names(DF)[-c(1:3)]
    ## Model observation times
    obsTimeDF <- ddply(DF, "ID", transform,
                       start=c(0, head(time, -1)), end=time, event=1)
    obsTimeFml <- as.formula(paste("Surv(start, end, event) ~ ",
                                   paste(nm, collapse="+"),
                                   "+ cluster(ID)"))
    obsTimeFit <- coxph(obsTimeFml, obsTimeDF)
    ## Model censoring time
    cenTimeDF <- cbind(ddply(DF, "ID", tail, n=1), event=1)
    cenTimeFml <- as.formula(paste("Surv(time, event) ~ ",
                                   paste(nm, collapse="+")))
    cenTimeFit <- coxph(cenTimeFml, cenTimeDF)
    sf <- survfit(cenTimeFit)
    survfun <- stepfun(sf$time, c(1, sf$surv), right=TRUE)
    ## Cumulative panelMatrix
    cumPanelMatrix <- matrix(NA, N, K)
    for (i in 1:N) {
        sq <- which(!is.na(panelMatrix[i, ]))
        cumPanelMatrix[i, sq] <- cumsum(panelMatrix[i, sq])
    }
    ## Estimating equation
    Nbar <- rowSums(cumPanelMatrix *
                    t(outer(survfun(timeGrid), - exp(c(X %*% cenTimeFit$coefficients)), "^")),
                    na.rm=TRUE)
    f <- function(beta) {
        c(t(X) %*% c(exp(- X %*% beta) * Nbar))
    }
    if (ncol(X) == 1) {
        beta <- uniroot(f, engine@interval)$root
    } else {
        beta <- nleqslv(engine@betaInit, f)$x
    }
    beta <- beta - obsTimeFit$coefficients
    list(beta=beta,
         baseline=function(x) rep(NA, length(x)),
         timeGrid=timeGrid,
         convergence=0)
}

##############################################################################
# Bootstrap variance estimation for any engine
##############################################################################
doPanelFit.Engine.Bootstrap <- function(DF, panelMatrix, timeGrid, X, engine, stdErr) {
    N <- nrow(panelMatrix)
    K <- ncol(panelMatrix)
    tau <- max(timeGrid)
    res <- doPanelFit(DF, panelMatrix, timeGrid, X, engine, NULL)
    engine@betaInit <- res$beta
    R <- stdErr@R
    betaMatrix <- matrix(0, R, length(res$beta))
    baselineMatrix <- matrix(NA, R, K)
    convergence <- rep(0, R)
    uID <- unique(DF$ID)
    for (i in 1:R) {
        index <- sort(sample(1:N, size=N, replace=TRUE))
        mylist <- apply(matrix(index), 1, function(x) which(DF$ID == uID[x]))
        DF2 <- DF[unlist(mylist), ]
        DF2$ID <- rep(1:N, unlist(lapply(mylist, length)))
        panelMatrix2 <- panelMatrix[index, ]
        X2 <- as.matrix(X[index, ])
        subCol <- which(colSums(!is.na(panelMatrix2)) > 0)
        panelMatrix2 <- panelMatrix2[, subCol]
        timeGrid2 <- timeGrid[subCol]
        res2 <- doPanelFit(DF2, panelMatrix2, timeGrid2, X2, engine, NULL)
        betaMatrix[i, ] <- res2$beta
        tau2 <- max(timeGrid2)
        sq <- which(timeGrid <= tau2)
        if (tau2 < tau)
            baselineMatrix[i, sq] <- res2$baseline(timeGrid[sq])
        else
            baselineMatrix[i, ] <- res2$baseline(timeGrid)
        convergence[i] <- res2$convergence
    }
    converged <- which(convergence == 0)
    ## if (sum(convergence != 0) > 0) ## warning("Some bootstrap samples failed to converge")
    ##     print("Warning: Some bootstrap samples failed to converge")
    if (sum(convergence != 0) > 0 || all(convergence != 0) || sum(convergence == 0) == 1) {
        ## warning("Some bootstrap samples failed to converge")
        print("Warning: some bootstrap samples failed to converge")
        converged <- 1:R
    }
    betaVar <- var(betaMatrix[converged, ], na.rm = TRUE)
    betaSE <- sqrt(diag(as.matrix(betaVar)))
    baselineSE <- sd(baselineMatrix[converged, ], na.rm=TRUE)
    # 2.5% and 97.5% quantiles of baseline bootstrap estimates, 2*K
    baselineQT <- apply(baselineMatrix[converged, ], 2, quantile,
                        probs=c(0.025, 0.975), na.rm=TRUE, names=FALSE)
    c(res, list(betaSE=betaSE, betaVar=betaVar, betaSEmat=betaMatrix,
                baselineMat=baselineMatrix, baselineSE=baselineSE,
                baselineQT=baselineQT, R=length(converged)))
}

##############################################################################
# Imputation based variance estimation for AEE only
##############################################################################
doPanelFit.AEE.Impute <- function(DF, panelMatrix, timeGrid, X, engine, stdErr) {
    N <- nrow(panelMatrix)
    K <- ncol(panelMatrix)
    eTime <- timeGrid
    sTime <- c(0, head(eTime, -1))
    ## Impute the exact recurrent event time, use multinomial assumption
    imputeSurvData <- function(lambda) {
        survData <- NULL
        for (i in 1:N) {
            eIndex <- as.numeric(which(!is.na(panelMatrix[i,])))
            sIndex <- c(0, head(eIndex, -1)) + 1
            impTime <- event <- NULL
            for (j in 1:length(eIndex)) {
                if (panelMatrix[i, eIndex[j]] > 0) {
                    indexSeq <- seq(sIndex[j], eIndex[j])
                    numEvent <- c(rmultinom(1,
                                            size=panelMatrix[i, eIndex[j]],
                                            prob=lambda[indexSeq]))
                    for (k in which(numEvent > 0)) {
                        impTime <- c(impTime, sort(runif(numEvent[k],
                                                         sTime[sIndex[j] - 1 + k],
                                                         eTime[sIndex[j] - 1 + k])))
                    }
                    event <- c(event, rep(1, panelMatrix[i, eIndex[j]]))
                } else {
                    impTime <- c(impTime, eTime[eIndex[j]])
                    event <- c(event, 0)
                }
            }
            survData <- rbind(survData, data.frame(ID=i,
                                                   start=c(0, head(impTime, -1)),
                                                   end=impTime,
                                                   event=event))
        }
        survData
    }
    ## ############################
    R <- stdErr@R
    betaMatrix <- matrix(0, R, ncol(X))
    betaVarMatrix <- matrix(0, ncol(X), ncol(X))
    baselineMatrix <- matrix(0, R, K)
    baselineVarMatrix <- matrix(0, R, K)
    res <- doPanelFit(DF, panelMatrix, timeGrid, X, engine, NULL)
    for (i in 1:R) {
        survData <- imputeSurvData(res$lambda)
        freq <- as.numeric(table(survData$ID))
        survData <- cbind(survData, apply(X, 2, function(y) rep(y, freq)))
        attr(survData, "names")[-(1:4)] <- xname <- paste("X", 1:ncol(X), sep="")
        fml <- as.formula(paste("Surv(start, end, event) ~ ",
                                paste(xname, collapse="+"),
                                "+ cluster(ID)"))
        resCoxph <- coxph(fml, survData)
        ## beta
        betaMatrix[i, ] <- as.numeric(resCoxph$coefficients)
        betaVarMatrix <- betaVarMatrix + resCoxph$var
        ## baseline
        newdf <- data.frame(t(rep(0, ncol(as.matrix(X)))))
        attr(newdf, "names") <- xname
        sfit <- survfit(resCoxph, newdata=newdf)
        baselineMatrix[i, ] <- stepfun(sfit$time, c(0, -log(sfit$surv)))(timeGrid)
        baselineVarMatrix[i, ] <- (stepfun(sfit$time, c(0, sfit$std.err))(timeGrid))^2
    }
    betaVar <- betaVarMatrix / R   + (1 + 1/R) * var(betaMatrix)
    betaSE <- sqrt(diag(as.matrix(betaVar)))
    baselineSE <- sqrt(colMeans(baselineVarMatrix) +
                       (1 + 1/R) * sd(baselineMatrix)^2)
    c(res, list(betaSE=betaSE, betaVar=betaVar, baselineSE=baselineSE))
}

##############################################################################
# Imputation based variance estimation for AEEX
##############################################################################

doPanelFit.AEEX.Impute <- function(DF, panelMatrix, timeGrid, X, engine, stdErr) {
    N <- nrow(panelMatrix)
    K <- ncol(panelMatrix)
    res <- doPanelFit(DF, panelMatrix, timeGrid, X, engine, NULL)
    ## Impute the number of count between the actuall censoring time and tau
    for (i in 1:N) {
        end <- which(!is.na(panelMatrix[i, ]))
        if (tail(end, 1) < K) {
            sq <- seq(tail(end, 1) + 1, K)
            panelMatrix[i, K] <- (sum(panelMatrix[i, end]) + engine@a) *
                sum(res$lambda[sq]) / (sum(res$lambda[-sq]) + engine@a)
        }
    }
    ## Use doPanelFit.AEE.Impute
    seRes <- doPanelFit(DF, panelMatrix, timeGrid, X,
                        new("AEE", betaInit=engine@betaInit), stdErr)
    c(res, list(betaSE=seRes$betaSE, betaVar=seRes$betaVar, baselineSE=seRes$baselineSE))
}

##############################################################################
# Observed information matrix based variance estimation for AEE
##############################################################################

doPanelFit.AEE.Sandwich <- function(DF, panelMatrix, timeGrid, X, engine, stdErr) {
    N <- nrow(panelMatrix)
    K <- ncol(panelMatrix)
    res <- doPanelFit(DF, panelMatrix, timeGrid, X, engine, NULL)
    beta <- res$beta
    lambda <- res$lambda
    atRiskMatrix <- matrix(0, N, K)
    lastObs <- apply(panelMatrix, 1, function(x) tail(which(!is.na(x)), 1))
    atRiskMatrix[col(atRiskMatrix) <= lastObs] <- 1
    ## A is the complete information matrix for all subjects
    A11 <- diag(c(t(exp(X %*% beta)) %*% atRiskMatrix))
    A21 <- t(c(exp(X %*% beta)) * X) %*% atRiskMatrix
    A22 <- t(X) %*% (X * c(exp(X %*% beta)) * c(atRiskMatrix %*% lambda))
    A <- rbind(cbind(A11, t(A21) * lambda),
               cbind(A21, A22))
    ## B is the missing information matrix for all subjects
    B <- matrix(0, K + ncol(X), K + ncol(X))
    for (i in 1:N) {
        sq <- which(!is.na(panelMatrix[i, ]))
        mi <- panelMatrix[i, sq]
        if (is.na(panelMatrix[i, K])) {
            sq <- c(sq, K)
            mi <- c(mi, 0)
        }
        dsq <- diff(c(0, sq))
        ndsq <- length(dsq)
        ## normalize lambda, multinomial
        p <- lambda / rep(diff(c(0, cumsum(lambda)[sq])), dsq)
        p[which(p == Inf)] <- 1
        blkp <- p * diag(1, ndsq)[rep(1:ndsq, dsq), ]
        ## atRisk (rij) is taken care of
        Xi <- X[i, ]
        B11 <- rep(mi, dsq) * (diag(p) - blkp %*% t(blkp))
        B12 <- rowSums(B11) %*% t(Xi)
        B22 <- sum(B11) * Xi %*% t(Xi)
        B <- B + rbind(cbind(B11, B12),
                       cbind(t(B12), B22))
        ## matrix.plot(B)
    }
    ## Inverse of observed information matrix
    V <- solve(A - B)
    ## Regularization
    dgV <- diag(V)
    dgV[which(dgV < 0)] <- 0
    diag(V) <- dgV
    ## Sandwich estimator
    e <- matrix(0, N, K)
    for (i in 1:N) {
        end <- which(!is.na(panelMatrix[i, ]))
        start <- c(1, head(end, -1) + 1)
        for (j in which(panelMatrix[i, end] > 0)) {
            sq <- seq(start[j], end[j])
            e[i, sq] <- panelMatrix[i, end[j]] * lambda[sq] / sum(lambda[sq])
        }
    }
    U1 <- (t(e) - outer(lambda, c(exp(X %*% beta)))) * t(atRiskMatrix)
    U2 <- t(colSums(U1) * X)
    U <- rbind(U1, U2)
    V <- V %*% (U %*% t(U)) %*% t(V)
    ##
    betaVar <- V[-c(1:K), -c(1:K)]
    betaSE <- sqrt(diag(as.matrix(betaVar)))
    lowOne <- matrix(0, K, K)
    lowOne[row(lowOne) >= col(lowOne)] <- 1
    vLambda <- diag(lowOne %*% V[1:K, 1:K] %*% t(lowOne))
    baselineSE <- sqrt(vLambda)
    c(res, list(betaSE=betaSE, betaVar=betaVar, baselineSE=baselineSE))
}

##############################################################################
# Observed information matrix based variance estimation for AEEX
##############################################################################

doPanelFit.AEEX.Sandwich <- function(DF, panelMatrix, timeGrid, X, engine, stdErr) {
    N <- nrow(panelMatrix)
    K <- ncol(panelMatrix)
    res <- doPanelFit(DF, panelMatrix, timeGrid, X, engine, NULL)
    beta <- res$beta
    lambda <- res$lambda
    atRiskMatrix <- matrix(1, N, K)
    ## A is the complete information matrix for all subjects
    A11 <- diag(c(t(exp(X %*% beta)) %*% atRiskMatrix))
    A21 <- t(c(exp(X %*% beta)) * X) %*% atRiskMatrix
    A22 <- t(X) %*% (X * c(exp(X %*% beta)) * c(atRiskMatrix %*% lambda))
    A <- rbind(cbind(A11, t(A21) * lambda),
               cbind(A21, A22))
    ## B is the missing information matrix for all subjects
    B <- matrix(0, K + ncol(X), K + ncol(X))
    for (i in 1:N) {
        sq <- which(!is.na(panelMatrix[i, ]))
        mi <- panelMatrix[i, sq]
        if (is.na(panelMatrix[i, K])) {
            y1 <- sum(mi)
            mu1 <- sum(lambda[seq(1, tail(mi, 1))])
            mu2 <- sum(lambda[seq(tail(mi, 1) + 1, K)])
            sq <- c(sq, K)
            mi <- c(mi, (y1 + engine@a) * mu2 / (mu1 + engine@a))
        }
        dsq <- diff(c(0, sq))
        ndsq <- length(dsq)
        ## normalize lambda, multinomial
        p <- lambda / rep(diff(c(0, cumsum(lambda)[sq])), dsq)
        p[which(p == Inf)] <- 1
        blkp <- p * diag(1, ndsq)[rep(1:ndsq, dsq), ]
        Xi <- X[i, ]
        B11 <- rep(mi, dsq) * (diag(p) - blkp %*% t(blkp))
        if (is.na(panelMatrix[i, K])) {
            p[seq(1, K - tail(dsq, 1))] <- 0
            B11 <- B11 + outer(p, p) * (y1 + engine@a) *
                mu2 / (mu2 + engine@a) * (1 + mu1 / (mu2 + engine@a))
        }
        B12 <- rowSums(B11) %*% t(Xi)
        B22 <- sum(B11) * Xi %*% t(Xi)
        B <- B + rbind(cbind(B11, B12),
                       cbind(t(B12), B22))
        ## matrix.plot(B)
    }
    ## Inverse of observed information matrix
    V <- solve(A - B)
    ## Regularization
    dgV <- diag(V)
    dgV[which(dgV < 0)] <- 0
    diag(V) <- dgV
    ## Sandwich estimator
    e <- matrix(0, N, K)
    for (i in 1:N) {
        end <- which(!is.na(panelMatrix[i, ]))
        start <- c(1, head(end, -1) + 1)
        for (j in which(panelMatrix[i, end] > 0)) {
            sq <- seq(start[j], end[j])
            e[i, sq] <- panelMatrix[i, end[j]] * lambda[sq] / sum(lambda[sq])
        }
    }
    U1 <- (t(e) - outer(lambda, c(exp(X %*% beta)))) * t(atRiskMatrix)
    U2 <- t(colSums(U1) * X)
    U <- rbind(U1, U2)
    V <- V %*% (U %*% t(U)) %*% t(V)
    ##
    betaVar <- V[-c(1:K), -c(1:K)]
    betaSE <- sqrt(diag(as.matrix(betaVar)))
    lowOne <- matrix(0, K, K)
    lowOne[row(lowOne) >= col(lowOne)] <- 1
    vLambda <- diag(lowOne %*% V[1:K, 1:K] %*% t(lowOne))
    baselineSE <- sqrt(vLambda)
    c(res, list(betaSE=betaSE, betaVar=betaVar, baselineSE=baselineSE))
}

##############################################################################
# Class Definition
##############################################################################

setClass("Engine",
         representation(betaInit="numeric", interval="numeric",
                        maxIter="numeric", absTol="numeric", relTol="numeric"),
         prototype(betaInit=0, interval=c(-5, 5),
                   maxIter=500, absTol=1e-4, relTol=1e-4),
         contains="VIRTUAL")

setClass("AEE",
         representation(),
         prototype(),
         contains="Engine")

setClass("AEEX",
         representation(a="numeric"),
         prototype(maxIter = 500, a = 0.1),
         contains="Engine")

setClass("AMM",
         representation(adjust="numeric"),
         prototype(adjust = 0.1),
         contains="Engine")

setClass("HWZ",
         representation(unitWeight="logical", adjust="character"),
         prototype(maxIter=150, absTol=1e-4,
                   unitWeight=TRUE, adjust="W"),
         validity=function(object) {
             object@adjust <- match.arg(object@adjust, choices=c("W", "H"))
             return(TRUE)
         }, contains="Engine")

setClass("MPL", contains="Engine")
setClass("MPLs", contains="Engine")
setClass("MLs", contains="Engine")
setClass("EE.HSWc", contains="Engine")
setClass("EE.HSWm", contains="Engine")
setClass("EE.SWa", contains="Engine")
setClass("EE.SWb", contains="Engine")
setClass("EE.SWc", contains="Engine")

setClass("StdErr")
setClass("Bootstrap",
         representation(R="numeric"),
         prototype(R=30),
         contains="StdErr")
setClass("Impute",
         representation(R="numeric"),
         prototype(R=30),
         contains="StdErr")
setClass("Sandwich",
         representation(),
         prototype(),
         contains="StdErr")
setClass("smBootstrap",
         representation(R = "numeric"),
         prototype(R=30),
         contains="StdErr")

##############################################################################
# Method Dispatch
##############################################################################
setGeneric("doPanelFit",
           function(DF, panelMatrix, timeGrid, X, engine, stdErr) {
               standardGeneric("doPanelFit")
           })

setMethod("doPanelFit",
          signature(engine="AEE", stdErr="NULL"),
          doPanelFit.AEE)

setMethod("doPanelFit",
          signature(engine="AEEX", stdErr="NULL"),
          doPanelFit.AEEX)

setMethod("doPanelFit",
          signature(engine="HWZ", stdErr="NULL"),
          doPanelFit.HWZ)

setMethod("doPanelFit",
          signature(engine="MPL", stdErr="NULL"),
          doPanelFit.MPL)

setMethod("doPanelFit",
          signature(engine="MPLs", stdErr="NULL"),
          doPanelFit.MPLs)

setMethod("doPanelFit",
          signature(engine="MLs", stdErr="NULL"),
          doPanelFit.MLs)

setMethod("doPanelFit",
          signature(engine="AMM", stdErr="NULL"),
          doPanelFit.AMM)

setMethod("doPanelFit",
          signature(engine="EE.HSWc", stdErr="NULL"),
          doPanelFit.EE.HSWc)

setMethod("doPanelFit",
          signature(engine="EE.HSWm", stdErr="NULL"),
          doPanelFit.EE.HSWm)

setMethod("doPanelFit",
          signature(engine="EE.SWa", stdErr="NULL"),
          doPanelFit.EE.SWa)

setMethod("doPanelFit",
          signature(engine="EE.SWb", stdErr="NULL"),
          doPanelFit.EE.SWb)

setMethod("doPanelFit",
          signature(engine="EE.SWc", stdErr="NULL"),
          doPanelFit.EE.SWc)

setMethod("doPanelFit",
          signature(engine="Engine", stdErr="Bootstrap"),
          doPanelFit.Engine.Bootstrap)

setMethod("doPanelFit",
          signature(engine="AEE", stdErr="Impute"),
          doPanelFit.AEE.Impute)

setMethod("doPanelFit",
          signature(engine="AEE", stdErr="Sandwich"),
          doPanelFit.AEE.Sandwich)

setMethod("doPanelFit",
          signature(engine="AEEX", stdErr="Impute"),
          doPanelFit.AEEX.Impute)

setMethod("doPanelFit",
          signature(engine="AEEX", stdErr="Sandwich"),
          doPanelFit.AEEX.Sandwich)

setMethod("doPanelFit",
          signature(engine="AMM", stdErr="smBootstrap"),
          doPanelFit.AMM.smBoot)

##############################################################################
## User's Main Function
##############################################################################

#' Fits Semiparametric Regression Models for Panel Count Survival Data
#'
#' @description
#' Fits an proportional means model:
#' \deqn{\Lambda (t; X_i) = E[N_i(t)|X_i] = \Lambda(t)e^{X_i ' \beta},}
#' where \eqn{\beta} is a \eqn{p \times 1} vector of covariate
#' coefficient and \eqn{\Lambda(\cdot)} is a completely unspecified
#' baseline mean function.
#' 
#' Estimating procedures include: Wang-Yan's augmented estimating
#' equations (\code{"AEE"}, \code{"AEEX"}),
#' Huang-Wang-Zhang's method (\code{"HWZ"}),
#' Zhang's maximum pseudo-likelihood (\code{"MPL"}),
#' Maximum pseudolikelihood with I-Splines (\code{"MPLs"}),
#' Maximum likelihood with I-Splines (\code{"MLs"}),
#' Sun-Wei's method (\code{"EE.SWa"}, \code{"EE.SWb"}, \code{"EE.SWc"}),
#' and Hu-Sun-Wei's method (\code{"EE.HSWc"}, \code{"EE.HSWm"}).
#'
#' The function can also fits an accelerated mean model (\code{"AMM"}):
#' \deqn{\Lambda (t; X_i) = E[N_i(t)|X_i] = \Lambda(te^{X_i ' \beta})e^{X_i ' \beta}.}
#' 
#' @details
#' Some assumptions details about the observation times and censoring
#' time need clarification. Three possible scenarios of observation
#' times are considered: 1) independent observation times -- the
#' observation times are independent of the underlying recurrent event
#' process; 2) conditional independent observation times -- the
#' observation times are independent of the event process given
#' observed covariates; 3) informative observation times -- after
#' conditioning on observed covariates, the observation times and the
#' event process are still dependent through an unobserved
#' multiplicative frailty. Similarly, the three scenarios apply to the
#' censoring time.
#'
#' \code{"AEE"} and \code{"AEEX"} are the augmented estimating
#'  equation methods under conditional independent censoring and
#'  informative censoring respectively. Both allow informative observation times.
#'
#' \code{"HWZ"} is Huang-Wang-Zhang's method. It allows both
#'   information observation times and informative censoring time. It does
#'   not need to specify the dependence structure or model the frailty.
#'
#' \code{"MPL"} and \code{"MPLs"} are maximum pseudolikelihood
#'   methods, with nonparametric and monotone spline estimates of the baseline
#'   mean function respectively. They assume conditional independent
#'   observation times and censoring time. The underlying event process
#'   is assumed to be Poisson, and the within subjects dependence is ignored.
#'
#'  \code{"MLs"} is maximum likelihood method with monotone splines
#'   estimates of the baseline mean function. It assumes conditional
#'   independent observation times and censoring time, and a Poisson
#'   underlying event process.
#'
#' \code{"EE.SWa"}, \code{"EE.SWb"} and \code{"EE.SWc"} are estimating
#'   equation approaches based on Sun-Wei's methods.
#'   The first assumes independent observation times and censoring
#'   time. The second assumes conditional independent observation times but
#'   independent censoring time. The third assumes conditional independent
#'   observation times and censoring time. All three variations work on
#'   centered covariates and avoid estimating the baseline mean.
#'
#'  \code{"EE.HSWc"} and \code{"EE.HSWm"} are estimating
#'   equation approaches based on Hu-Sun-Wei's methods.
#'   The first assumes independent observation times and censoring
#'   time. The second assumes conditional independent observation times but
#'   independent censoring time. Both variations work on centered covariates
#'   and avoid estimating the baseline mean.
#'
#' \code{"AMM"} is the accelerated mean model.
#'   The observation time process is allowed to be correlated with
#'   the underlying recurrent event process through a frailty variable,
#'   which does not need to be specified. The model also allows marginal
#'   interpretations for the regression parameters and connects naturally
#'   with AFT model. See Chiou et al. (2017) for more details.
#' 
#' For standard errors estimation method:
#'
#'   \code{"NULL"} means do not calculate standard errors;
#'   \code{"smBootstrap"} is the smoothed bootstrap estimation method
#'   that works with \code{"AMM"}.
#'   \code{"Bootstrap"} works with all fitting methods; \code{"Impute"} is
#'   the multiple imputation method that works with \code{"AEE"} and
#'   \code{"AEEX"}; \code{"Sandwich"} is the robust sandwich estimation
#'   method that works with \code{"AEE"} and \code{"AEEX"}.
#'
#'   The \code{control} argument is a list that can supply any of the
#'   following components:
#'   \describe{
#'     \item{\code{betaInit}:}{Object of class \code{"numeric"},
#'       initial value for covariate coefficient, default \code{0}.}
#'     \item{\code{interval}:}{Object of class \code{"numeric"},
#'       initial search interval for solving \code{beta}, default
#'       \code{(-5, 5)}.}
#'     \item{\code{maxIter}:}{Object of class \code{"numeric"}, maximum
#'       iteration allowed, default \code{500} for \code{"AEEX"} and
#'       \code{"HWZ"}, \code{150} for others.}
#'     \item{\code{absTol}:}{Object of class \code{"numeric"}, absolute
#'       tolerance, default \code{1e-6}.}
#'     \item{\code{relTol}:}{Object of class \code{"numeric"}, relative
#'       tolerance, default \code{1e-6}.}
#'     \item{\code{a}:}{Object of class \code{"numeric"}, a tune parameter,
#'       default \code{0.1}. In the case of gamma frailty, \code{"a"}
#'       corresponds to the value of both shape and rate parameters.}
#'     \item{\code{R}:}{Object of class \code{"numeric"}, number of
#'       bootstrap or imputation, default 30.}
#'   }
#'
#' @return An object of S3 class \code{"panelReg"} representing the fit.
#' See \code{panelReg.object} for details.
#'
#' @references Chiou, S., Xu, G., Yan, J., and Huang, C.-Y. (2017).
#' Semiparametric estimation of the accelerated mean model with panel count data under
#' informative examination times. \emph{Biometrics}, to appear.
#' <doi: 10.1111/biom.12840>.
#' @references Huang, C.-Y., Wang, M., and Zhang, Y. (2006).
#' Analysing panel count data with informative observation times.
#' \emph{Biometrika}, \bold{93}(4), 763--776.
#' @references Hu, X. J., Sun, J. and Wei, L. J. (2003).
#' Regression parameter estimation from panel counts.
#' \emph{Scandinavian Journal of Statistics}, \bold{30}, 25--43.
#' @references Lu, M., Zhang, Y., and Huang, J. (2007).
#' Estimation of the mean function with panel count data using monotone polynomial splines.
#' \emph{Biometrika}, \bold{94}(3), 705--718.
#' @references Sun, J. and Wei, L. J. (2000). Regression analysis of panel count
#' data with covariates-dependent observation and censoring times.
#' \emph{Journal of the Royal Statistical Society, Series B: Statistical Methodology},
#' \bold{62}(2), 293--302.
#' @references Wang, X. and Yan, J. (2011). Fitting semiparametric regressions for panel
#' count survival data with an R package spef.
#' \emph{Computer Methods and Programs in Biomedicine}, \bold{104}(2), 278--285.
#' @references Wang, X. and Yan, J. (2013). Augmented estimating equations for
#' semiparametric panel count regression with informative observation
#' times and censoring time. \emph{Statistica Sinica}, 359--381.
#' @references Zhang, Y. (2002). A Semiparametric pseudolikelihood estimation method
#' for panel count data. \emph{Biometrika}, \bold{89}(1), 39--48.
#'
#' @seealso \code{\link{panelReg.object}}
#'
#' @examples
#' \dontrun{
#' data(blaTum)
#' ## Fit the bladder tumor data set
#' formula <- PanelSurv(id, time, count) ~ num + size + treatment
#'
#' panelReg(formula, data = blaTum, method = "AEE", se = "Sandwich")
#' panelReg(formula, data = blaTum, method = "AEEX", se = "Impute",
#'          control = list(a = 0.1, R = 30))
#' panelReg(formula, data = blaTum, method = "HWZ", se = "Bootstrap",
#'          control = list(R = 30))
#' panelReg(formula, data = blaTum, method = "MLs", se = "NULL")
#' panelReg(formula, data = blaTum, method = "EE.SWa", se = "Bootstrap",
#'          control = list(R = 30))
#' panelReg(formula, data = blaTum, method = "EE.HSWc", se = "Bootstrap",
#'          control = list(R = 30))
#' }
#'
#' @param formula A formula object, with the response on the left of a
#'     "~" operator, and the terms on the right. The response must be a
#'     panel count survival object as returned by function \code{PanelSurv}.
#' @param data A data.frame in which to interpret the variables named in
#'     the \code{formula}. Three variables including subjects' id,
#'     observation times, and number of new events since last
#'     observation time are required to feed into function
#'     \code{PanelSurv} as response. At least one covariate variable is required.
#' @param method Fitting method. See \sQuote{Details}.
#' @param se Standard error estimation method. See \sQuote{Details}.
#' @param control A list of control parameters. See \sQuote{Details}.
#' @export
panelReg <- function(formula, data,
                     method=c("AEE", "AEEX", "HWZ", "MPL", "MPLs", "MLs", "AMM",
                              "EE.SWa", "EE.SWb", "EE.SWc", "EE.HSWc", "EE.HSWm"),
                     se = c("NULL", "smBootstrap", "Bootstrap", "Impute", "Sandwich"),
                     control = list()) {
    method <- match.arg(method)
    se <- match.arg(se)
    Call <- match.call()
    ## A PanelSurv object
    obj <- eval(formula[[2]], data)
    if (!is.PanelSurv(obj))
        stop("Response must be a PanelSurv object")
    ## Combine respones data frame and covariate data frame (remove intercept column)
    ## Multiple rows per subject
    formula[[2]] <- NULL
    if (formula == ~ 1) {
        DF <- cbind(obj$psDF, zero=0)
    } else {
        DF <- cbind(obj$psDF, model.matrix(formula, data))[, -4]
    }
    DF <- DF[order(DF$ID, DF$time), ]
    idTmp <- with(DF, as.numeric(unlist(lapply(split(ID, ID), length))))
    DF$ID <- rep(1:length(idTmp), idTmp)
    ## Design matrix, one row per subject
    X <- as.matrix(ddply(DF, "ID", head, n=1)[, -c(1:3)])
    ## Create an Engine object
    engine.control <- control[names(control) %in% names(attr(getClass(method), "slots"))]
    engine <- do.call("new", c(list(Class=method), engine.control))
    if (length(engine@betaInit) == 1 & ncol(X) > 1)
        engine@betaInit <- rep(engine@betaInit, ncol(X))
    if (length(engine@betaInit) > 1 & length(engine@betaInit) != ncol(X))
        stop("Invalid length of initial beta values!")
    ## Create a StdErr object
    if (se == "NULL")
        stdErr <- NULL
    else {
        stdErr.control <- control[names(control) %in% names(attr(getClass(se), "slots"))]
        stdErr <- do.call("new", c(list(Class=se), stdErr.control))
    }
    ## Dispatch, DF = "ID", "time", "count", covariates
    fit <- doPanelFit(DF=DF, panelMatrix=obj$panelMatrix, timeGrid=obj$timeGrid,
                      X=X, engine=engine, stdErr=stdErr)
    names(fit$beta) <- names(DF)[-c(1:3)]
    ## ~1?
    if (formula == ~1) fit$beta <- NULL
    fit$call <- Call
    fit$method = method
    class(fit) <- "panelReg"
    fit
}


##############################################################################
## Required functions for pcReg
##############################################################################

pcEq <- function(alpha, X, tij, mt, cluster, id, conv.tol = 1e-2, adjust = 0.02) {
    yi <- unlist(lapply(split(tij, id), max))
    yid <- cumsum(unlist(lapply(split(id, id), length)))
    mi <- unlist(lapply(split(mt, id), sum))
    n <- length(yi)
    p <- ncol(X)
    ## sY <- log(yi) + X %*% alpha
    sT <- log(tij) + X %*% alpha
    if (max(abs(exp(sT))) == Inf || all(exp(sT) == 0))
        Ft <- getLam(sT, cluster, id, mt, conv.tol = conv.tol)
    else Ft <- getLam(exp(sT), cluster, id, mt, conv.tol = conv.tol)
    Ly <- mean((mi + adjust) / (Ft[cumsum(unlist(lapply(split(tij, id), which.max)))] + adjust),
               na.rm = TRUE)
    res <- vector("double", p)
    res <- .C("alphaEq1", as.double(X[which(cluster == 1),]), as.double(Ft[yid] * Ly),
              as.integer(mi), as.integer(n), as.integer(p),
              out = as.double(res), PACKAGE = "spef")$out
    res / n^2
}

lamEm <- function(f, ordt, tij, id, mt, aijk, bik) {
    p <- diff(f)
    p0 <- rep(p, rep(nrow(bik), ncol(bik)))
    tmp <- mt * ((aijk * p0) / rowSums(aijk * p0) + (1 - bik) * p0 / rowSums(bik * p0))
    dk <- colSums(tmp * is.finite(tmp), na.rm = TRUE)
    f2 <- cumsum(dk) / sum(dk)
    return(c(0, f2))
}

getLam <- function(tij, cluster, id, mt, conv.tol = 1e-2) {
    ki <- unlist(lapply(split(id, id), length))
    if (sum(ki == 1) > 0) tmp <- tij[-which(id %in% names(ki)[which(ki == 1)])]
    else
        tmp <- tij
    tmp <- unique(tmp) ## newly added
    ordt <- c(0, tmp[order(tmp)])
    N <- length(tmp)
    Lambda0 <- rep(0, N)
    ub <- sapply(1:N + 1, function(x) tij >= ordt[x])
    tijTmp <- c(0, tij[-length(tij)])
    tijTmp <- ifelse(cluster == 1, 0, tijTmp)
    lb <- sapply(1:N, function(x) tijTmp <= ordt[x])
    aijk <- ub * lb
    yi <- unlist(lapply(split(tij, id), max))
    ## bik <- apply(sapply(1:N + 1, function(x) yi >= ordt[x]), 2, function(y) rep(y, ki))
    bik <- outer(rep(yi, ki), ordt[-1], ">=")
    f0 <- seq(0, 1, 1 / N)
    em <- squarem(par = f0, fixptfn = lamEm, objfn = lam.loglik,
                  ordt = ordt, tij = tij,
                  id = id, mt = mt, aijk = aijk, bik = bik,
                  control = list(tol = conv.tol))
    Lambda0 <- approx(ordt, em$par, tij, yright = max(em$par), yleft = min(em$par))$y
    ## Lambda0 <- sapply(1:length(tij), function(x) em$par[max(which(tij[x] >= ordt))])
    Lambda0
}


lam.loglik <- function(f, ordt, tij, id, mt, aijk, bik) {
    f <- ifelse(f < 0, 0, f)
    ki <- unlist(lapply(split(id, id), length))
    if (sum(id %in% which(ki == 1)) > 0) {
        ind <- id[-which(id %in% names(ki)[which(ki == 1)])]
        mti <- mt[-which(id %in% names(ki)[which(ki == 1)])]
        tiji <- tij[-which(id %in% names(ki)[which(ki == 1)])]
    } else {
        ind <- id
        mti <- mt
        tiji <- tij
    }
    ## Lambda0 <- sapply(1:length(tiji), function(x) f[max(which(tiji[x] >= ordt))])
    Lambda0 <- approx(ordt, f, tiji, yleft = min(f), yright = max(f))$y
    -sum(mti * log(1 + unlist(lapply(split(Lambda0, ind), function(x) diff(c(0,x)) / max(x)))))
}


getBtInf <- function(X, tij, mt, cluster, id, a, B, lamSm, conv.tol = 1e-2, method = "bootstrap", adjust = 0.02) {
    covmat <- matrix(NA, ncol = ncol(X), nrow = B)
    Lyi <- rep(NA, B)
    lami <- matrix(NA, ncol = length(tij), nrow = B)
    mi <- unlist(lapply(split(mt, id), sum))
    sT <- log(tij) + X %*% a
    yi <- unlist(lapply(split(exp(sT), id), max))
    for (k in 1:B) {
        mti <- Xi <- clusteri <- tiji <- idi <- NULL
        smp <- sample(unique(id), replace = TRUE)
        if (method == "bootstrap") {
            btId <- unlist(sapply(smp, function(x) which(id == x)))
            Xi <- X[btId,]
            clusteri <- cluster[btId]
            tiji <- tij[btId]
            mti <- mt[btId]
            idi <- rep(1:length(smp), unlist(sapply(smp, function(x) sum(id == x))))
            covmat[k,] <-  dfsane(a, pcEq, X = Xi, tij = tiji, mt = mti, cluster = clusteri, id = idi,
                                  conv.tol = conv.tol, alertConvergence = FALSE, quiet = TRUE,
                                  control = list(NM = FALSE, M = 100, noimp = 50, trace = FALSE,
                                                 tol = conv.tol))$par
            if (covmat[k,] %*% covmat[k,] > 1e4)
                covmat[k, ] <- BBoptim(a * 0, fn = function(x)
                    sum(pcEq(x, X = Xi, tij = tiji, mt = mti, cluster = clusteri, id = idi,
                             conv.tol = conv.tol)^2), control = list(trace = FALSE), quiet = TRUE)$par
        }        
        if (method == "smooth") {
            j <- 1
            for (i in smp) { ## prepare bootstrap data
                tmp <- rpois(1, mi[i])
                if (tmp > 0) {
                    probs <- diff(c(0, approx(lamSm$x, lamSm$y/max(lamSm$y), split(exp(sT), id)[[i]], yleft = 0, yright = 1)$y))
                    if (all(probs >= 0) & !all(probs == 0)) mti <- c(mti, c(rmultinom(1, size = tmp, prob = probs)))
                    else mti <- c(mti, c(rmultinom(1, size = tmp, prob = rep(1 / length(probs), length(probs)))))
                    ## ytmp <- sort(approx(lamSm$y, lamSm$x, runif(mi[i], max = approx(lamSm$x, lamSm$y, yi[i])$y))$y)
                    ## mti <- c(mti, diff(c(0, sapply(ttmp, function(x) max(0, which(x > ytmp))))))
                } else {
                    mti <- c(mti, rep(0, sum(id == i)))            
                }
                Xi <- rbind(Xi, X[which(id == i), ])
                clusteri <- c(clusteri, cluster[which(id == i)])
                tiji <- c(tiji, tij[which(id == i)])
                idi <- c(idi, rep(j, sum(id == i)))
                j <- j + 1
            }
            covmat[k, ] <-  dfsane(a, pcEq, X = Xi, tij = tiji, mt = mti, cluster = clusteri, id = idi,
                                   conv.tol = conv.tol, alertConvergence = FALSE, quiet = TRUE,
                                   control = list(NM = FALSE, M = 100, noimp = 50, trace = FALSE,
                                                  tol = conv.tol))$par
            if (covmat[k,] %*% covmat[k,] > 1e4)
                covmat[k, ] <- BBoptim(a * 0, fn = function(x)
                    sum(pcEq(x, X = Xi, tij = tiji, mt = mti, cluster = clusteri, id = idi,
                             conv.tol = conv.tol)^2), control = list(trace = FALSE), quiet = TRUE)$par
        } 
        if (method == "smooth2") {
            j <- 1
            for (i in smp) { ## prepare bootstrap data
                if (mi[i] > 0) {
                    probs <- diff(c(0, approx(lamSm$x, lamSm$y/max(lamSm$y), split(exp(sT), id)[[i]], yleft = 0, yright = 1)$y))
                    if (all(probs >= 0) & !all(probs == 0)) mti <- c(mti, c(rmultinom(1, size = tmp, prob = probs)))
                    else mti <- c(mti, c(rmultinom(1, size = tmp, prob = rep(1 / length(probs), length(probs)))))
                    ## ytmp <- sort(approx(lamSm$y, lamSm$x, runif(mi[i], max = approx(lamSm$x, lamSm$y, yi[i])$y))$y)
                    ## mti <- c(mti, diff(c(0, sapply(ttmp, function(x) max(0, which(x > ytmp))))))
                } else {
                    mti <- c(mti, rep(0, sum(id == i)))            
                }
                Xi <- rbind(Xi, X[which(id == i), ])
                clusteri <- c(clusteri, cluster[which(id == i)])
                tiji <- c(tiji, tij[which(id == i)])
                idi <- c(idi, rep(j, sum(id == i)))
                j <- j + 1
            }
            covmat[k, ] <-  dfsane(a, pcEq, X = Xi, tij = tiji, mt = mti, cluster = clusteri, id = idi,
                                   conv.tol = conv.tol, alertConvergence = FALSE, quiet = TRUE,
                                   control = list(NM = FALSE, M = 100, noimp = 50, trace = FALSE,
                                                  tol = conv.tol))$par
            if (covmat[k,] %*% covmat[k,] > 1e4)
                covmat[k, ] <- BBoptim(a * 0, fn = function(x)
                    sum(pcEq(x, X = Xi, tij = tiji, mt = mti, cluster = clusteri, id = idi,
                             conv.tol = conv.tol)^2), control = list(trace = FALSE), quiet = TRUE)$par
        } 
        sTi <- log(tiji) + Xi %*% covmat[k,]
        ## if (max(exp(sTi)) == Inf) {next}
        if (max(abs(exp(sTi))) == Inf || all(exp(sT) == 0))
            Fti <- getLam(sTi, clusteri, idi, mti, conv.tol = conv.tol)
        else Fti <- getLam(exp(sTi), clusteri, idi, mti, conv.tol = conv.tol)
        if (max(Fti) > 1e2)
            Fti <- getLam(sTi, clusteri, idi, mti, conv.tol = 0.01)
        mii <- unlist(lapply(split(mti, idi), sum))
        Lyi[k] <- mean((mii + adjust) / (Fti[cumsum(unlist(lapply(split(tiji, idi), which.max)))] + adjust),
                       na.rm = TRUE)
        lami[k,] <- approx(exp(sTi), Fti, exp(sT), method = "constant", yleft = 0, yright = 1)$y
    }
    list(covmat = covmat, Lyi = Lyi, lami = lami)
}

