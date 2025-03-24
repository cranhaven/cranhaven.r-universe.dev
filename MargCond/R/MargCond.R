MargCond <-
  function(formula, data, ID, tol = 1e-4, max.iter = 50, corstr = 'independence', silent = F){
    Call <- match.call()
    ID <- ID[complete.cases(data)]
    data <- data[complete.cases(data), ]
    outcomes  <- with(data, eval(parse(text = as.character(formula[2]))))
    out.names <- strsplit(substr(as.character(formula[2]), 3, nchar(as.character(formula[2])) - 1), ', ')[[1]]
    lDat <- data.frame(outcomes)
    for(i in names(data)[!names(data) %in% out.names]) lDat[, i] <- rep(data[, i], length(out.names))
    lDat$ID <- as.character(rep(ID, length(out.names)))
    lDat$var <- rep(out.names, each = nrow(lDat) / length(out.names))
    out.names <- sort(out.names)
    lDat <- lDat[order(lDat$var), ]
    sigmas <- rep(NA, length(out.names))
    Ds <- betas <- Xs <- Zs <- list()
    for(j in 1:length(out.names)){
      tMod <- lmer(as.formula(paste(out.names[j], '~', formula[3])), data = data)
      sigmas[j]  <- getME(tMod, 'sigma')
      Ds[[j]]    <- bdiag(VarCorr(tMod)) / (sigmas[j] ^ 2)
      betas[[j]] <- getME(tMod, 'beta') / sigmas[j]
      Xs[[j]] <- getME(tMod, 'X')
      Zs[[j]] <- do.call(cbind, getME(tMod, 'mmList'))
      if(any(duplicated(colnames(Zs[[j]])))) stop("Redundant random effects structure.")
    }
    lDat$scale_Y <- lDat$outcomes / rep(sigmas, each = nrow(data))
    pp <- ncol(Xs[[1]])
    qq <- ncol(Zs[[1]])
    D <- as.matrix(bdiag(Ds)) 
    Z <- as.matrix(bdiag(Zs)) 
    X <- as.matrix(bdiag(Xs))
    B <- unlist(betas)
    D_fix <- D == 0
    diag(D_fix) <- FALSE
    D_fix_vec <- D_fix
    D_fix_vec[upper.tri(D_fix_vec)] <- TRUE
    D_fix_vec <- c(D_fix_vec)
    D[D_fix] <- 0
    theta_new <- list(unlist(B), sigmas, diag(length(out.names)), D)
    E_bi. <- list()
    Var_bi. <- list()
    gf <- formula(paste('aug_Y ~ 0 + var + (', strsplit(as.character(formula[3]), ' + (', fixed = T)[[1]][1], '):var', sep = ''))
    lDat$clust <- interaction(lDat$times, lDat$ID)
    lDat$aug_Y <- lDat$re <- rep(NA, nrow(lDat))
    ord1 <- order(lDat$clust)
    conv_check <- 1
    max.iter_check <- 1
    conv_theta <- matrix(NA, nrow = max.iter + 1, ncol = length(c(theta_new[[1]], theta_new[[2]], theta_new[[4]][!D_fix_vec])))
    while(conv_check > tol){
      theta_old <- theta_new
      if(conv_check == 1){
        cc <- list()
        cc$working.correlation = diag(length(out.names))
      }
      lDat$scale_Y <- lDat$outcomes / rep(sigmas, each = nrow(data))
      E_bi. <- Var_bi. <- list()
      for(i in 1:length(unique(lDat$ID))){
        i_id <- which(lDat$ID == unique(lDat$ID)[i])
        R_i <- expandR(cc$working.correlation, length(lDat$scale_Y[i_id]) / length(out.names))
        E_bi.[[i]] <- Ebi(D, Z[i_id, ], R_i, lDat$scale_Y[i_id], X[i_id, ], B)
        Var_bi.[[i]] <- Vbi(D, Z[i_id, ], R_i)
        lDat$aug_Y[i_id] <- lDat$scale_Y[i_id] - Z[i_id, ] %*% E_bi.[[i]]
        lDat$re[i_id] <- Z[i_id, ] %*% E_bi.[[i]]
      }
      tlDat <- lDat[order(lDat$clust), ]
      ord2 <- order(interaction(tlDat$times, as.numeric(tlDat$ID), tlDat$var))
      sink(tempfile())
      cc <- suppressMessages(gee(gf, id = tlDat$clust, data = tlDat,
                                 corstr = corstr, b = B))
      sink()
      lDat$fe <- predict(cc)[ord2]
      bList <- rep(out.names, pp); B <- NULL
      for(j in 1:length(out.names)) B <- c(B, coef(cc)[bList == out.names[j]])
      for(j in 1:length(out.names)){
        a <- nrow(lDat[lDat$var == out.names[j], ])
        b <- with(lDat[lDat$var == out.names[j], ], sum(outcomes * (fe + re)))
        c <- -with(lDat[lDat$var == out.names[j], ], sum(outcomes ^ 2))
        sigmas[j] <- max((-b + c(-1, 1) * sqrt(b ^ 2 - 4 * a * c)) / (2 * a))
      }
      Ebibi <- list()
      Ebij.bij <- matrix(0, nrow = nrow(Var_bi.[[1]]), ncol = ncol(Var_bi.[[1]]))
      for(i in 1:length(unique(ID))){
        Ebibi[[i]] <- Var_bi.[[i]] + E_bi.[[i]] %*% t(E_bi.[[i]])
        Ebij.bij <- Ebij.bij + Ebibi[[i]]
      }
      D <- Ebij.bij / length(unique(lDat$ID))
      D[D_fix] <- 0
      D[lower.tri(D)] <- t(D)[lower.tri(D)]
      theta_new <- list(B, sigmas, cc$working.correlation, D)
      conv_theta[max.iter_check, ] <- c(theta_new[[1]], theta_new[[2]], theta_new[[4]][!D_fix_vec])
      max.iter_check <- max.iter_check + 1
      conv_check <- max(abs(unlist(theta_old) - unlist(theta_new)))
      if(max.iter_check > max.iter) conv_check <- 0
      if(!silent) cat('.')
    }
    if(max.iter_check < max.iter) cat('Converged.\n') else{cat('Iteration limit reached without convergeance.\n')}
    conv_theta <- conv_theta[1:(max.iter_check - 1), ]
    theta_new[[4]][upper.tri(theta_new[[4]]) | lower.tri(theta_new[[4]])] <- cov2cor(theta_new[[4]])[upper.tri(theta_new[[4]]) | lower.tri(theta_new[[4]])]
    diag(theta_new[[4]]) <- sqrt(diag(theta_new[[4]]))
    theta_old[[4]][upper.tri(theta_old[[4]]) | lower.tri(theta_old[[4]])] <- cov2cor(theta_old[[4]])[upper.tri(theta_old[[4]]) | lower.tri(theta_old[[4]])]
    diag(theta_old[[4]]) <- sqrt(diag(theta_old[[4]]))
    theta_new_vec <- c(theta_new[[1]], theta_new[[2]], theta_new[[4]][!D_fix_vec])
    theta_old_vec <- c(theta_old[[1]], theta_old[[2]], theta_old[[4]][!D_fix_vec])
    U_prime <- U_i2 <- matrix(0, nrow = length(theta_new_vec), ncol = length(theta_new_vec))
    nBeta <- length(theta_new[[1]])
    nSig  <- length(theta_new[[2]])
    for(i in 1:length(unique(lDat$ID))){
      U_i_old   <- getUi(unique(lDat$ID)[i], theta = theta_old, ldat = lDat, outnames = out.names, Z. = Z, X. = X, Dfix = D_fix_vec)
      U_i2    <- U_i2 + U_i_old %*% t(U_i_old)
      for(j in 1:length(U_i_old)){
        theta_temp <- theta_old_vec
        theta_temp[j] <- theta_new_vec[j]
        U_i_new <- getUi(unique(lDat$ID)[i], theta = reForm(theta_temp, nB = nBeta, nS = nSig, cc. = cc, theta = theta_new, Dfix = D_fix_vec), 
                         ldat = lDat, outnames = out.names, Z. = Z, X. = X, Dfix = D_fix_vec)
        for(h in 1:length(U_i_new)){
          U_prime[h, j]  <- U_prime[h, j] + (U_i_new - U_i_old)[h] / (theta_new_vec[j] - theta_old_vec[j])
        }
      }
    }
    v_theta  <- solve(U_prime) %*% U_i2 %*% solve(t(U_prime))
    varRC <- (nBeta + 1):(nBeta + nSig)
    bSig <- rep(varRC, each = length(theta_new[[1]]) / length(out.names))
    SE <- NULL
    B_scaled <- NULL
    for(k in 1:nBeta){
      SE <- c(SE, reScale(k, bSig[k], est = theta_new_vec, vc = v_theta))
      B_scaled <- c(B_scaled, B[k] * theta_new_vec[bSig[k]] + v_theta[k, bSig[k]])
    }
    SE <- c(SE, diag(v_theta)[varRC])
    dSig <- rep(varRC, each = length((nBeta + nSig + 1):(length(theta_new_vec))) / length(out.names))
    kk <- 1
    RE_scaled <- NULL
    for(k in (nBeta + nSig + 1):(length(theta_new_vec))){
      if(theta_new_vec[k] %in% diag(theta_new[[4]])){
        SE <- c(SE, reScale(k, dSig[kk], est = theta_new_vec, vc = v_theta))
      } else {
        SE <- c(SE, diag(v_theta)[k])
      }
      RE_scaled <- c(RE_scaled, theta_new_vec[k] * theta_new_vec[dSig[kk]] + v_theta[k, dSig[kk]])
      kk <- kk + 1
    }
    tD <- matrix(0, nrow = nrow(D), ncol = ncol(D))
    tD[!D_fix_vec] <- RE_scaled
    RE_scaled <- diag(tD)
    Y_new <- lDat$outcomes / rep(sigmas, each = nrow(data))
    Y_aug_new <- rep(NA, length(Y_new))
    for(i in 1:length(unique(lDat$ID))){
      i_id <- which(lDat$ID == unique(lDat$ID)[i])
      Y_aug_new[i_id] <- Y_new[i_id] - Z[i_id, ] %*% E_bi.[[i]] - X[i_id, ] %*% theta_new[[1]]
    }
    fit <- list()
    fit$coefficients <- B_scaled
    fit$sigma <- sigmas; names(fit$sigma) <- unique(lDat$var)
    filler <- rep(NA, nrow(lDat))
    for(i in 1:length(filler)){
      filler[i] <- fit$sigma[which(names(fit$sigma) == lDat$var[i])]
    }
    fit$SE <- sqrt(SE)
    fit$residuals <- Y_aug_new * filler
    fit$working.correlation  <- cc$working.correlation
    fit$rand.eff <- diag(RE_scaled) %*% cov2cor(D) %*% diag(RE_scaled)
    fit$outcomes <- unique(lDat$var)
    fit$Call <- Call
    fit$v.cov <- v_theta
    rownames(fit$vcov) <- colnames(fit$vcov)
    fit$obs <- nrow(data)
    fit$groups <- length(unique(data$ID))
    fit$converge <- max.iter_check < max.iter
    class(fit) <- "MargCond"
    return(fit)
  }
summary.MargCond <-
  function(object, ...){
    object$coefficients <- cbind(Estimate  = object$coefficients, 
                                 Std.Error = object$SE[1:length(object$coefficients)],
                                 'z value' = object$coefficients / object$SE[1:length(object$coefficients)],
                                 'Pr(>|z|)' = 2 * pnorm(abs(object$coefficients / object$SE[1:length(object$coefficients)]), lower.tail = F))
    class(object) <- 'summary.MargCond'
    return(object)
  }
print.summary.MargCond <-
  function(x, digits = max(3, getOption("digits") - 3), ...){
    cat("\nJoint Marginal-Conditional Model\n")
    cat("  Model:", deparse(x$Call$formula), "\n")
    cat("  Data:", deparse(x$Call$data), "\n")
    cat("\nFixed effects:", deparse(x$Call$formula[[3]]), "\n")
    print(coef(x), digits = digits, ...)
    cat("\n")
    cat("\nEstimated working correlation matrix:\n")
    print(x$working.correlation, digits = digits, ...)
    cat("\nNumber of Observations:", x$obs)
    cat("\nNumber of Groups: ", x$groups)
    cat("\n\n")
  }
print.MargCond <-
  function(x, digits = max(3, getOption("digits") - 3), ...){
    cat("\nJoint Marginal-Conditional Model\n")
    cat("  Model:", deparse(x$Call$formula), "\n")
    cat("  Data:", deparse(x$Call$data), "\n")
    cat("\nFixed Effects\n")
    print(coef(x), digits = digits, ...)
    cat("\n")
    cat("\nNumber of Observations:", x$obs)
    cat("\nNumber of Groups: ", x$groups)
    cat("\n\n")
  }