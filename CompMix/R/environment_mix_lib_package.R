


#'@import mvtnorm
#'@import glmnet
#'@import gglasso
#'@import higlasso
#'@import hierNet
#'@import glmnet
#'@import SuperLearner
#'@import randomForest
#'@import bkmr
#'@import qgcomp
#'@import gWQS
#'@import pROC
#'@import splines
#'@import Matrix
#'@importFrom stats as.formula cor gaussian kmeans optimize predict rbinom rnorm runif
#'@importFrom utils capture.output



#'@title Simulate covariate matrix with block structure
#'@param n a positive integer to indicate sample size
#'@param p a positive integer to specify the number of covariates
#'@param block_idx a vector of positive integers to indicate the block IDs.
#'The length of the vector is p.
#'@param sigma2_x a positive numeric scalar for variance of the covariates
#'@param within_rho a numeric scalar between 0 and 1 for the within block correlation
#'@param btw_rho a numeric scalar between 0 and 1 for the between block correlation
#'@return a list object of the following
#'\describe{
#'\item{x}{covariate matrix of dimension n by p}
#'\item{n}{sample size}
#'\item{p}{number of covariates}
#'\item{sigma2_x}{variance}
#'\item{within_rho}{within block correlation}
#'\item{btw_rho}{between block correaltion}
#'\item{block_idx}{block indices}
#'}
#'@author Wei Hao <weihao@umich.edu>
#'@examples
#'dat <- simul_x_block(n = 1000, p = 10, block_idx = rep(1:4,length=10))
#'@export
simul_x_block = function(n,
                         p,
                         block_idx,
                         sigma2_x = 1.0,
                         within_rho = 0.6,
                         btw_rho = 0.2)
{
  num_blocks = max(block_idx)
  Sigma = matrix(btw_rho, nrow = p, ncol = p)
  for (block in 1:num_blocks) {
    idx = which(block_idx == block)
    Sigma[idx, idx] = matrix(within_rho, nrow = length(idx), ncol = length(idx)) +
      (1 - within_rho) * diag(length(idx))
  }
  x = mvtnorm::rmvnorm(n, sigma = Sigma * sigma2_x)
  return(
    list(
      x = x,
      n = n,
      p = p,
      sigma2_x = sigma2_x,
      btw_rho = btw_rho,
      within_rho = within_rho,
      block_idx = block_idx
    )
  )
}


create_inter_idx_nm <- function(dat) {
  nm = names(dat$x)
  idxset = expand.grid(1:ncol(dat$x), 1:ncol(dat$x))
  subidx = which(idxset[, 1] < idxset[, 2])
  idx1 = idxset[subidx, 1]
  idx2 = idxset[subidx, 2]
  dat$idx1 = idx1
  dat$idx2 = idx2
  group_idx1 = dat$block_idx[idx1]
  group_idx2 = dat$block_idx[idx2]
  uniq_block = unique(dat$block_idx)
  blockgroup = expand.grid(uniq_block, uniq_block)
  block_subidx = which(blockgroup[, 1] < blockgroup[, 2])
  blockgroup_idx1 = blockgroup[block_subidx, 1]
  blockgroup_idx2 = blockgroup[block_subidx, 2]
  diff_group_idx = which(group_idx1 < group_idx2)
  inter_block_idx = group_idx1

  for (k in 1:length(diff_group_idx))
    inter_block_idx[diff_group_idx[k]] = length(uniq_block) + which(blockgroup_idx1 ==
                                                                      group_idx1[diff_group_idx[k]] &
                                                                      blockgroup_idx2 == group_idx2[diff_group_idx[k]])

  dat$inter_block_idx = c(dat$block_idx, inter_block_idx)
  dat$group_idx1 = group_idx1
  dat$group_idx2 = group_idx2
  dat$xi = cbind(dat$x, dat$x[, dat$idx1] * dat$x[, dat$idx2])
  inter_nm = paste0(nm[dat$idx1], nm[dat$idx2], collapse = "_")
  names(dat$xi) = c(nm, inter_nm)
  return(dat)
}

simul_y_lmi = function(dat,
                       trueBeta,
                       R2 = 0.6,
                       sigmaY = NULL) {
  n = nrow(dat$x)
  #dat$xi = cbind(dat$x,dat$x[,dat$idx1]*dat$x[,dat$idx2])
  eta = dat$xi %*% trueBeta
  if (is.null(sigmaY)) {
    sigmaY = sqrt(var(eta) * (1 - R2) / R2)
  }
  else{
    R2 = var(eta)
    R2 = R2 / (R2 + sigmaY ^ 2)
  }

  dat$y = eta + rnorm(nrow(dat$x), sd = sigmaY)
  dat$sigmaY = sigmaY
  dat$trueBeta = trueBeta
  dat$R2 = R2

  dat$z1 = rbinom(n, 1, 0.5)
  #dat$z1 = runif(n, 0, 1)
  dat$z2 = runif(n, 30, 45)
  dat$z = cbind(dat$z1, dat$z2)
  dat$z = as.matrix(dat$z)
  colnames(dat$z) = c("sex", "age")
  colnames(dat$x) = paste0("exposure.", 1:20)
  colnames(dat$y) = c("outcome")
  return(dat)
}


#'Simulate data from linear model with interactions
#'@param n a positive integer to indicate sample size
#'@param p a positive integer to specify the number of exposures
#'@param q a positive integer to specify the number of non-zero effects
#'@param block_idx a vector of positive integers to indicate the block IDs.
#'The length of the vector is p.
#'@param sigma2_x a positive numeric scalar for variance of the covariates
#'@param within_rho a numeric scalar between 0 and 1 for the within block correlation
#'@param btw_rho a numeric scalar between 0 and 1 for the between block correlation
#'@param R2 a numeric scalar for R-squared
#'@param effect_size a numeric scalar for effect size for main effect
#'@param effect_size_i a numeric scalar for effect size for interaction effect
#'@param cancel_effect a logic value to indicate whether there is effect cancelation
#'@return a list object of the following
#'\describe{
#'\item{x}{covariate matrix of dimension n by p}
#'\item{n}{sample size}
#'\item{p}{number of covariates}
#'\item{sigma2_x}{variance}
#'\item{within_rho}{within block correlation}
#'\item{btw_rho}{between block correaltion}
#'\item{block_idx}{block indices}
#'}
#'@author Wei Hao <weihao@umich.edu>
#'
#'@export
lmi_simul_dat <-
  function(n,
           p,
           q,
           block_idx = c(1, 1, 2, 2, 3, 1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3),
           sigma2_x = 1.0,
           within_rho = 0.6,
           btw_rho = 0.2,
           R2 = 0.8,
           effect_size = 1,
           effect_size_i = 1,
           cancel_effect = TRUE) {
    dat <- simul_x_block(
      n = n,
      p = p,
      block_idx,
      sigma2_x = sigma2_x,
      within_rho = within_rho,
      btw_rho = btw_rho
    )
    dat <- create_inter_idx_nm(dat)

    if (cancel_effect)
      beta <-
      c(rep(c(-effect_size, effect_size), length = q), rep(0, p -
                                                             q))
    else
      beta <- c(rep(effect_size, length = q), rep(0, p - q))

    subidx <- which(dat$idx1 <= q & dat$idx2 <= q)
    betai <- rep(0, length = p * (p - 1) / 2)
    betai[subidx] = effect_size_i
    beta <- c(beta, betai)

    dat <- simul_y_lmi(dat, trueBeta = beta, R2 = R2)
    return(dat)
  }



##########################################################################
########variable selection methods########################################
##########################################################################

#lasso
simple_lasso <- function(x, y, ...) {
  elapsed <- proc.time()[3]
  cv_res <- cv.glmnet(x, y, alpha = 1, ...)
  res <- glmnet(x, y, alpha = 1, lambda = cv_res$lambda.min, ...)
  elapsed <- proc.time()[3] - elapsed
  return(
    list(
      betaest = res$beta,
      select_idx = which(res$beta != 0),
      lambda = cv_res$lambda.1se,
      lasso_fit = res,
      elapsed = elapsed
    )
  )
}

#lasso
simple_lasso_real <- function(x, y, ...) {
  elapsed <- proc.time()[3]
  cv_res <- cv.glmnet(x, y, alpha = 1, ...)
  res <- glmnet(x, y, alpha = 1, lambda = cv_res$lambda.min, ...)
  elapsed <- proc.time()[3] - elapsed
  return(
    list(
      betaest = res$beta,
      select_idx = which(res$beta != 0),
      lambda = cv_res$lambda.min,
      lasso_fit = res,
      elapsed = elapsed
    )
  )
}

#lasso forcein z
simple_lasso_real_force <- function(x, y, z, ...) {
  xz <- cbind(x, z)
  pvec <- c(rep(1, length = ncol(x)), rep(0, length = ncol(z)))
  cv_res <- cv.glmnet(xz, y, alpha = 1, penalty.factor = pvec, ...)
  res <-
    glmnet(
      xz,
      y,
      alpha = 1,
      lambda = cv_res$lambda.min,
      penalty.factor = pvec,
      ...
    )
  return(
    list(
      betaest = res$beta[1:ncol(x),1,drop=FALSE],
      select_idx = which(res$beta[1:ncol(x)] != 0),
      lambda = cv_res$lambda.min,
      z_betaest = res$beta[ncol(x) + 1:ncol(z),1,drop=FALSE],
      lasso_fit = res
    )
  )
}

#enet
simple_enet <- function(x, y, alpha = 0.5, ...) {
  elapsed <- proc.time()[3]
  cv_res <- cv.glmnet(x, y, alpha = alpha, ...)
  res <-
    glmnet(x, y, alpha = alpha, lambda = cv_res$lambda.min, ...)
  elapsed <- proc.time()[3] - elapsed
  return(
    list(
      betaest = res$beta,
      select_idx = which(res$beta != 0),
      lambda = cv_res$lambda.1se,
      enet_fit = res,
      elapsed = elapsed
    )
  )
}

#enet
simple_enet_real <- function(x, y, alpha = 0.5, ...) {
  cv_res <- cv.glmnet(x, y, alpha = alpha, ...)
  res <-
    glmnet(x, y, alpha = alpha, lambda = cv_res$lambda.min, ...)
  return(
    list(
      betaest = res$beta,
      select_idx = which(res$beta != 0),
      lambda = cv_res$lambda.min,
      enet_fit = res
    )
  )
}

#enet forcein z
simple_enet_real_force <- function(x, y, z, alpha = 0.5, ...) {
  xz <- cbind(x, z)
  pvec <- c(rep(1, length = ncol(x)), rep(0, length = ncol(z)))
  cv_res <-
    cv.glmnet(xz, y, alpha = alpha, penalty.factor = pvec, ...)
  res <-
    glmnet(
      xz,
      y,
      alpha = alpha,
      lambda = cv_res$lambda.min,
      penalty.factor = pvec,
      ...
    )
  return(
    list(
      betaest = res$beta[1:ncol(x),1,drop=FALSE],
      select_idx = which(res$beta[1:ncol(x)] != 0),
      lambda = cv_res$lambda.min,
      z_betaest = res$beta[ncol(x) + 1:ncol(z)],
      enet_fit = res
    )
  )
}


#bkmr
simple_bkmr <- function(x, y,pip, varsel, iter = 2000, ...) {
  elapsed <- proc.time()[3]
  kmsel = kmbayes(
    y = y,
    Z = x,
    verbose = FALSE,
    varsel = varsel,
    iter = iter,
    ...
  )
  if (varsel == TRUE) {
    PIP = ExtractPIPs(kmsel)
    betaest = ifelse(PIP[, 2] >= pip, 1, 0)
    select_idx = which(betaest == 1)
    if (length(select_idx) > 0) {
      kmsel = kmbayes(
        y = y,
        Z = x[, select_idx],
        verbose = FALSE,
        varsel = FALSE,
        iter = iter,
        ...
      )
    } else{
      warning("bkmr did not select any variables!")
    }
  } else{
    betaest = rep(1, length = ncol(x))
    select_idx = which(betaest == 1)
  }
  elapsed <- proc.time()[3] - elapsed
  return(list(
    betaest = betaest,
    select_idx = select_idx,
    kmsel = kmsel,
    elapsed = elapsed
  ))
}

simple_bkmr_real <-
  function(x, y, z, pip, varsel, iter = 5000, ...) {
    kmsel = kmbayes(
      y = y,
      Z = x,
      X = z,
      verbose = FALSE,
      varsel = varsel,
      iter = iter,
      ...
    )
    if (varsel == TRUE) {
      PIP = ExtractPIPs(kmsel)
      betaest = ifelse(PIP[, 2] >= pip, 1, 0)
      select_idx = which(betaest == 1)
      if (length(select_idx) > 0) {
        kmsel = kmbayes(
          y = y,
          Z = x[, select_idx, drop = FALSE],
          X = z,
          verbose = FALSE,
          varsel = FALSE,
          iter = iter,
          ...
        )
      } else{
        warning("bkmr did not select any variables!")
      }
    } else{
      betaest = rep(1, length = ncol(x))
      select_idx = which(betaest == 1)
    }
    return(list(
      betaest = betaest,
      select_idx = select_idx,
      kmsel = kmsel,
      PIP = PIP
    ))
  }



#super learner
simple_sl <- function(x,
                      y,
                      family = gaussian(),
                      SL.library = c("SL.randomForest",
                                     "SL.gam", "SL.glmnet", "SL.xgboost"),
                      ...) {
  sl_fit <-
    SuperLearner(
      Y = as.numeric(y),
      X = as.data.frame(x),
      verbose = FALSE,
      family = family,
      SL.library = SL.library,
      ...
    )

  return(list(sl_fit = sl_fit))
}


#random forest
simple_randomforest <-
  function(x,
           y,
           binary = FALSE,
           importance = TRUE,
           proximity = TRUE,
           ...) {
    elapsed <- proc.time()[3]
    if (binary == FALSE) {
      rf_fit <-
        randomForest(x,
                     as.numeric(y),
                     importance = TRUE,
                     proximity = TRUE,
                     ...)
    } else{
      rf_fit <-
        randomForest(x,
                     factor(y, levels = c(0, 1)),
                     importance = TRUE,
                     proximity = TRUE,
                     ...)
    }
    kmeans_res <- kmeans(rf_fit$importance[, 1], centers = 2)
    id <- which.max(kmeans_res$centers)
    select_idx <- which(kmeans_res$cluster == id)
    betaest <- rep(0, length = ncol(x))
    betaest[select_idx] = 1
    elapsed <- proc.time()[3] - elapsed
    return(list(
      betaest = betaest,
      select_idx = select_idx,
      rf_fit = rf_fit,
      elapsed = elapsed
    ))
  }

#random forest
simple_randomforest <-
  function(x,
           y,
           binary = FALSE,
           importance = TRUE,
           proximity = TRUE,
           ...) {
    elapsed <- proc.time()[3]
    if (binary == FALSE) {
      rf_fit <-
        randomForest(x,
                     as.numeric(y),
                     importance = TRUE,
                     proximity = TRUE,
                     ...)
    } else{
      rf_fit <-
        randomForest(x,
                     factor(y, levels = c(0, 1)),
                     importance = TRUE,
                     proximity = TRUE,
                     ...)
    }
    kmeans_res <- kmeans(rf_fit$importance[, 1], centers = 2)
    id <- which.max(kmeans_res$centers)
    select_idx <- which(kmeans_res$cluster == id)
    betaest <- rep(0, length = ncol(x))
    betaest[select_idx] = 1
    elapsed <- proc.time()[3] - elapsed
    return(list(
      betaest = betaest,
      select_idx = select_idx,
      rf_fit = rf_fit,
      elapsed = elapsed
    ))
  }


#hierNet
simple_hiernet <- function(x, y, binary = FALSE, ...) {
  elapsed <- proc.time()[3]
  y = as.numeric(y)
  if (binary == FALSE) {
    capture.output(fit <- hierNet.path(x = x, y = y),file=nullfile())
    capture.output(fitcv <- hierNet.cv(fit, x = x, y = y),file=nullfile())
    lamhat = fitcv$lamhat
    capture.output(fit2 <- hierNet(x, y, lam = lamhat),file=nullfile())
  } else {
    capture.output(fit <- hierNet.logistic.path(x = x, y = y),file=nullfile())
    capture.output(fitcv <- hierNet.cv(fit, x = x, y = y),file=nullfile())
    lamhat = fitcv$lamhat
    capture.output(fit2 <- hierNet.logistic(x, y, lam = lamhat),file=nullfile())
  }
  #main effects
  main_effect = fit2$bp - fit2$bn  #main effects estimated from fit2
  main_select_idx = which(fit2$bp - fit2$bn != 0)

  #interaction effects
  inter_mat <- with(fit2, (th + t(th)) / 2)
  inter_effects <- inter_mat[upper.tri(inter_mat)]
  inter_idx = cbind(row(inter_mat)[upper.tri(inter_mat)], col(inter_mat)[upper.tri(inter_mat)])
  inter_select_idx = ncol(x) + which(inter_effects != 0)

  select_idx = c(main_select_idx, inter_select_idx)
  betaest <- rep(0, length = ncol(x) + length(inter_effects))
  betaest[select_idx] = 1
  elapsed <- proc.time()[3] - elapsed
  return(list(
    betaest = betaest,
    select_idx = select_idx,
    hiernet_fit = fit2,
    elapsed = elapsed
  ))
}


simple_hiernet_real <- function(x, y, binary = FALSE, ...) {
  y = as.numeric(y)
  if (binary == FALSE) {
    fit = hierNet.path(x = x, y = y)
    fitcv = hierNet.cv(fit, x = x, y = y)
    lamhat = fitcv$lamhat
    fit2 = hierNet(x, y, lam = lamhat)
  } else {
    fit = hierNet.logistic.path(x = x, y = y)
    fitcv = hierNet.cv(fit, x = x, y = y)
    lamhat = fitcv$lamhat
    fit2 = hierNet.logistic(x, y, lam = lamhat)
  }
  #main effects
  main_effect = fit2$bp - fit2$bn  #main effects estimated from fit2
  main_select_idx = which(fit2$bp - fit2$bn != 0)

  #interaction effects
  inter_mat <- with(fit2, (th + t(th)) / 2)
  inter_effects <- inter_mat[upper.tri(inter_mat)]
  inter_idx = cbind(row(inter_mat)[upper.tri(inter_mat)], col(inter_mat)[upper.tri(inter_mat)])
  inter_select_idx = ncol(x) + which(inter_effects != 0)

  select_idx = c(main_select_idx, inter_select_idx)
  betaest <- rep(0, length = ncol(x) + length(inter_effects))
  betaest[select_idx] = 1
  return(list(
    betaest = betaest,
    select_idx = select_idx,
    hiernet_fit = fit2
  ))
}



#snif
#simple_snif <- function(x, y, ...) {
#  elapsed <- proc.time()[3]
#  dat = data.frame(x = x, y = y)
#  names(dat) = c(paste0("x", 1:ncol(x)), "y")
#
#  inter_idx <- expand.grid(1:ncol(x), 1:ncol(x))
#  inter_idx <- inter_idx[inter_idx[, 1] < inter_idx[, 2],]
#  all_inter_nm <- paste0("x", inter_idx[, 1], ":x", inter_idx[, 2])
#
#  fit <- snif(formula = y ~ NULL, df = dat)
#  res <- summary(fit)
#  temp_idx <- which(res$summary$coefficients[, 4] < 0.05)
#  nm <- rownames(res$summary$coefficients)[temp_idx]
#  inter_idx <- grep(":", nm)
#  inter_nm <- nm[inter_idx]
#  if (length(inter_idx) > 0) {
#    main_nm = nm[-inter_idx]
#  } else {
#    main_nm = nm
#  }
#  varnm <- setdiff(names(dat), "y")
#  if (length(inter_idx) > 0) {
#    varlist <- strsplit(inter_nm, ":")
#
#    varidx <- lapply(1:length(varlist), function(i) {
#      idx <- which(is.element(varnm, varlist[[i]]))
#      for (k in 1:length(varnm)) {
#        s <- grep(varnm[k], varlist[[i]])
#        if (length(s) > 0) {
#          idx <- union(idx, k)
#        }
#      }
#      return(idx)
#    })
#
#
#    select_inter_nm <- sapply(1:length(varidx), function(i) {
#      paste0("x", sort(varidx[[i]]), collapse = ":")
#    })
#
#    inter_select_idx <-
#      which(is.element(all_inter_nm, unique(select_inter_nm)))
#  }
#  else{
#    inter_select_idx <- vector(mode = "integer")
#  }
#
#  if (length(main_nm) > 0) {
#    main_varidx <- sapply(1:length(main_nm), function(i) {
#      idx <- which(is.element(varnm, main_nm[i]))
#       for (k in 1:length(varnm)) {
#         s <- grep(varnm[k], main_nm[i])
#         if (length(s) > 0) {
#           idx <- union(idx, k)
#         }
#       }
#       return(idx)
#     })
#     main_varidx <- sort(unique(unlist(main_varidx)))
#   }
#   else{
#     main_varidx <- vector(mode = "integer")
#   }
#   select_idx = c(main_varidx, inter_select_idx + ncol(x))
#   betaest <- rep(0, length = ncol(x) + length(all_inter_nm))
#   betaest[select_idx] = 1
#   elapsed <- proc.time()[3] - elapsed
#   return(list(
#     betaest = betaest,
#     select_idx = select_idx,
#     snif_fit = fit,
#     elapsed = elapsed
#   ))
# }


simple_wqs = function(x,
                      y,
                      formula = as.formula("y~ wqs"),
                      family = "gaussian",
                      q = 10,
                      validation = 0.6,
                      b = 100,
                      b1_pos = TRUE,
                      b1_constr = FALSE,
                      ...)
{
  elapsed <- proc.time()[3]
  wqs_dat = data.frame(x, y)
  toxics <- names(wqs_dat)[1:ncol(x)]
  # we run the model and save the results in the variable "results"

  results <-
    gwqs(
      formula = formula,
      mix_name = toxics,
      data = wqs_dat,
      q = q,
      validation = validation,
      b = b,
      b1_pos = b1_pos,
      b1_constr = b1_constr,
      family = family,
      ...
    )
  elapsed <- proc.time()[3] - elapsed
  return(list(wqs_fit = results, elapsed = elapsed))
}


simple_wqs_int = function(x, y, p, family = "gaussian", ...)
{
  elapsed <- proc.time()[3]
  wqs_dat = data.frame(x, y)
  toxics <- names(wqs_dat)[1:p]
  int <- names(wqs_dat)[(p + 1):(ncol(wqs_dat) - 1)]
  y_name = names(wqs_dat)[ncol(wqs_dat)]
  # we run the model and save the results in the variable "results"

  fm = as.formula(paste0(paste0(y_name), "~wqs+", paste(int, collapse = "+")))

  # we run the model and save the results in the variable "results"
  results <- gwqs(
    fm,
    mix_name = toxics,
    data = wqs_dat,
    q = 10,
    validation = 0.6,
    b = 100,
    b1_pos = TRUE,
    b1_constr = FALSE,
    family = family,
    ...
  )

  elapsed <- proc.time()[3] - elapsed
  return(list(wqs_fit = results, elapsed = elapsed))
}

# #wqs+isage+edu_cat+child_gender+FINALGA_BEST
simple_wqs_real = function(x,
                           y,
                           z,
                           formula = NULL,
                           family = "gaussian",
                           q = 10,
                           validation = 0.6,
                           b = 100,
                           b1_pos = TRUE,
                           b1_constr = FALSE,
                           ...)
{
  wqs_dat = data.frame(x, y, z)
  toxics <- names(wqs_dat)[1:ncol(x)]
  conf <- names(wqs_dat)[(ncol(x) + 2):(ncol(wqs_dat))]
  y_name = names(wqs_dat)[ncol(x) + 1]
  if (is.null(formula)) {
    fm = as.formula(paste0(paste0(y_name), "~wqs+", paste(conf, collapse = "+")))
  } else{
    fm = formula
  }

  # we run the model and save the results in the variable "results"
  results <- gwqs(
    fm,
    mix_name = toxics,
    data = wqs_dat,
    q = q,
    validation = validation,
    b = b,
    b1_pos = b1_pos,
    b1_constr = FALSE,
    family = family,
    ...
  )
  return(list(wqs_fit = results, wqs_dat = wqs_dat))
}


simple_gcomp = function(x, y, family = "gaussian", ...)
{
  elapsed <- proc.time()[3]
  dat = data.frame(x, y)
  qc.fit <- qgcomp.noboot(y ~ ., data = dat, family = family, ...)
  elapsed <- proc.time()[3] - elapsed
  return(list(gcomp_fit = qc.fit, elapsed = elapsed))
}

simple_gcomp_int = function(x, y, family = "gaussian", ...)
{
  elapsed <- proc.time()[3]
  dat = data.frame(x, y)
  qc.fit <- qgcomp.noboot(y ~ ., data = dat, family = family, ...)
  elapsed <- proc.time()[3] - elapsed
  return(list(gcomp_fit = qc.fit, elapsed = elapsed))
}

simple_gcomp_real = function(x,
                             y,
                             z,
                             formula = NULL,
                             family = "gaussian",
                             expnms = NULL,
                             ...)
{
  dat = data.frame(x, y, z)
  conf <- names(dat)[(ncol(x) + 2):(ncol(dat))]

  if (is.null(formula)) {
    if (!is.null(colnames(y)))
      fm = as.formula(paste0(paste0(colnames(y), "~ . + "),paste(conf, collapse = "+")))
    else
      fm = as.formula(paste0("y~ . + ",paste(conf, collapse = "+")))
  } else{
    fm = formula
  }

  if (is.null(expnms))
    expnms = colnames(x)

  qc.fit <- qgcomp.noboot(fm,
                          data = dat,
                          expnms = expnms,
                          family = family,
                          ...)
  return(list(gcomp_fit = qc.fit))
}


###############identify the weights##################
lse_constr <- function(y,
                       X,
                       binary = FALSE,
                       max_iter = 1000,
                       tol = 1e-5) {
  p = ncol(X)
  yt  = y - X[, p]
  Xt  = X[,-p, drop = FALSE] - X[, p]

    obj_fun <- function(w, y_j, x_j) {
      return(sum((y_j - w * x_j) ^ 2))
    }
    obj_fun_binary <- function(w, y, x_j, xw) {
      eta <- w * x_j + xw
      return(sum(-y * eta + log1p(exp(eta))))
    }

  iter = 0
  w = rep(1 / p, length = p - 1)
  sum_w = sum(w)
  Xt_w = Xt %*% w
  eps = 1
  while ((eps > tol) && (iter < max_iter)) {
    w0 = w
    for (j in 1:(p - 1)) {
      if (binary == FALSE) {
        res_j <- optimize(
          obj_fun,
          lower = 0,
          upper = 1 - (sum_w - w[j]),
          y_j = yt - Xt_w + w[j] * Xt[, j],
          x_j = Xt[, j]
        )
      } else{
        res_j <- optimize(
          obj_fun_binary,
          lower = 0,
          upper = 1 - (sum_w - w[j]),
          y = yt,
          x_j = Xt[, j],
          xw =  Xt_w - w[j] * Xt[, j]
        )
      }
      sum_w <- sum_w - w[j]
      Xt_w <- Xt_w - w[j] * Xt[, j]
      w[j] <- res_j$minimum
      Xt_w <- Xt_w + w[j] * Xt[, j]
      sum_w <- sum_w + w[j]
    }
    iter <- iter + 1
    eps <- mean((w - w0) ^ 2)
  }
  return(list(
    w = c(w, 1 - sum_w),
    iter = iter,
    max_iter = max_iter,
    eps = eps
  ))
}


###################identify the weight from the 4 methods###########
super_4_combine = function(dat,
                           validation = 0.4,
                           n_rep_super = 5,
                           max_iter = 1000,
                           tol = 1e-5)
{
  ntrain = round(nrow(dat$xi) * (1 - validation))
  nval = nrow(dat$xi) - ntrain

  enet_pre = matrix(NA, nrow = nval, ncol = n_rep_super)
  bkmr_pre = matrix(NA, nrow = nval, ncol = n_rep_super)
#  snif_pre = matrix(NA, nrow = nval, ncol = n_rep_super)
  hiernet_pre = matrix(NA, nrow = nval, ncol = n_rep_super)
  y = matrix(NA, nrow = nval, ncol = n_rep_super)

  for (rp in 1:n_rep_super) {
    #split data into training and validation
    train_id = sort(sample(nrow(dat$xi), ntrain))
    train_dat = list(x = dat$x[train_id,],
                     xi = dat$xi[train_id,],
                     y = dat$y[train_id])
    val_dat = list(x = dat$x[-train_id,],
                   xi = dat$xi[-train_id,],
                   y = dat$y[-train_id])

    #obtain fits from 4 methods on training data

    enet_res = with(train_dat, simple_enet(xi, y))#1.enet

    bkmr_res = with(train_dat, simple_bkmr(x, y, varsel = TRUE))#2.bkmr

    hiernet_res = with(train_dat, simple_hiernet(x, y))#3.hiernet

    #snif_res = with(train_dat, simple_snif(x, y))#4.snif


    #obtain prediction from 4 methods on validation data
    enet_pre[, rp] = predict(enet_res$enet_fit, newx = val_dat$xi)
    bkmr_pre[, rp] = ComputePostmeanHnew(bkmr_res$kmsel, Znew = as.data.frame(val_dat$x[, bkmr_res$select_id]))$postmean
    hiernet_pre[, rp] = predict(hiernet_res$hiernet_fit, newx = val_dat$x)

    dat_val = data.frame(x = val_dat$x)
    names(dat_val) = c(paste0("x", 1:ncol(dat_val)))
    #snif_pre[, rp] = predict(snif_res$snif_fit, newdata = dat_val)
    y[, rp] = val_dat$y
  }

  pred_y = cbind(c(enet_pre), c(bkmr_pre), c(hiernet_pre))#, c(snif_pre))

  weight_res = lse_constr(
    y = c(y),
    X = pred_y,
    max_iter = max_iter,
    tol = 1e-5
  )
  return(weight_super = weight_res$w)
}


super_3_combine = function(dat,
                           validation = 0.4,
                           n_rep_super = 5,
                           max_iter = 1000,
                           tol = 1e-5)
{
  ntrain = round(nrow(dat$xi) * (1 - validation))
  nval = nrow(dat$xi) - ntrain

  enet_pre = matrix(NA, nrow = nval, ncol = n_rep_super)
  #snif_pre = matrix(NA, nrow = nval, ncol = n_rep_super)
  hiernet_pre = matrix(NA, nrow = nval, ncol = n_rep_super)
  y = matrix(NA, nrow = nval, ncol = n_rep_super)

  for (rp in 1:n_rep_super) {
    #split data into training and validation
    train_id = sort(sample(nrow(dat$xi), ntrain))
    train_dat = list(x = dat$x[train_id,],
                     xi = dat$xi[train_id,],
                     y = dat$y[train_id])
    val_dat = list(x = dat$x[-train_id,],
                   xi = dat$xi[-train_id,],
                   y = dat$y[-train_id])

    #obtain fits from 3 methods (remove bkmr due to computation) on training data

    enet_res = with(train_dat, simple_enet(xi, y))#1.enet

    hiernet_res = with(train_dat, simple_hiernet(x, y))#3.hiernet

    #snif_res = with(train_dat, simple_snif(x, y))#4.snif


    #obtain prediction from 3 methods on validation data
    enet_pre[, rp] = predict(enet_res$enet_fit, newx = val_dat$xi)
    hiernet_pre[, rp] = predict(hiernet_res$hiernet_fit, newx = val_dat$x)

    dat_val = data.frame(x = val_dat$x)
    names(dat_val) = c(paste0("x", 1:ncol(dat_val)))
    #snif_pre[, rp] = predict(snif_res$snif_fit, newdata = dat_val)
    y[, rp] = val_dat$y
  }

  pred_y = cbind(c(enet_pre), c(hiernet_pre))#, c(snif_pre))

  weight_res = lse_constr(
    y = c(y),
    X = pred_y,
    max_iter = max_iter,
    tol = 1e-5
  )
  return(weight_super = weight_res$w)
}


super_4_combine_real = function(dat,
                                validation = 0.4,
                                n_rep_super = 5,
                                max_iter = 1000,
                                tol = 1e-5,
                                verbose = TRUE)
{
  ntrain = round(nrow(dat$xi) * (1 - validation))
  nval = nrow(dat$xi) - ntrain

  enet_pre = matrix(NA, nrow = nval, ncol = n_rep_super)
  bkmr_pre = matrix(NA, nrow = nval, ncol = n_rep_super)
  #snif_pre = matrix(NA, nrow = nval, ncol = n_rep_super)
  hiernet_pre = matrix(NA, nrow = nval, ncol = n_rep_super)
  y = matrix(NA, nrow = nval, ncol = n_rep_super)

  for (rp in 1:n_rep_super) {
    #split data into training and validation
    train_id = sort(sample(nrow(dat$xi), ntrain))
    train_dat = list(x = dat$x[train_id,],
                     xi = dat$xi[train_id,],
                     y = dat$y[train_id])
    val_dat = list(x = dat$x[-train_id,],
                   xi = dat$xi[-train_id,],
                   y = dat$y[-train_id])

    train_dat_bkmr = list(
      x = dat$x[train_id, 1:39],
      y = dat$y[train_id],
      y_cat = dat$y_cat[train_id],
      z = dat$x[train_id, 40:43]
    )#40:43
    val_dat_bkmr = list(
      x = dat$x[-train_id, 1:39],
      y = dat$y[-train_id],
      y_cat = dat$y_cat[-train_id],
      z = dat$x[-train_id, 40:43]
    )#40:43

    # train_dat_snif=list(x=dat$x[train_id,c(1:40,43)],y=dat$y[train_id],y_cat=dat$y_cat[train_id])#remove ppsex edu_cat
    #val_dat_snif=list(x=dat$x[-train_id,c(1:40,43)],y=dat$y[-train_id],y_cat=dat$y_cat[-train_id])##remove ppsex edu_cat

    #obtain fits from 4 methods on training data

    enet_res = with(train_dat, simple_enet_real(xi, y))#1.enet
    if(verbose){
      cat("iter:", rp, "\n")
    }

    bkmr_res = with(train_dat_bkmr,
                    simple_bkmr_real(x, y, z, pip = 0.8, varsel = TRUE))#2.bkmr

    hiernet_res = with(train_dat, simple_hiernet_real(x, y))#3.hiernet

    #snif_res = with(train_dat, simple_snif(x, y))#4.snif


    #obtain prediction from 4 methods on validation data
    enet_pre[, rp] = predict(enet_res$enet_fit, newx = val_dat$xi)

    if (length(bkmr_res$select_idx) > 0) {
      bkmr_pre_mat = SamplePred(
        bkmr_res$kmsel,
        Znew = as.matrix(val_dat_bkmr$x[, bkmr_res$select_idx]),
        Xnew = as.matrix(val_dat_bkmr$z)
      )

    } else{
      bkmr_pre_mat = SamplePred(bkmr_res$kmsel,
                                Znew = NULL,
                                Xnew = as.matrix(val_dat_bkmr$z))

    }
    bkmr_pre = apply(bkmr_pre_mat, 2, mean)


    hiernet_pre[, rp] = predict(hiernet_res$hiernet_fit, newx = val_dat$x)

    dat_val = data.frame(x = val_dat$x)
    names(dat_val) = c(paste0("x", 1:ncol(dat_val)))
    #snif_pre[, rp] = predict(snif_res$snif_fit, newdata = dat_val)
    y[, rp] = val_dat$y
  }

  pred_y = cbind(c(enet_pre), c(bkmr_pre), c(hiernet_pre))#, c(snif_pre))

  weight_res = lse_constr(
    y = c(y),
    X = pred_y,
    max_iter = max_iter,
    tol = 1e-5
  )
  return(weight_super = weight_res$w)
}

super_4_combine_logit = function(dat,
                                 validation = 0.4,
                                 n_rep_super = 10,
                                 max_iter = 1000,
                                 tol = 1e-5)
{
  ntrain = round(nrow(dat$xi) * (1 - validation))
  nval = nrow(dat$xi) - ntrain

  lasso_pre = matrix(NA, nrow = nval, ncol = n_rep_super)
  enet_pre = matrix(NA, nrow = nval, ncol = n_rep_super)
  rf_pre = matrix(NA, nrow = nval, ncol = n_rep_super)
  hiernet_pre = matrix(NA, nrow = nval, ncol = n_rep_super)
  y = matrix(NA, nrow = nval, ncol = n_rep_super)

  for (rp in 1:n_rep_super) {
    #split data into training and validation
    train_id = sort(sample(nrow(dat$xi), ntrain))
    train_dat = list(x = dat$x[train_id,],
                     xi = dat$xi[train_id,],
                     y = dat$y[train_id])
    val_dat = list(x = dat$x[-train_id,],
                   xi = dat$xi[-train_id,],
                   y = dat$y[-train_id])

    #obtain fits from 4 methods on training data

    lasso_res = with(train_dat, simple_lasso(xi, y, family = "binomial"))#1.lasso

    enet_res = with(train_dat, simple_enet(xi, y, family = "binomial"))#2.enet

    rf_res = with(train_dat, simple_randomforest(x, y, binary = TRUE))#3.rf

    hiernet_res = with(train_dat, simple_hiernet(x, y, binary = TRUE))#4.hiernet


    #obtain prediction from 3 methods on validation data
    lasso_pre[, rp] = predict(lasso_res$lasso_fit, newx = val_dat$xi)
    enet_pre[, rp] = predict(enet_res$enet_fit, newx = val_dat$xi)
    rf_prob = predict(rf_res$rf_fit, newdata = val_dat$x, type = "prob")[, 2]
    rf_pre[, rp] = log(rf_prob / (1 - rf_prob) + 1e-15)
    hiernet_prob = predict(hiernet_res$hiernet_fit, newx = val_dat$x)$prob
    hiernet_pre[, rp] = log(hiernet_prob / (1 - hiernet_prob) + 1e-15)

    y[, rp] = val_dat$y
  }

  pred_y = cbind(c(lasso_pre), c(enet_pre), c(rf_pre), c(hiernet_pre))

  weight_res = lse_constr(
    y = c(y),
    X = pred_y,
    max_iter = max_iter,
    tol = 1e-5
  )
  return(weight_super = weight_res$w)
}

##########################################################################
########evaluation criteria#############################################
##########################################################################

select_acc <- function(estBeta, trueBeta) {
  est_select <- factor(as.numeric(estBeta != 0), levels = c(0, 1))
  true_select <- factor(as.numeric(trueBeta != 0), levels = c(0, 1))
  tab <- table(est_select, true_select)
  TP <- tab[2, 2]
  FP <- tab[2, 1]
  FN <- tab[1, 2]
  TN <- tab[1, 1]
  sens <- TP / max((TP + FN), 1)
  spec <- TN / max((TN + FP), 1)
  FPR <- 1 - spec
  FDR <- FP / max((FP + TP), 1)
  return(c(
    sens = sens,
    spec = spec,
    FDR = FDR,
    FPR = FPR
  ))
}


create_inter <- function(dat) {
  nm = colnames(dat$x)
  idxset = expand.grid(1:ncol(dat$x), 1:ncol(dat$x))
  subidx = which(idxset[, 1] < idxset[, 2])
  idx1 = idxset[subidx, 1]
  idx2 = idxset[subidx, 2]
  dat$idx1 = idx1
  dat$idx2 = idx2
  dat$xi = cbind(dat$x, dat$x[, dat$idx1] * dat$x[, dat$idx2])
  inter_nm = paste(nm[dat$idx1], nm[dat$idx2], sep = "_")
  colnames(dat$xi) = c(nm, inter_nm)
  return(dat)
}

create_inter_x <- function(x) {
  nm = colnames(x)
  idxset = expand.grid(1:ncol(x), 1:ncol(x))
  subidx = which(idxset[, 1] < idxset[, 2])
  idx1 = idxset[subidx, 1]
  idx2 = idxset[subidx, 2]
  xi = cbind(x, x[, idx1] * x[, idx2])
  inter_nm = paste(nm[idx1], nm[idx2], sep = "_")
  colnames(xi) = c(nm, inter_nm)
  return(xi)
}

#'@title A comprehensive toolkit for environmental mixtures analysis
#'@param y A vector of either continuous or binary values to indicate the health outcome
#'@param x A matrix of numeric values to indicate the chemical mixtures
#'@param z A matrix of numeric values to indicate the covariates
#'@param y.type  A character value of either "continuous" or "binary"
#'@param test.pct A numeric scalar between 0 and 1 to indicate the proportion allocated as test samples
#'@param interaction A logical value (TRUE/FALSE) to indicate whether to include
#'pairwise interaction terms between all the chemical mixtures x
#'@param interaction.exp.cov A logical value (TRUE/FALSE) to indicate whether to include
#'pairwise interaction terms between all the chemical mixtures x and covariates z. If interaction.exp.cov=TRUE,
#'interaction=TURE or interaction=FALSE will be ignored
#'@param covariates.forcein A logical value (TRUE/FALSE) to indicate whether to force in any covariates
#'@param bkmr.pip  A numeric scalar between 0 and 1 to indicate the cutoff for the
#'posterior inclusion probability in BKMR
#'@param bkmr.iter A positive integer to indicate the number of MCMC iterations for bkmr
#'@param var.select A logical value to indicate whether to perform variable selection
#'@param formula the formula for qgcomp and wqs
#'@param expnms a vector of characters for names of exposure variables
#'@param seed an integer value for seed
#'@param verbose a logical value to show information
#'@return A list object which may contain up to 8 cases
#'\describe{
#'\item{Case 1}{variable selection on main effects for exposures and confounders}
#'
#'
#'}
#'
#'Each case may contain some of the following elements
#'\describe{
#' \item{betaest}{a numeric vector of coeffcients for the exposures}
#' \item{z_betaest}{a numeric vector of coeffcients for the covariates}
#' \item{sse}{A positive scalar to indicate sum of squares error}
#' \item{corr}{A numeric scalar between -1 and 1 to indicate correlation coefficient}
#'}
#'
#'@author Wei Hao <weihao@umich.edu>
#'@examples
#'dat <- lmi_simul_dat(n=1000,p=20,q=5,
#'block_idx=c(1,1,2,2,3,1,1,1,1,1,2,2,2,2,3,3,3,3,3,3),
#'within_rho=0.6,btw_rho=0.1,R2=0.2,
#'effect_size=1,effect_size_i=1,
#'cancel_effect = FALSE)
#'#Example 1: The users would like to perform variable selections
#'#on main effects of exposures and covariates, and outcome, exposures and
#'#covariates are entered. For any individual interactions that the users would
#'#like to include in the models, they can add those into the covariate z.
#'res_ex1 <- Comp.Mix(y.type="continuous",y=dat$y, x=dat$x, z=dat$z, test.pct=0.5,
#'var.select = TRUE, interaction = FALSE, interaction.exp.cov = FALSE,
#'covariates.forcein = FALSE,
#'bkmr.pip=0.5, seed=2023)
#'
#'@export
Comp.Mix <- function(y,
                     x,
                     z = NULL,
                     y.type,
                     test.pct = 0.5,
                     var.select = NULL,
                     interaction = NULL,
                     interaction.exp.cov = NULL,
                     covariates.forcein = NULL,
                     bkmr.pip = 0.5,
                     bkmr.iter = 500,
                     formula = NULL,
                     expnms = NULL,
                     seed = 1234,
                     verbose=TRUE) {
  input.var.select = var.select
  input.interaction = interaction
  input.interaction.exp.cov = interaction.exp.cov
  input.covariates.forcein = covariates.forcein

  input.method = NULL

  if (is.null(input.var.select))
    var.select = TRUE
  if (is.null(input.interaction))
    interaction = FALSE
  if (is.null(input.covariates.forcein))
    covariates.forcein = FALSE
  if (is.null(input.interaction.exp.cov))
    interaction.exp.cov = FALSE



  #create interactions and split the data into training/testing
  set.seed(seed = seed)
  dat = list()
  dat$x = x
  dat = create_inter(dat)
  dat$y = y
  dat$z = z

  dat$x.z = cbind(dat$x, dat$z)
  dat$xzi = create_inter_x(dat$x.z)
  dat$xi.z = cbind(dat$xi, dat$z)

  ntrain = round(nrow(dat$x) * (1 - test.pct))
  train_id = sort(sample(nrow(dat$x), ntrain))
  train_dat = list(
    x = dat$x[train_id, ],
    y = dat$y[train_id],
    z = dat$z[train_id, ],
    x.z = dat$x.z[train_id, ],
    xi = dat$xi[train_id, ],
    xi.z = dat$xi.z[train_id, ],
    xzi = dat$xzi[train_id, ]
  )
  test_dat = list(
    x = dat$x[-train_id, ],
    y = dat$y[-train_id],
    z = dat$z[-train_id, ],
    x.z = dat$x.z[-train_id, ],
    xi = dat$xi[-train_id, ],
    xi.z = dat$xi.z[-train_id, ],
    xzi = dat$xzi[-train_id, ]
  )

  method_res = list()

###part 1: continuous outcome

 if (y.type=="continuous"){

#example 1: variable selection on x, z

  if (var.select == TRUE &
      interaction == FALSE &
      covariates.forcein == FALSE &
      interaction.exp.cov == FALSE & is.null(z) == FALSE) {
    if(verbose){
      case_nm = "case 1: variable selection on main effects for exposures and confounders"
      cat(case_nm, "\n")
    }

    if (is.null(input.method)) {
      method = c("lasso", "enet")
    } else{
      method = input.method
    }

    #output results
    betaest = list()
    sse = c()
    corr = c()

    if (is.element("lasso", method)) {
      #lasso fitting and prediction
      if(verbose){
      cat("Fitting lasso ... ", "\n")
      }
      lasso_res = with(train_dat, simple_lasso(x.z, y)) #lasso

      lasso_pre = predict(lasso_res$lasso_fit, newx = test_dat$x.z)
      lasso_sse = mean((lasso_pre - test_dat$y) ^ 2)
      if (var(lasso_pre) > 0) {
        lasso_corr = cor(lasso_pre, test_dat$y)
      } else {
        lasso_corr = 0
      }

      betaest$lasso = lasso_res$betaest[lasso_res$betaest[, 1] != 0, 1]
      sse = c(sse, lasso = lasso_sse)
      corr = c(corr, lasso = lasso_corr)
    }

    if (is.element("enet", method)) {
      #enet fitting and prediction
      if(verbose){
      cat("Fitting enet ... ", "\n")
      }
      enet_res = with(train_dat, simple_enet(x.z, y)) #enet
      enet_pre = predict(enet_res$enet_fit, newx = test_dat$x.z)
      enet_sse = mean((enet_pre - test_dat$y) ^ 2)
      if (var(enet_pre) > 0) {
        enet_corr = cor(enet_pre, test_dat$y)
      } else {
        enet_corr = 0
      }

      betaest$enet = enet_res$betaest[enet_res$betaest[, 1] != 0, 1]
      sse = c(sse, enet = enet_sse)
      corr = c(corr, enet = enet_corr)

    }

    method_res[[case_nm]] = list(betaest = betaest,
                                 sse = sse,
                                 corr = corr)

    if(length(betaest)==0){
      for(mm in 1:length(method))
        if(verbose){
        cat(method[mm],"is not available for this case!\n")
        }
    }

  }

  #############################################################


  #example 2: variable selection on xi and z (null/non-null)
  if (var.select == TRUE &
      interaction == TRUE & covariates.forcein == FALSE) {
    if(verbose){
    case_nm = "case 2: variable selection on main and interaction effects for exposures and main effects for confounders"
    cat(case_nm, "\n")
    }

    if (is.null(input.method)) {
      method = c("lasso", "enet")
    } else{
      method = input.method
    }

    #output results
    betaest = list()
    sse = c()
    corr = c()

    if (is.element("lasso", method)) {
      if(verbose){
      cat("Fitting lasso ... ", "\n")
      }
      #2.1 lasso fitting and prediction
      lasso_res = with(train_dat, simple_lasso(xi.z, y))

      lasso_pre = predict(lasso_res$lasso_fit, newx = test_dat$xi.z)
      lasso_sse = mean((lasso_pre - test_dat$y) ^ 2)
      if (var(lasso_pre) > 0) {
        lasso_corr = cor(lasso_pre, test_dat$y)
      } else {
        lasso_corr = 0
      }
      betaest$lasso = lasso_res$betaest[lasso_res$betaest[, 1] != 0, 1]
      sse = c(sse, lasso = lasso_sse)
      corr = c(corr, lasso = lasso_corr)
    }

    if (is.element("enet", method)) {
      if(verbose){
      cat("Fitting enet ... ", "\n")
      }
      #2.2 enet fitting and prediction
      enet_res = with(train_dat, simple_enet(xi.z, y))
      enet_pre = predict(enet_res$enet_fit, newx = test_dat$xi.z)
      enet_sse = mean((enet_pre - test_dat$y) ^ 2)
      if (var(enet_pre) > 0) {
        enet_corr = cor(enet_pre, test_dat$y)
      } else {
        enet_corr = 0
      }
      betaest$enet = enet_res$betaest[enet_res$betaest[, 1] != 0, 1]
      sse = c(sse, enet = enet_sse)
      corr = c(corr, enet = enet_corr)
    }


    method_res[[case_nm]] = list(betaest = betaest,
                                 sse = sse,
                                 corr = corr)


    if(length(betaest)==0){
      for(mm in 1:length(method))
        if(verbose){
        cat(method[mm],"is not available for this case!\n")
        }
    }
  }


  #############################################################


  #example 3: variable selection on xzi
  if (var.select == TRUE &
      interaction.exp.cov == TRUE & covariates.forcein == FALSE &
      is.null(z) == FALSE ) {
    if(verbose){
    case_nm = "case 3: variable selection on main and interaction effects among exposures and confounders"
    cat(case_nm, "\n")
    }


    if (is.null(input.method)) {
      method = c("lasso", "enet", "hiernet", "snif")
    } else{
      method = input.method
    }


    #output results
    betaest = list()
    sse = c()
    corr = c()

    if (is.element("lasso", method)) {
      if(verbose){
      cat("Fitting lasso ... ", "\n")
      }
      #3.1 lasso fitting and prediction
      lasso_res = with(train_dat, simple_lasso(xzi, y))

      lasso_pre = predict(lasso_res$lasso_fit, newx = test_dat$xzi)
      lasso_sse = mean((lasso_pre - test_dat$y) ^ 2)
      if (var(lasso_pre) > 0) {
        lasso_corr = cor(lasso_pre, test_dat$y)
      } else {
        lasso_corr = 0
      }

      betaest$lasso = lasso_res$betaest[lasso_res$betaest[, 1] != 0, 1]
      sse = c(sse, lasso = lasso_sse)
      corr = c(corr, lasso = lasso_corr)
    }

    if (is.element("enet", method)) {
      if(verbose){
      cat("Fitting enet ... ", "\n")
      }
      #3.2 enet fitting and prediction
      enet_res = with(train_dat, simple_enet(xzi, y))

      enet_pre = predict(enet_res$enet_fit, newx = test_dat$xzi)
      enet_sse = mean((enet_pre - test_dat$y) ^ 2)
      if (var(enet_pre) > 0) {
        enet_corr = cor(enet_pre, test_dat$y)
      } else {
        enet_corr = 0
      }

      betaest$enet = enet_res$betaest[enet_res$betaest[, 1] != 0, 1]
      sse = c(sse, enet = enet_sse)
      corr = c(corr, enet = enet_corr)
    }



    if (is.element("hiernet", method)) {
      if(verbose){
      cat("Fitting hiernet ... ", "\n")
      }
      #3.3 hiernet fitting and prediction
      hiernet_res = with(train_dat, simple_hiernet(x.z, y, trace=0))

      hiernet_pre = predict(hiernet_res$hiernet_fit, newx = test_dat$x.z)
      hiernet_sse = mean((hiernet_pre - test_dat$y) ^ 2)
      if (var(hiernet_pre) > 0) {
        hiernet_corr = cor(hiernet_pre, test_dat$y)
      } else {
        hiernet_corr = 0
      }
      betaest$hiernet = colnames(train_dat$xzi)[hiernet_res$betaest ==
                                          1]
      sse = c(sse, hiernet = hiernet_sse)
      corr = c(corr, hiernet = hiernet_corr)

    }

    # if (is.element("snif", method)) {
    #   cat("Fitting snif ... ", "\n")
    #   #3.4 snif fitting and prediction
    #   snif_res = with(train_dat, simple_snif(x.z, y))
    #
    #   dat_test = data.frame(test_dat$x.z)
    #   names(dat_test) = c(paste0("x", 1:ncol(dat_test)))
    #   snif_pre = predict(snif_res$snif_fit, newdata = dat_test)
    #   snif_sse = mean((snif_pre - test_dat$y) ^ 2)
    #   if (var(snif_pre) > 0) {
    #     snif_corr = cor(snif_pre, test_dat$y)
    #   } else {
    #     snif_corr = 0
    #   }
    #
    #   betaest$snif = colnames(train_dat$xzi)[snif_res$betaest == 1]
    #   sse = c(sse, snif = snif_sse)
    #   corr = c(corr, snif = snif_corr)
    # }


    method_res[[case_nm]] = list(betaest = betaest,
                                 sse = sse,
                                 corr = corr)

    if(length(betaest)==0){
      for(mm in 1:length(method))
        if(verbose){
        cat(method[mm],"is not available for this case!\n")
        }
    }
  }


  #############################################################


  # example 4: variable selection on x while controlling for z
  if (var.select == TRUE &
      interaction == FALSE &
      covariates.forcein == TRUE & is.null(z) == FALSE) {
    if(verbose){
    case_nm = "case 4: variable selection on main effects for exposures while controlling for confounders"
    cat(case_nm, "\n")
     }
    #output results
    betaest = list()
    z_betaest = list()
    sse = c()
    corr = c()

    if (is.null(input.method)) {
      method = c("lasso", "enet", "bkmr")
    } else{
      method = input.method
    }

    if (is.element("lasso", method)) {
      if(verbose){
      cat("Fitting lasso ... ", "\n")
      }
      #4.1 lasso fitting and prediction
      lasso_res = with(train_dat, simple_lasso_real_force(x, y, z))
      lasso_pre = predict(lasso_res$lasso_fit, newx = test_dat$x.z)
      lasso_sse = mean((lasso_pre - test_dat$y) ^ 2)
      if (var(lasso_pre) > 0) {
        lasso_corr = cor(lasso_pre, test_dat$y)
      } else {
        lasso_corr = 0
      }

      betaest$lasso = lasso_res$betaest[lasso_res$betaest[, 1] != 0, 1,drop=FALSE]
      z_betaest$lasso=lasso_res$z_betaest
      sse = c(sse, lasso = lasso_sse)
      corr = c(corr, lasso = lasso_corr)
    }

    if (is.element("enet", method)) {
      if(verbose){
      cat("Fitting enet ... ", "\n")
      }
      #4.2 enet fitting and prediction
      enet_res = with(train_dat, simple_enet_real_force(x, y, z))
      enet_pre = predict(enet_res$enet_fit, newx = test_dat$x.z)
      enet_sse = mean((enet_pre - test_dat$y) ^ 2)
      if (var(enet_pre) > 0) {
        enet_corr = cor(enet_pre, test_dat$y)
      } else {
        enet_corr = 0
      }

      betaest$enet = enet_res$betaest[enet_res$betaest[, 1] != 0, 1,drop=FALSE]
      z_betaest$enet=enet_res$z_betaest
      sse = c(sse, enet = enet_sse)
      corr = c(corr, enet = enet_corr)
    }


    if (is.element("bkmr", method)) {
      if(verbose){
      cat("Fitting bkmr ... ", "\n")
      }
      #4.3 bkmr fitting and prediction
      bkmr_res = with(
        train_dat,
        simple_bkmr_real(
          x,
          y,
          z,
          pip = bkmr.pip,
          varsel = TRUE,
          iter = bkmr.iter
        )
      )

      bkmr_pre = ComputePostmeanHnew(bkmr_res$kmsel, Znew = as.data.frame(test_dat$x[, bkmr_res$select_id]))$postmean
      bkmr_sse = mean((bkmr_pre - test_dat$y) ^ 2)
      if (var(bkmr_pre) > 0) {
        bkmr_corr = cor(bkmr_pre, test_dat$y)
      } else {
        bkmr_corr = 0
      }
      betaest$bkmr = colnames(train_dat$x)[bkmr_res$betaest == 1]
      sse = c(sse, bkmr =bkmr_sse)
      corr = c(corr, bkmr = bkmr_corr)
    }

    method_res[[case_nm]] = list(
      betaest = betaest,
      z_betaest = z_betaest,
      sse = sse,
      corr = corr
    )

    if(length(betaest)==0){
      for(mm in 1:length(method))
        if(verbose){
        cat(method[mm],"is not available for this case!\n")
        }
    }

  }


  #############################################################


  #example 5: variable selection on xi while controlling for z
  if (var.select == TRUE &
      interaction == TRUE &
      covariates.forcein == TRUE & is.null(z) == FALSE) {
    if(verbose){
    case_nm = "case 5: variable selection on main and interaction effects for exposures while controlling for confounders"
    cat(case_nm, "\n")
    }

    if (is.null(input.method)) {
      method = c("lasso", "enet")
    } else{
      method = input.method
    }

    #output results
    betaest = list()
    z_betaest = list()
    sse = c()
    corr = c()

    if (is.element("lasso", method)) {
      if(verbose){
      cat("Fitting lasso ... ", "\n")
      }
      #5.1 lasso fitting and prediction
      lasso_res = with(train_dat, simple_lasso_real_force(xi, y, z))

      lasso_pre = predict(lasso_res$lasso_fit, newx = test_dat$xi.z)
      lasso_sse = mean((lasso_pre - test_dat$y) ^ 2)
      if (var(lasso_pre) > 0) {
        lasso_corr = cor(lasso_pre, test_dat$y)
      } else {
        lasso_corr = 0
      }

      betaest$lasso = lasso_res$betaest[lasso_res$betaest[, 1] != 0, 1,drop=FALSE]
      z_betaest$lasso=lasso_res$z_betaest
      sse = c(sse, lasso = lasso_sse)
      corr = c(corr, lasso = lasso_corr)

    }

    if (is.element("enet", method)) {
      if(verbose){
      cat("Fitting enet ... ", "\n")
      }
      #5.2 enet fitting and prediction
      enet_res = with(train_dat, simple_enet_real_force(xi, y, z))

      enet_pre = predict(enet_res$enet_fit, newx = test_dat$xi.z)
      enet_sse = mean((enet_pre - test_dat$y) ^ 2)
      if (var(enet_pre) > 0) {
        enet_corr = cor(enet_pre, test_dat$y)
      } else {
        enet_corr = 0
      }

      betaest$enet = enet_res$betaest[enet_res$betaest[, 1] != 0, 1,drop=FALSE]
      z_betaest$enet=enet_res$z_betaest
      sse = c(sse, enet = enet_sse)
      corr = c(corr, enet = enet_corr)
    }

    method_res[[case_nm]] = list(
      betaest = betaest,
      z_betaest = z_betaest,
      sse = sse,
      corr = corr
    )

    if(length(betaest)==0){
      for(mm in 1:length(method))
        if(verbose){
        cat(method[mm],"is not available for this case!\n")
        }
    }

  }

  #############################################################
  if (is.null(input.var.select))
    var.select = FALSE
  if (is.null(input.interaction))
    interaction = FALSE
  if (is.null(input.covariates.forcein))
    covariates.forcein = TRUE

  #example 6: no variable selection while controlling for z
  if (var.select == FALSE &
      interaction == FALSE &
      covariates.forcein == TRUE & is.null(z) == FALSE) {
    if(verbose){
    case_nm = "case 6: estimating mixtures effects without variable selection while controlling for confounders"
    cat(case_nm, "\n")
    }

    if (is.null(input.method)) {
      method = c("wqs", "qgcomp")
    } else{
      method = input.method
    }

    #output results
    betaest = list()
    mixture.coef = list()
    sse = c()
    corr = c()

    if (is.element("wqs", method)) {
      if(verbose){
      cat("Fitting wqs ... ", "\n")
      }
      #6.1 wqs fitting and prediction
      wqs_full_res = with(train_dat, simple_wqs_real(x, y, z))#wqs
      wqs_test_full_dat = data.frame(test_dat$x, y = rep(0, length = nrow(test_dat$x)), test_dat$z)
      wqs_full_pred = predict(wqs_full_res$wqs_fit, wqs_test_full_dat)
      wqs_full_pre = wqs_full_pred$df_pred$ypred
      wqs_full_sse = mean((wqs_full_pre - test_dat$y) ^ 2)
      wqs_full_corr = cor(wqs_full_pre, test_dat$y)

      betaest$wqs = wqs_full_res$wqs_fit$final_weights
      mixture.coef$wqs = summary(wqs_full_res$wqs_fit)$coef
      sse = c(sse, wqs = wqs_full_sse)
      corr = c(corr, wqs = wqs_full_corr)
    }

    if (is.element("qgcomp", method)) {
      if(verbose){
      cat("Fitting qgcomp ... ", "\n")
      }
      #6.2 qgcomp fitting and prediction
      gcomp_full_res = with(train_dat,
                            simple_gcomp_real(x, y, z, expnms = colnames(x), bayes = TRUE))
      gcomp_test_full_dat = data.frame(test_dat$x, test_dat$z)
      gcomp_full_pre = predict(gcomp_full_res$gcomp_fit, newdata = gcomp_test_full_dat)
      gcomp_full_sse = mean((gcomp_full_pre - test_dat$y) ^ 2)
      gcomp_full_corr = cor(gcomp_full_pre, test_dat$y)

      betaest$gcomp = gcomp_full_res$gcomp_fit
      mixture.coef$gcomp = summary(gcomp_full_res$gcomp_fit)$coef

      sse = c(sse, gcomp = gcomp_full_sse)
      corr = c(corr, gcomp = gcomp_full_corr)
    }

    method_res[[case_nm]] = list(
      betaest = betaest,
      mixture.coef = mixture.coef,
      sse = sse,
      corr = corr
    )

    if(length(betaest)==0){
      for(mm in 1:length(method))
        if(verbose){
        cat(method[mm],"is not available for this case!\n")
        }
    }

  }

  #############################################################
  if (is.null(input.var.select))
    var.select = FALSE
  if (is.null(input.interaction))
    interaction = FALSE
  if (is.null(input.covariates.forcein))
    covariates.forcein = FALSE

  #example 7: no variable selection and no controlling for z (random forest)
  if (var.select == FALSE &
      interaction == FALSE & covariates.forcein == FALSE) {
    if(verbose){
    case_nm = "case 7: importance ranking among exposures and confounders"
    cat(case_nm, "\n")
    }

    if (is.null(input.method)) {
      method = c("rf")
    } else{
      method = input.method
    }

    ranking = NULL

    if (is.element("rf", method)) {
      if(verbose){
      cat("Fitting rf ... ", "\n")
      }
      rf_res = with(train_dat, simple_randomforest(x.z, y))
      rf_pre = predict(rf_res$rf_fit, newx = test_dat$x.z)
      rf_sse = mean((rf_pre - test_dat$y) ^ 2)
      if (var(rf_pre) > 0) {
        rf_corr = cor(rf_pre, test_dat$y)
      } else {
        rf_corr = 0
      }

      ranking = sort(rf_res$rf_fit$importance[, 1], decreasing = T)
    }

    method_res[[case_nm]] = list(ranking =
                                   ranking)

    if(length(ranking)==0){
      for(mm in 1:length(method))
        if(verbose){
        cat(method[mm],"is not available for this case!\n")
        }
    }
  }


  #############################################################


  #when z is null
  #example 8: variable selection on x
  if (var.select == TRUE &
      interaction == FALSE &
      covariates.forcein == FALSE & is.null(z) == TRUE) {
    if(verbose){
    case_nm = "case 8: variable selection on main effects without input confounders"
    cat(case_nm, "\n")
    }

    if (is.null(input.method)) {
      method = c("lasso", "enet", "bkmr")
    } else{
      method = input.method
    }

    #output results
    betaest = list()
    sse = c()
    corr = c()

    if (is.element("lasso", method)) {
      if(verbose){
      cat("Fitting lasso ... ", "\n")
      }
      #8.1 lasso fitting and prediction
      lasso_res = with(train_dat, simple_lasso(x, y))
      lasso_pre = predict(lasso_res$lasso_fit, newx = test_dat$x)
      lasso_sse = mean((lasso_pre - test_dat$y) ^ 2)
      if (var(lasso_pre) > 0) {
        lasso_corr = cor(lasso_pre, test_dat$y)
      } else {
        lasso_corr = 0
      }

      betaest$lasso = lasso_res$betaest[lasso_res$betaest[, 1] != 0, 1,drop=FALSE]
      sse = c(sse, lasso = lasso_sse)
      corr = c(corr, lasso = lasso_corr)

    }

    if (is.element("enet", method)) {
      if(verbose){
      cat("Fitting enet ... ", "\n")
      }
      #8.2 enet fitting and prediction
      enet_res = with(train_dat, simple_enet(x, y))
      enet_pre = predict(enet_res$enet_fit, newx = test_dat$x)
      enet_sse = mean((enet_pre - test_dat$y) ^ 2)
      if (var(enet_pre) > 0) {
        enet_corr = cor(enet_pre, test_dat$y)
      } else {
        enet_corr = 0
      }

      betaest$enet = enet_res$betaest[enet_res$betaest[, 1] != 0, 1,drop=FALSE]

      sse = c(sse, enet = enet_sse)
      corr = c(corr, enet = enet_corr)
    }

    if (is.element("bkmr", method)) {
      if(verbose){
      cat("Fitting bkmr ... ", "\n")
      }
      #8.3 bkmr fitting and prediction
      bkmr_res = with(train_dat,
                      simple_bkmr(
                        x,
                        y,
                        pip = bkmr.pip,
                        varsel = var.select,
                        iter = bkmr.iter
                      ))
      bkmr_pre = ComputePostmeanHnew(bkmr_res$kmsel, Znew = as.data.frame(test_dat$x[, bkmr_res$select_id]))$postmean
      bkmr_sse = mean((bkmr_pre - test_dat$y) ^ 2)
      if (var(bkmr_pre) > 0) {
        bkmr_corr = cor(bkmr_pre, test_dat$y)
      } else {
        bkmr_corr = 0
      }

      betaest$bkmr = colnames(train_dat$x)[bkmr_res$betaest == 1]
      sse = c(sse, bkmr = bkmr_sse)
      corr = c(corr, bkmr = bkmr_corr)
    }


    method_res[[case_nm]] = list(betaest =
                                   betaest,
                                 sse = sse,
                                 corr = corr)

    if(length(betaest)==0){
      for(mm in 1:length(method))
        if(verbose){
        cat(method[mm],"is not available for this case!\n")
        }
    }
  }

  return(method_res)
 }


  ###part 2: binary outcome

  if (y.type=="binary"){

    #example 1: variable selection on x, z

    if (var.select == TRUE &
        interaction == FALSE &
        covariates.forcein == FALSE &
        interaction.exp.cov == FALSE & is.null(z) == FALSE) {
      if(verbose){
      case_nm = "case 1: variable selection on main effects for exposures and confounders"
      cat(case_nm, "\n")
      }

      if (is.null(input.method)) {
        method = c("lasso", "enet")
      } else{
        method = input.method
      }

      #output results
      betaest = list()
      brier = c()
      auc= c()

      if (is.element("lasso", method)) {
        #lasso fitting and prediction
        if(verbose){
        cat("Fitting lasso ... ", "\n")
        }
        lasso_res = with(train_dat, simple_lasso(x.z, y,family="binomial")) #lasso

        lasso_pre = predict(lasso_res$lasso_fit, newx = test_dat$x.z)

        lasso_prob=predict(lasso_res$lasso_fit, newx = test_dat$x.z, type = "response")
        lasso_brier=mean((lasso_prob - test_dat$y)^2)
        lasso_roc =roc(test_dat$y, lasso_pre[,1])
        lasso_auc=as.numeric(lasso_roc$auc)

        betaest$lasso = lasso_res$betaest[lasso_res$betaest[, 1] != 0, 1]
        brier= c(brier, lasso = lasso_brier)
        auc = c(auc, lasso = lasso_auc)

      }

      if (is.element("enet", method)) {
        #enet fitting and prediction
        if(verbose){
        cat("Fitting enet ... ", "\n")
        }
        enet_res = with(train_dat, simple_enet(x.z, y,family="binomial")) #enet
        enet_pre = predict(enet_res$enet_fit, newx = test_dat$x.z)
        enet_prob=predict(enet_res$enet_fit, newx = test_dat$x.z, type = "response")
        enet_brier=mean((enet_prob - test_dat$y)^2)
        enet_roc =roc(test_dat$y, enet_pre[,1])
        enet_auc=as.numeric(enet_roc$auc)

        betaest$enet = enet_res$betaest[enet_res$betaest[, 1] != 0, 1]
        brier = c(brier, enet = enet_brier)
        auc = c(auc, enet = enet_auc)

      }

      method_res[[case_nm]] = list(betaest = betaest,
                                   brier = brier,
                                   auc = auc)

      if(length(betaest)==0){
        for(mm in 1:length(method))
          if(verbose){
          cat(method[mm],"is not available for this case!\n")
          }
      }

    }

    #############################################################


    #example 2: variable selection on xi and z (null/non-null)
    if (var.select == TRUE &
        interaction == TRUE & covariates.forcein == FALSE) {
      if(verbose){
      case_nm = "case 2: variable selection on main and interaction effects for exposures and main effects for confounders"
      cat(case_nm, "\n")
      }

      if (is.null(input.method)) {
        method = c("lasso", "enet")
      } else{
        method = input.method
      }

      #output results
      betaest = list()
      brier = c()
      auc = c()

      if (is.element("lasso", method)) {
        if(verbose){
        cat("Fitting lasso ... ", "\n")
        }
        #2.1 lasso fitting and prediction
        lasso_res = with(train_dat, simple_lasso(xi.z, y,family="binomial"))

        lasso_pre = predict(lasso_res$lasso_fit, newx = test_dat$xi.z)
        lasso_prob=predict(lasso_res$lasso_fit, newx = test_dat$xi.z, type = "response")
        lasso_brier=mean((lasso_prob - test_dat$y)^2)
        lasso_roc =roc(test_dat$y, lasso_pre[,1])
        lasso_auc=as.numeric(lasso_roc$auc)


        betaest$lasso = lasso_res$betaest[lasso_res$betaest[, 1] != 0, 1]
        brier = c(brier, lasso = lasso_brier)
        auc = c(auc, lasso = lasso_auc)
      }

      if (is.element("enet", method)) {
        if(verbose){
        cat("Fitting enet ... ", "\n")
        }
        #2.2 enet fitting and prediction
        enet_res = with(train_dat, simple_enet(xi.z, y,family="binomial"))
        enet_pre = predict(enet_res$enet_fit, newx = test_dat$xi.z)
        enet_prob=predict(enet_res$enet_fit, newx = test_dat$xi.z, type = "response")
        enet_brier=mean((enet_prob - test_dat$y)^2)
        enet_roc =roc(test_dat$y, enet_pre[,1])
        enet_auc=as.numeric(enet_roc$auc)

        betaest$enet = enet_res$betaest[enet_res$betaest[, 1] != 0, 1]
        brier = c(brier, enet = enet_brier)
        auc = c(auc, enet = enet_auc)
      }


      method_res[[case_nm]] = list(betaest = betaest,
                                   brier = brier,
                                   auc= auc)


      if(length(betaest)==0){
        for(mm in 1:length(method))
          if(verbose){
          cat(method[mm],"is not available for this case!\n")
          }
      }
    }


    #############################################################


    #example 3: variable selection on xzi
    if (var.select == TRUE &interaction == FALSE&
        interaction.exp.cov == TRUE & covariates.forcein == FALSE &
        is.null(z) == FALSE ) {
      if(verbose){
      case_nm = "case 3: variable selection on main and interaction effects among exposures and confounders"
      cat(case_nm, "\n")
      }

      if (is.null(input.method)) {
        method = c("lasso", "enet", "hiernet", "snif")
      } else{
        method = input.method
      }


      #output results
      betaest = list()
      brier = c()
      auc = c()

      if (is.element("lasso", method)) {
        if(verbose){
        cat("Fitting lasso ... ", "\n")
        }
        #3.1 lasso fitting and prediction
        lasso_res = with(train_dat, simple_lasso(xzi, y,family="binomial"))

        lasso_pre = predict(lasso_res$lasso_fit, newx = test_dat$xzi)
        lasso_prob=predict(lasso_res$lasso_fit, newx = test_dat$xzi, type = "response")
        lasso_brier=mean((lasso_prob - test_dat$y)^2)
        lasso_roc =roc(test_dat$y, lasso_pre[,1])
        lasso_auc=as.numeric(lasso_roc$auc)


        betaest$lasso = lasso_res$betaest[lasso_res$betaest[, 1] != 0, 1]
        brier = c(brier, lasso = lasso_brier)
        auc = c(auc, lasso = lasso_auc)
      }

      if (is.element("enet", method)) {
        if(verbose){
        cat("Fitting enet ... ", "\n")
        }
        #3.2 enet fitting and prediction
        enet_res = with(train_dat, simple_enet(xzi, y,family="binomial"))

        enet_pre = predict(enet_res$enet_fit, newx = test_dat$xzi)
        enet_prob=predict(enet_res$enet_fit, newx = test_dat$xzi, type = "response")
        enet_brier=mean((enet_prob - test_dat$y)^2)
        enet_roc =roc(test_dat$y, enet_pre[,1])
        enet_auc=as.numeric(enet_roc$auc)

        betaest$enet = enet_res$betaest[enet_res$betaest[, 1] != 0, 1]
        brier = c(brier, enet = enet_brier)
        auc = c(auc, enet = enet_auc)
      }



      if (is.element("hiernet", method)) {
        if(verbose){
        cat("Fitting hiernet ... ", "\n")
        }
        #3.3 hiernet fitting and prediction
        hiernet_res = with(train_dat, simple_hiernet(x.z, y,binary=TRUE,trace=0))

        hiernet_prob=predict(hiernet_res$hiernet_fit, newx = test_dat$x.z)$prob
        hiernet_pre=log(hiernet_prob/(1-hiernet_prob)+1e-15)#eta,ers
        hiernet_brier=mean((hiernet_prob - test_dat$y)^2)
        hiernet_roc =roc(test_dat$y, hiernet_pre)
        hiernet_auc=as.numeric(hiernet_roc$auc)

        betaest$hiernet = colnames(train_dat$xzi)[hiernet_res$betaest ==
                                                    1]
        brier = c(brier, hiernet = hiernet_brier)
        auc = c(auc, hiernet = hiernet_auc)

      }

      method_res[[case_nm]] = list(betaest = betaest,
                                   brier = brier,
                                   auc = auc)

      if(length(betaest)==0){
        for(mm in 1:length(method))
          if(verbose){
          cat(method[mm],"is not available for this case!\n")
          }
      }
    }


    #############################################################


    # example 4: variable selection on x while controlling for z
    if (var.select == TRUE &
        interaction == FALSE &
        covariates.forcein == TRUE & is.null(z) == FALSE) {
      if(verbose){
      case_nm = "case 4: variable selection on main effects for exposures while controlling for confounders"
      cat(case_nm, "\n")
      }
      #output results
      betaest = list()
      z_betaest = list()
      brier = c()
      auc= c()

      if (is.null(input.method)) {
        method = c("lasso", "enet", "bkmr")
      } else{
        method = input.method
      }

      if (is.element("lasso", method)) {
        if(verbose){
        cat("Fitting lasso ... ", "\n")
        }
        #4.1 lasso fitting and prediction
        lasso_res = with(train_dat, simple_lasso_real_force(x, y, z,family="binomial"))
        lasso_pre = predict(lasso_res$lasso_fit, newx = test_dat$x.z)

        lasso_prob=predict(lasso_res$lasso_fit, newx = test_dat$x.z, type = "response")
        lasso_brier=mean((lasso_prob - test_dat$y)^2)
        lasso_roc =roc(test_dat$y, lasso_pre[,1])
        lasso_auc=as.numeric(lasso_roc$auc)


        betaest$lasso = lasso_res$betaest[lasso_res$betaest[, 1] != 0, 1,drop=FALSE]
        z_betaest$lasso=lasso_res$z_betaest
        brier = c(brier, lasso = lasso_brier)
        auc = c(auc, lasso = lasso_auc)
      }

      if (is.element("enet", method)) {
        if(verbose){
        cat("Fitting enet ... ", "\n")
        }
        #4.2 enet fitting and prediction
        enet_res = with(train_dat, simple_enet_real_force(x, y, z,family="binomial"))
        enet_pre = predict(enet_res$enet_fit, newx = test_dat$x.z)

        enet_prob=predict(enet_res$enet_fit, newx = test_dat$x.z, type = "response")
        enet_brier=mean((enet_prob - test_dat$y)^2)
        enet_roc =roc(test_dat$y, enet_pre[,1])
        enet_auc=as.numeric(enet_roc$auc)


        betaest$enet = enet_res$betaest[enet_res$betaest[, 1] != 0, 1,drop=FALSE]
        z_betaest$enet=enet_res$z_betaest
        brier = c(brier, enet = enet_brier)
        auc = c(auc, enet = enet_auc)
      }


      method_res[[case_nm]] = list(
        betaest = betaest,
        z_betaest = z_betaest,
        brier = brier,
        auc = auc
      )

      if(length(betaest)==0){
        for(mm in 1:length(method))
          if(verbose){
          cat(method[mm],"is not available for this case!\n")
          }
      }

    }


    #############################################################


    #example 5: variable selection on xi while controlling for z
    if (var.select == TRUE &
        interaction == TRUE &
        covariates.forcein == TRUE & is.null(z) == FALSE) {
      if(verbose){
      case_nm = "case 5: variable selection on main and interaction effects for exposures while controlling for confounders"
      cat(case_nm, "\n")
      }

      if (is.null(input.method)) {
        method = c("lasso", "enet")
      } else{
        method = input.method
      }

      #output results
      betaest = list()
      z_betaest = list()
      brier = c()
      auc = c()

      if (is.element("lasso", method)) {
        if(verbose){
        cat("Fitting lasso ... ", "\n")
        }
        #5.1 lasso fitting and prediction
        lasso_res = with(train_dat, simple_lasso_real_force(xi, y, z,family="binomial"))

        lasso_pre = predict(lasso_res$lasso_fit, newx = test_dat$xi.z)
        lasso_prob=predict(lasso_res$lasso_fit, newx = test_dat$xi.z, type = "response")
        lasso_brier=mean((lasso_prob - test_dat$y)^2)
        lasso_roc =roc(test_dat$y, lasso_pre[,1])
        lasso_auc=as.numeric(lasso_roc$auc)


        betaest$lasso = lasso_res$betaest[lasso_res$betaest[, 1] != 0, 1,drop=FALSE]
        z_betaest$lasso=lasso_res$z_betaest
        brier = c(brier, lasso = lasso_brier)
        auc = c(auc, lasso = lasso_auc)

      }

      if (is.element("enet", method)) {
        if(verbose){
        cat("Fitting enet ... ", "\n")
        }
        #5.2 enet fitting and prediction
        enet_res = with(train_dat, simple_enet_real_force(xi, y, z,family="binomial"))

        enet_pre = predict(enet_res$enet_fit, newx = test_dat$xi.z)
        enet_prob=predict(enet_res$enet_fit, newx = test_dat$xi.z, type = "response")
        enet_brier=mean((enet_prob - test_dat$y)^2)
        enet_roc =roc(test_dat$y, enet_pre[,1])
        enet_auc=as.numeric(enet_roc$auc)

        betaest$enet = enet_res$betaest[enet_res$betaest[, 1] != 0, 1,drop=FALSE]
        z_betaest$enet=enet_res$z_betaest
        brier = c(brier, enet = enet_brier)
        auc = c(auc, enet = enet_auc)
      }

      method_res[[case_nm]] = list(
        betaest = betaest,
        z_betaest = z_betaest,
        brier = brier,
        auc = auc
      )

      if(length(betaest)==0){
        for(mm in 1:length(method))
          if(verbose){
          cat(method[mm],"is not available for this case!\n")
          }
      }

    }

    #############################################################
    if (is.null(input.var.select))
      var.select = FALSE
    if (is.null(input.interaction))
      interaction = FALSE
    if (is.null(input.covariates.forcein))
      covariates.forcein = TRUE

    #example 6: no variable selection while controlling for z
    if (var.select == FALSE &
        interaction == FALSE &
        covariates.forcein == TRUE & is.null(z) == FALSE) {
      if(verbose){
      case_nm = "case 6: estimating mixtures effects without variable selection while controlling for confounders"
      cat(case_nm, "\n")
      }

      if (is.null(input.method)) {
        method = c("wqs", "qgcomp")
      } else{
        method = input.method
      }

      #output results
      betaest = list()
      mixture.coef = list()
      brier = c()
      auc = c()

      if (is.element("wqs", method)) {
        if(verbose){
        cat("Fitting wqs ... ", "\n")
        }
        #6.1 wqs fitting and prediction
        wqs_full_res = with(train_dat, simple_wqs_real(x, y, z,family="binomial"))#wqs
        wqs_test_full_dat = data.frame(test_dat$x, y = rep(0, length = nrow(test_dat$x)), test_dat$z)
        wqs_full_pred = predict(wqs_full_res$wqs_fit, wqs_test_full_dat)
        wqs_full_pre = wqs_full_pred$df_pred$ypred
        wqs_full_prob=predict(wqs_full_res$wqs_fit, wqs_test_full_dat,type="response")$df_pred$ypred
        wqs_full_brier=mean((wqs_full_prob-test_dat$y)^2)
        wqs_full_roc =roc(test_dat$y, wqs_full_pre)
        wqs_full_auc=as.numeric(wqs_full_roc$auc)


        betaest$wqs = wqs_full_res$wqs_fit$final_weights
        mixture.coef$wqs = summary(wqs_full_res$wqs_fit)$coef
        brier = c(brier, wqs = wqs_full_brier)
        auc = c(auc, wqs = wqs_full_auc)
      }

      if (is.element("qgcomp", method)) {
        if(verbose){
        cat("Fitting qgcomp ... ", "\n")
        }
        #6.2 qgcomp fitting and prediction
        gcomp_full_res = with(train_dat,
                              simple_gcomp_real(x, y, z, expnms = colnames(x), bayes = TRUE,family="binomial"))
        gcomp_test_full_dat = data.frame(test_dat$x, test_dat$z)
        gcomp_full_prob=predict(gcomp_full_res$gcomp_fit,newdata=gcomp_test_full_dat)
        gcomp_full_pre = log(gcomp_full_prob/(1-gcomp_full_prob)+1e-15)

        gcomp_full_brier=mean((gcomp_full_prob-test_dat$y)^2)
        gcomp_full_roc =roc(test_dat$y, gcomp_full_pre)
        gcomp_full_auc=as.numeric(gcomp_full_roc$auc)

        betaest$gcomp = gcomp_full_res$gcomp_fit
        mixture.coef$gcomp = summary(gcomp_full_res$gcomp_fit)$coef

        brier = c(brier, gcomp = gcomp_full_brier)
        auc = c(auc, gcomp = gcomp_full_auc)
      }

      method_res[[case_nm]] = list(
        betaest = betaest,
        mixture.coef = mixture.coef,
        brier = brier,
        auc = auc
      )

      if(length(betaest)==0){
        for(mm in 1:length(method))
          if(verbose){
          cat(method[mm],"is not available for this case!\n")
          }
      }

    }

    #############################################################
    if (is.null(input.var.select))
      var.select = FALSE
    if (is.null(input.interaction))
      interaction = FALSE
    if (is.null(input.covariates.forcein))
      covariates.forcein = FALSE

    #example 7: no variable selection and no controlling for z (random forest)
    if (var.select == FALSE &
        interaction == FALSE & covariates.forcein == FALSE) {
      if(verbose){
      case_nm = "case 7: importance ranking among exposures and confounders"
      cat(case_nm, "\n")
      }

      if (is.null(input.method)) {
        method = c("rf")
      } else{
        method = input.method
      }

      ranking = NULL

      if (is.element("rf", method)) {
        if(verbose){
        cat("Fitting rf ... ", "\n")
        }
        rf_res = with(train_dat, simple_randomforest(x.z, y,binary=TRUE))
        rf_prob=predict(rf_res$rf_fit, newdata = test_dat$x.z,type="prob")[,2]
        rf_pre=log(rf_prob/(1-rf_prob)+1e-15)#eta,ers
        rf_brier=mean((rf_prob - test_dat$y)^2)
        rf_roc =roc(test_dat$y, rf_pre)
        rf_auc=as.numeric(rf_roc$auc)

        ranking = sort(rf_res$rf_fit$importance[, 1], decreasing = T)
      }

      method_res[[case_nm]] = list(ranking =
                                     ranking)

      if(length(ranking)==0){
        for(mm in 1:length(method))
          if(verbose){
          cat(method[mm],"is not available for this case!\n")
          }
      }
    }


    #############################################################


    #when z is null
    #example 8: variable selection on x
    if (var.select == TRUE &
        interaction == FALSE &
        covariates.forcein == FALSE & is.null(z) == TRUE) {
      case_nm = "case 8: variable selection on main effects without input confounders"
      if(verbose){
      cat(case_nm, "\n")
      }

      if (is.null(input.method)) {
        method = c("lasso", "enet")
      } else{
        method = input.method
      }

      #output results
      betaest = list()
      brier = c()
      auc= c()

      if (is.element("lasso", method)) {
        if(verbose){
        cat("Fitting lasso ... ", "\n")
        }
        #8.1 lasso fitting and prediction
        lasso_res = with(train_dat, simple_lasso(x, y,family="binomial"))
        lasso_pre = predict(lasso_res$lasso_fit, newx = test_dat$x)
        lasso_prob=predict(lasso_res$lasso_fit, newx = test_dat$x, type = "response")
        lasso_brier=mean((lasso_prob - test_dat$y)^2)
        lasso_roc =roc(test_dat$y, lasso_pre[,1])
        lasso_auc=as.numeric(lasso_roc$auc)


        betaest$lasso = lasso_res$betaest[lasso_res$betaest[, 1] != 0, 1,drop=FALSE]
        brier= c(brier, lasso = lasso_brier)
        auc = c(auc, lasso = lasso_auc)

      }

      if (is.element("enet", method)) {
        if(verbose){
        cat("Fitting enet ... ", "\n")
        }
        #8.2 enet fitting and prediction
        enet_res = with(train_dat, simple_enet(x, y))
        enet_pre = predict(enet_res$enet_fit, newx = test_dat$x)
        enet_prob=predict(enet_res$enet_fit, newx = test_dat$x, type = "response")
        enet_brier=mean((enet_prob - test_dat$y)^2)
        enet_roc =roc(test_dat$y, enet_pre[,1])
        enet_auc=as.numeric(enet_roc$auc)


        betaest$enet = enet_res$betaest[enet_res$betaest[, 1] != 0, 1,drop=FALSE]

        brier = c(brier, enet = enet_brier)
        auc= c(auc, enet = enet_auc)
      }


      method_res[[case_nm]] = list(betaest =betaest,
                                   brier =brier,
                                   auc= auc)

      if(length(betaest)==0){
        for(mm in 1:length(method))
          if(verbose){
          cat(method[mm],"is not available for this case!\n")
          }
      }
    }

    return(method_res)
  }


}
