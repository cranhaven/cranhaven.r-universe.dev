#' Phase I of the FMRCC
#'
#' Performs Phase I of the Functional Mixture Regression Control Chart methodology,
#' which consists of model estimation
#' and control limit calculation using training and tuning datasets.
#'
#' @param Y_train Training response variable. Object of class \code{'mfd'} (dense
#'   functional data) or \code{'list'} (sparse functional data). For dense data,
#'   \code{\link{pca_mfd}} is performed. For sparse data, PACE (Yao et al., 2005)
#'   via \code{\link[fdapace]{FPCA}} is used.
#' @param X_train Training predictor variables. Object of class \code{'mfd'} (dense
#'   functional), \code{'matrix'} (scalar), or \code{'list'} (sparse functional).
#'   For dense data, \code{\link{pca_mfd}} is performed. For sparse data, PACE
#'   (Yao et al., 2005) via \code{\link[fdapace]{FPCA}} is used.
#' @param Y_tun Tuning response variable for control limit calculation. Must be
#'   same type as \code{Y_train}.
#' @param X_tun Tuning predictor variables for control limit calculation. Must be
#'   same type as \code{X_train}.
#' @param FVEy Fraction of variance explained threshold for response variable.
#' @param FVEx Fraction of variance explained threshold for covariates.
#'   Ignored if covariates are scalar.
#' @param studentized Logical. If \code{TRUE}, statistics are studentized. Default is \code{TRUE}.
#' @param alpha Type I error rate for control limit calculation. Default is \code{0.01}.
#' @param intercept Logical. If \code{TRUE}, model includes an intercept. Default is \code{TRUE}.
#' @param init_met Initialization method: \code{'kmeans'} or \code{'random'}. If
#'   \code{'random'}, \code{ninit} initializations are performed and the model with
#'   lowest BIC is retained. Default is \code{'kmeans'}.
#' @param ninit Number of random starts for model estimation. Ignored if
#'   \code{init_met = 'kmeans'}. Default is \code{10}.
#' @param groups Integer vector specifying number of mixture components to consider.
#'   Default is \code{1:5}.
#' @param sigma_par Character vector of covariance parametrizations to consider.
#'   Options are \code{'VVV'} (variable volume, shape, orientation), \code{'EEE'}
#'   (equal volume, shape, orientation), \code{'VII'} (variable volume, spherical),
#'   \code{'EII'} (equal volume, spherical). Default is \code{c('VVV','EEE','VII','EII')}.
#' @param scale Logical. Should dense functional objects be scaled? Default is \code{TRUE}.
#' @param ncompx Integer. Number of principal components to retain for functional
#'   covariates. If \code{NULL}, chosen according to \code{FVEx}. Default is \code{NULL}.
#' @param ncompy Integer. Number of principal components to retain for functional
#'   response. If \code{NULL}, chosen according to \code{FVEy}. Default is \code{NULL}.
#' @param userBwCov Bandwidth for covariance smoothing in PACE. See \code{\link[fdapace]{FPCA}}
#'   for details. Default is \code{NULL}.
#'
#' @return A list containing:
#' \item{model}{The best fitted mixture regression model}
#' \item{phaseI}{Phase I results including control limits}
#' \item{estimate}{Estimation results including values to studentize residuals}
#' \item{fpca}{FPCA results for response and (if applicable) covariates}
#' \item{BIC_plt}{ggplot object showing BIC values across models}
#' \item{studentized}{Logical indicating if studentization was used}
#' \item{intercept}{Logical indicating if intercept was included}
#' \item{type_y}{Character indicating response type ('dense' or 'sparse')}
#' \item{type_x}{Character indicating covariate type ('dense', 'sparse', or 'scalar')}
#'
#' @references
#' Capezza, C., Centofanti, F., Forcina, D., Lepore, A., & Palumbo, B. (2025).
#' Functional Mixture Regression Control Chart. Accepted for publication in \emph{Annals of Applied Statistics}.
#' arXiv:2410.20138.
#'
#' Yao, F., Müller, H. G., & Wang, J. L. (2005). Functional data analysis for sparse
#' longitudinal data. \emph{Journal of the American Statistical Association}, 100(470),
#' 577-590.
#' @seealso \code{\link{FMRCC_PhaseII}}, \code{\link[fdapace]{FPCA}}
#' @import fdapace
#' @importFrom stats predict
#' @export
#'
#' @examples
#' \donttest{
#' # Example with dense functional data
#' # Length of the functional grid
#' l <- 100
#' # Number of observations
#' n <- 300
#'
#' # Generate training in-control data with three equally-sized clusters, maximum dissimilarity
#' data <- simulate_data_fmrcc(n_obs = n, delta_1 = 1, delta_2 = 0.5, len_grid = l, severity = 0)
#' X_train_mfd <- get_mfd_list(data_list = data['X'], n_basis = 20)
#' Y_train_mfd <- get_mfd_list(data_list = data['Y'], n_basis = 20)
#'
#' # Generate tuning in-control data with three equally-sized clusters, maximum dissimilarity
#' data <- simulate_data_fmrcc(n_obs = n, delta_1 = 1, delta_2 = 0.5, len_grid = l, severity = 0)
#' X_tun_mfd <- get_mfd_list(data_list = data['X'], n_basis = 20)
#' Y_tun_mfd <- get_mfd_list(data_list = data['Y'], n_basis = 20)
#'
#' # Example with dense functional data
#' phaseI_results <- FMRCC_PhaseI(
#'   Y_train = Y_train_mfd,
#'   X_train = X_train_mfd,
#'   Y_tun = Y_tun_mfd,
#'   X_tun = X_tun_mfd,
#'   FVEy = 0.95,
#'   FVEx = 0.90,
#'   alpha = 0.01,
#'   groups = 1:3,
#'   sigma_par = c('VVV', 'EEE')
#' )
#'
#' # View BIC plot
#' phaseI_results$BIC_plt
#' }
FMRCC_PhaseI <- function(Y_train,
                         X_train,
                         Y_tun,
                         X_tun,
                         FVEy,
                         FVEx,
                         studentized = T,
                         alpha = 0.01 ,
                         intercept = T,
                         init_met = 'kmeans',
                         ninit = 10,
                         groups = 1:5,
                         sigma_par = c('VVV', 'EEE', 'VII', 'EII'),
                         scale = T,
                         ncompx = NULL ,
                         ncompy = NULL,
                         userBwCov = NULL) {
  type_y <- 'dense'
  if (!is.mfd(Y_train))
    type_y <- 'sparse'
  if (type_y == 'sparse') {
    if (is.mfd(Y_tun))
      stop('Y_tun must be of the same type as Y_train')
  }
  if (type_y == 'dense') {
    if (!is.mfd(Y_tun))
      stop('Y_tun must be of the same type as Y_train')
  }

  type_x <- 'sparse'
  if (is.matrix(X_train))
    type_x <- 'scalar'
  if (is.mfd(X_train))
    type_x <- 'dense'
  if (type_x == 'scalar') {
    if (!is.matrix(X_tun))
      stop('X_tun must be of the same type as X_train')
  } else if (type_x == 'dense') {
    if (!is.mfd(X_tun))
      stop('X_tun must be of the same type as X_train')
  } else{
    if (!is.list(X_tun))
      stop('X_tun must be of the same type as X_train')
  }

  if (type_y == 'dense') {
    pca_y <- pca_mfd(mfdobj = Y_train , scale = scale)
    components_enough_var <- cumsum(pca_y$varprop) > FVEy
    ncomponents <- which(cumsum(pca_y$varprop) > FVEy)[1]
    if (is.na(ncomponents))
      ncomponents <- length(pca_y$varprop)
    if (!is.null(ncompy))
      ncomponents <- ncompy
    components <- 1:ncomponents
    if (!scale)
      pca_y$scale_fd <- scale
    fpca_results <- list(pcay = pca_y, ncomponents_y = ncomponents)

    score_y <-  as.matrix(fpca_results$pcay$pcscores[, 1:fpca_results$ncomponents_y])
  } else if (type_y == 'sparse') {
    pca_y <- fdapace::FPCA(
      Y_train$Ly,
      Y_train$Lt,
      list(
        dataType = 'Sparse',
        FVEthreshold = FVEy,
        userBwCov = userBwCov
      )
    )
    if (!is.null(ncompy)) {
      pca_y$xiEst <- pca_y$xiEst[, 1:ncompy]
      pca_y$lambda <- pca_y$lambda[1:ncompy]
      pca_y$phi <- pca_y$phi[, 1:ncompy]
      pca_y$selectK <- ncompy
    }


    score_y <- pca_y$xiEst
    fpca_results <- list(pcay = pca_y)
  }

  if (type_x == 'dense') {
    pca_x <- pca_mfd(mfdobj = X_train , scale = scale)
    components_enough_var <- cumsum(pca_x$varprop) > FVEx
    ncomponents_x <- which(cumsum(pca_x$varprop) > FVEx)[1]
    if (is.na(ncomponents_x))
      ncomponents_x <- length(pca_x$varprop)
    if (!is.null(ncompx))
      ncomponents_x <- ncompx
    components_x <- 1:ncomponents_x
    if (!scale)
      pca_x$scale_fd <- scale
    fpca_results <- list(
      pcay = pca_y,
      ncomponents_y = ncomponents,
      pcax = pca_x,
      ncomponents_x = ncomponents_x
    )
    score_x <- as.matrix(fpca_results$pcax$pcscores[, 1:fpca_results$ncomponents_x])
  } else if (type_x == 'sparse') {
    pca_x <- FPCA(
      X_train$Ly,
      X_train$Lt,
      list(
        dataType = 'Sparse',
        FVEthreshold = FVEx,
        userBwCov = userBwCov
      )
    )
    if (!is.null(ncompx)) {
      pca_x$xiEst <- pca_x$xiEst[, 1:ncompx]
      pca_x$lambda <- pca_x$lambda[1:ncompx]
      pca_x$phi <- pca_x$phi[, 1:ncompx]
      pca_x$selectK <- ncompx
    }

    score_x <- pca_x$xiEst
    fpca_results <- list(pcay = pca_y, pcax = pca_x)
  } else{
    score_x <- as.matrix(X_train)
  }


  if (init_met == 'kmeans')
    ninit <- 1

  estimate <- estimate_mixture(
    y = score_y ,
    x = score_x ,
    ninit = ninit ,
    groups = groups ,
    mode = 'regression',
    intercept = intercept ,
    init_met = init_met,
    sigma_par = sigma_par
  )

  best_model <- estimate$best_model
  num_groups <- length(best_model$prop)

  phaseI_fmrcc <- function(Y_tun = NULL ,
                           X_tun = NULL ,
                           fpca_results = NULL ,
                           model_estimate = NULL ,
                           alpha = NULL ,
                           intercept = T,
                           studentized = F,
                           type_quantile = 7,
                           kde = F,
                           posterior = F,
                           type_y,
                           type_x) {
    if (type_y == 'dense') {
      # Standardization and score calculation
      y_tuning_std_mfd <- scale_mfd(
        mfdobj = Y_tun ,
        center = fpca_results$pcay$center_fd ,
        scale = fpca_results$pcay$scale_fd
      )
      y_score_new <- get_scores(
        pca = fpca_results$pcay ,
        components = 1:fpca_results$ncomponents_y ,
        newdata_scaled = y_tuning_std_mfd
      )
    } else if (type_y == 'sparse') {
      y_score_new <- predict(object = fpca_results$pcay,
                             newLy = Y_tun$Ly,
                             newLt = Y_tun$Lt)$scores
    } else{
      stop('Type_y incorrect')
    }

    if (type_x == 'dense') {
      x_tuning_std_mfd <- scale_mfd(
        mfdobj = X_tun ,
        center = fpca_results$pcax$center_fd ,
        scale = fpca_results$pcax$scale_fd
      )
      x_score_new <- get_scores(
        pca = fpca_results$pcax ,
        components = 1:fpca_results$ncomponents_x ,
        newdata_scaled = x_tuning_std_mfd
      )
    } else if (type_x == 'sparse') {
      x_score_new <- predict(object = fpca_results$pcax,
                             newLy = X_tun$Ly,
                             newLt = X_tun$Lt)$scores
    } else if (type_x == 'scalar') {
      x_score_new <- as.matrix(X_tun)
    } else{
      stop('Type_x incorrect')
    }

    if (intercept) {
      x_score_new <- cbind(rep(1, nrow(x_score_new)), x_score_new)
    }

    best_model <- model_estimate$best_model
    p <- ncol(y_score_new)
    num_groups <- length(best_model$prop)

    if (studentized) {
      # Tuning Residuals variance
      I_tun <- diag(1, nrow = nrow(y_score_new))
      c_tuning <- list(num_groups)
      for (kk in 1:num_groups) {
        c_tuning[[kk]] <- as.numeric(diag(
          I_tun + x_score_new %*% model_estimate$H[[kk]] %*% t(x_score_new)
        ))
      }
      c_tuning <- sapply(c_tuning , cbind)
      compsum_stud <- matrix(nrow = nrow(y_score_new) , ncol = num_groups)

      if (posterior) {
        post <- predict_mixture(y = y_score_new ,
                                x = x_score_new ,
                                model = best_model)$membership
        for (ii in 1:nrow(y_score_new)) {
          comp <- lapply(1:num_groups, function(i) {
            lk <-
              diag((det(
                c_tuning[ii, i] * best_model$Sigma[[i]]
              )^0.5) * exp(
                -0.5 * (y_score_new[ii, ] - x_score_new[ii, ] %*% best_model$B[[i]]) %*% solve(c_tuning[ii, i] * best_model$Sigma[[i]]) %*% t(y_score_new[ii, ] - x_score_new[ii, ] %*% best_model$B[[i]])
              ))
            post[ii, i] * lk
          })
          compsum_stud[ii, ] <- sapply(comp, cbind)
        }
        comp <- compsum_stud
      } else{
        for (ii in 1:nrow(y_score_new)) {
          comp <- lapply(1:num_groups, function(i) {
            lk <-
              diag((det(
                c_tuning[ii, i] * best_model$Sigma[[i]]
              )^0.5) * exp(
                -0.5 * (y_score_new[ii, ] - x_score_new[ii, ] %*% best_model$B[[i]]) %*% solve(c_tuning[ii, i] *
                                                                                                 best_model$Sigma[[i]]) %*% t(y_score_new[ii, ] - x_score_new[ii, ] %*% best_model$B[[i]])
              ))
            best_model$prop[i] * lk
          })
          compsum_stud[ii, ] <- sapply(comp, cbind)
        }
        comp <- compsum_stud
      }
    } else{
      if (posterior) {
        compsum <- matrix(nrow = nrow(y_score_new) , ncol = num_groups)
        post <- predict_mixture(y = y_score_new ,
                                x = x_score_new ,
                                model = best_model)$membership
        for (ii in 1:nrow(y_score_new)) {
          comp <- lapply(1:num_groups, function(i) {
            lk <-
              diag((det(best_model$Sigma[[i]])^0.5) * exp(
                -0.5 * (y_score_new[ii, ] - x_score_new[ii, ] %*% best_model$B[[i]]) %*% solve(best_model$Sigma[[i]]) %*%
                  t(y_score_new[ii, ] - x_score_new[ii, ] %*% best_model$B[[i]])
              ))
            post[ii, i] * lk
          })
          compsum[ii, ] <- sapply(comp, cbind)
        }
        comp <- compsum
      } else{
        comp <- lapply(1:num_groups, function(i) {
          lk <-
            diag((det(best_model$Sigma[[i]])^0.5) * exp(
              -0.5 * (y_score_new - x_score_new %*% best_model$B[[i]]) %*% solve(best_model$Sigma[[i]]) %*%
                t(y_score_new - x_score_new %*% best_model$B[[i]])
            ))
          best_model$prop[i] * lk
        })
        comp <- sapply(comp, cbind)

      }
    }
    compsum <- log(apply(comp, 1, sum))
    if (kde) {
      dens <- stats::density(-compsum, n = 1024)
      cdf_vals <- cumsum(dens$y) / sum(dens$y)  # normalize to get CDF
      quantile_kde <- stats::approxfun(cdf_vals, dens$x)
      lim <- quantile_kde(1 - alpha)

    } else{
      lim <- stats::quantile(-compsum, 1 - alpha, type = type_quantile)
    }


    status <- numeric(nrow(y_score_new))
    status[which(-compsum > lim)] <- 'OC'
    status[which(-compsum <= lim)] <- 'IC'
    df <- data.frame(
      id = 1:nrow(y_score_new),
      loglikelihood = -compsum ,
      status = status
    )
    return(
      list(
        lim = lim,
        df = df,
        studentized = studentized,
        posterior = posterior,
        type_x = type_x,
        type_y = type_y
      )
    )
  }


  phaseI <- phaseI_fmrcc(
    Y_tun = Y_tun,
    X_tun = X_tun ,
    fpca_results = fpca_results ,
    model_estimate = estimate ,
    alpha = alpha ,
    intercept = intercept ,
    studentized = studentized,
    type_quantile = 7,
    kde = F,
    posterior = F,
    type_y = type_y,
    type_x = type_x
  )

  plt <- NULL
  mod <- estimate
  a <- c(as.matrix(mod$BIC))
  a[which(a == 1000000)] <- NA
  b <- rep(sigma_par, length(groups))
  c <- rep(groups, each = length(sigma_par))
  n_clust <- NULL
  method <- NULL
  BIC <- data.frame(BIC = a ,
                    method = b ,
                    n_clust = c)
  plt <- ggplot2::ggplot(BIC ,
                         ggplot2::aes(
                           x = n_clust,
                           y = BIC ,
                           col = method,
                           shape = method
                         )) +
    ggplot2::geom_point(size = 3) +
    ggplot2::scale_shape_manual(values = c(16, 15, 17, 18)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::theme_bw()

  return(
    list(
      model = best_model,
      phaseI = phaseI,
      estimate = estimate,
      fpca = fpca_results,
      BIC_plt = plt,
      studentized = studentized,
      intercept = intercept,
      type_y = type_y,
      type_x = type_x
    )
  )
}


#' Phase II of the FMRCC
#'
#' Performs Phase II of the FMRCC methodology.
#'
#' @param Y_test Test response variable. Must be same type as training data
#'   (\code{mfd} for dense or \code{list} for sparse functional data).
#' @param X_test Test predictor variables. Must be same type as training data
#'   (\code{mfd}, \code{matrix}, or \code{list}).
#' @param phaseI Output from \code{\link{FMRCC_PhaseI}} containing the trained
#'   model and parameters.
#'
#' @return A list containing:
#' \item{ARL}{Average Run Length}
#' \item{phaseII}{Detailed Phase II results, a list with:
#'   \itemize{
#'     \item \code{df}: Data frame with columns:
#'       \itemize{
#'         \item \code{id}: Observation identifier
#'         \item \code{loglikelihood}: Log-likelihood statistic for each observation
#'         \item \code{status}: 'IC' (in-control) or 'OC' (out-of-control)
#'       }
#'     \item \code{ARL}: Average Run Length value
#'   }
#' }
#'
#' @seealso \code{\link{FMRCC_PhaseI}}
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Length of the functional grid
#' l <- 100
#' # Number of observations
#' n <- 300
#'
#' # Generate training in-control data with three equally-sized clusters, maximum dissimilarity
#' data <- simulate_data_fmrcc(n_obs = n, delta_1 = 1, delta_2 = 0.5, len_grid = l, severity = 0)
#' X_train_mfd <- get_mfd_list(data_list = data['X'], n_basis = 20)
#' Y_train_mfd <- get_mfd_list(data_list = data['Y'], n_basis = 20)
#'
#' # Generate tuning in-control data with three equally-sized clusters, maximum dissimilarity
#' data <- simulate_data_fmrcc(n_obs = n, delta_1 = 1, delta_2 = 0.5, len_grid = l, severity = 0)
#' X_tun_mfd <- get_mfd_list(data_list = data['X'], n_basis = 20)
#' Y_tun_mfd <- get_mfd_list(data_list = data['Y'], n_basis = 20)
#'
#' # Example with dense functional data
#' phaseI_results <- FMRCC_PhaseI(
#'   Y_train = Y_train_mfd,
#'   X_train = X_train_mfd,
#'   Y_tun = Y_tun_mfd,
#'   X_tun = X_tun_mfd,
#'   FVEy = 0.95,
#'   FVEx = 0.90,
#'   alpha = 0.01,
#'   groups = 1:3,
#'   sigma_par = c('VVV', 'EEE')
#' )
#'
#' # View BIC plot
#' phaseI_results$BIC_plt
#'
#' # Generate out-of-control data with three equally-sized clusters, maximum dissimilarity
#' data <- simulate_data_fmrcc(n_obs = n, delta_1 = 1, delta_2 = 0.5, len_grid = l, severity = 2)
#' X_test_mfd <- get_mfd_list(data_list = data['X'], n_basis = 20)
#' Y_test_mfd <- get_mfd_list(data_list = data['Y'], n_basis = 20)
#'
#' # Perform the monitoring of the Phase II data
#' phaseII_results <- FMRCC_PhaseII(
#'   Y_test = Y_test_mfd,
#'   X_test = X_test_mfd,
#'   phaseI = phaseI_results
#' )
#'
#' # Check Average Run Length
#' phaseII_results$ARL
#'
#' # View monitoring results
#' head(phaseII_results$phaseII$df)
#'
#' # Identify out-of-control observations
#' oc_observations <- phaseII_results$phaseII$df[phaseII_results$phaseII$df$status == 'OC',]
#' oc_observations
#' }
FMRCC_PhaseII <- function(Y_test, X_test, phaseI) {
  studentized <- phaseI$studentized
  intercept <- phaseI$intercept

  phaseII_fmrcc <- function(Y_test = NULL ,
                            X_test = NULL ,
                            fpca_results = NULL ,
                            model_estimate = NULL ,
                            limit = NULL ,
                            intercept = T ,
                            studentized = F,
                            posterior = F,
                            type_y ,
                            type_x) {
    if (type_y == 'dense') {
      y_testing_std_mfd <- scale_mfd(
        mfdobj = Y_test ,
        center = fpca_results$pcay$center_fd ,
        scale = fpca_results$pcay$scale_fd
      )
      y_score_new <- get_scores(
        pca = fpca_results$pcay ,
        components = 1:fpca_results$ncomponents_y ,
        newdata_scaled = y_testing_std_mfd
      )
    } else if (type_y == 'sparse') {
      y_score_new <- predict(object = fpca_results$pcay,
                             newLy = Y_test$Ly,
                             newLt = Y_test$Lt)$scores
    } else{
      stop('Type_y incorrect')
    }

    if (type_x == 'dense') {
      x_testing_std_mfd <- scale_mfd(
        mfdobj = X_test ,
        center = fpca_results$pcax$center_fd ,
        scale = fpca_results$pcax$scale_fd
      )
      x_score_new <- get_scores(
        pca = fpca_results$pcax ,
        components = 1:fpca_results$ncomponents_x ,
        newdata_scaled = x_testing_std_mfd
      )
    } else if (type_x == 'scalar') {
      x_score_new <- as.matrix(X_test)
    } else if (type_x == 'sparse') {
      x_score_new <- predict(object = fpca_results$pcax,
                             newLy = X_test$Ly,
                             newLt = X_test$Lt)$scores
    } else{
      stop('Type_x incorrect')
    }
    if (intercept) {
      x_score_new <- cbind(rep(1, nrow(x_score_new)), x_score_new)
    }
    best_model <- model_estimate$best_model
    p <- ncol(y_score_new)
    num_groups <- length(best_model$prop)

    if (studentized) {
      I_tun <- diag(1, nrow = nrow(y_score_new))
      c_testing <- list(num_groups)

      for (kk in 1:num_groups) {
        c_testing[[kk]] <- as.numeric(diag(
          I_tun + x_score_new %*% model_estimate$H[[kk]] %*% t(x_score_new)
        ))
      }

      c_testing <- sapply(c_testing , cbind)
      compsum_stud <- matrix(nrow = nrow(y_score_new) , ncol = num_groups)

      if (posterior) {
        post <- predict_mixture(y = y_score_new ,
                                x = x_score_new ,
                                model = best_model)$membership
        for (ii in 1:nrow(y_score_new)) {
          comp <- lapply(1:num_groups, function(i) {
            lk <-
              diag((det(
                c_testing[ii, i] * best_model$Sigma[[i]]
              )^0.5) * exp(
                -0.5 * (y_score_new[ii, ] - x_score_new[ii, ] %*% best_model$B[[i]]) %*% solve(c_testing[ii, i] *
                                                                                                 best_model$Sigma[[i]]) %*% t(y_score_new[ii, ] - x_score_new[ii, ] %*% best_model$B[[i]])
              ))
            post[ii, i] * lk
          })
          compsum_stud[ii, ] <- sapply(comp, cbind)
        }
      } else{
        for (ii in 1:nrow(y_score_new)) {
          comp <- lapply(1:num_groups, function(i) {
            lk <-
              diag((det(
                c_testing[ii, i] * best_model$Sigma[[i]]
              )^0.5) * exp(
                -0.5 * (y_score_new[ii, ] - x_score_new[ii, ] %*% best_model$B[[i]]) %*% solve(c_testing[ii, i] *
                                                                                                 best_model$Sigma[[i]]) %*% t(y_score_new[ii, ] - x_score_new[ii, ] %*% best_model$B[[i]])
              ))
            best_model$prop[i] * lk
          })
          compsum_stud[ii, ] <- sapply(comp, cbind)
        }
      }

      comp <- compsum_stud

    } else{
      if (posterior) {
        compsum <- matrix(nrow = nrow(y_score_new) , ncol = num_groups)
        post <- predict_mixture(y = y_score_new ,
                                x = x_score_new ,
                                model = best_model)$membership
        for (ii in 1:nrow(y_score_new)) {
          comp <- lapply(1:num_groups, function(i) {
            lk <-
              diag((det(best_model$Sigma[[i]])^0.5) * exp(
                -0.5 * (y_score_new[ii, ] - x_score_new[ii, ] %*% best_model$B[[i]]) %*% solve(best_model$Sigma[[i]]) %*%
                  t(y_score_new[ii, ] - x_score_new[ii, ] %*% best_model$B[[i]])
              ))
            post[ii, i] * lk
          })
          compsum[ii, ] <- sapply(comp, cbind)
        }
        comp <- compsum
      } else{
        comp <- lapply(1:num_groups, function(i) {
          lk <-
            diag((det(best_model$Sigma[[i]])^0.5) * exp(
              -0.5 * (y_score_new - x_score_new %*% best_model$B[[i]]) %*% solve(best_model$Sigma[[i]]) %*%
                t(y_score_new - x_score_new %*% best_model$B[[i]])
            ))
          best_model$prop[i] * lk
        })
        comp <- sapply(comp, cbind)
      }
    }
    compsum <- log(apply(comp, 1, sum))
    phase_II <- function(loglikelihood = NULL ,
                         limit = NULL) {
      status <- (loglikelihood > limit)
      status[which(status == T)] <- 'OC'
      status[which(status == F)] <- 'IC'
      df <- data.frame(
        id = 1:length(status),
        loglikelihood = loglikelihood,
        status = status
      )
      alpha <- sum(status == 'OC') / length(status)
      ARL <- 1 / (1 - (1 - alpha))
      return(list(df = df, ARL = ARL))

    }
    phaseII_list <- phase_II(loglikelihood = -compsum, limit = limit)
    return(phaseII_list)
  }

  phaseII <- phaseII_fmrcc(
    Y_test = Y_test ,
    X_test = X_test ,
    fpca_results = phaseI$fpca ,
    model_estimate = phaseI$estimate ,
    limit = phaseI$phaseI$lim,
    intercept = intercept ,
    studentized = studentized,
    posterior = F,
    type_y = phaseI$type_y,
    type_x = phaseI$type_x
  )
  ARL <- phaseII$ARL
  return(list(ARL = ARL, phaseII = phaseII))
}

#' Simulate Data for Functional Mixture Regression Control Chart (FMRCC)
#'
#' @description
#' #' @description
#' Generates synthetic in-control and out-of-control functional data for testing the Functional Mixture Regression
#' Control Chart (FMRCC) framework. The function simulates a functional response Y
#' influenced by a functional covariate X through a mixture of functional linear models
#' (FLMs) with three distinct regression structures, as described in Section 3.1 of
#' Capezza et al. (2025).
#'
#' @param n_obs Integer. Total number of observations to generate. Default is 3000.
#' @param mixing_prop Numeric vector of length 3. Mixing proportions for the three
#'   clusters (must sum to 1). Default is c(1/3, 1/3, 1/3).
#' @param len_grid Integer. Number of grid points for evaluating functional data on
#'   domain \[0,1\]. Default is 500.
#' @param SNR Numeric. Signal-to-noise ratio controlling the variance of the error term.
#'   Default is 4.
#' @param shift_coef Numeric vector of length 4 or character string. Controls the type
#'   and shape of the mean shift:
#'   \itemize{
#'     \item Numeric vector: Coefficients c(a3, a2, a1, a0) for polynomial shift:
#'       \eqn{Shift(t) = severity \times (a_3 t^3 + a_2 t^2 + a_1 t + a_0)}
#'     \item 'low': Applies a "low" shift pattern based on RSW dynamic resistance curves
#'     \item 'high': Applies a "high" shift pattern based on RSW dynamic resistance curves
#'   }
#'   Default is c(0,0,0,0) (no shift).
#' @param severity Numeric. Multiplier controlling the magnitude of the shift. Higher
#'   values produce larger shifts. This corresponds to the "Severity Level (SL)" in the
#'   simulation study.
#'   Default is 0 (no shift).
#' @param ncompx Integer. Number of functional principal components used to generate the
#'   functional covariate X. Default is 20.
#' @param delta_1 Numeric in \[0,1\]. Controls dissimilarity between clusters in regression
#'   coefficient functions and functional intercepts (analogous to delta_1 in
#'   simulate_data_fmrcc). Required parameter with no default.
#' @param delta_2 Numeric in \[0,1\]. Controls the relative contribution of functional
#'   intercept vs. regression coefficient function (analogous to delta_2 in
#'   simulate_data_fmrcc). Required parameter with no default.
#' @param measurement_noise_sigma Numeric. Standard deviation of Gaussian measurement
#'   error added to both X and Y. Default is 0 (no measurement error).
#' @param fun_noise Character. Distribution for functional error term. Options:
#'   \itemize{
#'     \item \code{'normal'}: Gaussian errors (default)
#'     \item \code{'t'}: Student's t-distribution errors with df degrees of freedom
#'     \item \code{'skewnormal'}: Skew-normal distribution with skewness parameter alphasn
#'   }
#' @param df Numeric. Degrees of freedom for Student's t-distribution when
#'   \code{fun_noise = 't'}. Default is 3.
#' @param alphasn Numeric. Skewness parameter for skew-normal distribution when
#'   \code{fun_noise = 'skewnormal'}. Default is 4.
#'
#' @return A list containing:
#'   \item{X}{Matrix (\code{len_grid} \eqn{\times} \code{n_obs}) of functional covariate observations.}
#'   \item{Y}{Matrix (\code{len_grid} \eqn{\times} \code{n_obs}) of shifted functional response observations.}
#'   \item{Eps_1, Eps_2, Eps_3}{Matrices of functional error terms for each cluster.}
#'   \item{beta_matrix_1, beta_matrix_2, beta_matrix_3}{Matrices (\code{len_grid} \eqn{\times} \code{len_grid})
#'     containing the bivariate regression coefficient functions \eqn{\beta^X_k(s,t)} for k=1,2,3.}
#'
#' @details
#' The data generation follows Equation (18) in the paper:
#' \deqn{Y(t) = (1 - \Delta_2)\beta^0_k(t) + \int_S \Delta_2(\beta^X_k(s,t))^T X(s)ds + \varepsilon(t)}
#'
#' The three clusters are characterized by:
#' \itemize{
#'   \item Different functional intercepts \eqn{\beta^0_k(t)} (inspired by dynamic resistance
#'     curves in RSW processes)
#'   \item Different bivariate regression coefficient functions \eqn{\beta^X_k(s,t)}
#'   \item Functional errors with variance adjusted to achieve the specified SNR
#' }
#'
#'Moreover, when when \code{severity != 0}, it applies a controlled shift to the functional response Y to
#' simulate out-of-control conditions. The shift types include:
#'
#' \strong{Polynomial shifts:} When \code{shift_coef} is numeric, a polynomial of degree 3 is
#' applied: \eqn{Shift(t) = severity \times (a_3 t^3 + a_2 t^2 + a_1 t + a_0)}
#'
#' \strong{Linear shift example:} \code{shift_coef = c(0, 0, 1, 0)} produces a linear shift
#'
#' \strong{Quadratic shift example:} \code{shift_coef = c(0, 1, 0, 0)} produces a quadratic shift
#'
#' \strong{RSW-specific shifts:} When \code{shift_coef = 'low'} or \code{'high'}, the function applies
#' shifts based on modifications to the dynamic resistance curve (DRC) parameters,
#' simulating realistic fault patterns in resistance spot welding processes.


#' The functional covariate X is generated using functional principal component analysis
#' with standardized magnitudes (scaled by 1/5).
#'
#' @references
#' Capezza, C., Centofanti, F., Forcina, D., Lepore, A., and Palumbo, B. (2025).
#' Functional Mixture Regression Control Chart. Annals of Applied Statistics.
#'
#' @examples
#' \donttest{
#' # Generate in-control data with three equally-sized clusters, maximum dissimilarity
#' data <- simulate_data_fmrcc(n_obs = 300, delta_1 = 1, delta_2 = 0.5, severity = 0)
#'
#' # In-control single cluster case (delta_1 = 0)
#' data_single <- simulate_data_fmrcc(n_obs = 300, delta_1 = 0, delta_2 = 0.5, severity = 0)
#'
#' # In-control clusters differing only in regression coefficients
#' data_beta_only <- simulate_data_fmrcc(n_obs = 300, delta_1 = 1, delta_2 = 1, severity = 0)
#'
#' # Add measurement noise and use t-distributed errors
#' data_t_noise <- simulate_data_fmrcc(n_obs = 300, delta_1 = 1, delta_2 = 0.5, severity = 0,
#'                                     measurement_noise_sigma = 0.01,
#'                                     fun_noise = 't', df = 5)
#'
#' # Generate out-of-control data with linear shift
#' data_oc <- simulate_data_fmrcc(n_obs = 300,
#'                                shift_coef = c(0, 0, 1, 0),
#'                                severity = 2,
#'                                delta_1 = 1,
#'                                delta_2 = 0.5)
#'
#' # Generate OC data with quadratic shift
#' data_quad <- simulate_data_fmrcc(n_obs = 300,
#'                                  shift_coef = c(0, 1, 0, 0),
#'                                  severity = 3,
#'                                  delta_1 = 1,
#'                                  delta_2 = 0.5)
#'
#' # Generate OC data with RSW-specific "low" shift pattern
#' data_rsw_low <- simulate_data_fmrcc(n_obs = 300,
#'                                     shift_coef = 'low',
#'                                     severity = 1.5,
#'                                     delta_1 = 1,
#'                                     delta_2 = 0.5)
#'
#' # Generate OC data with RSW-specific "high" shift pattern
#' data_rsw_high <- simulate_data_fmrcc(n_obs = 300,
#'                                      shift_coef = 'high',
#'                                      severity = 2,
#'                                      delta_1 = 0.66,
#'                                      delta_2 = 0.5)
#' }
#'
#' @export
simulate_data_fmrcc <- function(n_obs = 3000,
                                mixing_prop = c(1 / 3, 1 / 3, 1 / 3),
                                len_grid = 500,
                                SNR = 4,
                                shift_coef = c(0, 0, 0, 0),
                                severity = 0,
                                ncompx = 20,
                                delta_1,
                                delta_2,
                                measurement_noise_sigma = 0,
                                fun_noise = 'normal',
                                df = 3,
                                alphasn = 4) {
  if (!is.numeric(n_obs) ||
      length(n_obs) != 1 || n_obs <= 0 || n_obs != floor(n_obs)) {
    stop("'n_obs' must be a positive integer.")
  }
  if (!is.numeric(mixing_prop) || length(mixing_prop) != 3) {
    stop("'mixing_prop' must be a numeric vector of length 3.")
  }
  if (any(mixing_prop < 0) || any(mixing_prop > 1)) {
    stop("All elements of 'mixing_prop' must be between 0 and 1.")
  }
  if (abs(sum(mixing_prop) - 1) > 1e-10) {
    stop("Elements of 'mixing_prop' must sum to 1.")
  }
  if (!is.numeric(len_grid) ||
      length(len_grid) != 1 ||
      len_grid <= 0 || len_grid != floor(len_grid)) {
    stop("'len_grid' must be a positive integer.")
  }
  if (len_grid < 10) {
    warning("'len_grid' is very small (< 10). Results may be unreliable.")
  }
  if (!is.numeric(SNR) || length(SNR) != 1 || SNR <= 0) {
    stop("'SNR' must be a positive numeric value.")
  }
  valid_shift_types <- c('low', 'high')
  if (is.character(shift_coef)) {
    if (length(shift_coef) != 1 ||
        !(shift_coef %in% valid_shift_types)) {
      stop(paste0(
        "When 'shift_coef' is character, it must be one of: ",
        paste(valid_shift_types, collapse = ", ")
      ))
    }
  } else if (is.numeric(shift_coef)) {
    if (length(shift_coef) != 4) {
      stop(
        "When 'shift_coef' is numeric, it must be a vector of length 4 (polynomial coefficients)."
      )
    }
  } else {
    stop(
      "'shift_coef' must be either a numeric vector of length 4 or a character string ('low' or 'high')."
    )
  }
  if (!is.numeric(severity) || length(severity) != 1) {
    stop("'severity' must be a single numeric value.")
  }
  if (severity < 0) {
    warning("'severity' is negative. This will reverse the direction of the shift.")
  }
  if (!is.numeric(ncompx) ||
      length(ncompx) != 1 || ncompx <= 0 || ncompx != floor(ncompx)) {
    stop("'ncompx' must be a positive integer.")
  }
  if (missing(delta_1)) {
    stop("'delta_1' is required and must be specified.")
  }
  if (!is.numeric(delta_1) ||
      length(delta_1) != 1 || delta_1 < 0 || delta_1 > 1) {
    stop("'delta_1' must be a numeric value between 0 and 1.")
  }
  if (missing(delta_2)) {
    stop("'delta_2' is required and must be specified.")
  }
  if (!is.numeric(delta_2) ||
      length(delta_2) != 1 || delta_2 < 0 || delta_2 > 1) {
    stop("'delta_2' must be a numeric value between 0 and 1.")
  }
  if (!is.numeric(measurement_noise_sigma) ||
      length(measurement_noise_sigma) != 1 ||
      measurement_noise_sigma < 0) {
    stop("'measurement_noise_sigma' must be a non-negative numeric value.")
  }
  valid_noise_types <- c('normal', 't', 'skewnormal')
  if (!is.character(fun_noise) ||
      length(fun_noise) != 1 || !(fun_noise %in% valid_noise_types)) {
    stop(paste0(
      "'fun_noise' must be one of: ",
      paste(valid_noise_types, collapse = ", ")
    ))
  }
  if (!is.numeric(df) || length(df) != 1 || df <= 0) {
    stop("'df' must be a positive numeric value.")
  }
  if (fun_noise == 't' && df < 1) {
    warning("'df' < 1 for t-distribution may produce unreliable results.")
  }
  if (!is.numeric(alphasn) || length(alphasn) != 1) {
    stop("'alphasn' must be a numeric value.")
  }
  min_obs_per_cluster <- min(n_obs * mixing_prop)
  if (min_obs_per_cluster < 10) {
    warning(
      paste0(
        "At least one cluster will have fewer than 10 observations (",
        round(min_obs_per_cluster),
        "). Consider increasing 'n_obs' or adjusting 'mixing_prop'."
      )
    )
  }
  k <- length(mixing_prop)
  length_tot <- len_grid
  grid_s <- grid_t <- seq(0, 1, length.out = length_tot)
  mixing_prop <- matrix(mixing_prop, nrow = 1)
  domain <- c(0, 1)

  # generate X --------------------------------------------------------------

  # n_obs_g <- max(10000,n_obs)
  n_obs_g <- n_obs

  X_fd <- simulate_x(nobs = n_obs_g ,
                     n_comp = ncompx ,
                     gamma = 1)
  X_eval <- (1 / 5) * fda::eval.fd(grid_s , X_fd)[, , 1]

  # Generate ERROR ----------------------------------------------------------

  n_basis_eps <- 20
  eps_basis <- fda::create.bspline.basis(domain, norder = 4, nbasis = n_basis_eps)
  eps_coef <- vector(mode = "list", length = k)
  eps_fd <- vector(mode = "list", length = k)
  Eps <- vector(mode = "list", length = k)

  if (fun_noise == 'normal') {
    for (kk in 1:k) {
      eps_coef[[kk]] <- matrix(
        stats::rnorm(round(n_obs_g * mixing_prop[kk]) * n_basis_eps, mean = 0),
        nrow = n_basis_eps,
        ncol = round(n_obs_g * mixing_prop[kk])
      )
      eps_fd[[kk]] <- fda::fd(eps_coef[[kk]], eps_basis)
      Eps[[kk]] <- fda::eval.fd(grid_t, eps_fd[[kk]])
    }
  } else if (fun_noise == 't') {
    for (kk in 1:k) {
      eps_coef[[kk]] <- matrix(
        stats::rt(round(n_obs_g * mixing_prop[kk]) * n_basis_eps, df = df),
        nrow = n_basis_eps,
        ncol = round(n_obs_g * mixing_prop[kk])
      )
      eps_fd[[kk]] <- fda::fd(eps_coef[[kk]], eps_basis)
      Eps[[kk]] <- fda::eval.fd(grid_t, eps_fd[[kk]])
    }
  } else if (fun_noise == 'skewnormal') {
    for (kk in 1:k) {
      eps_coef[[kk]] <- matrix(
        sn::rsn(
          n = round(n_obs_g * mixing_prop[kk]) * n_basis_eps,
          alpha = alphasn
        ),
        nrow = n_basis_eps,
        ncol = round(n_obs_g * mixing_prop[kk])
      )
      eps_coef[[kk]] <- eps_coef[[kk]] - mean(eps_coef[[kk]])

      eps_fd[[kk]] <- fda::fd(eps_coef[[kk]], eps_basis)
      Eps[[kk]] <- fda::eval.fd(grid_t, eps_fd[[kk]])
    }
  } else{
    cat('fun_noise should be normal, t or skewnormal')
    stop()
  }


  # Define beta -----------------------------------------------------------

  beta_1 <- function(s, t) {
    a = 0.3
    b = 0.3
    c = 0.3
    d = 0.3
    f_1 <- function(s, t) {
      (((t - 0.5) / c)^3 + ((s - 0.5) / d)^3 + ((t - 0.5) / b)^2 - ((s - 0.5) /
                                                                      a)^2 + 5)
    }
    z <- outer(s, t, f_1)
    z
  }

  beta_2 <- function(s, t) {
    a = 0.2  #0.2
    b = 0.15 #0.15
    c = 0.9  #0.9
    d = 0.9  #0.9
    f_1 <- function(s, t) {
      (((t - 0.5) / c)^3 + ((s - 0.5) / d)^3 + ((t - 0.5) / b)^2 - ((s - 0.5) /
                                                                      a)^2 - 5)
    }
    z <- outer(s, t, f_1)
    z
  }

  beta_3 <- function(s, t) {
    a = 0.3  #0.9
    b = 0.3  #0.9
    c = 0.3 #-0.3
    d = 0.3  #-0.3
    f_1 <- function(s, t) {
      -(((t - 0.5) / c)^3 + ((s - 0.5) / d)^3 + ((t - 0.5) / b)^2 - ((s - 0.5) /
                                                                       a)^2 + 5)
    }
    z <- outer(s, t, f_1)
    z
  }

  b2 <- (1 - delta_1) * beta_1(grid_s, grid_t) + (delta_1) * beta_2(grid_s, grid_t)
  b3 <- (1 - delta_1) * beta_1(grid_s, grid_t) + (delta_1) * beta_3(grid_s, grid_t)

  x <- (grid_t - min(grid_t)) / (max(grid_t) - min(grid_t)) * (0.15 - 0.0045) +
    0.0045
  int_1 <- 100 * (0.2074 + 0.3117 * exp((-(371.4)) * x) + 0.5284 * (1 - exp((-(
    -0.8217
  )) * x)) - 423.3 * (1 + tanh(-26.15 * (x - (
    -0.1715
  )))))

  x <- (grid_t - min(grid_t)) / (max(grid_t) - min(grid_t)) * (0.4 - 0.0045) +
    0.0045
  int_2 <- 100 * (0.187 + 0.3117 * exp((-(371.4)) * x) + 0.5284 * (1 - exp((-(
    -0.2
  )) * x)) - 423.3 * (1 + tanh(-27 * (x - (
    -0.1715
  )))))

  x <- (grid_t - min(grid_t)) / (max(grid_t) - min(grid_t)) * (0.08 - 0.0045) +
    0.0045
  int_3 <- 100 * (0.3 + 0.3117 * exp((-(371.4)) * x) + 0.5284 * (1 - exp((-(
    -4
  )) * x)) - 423.3 * (1 + tanh(-24 * (x - (
    -0.1715
  )))))

  int_2 <- (1 - delta_1) * int_1 + (delta_1) * int_2
  int_3 <- (1 - delta_1) * int_1 + (delta_1) * int_3

  b1 <- (delta_2) * beta_1(grid_s, grid_t)
  b2 <- (delta_2) * b2
  b3 <- (delta_2) * b3

  int_1 <- (1 - delta_2) * int_1
  int_2 <- (1 - delta_2) * int_2
  int_3 <- (1 - delta_2) * int_3

  Y_parz1 <- (1 / length(grid_s)) * t(t(X_eval[, 1:round(n_obs_g * mixing_prop[1])]) %*%
                                        b1) + int_1
  Y_parz2 <- (1 / length(grid_s)) * t(t(X_eval[, (round(n_obs_g * mixing_prop[1]) +
                                                    1):(round(n_obs_g * sum(mixing_prop[1:2])))]) %*% b2) + int_2
  Y_parz3 <- (1 / length(grid_s)) * t(t(X_eval[, ((round(n_obs_g * sum(mixing_prop[1:2]))) +
                                                    1):n_obs_g]) %*% b3) + int_3

  X1 <- X_eval[, 1:round(n_obs_g * mixing_prop[1])]
  X2 <- X_eval[, (round(n_obs_g * mixing_prop[1]) + 1):(round(n_obs_g *
                                                                sum(mixing_prop[1:2])))]
  X3 <- X_eval[, ((round(n_obs_g * sum(mixing_prop[1:2]))) + 1):n_obs_g]

  X1 <- X1[, 1:(n_obs * mixing_prop[1])]
  X2 <- X2[, 1:(n_obs * mixing_prop[2])]
  X3 <- X3[, 1:(n_obs * mixing_prop[3])]
  if (measurement_noise_sigma > 0) {
    X1 = X1 + matrix(
      stats::rnorm(length(X1), mean = 0, sd = measurement_noise_sigma),
      nrow = nrow(X1),
      ncol = ncol(X1)
    )
    X2 = X2 + matrix(
      stats::rnorm(length(X2), mean = 0, sd = measurement_noise_sigma),
      nrow = nrow(X2),
      ncol = ncol(X2)
    )
    X3 = X3 + matrix(
      stats::rnorm(length(X3), mean = 0, sd = measurement_noise_sigma),
      nrow = nrow(X3),
      ncol = ncol(X3)
    )
  }
  X_eval <- cbind(X1, X2, X3)


  signal_to_noise_ratio <- SNR
  num <- Rfast::rowVars(Y_parz1) + Rfast::rowVars(Eps[[1]])
  den <- signal_to_noise_ratio * (Rfast::rowVars(Eps[[1]]))
  h <- as.numeric(sqrt(num / den))
  Y1 = Y_parz1 + diag(h) %*% Eps[[1]]

  num <- Rfast::rowVars(Y_parz2) + Rfast::rowVars(Eps[[2]])
  den <- signal_to_noise_ratio * (Rfast::rowVars(Eps[[2]]))
  h <- as.numeric(sqrt(num / den))
  Y2 = Y_parz2 + diag(h) %*% Eps[[2]]

  num <- Rfast::rowVars(Y_parz3) + Rfast::rowVars(Eps[[3]])
  den <- signal_to_noise_ratio * (Rfast::rowVars(Eps[[3]]))
  h <- as.numeric(sqrt(num / den))
  Y3 = Y_parz3 + diag(h) %*% Eps[[3]]
  Y1 <- Y1[, 1:(n_obs * mixing_prop[1])]
  Y2 <- Y2[, 1:(n_obs * mixing_prop[2])]
  Y3 <- Y3[, 1:(n_obs * mixing_prop[3])]
  if (!is.character(shift_coef)) {
    Shift <- severity * shift_coef[1] * (grid_t)^3 + severity * shift_coef[2] *
      (grid_t)^2 + severity * shift_coef[3] * (grid_t) + severity * shift_coef[4]

    Y1 <- Y1 + Shift
    Y2 <- Y2 + Shift
    Y3 <- Y3 + Shift

  } else if (shift_coef == 'low') {
    f1 <- 0.2074
    g1 <- 0.8217
    h1 <- 26.15 - 26.15 * 0.011
    m1 <- 0.0045

    f2 <- 0.187
    g2 <- 0.2
    h2 <- 27 - 27 * 0.011

    f3 <- 0.3
    g3 <- 4
    h3 <- 24 - 24 * 0.006

    x <- (grid_t - min(grid_t)) / (max(grid_t) - min(grid_t)) * (0.15 -
                                                                   0.0045) + 0.0045
    int_1 <- 100 * (0.2074 + 0.3117 * exp((-(371.4)) * x) + 0.5284 * (1 - exp((-(
      -0.8217
    )) * x)) - 423.3 * (1 + tanh(-26.15 * (x - (
      -0.1715
    )))))
    x <- (grid_t - min(grid_t)) / (max(grid_t) - min(grid_t)) * (0.4 - 0.0045) +
      0.0045
    int_2 <- 100 * (0.187 + 0.3117 * exp((-(371.4)) * x) + 0.5284 * (1 - exp((-(
      -0.2
    )) * x)) - 423.3 * (1 + tanh(-27 * (x - (
      -0.1715
    )))))
    x <- (grid_t - min(grid_t)) / (max(grid_t) - min(grid_t)) * (0.08 -
                                                                   0.0045) + 0.0045
    int_3 <- 100 * (0.3 + 0.3117 * exp((-(371.4)) * x) + 0.5284 * (1 - exp((-(
      -4
    )) * x)) - 423.3 * (1 + tanh(-24 * (x - (
      -0.1715
    )))))

    x <- (grid_t - min(grid_t)) / (max(grid_t) - min(grid_t)) * (0.15 -
                                                                   0.0045) + 0.0045
    int_1s <- 100 * (f1 + 0.3117 * exp((-(371.4)) * x) + 0.5284 * (1 - exp((-(
      -g1
    )) * x)) - 423.3 * (1 + tanh(-h1 * (x - (
      -0.1715
    )))))
    x <- (grid_t - min(grid_t)) / (max(grid_t) - min(grid_t)) * (0.4 - 0.0045) +
      0.0045
    int_2s <- 100 * (f2 + 0.3117 * exp((-(371.4)) * x) + 0.5284 * (1 - exp((-(
      -g2
    )) * x)) - 423.3 * (1 + tanh(-h2 * (x - (
      -0.1715
    )))))
    x <- (grid_t - min(grid_t)) / (max(grid_t) - min(grid_t)) * (0.08 -
                                                                   0.0045) + 0.0045
    int_3s <- 100 * (0.3 + 0.3117 * exp((-(371.4)) * x) + 0.5284 * (1 - exp((-(
      -g3
    )) * x)) - 423.3 * (1 + tanh(-h3 * (x - (
      -0.1715
    )))))

    Shift1 <- int_1 - int_1s
    Shift2 <- int_2 - int_2s
    Shift3 <- int_3 - int_3s

    Y1 <- Y1 - Shift1 * severity * 1.25
    Y2 <- Y2 - Shift2 * severity * 1.25
    Y3 <- Y3 - Shift3 * severity * 1.25

  } else{
    f1 <- 0.2074 + 0.2074 * 0.01 * 0.75 * 5
    g1 <- 0.8217 + 0.8217 * 0.03 * 0.75 * 5
    h1 <- 26.15 - 26.15 * 0.003 * 0.75 * 5
    m1 <- 0.0045

    f2 <- 0.187 + 0.187 * 0.01 * 0.75 * 5
    g2 <- 0.2 + 0.2 * 0.05 * 0.75 * 5
    h2 <- 27 - 27 * 0.004 * 0.75 * 5

    f3 <- 0.3 + 0.3 * 0.02 * 0.75 * 5
    g3 <- 4 + 4 * 0.025 * 0.75 * 5
    h3 <- 24 - 24 * 0.004 * 0.75 * 5

    x <- (grid_t - min(grid_t)) / (max(grid_t) - min(grid_t)) * (0.15 -
                                                                   0.0045) + 0.0045
    int_1 <- 100 * (0.2074 + 0.3117 * exp((-(371.4)) * x) + 0.5284 * (1 - exp((-(
      -0.8217
    )) * x)) - 423.3 * (1 + tanh(-26.15 * (x - (
      -0.1715
    )))))
    x <- (grid_t - min(grid_t)) / (max(grid_t) - min(grid_t)) * (0.4 - 0.0045) +
      0.0045
    int_2 <- 100 * (0.187 + 0.3117 * exp((-(371.4)) * x) + 0.5284 * (1 - exp((-(
      -0.2
    )) * x)) - 423.3 * (1 + tanh(-27 * (x - (
      -0.1715
    )))))
    x <- (grid_t - min(grid_t)) / (max(grid_t) - min(grid_t)) * (0.08 -
                                                                   0.0045) + 0.0045
    int_3 <- 100 * (0.3 + 0.3117 * exp((-(371.4)) * x) + 0.5284 * (1 - exp((-(
      -4
    )) * x)) - 423.3 * (1 + tanh(-24 * (x - (
      -0.1715
    )))))

    x <- (grid_t - min(grid_t)) / (max(grid_t) - min(grid_t)) * (0.15 -
                                                                   0.0045) + 0.0045
    int_1s <- 100 * (f1 + 0.3117 * exp((-(371.4)) * x) + 0.5284 * (1 - exp((-(
      -g1
    )) * x)) - 423.3 * (1 + tanh(-h1 * (x - (
      -0.1715
    )))))
    x <- (grid_t - min(grid_t)) / (max(grid_t) - min(grid_t)) * (0.4 - 0.0045) +
      0.0045
    int_2s <- 100 * (f2 + 0.3117 * exp((-(371.4)) * x) + 0.5284 * (1 - exp((-(
      -g2
    )) * x)) - 423.3 * (1 + tanh(-h2 * (x - (
      -0.1715
    )))))
    x <- (grid_t - min(grid_t)) / (max(grid_t) - min(grid_t)) * (0.08 -
                                                                   0.0045) + 0.0045
    int_3s <- 100 * (f3 + 0.3117 * exp((-(371.4)) * x) + 0.5284 * (1 - exp((-(
      -g3
    )) * x)) - 423.3 * (1 + tanh(-h3 * (x - (
      -0.1715
    )))))
    Shift1 <- int_1 - int_1s
    Shift2 <- int_2 - int_2s
    Shift3 <- int_3 - int_3s
    Y1 <- Y1 - Shift1 * severity * 1.1
    Y2 <- Y2 - Shift2 * severity * 1.1
    Y3 <- Y3 - Shift3 * severity * 1.1
  }
  if (measurement_noise_sigma > 0) {
    Y1 = Y1 + matrix(
      stats::rnorm(length(Y1), mean = 0, sd = measurement_noise_sigma),
      nrow = nrow(Y1),
      ncol = ncol(Y1)
    )
    Y2 = Y2 + matrix(
      stats::rnorm(length(Y2), mean = 0, sd = measurement_noise_sigma),
      nrow = nrow(Y2),
      ncol = ncol(Y2)
    )
    Y3 = Y3 + matrix(
      stats::rnorm(length(Y3), mean = 0, sd = measurement_noise_sigma),
      nrow = nrow(Y3),
      ncol = ncol(Y3)
    )
  }
  Y <- cbind(Y1, Y2, Y3)
  colnames(Y) <- NULL


  out <- list(
    X = X_eval,
    Y = Y,
    Eps_1 = Eps[[1]],
    Eps_2 = Eps[[2]],
    Eps_3 = Eps[[3]],
    beta_matrix_1 = b1,
    beta_matrix_2 = b2,
    beta_matrix_3 = b3
  )

  return(out)
}





#'Performs the estimation of gaussian mixtures of regression models and gaussian mixture models.
#'Used in FMRCC_PhaseI.
#'
#'@export
#'@param y a matrix with the scores of the response variable
#'@param x a matrix with the scores of the covariates
#'@param intercept logical, if TRUE the model includes an intercept. Default is TRUE
#'@param init_met the method to initialize the model, it can be 'kmeans' or 'random'. Default is 'kmeans'
#'@param ninit the number of random starts for the model estimation. It is ignored if init_met = 'kmeans'. Default is 10
#'@param groups the number of groups to consider in the model estimation. Default is 1:3
#'@param sigma_par the covariance parametrization to consider in the model estimation. Default is c('VVV','EEE','VII','EII')
#'@param mode the type of model to estimate, it can be 'regression' or 'clustering'. Default is 'regression'
#'@return a list with the model estimated, the residuals variance matrix and the BIC values
#'
estimate_mixture <- function(y = NULL ,
                             x = NULL ,
                             ninit = 10 ,
                             groups = 1:5,
                             mode = 'regression',
                             intercept = TRUE,
                             init_met = 'kmeans',
                             sigma_par = c('VVV', 'EEE', 'VII', 'EII')) {
  if (sum(!sigma_par %in%  c('VVV', 'EEE', 'VII', 'EII')) != 0) {
    stop('The selected covariance parametrization is not valid')
  }

  minBIC <- 1000000
  minBIC_clust <- 100000
  BIC <- matrix(nrow = length(sigma_par), ncol = length(groups))
  models <- vector(mode = 'list', length = length(groups))

  nn <- length(groups) * length(sigma_par) * ninit

  pb <- utils::txtProgressBar(
    min = 0,
    # Minimum value of the progress bar
    max = nn,
    # Maximum value of the progress bar
    style = 3,
    # Progress bar style (also available style = 1 and style = 2)
    width = 50,
    # Progress bar width. Defaults to getOption("width")
    char = "="
  )   # Character used to create the bar

  if (mode == 'regression') {
    ii <- 0
    for (kk in 1:length(groups)) {
      h <- 0
      k <- groups[kk]
      for (hh in sigma_par) {
        h <- h + 1
        BIC_group <- numeric(ninit)
        for (i in 1:ninit) {
          ii <- ii + 1
          utils::setTxtProgressBar(pb, ii)
          est <- mixregfit_multivariate(
            y,
            x,
            k = k,
            init_met = init_met,
            intercept = intercept,
            eps = 1e-4,
            max_iter = 500,
            model_Sigma = hh
          )
          if (is.na(est[1])) {
            BIC_group[i] <- 1000000
            next
          }
          BIC_group[i] <- est$BIC
          if (BIC_group[i] < minBIC_clust) {
            minBIC_clust <- BIC_group[i]
            if (minBIC_clust < minBIC) {
              model <- est
              minBIC <- minBIC_clust
            }
          }
        }
        BIC[h, kk] <- min(BIC_group)
        minBIC_clust <- 100000
      }
    }
    close(pb)

    BIC <- as.data.frame(BIC)
    rownames(BIC) <- sigma_par

    best_model <- model
    num_groups <- length(best_model$prop)

    # Matrix for Residuals variance
    x <- as.matrix(x)
    if (intercept) {
      if (sum(x[, 1] != 1) != 0) {
        x <- cbind(1, x)
      }
    }
    H <- list(num_groups)
    for (kk in 1:num_groups) {
      H[[kk]] <- solve(t(x) %*% diag(best_model$z[, kk]) %*% x) %*% t(x) %*% diag(best_model$z[, kk])
      H[[kk]] <- H[[kk]] %*% t(H[[kk]])
    }

    return(list(
      best_model = best_model,
      H = H,
      BIC = BIC
    ))
  }

  if (mode == 'clustering') {
    y <- as.matrix(y)
    x <- matrix(1, ncol = 1 , nrow = nrow(y))
    ii <- 0
    for (kk in 1:length(groups)) {
      h <- 0
      for (hh in sigma_par) {
        h <- h + 1
        BIC_group <- numeric(ninit)
        for (i in 1:ninit) {
          ii <- ii + 1
          utils::setTxtProgressBar(pb, ii)
          est <- mixregfit_multivariate(
            y,
            x,
            k = groups[kk],
            init_met = init_met,
            intercept = F,
            eps = 1e-03,
            max_iter = 500,
            model_Sigma = hh
          )
          if (is.na(est[1])) {
            BIC_group[i] <- 1000000
            next
          }
          BIC_group[i] <- est$BIC
          if (BIC_group[i] < minBIC_clust) {
            minBIC_clust <- BIC_group[i]
            if (minBIC_clust < minBIC) {
              model <- est
              minBIC <- minBIC_clust
            }
          }
        }
        BIC[h, kk] <- min(BIC_group)
        minBIC_clust <- 100000
      }
    }
    close(pb)

    BIC <- as.data.frame(BIC)
    rownames(BIC) <- sigma_par

    best_model <- model
    num_groups <- length(best_model$prop)

    return(list(best_model = best_model, BIC = BIC))
  }

}

## Simulate x ##
simulate_x <- function(nobs ,
                       n_comp = 50 ,
                       gamma = 1) {
  P <- 200
  x_seq <- seq(0, 1, l = P)
  get_mat <- function(cov) {
    P <- length(cov)
    covmat <- matrix(0, nrow = P, ncol = P)
    for (ii in seq_len(P)) {
      covmat[ii, ii:P] <- cov[seq_len(P - ii + 1)]
      covmat[P - ii + 1, seq_len(P - ii + 1)] <- rev(cov[seq_len(P - ii + 1)])
    }
    covmat
  }
  w <- 1 / P
  cov_fun <- function(x)
    exp(-gamma * sqrt(x))
  cov_mat <- get_mat(cov_fun(x_seq))
  eig <- RSpectra::eigs_sym(cov_mat, n_comp + 10)
  eig$values <- eig$values * w
  eig$vectors <- eig$vectors / sqrt(w)
  eig$values <- eig$values[seq_len(n_comp)]
  eig$vectors <- eig$vectors[, seq_len(n_comp)]
  e <- eig
  csi_X <- stats::rnorm(n = length(e$values) * nobs,
                        mean = 0,
                        sd = sqrt(rep(e$values, each = nobs)))
  csi_X <- matrix(csi_X, nrow = nobs)
  X <- csi_X %*% t(e$vectors)
  # matplot(t(X), type = "l")
  x <- get_mfd_list(list(X1 = X))
  return(x)
}

predict_mixture <- function(y = NULL ,
                            x = NULL ,
                            hard = F ,
                            model = NULL) {
  #Conditions--------------------------------------------------------------
  if (is.null(y)) {
    cat('y is missing')
  }
  if (is.null(x)) {
    cat('x is missing')
  }
  if (is.null(model)) {
    cat('model is missing')
  }

  #Initialization ---------------------------------------------------------
  y <- as.matrix(y)
  x <- as.matrix(x)
  p <- ncol(y)
  n <- nrow(y)
  Sigma <- model$Sigma
  B <- model$B
  prop <- model$prop
  k <- length(prop)
  z <- matrix(nrow = n , ncol = k)
  resp <- vector(mode = "list", length = k)
  y_cond <- matrix(rep(0, n * p) , nrow = n , ncol = p)
  y_hard <- matrix(rep(0, n * p) , nrow = n , ncol = p)

  #Compute the posterior probabilities-------------------------------------
  comp <- lapply(1:k, function(i) {
    lk <-
      diag((1 / ((2 * pi)^(p / 2) * det(Sigma[[i]])^0.5)) * exp(-0.5 * (y -
                                                                          x %*% B[[i]]) %*% solve(Sigma[[i]]) %*% t(y - x %*% B[[i]])))
    prop[i] * lk
  })

  comp <- sapply(comp, cbind)
  if (!is.matrix(comp)) {
    comp <- t(as.matrix(comp))
  }

  comp[which(comp == 0)] <- 1e-100
  comp

  compsum <- apply(comp, 1, sum)

  for (kk in 1:k) {
    z[, kk] <- comp[, kk] / compsum
  }

  # Compute the responses per group----------------------------------------
  for (kk in 1:k) {
    resp[[kk]] <- x %*% B[[kk]]
  }

  #Conditional prediction--------------------------------------------------
  if (hard == F) {
    # Compute the conditional responses---------------------------------------
    for (i in 1:n) {
      for (kk in 1:k) {
        y_cond[i, ] <- y_cond[i, ] + z[i, kk] * resp[[kk]][i, ]
      }
    }
    pred <- y_cond
  } else{
    hard_membership <- apply(z, FUN = which.max, MARGIN = 1)
    for (kk in 1:k) {
      z[, kk] <- as.numeric(hard_membership == kk)
    }

    # Compute the conditional responses---------------------------------------
    for (i in 1:n) {
      for (kk in 1:k) {
        y_hard[i, ] <- y_hard[i, ] + z[i, kk] * resp[[kk]][i, ]
      }
    }
    pred <- y_hard
  }

  return(list(prediction = pred, membership = z))
}

#'Performs the estimation of gaussian mixtures of regression models and gaussian mixture models.
#'Used in FMRCC_PhaseI.
#'
#'@param y a matrix with the scores of the response variable
#'@param x a matrix with the scores of the covariates
#'@param k the number of groups to consider in the model estimation
#'@param intercept logical, if TRUE the model includes an intercept. Default is TRUE
#'@param init_met the method to initialize the model, it can be 'kmeans' or 'random'. Default is 'kmeans'
#'@param eps the convergence criterion. Default is 1e-6
#'@param max_iter the maximum number of iterations. Default is 500
#'@param model_Sigma the parametrization of the covariance. It can be 'VVV', 'EEE', 'VII' or 'EII', with no default
#'@return a list with the estimated parameters of the model
#'
mixregfit_multivariate <- function(y,
                                   x,
                                   k,
                                   init_met = 'random',
                                   intercept = FALSE,
                                   eps = 1e-6,
                                   max_iter = 500 ,
                                   model_Sigma) {
  #INITIALIZATION
  y <- as.matrix(y)
  x <- as.matrix(x)
  if (intercept) {
    x = cbind(1, x)
  }
  n <- nrow(y)
  p <- ncol(y)
  q <- ncol(x)
  z <- matrix(nrow = n, ncol = k)
  B <- list()
  sing <- 0
  Sigma <- list()
  eps_iter <- 100
  restarts <- 0

  if (init_met == 'kmeans') {
    z_kmeans <- stats::kmeans(y, centers = k, nstart = 20)$cluster
  }
  if (init_met == 'random') {
    z_kmeans <- sample(1:k, n, replace = T)
  }

  #PARAMETERS CALCULATION FROM Z
  for (kk in 1:k) {
    z[, kk] <- as.numeric(z_kmeans == kk)
  }
  prop <- apply(z, 2, mean)

  if (intercept) {
    B <- lapply(1:k, function(kk)
      as.matrix(stats::lm(y ~ x[, -1], weights = z[, kk])$coef))
  } else{
    B <- lapply(1:k, function(kk)
      as.matrix(stats::lm(y ~ x - 1, weights = z[, kk])$coef))
  }

  Sigma <- computeSigma(
    x = x,
    y = y,
    B = B,
    z = z,
    model_Sigma = model_Sigma,
    sing = sing,
    p = p ,
    k = k ,
    n = n
  )
  for (kk in 1:k) {
    Sigma[[kk]] <- symmetrize(Sigma[[kk]])
  }
  comp <- try(computeComp(x, y, B, Sigma, prop), silent = T)
  if (inherits(comp, "try-error")) {
    sing <- 1
  } else{
    comp <- sapply(comp, cbind)
    compsum <- apply(comp, 1, sum)
    obsloglik <- sum(log(compsum))

  }

  #-----------------------------------------------------------------------------------------------------

  iter <- 0

  while (eps_iter >= eps & iter < max_iter) {
    iter <- iter + 1

    if (sing == 0) {
      z <- apply(comp, 2, function(zz)
        zz / compsum)
      prop <- apply(z, 2, mean)
    }

    if (sum(prop < 1e-20) > 0 || is.na(sum(prop)) || sing != 0) {
      sing <- 1
    } else{
      if (intercept) {
        B <- lapply(1:k, function(kk)
          as.matrix(stats::lm(y ~ x[, -1], weights = z[, kk])$coef))
      } else{
        B <- lapply(1:k, function(kk)
          as.matrix(stats::lm(y ~ x - 1, weights = z[, kk])$coef))
      }

      Sigma <- try(computeSigma(
        x = x,
        y = y,
        B = B,
        z = z,
        model_Sigma = model_Sigma,
        sing = sing,
        p = p ,
        k = k ,
        n = n
      ),
      silent = T)

      if (inherits(Sigma, "try-error")) {
        sing <- 1
      } else {
        sing <- Sigma[[k + 1]]
        for (kk in 1:k) {
          Sigma[[kk]] <- symmetrize(Sigma[[kk]])
        }
      }


      if (sing == 0) {
        comp <- try(computeComp(x, y, B, Sigma, prop), silent = T)
        if (inherits(comp, "try-error")) {
          sing <- 1
        } else{
          comp <- sapply(comp, cbind)
          compsum <- apply(comp, 1, sum)
          newobsloglik <- sum(log(compsum))
          eps_iter <- abs(newobsloglik - obsloglik)
          obsloglik <- newobsloglik
        }
      }
    }

    if (sing > 0 || is.na(newobsloglik) || newobsloglik <
        obsloglik ||
        abs(newobsloglik) == Inf & init_met == 'kmeans') {
      cat("Need new starting values due to singularity...", "\n")
      restarts <- restarts + 1
      if (restarts > 15) {
        cat("Too many tries!")
        return(NA)
      }

      z <- matrix(nrow = n, ncol = k)
      B <- list()
      Sigma <- list()
      eps_iter <- 100
      sing <- 0

      z_kmeans <- sample(1:k, n, replace = T)

      #PARAMETERS CALCULATION FROM Z
      for (kk in 1:k) {
        z[, kk] <- as.numeric(z_kmeans == kk)
      }

      prop <- apply(z, 2, mean)

      if (intercept) {
        B <- lapply(1:k, function(kk)
          as.matrix(stats::lm(y ~ x[, -1], weights = z[, kk])$coef))
      } else{
        B <- lapply(1:k, function(kk)
          as.matrix(stats::lm(y ~ x - 1, weights = z[, kk])$coef))
      }

      Sigma <- computeSigma(
        x = x,
        y = y,
        B = B,
        z = z,
        model_Sigma = model_Sigma,
        sing = sing,
        p = p ,
        k = k ,
        n = n
      )
      sing <- Sigma[[k + 1]]
      for (kk in 1:k) {
        Sigma[[kk]] <- symmetrize(Sigma[[kk]])
      }

      if (sing == 0) {
        comp <- try(computeComp(x, y, B, Sigma, prop), silent = T)
        if (inherits(comp, "try-error")) {
          sing <- 1
        } else{
          comp <- sapply(comp, cbind)
          compsum <- apply(comp, 1, sum)
          obsloglik <- sum(log(compsum))
        }
      }
      # comp <- computeComp(x, y, B, Sigma, prop)
      # comp <- sapply(comp, cbind)
      # compsum <- apply(comp, 1, sum)
      # obsloglik <- sum(log(compsum))
      iter <- 0
    }
  }

  group_member <- apply(z, FUN = which.max, MARGIN = 1)

  if (model_Sigma == 'VVV') {
    if (q == 1) {
      BIC <- -2 * obsloglik + log(n) * (k * p + k * (p * (p + 1) / 2) + k - 1)
    } else{
      BIC <- -2 * obsloglik + log(n) * (k * p + k * p * q + k * (p * (p + 1) /
                                                                   2) + k - 1)
    }
  } else if (model_Sigma == 'EEE') {
    if (q == 1) {
      BIC <- -2 * obsloglik + log(n) * (k * p + (p * (p + 1) / 2) + k - 1)
    } else{
      BIC <- -2 * obsloglik + log(n) * (k * p + k * p * q + (p * (p + 1) / 2) + k - 1)
    }
  } else if (model_Sigma == 'VII') {
    if (q == 1) {
      BIC <- -2 * obsloglik + log(n) * (k * p + k + k - 1)
    } else{
      BIC <- -2 * obsloglik + log(n) * (k * p + k * p * q + k + k - 1)
    }
  } else if (model_Sigma == 'EII') {
    if (q == 1) {
      BIC <- -2 * obsloglik + log(n) * (k * p + 1 + k - 1)
    } else{
      BIC <- -2 * obsloglik + log(n) * (k * p + k * p * q + 1 + k - 1)
    }
  }
  return (
    list(
      B = B,
      Sigma = Sigma,
      prop = prop,
      group_member = group_member,
      logLik = obsloglik,
      BIC = BIC,
      z = z
    )
  )
}
symmetrize <- function(mat) {
  return((mat + t(mat)) / 2)
}
