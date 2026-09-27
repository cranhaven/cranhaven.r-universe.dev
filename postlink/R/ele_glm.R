#' GLM with ELE-Based Linkage Error Adjustment
#'
#' @description
#' Fits a generalized linear model (GLM) accounting for exchangeable linkage errors
#' (ELE) as defined by Chambers (2009). It solves the unbiased estimating equations resulting
#' from the modified mean function induced by mismatch errors.
#'
#' @param x A numeric matrix of predictors (design matrix).
#' @param y A numeric vector of responses.
#' @param family the type of regression model ("gaussian" - default, "poisson",
#' "binomial", "gamma"). Standard link functions are used ("identity" for Gaussian,
#' "log" for Poisson and Gamma, and "logit" for binomial).
#' @param m.rate A numeric vector of mismatch rates. If the length is 1, it is
#'   replicated for all blocks. If length > 1, it must match the number of unique blocks.
#' @param audit.size a vector of block sizes in the audit sample (selected by
#' simple random sampling) if used to estimate the m.rate (optional). If a
#' single value is provided, assume the same value for all blocks and put out a warning.
#' @param blocks A vector indicating the block membership of each observation.
#' @param weight.matrix A character string specifying the weighting method
#' ("ratio-type", "Lahiri-Larsen", "BLUE", or "all" (default))
#' @param control an optional list variable to of control arguments including
#' "init.beta" for the initial outcome model coefficient estimates) - by
#' default is the naive estimator when the weight matrix is ratio-type or
#' Lahiri-Larsen and is the Lahiri-Larsen estimator for the BLUE weight matrix.
#' @param ... Pass control arguments directly.
#'
#' @returns A list of results:
#' \item{coefficients}{A named vector (or matrix) of coefficients for the outcome model.}
#' \item{residuals}{The working residuals, defined as \code{y - fitted.values}.}
#' \item{fitted.values}{The fitted mean values of the outcome model, obtained by transforming the linear predictors by the inverse of the link function.}
#' \item{linear.predictors}{The linear fit on the link scale.}
#' \item{deviance}{The deviance of the weighted outcome model at convergence.}
#' \item{null.deviance}{The deviance of the weighted null outcome model.}
#' \item{var}{The estimated variance-covariance matrix of the parameters (sandwich estimator).}
#' \item{dispersion}{The estimated dispersion parameter (e.g., variance for Gaussian, 1/shape for Gamma).}
#' \item{rank}{The numeric rank of the fitted linear model.}
#' \item{df.residual}{The residual degrees of freedom.}
#' \item{df.null}{The residual degrees of freedom for the null model.}
#' \item{family}{The \code{family} object used.}
#' \item{call}{The matched call.}
#'
#' @importFrom stats gaussian binomial poisson Gamma family glm coef aggregate ave density model.matrix var
#' @importFrom nleqslv nleqslv
#'
#' @examples
#' data(brfss)
#' brfss <- na.omit(brfss)
#'
#' x <- cbind(1, subset(brfss, select = c(Height,Physhlth,Menthlth,Exerany)))
#' y <- brfss$Weight
#'
#' fit <- glmELE(x, y, family = "gaussian",
#'              m.rate = unique(brfss$m.rate), blocks = brfss$imonth,
#'              weight.matrix = "BLUE")
#'
#' @references
#' Chambers, R. (2009). Regression analysis of probability-linked data.
#' \emph{Official Statistics Research Series}, 4, 1-15.
#'
#' @export
glmELE <- function(x, y, family = "gaussian",
                   m.rate, audit.size = NULL,
                   blocks, weight.matrix = "all",
                   control = list(init.beta = NULL), ...) {

  if (missing(x) || missing(y)) stop("Error: x and y are required.")
  if (!is.matrix(x) && !is.data.frame(x)) stop("Error: x must be a matrix or data.frame.")
  x <- as.matrix(x)

  dcontrols <- list(...)
  if ("init.beta" %in% names(dcontrols)) {
    init.beta <- dcontrols$init.beta
  } else {
    if ("init.beta" %in% names(control)) {
      init.beta <- control$init.beta
    } else {
      init.beta <- NULL
    }
  }

  # Determine family object
  if (is.character(family)) {
    if (family == "gaussian") fam_obj <- stats::gaussian()
    else if (family == "binomial") fam_obj <- stats::binomial()
    else if (family == "poisson") fam_obj <- stats::poisson()
    else if (family == "gamma") fam_obj <- stats::Gamma(link = "log")
    else stop("Error: family not supported")
  } else {
    fam_obj <- family
    family <- fam_obj$family
  }

  if (family == "binomial") {
    y <- as.numeric(as.vector(y))
    if (sum(y > 1) != 0) stop("Error: y values must be 0 or 1 for binomial family")
  } else {
    y <- as.numeric(y)
  }

  if (nrow(x) != length(y)) stop("Error: Number of rows in x must match length of y.")
  if (any(is.na(x)) | any(is.na(y))) stop("Error: Cannot have missing observations")

  n <- nrow(x)
  p <- ncol(x)

  if (is.null(init.beta)) {
    if (is.null(colnames(x))) colnames(x) <- paste0("V", 1:p)
    # Use suppressWarnings to handle potential convergence issues in naive fit
    naive_fit <- suppressWarnings(stats::glm(y ~ . - 1, data = data.frame(x), family = fam_obj))
    init.beta <- coef(naive_fit)
  } else {
    init.beta <- as.vector(init.beta)
    if (length(init.beta) != p) {
      warning("Provided 'init.beta' length mismatch. Using default naive estimates.")
      if (is.null(colnames(x))) colnames(x) <- paste0("V", 1:p)
      init.beta <- coef(stats::glm(y ~ . - 1, data = data.frame(x), family = fam_obj))
    }
  }

  if (missing(blocks) || is.null(blocks)) {
    blocks <- rep(1, n)
    warning("'blocks' argument is missing or NULL - assuming a single block for all observations.")
  }

  if (length(blocks) != n) {
    stop("Error: 'blocks' length does not match the number of observations used in the model.")
  }
  m.rate <- as.numeric(m.rate)

  blocks <- match(blocks, sort(unique(blocks)))
  unique_blocks <- unique(blocks)
  n_blocks <- length(unique_blocks)

  lambda <- 1 - m.rate
  if (length(lambda) == 1 && n_blocks > 1) {
    lambda <- rep(lambda, n_blocks)
  }
  if (length(lambda) != n_blocks) stop("Error: Length of m.rate does not match number of blocks.")

  #if (is.null(audit.size)) {
  #  warning("Argument 'audit.size' is NULL. Linkage rates ('m.rate') are treated as fixed/known.")
  #} else {
    if (length(audit.size) == 1 && n_blocks > 1) {
      audit.size <- rep(audit.size, n_blocks)
      warning("Single 'audit.size' provided; applying to all blocks.")
    }
  #}

  if (length(weight.matrix) == 1 && weight.matrix == "all") {
    weight.matrix <- c("ratio", "LL", "BLUE")
  }

  # Definitions for Internal Functions

  IEq.M <- function(M, lambda, blocks, type = "Eq") {
    nq <- as.numeric(tapply(y, INDEX = blocks, FUN = length))
    alphaq <- 1 - lambda
    if (type == "Eq") {
      c0q <- ifelse(nq > 1, alphaq * (nq / (nq - 1)), 0)
      c1q <- ifelse(nq > 1, (1 - alphaq - alphaq / (nq - 1)), 1)
    }
    if (type == "Iq") {
      c0q <- ifelse(nq > 1, -alphaq * (nq / (nq - 1)), 0)
      c1q <- ifelse(nq > 1, 1 - (1 - alphaq - alphaq / (nq - 1)), 0)
    }

    if (!is.matrix(M) || ncol(M) == 1) {
      M_means <- as.matrix(tapply(M, list(blocks), mean))
      M_means <- M_means[blocks]
      prod <- M * c1q[blocks] + M_means * c0q[blocks]
    } else {
      M_agg <- stats::aggregate(M, by = list(blocks), FUN = mean, drop = TRUE)
      corr <- as.matrix(M_agg[, -1])[blocks, , drop = FALSE] * c0q[blocks]
      Mtilde <- sweep(M, MARGIN = 1, FUN = "*", STATS = c1q[blocks])
      prod <- Mtilde + corr
    }
    return(prod)
  }

  fq_fun <- function(family, beta_est) {
    if (family == "gaussian") {
      fq <- x %*% beta_est
      dfq <- x
    }
    if (family == "binomial") {
      eta <- x %*% beta_est
      fq <- 1 / (1 + exp(-eta))
      dfq <- sweep(x, MARGIN = 1, FUN = "*", STATS = fq * (1 - fq))
    }
    if (family %in% c("poisson", "gamma")) {
      fq <- exp(x %*% beta_est)
      dfq <- sweep(x, MARGIN = 1, FUN = "*", STATS = fq)
    }
    return(list(fq = fq, dfq = dfq))
  }

  sigma2 <- function(beta_hat, family) {
    fq_obj <- fq_fun(family, beta_hat)
    fq <- fq_obj$fq
    num1 <- crossprod(y - fq)
    num2 <- -2 * crossprod(fq, IEq.M(fq, lambda, blocks, type = "Iq"))
    num <- ifelse(num1 + num2 < 0, num1 / 2, num1 + num2)
    if (family == "gaussian") {
      val <- c(num / nrow(x))
    } else if (family == "gamma") {
      val <- c(num / crossprod(fq))
    } else {
      val <- 1
    }
    return(val)
  }

  Var_y <- function(beta_hat, family) {
    fq_obj <- fq_fun(family, beta_hat)
    fq <- fq_obj$fq
    fqbar <- as.matrix(tapply(fq, list(blocks), mean)[blocks])
    fq2bar <- as.matrix(tapply(fq^2, list(blocks), mean)[blocks])
    Vq <- (1 - lambda[blocks]) * (lambda[blocks] * (fq - fqbar)^2 + fq2bar - fqbar^2)

    if (family == "gaussian") {
      Exp <- sigma2(beta_hat, family)
    } else {
      if (family == "binomial") v <- fq * (1 - fq)
      if (family == "poisson") v <- fq
      if (family == "gamma") v <- sigma2(beta_hat, family) * fq^2
      vsum <- as.matrix(tapply(v, list(blocks), sum)[blocks])
      nq <- as.numeric(tapply(y, INDEX = blocks, FUN = length))[blocks]
      gamma_term <- ifelse(nq > 1, (1 - lambda[blocks]) / (nq - 1), 0)
      Exp <- (lambda[blocks] - gamma_term) * v + (gamma_term) * vsum
    }
    return(Exp + Vq)
  }

  Delta_fun <- function(beta_hat, family, audit.size, Gq) {
    if (is.null(audit.size)) return(matrix(0, ncol(Gq), ncol(Gq)))
    fq <- fq_fun(family, beta_hat)$fq
    fqbar <- stats::ave(fq, blocks)
    r <- fq - fqbar
    Mq_vec <- tapply(y, blocks, length)
    M <- Mq_vec[blocks]
    m <- audit.size[blocks]
    lam <- lambda[blocks]
    k <- ifelse(M > 1, (M / (M - 1))^2 * lam * (1 - lam) / m, 0)
    k_unique <- tapply(k, blocks, function(x) x[1])
    Z <- sweep(Gq, 1, r, `*`)
    U <- rowsum(Z, blocks)
    U_scaled <- sweep(U, 1, sqrt(k_unique), `*`)
    return(crossprod(U_scaled))
  }

  Gq_fun <- function(family, wm, beta_est) {
    if (wm == "ratio") Gq <- x
    if (wm == "LL") Gq <- IEq.M(x, lambda, blocks)
    if (wm == "BLUE") {
      dfq <- fq_fun(family, beta_est)$dfq
      inv_Var_y <- (Var_y(beta_est, family))^(-1)
      Gq <- sweep(IEq.M(dfq, lambda, blocks), MARGIN = 1, FUN = "*", STATS = inv_Var_y)
    }
    return(Gq)
  }

  if ("BLUE" %in% weight.matrix) weights.find <- unique(c(weight.matrix, "LL"))
  else weights.find <- weight.matrix

  all.weights <- c("ratio", "LL", "BLUE")
  weights.find <- all.weights[all.weights %in% weights.find]

  nwm <- length(weights.find)
  coef_mat <- matrix(nrow = nwm, ncol = p, NA)
  var_mat <- matrix(nrow = nwm, ncol = p, NA)
  covhat_list <- list()

  residuals_mat <- matrix(nrow = nwm, ncol = n, NA)
  fitted_mat <- matrix(nrow = nwm, ncol = n, NA)
  deviance_vec <- numeric(nwm)

  # Calculate Null Deviance (Intercept only model)
  # Standard null model uses mean(y) or weighted mean
  null_mu <- rep(mean(y), n)
  null_deviance_val <- sum(fam_obj$dev.resids(y, null_mu, rep(1, n)))

  for (i in 1:nwm) {
    wm <- weights.find[i]
    AEE_solve <- function(b) {
      Gq <- Gq_fun(family, wm, b)
      fq_res <- fq_fun(family, b)
      Eqfq <- IEq.M(fq_res$fq, lambda, blocks)
      return(crossprod(Gq, y) - crossprod(Gq, Eqfq))
    }

    if (wm == "BLUE") {
      idx_ll <- which(weights.find == "LL")
      if (length(idx_ll) > 0) current_init <- coef_mat[idx_ll, ] else current_init <- init.beta
    } else {
      current_init <- init.beta
    }

    res <- nleqslv::nleqslv(current_init, AEE_solve, jacobian = TRUE)
    if (res$termcd > 2) warning(paste("Convergence warning for", wm, "estimator. Code:", res$termcd))
    est_beta <- res$x
    coef_mat[i, ] <- est_beta
    J <- -res$jac
    Gq <- Gq_fun(family, wm, est_beta)
    Vy <- Var_y(est_beta, family)
    Vh_diag <- crossprod(sweep(Gq, MARGIN = 1, FUN = "*", STATS = sqrt(Vy)))
    Vh_delta <- Delta_fun(est_beta, family, audit.size, Gq)
    Vh <- Vh_diag + Vh_delta
    J_inv <- try(solve(J), silent = TRUE)
    if (inherits(J_inv, "try-error")) cov_mat <- matrix(NA, p, p)
    else cov_mat <- J_inv %*% Vh %*% t(J_inv)
    colnames(cov_mat) <- colnames(x)
    rownames(cov_mat) <- colnames(x)
    covhat_list[[i]] <- cov_mat
    var_mat[i, ] <- diag(cov_mat)

    mu_est <- fam_obj$linkinv(x %*% est_beta)
    fitted_mat[i, ] <- as.vector(mu_est)

    residuals_mat[i, ] <- as.vector(y - mu_est)

    dev_resid <- fam_obj$dev.resids(y, mu_est, rep(1, n))
    deviance_vec[i] <- sum(dev_resid)
  }

  rownames(coef_mat) <- weights.find
  rownames(var_mat) <- weights.find
  colnames(coef_mat) <- colnames(x)
  names(covhat_list) <- weights.find

  rownames(residuals_mat) <- weights.find
  rownames(fitted_mat) <- weights.find
  names(deviance_vec) <- weights.find

  keep_idx <- which(weights.find %in% weight.matrix)
  coef_mat <- coef_mat[keep_idx, , drop = FALSE]
  var_mat <- var_mat[keep_idx, , drop = FALSE]
  covhat_list <- covhat_list[names(covhat_list) %in% weight.matrix]

  residuals_mat <- residuals_mat[keep_idx, , drop = FALSE]
  fitted_mat <- fitted_mat[keep_idx, , drop = FALSE]
  deviance_vec <- deviance_vec[keep_idx]

  linear.predictors <- x %*% t(coef_mat)
  colnames(linear.predictors) <- rownames(coef_mat)

  model_rank <- qr(x)$rank

  output <- list(
    coefficients = coef_mat,
    residuals = residuals_mat,
    fitted.values = fitted_mat,
    linear.predictors = linear.predictors,
    deviance = deviance_vec,
    null.deviance = null_deviance_val,
    var = covhat_list,
    rank = model_rank,
    df.residual = n - p,
    df.null = n - 1,
    family = fam_obj,
    call = match.call()
  )

  if (family %in% c("gamma", "gaussian")) {
    disp_vals <- sapply(1:nrow(coef_mat), function(k) sigma2(coef_mat[k, ], family))
    names(disp_vals) <- rownames(coef_mat)
    output$dispersion <- disp_vals
  }

  class(output) <- "glmELE"
  return(output)
}

#' @keywords internal
#' @export
fitglm.adjELE <- function(x, y, family, adjustment, control, ...) {

  # Validation and Data Retrieval
  # Access the full data by reference as per design
  full_data <- adjustment$data_ref$data
  if (is.null(full_data)) {
    stop("The 'adjustment' object does not contain linked data. ",
         "Please recreate the object with 'linked.data' provided.", call. = FALSE)
  }

  # Subset Alignment
  # plglm() applies subsetting and NA handling to x and y. We align the
  # adjustment parameters (blocks, m.rate, audit.size) to this subset.

  subset_names <- rownames(x)
  idx_map <- NULL

  if (!is.null(subset_names)) {
    # Strict matching by row names ensures perfect alignment
    idx_map <- match(subset_names, rownames(full_data))

    if (anyNA(idx_map)) {
      stop("Row mismatch: Some observations in the model matrix could not be matched ",
           "to the adjustment data. Ensure the 'data' passed to plglm() matches ",
           "the 'linked.data' used in the adjustment object.", call. = FALSE)
    }
  } else {
    # If no row names, assume 1:1 mapping if sizes match.
    if (nrow(x) == nrow(full_data)) {
      idx_map <- seq_len(nrow(full_data))
    } else {
      stop("Row mismatch: Model matrix has no row names and length differs from ",
           "adjustment data. Cannot synchronize blocks safely.", call. = FALSE)
    }
  }

  # Parameter Preparation for glmELE
  # Process Blocks
  blocks_full <- adjustment$blocks
  blocks_sub <- blocks_full[idx_map]
  unique_blocks_sub <- sort(unique(blocks_sub))

  # Process m.rate
  m_rate_in <- adjustment$m.rate
  m_rate_out <- NULL

  # Logic to ensure m.rate passed to glmELE matches the subsetted blocks
  if (length(m_rate_in) == 1) {
    m_rate_out <- m_rate_in
  } else {
    if (length(m_rate_in) == length(unique(blocks_full))) {
      u_b_orig <- sort(unique(blocks_full))
      rate_map <- stats::setNames(m_rate_in, as.character(u_b_orig))
      m_rate_full_vec <- rate_map[as.character(blocks_full)]
    } else {
      m_rate_full_vec <- m_rate_in
    }
    m_rate_sub_vec <- m_rate_full_vec[idx_map]
    m_rate_out <- tapply(m_rate_sub_vec, blocks_sub, function(z) z[1])
    m_rate_out <- m_rate_out[as.character(unique_blocks_sub)]
    m_rate_out <- as.numeric(m_rate_out)
  }

  # Process audit.size
  audit_in <- adjustment$audit.size
  audit_out <- NULL

  if (!is.null(audit_in)) {
    if (length(audit_in) == 1) {
      audit_out <- audit_in
    } else {
      # Same logic as m.rate - expand to full, subset, re-aggregate
      if (length(audit_in) == length(unique(blocks_full))) {
        u_b_orig <- sort(unique(blocks_full))
        audit_map <- stats::setNames(audit_in, as.character(u_b_orig))
        audit_full_vec <- audit_map[as.character(blocks_full)]
      } else {
        audit_full_vec <- audit_in
      }

      audit_sub_vec <- audit_full_vec[idx_map]
      audit_out <- tapply(audit_sub_vec, blocks_sub, function(z) z[1])
      audit_out <- audit_out[as.character(unique_blocks_sub)]
      audit_out <- as.numeric(audit_out)
    }
  }

  # Dispatch to Internal Function
  fit <- glmELE(
    x = x,
    y = y,
    family = family,
    m.rate = m_rate_out,
    audit.size = audit_out,
    blocks = blocks_sub,
    weight.matrix = adjustment$weight.matrix,
    control = control,
    ...
  )

  # Post-Processing
  if (!is.null(subset_names)) {
    if (is.matrix(fit$residuals)) {
      colnames(fit$residuals) <- subset_names
    }
    if (is.matrix(fit$fitted.values)) {
      colnames(fit$fitted.values) <- subset_names
    }
  }

  # Return Fitted Model Object
  return(fit)
}
