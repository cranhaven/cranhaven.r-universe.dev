#### Default Method ####

#' Normal outcome for kDGLM models
#'
#' Creates an outcome with Normal distribution with the chosen parameters (can only specify 2).
#'
#' @param mu character: Name of the linear predictor associated with the mean parameter of the Normal distribution. The parameter is treated as unknown and equal to the associated linear predictor.
#' @param V character or numeric: If V is a character, it is interpreted as the names of the linear predictors associated with the variance parameter of the Normal distribution. If V is numeric, the variance is considered known and equal to the value of V, otherwise, the variance is considered unknown and equal to the exponential of the linear predictor informed in V. If the outcome is a Multivariate Normal, then V must be a matrix and, if the variance is unknown, the elements outside its main diagonal are treated as the linear predictor associated with the correlation between each coordinate of the outcome, otherwise V is treated as the covariance matrix. The user cannot specify V with Tau or Sd.
#' @param Tau character or numeric: If Tau is a character, it is interpreted as the names of the linear predictors associated with the precisions parameter of the Normal distribution. If Tau is numeric, the precision is considered known and equal to the value of Tau, otherwise, the precision is considered unknown and equal to the exponential of the linear predictor informed in Tau. If the outcome is a Multivariate Normal, then Tau must be a matrix and, if the precision is unknown, the elements outside its main diagonal are treated as the linear predictor associated with the correlation between each coordinate of the outcome, otherwise Tau is treated as the precision matrix. The user cannot specify Tau with V or Sd.
#' @param Sd character or numeric: If Sd is a character, it is interpreted as the names of the linear predictors associated with the standard deviation parameter of the Normal distribution. If Sd is numeric, the standard deviation is considered known and equal to the value of Sd, otherwise, the precision is considered unknown and equal to the exponential of the linear predictor informed by in Sd. If the outcome is a Multivariate Normal, then Tau must be a matrix and the elements outside its main diagonal are treated as the correlation (or the name of the linear predictor associated) between each coordinate of the outcome. The user cannot specify Sd with V or Tau.
#' @param data numeric: Values of the observed data.
#'
#' @return A object of the class dlm_distr
#' @importFrom Rfast upper_tri.assign lower_tri.assign is.symmetric
#' @export
#'
#' @examples
#'
#' # Univariate Normal case
#' structure <- polynomial_block(mu = 1, D = 0.95) +
#'   polynomial_block(V = 1, D = 0.95)
#'
#' outcome <- Normal(mu = "mu", V = "V", data = cornWheat$corn.log.return[1:500])
#' fitted.data <- fit_model(structure, corn = outcome)
#' summary(fitted.data)
#' plot(fitted.data, plot.pkg = "base")
#'
#' # Bivariate Normal case
#' structure <- (polynomial_block(mu = 1, D = 0.95) +
#'   polynomial_block(V = 1, D = 0.95)) * 2 +
#'   polynomial_block(rho = 1, D = 0.95)
#'
#' outcome <- Normal(
#'   mu = c("mu.1", "mu.2"),
#'   V = matrix(c("V.1", "rho", "rho", "V.2"), 2, 2),
#'   data = cornWheat[1:500, c(4, 5)]
#' )
#' fitted.data <- fit_model(structure, cornWheat = outcome)
#' summary(fitted.data)
#' plot(fitted.data, plot.pkg = "base")
#'
#' @details
#'
#' If V/Sigma/Tau/Sd is a string, we use the method proposed in \insertCite{ArtigokParametrico;textual}{kDGLM}.
#' Otherwise, if V/Sigma/Tau/Sd is numeric, we follow the theory presented in \insertCite{WestHarr-DLM;textual}{kDGLM}.
#'
#' For the details about the implementation see  \insertCite{ArtigoPacote;textual}{kDGLM}.
#'
#' @seealso \code{\link{fit_model}}
#'
#' @family auxiliary functions for a creating outcomes
#'
#' @references
#'    \insertAllCited{}
Normal <- function(mu, V = NA, Tau = NA, Sd = NA, data) {
  offset <- as.matrix(data)**0
  alt.method <- FALSE
  data <- as.matrix(data)
  t <- if.null(dim(data)[1], length(data))
  r <- if.null(dim(data)[2], 1)

  parms <- list()

  # mu=deparse(substitute(mu))[[1]] |> check.expr()
  # V=deparse(substitute(V))[[1]] |> check.expr()
  # Tau=deparse(substitute(Tau))[[1]] |> check.expr()
  # Sigma=deparse(substitute(Sigma))[[1]] |> check.expr()
  # Sd=deparse(substitute(Sd))[[1]] |> check.expr()

  #### Consistancy check ####
  {
    if (length(mu) != r) {
      stop("Error: The number of means does not match the number of series.")
    }
    if ((any(!is.na(V)) + any(!is.na(Tau)) + any(!is.na(Sd))) > 1) {
      stop("Error: Can only specify one of V, Tau or Sd")
    }
    if ((any(!is.na(V)) + any(!is.na(Tau)) + any(!is.na(Sd))) == 0) {
      stop("Error: Must specify one of V, Tau or Sd")
    }
    Var <- as.matrix(if (any(!is.na(Tau))) {
      Tau
    } else if (any(!is.na(V))) {
      V
    } else {
      Sd
    })
    Var.name <- as.matrix(if (any(!is.na(Tau))) {
      "Precision (Tau)"
    } else if (any(!is.na(V))) {
      "Variance (V)"
    } else {
      "Scale (Sd)"
    })

    lower.index <- matrix(seq_len(r**2), r, r)[lower.tri(Var)]
    upper.index <- t(matrix(seq_len(r**2), r, r))[lower.tri(Var)]
    lower.flag <- lower.tri(Var)
    upper.flag <- upper.tri(Var)

    ###### Dimensions check ######
    if (!all(is.na(Var))) {
      if (!all(dim(Var) == r)) {
        stop(paste0("Error: The ", Var.name, " size is not compatible with the number of series. Expected ", r, "x", r, ", got ", paste0(dim(Var), collapse = "x")))
      }
    }

    ###### Symmetry test #####
    flags.up <- is.na(Var[upper.flag]) | is.null(Var[upper.flag])
    Var[upper.flag] <- ifelse(flags.up, Var[lower.flag], Var[upper.flag])

    flags.down <- is.na(Var[lower.flag]) | is.null(Var[lower.flag])
    Var[lower.flag] <- ifelse(flags.down, Var[upper.flag], Var[lower.flag])

    if (any(Var[lower.flag & !is.na(Var)] != Var[upper.flag & !is.na(Var)])) {
      if (!is.symmetric(Var)) {
        stop(paste0("Error: ", Var.name, " matrix is not symmetric."))
      }
    }

    ###### Under specification test #####
    if (any(is.na(Var))) {
      stop(paste0("Error: Covariance is not fully specified."))
    }
  }
  #### Specifying atributes and methods ####
  if (all(sapply(Var, is.numeric))) {
    pred.names <- c(mu)
    names(pred.names) <- c("mu")
    k <- r

    if (Var.name == "Standard deviation") {
      warning("Covariance matrix specified by the standard deviation. Non diagonal values are intepreted as correlation.")
    }

    Var <- if (Var.name == "Precision (Tau)") {
      ginv(Tau)
    } else if (Var.name == "Variance (V)") {
      V
    } else {
      Cor <- Sd
      Sd <- diag(r)
      diag(Sd) <- diag(Cor)
      diag(Cor) <- 1
      Var <- diag(Sd) %*% Cor %*% diag(Sd)
    }
    distr <- list(
      conj_distr = format_ft,
      norm_distr = format_param,
      update = update_Normal,
      smoother = generic_smoother,
      calc_pred = normal_pred,
      apply_offset = function(ft, Qt, offset) {
        list("ft" = ft, "Qt" = Qt)
      },
      link_function = function(x) {
        x
      },
      inv_link_function = function(x) {
        x
      },
      log_like_function = function(x, param) {
        dmvnorm(x, param, Var)
      },
      param.names = generic_param_names(r)
    )
    parms$V <- Var
    convert.mat.canom <- convert.mat.default <- diag(r)
    convert.canom.flag <- FALSE

    distr$alt.method <- FALSE
  } else {
    k <- r + r * (r + 1) / 2
    if (r > 2) {
      stop("Error: The general r-variated Normal case with unknown covariance matrix has not been implemented yet. Currently only r=1 and r=2 cases are supported.")
    }
    # warning("Covariance matrix is unknown. Beware non diagonal values are interpreted as correlation.")

    pred.names <- c(mu, diag(Var), Var[lower.index])
    mu.index <- seq_len(r)
    var.index <- mu.index + r
    cor.index <- (2 * r + 1):k

    if (r > 1) {
      names.mat <- matrix(paste0(
        "Correlation between series ",
        matrix(mu.index, r, r, byrow = TRUE),
        " and ", matrix(mu.index, r, r),
        " (rho.", matrix(mu.index, r, r, byrow = TRUE), matrix(mu.index, r, r), ")"
      ), r, r)
      diag(names.mat) <- paste0(Var.name, " for serie ", mu.index)
      names(pred.names) <- c(paste0("Mean for Serie ", mu.index, " (mu.", mu.index, ")"), diag(names.mat), names.mat[lower.index])
    } else {
      names(pred.names) <- c("Mean (mu)", Var.name)
    }
    distr <- list(
      conj_distr = if (r > 1) {
        convert_multi_NG_Normal
      } else {
        convert_NG_Normal
      },
      norm_distr = if (r > 1) {
        convert_multi_Normal_NG
      } else {
        convert_Normal_NG
      },
      update = if (r > 1) {
        update_multi_NG_correl
        # update_multi_NG_chol
      } else {
        if (alt.method) {
          # update_NG_alt
          update_NG
        } else {
          update_NG
        }
      },
      smoother = generic_smoother,
      calc_pred = multi_normal_gamma_pred,
      apply_offset = function(ft, Qt, offset) {
        list("ft" = ft, "Qt" = Qt)
      },
      link_function = if (r > 1) {
        function(x) {
          x[var.index, ] <- log(x[var.index, ])
          a <- x[cor.index, ]
          a <- ((a + 1) / 2)
          a <- log(a / (1 - a))
          x[cor.index, ] <- a
          return(x)
        }
        # function(x) {
        #   x[var.index, ] <- log(x[var.index, ])
        #   a=matrix(1,r,r)
        #   a=upper_tri.assign(a,x[cor.index, ])
        #   a=lower_tri.assign(a,x[cor.index, ])
        #   A=eigen(a)
        #   vals=log(A$values)
        #   Q=A$vectors
        #   a <- Q%*%diag(vals)%*%t(Q)
        #   # a_raw
        #   # for(i in 1:r){
        #   #   a[r,]= a[r,]/sqrt(a_raw[r,r])
        #   #   a[,r]= a[,r]/sqrt(a_raw[r,r])
        #   # }
        #   x[cor.index, ] <- lower_tri(a)
        #   return(x)
        # }
      } else {
        function(x) {
          x[var.index, ] <- log(x[var.index, ])
          return(x)
        }
      },
      inv_link_function = if (r > 1) {
        function(x) {
          x[var.index, ] <- exp(x[var.index, ])
          a <- x[cor.index, ]
          a <- 1 / (1 + exp(-a))
          a <- 2 * a - 1
          x[cor.index, ] <- a
          return(x)
        }
        # function(x) {
        #   x[var.index, ] <- exp(x[var.index, ])
        #   a=matrix(0,r,r)
        #   a=upper_tri.assign(a,x[cor.index, ])
        #   a=lower_tri.assign(a,x[cor.index, ])
        #   A=eigen(a)
        #   vals=exp(A$values)
        #   Q=A$vectors
        #   a <- Q%*%diag(vals)%*%t(Q)
        #   a_raw=a
        #   for(i in 1:r){
        #     a[r,]= a[r,]/sqrt(a_raw[r,r])
        #     a[,r]= a[,r]/sqrt(a_raw[r,r])
        #   }
        #   x[cor.index, ] <- lower_tri(a)
        #   return(x)
        # }
      } else {
        function(x) {
          x[var.index, ] <- exp(x[var.index, ])
          return(x)
        }
      },
      log_like_function = function(x, param) {
        mu <- param[mu.index]
        V <- diag(r)
        diag(V) <- param[var.index]
        V[upper.index] <- V[lower.index] <- param[cor.index]
        dmvnorm(x, mu, V)
      },
      param.names = c("mu", "c", "alpha", "beta")
    )
    if (r > 1) {
      distr$param.names <- paste(rep(distr$param.names, r), sort(rep(mu.index, 4)), sep = ".")
    }

    parms <- append(
      parms,
      list(
        alt.method = alt.method,
        mu.index = mu.index,
        var.index = var.index,
        cor.index = cor.index,
        upper.index = upper.index,
        lower.index = lower.index
      )
    )
    convert.mat.canom <- convert.mat.default <- diag(k)
    convert.canom.flag <- FALSE
    if (Var.name == "Scale (Sd)") {
      diag(convert.mat.canom)[var.index] <- -2
      diag(convert.mat.default)[var.index] <- -0.5
      convert.canom.flag <- TRUE
    } else if (Var.name == "Variance (V)") {
      diag(convert.mat.canom)[var.index] <- -1
      diag(convert.mat.default)[var.index] <- -1
      convert.canom.flag <- TRUE
    }

    distr$alt.method <- alt.method | r > 1
  }

  distr$pred.names <- pred.names
  distr$r <- r
  distr$k <- k
  distr$l <- k
  distr$t <- t
  distr$na.condition <- all_na
  distr$offset <- matrix(offset, t, r)
  distr$data <- matrix(data, t, r)
  distr$convert.mat.canom <- convert.mat.canom
  distr$convert.mat.default <- convert.mat.default
  distr$convert.canom.flag <- convert.canom.flag
  distr$parms <- parms
  distr$name <- "Normal"
  if (r > 1) {
    if (is.null(colnames(data))) {
      distr$sufix <- paste0(".", formatC(1:r, flag = "0", width = ceiling(log10(r))))
    } else {
      if (any(table(colnames(data)) > 1)) {
        stop("Error: Cannot have repeated names in data argument.")
      }
      if (!is.null(colnames(offset))) {
        if (any(colnames(data) != colnames(offset))) {
          stop("Error: Column names of offset argument do not match column names of data argument.")
        }
      }
      distr$sufix <- paste0(".", colnames(data))
    }
  }

  class(distr) <- "dlm_distr"

  return(distr)
}

#### Default Method ####
##### Normal with known variance #####

#' update_Normal
#'
#' Calculate posterior parameter for the Normal, assuming that the observed values came from a Normal model from which the covariance is known and the prior distribution for the mean vector have Normal distribution
#'
#' @param conj.param list: A vector containing the concentration parameters of the Normal.
#' @param ft numeric: A vector representing the means from the normal distribution. Not used in the default method.
#' @param Qt matrix: A matrix representing the covariance matrix of the normal distribution. Not used in the default method.
#' @param y numeric: A vector containing the observations.
#' @param parms list: A list of extra known parameters of the distribution. For this kernel, parms should containing the covariance matrix parameter (V) for the observational Normal model.
#'
#' @return The parameters of the posterior distribution.
#' @keywords internal
#'
#' @family auxiliary functions for a Normal outcome
update_Normal <- function(conj.param, ft, Qt, y, parms) {
  V <- parms$V
  if (length(V) == 1) {
    null.flag <- V == 0
  } else {
    null.flag <- all(diag(V) == 0)
  }

  if (null.flag) {
    Qt <- V * 0
    ft <- y
  } else {
    Tau0 <- ginv(Qt)
    Tau1 <- ginv(parms$V)

    Qt <- ginv(Tau0 + Tau1)
    ft <- Qt %*% (Tau0 %*% ft + Tau1 %*% y)
  }
  return(do.call(c, list(ft, Qt)))
}

#' normal_pred
#'
#' Calculate the values for the predictive distribution given the values of the parameter of the conjugated distribution of the linear predictor.
#' The data is assumed to have Normal distribution with known variance and its mean having distribution Normal.
#' In this scenario, the marginal distribution of the data is also Normal.
#'
#' @param conj.param list or data.frame: The parameters of the conjugated distributions of the linear predictor.
#' @param outcome numeric or matrix (optional): The observed values at the current time. Not used in this function.
#' @param parms list: A list of extra parameters for the model. For this function, it must contain the observational covariance matrix, V
#' @param pred.cred numeric: the desired credibility for the credibility interval.
#'
#' @return A list containing the following values:
#' \itemize{
#'    \item pred numeric/matrix: the mean of the predictive distribution of a next observation. Same type and shape as the parameter in model.
#'    \item var.pred numeric/matrix: the variance of the predictive distribution of a next observation. Same type and shape as the parameter in model.
#'    \item icl.pred numeric/matrix: the percentile of 100*((1-pred.cred)/2)% of the predictive distribution of a next observation. Same type and shape as the parameter in model.
#'    \item icu.pred numeric/matrix: the percentile of 100*(1-(1-pred.cred)/2)% of the predictive distribution of a next observation. Same type and shape as the parameter in model.
#'    \item log.like numeric: the The log likelihood for the outcome given the conjugated parameters.
#' }
#'
#' @importFrom Rfast data.frame.to_matrix
#' @importFrom stats qnorm
#' @keywords internal
#'
#' @family auxiliary functions for a Normal outcome
normal_pred <- function(conj.param, outcome = NULL, parms = list(), pred.cred = 0.95) {
  pred.flag <- !is.na(pred.cred)
  like.flag <- !is.null(outcome)
  if (!like.flag && !pred.flag) {
    return(list())
  }

  if (is.null(dim(conj.param))) {
    conj.param <- conj.param |> matrix(1, length(conj.param))
  }
  r <- dim(conj.param)[2]
  r <- (-1 + sqrt(1 + 4 * r)) / 2
  t <- dim(conj.param)[1]

  pred <- t(data.frame.to_matrix(conj.param[, 1:r, drop = FALSE]))
  var.pred <- conj.param[(r + 1):(r * r + r)] |>
    t() |>
    array(c(r, r, t))
  icl.pred <- matrix(NA, r, t)
  icu.pred <- matrix(NA, r, t)
  log.like <- rep(NA, t)
  if (like.flag) {
    outcome <- matrix(outcome, t, r)
  }
  for (i in seq_len(t)) {
    mu <- pred[, i]
    sigma2 <- (var.pred[, , i] + parms$V) |> matrix(r, r)
    if (pred.flag) {
      var.pred[, , i] <- sigma2
      # if (length(sigma2) > 1) {
      #   sigma2 <- diag(sigma2)
      # }
      icl.pred[, i] <- rep(qnorm((1 - pred.cred) / 2), r) * sqrt(diag(sigma2)) + mu
      icu.pred[, i] <- rep(qnorm(1 - (1 - pred.cred) / 2), r) * sqrt(diag(sigma2)) + mu
    }
    if (like.flag) {
      log.like[i] <- dmvnorm(outcome[i, ], mu, sigma2)
    }
  }
  if (!pred.flag) {
    pred <- NULL
    var.pred <- NULL
    icl.pred <- NULL
    icu.pred <- NULL
  }
  if (!like.flag) {
    log.like <- NULL
  }

  list(
    "pred" = pred,
    "var.pred" = var.pred,
    "icl.pred" = icl.pred,
    "icu.pred" = icu.pred,
    "log.like" = log.like
  )
}

convert_Normal_Gamma_Normal <- function(ft, Qt, parms = list()) {
  ft <- matrix(ft, 2, 1)
  mu0 <- ft[1, ]

  # if(ft[2,]<=0){
  #   stop('Error: Precision mean cannot be negative.')
  # }
  # beta <- ft[2,]/Qt[2,2]
  # alpha <- ft[2,]*beta

  helper <- -3 + 3 * sqrt(1 + 2 * Qt[2, 2] / 3)
  alpha <- 1 / helper
  # alpha <- f_root(
  #   function(x) {
  #     trigamma(x) - Qt[2, 2]
  #   },
  #   function(x) {
  #     psigamma(x, 2)
  #   },
  #   alpha,
  #   tol = 1e-15
  # )$root
  # beta <- exp(digamma(alpha) - ft[2, ])
  beta <- exp(digamma(alpha) - ft[2, ])

  c0 <- beta / (Qt[1, 1] * alpha)
  # c0 <- 1 / Qt[1, 1]
  # c0 <- Qt[1, 1]
  return(list("mu0" = mu0, "c0" = c0, "alpha" = alpha, "beta" = beta))
}

convert_Normal_Normal_Gamma <- function(conj.param, parms = list()) {
  f1 <- conj.param$mu
  # f2 <- conj.param$alpha/conj.param$beta
  f2 <- digamma(conj.param$alpha) - log(conj.param$beta)
  q1 <- conj.param$beta / (conj.param$c * conj.param$alpha)
  # q1 <- 1 / conj.param$c
  # q1 <- conj.param$c
  # q2 <- conj.param$alpha/(conj.param$beta**2)
  # q2 <- trigamma(conj.param$alpha)
  helper <- 1 / conj.param$alpha
  q2 <- 3 * ((helper / 3 + 1)**2 - 1) / 2

  q12 <- 0

  ft <- c(f1, f2)
  Qt <- matrix(c(q1, q12, q12, q2), byrow = F, ncol = 2)
  return(list("ft" = ft, "Qt" = Qt))
}

##### Normal with unknown variance #####


convert_NG_Normal <- function(ft, Qt, parms = list()) {
  ft <- matrix(ft, 2, 1)
  mu0 <- ft[1, ] + Qt[1, 2]
  c0 <- exp(-ft[2, ] - Qt[2, 2] / 2) / (Qt[1, 1] + 1e-10)
  helper <- -3 + 3 * sqrt(1 + 2 * Qt[2, 2] / 3)
  # helper=Qt[2,2]
  alpha <- 1 / helper
  beta <- alpha * exp(-ft[2, ] - Qt[2, 2] / 2)
  return(list("mu0" = mu0, "c0" = c0, "alpha" = alpha, "beta" = beta))
}

convert_Normal_NG <- function(conj.param, parms = list()) {
  # q12 <- conj.param$mu0*(digamma(conj.param$alpha)-log(conj.param$beta))/(1-log(conj.param$alpha)+log(conj.param$beta))
  f1 <- conj.param$mu #-q12
  f2 <- digamma(conj.param$alpha) - log(conj.param$beta + 1e-40)
  # f2 <- log(conj.param$alpha)-log(conj.param$beta)
  q1 <- conj.param$beta / (conj.param$c * conj.param$alpha)
  q2 <- trigamma(conj.param$alpha)
  q12 <- 0

  ft <- c(f1, f2)
  Qt <- matrix(c(q1, q12, q12, q2), byrow = F, ncol = 2)
  return(list("ft" = ft, "Qt" = Qt))
}

#' update_NG
#'
#' Calculate posterior parameter for the Normal-Gamma, assuming that the observed values came from a Normal model from which the prior distribution for the mean and the precision have joint distribution Normal-Gamma
#'
#' @param conj.param list: A vector containing the parameters of the Normal-Gamma (mu0,c0,alpha,beta).
#' @param ft numeric: A vector representing the means from the normal distribution. Not used in the default method.
#' @param Qt matrix: A matrix representing the covariance matrix of the normal distribution. Not used in the default method.
#' @param y numeric: A vector containing the observations.
#' @param parms list: A list of extra known parameters of the distribution. Not used in this kernel.
#'
#' @return The parameters of the posterior distribution.
#' @keywords internal
update_NG <- function(conj.param, ft, Qt, y, parms = list()) {
  mu <- (conj.param$c * conj.param$mu + y) / (conj.param$c + 1)
  c <- conj.param$c + 1
  alpha <- conj.param$alpha + 0.5
  beta <- conj.param$beta + 0.5 * conj.param$c * ((conj.param$mu - y)**2) / (conj.param$c + 1)
  return(list("mu" = mu, "c" = c, "alpha" = alpha, "beta" = beta))
}

#' update_NG
#'
#' Calculate posterior parameter for the Normal-Gamma, assuming that the observed values came from a Normal model from which the prior distribution for the mean and the precision have joint distribution Normal-Gamma
#'
#' @param conj.param list: A vector containing the parameters of the Normal-Gamma (mu0,c0,alpha,beta).
#' @param ft numeric: A vector representing the means from the normal distribution. Not used in the default method.
#' @param Qt matrix: A matrix representing the covariance matrix of the normal distribution. Not used in the default method.
#' @param y numeric: A vector containing the observations.
#' @param parms list: A list of extra known parameters of the distribution. Not used in this kernel.
#'
#' @return The parameters of the posterior distribution.
#' @keywords internal
update_NG2 <- function(conj.param, ft, Qt, y, parms = list()) {
  mu0 <- conj.param$mu
  alpha0 <- conj.param$alpha
  beta0 <- conj.param$beta
  S0 <- beta0 / alpha0
  Rt <- S0 / conj.param$c
  # Rt=conj.param$c

  error <- y - mu0
  Q <- Rt + S0
  A <- Rt / Q

  alpha <- alpha0 + 0.5
  beta <- beta0 + 0.5 * S0 * (error**2) / Q
  S <- beta / alpha

  mu <- mu0 + A * error
  Ct <- A * S
  c <- beta / (Ct * alpha)
  # c <- Ct
  return(list("mu" = mu, "c" = c, "alpha" = alpha, "beta" = beta))
}
