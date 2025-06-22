#' @title Simulate Longitudinal Data with Missingness
#' @description Generates a dataset with longitudinal data containing missing covariates and outcomes. 
#' The function allows customization of random effects, residuals, and the alignment of covariates to simulate 
#' data under different conditions.
#'
#' @param n_subject Number of subjects in the dataset. Each subject has multiple observations. Default: \code{800}.
#' @param seed Random seed for reproducibility. Default: \code{123}.
#' @param nonrandeff Logical value indicating whether the random effects are non-normal. Default: \code{FALSE}.
#' @param nonresidual Logical value indicating whether the residuals are non-normal. Default: \code{FALSE}.
#' @param alligned Logical value indicating whether the covariates should be aligned (\code{TRUE}) 
#' or shuffled (\code{FALSE}). Default: \code{FALSE}. If it is shuffled, we will return covariate order as X1, X2, X3, X4, X5, X6, X9, X8, X7. If it is alligned, we will return covariate order as X1, X2, X3, X4, X5, X6, X7, X8, X9.
#' @return A list containing:
#' \describe{
#'   \item{\code{X_mis}}{Matrix of missing covariates.}
#'   \item{\code{Y_mis}}{Vector of missing outcomes.}
#'   \item{\code{Z}}{Matrix of complete random predictors.}
#'   \item{\code{subject_id}}{Vector of subject IDs.}
#'   \item{\code{time}}{Time points for each observation.}
#'   \item{\code{X_O}}{Matrix of original complete covariates (for evaluation).}
#'   \item{\code{Y_O}}{Vector of original complete outcomes (for evaluation).}
#' }
#'
#' @details This function creates longitudinal data for multiple subjects, each observed across 6  
#' time points. Non-normal or normal random effects and residual conditions can be specified. Missing values are introduced based MAR assumption. 
#' The alignment of covariates can be customized to test different imputation scenarios.
#'
#' @examples 
#' simulated_data <- simulation_imputation(
#'   n_subject = 800,
#'   seed = 123,
#'   nonrandeff = TRUE,
#'   nonresidual = TRUE,
#'   alligned = FALSE
#' )
#' @seealso 
#'  \code{\link[stats]{Normal}}, \code{\link[stats]{Uniform}}, \code{\link[stats]{Binomial}}, \code{\link[stats]{Chisquare}}, \code{\link[stats]{GammaDist}}
#'  \code{\link[mvtnorm]{Mvnorm}}
#'  \code{\link[sn]{dmst}}
#'  \code{\link[dplyr]{reexports}}, \code{\link[dplyr]{mutate}}, \code{\link[dplyr]{select}}
#'  \code{\link[tidyr]{pivot_wider}}, \code{\link[tidyr]{pivot_longer}}, \code{\link[tidyr]{reexports}}, \code{\link[tidyr]{separate}}
#'  \code{\link[arm]{invlogit}}
#' @rdname simulation_imputation
#' @export 
#' @importFrom stats rnorm runif rbinom rchisq rgamma
#' @importFrom mvtnorm rmvnorm
#' @importFrom sn rmst
#' @importFrom dplyr tibble as_tibble mutate select %>%
#' @importFrom tidyr pivot_wider pivot_longer starts_with separate
#' @importFrom arm invlogit
simulation_imputation = function(n_subject = 800, seed = NULL, nonrandeff = FALSE, nonresidual = FALSE, alligned = FALSE){
  if (!is.null(seed))
    set.seed(seed)
  n_obs_per_sub = 6
  nonlinear = TRUE
  n_obs_per_sub = sapply(1:n_subject, function(x) n_obs_per_sub)
  subject_id = c(unlist(sapply(1:n_subject, function(x) rep(x, n_obs_per_sub[x]))))
  n_obs = length(subject_id)
  Z = c(unlist(sapply(1:n_subject, function(x) 1:n_obs_per_sub[x])))
  trajectory = cbind(Z)
  Z = cbind(Z, Z^2)
  Z = cbind(rep(1, length(subject_id)), Z)
  
  
  Z = apply(Z[,-1], 2, scale)
  Z = cbind(1, Z)
  Z_O = cbind(Z)
  n = dim(Z)[1]
  
  X_1 = cbind(stats::rnorm(n_obs))
  X_1 = apply(X_1, 2, function(X_v){
    Bi = mvtnorm::rmvnorm(n_subject, rep(0, dim(Z)[2]))
    re = sapply(1:length(subject_id), function(x){
      Z[x,] %*% Bi[subject_id[x],]
    })
    X_v + re
  })
  
  X_2 = cbind(stats::rnorm(n_obs))
  X_2 = apply(X_2, 2, function(X_v){
    Bi = mvtnorm::rmvnorm(n_subject, rep(0, dim(Z)[2]))
    re = sapply(1:length(subject_id), function(x){
      Z[x,] %*% Bi[subject_id[x],]
    })
    X_v + re
  })
  
  X_3 = cbind(stats::rnorm(n_obs))
  X_3 = apply(X_3, 2, function(X_v){
    Bi = mvtnorm::rmvnorm(n_subject, rep(0, dim(Z)[2]))
    re = sapply(1:length(subject_id), function(x){
      Z[x,] %*% Bi[subject_id[x],]
    })
    X_v + re
  })
  
  X_4 = cbind(X_1 + X_2)
  if(nonlinear)
    X_4 = 0.5 * cbind(X_1 * X_2)
  X_4 = apply(X_4, 2, function(X_v){
    Bi = mvtnorm::rmvnorm(n_subject, rep(0, dim(Z)[2]))
    re = sapply(1:length(subject_id), function(x){
      Z[x,] %*% Bi[subject_id[x],]
    })
    X_v + re
  })
  X_4 = X_4 + stats::rnorm(n_obs, 0, 2)
  
  X_5 = cbind(X_2 + X_3)
  if(nonlinear)
    X_5 = 0.5 * cbind(X_2 * X_3)
  X_5 = apply(X_5, 2, function(X_v){
    Bi = mvtnorm::rmvnorm(n_subject, rep(0, dim(Z)[2]))
    re = sapply(1:length(subject_id), function(x){
      Z[x,] %*% Bi[subject_id[x],]
    })
    X_v + re
  })
  X_5 = X_5 + stats::rnorm(n_obs,  0, 2)
  
  X_6 = cbind(X_1 + X_3)
  if(nonlinear)
    X_6 = 0.5 * cbind(X_1 * X_3)
  X_6 = apply(X_6, 2, function(X_v){
    Bi = mvtnorm::rmvnorm(n_subject, rep(0, dim(Z)[2]))
    re = sapply(1:length(subject_id), function(x){
      Z[x,] %*% Bi[subject_id[x],]
    })
    X_v + re
  })
  X_6 = X_6 + stats::rnorm(n_obs, 2)
  
  
  if(nonlinear){
    X_7 = 0.3 * X_4 * X_5 - 0.2 * X_4 * X_6 + 0.4 * X_5^2
    if(nonresidual){
      X_7 = X_7 + stats::runif(n_obs, 0, 10)
    }else{
      X_7 = X_7 + stats::rnorm(n_obs,  0, 5)
    }
  }else{
    X_7 = X_4  + 2 * X_5 + 3 * X_6 - 5
    if(nonresidual){
      X_7 = X_7 + stats::runif(n_obs, 0, 5)
    }else{
      X_7 = X_7 + stats::rnorm(n_obs,  0, 2)
    }
  }
      
  X_7 = apply(X_7, 2, function(X_v){
    if (nonrandeff){
      Bi = sapply(1:(dim(Z)[2]), function(i){
        (-1)^stats::rbinom(n_subject, 1, prob = 0.8) * stats::rchisq(n_subject, 1)
      })
    }else{
      Bi = mvtnorm::rmvnorm(n_subject, rep(0, dim(Z)[2]))
    }
    re = sapply(1:length(subject_id), function(x){
      Z[x,] %*% Bi[subject_id[x],]
    })
    X_v + re
  })
  
  if(nonlinear){
    X_8 = -(0.2 * X_4 * X_5 - 0.5 * X_6^2 - 0.2 * X_5 ) - 0.05 * X_5 * X_7 + X_7
    if(nonresidual){
      X_8 = X_8 + stats::rgamma(n_obs, 10, 3)
    }else{
      X_8 = X_8 + stats::rnorm(n_obs, 0, 3)
    }
  }else{
    X_8 = X_4 - 5 * X_5 - X_6 + X_7
    if(nonresidual){
      X_8 = X_8 + stats::rgamma(n_obs, 5, 3)
    }else{
      X_8 = X_8 + stats::rnorm(n_obs, 0, 2)
    }
  }

  X_8 = apply(X_8, 2, function(X_v){
    if (nonrandeff){
      Bi = sapply(1:(dim(Z)[2]), function(i){
        stats::rgamma(n_subject, 2, 1)
      })
    }else{
      Bi = mvtnorm::rmvnorm(n_subject, rep(0, dim(Z)[2]))
    }
    re = sapply(1:length(subject_id), function(x){
      Z[x,] %*% Bi[subject_id[x],]
    })
    X_v + re
  })
  
  
  
  if(FALSE){
    X_9 = - 0.5 * X_4 * X_6 - 0.01 * X_5^3 + 0.2 * (X_7 - 0.5 * X_8)*X_4
    if(nonresidual){
      X_9 = X_9 +  (-1)^stats::rbinom(n_obs, 1, prob = 0.5) * stats::rchisq(n_subject, 10)#stats::rnormmix(n_obs, mu = c(-10, -5, 0, 5, 10), sigma = c(1, 1, 1, 1, 1), lambda = c(1/5, 1/5, 1/5, 1/5, 1/5))
    }else{
      X_9 = X_9 + stats::rnorm(n_obs,  0, 7)
    }
  }else{
    X_9 = X_4 * -4 + 5 * X_5 - X_6 + X_7  + X_8
    if(nonresidual){
      X_9 = X_9 + (-1)^stats::rbinom(n_obs, 1, prob = 0.5) * stats::rchisq(n_subject, 10)#stats::rnormmix(n_obs, mu = c(-10, -5, 0, 5, 10), sigma = c(1, 1, 1, 1, 1), lambda = c(1/5, 1/5, 1/5, 1/5, 1/5))
    }else{
      X_9 = X_9 + stats::rnorm(n_obs,  0, 4)
    }
  }
    
  X_9 = apply(X_9, 2, function(X_v){
    if (nonrandeff){
      Bi = sapply(1:(dim(Z)[2]), function(i){
        (-1)^stats::rbinom(n_subject, 1, prob = 0.5) * stats::rgamma(n_subject, 10, 10)
      })
    }else{
      Bi = mvtnorm::rmvnorm(n_subject, rep(0, dim(Z)[2]))
    }
    re = sapply(1:length(subject_id), function(x){
      Z[x,] %*% Bi[subject_id[x],]
    })
    X_v + re
  })
 
 
  
  if(nonlinear){
    Y =          # Interaction1 (scaled down)
      0.3 * X_4^2 +        # Interaction2 (scaled down)
      0.03 * (X_5 * X_8) +        # Simple addition interaction (scaled down)
      0.4 * X_6^2 + 0.1 * (X_7 * X_6) + (X_4 - X_9)
    if(nonresidual){
      Y = Y + stats::rnorm(n_obs, -1, 3) * stats::rgamma(n_obs, 5, 3)
    }else{
      Y = Y + stats::rnorm(n_obs, 0, 4)
    }
  }else{
    Y = X_4 - X_5 + 3 * X_6 - 2 * X_7 + 0.5 * X_8 - X_9
    if(nonresidual){
      Y = Y + stats::rnorm(n_obs, -1, 3) * stats::rgamma(n_obs, 5, 3)
    }else{
      Y = Y + stats::rnorm(n_obs, 0, 4)
    }
  }
    
  if (nonrandeff){
    xi <- c(0, 0, 0)  # Location vector
    Omega <- matrix(c(1.0, 0.5, 0.3,
                      0.5, 1.0, 0.4,
                      0.3, 0.4, 1.0),
                    nrow = 3, byrow = TRUE)
    alpha <- c(1, 1, 1) 
    Bi <- sn::rmst(n = n_subject, xi = xi, Omega = Omega, alpha = alpha, nu = 5)
  }else{
    Bi = mvtnorm::rmvnorm(n_subject, rep(0, dim(Z)[2]))
  }
  re = sapply(1:length(subject_id), function(x){
    Z[x,] %*% Bi[subject_id[x],]
  })
  Y = Y + re
  
  
  data_O = dplyr::tibble(
    subject_id = subject_id,
    time = c(unlist(sapply(1:n_subject, function(x) 1:n_obs_per_sub[x]))),
    X1 = X_1[,1],
    X2 = X_2[,1],
    X3 = X_3[,1],
    X4 = X_4[,1],
    X5 = X_5[,1],
    X6 = X_6[,1],
    X7 = X_7[,1],
    X8 = X_8[,1],
    X9 = X_9[,1],
    Y = Y[,1]
  )
  
  
  data = data_O %>% 
    tidyr::pivot_wider(id_cols = "subject_id", names_from = "time", values_from = c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8", "X9", "Y"))
  
  
  
  
  for (i in c(38, 44, 50, 56)) {
    if(i == 38){
      logit_cov = as.data.frame(data[, c(i - 36, i - 18)])
      s_p = arm::invlogit(-1 * logit_cov[,1] + 1.5 * logit_cov[,2])
      s_p = n_subject * 0.1 * s_p / sum(s_p)
      s_p[s_p>=1] = 1
      R_p = which(stats::rbinom(n_subject, 1, s_p) == 1)
    }
    if(i == 44){
      logit_cov = as.data.frame(data[, c(i - 36, i - 18)])
      s_p = arm::invlogit(-1 * logit_cov[,1] - 2 * logit_cov[,2])
      s_p = n_subject * 0.1 * s_p / sum(s_p)
      s_p[s_p>=1] = 1
      R_p = which(stats::rbinom(n_subject, 1, s_p) == 1)
    }
    if(i == 50){
      logit_cov = as.data.frame(data[, c(i - 36, i - 18)])
      s_p = arm::invlogit(-1 * logit_cov[,1] - 1 * logit_cov[,2])
      s_p = n_subject * 0.1 * s_p / sum(s_p)
      s_p[s_p>=1] = 1
      R_p = which(stats::rbinom(n_subject, 1, s_p) == 1)
    }
    if(i == 56){
      logit_cov = as.data.frame(data[, c(2, 8, 14)])
      s_p = arm::invlogit(logit_cov[,1] + logit_cov[,2] + logit_cov[,3])
      s_p = n_subject * 0.1 * s_p / sum(s_p)
      s_p[s_p>=1] = 1
      R_p = which(stats::rbinom(n_subject, 1, s_p) == 1)
    }
    data[R_p,i] = NA
  }
  
  for (i in 39:43) {
    logit_cov = as.data.frame(data[, c(i - 36, i - 18, i - 1)])
    pr = arm::invlogit(-1 * logit_cov[,1] + 1.5 * logit_cov[,2] - logit_cov[,3])
    pr[is.na(pr)] = 0
    pr = n_subject * 0.1 * pr / sum(pr)
    pr[pr>=1] = 1
    R_p = which(stats::rbinom(n_subject, 1, pr) == 1)
    data[R_p,i] = NA
  }
  
  for (i in 45:49) {
    logit_cov = as.data.frame(data[, c(i - 36, i - 18, i - 1)])
    pr = arm::invlogit(-1 * logit_cov[,1] - 2 * logit_cov[,2] - logit_cov[,3])
    pr[is.na(pr)] = 0
    pr = n_subject * 0.1 * pr / sum(pr)
    pr[pr>=1] = 1
    R_p = which(stats::rbinom(n_subject, 1, pr) == 1)
    data[R_p,i] = NA
  }
  
  for (i in 51:55) {
    logit_cov = as.data.frame(data[, c(i - 36, i - 18, i - 1)])
    pr = arm::invlogit(-1 * logit_cov[,1] - 1 * logit_cov[,2] - logit_cov[,3])
    pr[is.na(pr)] = 0
    pr = n_subject * 0.1 * pr / sum(pr)
    pr[pr>=1] = 1
    R_p = which(stats::rbinom(n_subject, 1, pr) == 1)
    data[R_p,i] = NA
  }

  for (i in 57:61) {
    logit_cov = as.data.frame(data[, c(i - 54, i - 48, i - 42, i - 1)])
    pr = arm::invlogit(logit_cov[,1] + logit_cov[,2] + logit_cov[,3] +  logit_cov[,4]*0.1)
    pr[is.na(pr)] = 0
    pr = n_subject * 0.1 * pr / sum(pr)
    pr[pr>=1] = 1
    R_p = which(stats::rbinom(n_subject, 1, pr) == 1)
    data[R_p,i] = NA
  }

  
  b = dplyr::as_tibble(data) %>%
    tidyr::pivot_longer(cols = c(tidyr::starts_with("X"), tidyr::starts_with("Y")), names_prefix = "_", values_to = "value", names_to = "Variable") %>%
    tidyr::separate(col = "Variable", into = c("Variable", "Time"), sep = "_") %>% 
    tidyr::pivot_wider(id_cols = c("subject_id", "Time"), names_from = "Variable", values_from = "value") %>% 
    dplyr::mutate(followup = ifelse(
      is.na("X1") & is.na("X2") & is.na("X3") & is.na("X4") & is.na("X5") & is.na("X6") & is.na("X7") & is.na("Y"),
      0,
      1
    ))
  
  loss_pro = stats::rbinom(length(unique(b$subject_id)), 1, 0.1)
  for (i in 1:length(unique(b$subject_id))) {
    if(loss_pro[i] == 1){
      if(stats::rbinom(1, 1, 0.5) == 1){
        b[b$subject_id == unique(b$subject_id)[i] & b$Time %in% c(5,6), c("X7", "X8", "X9", "Y")] = NA
      }else{
        b[b$subject_id == unique(b$subject_id)[i] & b$Time %in% c(6), c("X7", "X8", "X9", "Y")] = NA
      }
    }
  }
  
  
  X_O = data_O[,3:12]
  X_mis = b[,3:12]
  
  Y_O = unlist(X_O[,10])
  Y = unlist(X_mis[,10])
  
  
  if(alligned == FALSE){
    X_O = X_O %>% dplyr::select("X1", "X2", "X3", "X4", "X5", "X6", "X9", "X8", "X7", "Y")
    X_mis = X_mis %>% dplyr::select("X1", "X2", "X3", "X4", "X5", "X6", "X9", "X8", "X7", "Y")
  }
  
  X_O = X_O[,1:9]
  X_mis = X_mis[,1:9]
  
  subject_id = data_O$subject_id
  
  omit_sub = sapply(unique(subject_id), function(sub){
    t = sum(subject_id == sub)
    X_sub = cbind(X_mis[subject_id == sub,], Y[subject_id == sub])
    if (sum(colSums(is.na(X_sub)) == t) > 0){
      return(sub)
    }
    return(NA)
  })
  omit_sub = omit_sub[!is.na(omit_sub)]
  if(length(omit_sub) > 0){
    warning(paste0("Some variables of ", length(omit_sub), " subjects are missing at all time points, we delete data from these ", length(omit_sub), " subjects:\n", paste(omit_sub, collapse = ", ")))
    X_mis = X_mis[!subject_id %in% omit_sub,]
    X_O = X_O[!subject_id %in% omit_sub,]
    Y = Y[!subject_id %in% omit_sub]
    Y_O = Y_O[!subject_id %in% omit_sub]
    Z_O = Z_O[!subject_id %in% omit_sub,]
    
    
    trajectory = trajectory[!subject_id %in% omit_sub]
    Bi = Bi[-omit_sub,]
    re = re[!subject_id %in% omit_sub]
    subject_id = subject_id[!subject_id %in% omit_sub]
  }
  
  
  return(list(Y_mis = Y, Y_O = Y_O, X_O = as.matrix(X_O), X_mis = as.matrix(X_mis), Z = Z_O, subject_id = subject_id, time = trajectory))
}
