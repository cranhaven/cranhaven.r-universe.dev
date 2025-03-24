simDat <-
function(n, fixed_effects, rand_effects, error_var = c(2, 2), error_structure = 'normal', rho = 0,
                    times = 1:5, X = NULL, Z = NULL){
  ID <- rep(1:n, each = length(times))
  if(class(error_var) != 'matrix') error_var <- diag(error_var)
  if(class(rho) != 'matrix'){
    rho1 <- matrix(rho, nrow = nrow(error_var), ncol = ncol(error_var))
    diag(rho1) <- rep(1, length(diag(rho1)))
    rho <- rho1
  }
  sig <- sqrt(error_var) %*% rho %*% sqrt(error_var)
  if(is.null(X)){
    X <- matrix(NA, nrow = n * length(times), ncol = length(fixed_effects[[1]]))
    X[, 1] <- 1
    X[, 2:length(fixed_effects[[1]])] <- rep(rbinom(n * (length(fixed_effects[[1]]) - 1), 1, .5), each = length(times))
  }
  means <- NULL
  for(i in 1:length(fixed_effects)){means <- cbind(means, X %*% fixed_effects[[i]])}
  Zu <- NULL
  if(length(rand_effects[[1]]) == 1){
    for(i in 1:length(rand_effects)){Zu <- cbind(Zu, rep(rnorm(n, sd = rand_effects[[i]]), each = length(times)))}
  } else{
    for(i in 1:length(rand_effects)){
      Zu. <- NULL
      for(j in 1:length(unique(ID))){
        Zu. <- c(Zu., Z[ID == unique(ID)[j], ] %*% mvrnorm(n = 1, mu = rep(0, nrow(rand_effects[[i]])), Sigma = rand_effects[[i]]))
      }
      Zu <- cbind(Zu, Zu.)
    }
  }
  outcome <- matrix(NA, nrow = n * length(times), ncol = length(fixed_effects))
  if(error_structure == 'normal'){
    for(i in 1:nrow(outcome)){
      outcome[i, ] <- mvrnorm(1, mu = means[i, ], Sigma = sig) + Zu[i, ]
    }
  }
  if(error_structure == '50:50 normal'){
    nsig <- sqrt(error_var + diag(rep(1, nrow(error_var)))) %*% rho %*% sqrt(error_var + diag(rep(1, nrow(error_var))))
    nrho <- cov2cor(nsig - diag(rep(1, nrow(sig))))
    sig  <- sqrt(error_var) %*% nrho %*% sqrt(error_var)
    mix_err <- matrix(sample(c(-1, 1), nrow(means) * ncol(means), replace = T, prob = c(.5, .5)), nrow = nrow(means), ncol = ncol(means))
    for(i in 1:nrow(outcome)){
      outcome[i, ] <- mvrnorm(1, mu = means[i, ] + mix_err[i, ], Sigma = sig) + Zu[i, ]
    }
  }
  if(!error_structure %in% c('normal', '50:50 normal')) stop('Unknown error specification.')
  colnames(outcome) <- paste('outcome', 1:length(fixed_effects), sep = '')
  out <- data.frame(cbind(outcome), X, ID, rep(times, n))
  names(out)[ncol(out)] <- 'times'
  return(out)
}
