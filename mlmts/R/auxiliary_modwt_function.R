

auxiliary_modwt_function <- function(Xi, wf, J){

  N <- dim(Xi)[1]
  K <- dim(Xi)[2]
  Xi.modwt <- apply(Xi, 2, waveslim::modwt, wf, J)
  Xi.modwt.bw <- lapply(Xi.modwt, waveslim::brick.wall, wf)
  Xi.var <- lapply(Xi.modwt.bw, waveslim::wave.variance)
  Xi.var.2 <- (do.call(cbind, lapply(Xi.var, '[[', 1)))
  features <- t(Xi.var.2)

  if (K > 1) {

    for (j in 1:(K-1))
      for (i in (j+1):K)
      {
        x <- waveslim::wave.correlation(Xi.modwt.bw[[j]], Xi.modwt.bw[[i]], N)[,1]
        features <- rbind(features, x)
      }

  }

  return(c(features[, -dim(features)[2]]))
}
