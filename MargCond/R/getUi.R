getUi <-
function(i, theta, ldat, outnames, Z., X., Dfix){
  re_sig <- diag(theta[[4]])
  diag(theta[[4]]) <- 1
  theta[[4]] <- diag(re_sig) %*% theta[[4]] %*% diag(re_sig)
  i_id <- which(ldat$ID == i)
  iDat <- ldat[i_id, ]
  Y_i  <- iDat$outcomes / rep(theta[[2]], each = nrow(iDat) / length(outnames))
  R_i  <- expandR(theta[[3]], length(Y_i) / length(outnames))
  E_bi.t   <- Ebi(theta[[4]], Z.[i_id, ], R_i, Y_i, X.[i_id, ], theta[[1]])
  Var_bi.t <- Vbi(theta[[4]], Z.[i_id, ], R_i)
  u1 <- t(X.[i_id, ]) %*% solve(R_i) %*% (Y_i - Z.[i_id, ] %*% E_bi.t - X.[i_id, ] %*% theta[[1]])
  u2 <- matrix(tapply(Y_i * (Y_i - Z.[i_id, ] %*% E_bi.t - (X.[i_id, ] %*% theta[[1]])) - 1, iDat$var, sum), ncol = 1)
  u3 <- matrix((Var_bi.t + E_bi.t %*% t(E_bi.t) - theta[[4]])[!Dfix], ncol = 1)
  return(rbind(u1, u2, u3))
}
