#' @export
intpoint_gmonl <- function(subj, U, y, kn, degree, d, lambda, tau, px,omega,range,mv){
  # d is the value of differencing order
  dim = length(y)
  if (px == length(lambda)) 
    lambdapx = lambda
  else lambdapx = rep(lambda, px)
  W = Weight_Ni(y, subj)$W # calculating weights (W) for the repeated measurements
  md = numeric(0)
  m = numeric(0)
  vb0 = numeric(0)
  for (k in 1:px) {
    m = c(m, kn[k] + degree[k])
    md = c(md, m[k] - d[k])
    vb0 = c(vb0, rep(0, ((m[k] - d[k]))))
  }
  b = c(W * y, rep(0, m[mv]-1),vb0)
  mtot = sum(m)
  dtot = sum(d)
  cum_mB = cumsum(m)
  cum_mA = c(1, c(cum_mB + 1))
  D = list()
  D0 = list()
  DD = matrix(NA, (mtot - dtot)+(m[mv]-1), mtot)
  cum_mdB = cumsum(c(md,md[mv]))
  cum_mdA = c(1, c(cum_mdB + 1))
  rhs_pen = matrix(NA, mtot, px)
  D[[px+1]] = matrix(0, (m[mv] - 1), mtot)
  D[[px+1]][1:(m[mv] - 1), (cum_mA[mv]:cum_mB[mv])] =omega*(-diff(diag(m[mv]),diff = 1))
  DD[cum_mdA[px+1]:cum_mdB[px+1], ] = D[[px+1]]
  for (k in 1:px) {
    D[[k]] = matrix(0, (m[k] - d[k]), mtot)
    D0[[k]] = diff(diag(m[k]), diff = d[k])
    D[[k]][(1:(m[k] - d[k])), (cum_mA[k]:cum_mB[k])] = lambdapx[k] * range[k]*
      D0[[k]]
    DD[cum_mdA[k]:cum_mdB[k], ] = D[[k]]
    rhs_pen[, k] = t(D[[k]]) %*% rep(1/2, (m[k] - d[k]))
  }
  DD = as.matrix.csr(DD)
  WU = as.matrix.csr(W * U)
  Fidelity = WU
  Penalty = DD
  FP = rbind(Fidelity, Penalty)
  rhs = (1 - tau) * (t(WU) %*% rep(1, dim)) + rowSums(rhs_pen)+t(D[[px+1]])%*%rep(1,m[mv]-1)
  tmpmax = floor(1e+05 + exp(-12.1) * (WU@ia[mtot] - 1)^2.35)
  fit = rq.fit.sfn(FP, b, rhs = rhs, control = list(tmpmax = tmpmax))
  alpha = fit$coef
  intout = list(alpha = alpha, D = D)
  return(intout)
}