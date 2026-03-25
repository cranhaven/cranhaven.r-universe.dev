# sample mu ------------------------------------------------------------------
w2_hat <- function(w2_0, phi, sig_eta, TT) {
  1 / (1/w2_0 + ((1-phi^2)+(TT-1)*(1-phi)^2) / sig_eta^2)
}
mu_hat <- function(w2, w2_0, mu_0, phi, TT, beta, sig_eta) {
  num <- (1-phi^2) * beta[1] + (1-phi) * sum(beta[-1] - phi * beta[-TT])
  w2 * (mu_0 / w2_0 + num / sig_eta^2)
}
sample_mu <- function(phi, sig_eta, beta, d, w2_0, TT, mu_0, K) {
  v <- sig_eta / sqrt(1-phi^2)
  w2 <- w2_hat(w2_0, phi, sig_eta, TT)
  mu <- mu_hat(w2, w2_0, mu_0, phi, TT, beta, sig_eta)
  # generates mu from truncated normal distribution
  mu_try <- stats::rnorm(1, mu, sqrt(w2))
  ntry <- 0
  while(ntry < 1000 && (d > abs(mu_try) + K * v)) {
    mu_try <- stats::rnorm(1, mu, sqrt(w2))
    ntry <- ntry + 1
  }
  mu_try
}

# sample phi -----------------------------------------------------------------
sample_phi <- function(beta, mu, TT, sig_eta, d, alpha_0, beta_0, phi, K) {
  # calcula os betas centralizados
  b_cen <- beta - mu
  # calcula a soma dos betas centralizados ao quadrado
  b_cen2 <- sum(b_cen[-c(1,TT)]^2)
  phi_hat <- sum(b_cen[-1]*b_cen[-TT]) / b_cen2
  sig_phi2 <- sig_eta^2 / b_cen2

  # só tenta gerar 1000 vezes
  ntry <- 0
  phi_try <- stats::rnorm(1, phi_hat, sqrt(sig_phi2))
  phi_star <- phi
  # a regra de parada exclui phi's não estacionários
  while(ntry < 1000 && (phi_try <= 0 || abs(phi_try) >= 1-1e-4 || (d > abs(mu)+K*sig_eta/sqrt(1-phi_try^2)))) {
    phi_try <- stats::rnorm(1, phi_hat, sqrt(sig_phi2))
    ntry <- ntry + 1
  }
  if (ntry < 1000) phi_star <- phi_try

  # accept
  dphi <- stats::dbeta((phi+1)/2, alpha_0, beta_0, log = TRUE)
  dphi_star <- stats::dbeta((phi_star+1)/2, alpha_0, beta_0, log = TRUE)
  # conta com os phi
  sq_phi <- log(sqrt(1-phi^2))
  sq_phi_star <- log(sqrt(1-phi_star^2))
  # razao e denominador conforme codigo
  dfrac <- exp(dphi_star + sq_phi_star - dphi - sq_phi)
  if (stats::runif(1) < dfrac) {
    phi_try
  } else {
    phi
  }
}

# sample sig_eta -------------------------------------------------------------
sample_sig_eta <- function(v0, V0, TT, beta, mu, phi, d, K) {
  b_cen <- beta - mu
  v_hat <- v0 + TT
  # no paper, b_cen[1] está ao quadrado, no código não
  V_hat <- V0 + (1-phi^2) * b_cen[1] + sum((b_cen[-1]-phi*b_cen[-TT])^2)
  sig_eta_try <- sqrt(1/stats::rgamma(1, v_hat/2, V_hat/2))
  ntry <- 0
  while(ntry < 1000 && (d > abs(mu) + K*sig_eta_try/sqrt(1-phi^2))) {
    sig_eta_try <- sqrt(1/stats::rgamma(1, v_hat/2, V_hat/2))
    ntry <- ntry + 1
  }
  sig_eta_try
}

# sample sig -----------------------------------------------------------------
sample_sig <- function(n0, S0, TT, beta, y, x) {
  n_hat <- n0 + TT * dim(x)
  S_hat <- S0 + sum((y - t(apply(x, 1, function(z) rowSums(z * beta))))^2)
  sqrt(1 / stats::rgamma(1, n_hat/2, S_hat/2))
}

# sample alpha --------------------------------------------------------------
sample_alpha <- function(mu0, s0, beta, sig, TT, ni, y, x) {
  if (is.null(dim(y))) {
    ybar <- mean(y)
  } else {
    ybar <- rowMeans(y)
  }
  a <- ybar - apply(x, 1, function(z) mean(z * beta))
  mu_new <- (TT * a + mu0 * s0) / (TT + s0)
  stats::rnorm(ni, mu_new, sig)
}

# sample beta_1:T ------------------------------------------------------------
Mt_inv <- function(t, sig, xt, sig_eta, phi, TT) {
  nk <- length(sig_eta)
  xx_sig <- (t(xt) %*% xt) / sig^2
  ## melhorando a eficiencia tirando o solve
  # sig_eta_inv <- solve(diag(nk)*sig_eta^2)
  sig_eta_inv <- diag(1/sig_eta^2)
  if (t == 1) {
    v2 <- diag(nk) * (sig_eta^2 / (1-diag(phi)^2))
    xx_sig + solve(v2) + sig_eta_inv * phi^2
  } else if (t == TT) {
    xx_sig + sig_eta_inv
  } else {
    xx_sig + sig_eta_inv %*% (diag(nk) + phi^2)
  }
}
mt <- function(t, sig, Mt, xt, yt, b_lag, b_lead, mu, TT, sig_eta, phi) {
  parte_xy <- (t(xt)%*%yt)/sig^2
  nk <- length(sig_eta)
  mu <- as.numeric(mu)
  ## melhorando a eficiencia tirando o solve
  # sig_eta_inv <- solve(diag(nk)*sig_eta^2)
  sig_eta_inv <- diag(1/sig_eta^2)
  if (t == 1) {
    ## melhorando a eficiencia tirando o solve
    # v2 <- diag(nk) * (sig_eta^2 / (1-diag(phi)^2))
    # parte_v2 <- solve(v2) %*% mu
    v2 <- diag(1/(sig_eta^2 / (1-diag(phi)^2)))
    parte_v2 <- v2 %*% mu
    parte_beta <- b_lead - (diag(nk)-phi) %*% mu
    parte_beta_mu <- parte_v2 + sig_eta_inv %*% phi %*% parte_beta
    Mt %*% (parte_xy + parte_beta_mu)
  } else if (t == TT) {
    parte_beta_mu <- sig_eta_inv %*% (phi%*%b_lag + (diag(nk)-phi) %*% mu)
    Mt %*% (parte_xy + parte_beta_mu)
  } else {
    parte_beta <- phi %*% (b_lag+b_lead)
    parte_mu <- (diag(nk) - 2*phi + phi^2) %*% mu
    Mt %*% (parte_xy + sig_eta_inv %*% (parte_beta + parte_mu))
  }
}
sample_beta_t <- function(t, beta_t, d_star, sig, xt, yt, b_lag, b_lead, mu, TT, phi, sig_eta) {

  Mt_inv <- Mt_inv(t, sig, xt, sig_eta, phi, TT)
  Mt <- solve(Mt_inv)
  mt <- mt(t, sig, Mt, xt, yt, b_lag, b_lead, mu, TT, sig_eta, phi)

  # accept
  beta_t_star <- mvnfast::rmvn(1, mt, Mt)
  dhn <- mvnfast::dmvn(beta_t_star, mt, Mt, log = TRUE)
  dho <- mvnfast::dmvn(beta_t, mt, Mt, log = TRUE)

  # recalculates Mt and mt for beta_t_star if it does not pass threshold
  if (any(abs(beta_t_star) < as.numeric(d_star))) {
    vxh <- t(apply(xt, 1, function(zt) {
      zt * (abs(beta_t_star) >= as.numeric(d_star))
    }))
    Mt_inv <- Mt_inv(t, sig, vxh, sig_eta, phi, TT)
    Mt <- solve(Mt_inv)
    mt <- mt(t, sig, Mt, vxh, yt, b_lag, b_lead, mu, TT, sig_eta, phi)
    dln <- mvnfast::dmvn(beta_t_star, mt, Mt, log = TRUE)
  } else {
    dln <- dhn
  }

  # recalculates Mt and mt for beta_t if it does not pass threshold
  if (any(abs(beta_t) < as.numeric(d_star))) {
    vxh <- t(apply(xt, 1, function(zt) {
      zt * (abs(beta_t) >= as.numeric(d_star))
    }))
    Mt_inv <- Mt_inv(t, sig, vxh, sig_eta, phi, TT)
    Mt <- solve(Mt_inv)
    mt <- mt(t, sig, Mt, vxh, yt, b_lag, b_lead, mu, TT, sig_eta, phi)
    dlo <- mvnfast::dmvn(beta_t, mt, Mt, log = TRUE)
  } else {
    dlo <- dho
  }

  # accept probability
  dfrac <- exp(dln - dhn - dlo + dho)
  if (stats::runif(1) < dfrac) {
    beta_t_star
  } else {
    beta_t
  }
}

# sample threshold d ---------------------------------------------------------
fXtb <- function(beta, d, x) {
  mat <- matrix(d, ncol = ncol(beta), nrow = nrow(beta), byrow = TRUE)
  b_star <- beta * (abs(beta) >= mat)
  apply(x, 1, function(z) Rfast::rowsums(z * b_star))
}

sample_d <- function(mu, K, sig_eta, phi, d, x, y, beta, sig, fast = FALSE) {

  nk <- ncol(phi)
  v <- sig_eta * solve(sqrt(diag(nk)-phi^2))
  lim_superior <- abs(mu) + K * diag(v)
  d_novo <- as.numeric(d)
  ty <- t(y)

  if (fast) {
    ## alternative: faster, but statistically wrong
    xb_velho <- fXtb(beta, d, x)
    d_novo <- stats::runif(nk) * lim_superior
    xb_novo <- fXtb(beta, d_novo, x)
    dln <- -0.5 * sum((ty - xb_novo)^2) / sig^2
    dlo <- -0.5 * sum((ty - xb_velho)^2) / sig^2
    if (stats::runif(1) < exp(dln-dlo)) d <- d_novo
  } else {
    xb_velho <- fXtb(beta, d, x)
    for(i in 1:nk) {
      d_novo[i] <- stats::runif(1) * lim_superior[i]
      xb_novo <- fXtb(beta, d_novo, x)
      dln <- -0.5 * sum((ty - xb_novo)^2) / sig^2
      dlo <- -0.5 * sum((ty - xb_velho)^2) / sig^2
      if (stats::runif(1) < exp(dln-dlo)) {
        d <- d_novo
        xb_velho <- fXtb(beta, d, x)
      }
    }
  }

  matrix(d, nrow = nk)
}
