#' generic_smoother
#'
#' Generic smoother for all models.
#'
#' @param mt matrix: A matrix containing the filtered mean of the latent states at each time. Each row should represent one variable.
#' @param Ct array: A 3D-array representing the filtered covariance matrix of the latent states at each time. The third dimension should represent the time index.
#' @param at matrix: A matrix containing the one-step-ahead mean of the latent states at each time based upon the filtered mean. Each row should represent one variable.
#' @param Rt array: A 3D-array representing the one-step-ahead covariance matrix of the latent states at each time based upon the filtered covariance matrix. The third dimension should represent the time index.
#' @param G array: A 3D-array representing the transition matrix of the model at each time.
#' @param G.labs matrix: A character matrix containing the type associated with each value in G.
#' @param G.idx matrix: A numeric matrix containing the index associated with each value in G.
#'
#' @return A list containing the smoothed mean (mts) and covariance (Cts) of the latent states at each time. Their dimension follows, respectively, the dimensions of mt and Ct.
#'
#' @importFrom Rfast transpose
#' @keywords internal
#'
#' @details
#'
#' For the models covered in this package, we always assume that the latent states have Multivariate Normal distribution. With that assumption, we can use Kalman Smoother algorithm to calculate the posterior of the states at each time, given everything that has been observed (assuming that we already know the filtered distribution of the states).
#'
#' For the details about the implementation see \insertCite{ArtigoPacote;textual}{kDGLM}.
#'
#' For the details about the algorithm implemented see \insertCite{ArtigokParametrico;textual}{kDGLM}, \insertCite{Petris-DLM;textual}{kDGLM}, chapter 2, \insertCite{WestHarr-DLM;textual}{kDGLM}, chapter 4, and \insertCite{Kalman_filter_origins;textual}{kDGLM}.
#'
#' @seealso \code{\link{fit_model}}
#' @seealso \code{\link{analytic_filter}}
#' @references
#'    \insertAllCited{}
generic_smoother <- function(mt, Ct, at, Rt, G, G.labs, G.idx) {
  T_len <- dim(mt)[2]
  mts <- mt
  Cts <- Ct
  if (T_len > 1) {
    for (t in (T_len - 1):1) {
      mt.step <- mt[, t, drop = FALSE]
      Ct.step <- Ct[, , t]
      Rt.step <- Rt[, , t + 1]
      G.step <- calc_current_G(mt.step, Ct.step, G[, , t + 1], G.labs, G.idx)$G
      # G.step <- G[, , t + 1]

      simple.Rt.inv <- Ct.step %*% transpose(G.step) %*% ginv(Rt.step)
      simple.Rt.inv.t <- transpose(simple.Rt.inv)

      mts[, t] <- mt.step + simple.Rt.inv %*% (mts[, t + 1] - at[, t + 1])
      Cts[, , t] <- Ct.step + simple.Rt.inv %*% (Cts[, , t + 1] - Rt.step) %*% simple.Rt.inv.t
    }
  }
  return(list("mts" = mts, "Cts" = Cts))
}

#' analytic_filter
#'
#' Fit a model given the observed value and the model parameters.
#'
#' @param outcomes list: The observed data. It should contain objects of the class dlm_distr.
#' @param a1 numeric: The prior mean at the latent vector.
#' @param R1 matrix: The prior covariance matrix at the latent vector.
#' @param FF array: A 3D-array containing the planning matrix at each time. Its dimension should be n x k x t, where n is the number of latent states, k is the number of linear predictors in the model and t is the time series length.
#' @param FF.labs matrix: A character matrix containing the label associated with each value in FF.
#' @param G array: A 3D-array containing the evolution matrix at each time. Its dimension should be n x n x t, where n is the number of latent states and t is the time series length.
#' @param G.labs matrix: A character matrix containing the label associated with each value in G.
#' @param G.idx matrix: A numeric matrix containing the index associated with each value in G.
#' @param D array: A 3D-array containing the discount factor matrix at each time. Its dimension should be n x n x t, where n is the number of latent states and t is the time series length.
#' @param h matrix: A drift to be added after the temporal evolution (can be interpreted as the mean of the random noise at each time). Its dimension should be n x t, where t is the length of the series and n is the number of latent states.
#' @param H array: A 3D-array containing the covariance matrix of the noise at each time. Its dimension should be the same as D.
#' @param p.monit numeric (optional): The prior probability of changes in the latent space variables that are not part of its dynamic.
#' @param monitoring numeric: A vector of flags indicating which latent states should be monitored.
#'
#' @importFrom Rfast transpose
#'
#' @return A list containing the following values:
#' \itemize{
#'    \item mt matrix: The filtered mean of the latent states for each time. Dimensions are n x t.
#'    \item Ct array: A 3D-array containing the filtered covariance matrix of the latent states for each time. Dimensions are n x n x t.
#'    \item at matrix: The one-step-ahead mean of the latent states at each time. Dimensions are n x t.
#'    \item Rt array: A 3D-array containing the one-step-ahead covariance matrix for latent states at each time. Dimensions are n x n x t.
#'    \item ft matrix: The one-step-ahead mean of the linear predictors at each time. Dimensions are k x t.
#'    \item Qt array: A 3D-array containing the one-step-ahead covariance matrix for linear predictors at each time. Dimensions are k x k x t.
#'    \item ft.star matrix: The filtered mean of the linear predictors for each time. Dimensions are k x t.
#'    \item Qt.star array: A 3D-array containing the linear predictors matrix of the latent state for each time. Dimensions are k x k x t.
#'    \item FF array: The same as the argument (same values).
#'    \item G matrix: The same as the argument (same values).
#'    \item G.labs matrix: The same as the argument (same values).
#'    \item G.idx matrix: The same as the argument (same values).
#'    \item D array: The same as the argument (same values).
#'    \item h array: The same as the argument (same values).
#'    \item H array: The same as the argument (same values).
#'    \item W array: A 3D-array containing the effective covariance matrix of the noise for each time, i.e., considering both H and D. Its dimension are the same as H and D.
#'    \item monitoring numeric: The same as the argument (same values).
#'    \item outcomes list: The same as the argument outcomes (same values).
#'    \item pred.names numeric: The names of the linear predictors.
#' }
#'
#' @keywords internal
#' @details
#'
#' For the models covered in this package, we always use the approach described in \insertCite{ArtigokParametrico;textual}{kDGLM}, including, in particular, the filtering algorithm presented in that work.
#'
#' For the details about the implementation see \insertCite{ArtigoPacote;textual}{kDGLM}.
#'
#' For the details about the algorithm implemented see \insertCite{ArtigokParametrico;textual}{kDGLM}, \insertCite{Petris-DLM;textual}{kDGLM}, chapter 2, \insertCite{WestHarr-DLM;textual}{kDGLM}, chapter 4, and \insertCite{Kalman_filter_origins;textual}{kDGLM}.
#'
#' @seealso \code{\link{fit_model}}
#' @seealso \code{\link{generic_smoother}}
#' @references
#'    \insertAllCited{}
analytic_filter <- function(outcomes, a1 = 0, R1 = 1,
                            FF, FF.labs, G, G.labs, G.idx, D, h, H,
                            p.monit = NA, monitoring = FALSE) {
  # Defining quantities
  T_len <- dim(FF)[3]
  n <- dim(FF)[1]
  k <- dim(FF)[2]


  na.flag <- rep(TRUE, T_len)
  for (outcome.name in names(outcomes)) {
    outcome <- outcomes[[outcome.name]]
    na.condition <- outcome$na.condition
    data <- outcome$data
    offset <- outcome$offset
    outcomes[[outcome.name]]$na.flag <- sapply(1:T_len, function(i) {
      na.condition(data[i, ], offset[i, ])
    })

    na.flag <- na.flag & outcomes[[outcome.name]]$na.flag
  }
  D[, , na.flag] <- 0

  pred.names <- colnames(FF)
  if (length(monitoring) == 1) {
    monitoring <- rep(monitoring, n)
  }
  D.flags <- (D == 0) | array(crossprod(t(monitoring), t(monitoring)) == 0, c(n, n, T_len))
  D <- ifelse(D == 0, 1, D)
  D.inv <- 1 / D

  a1 <- matrix(a1, n, 1)
  R1 <- R1
  W <- array(NA, dim = c(n, n, T_len))

  mt <- matrix(NA, nrow = n, ncol = T_len)
  Ct <- array(NA, dim = c(n, n, T_len))
  at <- matrix(NA, nrow = n, ncol = T_len)
  Rt <- array(NA, dim = c(n, n, T_len))

  ft <- matrix(NA, nrow = k, ncol = T_len)
  Qt <- array(NA, dim = c(k, k, T_len))
  ft.star <- matrix(NA, nrow = k, ncol = T_len)
  Qt.star <- array(NA, dim = c(k, k, T_len))

  monitoring <- array(monitoring, c(n))
  null.flags <- G.labs == "noise.disc"

  last.m <- a1
  last.C <- R1
  models <- list()
  D.mult <- list("null.model" = 0, "alt.model" = 100)
  H.add <- list("null.model" = diag(n) * 0, "alt.model" = diag(n) * 0)
  monit.win <- 1
  log.like.null <- rep(NA, T_len)
  log.like.alt <- rep(-Inf, T_len)
  alt.flags <- rep(0, T_len)

  for (outcome.name in names(outcomes)) {
    if (!inherits(outcomes[[outcome.name]], "dlm_distr")) {
      stop(paste0("Error: Outcome contains is not of the right class Expected a dlm_distr object, got a ", class(outcomes[[outcome.name]]), " object."))
    }
  }

  c <- c.monit <- 1
  p <- if.na(p.monit, 0)
  threshold <- log(c.monit) + log(p) - log(1 - p)

  H.prev <- R1
  D.prev <- R1 * 0
  for (t in seq_len(T_len)) {
    model.list <- c("null.model")
    if (!is.na(p.monit)) {
      model.list <- c("alt.model", model.list)
    }
    FF.step <- matrix(FF[, , t], n, k, dimnames = list(NULL, pred.names))
    for (model in model.list) {
      models[[model]] <- list()
      D.p.inv <- D.inv[, , t]
      D.p <- D[, , t]

      G.now <- G[, , t] |> matrix(n, n)
      H.now <- H[, , t] |> matrix(n, n)
      H.now[which(monitoring), which(monitoring)] <- H.now[which(monitoring), which(monitoring)] + last.C[which(monitoring), which(monitoring)] * D.mult[[model]]

      H.holder <- 0
      if (na.flag[t]) {
        H.holder <- H.now
        H.now <- H.now + D.prev
      }

      if (any(null.flags)) {
        D.mat <- diag(D.p)
        weight <- (((t - 1) / t)) * D.mat
        # weight <- (((t-1) / t))
        # weight <- D.mat
        # weight <- D.mat
        m2 <- last.m %*% t(last.m) + last.C
        # m2=last.C*0
        # diag(m2)=diag(last.C)+last.m**2
        noise.est <- (weight * H.prev + (1 - weight) * m2) # /(1-(1-weight)**t)
        D.p.inv[null.flags] <- 1
        H.now[null.flags] <- noise.est[null.flags]
      }

      next.step <- one_step_evolve(
        last.m, last.C,
        G.now, G.labs, G.idx,
        D.p.inv, h[, t], H.now
      )

      models[[model]]$at <- at.step <- next.step$at
      models[[model]]$Rt <- Rt.step <- next.step$Rt
      models[[model]]$D.prev <- (next.step$W - H.holder)
      models[[model]]$W <- next.step$W
      models[[model]]$H <- H.now

      lin.pred <- calc_lin_pred(at.step, Rt.step, FF.step, FF.labs, pred.names, 1:k)
      models[[model]]$ft <- ft.step <- lin.pred$ft
      models[[model]]$Qt <- Qt.step <- lin.pred$Qt
      models[[model]]$FF <- lin.pred$FF


      if (!is.na(p.monit)) {
        log.like <- 0
        for (outcome.name in names(outcomes)) {
          outcome <- outcomes[[outcome.name]]

          offset.step <- outcome$offset[t, ]
          if (!outcome$na.flag[t]) {
            pred.index <- match(outcome$pred.names, pred.names)
            ft.canom <- ft.step[pred.index, , drop = FALSE]
            Qt.canom <- Qt.step[pred.index, pred.index, drop = FALSE]
            if (outcome$convert.canom.flag) {
              ft.canom <- outcome$convert.mat.canom %*% ft.canom
              Qt.canom <- outcome$convert.mat.canom %*% Qt.canom %*% transpose(outcome$convert.mat.canom)
            }
            offset.pred <- outcome$apply_offset(ft.canom, Qt.canom, offset.step)
            ft.canom <- offset.pred$ft
            Qt.canom <- offset.pred$Qt
            conj.prior <- outcome$conj_distr(ft.canom, Qt.canom, parms = outcome$parms)
            log.like <- log.like + outcome$calc_pred(conj.prior, outcome$data[t, ], parms = outcome$parms, pred.cred = NA)$log.like |> sum()
          }
        }

        models[[model]]$log.like <- ifelse(is.nan(log.like), -Inf, log.like)
      }
    }

    model <- models$null.model
    if (!is.na(p.monit)) {
      log.like.null[t] <- models$null.model$log.like
      log.like.alt[t] <- models$alt.model$log.like
      bayes.factor <- sum(log.like.null[(t - monit.win + 1):t] +
        -log.like.alt[(t - monit.win + 1):t], na.rm = TRUE) |>
        if.nan(0)

      if (monit.win > 0) {
        if (bayes.factor < threshold) {
          model <- models$alt.model
          conj.prior <- models$alt.model$conj.prior
          monit.win <- -6 # The -6 is to avoid consecutive interventions. After a intervention is done, you have to wait 5 observations before the next one.
          alt.flags[t] <- 1
        } else if (bayes.factor > 0) {
          monit.win <- 0
        }
      }
      monit.win <- monit.win + 1
    }
    ft.step <- model$ft
    Qt.step <- model$Qt
    H.prev <- model$H
    D.prev <- model$D.prev

    mt.step <- model$at
    Ct.step <- model$Rt
    FF.cur.step <- model$FF
    for (outcome.name in names(outcomes)) {
      outcome <- outcomes[[outcome.name]]
      pred.index <- match(outcome$pred.names, pred.names)

      offset.step <- outcome$offset[t, ]
      data.step <- outcome$data[t, ]
      if (!outcome$na.flag[t]) {
        lin.pred <- calc_lin_pred(mt.step, Ct.step, FF.step, FF.labs, pred.names, pred.index)
        ft.step.part <- lin.pred$ft
        Qt.step.part <- lin.pred$Qt
        Qt.inv <- ginv(Qt.step.part)
        FF.cur.step <- lin.pred$FF

        if (outcome$convert.canom.flag) {
          ft.step.part <- outcome$convert.mat.canom %*% ft.step.part
          Qt.step.part <- outcome$convert.mat.canom %*% Qt.step.part %*% transpose(outcome$convert.mat.canom)
        }
        offset.pred <- outcome$apply_offset(ft.step.part, Qt.step.part, offset.step)
        ft.step.part <- offset.pred$ft
        Qt.step.part <- offset.pred$Qt

        conj.prior <- outcome$conj_distr(ft.step.part, Qt.step.part, parms = outcome$parms)
        conj.post <- outcome$update(conj.prior,
          ft = ft.step.part, Qt = Qt.step.part,
          y = data.step, parms = outcome$parms
        )
        if (outcome$alt.method) {
          norm.post <- conj.post
        } else {
          norm.post <- outcome$norm_distr(conj.post, parms = outcome$parms)
        }
        ft.star.part <- norm.post$ft
        Qt.star.part <- norm.post$Qt
        error.ft <- (ft.star.part - ft.step.part)
        error.Qt <- (Qt.star.part - Qt.step.part)

        # print('#########################')
        # print(Qt.step.part)
        # print(Qt.star.part)

        if (outcome$convert.canom.flag) {
          error.ft <- outcome$convert.mat.default %*% error.ft
          error.Qt <- outcome$convert.mat.default %*% error.Qt %*% transpose(outcome$convert.mat.default)
        }

        At <- Ct.step %*% FF.cur.step[, pred.index] %*% Qt.inv
        mt.step <- mt.step + At %*% error.ft
        Ct.step <- Ct.step + At %*% error.Qt %*% t(At)
        if (any(is.nan(Qt.star.part))) {
          stop(paste0("The approximate posterior at time ", t, " could not be computed."))
        }
        if (any(is.nan(Ct.step))) {
          # print(eigen(Qt.step.part))
          # print(Qt.step.part)
          # print(Qt.star.part)
          stop(paste0("Invalid covariance matrix at time ", t, "."))
        }
        if (max(Ct.step - transpose(Ct.step)) > 2e-6) {
          Ct.step <- (Ct.step + transpose(Ct.step)) / 2
        }
      }
    }
    lin.pred <- calc_lin_pred(mt.step, Ct.step, FF.step, FF.labs, pred.names, 1:k)
    ft.star.step <- lin.pred$ft
    Qt.star.step <- lin.pred$Qt
    models[["null.model"]]$at.step <- mt.step
    models[["null.model"]]$Rt.step <- Ct.step

    at[, t] <- model$at
    Rt[, , t] <- model$Rt
    mt[, t] <- last.m <- mt.step
    Ct[, , t] <- last.C <- Ct.step

    ft[, t] <- ft.step
    Qt[, , t] <- Qt.step
    ft.star[, t] <- ft.star.step
    Qt.star[, , t] <- Qt.star.step

    W[, , t] <- model$W
  }

  result <- list(
    mt = mt, Ct = Ct,
    at = at, Rt = Rt,
    ft = ft, Qt = Qt,
    ft.star = ft.star, Qt.star = Qt.star,
    FF = FF, FF.labs = FF.labs,
    G = G, G.labs = G.labs, G.idx = G.idx,
    D = D, h = h, H = H, W = W,
    log.like.null = log.like.null,
    log.like.alt = log.like.alt,
    alt.flags = alt.flags,
    monitoring = monitoring, smooth = FALSE,
    outcomes = outcomes, pred.names = pred.names
  )
  return(result)
}

calc_current_G <- function(m0, C0, G, G.labs, G.idx) {
  n <- length(m0)
  G.now <- G |> matrix(n, n)

  drift <- m0 * 0
  flag.na <- rowSums(is.na(G.now)) >= 1
  index.na <- seq_len(n)[flag.na]
  if (any(flag.na)) {
    for (index.row in index.na) {
      index.col <- seq_len(n)[is.na(G.now[index.row, ])]
      flags.const <- G.labs[index.row, index.col] == "constrained"

      index.coef <- G.idx[index.row, index.col][flags.const]
      if (any(flags.const)) {
        index.const <- index.col[flags.const]
        rho <- tanh(m0[index.coef])
        G.now[index.row, index.const] <- rho
        G.now[index.row, index.coef] <- (1 + rho) * (1 - rho) * m0[index.const]
        drift[index.row] <- drift[index.row] - sum((1 + rho) * (1 - rho) * m0[index.col] * m0[index.col + 1])
      }

      flags.free <- G.labs[index.row, index.col] == "free"
      index.coef <- G.idx[index.row, index.col][flags.free]
      if (any(flags.free)) {
        index.free <- index.col[flags.free]
        G.now[index.row, index.free] <- m0[index.coef]
        G.now[index.row, index.coef] <- m0[index.free]
        drift[index.row] <- drift[index.row] - sum(m0[index.free] * m0[index.coef])
      }


      m1 <- m0
      C1 <- C0
      flags.kl <- G.labs[index.row, index.col] == "kl"
      index.coef <- G.idx[index.row, index.col][flags.kl]
      if (any(flags.kl)) {
        index.kl <- index.col[flags.kl]
        for (i in index.kl) {
          m1[i] <- m0[i] * m0[index.coef] + C0[i, index.coef]
          C1[i, i] <- C0[i, i] * C0[index.coef, index.coef] +
            C0[i, index.coef]**2 +
            2 * m0[i] * m0[index.coef] * C0[i, index.coef] +
            (m0[i]**2) * C0[index.coef, index.coef] + (m0[index.coef]**2) * C0[i, i]
          cov.vec <- c(
            C0[i, index.coef] * m0[i] + C0[i, i] * m0[index.coef],
            C0[i, index.coef] * m0[index.coef] + C0[index.coef, index.coef] * m0[i]
          )
          C1[-i, i] <-
            C1[i, -i] <-
            cov.vec %*% ginv(C0[c(i, index.coef), c(i, index.coef)]) %*% C0[c(i, index.coef), -i]
          G.now[i, i] <- 1
        }
        a1 <- G.now %*% m1
        R1 <- G.now %*% C1 %*% transpose(G.now)
        index.G <- c(seq_len(n)[-index.kl], index.kl)
        index.G.inv <- order(index.G)
        G.now <- create_G(C0[index.G, index.G], C1[index.G, index.G])[index.G.inv, index.G.inv]
        drift <- drift + a1 - G.now %*% m0

        # n_kl=sum(flags.kl)
        # m1[index.row]=0
        # C1[index.row, index.row]=0
        # C1[-index.row, index.row]=0
        # C1[index.row, -index.row]=0
        #
        # cross_prod=m0%*%t(m0)+C0
        #
        # m1[index.row] <- sum(diag(cross_prod[index.kl,index.kl+1]))
        # C1[index.row, index.row] <- sum(
        #   cross_prod[flags.kl,flags.kl]*cross_prod[flags.kl+1,flags.kl+1]+
        #     cross_prod[flags.kl,flags.kl+1]*cross_prod[flags.kl+1,flags.kl]
        # )
        #
        # if(n_kl>1){
        #   out_kl=index.kl[index.kl!=i]
        #
        #
        #   cross_prod[i,out_kl]*cross_prod[i+1,out_kl+1]+
        #     cross_prod[i,out_kl+1]*cross_prod[i+1,out_kl]
        #
        #   C1[index.row, index.row] <- C1[index.row, index.row]+
        #     C0[i, i] * C0[i + 1, i + 1] +
        #     C0[i, i + 1]**2 +
        #     2 * m0[i] * m0[i + 1] * C0[i, i + 1] +
        #     (m0[i]**2) * C0[i + 1, i + 1] + (m0[i + 1]**2) * C0[i, i]
        # }
      }
    }
  }

  a1 <- G.now %*% m0 + drift
  R1 <- G.now %*% C0 %*% transpose(G.now)

  list("a1" = a1, "R1" = R1, "G" = G.now, "drift" = drift)
}

one_step_evolve <- function(m0, C0, G, G.labs, G.idx, D.inv, h, H) {
  G.vals <- calc_current_G(m0, C0, G, G.labs, G.idx)

  # print('####################')
  # print(cbind(m0,G.vals$m1))
  # print(cbind(C0,G.vals$C1))
  G.now <- G.vals$G
  drift <- G.vals$drift
  at <- G.vals$a1 + h
  Pt <- G.vals$R1
  W <- ((D.inv - 1) * Pt) + H
  Rt <- W + Pt

  list("at" = at, "Rt" = Rt, "G" = G.now, "h" = h + drift, "W" = W)
}

calc_current_F <- function(at, Rt, FF, FF.labs, pred.names) {
  n <- dim(FF)[1]
  k <- dim(FF)[2]

  charge <- matrix(0, k, 1)
  at.mod <- c(at)
  Rt.mod <- Rt
  count.na <- sum(is.na(FF))
  if (any(is.na(FF) & (FF.labs == "const" | FF.labs == "Covariate"))) {
    stop("Error: Unexpected NA values in the FF matrix.")
  }
  while (count.na > 0) {
    flag.na <- colSums(is.na(FF)) > 0
    index.na <- seq_len(k)[flag.na]
    for (index.pred in index.na) {
      flag.var <- is.na(FF[, index.pred])
      index.var <- seq_len(n)[flag.var]
      for (index.effect in index.var) {
        effect.name <- FF.labs[index.effect, index.pred]
        effect.vals <- FF[, effect.name == pred.names]
        if (any(is.na(effect.vals))) {
          break
        }
        FF[index.effect, index.pred] <- sum(effect.vals * at.mod)
        FF[, index.pred] <- FF[, index.pred] + effect.vals * at.mod[index.effect]
        charge[index.pred, 1] <- charge[index.pred, 1] - at.mod[index.effect] * sum(effect.vals * at.mod)

        # FF[index.effect, index.pred] <- sum(effect.vals*exp(at.mod))
        # FF[, index.pred] <- FF[, index.pred] +  at.mod[index.effect]*effect.vals*exp(at.mod)
        # charge[index.pred, 1] <- charge[index.pred, 1] - sum(at.mod[index.effect]*at.mod*effect.vals*exp(at.mod))

        # FF[index.effect, index.pred] <- sum(effect.vals * log(exp(at.mod) + 1))
        # FF[, index.pred] <- FF[, index.pred] + at.mod[index.effect] * effect.vals / (1 + exp(-at.mod))
        # charge[index.pred, 1] <- charge[index.pred, 1] - sum(at.mod[index.effect] * at.mod * effect.vals / (1 + exp(-at.mod)))
      }
    }
    new.count.na <- sum(is.na(FF))
    if (count.na == new.count.na) {
      stop("Error: A circularity was detected in the specification of FF. Revise the definition of the linear predictors.")
    }
    count.na <- new.count.na
  }

  list("FF" = FF, "FF.diff" = charge)
}

calc_lin_pred <- function(at, Rt, FF, FF.labs, pred.names, pred.index) {
  FF.vals <- calc_current_F(at, Rt, FF, FF.labs, pred.names)
  FF <- FF.vals$FF
  FF.sep <- FF[, pred.index]
  FF.diff <- FF.vals$FF.diff
  # print('#########################')
  # print(FF)
  # print(at)
  # ft <- crossprod(FF.sep, at) + FF.diff[pred.index]
  # Qt <- crossprod(FF.sep, Rt) %*% FF.sep
  ft <- t(FF.sep) %*% at + FF.diff[pred.index]
  Qt <- t(FF.sep) %*% Rt %*% FF.sep
  list("ft" = ft, "Qt" = Qt, "FF" = FF, "FF.diff" = FF.diff)
}

format_ft <- function(ft, Qt, parms) {
  return(c(ft, Qt))
}

format_param <- function(conj.param, parms) {
  if (is.null(dim(conj.param))) {
    r.star <- length(conj.param)
    r <- (-1 + sqrt(1 + 4 * r.star)) / 2
    t <- 1
    ft <- conj.param[seq_len(r)] |> matrix(r, t)
    Qt <- conj.param[(r + 1):(r * r + r)] |> array(c(r, r, t))
  } else {
    r.star <- dim(conj.param)[2]
    t <- dim(conj.param)[1]
    r <- (-1 + sqrt(1 + 4 * r.star)) / 2
    ft <- conj.param[, seq_len(r)] |>
      data.frame.to_matrix() |>
      t()
    Qt <- conj.param[, (r + 1):(r * r + r)] |>
      data.frame.to_matrix() |>
      t() |>
      array(c(r, r, t))
  }
  if (t == 1) {
    Qt <- matrix(Qt, r, r)
  }
  return(list("ft" = ft, "Qt" = Qt))
}

generic_param_names <- function(k) {
  index <- seq_len(k)
  c(
    paste0("ft.", index),
    paste0(
      "Qt.",
      c(matrix(index, k, k)),
      c(matrix(index, k, k, byrow = TRUE))
    )
  )
}
