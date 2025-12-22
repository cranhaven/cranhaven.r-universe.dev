estimate.scam2<-function (G, optimizer, optim.method, rho, gamma, env, control)
{
  if (!(optimizer %in% c("bfgs", "nlm", "optim", "nlm.fd",
                         "efs")))
    stop("unknown outer optimization method")
  if (length(rho) == 0) {
    optimizer <- "no.sps"
  }
  if (optimizer == "bfgs") {
    b <- bfgs_gcv.ubre2(scam::gcv.ubre_grad, rho = rho, G = G,
                        gamma = gamma, env = env, control = control)
    sp <- exp(b$rho)
    object <- b$object
    object$gcv.ubre <- b$gcv.ubre
    object$dgcv.ubre <- b$dgcv.ubre
    object$termcode <- b$termcode
    object$check.grad <- b$check.grad
    object$dgcv.ubre.check <- b$dgcv.ubre.check
    object$conv.bfgs <- b$conv.bfgs
    object$iterations <- b$iterations
    object$score.hist <- b$score.hist
  }
  else if (optimizer == "optim") {
    if (!(optim.method[1] %in% c("Nelder-Mead", "BFGS",
                                 "CG", "L-BFGS-B", "SANN"))) {
      warning("unknown optim() method, `L-BFGS-B' were used")
      optim.method[1] <- "L-BFGS-B"
    }
    if (is.na(optim.method[2])) {
      warning("the second parameter of optim.method argument is not supplied, \n                      finite-difference approximation of the gradient were used")
      grr <- NULL
    }
    else if (!(optim.method[2] %in% c("fd", "grad"))) {
      warning("only `fd' and `grad' options are possible, finite-difference \n                              approximation of the gradient were used")
      grr <- NULL
    }
    else if (optim.method[2] == "grad")
      grr <- gcv.ubre.derivative
    else grr <- NULL
    b <- stats::optim(par = rho, fn = gcv.ubre, gr = grr, method = optim.method[1],
               G = G, control = list(factr = control$optim$factr,
                                     lmm = min(5, length(rho))), gamma = gamma, env = env)
    sp <- exp(b$par)
    gcv.ubre <- b$value
    dgcv.ubre <- NULL
    iterations <- b$counts
    termcode <- b$convergence
    if (termcode == 0)
      conv <- "Successful completion"
    else if (termcode == 1)
      conv <- "The iteration limit `maxit' had been reached"
    else if (termcode == 10)
      conv <- "Degeneracy of the Nelder-Mead simplex"
    else if (termcode == 51)
      conv <- "A warning from the `L-BFGS-B' method; see help for `optim' for further details"
    else if (termcode == 52)
      conv <- "An error from the `L-BFGS-B' method; see help for `optim' for further details"
  }
  else if (optimizer == "nlm.fd") {
    b <- stats::nlm(f = gcv.ubre, p = rho, typsize = rho, stepmax = control$nlm$stepmax,
             ndigit = control$nlm$ndigit, gradtol = control$nlm$gradtol,
             steptol = control$nlm$steptol, iterlim = control$nlm$iterlim,
             G = G, gamma = gamma, env = env, control = control)
  }
  else if (optimizer == "nlm") {
    b <- stats::nlm(f = dgcv.ubre.nlm, p = rho, typsize = rho,
             stepmax = control$nlm$stepmax, ndigit = control$nlm$ndigit,
             gradtol = control$nlm$gradtol, steptol = control$nlm$steptol,
             iterlim = control$nlm$iterlim, G = G, gamma = gamma,
             env = env, control = control)
  }
  else if (optimizer == "efs") {
    b <- efsudr.scam(G = G, lsp = rho, gamma = gamma, env = env,
                     control = control)
    sp <- b$sp
    object <- b
    object$iterations <- b$niter
    object$conv <- b$outer.info$conv
    object$score.hist <- b$outer.info$score.hist
    object$gcv.ubre <- b$gcv
  }
  if (optimizer == "nlm.fd" || optimizer == "nlm") {
    sp <- exp(b$estimate)
    gcv.ubre <- b$minimum
    dgcv.ubre <- b$gradient
    iterations <- b$iterations
    termcode <- b$code
    if (termcode == 1)
      conv <- "Relative gradient is close to zero, current iterate is probably solution"
    else if (termcode == 2)
      conv <- "Successive iterates within tolerance, current iterate is probably solution"
    else if (termcode == 3)
      conv <- "Last global step failed to locate a point lower than `estimate'. Either \n                       `estimate' is an approximate local minimum of the function or \n                        `steptol' is too small"
    else if (termcode == 4)
      conv <- "Iteration limit exceeded"
    else if (termcode == 5)
      conv <- "Maximum step size `stepmax' exceeded five consecutive \n                         times. Either the function is unbounded below, becomes asymptotic \n                         to a finite value from above in some direction or stepmax is too small"
  }
  if (optimizer == "nlm.fd" || optimizer == "nlm" || optimizer ==
      "optim") {
    object <- scam::scam.fit(G = G, sp = sp, env = env, control = control)
    object$gcv.ubre <- gcv.ubre
    object$dgcv.ubre <- dgcv.ubre
    object$termcode <- termcode
    object$conv <- conv
    object$iterations <- iterations
  }
  else if (optimizer == "no.sps") {
    object <- scam::scam.fit(G = G, sp = exp(rho), env = env,
                       control = control)
    sp <- rep(0, 0)
  }
  if (optimizer == "optim") {
    object$optim.method <- rep(NA, 2)
    object$optim.method[1] <- optim.method[1]
    if (!is.null(grr))
      object$optim.method[2] <- "grad"
  }
  object$sp <- sp
  object$q.f <- G$q.f
  object$p.ident <- G$p.ident
  object$S <- G$S
  object
}
bfgs_gcv.ubre2<-function (fn = scam::gcv.ubre_grad, rho, ini.fd = TRUE, G, gamma = 1,
                          env, n.pen = length(rho), typx = rep(1, n.pen), typf = 1,
                          control)
{
  Sp <- 1/typx
  maxNstep <- control$bfgs$maxNstep
  rho1 <- rho
  old.rho <- rho
  not.exp <- G$not.exp
  b <- fn(rho, G,  env, control = control)
  old.score <- score <- b$gcv.ubre
  max.step <- if(!is.null(control$bfgs$max.step))control$bfgs$max.step else 200
  score.hist <- rep(NA, max.step + 1)
  score.hist[1] <- score
  grad <- b$dgcv.ubre
  scale.est <- b$scale.est
  rm(b)
  if (ini.fd) {
    B <- matrix(0, n.pen, n.pen)
    feps <- 1e-04
    for (j in 1:n.pen) {
      rho2 <- rho
      rho2[j] <- rho[j] + feps
      b2 <- fn(rho2, G,  env, control = control)
      B[, j] <- (b2$dgcv.ubre - grad)/feps
      rm(b2)
    }
    B <- (B + t(B))/2
    eh <- eigen(B, symmetric = TRUE)
    eh$values <- abs(eh$values)
    thresh <- max(eh$values) * 1e-04
    eh$values[eh$values < thresh] <- thresh
    B <- eh$vectors %*% (t(eh$vectors)/eh$values)
  }
  else B <- diag(n.pen) * 100
  if(is.infinite(B))B <- diag(n.pen) * 100

  uconv.ind <- rep(TRUE, ncol(B))
  c1 <- 1e-04
  c2 <- 0.9
  score.scale <- abs(scale.est) + abs(score)
  unconv.ind <- abs(grad) > score.scale * control$bfgs$gradtol.bfgs
  if (!sum(unconv.ind))
    unconv.ind <- unconv.ind | TRUE
  consecmax <- 0
  for (i in 1:max.step) {
    Nstep <- 0 * grad
    Nstep[unconv.ind] <- -drop(B[unconv.ind, unconv.ind] %*%
                                 grad[unconv.ind])
    if (sum(Nstep * grad) >= 0) {
      Nstep <- -diag(B) * grad
      Nstep[!uconv.ind] <- 0
    }
    Dp <- Sp * Nstep
    Newtlen <- (sum(Dp^2))^0.5
    if (Newtlen > maxNstep) {
      Nstep <- maxNstep * Nstep/Newtlen
      Newtlen <- maxNstep
    }
    maxtaken <- FALSE
    retcode <- 2
    initslope <- sum(Nstep * grad)
    rellength <- max(abs(Nstep)/max(abs(rho), 1/Sp))
    alpha.min <- control$bfgs$steptol.bfgs/rellength
    alpha.max <- maxNstep/Newtlen
    ms <- max(abs(Nstep))
    if (ms > maxNstep) {
      alpha <- maxNstep/ms
      alpha.max <- alpha * 1.05
    }
    else {
      alpha <- 1
      alpha.max <- min(2, maxNstep/ms)
    }
    ii <- 0
    step <- alpha * Nstep
    curv.condition <- TRUE
    old.alpha=old.score1=10
    repeat {
      rho1 <- rho + alpha * Nstep
      b <- fn(rho = rho1, G, env, control = control)
      score1 <- b$gcv.ubre
      if (score1 <= score + c1 * alpha * initslope) {
        grad1 <- b$dgcv.ubre
        newslope <- sum(grad1 * Nstep)
        curv.condition <- TRUE
        if (newslope < c2 * initslope) {
          if (alpha == 1 && Newtlen < maxNstep) {
            repeat {
              old.alpha <- alpha
              old.score1 <- score1
              alpha <- min(2 * alpha, alpha.max)
              rho1 <- rho + alpha * Nstep
              b <- fn(rho = rho1, G,
                      env, control = control)
              score1 <- b$gcv.ubre
              if (score1 <= score + c1 * alpha * initslope) {
                grad1 <- b$dgcv.ubre
                newslope <- sum(grad1 * Nstep)
              }
              if (score1 > score + c1 * alpha * initslope)
                break
              if (newslope >= c2 * initslope)
                break
              if (alpha >= alpha.max)
                break
            }
          }
          if ((alpha < 1) || (alpha > 1 && (score1 >
                                            score + c1 * alpha * initslope))) {
            alpha.lo <- min(alpha, old.alpha)
            alpha.diff <- abs(old.alpha - alpha)
            if (alpha < old.alpha) {
              sc.lo <- score1
              sc.hi <- old.score1
            }
            else {
              sc.lo <- old.score1
              sc.hi <- score1
            }
            iter=1
            repeat {

              iter=iter+1

              alpha.incr <- -newslope * alpha.diff^2/(2 *
                                                        (sc.hi - (sc.lo + newslope * alpha.diff)))
              if (alpha.incr < 0.2 * alpha.diff)
                alpha.incr <- 0.2 * alpha.diff
              alpha <- alpha.lo + alpha.incr
              rho1 <- rho + alpha * Nstep
              b <- fn(rho = rho1, G,
                      env, control = control)
              score1 <- b$gcv.ubre
              if (score1 > score + c1 * alpha * initslope) {
                alpha.diff <- alpha.incr
                sc.hi <- score1
              }
              else {
                grad1 <- b$dgcv.ubre
                newslope <- sum(grad1 * Nstep)
                if (newslope < c2 * initslope) {
                  alpha.lo <- alpha
                  alpha.diff <- alpha.diff - alpha.incr
                  sc.lo <- score1
                }
              }
              if (abs(newslope) <= -c2 * initslope)
                break
              if (alpha.diff < alpha.min)
                break
              if (iter > 1000)
                break
            }
            if (newslope < c2 * initslope) {
              curv.condition <- FALSE
              score1 <- sc.lo
              rho1 <- rho + alpha.lo * Nstep
              b <- fn(rho = rho1, G,
                      env, control = control)
            }
          }
        }
        retcode <- 0
        if (newslope < c2 * initslope)
          curv.condition <- FALSE
        if (alpha * Newtlen > 0.99 * maxNstep)
          maxtaken <- TRUE
      }
      else if (alpha < alpha.min) {
        retcode <- 1
        rho1 <- rho
        b <- fn(rho = rho1, G, env, control = control)
      }
      else {
        ii <- ii + 1
        if (alpha == 1) {
          alpha.temp <- -initslope/(score1 - score -
                                      initslope)/2
        }
        else {
          A1 <- matrix(0, 2, 2)
          bb1 <- rep(0, 2)
          ab <- rep(0, 2)
          A1[1, 1] <- 1/alpha^2
          A1[1, 2] <- -1/old.alpha^2
          A1[2, 1] <- -old.alpha/alpha^2
          A1[2, 2] <- alpha/old.alpha^2
          bb1[1] <- score1 - score - alpha * initslope
          bb1[2] <- old.score1 - score - old.alpha *
            initslope
          ab <- 1/(alpha - old.alpha) * A1 %*% bb1
          disc <- ab[2]^2 - 3 * ab[1] * initslope
          if (ab[1] == 0)
            alpha.temp <- -initslope/ab[2]/2
          else alpha.temp <- (-ab[2] + disc^0.5)/(3 *
                                                    ab[1])
          if (alpha.temp > 0.5 * alpha)
            alpha.temp <- 0.5 * alpha
        }
        old.alpha <- alpha
        old.score1 <- score1
        if (alpha.temp <= 0.1 * alpha)
          alpha <- 0.1 * alpha
        else alpha <- alpha.temp
      }
      if (ii == control$bfgs$maxHalf)
        break
      if (retcode < 2)
        break
    }
    step <- alpha * Nstep
    old.score <- score
    old.rho <- rho
    rho <- rho1
    old.grad <- grad
    score <- score1
    grad <- b$dgcv.ubre
    score.hist[i + 1] <- score
    yg <- grad - old.grad
    skipupdate <- TRUE
    for (j in 1:n.pen) {
      closeness <- step[j] - B[j, ] %*% yg
      if (abs(closeness) >= control$bfgs$gradtol.bfgs *
          max(abs(grad[j]), abs(old.grad[j])))
        skipupdate <- FALSE
    }
    if (!curv.condition)
      skipupdate <- TRUE
    if (!skipupdate) {
      if (i == 1) {
        B <- B * alpha
      }
      rr <- 1/sum(yg * step)
      B <- B - rr * step %*% crossprod(yg, B)
      B <- B - rr * tcrossprod((B %*% yg), step) + rr *
        tcrossprod(step)
    }
    termcode <- 0
    if (retcode == 1) {
      if (max(abs(grad) * max(abs(rho), 1/Sp)/max(abs(score),
                                                  typf)) <= control$bfgs$gradtol.bfgs * 6.0554)
        termcode <- 1
      else termcode <- 3
    }
    else if (max(abs(grad) * max(abs(rho), 1/Sp)/max(abs(score),
                                                     typf)) <= control$bfgs$gradtol.bfgs * 6.0554)
      termcode <- 1
    else if (max(abs(rho - old.rho)/max(abs(rho), 1/Sp)) <=
             control$bfgs$steptol.bfgs)
      termcode <- 2
    else if (i == max.step)
      termcode <- 4
    else if (maxtaken) {
      consecmax <- consecmax + 1
      if (consecmax == 5)
        termcode <- 5
    }
    else consecmax <- 0
    if (termcode > 0)
      break
    else {
      converged <- TRUE
      score.scale <- abs(b$scale.est) + abs(score)
      unconv.ind <- abs(grad) > score.scale * control$bfgs$gradtol.bfgs
      if (sum(unconv.ind))
        converged <- FALSE
      if (abs(old.score - score) > score.scale * control$bfgs$gradtol.bfgs) {
        if (converged)
          unconv.ind <- unconv.ind | TRUE
        converged <- FALSE
      }
    }
  }
  if (termcode == 1)
    ct <- "Full convergence"
  else if (termcode == 2)
    ct <- "Successive iterates within tolerance, current iterate is probably solution"
  else if (termcode == 3)
    ct <- "Last step failed to locate a lower point than old.rho"
  else if (termcode == 4)
    ct <- "Iteration limit reached"
  else if (termcode == 5)
    ct <- "Five consecutive steps of length maxNstep have been taken"
  list(gcv.ubre = score, rho = rho, dgcv.ubre = grad, iterations = i,
       B = B, conv.bfgs = ct, object = b$object, score.hist = score.hist[!is.na(score.hist)],
       termcode = termcode, check.grad = b$check.grad, dgcv.ubre.check = b$dgcv.ubre.check)
}
scam2<-function (formula, family = stats::gaussian(), data = list(), gamma = 1,
                 sp = NULL, weights = NULL, offset = NULL, optimizer = "bfgs",
                 optim.method = c("Nelder-Mead", "fd"), scale = 0, knots = NULL,
                 not.exp = FALSE, start = NULL, etastart, mustart, control = list(),
                 AR1.rho = 0, AR.start = NULL, drop.unused.levels = TRUE)
{
  myfun <- get("scam.control", asNamespace("scam"))
  control <- do.call(myfun, control)
  gp <- mgcv::interpret.gam(formula)
  cl <- match.call()
  mf <- match.call(expand.dots = FALSE)
  mf$formula <- gp$fake.formula
  mf$family <- mf$control <- mf$scale <- mf$knots <- mf$sp <- mf$min.sp <- mf$H <- mf$select <- mf$drop.intercept <- mf$gamma <- mf$method <- mf$fit <- mf$paraPen <- mf$G <- mf$optimizer <- mf$optim.method <- mf$not.exp <- mf$in.out <- mf$AR1.rho <- mf$devtol.fit <- mf$steptol.fit <- mf$del <- mf$... <- NULL
  mf$drop.unused.levels <- drop.unused.levels
  mf[[1]] <- quote(stats::model.frame)
  pmf <- mf
  mf <- eval(mf, parent.frame())
  if (nrow(mf) < 2)
    stop("Not enough (non-NA) data to do anything meaningful")
  terms <- attr(mf, "terms")
  vars <- all_vars1(gp$fake.formula[-2])
  inp <- parse(text = paste("list(", paste(vars, collapse = ","),
                            ")"))
  if (!is.list(data) && !is.data.frame(data))
    data <- as.data.frame(data)
  dl <- eval(inp, data, parent.frame())
  names(dl) <- vars
  var.summary <- variable.summary(gp$pf, dl, nrow(mf))
  rm(dl)
  if (is.list(formula)) {
    environment(formula) <- environment(formula[[1]])
    pterms <- list()
    tlab <- rep("", 0)
    for (i in 1:length(formula)) {
      pmf$formula <- gp[[i]]$pf
      pterms[[i]] <- attr(eval(pmf, parent.frame()), "terms")
      tlabi <- attr(pterms[[i]], "term.labels")
      if (i > 1 && length(tlabi) > 0)
        tlabi <- paste(tlabi, i - 1, sep = ".")
      tlab <- c(tlab, tlabi)
    }
    attr(pterms, "term.labels") <- tlab
  }
  else {
    pmf$formula <- gp$pf
    pmf <- eval(pmf, parent.frame())
    pterms <- attr(pmf, "terms")
  }
  if (is.character(family))
    family <- eval(parse(text = family))
  if (is.function(family))
    family <- family()
  if (is.null(family$family))
    stop("family not recognized")
  if (family$family[1] == "gaussian" && family$link == "identity")
    am <- TRUE
  else am <- FALSE
  if (AR1.rho != 0 && !is.null(mf$"(AR.start)"))
    if (!is.logical(mf$"(AR.start)"))
      stop("AR.start must be logical")
  if (AR1.rho != 0 && !am)
    stop("residual autocorrelation, AR1, is currently available only for the Gaussian identity link model.")
  if (!control$keepData)
    rm(data)
  G <- do.call(gam.setup, list(formula = gp, pterms = pterms,
                                      data = mf, knots = knots, sp = sp, absorb.cons = TRUE,
                                      sparse.cons = 0))
  G$var.summary <- var.summary
  G$family <- family
  if ((is.list(formula) && (is.null(family$nlp) || family$nlp !=
                            gp$nlp)) || (!is.list(formula) && !is.null(family$npl) &&
                                         (family$npl > 1)))
    stop("incorrect number of linear predictors for family")
  G$terms <- terms
  G$mf <- mf
  G$cl <- cl
  G$am <- am
  G$AR1.rho <- AR1.rho
  G$AR.start <- AR.start
  if (is.null(G$offset))
    G$offset <- rep(0, G$n)
  G$min.edf <- G$nsdf
  if (G$m)
    for (i in 1:G$m) G$min.edf <- G$min.edf + G$smooth[[i]]$null.space.dim
  G$formula <- formula
  G$pred.formula <- gp$pred.formula
  environment(G$formula) <- environment(formula)
  if (ncol(G$X) > nrow(G$X))
    stop("Model has more coefficients than data")
  n.terms <- length(G$smooth)
  n <- nrow(G$X)
  intercept <- G$intercept
  G$offset <- as.vector(stats::model.offset(mf))
  if (is.null(G$offset))
    G$offset <- rep.int(0, n)
  weights <- G$w
  fam.name <- G$family[1]
  if (scale == 0) {
    if (fam.name == "binomial" || fam.name == "poisson")
      sig2 <- 1
    else sig2 <- -1
  }
  else {
    sig2 <- scale
  }
  if (sig2 > 0)
    scale.known <- TRUE
  else scale.known <- FALSE
  Q <- penalty_pident(G)
  if (!is.null(sp)) {
    neg <- FALSE
    if (length(sp) != length(G$off)) {
      warning("Supplied smoothing parameter vector is too short - ignored.")
      sp <- NULL
    }
    else if (sum(is.na(sp))) {
      warning("NA's in supplied smoothing parameter vector - ignoring.")
      sp <- NULL
    }
    else {
      good <- sp < 0
      if (sum(good) > 0) {
        warning("Supplied smoothing parameter vector has negative values - ignored.")
        neg <- TRUE
      }
    }
    if (neg)
      sp <- NULL
  }
  env <- new.env()
  assign("start", rep(0, 0), envir = env)
  assign("dbeta.start", rep(0, 0), envir = env)
  assign("sp.last", rep(0, 0), envir = env)
  q.f <- rep(0, n.terms)
  if (n.terms > 0)
    for (i in 1:n.terms) q.f[i] <- ncol(G$smooth[[i]]$S[[1]]) +
    1
  G$S <- Q$S
  G$q.f <- q.f
  G$q0 <- G$off[1] - 1
  G$p.ident <- Q$p.ident
  G$n.terms <- n.terms
  G$weights <- weights
  G$sig2 <- sig2
  G$scale.known <- scale.known
  G$not.exp <- not.exp
  object <- list()
  if (is.null(sp)) {
    start <- etastart <- mustart <- NULL
    y <- G$y
    family <- G$family
    nobs <- NROW(y)
    eval(family$initialize)
    G$y <- y
    def.sp <- initial.sp.scam(G, Q, q.f = q.f, n.terms = n.terms,
                                     family = family, intercept = intercept, offset = G$offset,
                                     env = env, weights = weights, control = control)
    rho <- log(def.sp + 1e-04)
    ptm <- proc.time()
    G$gamma=gamma
    re <- estimate.scam2(G = G, optimizer = optimizer, optim.method = optim.method,
                         rho = rho, gamma = gamma, env = env, control = control)
    CPU.time <- proc.time() - ptm
    best <- re
    object$gcv.ubre <- re$gcv.ubre
    object$dgcv.ubre <- re$dgcv.ubre
    best$p.ident <- Q$p.ident
    best$S <- Q$S
    object$optimizer <- optimizer
    object$edf1 <- re$edf1
    object$termcode <- re$termcode
    if (optimizer == "bfgs") {
      object$check.grad <- re$check.grad
      object$dgcv.ubre.check <- re$dgcv.ubre.check
    }
  }
  else {
    best <- scam::scam.fit(G = G, sp = sp, gamma = gamma, env = env,
                            control = control)
    object$optimizer <- "NA"
  }
  best$n.smooth <- object$n.smooth <- n.terms
  best$formula <- object$formula <- formula
  best$family <- object$family <- G$family
  best$smooth <- object$smooth <- G$smooth
  best$model <- object$model <- G$mf
  object$R <- best$R
  if (is.null(object$R)) {
    rr <- scam::scam.fit(G = G, sp = best$sp, gamma = gamma, env = env,
                          control = control)
    object$R <- rr$R
  }
  object$sp <- best$sp
  names(object$sp) <- names(G$sp)
  if (sum(is.na(names(object$sp))) != 0) {
    if (n.terms > 0)
      for (i in 1:n.terms) names(object$sp)[i] <- object$smooth[[i]]$label
  }
  object$conv <- best$conv
  post <- scam.fit.post(G, object = best)
  object$edf <- best$edf
  object$edf1 <- post$edf1
  object$trA <- best$trA
  names(object$edf) <- G$term.names
  names(object$edf1) <- G$term.names
  object$aic <- post$aic
  object$null.deviance <- post$null.dev
  object$deviance <- post$deviance
  object$residuals <- post$residuals
  object$df.residual <- nrow(G$X) - sum(post$edf)
  object$rank <- post$rank
  object$var.summary <- G$var.summary
  object$cmX <- G$cmX
  object$model <- G$mf
  object$full.sp <- G$full.sp
  if (!is.null(object$full.sp))
    names(object$full.sp) <- names(G$full.sp)
  object$na.action <- attr(G$mf, "na.action")
  object$df.null <- post$df.null
  object$Ve <- post$Ve
  object$Vp <- post$Vb
  object$Ve.t <- post$Ve.t
  object$Vp.t <- post$Vb.t
  object$sig2 <- post$sig2
  object$coefficients <- best$beta
  object$coefficients.t <- best$beta.t
  object$beta <- best$beta
  object$beta.t <- best$beta.t
  object$pterms <- G$pterms
  object$terms <- G$terms
  object$assign <- G$assign
  object$contrasts <- G$contrasts
  object$xlevels <- G$xlevels
  object$nsdf <- G$nsdf
  object$y <- G$y
  if (control$keepData)
    object$data <- data
  object$control <- control
  object$offset <- G$offset
  object$not.exp <- G$not.exp
  object$scale.estimated <- !scale.known
  object$prior.weights <- weights
  object$weights <- best$w
  object$fitted.values <- post$mu
  object$linear.predictors <- post$eta
  object$call <- cl
  object$p.ident <- Q$p.ident
  object$intercept <- G$intercept
  object$min.edf <- G$min.edf
  object$gamma <- gamma
  object$iter <- best$iter
  if (is.null(sp))
    object$CPU.time <- CPU.time
  else object$CPU.time <- NULL
  object$AR1.rho <- AR1.rho
  if (is.null(sp)) {
    if (optimizer == "bfgs") {
      object$bfgs.info <- list()
      object$bfgs.info$conv <- re$conv.bfgs
      object$bfgs.info$iter <- re$iterations
      object$bfgs.info$grad <- re$dgcv.ubre
      object$bfgs.info$score.hist <- re$score.hist
    }
    else if (optimizer == "nlm.fd" || optimizer == "nlm") {
      object$nlm.info <- list()
      object$nlm.info$conv <- re$conv
      object$nlm.info$iter <- re$iterations
      object$nlm.info$grad <- re$dgcv.ubre
    }
    else if (optimizer == "optim") {
      object$optim.info <- list()
      object$optim.info$conv <- re$conv
      object$optim.info$iter <- re$iterations
      object$optim.method <- re$optim.method
    }
    else if (optimizer == "efs") {
      object$efs.info <- list()
      object$efs.info$conv <- re$conv
      object$efs.info$iter <- re$iterations
      object$efs.info$score.hist <- re$score.hist
    }
  }
  if (scale.known)
    object$method <- "UBRE"
  else object$method <- "GCV"
  if (G$nsdf > 0)
    term.names <- colnames(G$X)[1:G$nsdf]
  else term.names <- array("", 0)
  if (n.terms > 0)
    for (i in 1:n.terms) {
      k <- 1
      for (j in G$smooth[[i]]$first.para:G$smooth[[i]]$last.para) {
        term.names[j] <- paste(G$smooth[[i]]$label,
                               ".", as.character(k), sep = "")
        k <- k + 1
      }
    }
  names(object$coefficients) <- term.names
  names(object$coefficients.t) <- term.names
  ynames <- if (is.matrix(G$y))
    rownames(G$y)
  else names(G$y)
  names(object$residuals) <- ynames
  class(object) <- c("scam", "glm", "lm")
  rm(G)
  dev <- object$deviance
  # if (AR1.rho != 0) {
  #   object$std.rsd <- AR.resid(object$residuals, AR1.rho,
  #                              object$model$"(AR.start)")
  #   dev <- sum(object$std.rsd^2)
  #   object$deviance <- sum(object$residuals^2)
  # }
  object$aic <- object$family$aic(object$y, 1, object$fitted.values,
                                  object$prior.weights, dev)
  object$aic <- object$aic - 2 * (length(object$y) - sum(sum(object$model[["(AR.start)"]]))) *
    log(1/sqrt(1 - AR1.rho^2)) + 2 * sum(object$edf)
  object
}



initial.sp.scam2<-function (G, Q, q.f, n.terms, family, intercept, offset, env = env,
                            weights, control = control)
{
  control$devtol.fit <- 1e-04
  control$steptol.fit <- 1e-04
  b <- scam::scam.fit(G = G, sp = rep(0.05, length(G$off)), env = env,
                 control = control)
  H <- crossprod(b$wX1) - b$E
  n.p <- length(Q$S)
  def.sp <- array(0, n.p)
  j <- 1
  if (n.terms > 0)
    for (i in 1:n.terms) {
      for (kk in 1:length(G$smooth[[i]]$S)) {
        start <- G$off[j]
        finish <- start + ncol(G$smooth[[i]]$S[[kk]]) -
          1
        Hi.norm <- sum(H[start:finish, start:finish] *
                         H[start:finish, start:finish])
        Si.norm <- sum(G$smooth[[i]]$S[[kk]] * G$smooth[[i]]$S[[kk]])
        def.sp[j] <- (Hi.norm/Si.norm)^0.5
        j <- j + 1
      }
    }
  env <- new.env()
  assign("start", rep(0, 0), envir = env)
  assign("dbeta.start", rep(0, 0), envir = env)
  assign("sp.last", rep(0, 0), envir = env)
  def.sp
}
D2notExp<-function (x)
{
  f <- x
  ind <- x > 1
  f[ind] <- exp(1)
  ind <- (x <= 1) & (x > -1)
  f[ind] <- exp(x[ind])
  ind <- (x <= -1)
  f[ind] <- (12 * x[ind]^2 - 4)/exp(1)/(x[ind]^2 + 1)^3
  f
}
D3notExp<-function (x)
{
  f <- x
  ind <- x > 1
  f[ind] <- 0
  ind <- (x <= 1) & (x > -1)
  f[ind] <- exp(x[ind])
  ind <- (x <= -1)
  f[ind] <- 48 * x[ind] * (1 - x[ind]^2)/exp(1)/(x[ind]^2 +
                                                   1)^4
  f
}
DnotExp<-function (x)
{
  f <- x
  ind <- x > 1
  f[ind] <- exp(1) * x[ind]
  ind <- (x <= 1) & (x > -1)
  f[ind] <- exp(x[ind])
  ind <- (x <= -1)
  f[ind] <- -4 * x[ind]/exp(1)/(x[ind]^2 + 1)^2
  f
}

all_vars1<-function (form)
{
  vars <- all.vars(form)
  vn <- all.names(form)
  vn <- vn[vn %in% c(vars, "$", "[[")]
  if ("[[" %in% vn)
    stop("can't handle [[ in formula")
  ii <- which(vn %in% "$")
  if (length(ii)) {
    vn1 <- if (ii[1] > 1)
      vn[1:(ii[1] - 1)]
    go <- TRUE
    k <- 1
    while (go) {
      n <- 2
      while (k < length(ii) && ii[k] == ii[k + 1] - 1) {
        k <- k + 1
        n <- n + 1
      }
      vn1 <- c(vn1, paste(vn[ii[k] + 1:n], collapse = "$"))
      if (k == length(ii)) {
        go <- FALSE
        ind <- if (ii[k] + n < length(vn))
          (ii[k] + n + 1):length(vn)
        else rep(0, 0)
      }
      else {
        k <- k + 1
        ind <- if (ii[k - 1] + n < ii[k] - 1)
          (ii[k - 1] + n + 1):(ii[k] - 1)
        else rep(0, 0)
      }
      vn1 <- c(vn1, vn[ind])
    }
  }
  else vn1 <- vn
  vn1
}
dgcv.ubre.nlm<-function (rho, G, gamma, env, control)
{
  gg <- scam::gcv.ubre_grad(rho, G, gamma, env, control = control)
  attr(gg$gcv.ubre, "gradient") <- gg$gcv.ubre.rho
  gg$gcv.ubre
}

efsudr.scam<-function (G, lsp, gamma, env, control)
{
  deriv.dev.edf <- function(G, fit, sp) {
    b <- fit
    nsp <- length(G$S)
    q <- ncol(G$X)
    n <- nrow(G$X)
    y.mu <- drop(b$y) - b$mu
    c <- -2 * y.mu/(b$Var * b$dlink.mu)
    D.beta <- t(b$X1) %*% c
    D.rho <- rep(0, nsp)
    D.rho <- t(D.beta) %*% b$dbeta.rho
    d2link.dlink <- b$d2link.mu/b$dlink.mu
    a1 <- as.numeric(y.mu * d2link.dlink)
    a2 <- as.numeric(b$w^2 * (b$dvar.mu * b$dlink.mu + 2 *
                                b$Var * b$d2link.mu))
    eta.rho <- matrix(0, n, nsp)
    N_rho <- matrix(0, q, nsp)
    w.rho <- matrix(0, n, nsp)
    alpha.rho <- matrix(0, n, nsp)
    T_rho <- matrix(0, n, nsp)
    dvar.var <- b$dvar.mu/b$Var
    alpha1 <- as.numeric(-(dvar.var + d2link.dlink)/b$dlink.mu -
                           y.mu * (dvar.var^2 + d2link.dlink^2 - b$d2var.mu/b$Var -
                                     b$d3link.mu/b$dlink.mu)/b$dlink.mu)
    eta.rho <- b$X1 %*% b$dbeta.rho
    N_rho[b$iv, ] <- b$dbeta.rho[b$iv, ]
    alpha.rho <- alpha1 * eta.rho
    w.rho <- -a2 * b$alpha * eta.rho + b$w1 * alpha.rho
    T_rho <- w.rho/b$abs.w
    z2 <- b$dlink.mu * y.mu
    w1.rho <- matrix(0, n, nsp)
    T1_rho <- matrix(0, n, nsp)
    Q_rho <- matrix(0, q, nsp)
    w1.rho <- -a2 * eta.rho
    T1_rho <- w1.rho/b$w1
    term <- T1_rho * z2 + a1 * eta.rho - eta.rho
    Q_rho <- N_rho * drop(b$C2diag * crossprod(b$X, b$w1 *
                                                 z2)) + b$C1diag * crossprod(b$X, b$w1 * term)
    KtIL <- t((b$L * b$I.plus) * b$K)
    KtILK <- KtIL %*% b$K
    KKtILK <- b$K %*% KtILK
    trA.rho <- rep(0, nsp)
    for (j in 1:nsp) {
      trA.rho[j] <- -2 * sum(KtILK * (b$KtIQ1R %*% (N_rho[,
                                                          j] * b$P))) - sum((T_rho[, j] * KKtILK) * b$K) -
        sp[j] * sum((t(b$P) %*% b$S[[j]] %*% b$P) *
                      t(KtILK)) + sum((t(b$P) %*% (c(Q_rho[, j]) *
                                                     b$P)) * t(KtILK)) + 2 * sum(b$KtILQ1R * t(N_rho[,
                                                                                                     j] * b$P)) + sum(KtIL * t(T1_rho[, j] * b$K))
    }
    list(trA.rho = trA.rho, D.rho = D.rho)
  }
  nsp <- length(G$S)
  spind <- 1:nsp
  lsp[spind] <- lsp[spind] + 2.5
  mult <- 1
  fit <- scam::scam.fit(G = G, sp = exp(lsp), gamma = gamma, env = env,
                  control = control)
  q <- ncol(G$X)
  n <- nrow(G$X)
  score.hist <- rep(0, 200)
  D.rho <- tau.rho <- rep(0, nsp)
  for (iter in 1:200) {
    der <- deriv.dev.edf(G = G, fit = fit, sp = exp(lsp))
    D.rho <- der$D.rho
    tau.rho <- der$trA.rho
    a <- pmax(0, -2 * fit$deviance * tau.rho/(n - fit$trA))
    r <- a/pmax(0, D.rho)
    r[a == 0 | D.rho == 0] <- 1
    r[!is.finite(r)] <- 1e+06
    lsp1 <- lsp
    lsp1 <- pmin(lsp + log(r) * mult, control$efs.lspmax)
    max.step <- max(abs(lsp1 - lsp))
    old.gcv <- fit$gcv
    fit <-  scam::scam.fit(G = G, sp = exp(lsp1), gamma = gamma,
                    env = env, control = control)
    if (fit$gcv <= old.gcv) {
      if (max.step < 0.05) {
        lsp2 <- lsp
        lsp2 <- pmin(lsp + log(r) * mult * 2, control$efs.lspmax)
        fit2 <- scam::scam.fit(G = G, sp = exp(lsp2), gamma = gamma,
                         env = env, control = control)
        if (fit2$gcv < fit$gcv) {
          fit <- fit2
          lsp <- lsp2
          mult <- mult * 2
        }
        else {
          lsp <- lsp1
        }
      }
      else lsp <- lsp1
    }
    else {
      gcv.thresh <- 10 * (0.1 + abs(old.gcv)) * .Machine$double.eps^0.5
      ii <- 1
      maxHalf.fit <- 15
      while (is.na(fit$gcv) || (fit$gcv - old.gcv) > gcv.thresh) {
        if (ii > maxHalf.fit)
          break
        ii <- ii + 1
        mult <- mult/2
        lsp1 <- lsp
        lsp1 <- pmin(lsp + log(r) * mult, control$efs.lspmax)
        fit <-  scam::scam.fit(G = G, sp = exp(lsp1), gamma = gamma,
                        env = env, control = control)
      }
      lsp <- lsp1
      if (mult < 1)
        mult <- 1
    }
    score.hist[iter] <- fit$gcv
    if (iter > 3 && max.step < 0.05 && max(abs(diff(score.hist[(iter -
                                                                3):iter]))) < control$efs.tol)
      break
    if (iter == 1)
      old.dev <- fit$deviance
    else {
      if (abs(old.dev - fit$deviance) < 1000 * control$devtol.fit *
          abs(fit$deviance))
        break
      old.dev <- fit$deviance
    }
  }
  fit$sp <- exp(lsp)
  fit$niter <- iter
  fit$outer.info <- list(iter = iter, score.hist = score.hist[1:iter])
  fit$outer.info$conv <- if (iter == 200)
    "iteration limit reached"
  else "full convergence"
  fit
}
gam.setup<-function (formula, pterms, data = stop("No data supplied to gam.setup"),
          knots = NULL, sp = NULL, min.sp = NULL, H = NULL, absorb.cons = TRUE,
          sparse.cons = 0, select = FALSE, idLinksBases = TRUE, scale.penalty = TRUE,
          paraPen = NULL, gamm.call = FALSE, drop.intercept = FALSE,
          diagonal.penalty = FALSE, apply.by = TRUE, list.call = FALSE,
          modCon = 0)
{
  if (inherits(formula, "split.gam.formula"))
    split <- formula
  else if (inherits(formula, "formula"))
    split <- mgcv::interpret.gam(formula)
  else stop("First argument is no sort of formula!")
  if (length(split$smooth.spec) == 0) {
    if (split$pfok == 0)
      stop("You've got no model....")
    m <- 0
  }
  else m <- length(split$smooth.spec)
  G <- list(m = m, min.sp = min.sp, H = H, pearson.extra = 0,
            dev.extra = 0, n.true = -1, pterms = pterms)
  if (is.null(attr(data, "terms")))
    mf <- stats::model.frame(split$pf, data, drop.unused.levels = FALSE)
  else mf <- data
  G$intercept <- attr(attr(mf, "terms"), "intercept") > 0
  if (list.call) {
    offi <- attr(pterms, "offset")
    if (!is.null(offi)) {
      G$offset <- mf[[names(attr(pterms, "dataClasses"))[offi]]]
    }
  }
  else G$offset <- stats::model.offset(mf)
  if (!is.null(G$offset))
    G$offset <- as.numeric(G$offset)
  if (drop.intercept)
    attr(pterms, "intercept") <- 1
  X <- stats::model.matrix(pterms, mf)
  if (drop.intercept) {
    xat <- attributes(X)
    ind <- xat$assign > 0
    X <- X[, ind, drop = FALSE]
    xat$assign <- xat$assign[ind]
    xat$dimnames[[2]] <- xat$dimnames[[2]][ind]
    xat$dim[2] <- xat$dim[2] - 1
    attributes(X) <- xat
    G$intercept <- FALSE
  }
  rownames(X) <- NULL
  G$nsdf <- ncol(X)
  G$contrasts <- attr(X, "contrasts")
  G$xlevels <- stats::.getXlevels(pterms, mf)
  G$assign <- attr(X, "assign")
  PP <- parametricPenalty(pterms, G$assign, paraPen, sp)
  if (!is.null(PP)) {
    ind <- 1:length(PP$sp)
    if (!is.null(sp))
      sp <- sp[-ind]
    if (!is.null(min.sp)) {
      PP$min.sp <- min.sp[ind]
      min.sp <- min.sp[-ind]
    }
  }
  G$smooth <- list()
  G$S <- list()
  if (gamm.call) {
    if (m > 0)
      for (i in 1:m) attr(split$smooth.spec[[i]], "gamm") <- TRUE
  }
  if (m > 0 && idLinksBases) {
    id.list <- list()
    for (i in 1:m) if (!is.null(split$smooth.spec[[i]]$id)) {
      id <- as.character(split$smooth.spec[[i]]$id)
      if (length(id.list) && id %in% names(id.list)) {
        ni <- length(id.list[[id]]$sm.i)
        id.list[[id]]$sm.i[ni + 1] <- i
        base.i <- id.list[[id]]$sm.i[1]
        split$smooth.spec[[i]] <- clone.smooth.spec(split$smooth.spec[[base.i]],
                                                    split$smooth.spec[[i]])
        temp.term <- split$smooth.spec[[i]]$term
        for (j in 1:length(temp.term)) id.list[[id]]$data[[j]] <- cbind(id.list[[id]]$data[[j]],
                                                                        mgcv::get.var(temp.term[j], data, vecMat = FALSE))
      }
      else {
        id.list[[id]] <- list(sm.i = i)
        id.list[[id]]$data <- list()
        term <- split$smooth.spec[[i]]$term
        for (j in 1:length(term)) id.list[[id]]$data[[j]] <- mgcv::get.var(term[j],
                                                                     data, vecMat = FALSE)
      }
    }
  }
  G$off <- array(0, 0)
  first.para <- G$nsdf + 1
  sm <- list()
  newm <- 0
  if (m > 0)
    for (i in 1:m) {
      id <- split$smooth.spec[[i]]$id
      if (is.null(id) || !idLinksBases) {
        sml <- mgcv::smoothCon(split$smooth.spec[[i]], data,
                         knots, absorb.cons, scale.penalty = scale.penalty,
                         null.space.penalty = select, sparse.cons = sparse.cons,
                         diagonal.penalty = diagonal.penalty, apply.by = apply.by,
                         modCon = modCon)
      }
      else {
        names(id.list[[id]]$data) <- split$smooth.spec[[i]]$term
        sml <- mgcv::smoothCon(split$smooth.spec[[i]], id.list[[id]]$data,
                         knots, absorb.cons, n = nrow(data), dataX = data,
                         scale.penalty = scale.penalty, null.space.penalty = select,
                         sparse.cons = sparse.cons, diagonal.penalty = diagonal.penalty,
                         apply.by = apply.by, modCon = modCon)
      }
      for (j in 1:length(sml)) {
        newm <- newm + 1
        sm[[newm]] <- sml[[j]]
      }
    }
  G$m <- m <- newm
  if (m > 0) {
    sm <- mgcv::gam.side(sm, X, tol = .Machine$double.eps^0.5)
    if (!apply.by)
      for (i in 1:length(sm)) {
        if (!is.null(sm[[i]]$X0)) {
          ind <- attr(sm[[i]], "del.index")
          sm[[i]]$X <- if (is.null(ind))
            sm[[i]]$X0
          else sm[[i]]$X0[, -ind, drop = FALSE]
        }
      }
  }
  idx <- list()
  L <- matrix(0, 0, 0)
  lsp.names <- sp.names <- rep("", 0)
  if (m > 0)
    for (i in 1:m) {
      id <- sm[[i]]$id
      length.S <- length(sm[[i]]$S)
      if (is.null(sm[[i]]$L))
        Li <- diag(length.S)
      else Li <- sm[[i]]$L
      if (length.S > 0) {
        if (length.S == 1)
          lspn <- sm[[i]]$label
        else {
          Sname <- names(sm[[i]]$S)
          lspn <- if (is.null(Sname))
            paste(sm[[i]]$label, 1:length.S, sep = "")
          else paste(sm[[i]]$label, Sname, sep = "")
        }
        spn <- lspn[1:ncol(Li)]
      }
      if (is.null(id) || is.null(idx[[id]])) {
        if (!is.null(id)) {
          idx[[id]]$c <- ncol(L) + 1
          idx[[id]]$nc <- ncol(Li)
        }
        L <- rbind(cbind(L, matrix(0, nrow(L), ncol(Li))),
                   cbind(matrix(0, nrow(Li), ncol(L)), Li))
        if (length.S > 0) {
          sp.names <- c(sp.names, spn)
          lsp.names <- c(lsp.names, lspn)
        }
      }
      else {
        L0 <- matrix(0, nrow(Li), ncol(L))
        if (ncol(Li) > idx[[id]]$nc) {
          stop("Later terms sharing an `id' can not have more smoothing parameters than the first such term")
        }
        L0[, idx[[id]]$c:(idx[[id]]$c + ncol(Li) - 1)] <- Li
        L <- rbind(L, L0)
        if (length.S > 0) {
          lsp.names <- c(lsp.names, lspn)
        }
      }
    }
  Xp <- NULL
  if (m > 0)
    for (i in 1:m) {
      n.para <- ncol(sm[[i]]$X)
      sm[[i]]$first.para <- first.para
      first.para <- first.para + n.para
      sm[[i]]$last.para <- first.para - 1
      Xoff <- attr(sm[[i]]$X, "offset")
      if (!is.null(Xoff)) {
        if (is.null(G$offset))
          G$offset <- Xoff
        else G$offset <- G$offset + Xoff
      }
      if (is.null(sm[[i]]$Xp)) {
        if (!is.null(Xp))
          Xp <- methods::cbind2(Xp, sm[[i]]$X)
      }
      else {
        if (is.null(Xp))
          Xp <- X
        Xp <- methods::cbind2(Xp, sm[[i]]$Xp)
        sm[[i]]$Xp <- NULL
      }
      X <- methods::cbind2(X, sm[[i]]$X)
      sm[[i]]$X <- NULL
      G$smooth[[i]] <- sm[[i]]
    }
  if (is.null(Xp)) {
    G$cmX <- colMeans(X)
  }
  else {
    G$cmX <- colMeans(Xp)
    qrx <- qr(Xp, LAPACK = TRUE)
    R <- qr.R(qrx)
    p <- ncol(R)
    rank <- mgcv::Rrank(R)
    QtX <- qr.qty(qrx, X)[1:rank, ]
    if (rank < p) {
      R <- R[1:rank, ]
      qrr <- qr(t(R), tol = 0)
      R <- qr.R(qrr)
      G$P <- forwardsolve(t(R), QtX)
    }
    else {
      G$P <- backsolve(R, QtX)
    }
    if (rank < p) {
      G$P <- qr.qy(qrr, rbind(G$P, matrix(0, p - rank,
                                          p)))
    }
    G$P[qrx$pivot, ] <- G$P
  }
  G$X <- X
  rm(X)
  n.p <- ncol(G$X)
  if (!is.null(sp)) {
    ok <- TRUE
    if (length(sp) < ncol(L)) {
      warning("Supplied smoothing parameter vector is too short - ignored.")
      ok <- FALSE
    }
    if (sum(is.na(sp))) {
      warning("NA's in supplied smoothing parameter vector - ignoring.")
      ok <- FALSE
    }
  }
  else ok <- FALSE
  G$sp <- if (ok)
    sp[1:ncol(L)]
  else rep(-1, ncol(L))
  names(G$sp) <- sp.names
  k <- 1
  if (m > 0)
    for (i in 1:m) {
      id <- sm[[i]]$id
      if (is.null(sm[[i]]$L))
        Li <- diag(length(sm[[i]]$S))
      else Li <- sm[[i]]$L
      if (is.null(id)) {
        spi <- sm[[i]]$sp
        if (!is.null(spi)) {
          if (length(spi) != ncol(Li))
            stop("incorrect number of smoothing parameters supplied for a smooth term")
          G$sp[k:(k + ncol(Li) - 1)] <- spi
        }
        k <- k + ncol(Li)
      }
      else {
        spi <- sm[[i]]$sp
        if (is.null(idx[[id]]$sp.done)) {
          if (!is.null(spi)) {
            if (length(spi) != ncol(Li))
              stop("incorrect number of smoothing parameters supplied for a smooth term")
            G$sp[idx[[id]]$c:(idx[[id]]$c + idx[[id]]$nc -
                                1)] <- spi
          }
          idx[[id]]$sp.done <- TRUE
          k <- k + idx[[id]]$nc
        }
      }
    }
  k <- 1
  if (length(idx))
    for (i in 1:length(idx)) idx[[i]]$sp.done <- FALSE
  if (m > 0)
    for (i in 1:m) {
      id <- sm[[i]]$id
      if (!is.null(id)) {
        if (idx[[id]]$nc > 0) {
          G$smooth[[i]]$sp <- G$sp[idx[[id]]$c:(idx[[id]]$c +
                                                  idx[[id]]$nc - 1)]
        }
        if (!idx[[id]]$sp.done) {
          idx[[id]]$sp.done <- TRUE
          k <- k + idx[[id]]$nc
        }
      }
      else {
        if (is.null(sm[[i]]$L))
          nc <- length(sm[[i]]$S)
        else nc <- ncol(sm[[i]]$L)
        if (nc > 0)
          G$smooth[[i]]$sp <- G$sp[k:(k + nc - 1)]
        k <- k + nc
      }
    }
  if (!is.null(min.sp)) {
    if (length(min.sp) < nrow(L))
      stop("length of min.sp is wrong.")
    min.sp <- min.sp[1:nrow(L)]
    if (sum(is.na(min.sp)))
      stop("NA's in min.sp.")
    if (sum(min.sp < 0))
      stop("elements of min.sp must be non negative.")
  }
  k.sp <- 0
  G$rank <- array(0, 0)
  if (m > 0)
    for (i in 1:m) {
      sm <- G$smooth[[i]]
      if (length(sm$S) > 0)
        for (j in 1:length(sm$S)) {
          k.sp <- k.sp + 1
          G$off[k.sp] <- sm$first.para
          G$S[[k.sp]] <- sm$S[[j]]
          G$rank[k.sp] <- sm$rank[j]
          if (!is.null(min.sp)) {
            if (is.null(H))
              H <- matrix(0, n.p, n.p)
            H[sm$first.para:sm$last.para, sm$first.para:sm$last.para] <- H[sm$first.para:sm$last.para,
                                                                           sm$first.para:sm$last.para] + min.sp[k.sp] *
              sm$S[[j]]
          }
        }
    }
  if (!is.null(PP)) {
    L <- rbind(cbind(L, matrix(0, nrow(L), ncol(PP$L))),
               cbind(matrix(0, nrow(PP$L), ncol(L)), PP$L))
    G$off <- c(PP$off, G$off)
    G$S <- c(PP$S, G$S)
    G$rank <- c(PP$rank, G$rank)
    G$sp <- c(PP$sp, G$sp)
    lsp.names <- c(PP$full.sp.names, lsp.names)
    G$n.paraPen <- length(PP$off)
    if (!is.null(PP$min.sp)) {
      if (is.null(H))
        H <- matrix(0, n.p, n.p)
      for (i in 1:length(PP$S)) {
        ind <- PP$off[i]:(PP$off[i] + ncol(PP$S[[i]]) -
                            1)
        H[ind, ind] <- H[ind, ind] + PP$min.sp[i] *
          PP$S[[i]]
      }
    }
  }
  else G$n.paraPen <- 0
  fix.ind <- G$sp >= 0
  if (sum(fix.ind)) {
    lsp0 <- G$sp[fix.ind]
    ind <- lsp0 == 0
    ef0 <- indi <- (1:length(ind))[ind]
    if (length(indi) > 0)
      for (i in 1:length(indi)) {
        ii <- G$off[i]:(G$off[i] + ncol(G$S[[i]]) -
                          1)
        ef0[i] <- norm(G$X[, ii], type = "F")^2/norm(G$S[[i]],
                                                     type = "F") * .Machine$double.eps * 0.1
      }
    lsp0[!ind] <- log(lsp0[!ind])
    lsp0[ind] <- log(ef0)
    lsp0 <- as.numeric(L[, fix.ind, drop = FALSE] %*% lsp0)
    L <- L[, !fix.ind, drop = FALSE]
    G$sp <- G$sp[!fix.ind]
  }
  else {
    lsp0 <- rep(0, nrow(L))
  }
  G$H <- H
  if (ncol(L) == nrow(L) && !sum(L != diag(ncol(L))))
    L <- NULL
  G$L <- L
  G$lsp0 <- lsp0
  names(G$lsp0) <- lsp.names
  if (absorb.cons == FALSE) {
    G$C <- matrix(0, 0, n.p)
    if (m > 0) {
      for (i in 1:m) {
        if (is.null(G$smooth[[i]]$C))
          n.con <- 0
        else n.con <- nrow(G$smooth[[i]]$C)
        C <- matrix(0, n.con, n.p)
        C[, G$smooth[[i]]$first.para:G$smooth[[i]]$last.para] <- G$smooth[[i]]$C
        G$C <- rbind(G$C, C)
        G$smooth[[i]]$C <- NULL
      }
      rm(C)
    }
  }
  G$y <- data[[split$response]]
  G$n <- nrow(data)
  if (is.null(data$"(weights)"))
    G$w <- rep(1, G$n)
  else G$w <- data$"(weights)"
  if (G$nsdf > 0)
    term.names <- colnames(G$X)[1:G$nsdf]
  else term.names <- array("", 0)
  n.smooth <- length(G$smooth)
  if (n.smooth)
    for (i in 1:n.smooth) {
      k <- 1
      jj <- G$smooth[[i]]$first.para:G$smooth[[i]]$last.para
      if (G$smooth[[i]]$df > 0)
        for (j in jj) {
          term.names[j] <- paste(G$smooth[[i]]$label,
                                 ".", as.character(k), sep = "")
          k <- k + 1
        }
      if (!is.null(G$smooth[[i]]$g.index)) {
        if (is.null(G$g.index))
          G$g.index <- rep(FALSE, n.p)
        G$g.index[jj] <- G$smooth[[i]]$g.index
      }
    }
  G$term.names <- term.names
  G$pP <- PP
  G
}
gcv.ubre.derivative<-function (rho, G, gamma, env, control)
{
  scam::gcv.ubre_grad(rho, G, gamma, env, control = control)$gcv.ubre.rho
}
initial.sp.scam<-function (G, Q, q.f, n.terms, family, intercept, offset, env = env,
          weights, control = control)
{
  control$devtol.fit <- 1e-04
  control$steptol.fit <- 1e-04
  b <- scam::scam.fit(G = G, sp = rep(0.05, length(G$off)), env = env,
                control = control)
  H <- crossprod(b$wX1) - b$E
  n.p <- length(Q$S)
  def.sp <- array(0, n.p)
  j <- 1
  if (n.terms > 0)
    for (i in 1:n.terms) {
      for (kk in 1:length(G$smooth[[i]]$S)) {
        start <- G$off[j]
        finish <- start + ncol(G$smooth[[i]]$S[[kk]]) -
          1
        Hi.norm <- sum(H[start:finish, start:finish] *
                         H[start:finish, start:finish])
        Si.norm <- sum(G$smooth[[i]]$S[[kk]] * G$smooth[[i]]$S[[kk]])
        def.sp[j] <- (Hi.norm/Si.norm)^0.5
        j <- j + 1
      }
    }
  env <- new.env()
  assign("start", rep(0, 0), envir = env)
  assign("dbeta.start", rep(0, 0), envir = env)
  assign("sp.last", rep(0, 0), envir = env)
  def.sp
}
penalty_pident<-function (object)
{
  n.terms <- length(object$smooth)
  q <- ncol(object$X)
  cons.terms <- rep(0, n.terms)
  if (n.terms > 0)
    for (i in 1:n.terms) {
      if (!is.null(object$smooth[[i]]$p.ident))
        cons.terms[i] <- 1
    }
  p.ident <- rep(FALSE, q)
  off.terms <- rep(0, n.terms)
  off <- object$off
  if (n.terms == length(off))
    off.terms <- off
  else {
    off.terms[1] <- off[1]
    k <- 1
    l <- 1
    while (l < length(off)) {
      if (off[l] != off[l + 1]) {
        off.terms[k + 1] <- off[l + 1]
        k <- k + 1
        l <- l + 1
      }
      else l <- l + 1
    }
  }
  if (n.terms > 0)
    for (i in 1:n.terms) {
      if (cons.terms[i] == 1)
        p.ident[off.terms[i]:(off.terms[i] + ncol(object$smooth[[i]]$S[[1]]) -
                                1)] <- object$smooth[[i]]$p.ident
    }
  S <- list()
  j <- 1
  if (n.terms > 0)
    for (i in 1:n.terms) {
      for (kk in 1:length(object$smooth[[i]]$S)) {
        S[[j]] <- matrix(0, q, q)
        S[[j]][off.terms[i]:(off.terms[i] + ncol(object$smooth[[i]]$S[[kk]]) -
                               1), off.terms[i]:(off.terms[i] + ncol(object$smooth[[i]]$S[[kk]]) -
                                                   1)] <- object$smooth[[i]]$S[[kk]]
        j <- j + 1
      }
    }
  object$S <- S
  object$p.ident <- p.ident
  object
}
parametricPenalty<-function (pterms, assign, paraPen, sp0)
{
  S <- list()
  off <- rep(0, 0)
  rank <- rep(0, 0)
  sp <- rep(0, 0)
  full.sp.names <- rep("", 0)
  L <- matrix(0, 0, 0)
  k <- 0
  tind <- unique(assign)
  n.t <- length(tind)
  if (n.t > 0)
    for (j in 1:n.t) if (tind[j] > 0) {
      term.label <- attr(pterms[tind[j]], "term.label")
      P <- paraPen[[term.label]]
      if (!is.null(P)) {
        ind <- (1:length(assign))[assign == tind[j]]
        Li <- P$L
        P$L <- NULL
        spi <- P$sp
        P$sp <- NULL
        ranki <- P$rank
        P$rank <- NULL
        np <- length(P)
        if (!is.null(ranki) && length(ranki) != np)
          stop("`rank' has wrong length in `paraPen'")
        if (np)
          for (i in 1:np) {
            k <- k + 1
            S[[k]] <- P[[i]]
            off[k] <- min(ind)
            if (ncol(P[[i]]) != nrow(P[[i]]) || nrow(P[[i]]) !=
                length(ind))
              stop(" a parametric penalty has wrong dimension")
            if (is.null(ranki)) {
              ev <- eigen(S[[k]], symmetric = TRUE,
                          only.values = TRUE)$values
              rank[k] <- sum(ev > max(ev) * .Machine$double.eps *
                               10)
            }
            else rank[k] <- ranki[i]
          }
        if (np) {
          if (is.null(Li))
            Li <- diag(np)
          if (nrow(Li) != np)
            stop("L has wrong dimension in `paraPen'")
          L <- rbind(cbind(L, matrix(0, nrow(L), ncol(Li))),
                     cbind(matrix(0, nrow(Li), ncol(L)), Li))
          ind <- (length(sp) + 1):(length(sp) + ncol(Li))
          ind2 <- (length(sp) + 1):(length(sp) + nrow(Li))
          if (is.null(spi)) {
            sp[ind] <- -1
          }
          else {
            if (length(spi) != ncol(Li))
              stop("`sp' dimension wrong in `paraPen'")
            sp[ind] <- spi
          }
          if (length(ind) > 1)
            names(sp)[ind] <- paste(term.label, ind -
                                      ind[1] + 1, sep = "")
          else names(sp)[ind] <- term.label
          if (length(ind2) > 1)
            full.sp.names[ind2] <- paste(term.label,
                                         ind2 - ind2[1] + 1, sep = "")
          else full.sp.names[ind2] <- term.label
        }
      }
    }
  if (k == 0)
    return(NULL)
  if (!is.null(sp0)) {
    if (length(sp0) < length(sp))
      stop("`sp' too short")
    sp0 <- sp0[1:length(sp)]
    sp[sp < 0] <- sp0[sp < 0]
  }
  list(S = S, off = off, sp = sp, L = L, rank = rank, full.sp.names = full.sp.names)
}

scam.fit.post<-function (G, object)
{
  y <- G$y
  X <- G$X
  sig2 <- G$sig2
  offset <- G$offset
  intercept <- G$intercept
  weights <- G$weights
  scale.known <- G$scale.known
  not.exp <- G$not.exp
  n <- nobs <- NROW(y)
  q <- ncol(X)
  # if (G$AR1.rho != 0) {
  #   ld <- 1/sqrt(1 - G$AR1.rho^2)
  #   sd <- -G$AR1.rho * ld
  #   row <- c(1, rep(1:nobs, rep(2, nobs))[-c(1, 2 * nobs)])
  #   weight.r <- c(1, rep(c(sd, ld), nobs - 1))
  #   end <- c(1, 1:(nobs - 1) * 2 + 1)
  #   if (!is.null(G$AR.start)) {
  #     ii <- which(G$AR.start == TRUE)
  #     if (length(ii) > 0) {
  #       if (ii[1] == 1)
  #         ii <- ii[-1]
  #       weight.r[ii * 2 - 2] <- 0
  #       weight.r[ii * 2 - 1] <- 1
  #     }
  #   }
  #   X <- rwMatrix(end, row, weight.r, X)
  #   y <- rwMatrix(end, row, weight.r, y)
  # }
  linkinv <- object$family$linkinv
  dev.resids <- object$family$dev.resids
  dg <- mgcv::fix.family.link(object$family)
  dv <- mgcv::fix.family.var(object$family)
  eta <- as.numeric(X %*% object$beta.t + offset)
  mu <- linkinv(eta)
  dev <- sum(dev.resids(y, mu, weights))
  wtdmu <- if (intercept)
    sum(weights * G$y)/sum(weights)
  else linkinv(offset)
  null.dev <- sum(dev.resids(G$y, wtdmu, weights))
  n.ok <- nobs - sum(weights == 0)
  nulldf <- n.ok - as.integer(intercept)
  Cdiag <- rep(1, q)
  C1diag <- rep(0, q)
  iv <- object$iv
  if (!not.exp) {
    Cdiag[iv] <- C1diag[iv] <- object$beta.t[iv]
  }
  else {
    Cdiag[iv] <- DnotExp(object$beta[iv])
    C1diag[iv] <- D2notExp(object$beta[iv])
  }
  X1 <- t(Cdiag * t(X))
  g.deriv <- 1/object$family$mu.eta(eta)
  w1 <- weights/(object$family$variance(mu) * g.deriv^2)
  alpha <- 1 + (y - mu) * (dv$dvar(mu)/object$family$variance(mu) +
                             dg$d2link(mu)/g.deriv)
  w <- w1 * alpha
  E <- matrix(0, q, q)
  diag(E) <- drop((C1diag * t(X)) %*% (w1 * g.deriv * (y -
                                                         mu)))
  abs.w <- abs(w)
  I.minus <- rep(0, nobs)
  I.minus[w < 0] <- 1
  wX1 <- sqrt(abs.w)[1:nobs] * X1
  wX11 <- rbind(wX1, object$rS)
  illcond <- FALSE
  Q <- qr(wX11, LAPACK = TRUE)
  R <- qr.R(Q)
  rp <- 1:ncol(R)
  rp[Q$pivot] <- rp
  R.out <- R[, rp]
  rank <- mgcv::Rrank(R)
  if (rank == ncol(R)) {
    R.inv <- backsolve(R, diag(ncol(R)))[rp, ]
    tR.inv <- t(R.inv)
  }
  else {
    R <- R[, rp]
    svd.r <- svd(R)
    d.inv <- rep(0, q)
    good <- svd.r$d >= max(svd.r$d) * .Machine$double.eps^0.5
    d.inv[good] <- 1/svd.r$d[good]
    if (sum(!good) > 0)
      illcond <- TRUE
    R <- svd.r$d * t(svd.r$v)
    Q <- qr.qy(Q, rbind(svd.r$u, matrix(0, nobs, q)))
    tR.inv <- d.inv * t(svd.r$v)
    R.inv <- t(tR.inv)
  }
  QtQRER <- tR.inv %*% (diag(E) * R.inv)
  if (sum(I.minus) > 0) {
    if (is.qr(Q)) {
      QtQRER <- QtQRER + 2 * crossprod(I.minus * qr.Q(Q)[1:nobs,
      ])
    }
    else {
      QtQRER <- QtQRER + 2 * crossprod(I.minus * Q[1:nobs,
      ])
    }
  }
  ei <- eigen(QtQRER, symmetric = TRUE)
  d <- ei$values
  ok1 <- sum(d > 1) > 0
  if (ok1) {
    wX1 <- sqrt(w1)[1:nobs] * X1
    wX11 <- rbind(wX1, object$rS)
    Q <- qr(wX11, LAPACK = TRUE)
    R <- qr.R(Q)
    rp <- 1:ncol(R)
    rp[Q$pivot] <- rp
    R.out <- R[, rp]
    rank <- mgcv::Rrank(R)
    if (rank == ncol(R)) {
      P <- backsolve(R, diag(ncol(R)))[rp, ]
      K <- qr.Q(Q)[1:nobs, ]
    }
    else {
      R <- R[, rp]
      s1 <- svd(R)
      d.inv1 <- rep(0, q)
      good1 <- s1$d >= max(s1$d) * .Machine$double.eps^0.5
      d.inv1[good1] <- 1/s1$d[good1]
      P <- t(d.inv1 * t(s1$v))
      K <- qr.qy(Q, rbind(s1$u, matrix(0, nobs, q)))[1:nobs,
      ]
    }
  }
  else {
    Id.inv.r <- 1/(1 - d)^0.5
    ii <- (1 - d) < .Machine$double.eps
    Id.inv.r[ii] <- 0
    eidrop <- t(Id.inv.r * t(ei$vectors))
    P <- R.inv %*% eidrop
    if (is.qr(Q)) {
      K <- qr.qy(Q, rbind(eidrop, matrix(0, nobs, q)))[1:nobs,
      ]
    }
    else {
      K <- Q[1:nobs, ] %*% eidrop
    }
  }
  I.plus <- rep(1, nobs)
  I.plus[w < 0] <- -1
  L <- c(1/alpha)
  KtILQ1R <- crossprod(L * I.plus * K, wX1)
  F <- P %*% (KtILQ1R)
  edf <- diag(F)
  edf1 <- 2 * edf - rowSums(t(F) * F)
  trA <- sum(edf)
  if (!scale.known)
    sig2 <- dev/(nobs - trA)
  Vb <- tcrossprod(P) * sig2
  Ve <- crossprod(K %*% t(P)) * sig2
  df.p <- rep(1, q)
  df.p[object$iv] <- object$beta.t[object$iv]
  Vb.t <- t(df.p * t(df.p * Vb))
  Ve.t <- t(df.p * t(df.p * Ve))
  eta <- as.numeric(G$X %*% object$beta.t + offset)
  mu <- linkinv(eta)
  residuals <- rep.int(NA, nobs)
  g.deriv <- 1/object$family$mu.eta(eta)
  residuals <- (G$y - mu) * g.deriv
  aic.model <- object$family$aic(y, n, mu, weights, dev) +
    2 * sum(edf)
  if (G$AR1.rho != 0) {
    df <- 1
    aic.model <- aic.model - 2 * (n - df) * log(1/sqrt(1 -
                                                         G$AR1.rho^2))
  }
  list(null.dev = null.dev, df.null = nulldf, Vb = Vb, Vb.t = Vb.t,
       Ve = Ve, Ve.t = Ve.t, rank = rank, sig2 = sig2, edf = edf,
       edf1 = edf1, trA = trA, deviance = dev, residuals = residuals,
       aic = aic.model, mu = mu, eta = eta)
}
variable.summary<-function (pf, dl, n)
{
  v.n <- length(dl)
  v.name <- v.name1 <- names(dl)
  if (v.n) {
    k <- 0
    for (i in 1:v.n) if (length(dl[[i]]) >= n) {
      k <- k + 1
      v.name[k] <- v.name1[i]
    }
    if (k > 0)
      v.name <- v.name[1:k]
    else v.name <- rep("", k)
  }
  p.name <- all.vars(pf[-2])
  vs <- list()
  v.n <- length(v.name)
  if (v.n > 0)
    for (i in 1:v.n) {
      if (v.name[i] %in% p.name)
        para <- TRUE
      else para <- FALSE
      if (para && is.matrix(dl[[v.name[i]]]) && ncol(dl[[v.name[i]]]) >
          1) {
        x <- matrix(apply(dl[[v.name[i]]], 2, stats::quantile,
                          probs = 0.5, type = 3, na.rm = TRUE), 1, ncol(dl[[v.name[i]]]))
      }
      else {
        x <- dl[[v.name[i]]]
        if (is.character(x))
          x <- as.factor(x)
        if (is.factor(x)) {
          x <- x[!is.na(x)]
          lx <- levels(x)
          freq <- tabulate(x)
          ii <- min((1:length(lx))[freq == max(freq)])
          x <- factor(lx[ii], levels = lx)
        }
        else {
          x <- as.numeric(x)
          x <- c(min(x, na.rm = TRUE), as.numeric(stats::quantile(x,
                                                           probs = 0.5, type = 3, na.rm = TRUE)), max(x,
                                                                                                      na.rm = TRUE))
        }
      }
      vs[[v.name[i]]] <- x
    }
  vs
}
clone.smooth.spec<-function (specb, spec)
{
  if (specb$dim != spec$dim)
    stop("`id' linked smooths must have same number of arguments")
  if (inherits(specb, c("tensor.smooth.spec", "t2.smooth.spec"))) {
    specb$term <- spec$term
    specb$label <- spec$label
    specb$by <- spec$by
    k <- 1
    for (i in 1:length(specb$margin)) {
      if (is.null(spec$margin)) {
        for (j in 1:length(specb$margin[[i]]$term)) {
          specb$margin[[i]]$term[j] <- spec$term[k]
          k <- k + 1
        }
        specb$margin[[i]]$label <- ""
      }
      else {
        specb$margin[[i]]$term <- spec$margin[[i]]$term
        specb$margin[[i]]$label <- spec$margin[[i]]$label
        specb$margin[[i]]$xt <- spec$margin[[i]]$xt
      }
    }
  }
  else {
    specb$term <- spec$term
    specb$label <- spec$label
    specb$by <- spec$by
    specb$xt <- spec$xt
  }
  specb
}
