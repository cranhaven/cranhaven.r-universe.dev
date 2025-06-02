comp <-
  function(est,
           covmtrx,
           constr,
           rhs,
           nec,
           logLik,
           restrictedest,
           wt_bar) {
    if(nec > 0L) {
      constr.ceq <- constr[1:nec, , drop = FALSE]
      rhs.ceq <- rhs[1:nec]
      constr.ciq <- constr[-c(1:nec), , drop = FALSE]
      rhs.ciq <- rhs[-c(1:nec)]
    } else {
      constr.ceq <- matrix(0, nrow = 0, ncol = ncol(constr))
      rhs.ceq <- rep(0, 0)
      constr.ciq <- constr[, , drop = FALSE]
      rhs.ciq <- rhs
    }

    # check if any equality constraint is violated
    check.ceq <- !(all(constr.ceq %*% c(est) - rhs.ceq == 0))
    if (nrow(constr) > nec) {
      # check if any inequality constraint is violated
      check.ciq <- !(all(constr.ciq %*% c(est) - rhs.ciq >= 0))
    } else {
      check.ciq <- FALSE
    }

    ## compute log-likelihood for complement
    # check if any constraint is violated
    if (check.ciq || check.ceq) {
      llc <-
        ormle(
          est, # was x
          covmtrx,
          constr = matrix(c(rep(0, length(
            est #was x
          ))), nrow = 1),
          nec = 0,
          rhs = 0
        )$logLik
      betasc <- est #was x
      # if any constraints is violated LL_c = LL_u
    } else if (nrow(constr) > nec && !(all(c(constr) == 0L))) {
      # number of rows
      nr <- 1:nrow(constr)
      # remove rows corresponding to equality constraints
      if (nec > 0L) {
        nr <- nr[-c(0:nec)]
      }
      # treat each row of constr as an equality constraint
      gorica_list <- lapply(nr, function(l) {
        idx <- c(nr[l], nr[-l])
        constrx <- constr[idx, , drop = FALSE]
        ormle(est,
                       covmtrx,
                       constr = constrx,
                       rhs = rhs[idx],
                       nec = 1)
        #ll[[l]] <- out$logLik
        #betas[[l]] <- out$restrictedest
      })

      # take the highest log-likelihood value as a substitute for
      # the complement
      ll.unlist <- sapply(gorica_list, `[[`, "logLik")
      ll.idx <- which(ll.unlist == max(ll.unlist))
      llc <- max(ll.unlist)
      betasc <- gorica_list[[ll.idx]]$restrictedest
    } else if (nrow(constr) == nec) {
      # redundant, this will be catched by the first statement.
      # in case of equality constraints only, the complement is
      # equal to the unconstrained log-likelihood
      llc <-
        ormle(
          est, #was x
          covmtrx,
          constr = matrix(c(rep(0, length(
            est #was x
          ))), nrow = 1),
          nec = 0,
          rhs = 0
        )$logLik
      betasc <- est # was x
    } else if (all(c(constr) == 0L)) {
      # unconstrained setting
      stop("No complement exists for the unrestricted hypothesis.")
    } else {
      stop("You might have found a bug, please contact the package maintainer.")
    }

    # compute free parameters f in the complement
    p <- length(est)
    # nrow q1
    lq1 <- nrow(constr.ciq)
    # nrow q2
    lq2 <- nrow(constr.ceq)
    # free parameters. Note that constr includes q1 and q2 constraints
    f <- p - nrow(constr) # p - q1 - q2
    idx <- length(wt_bar)
    # compute penalty term value PTc
    # Gorica heeft geen atrribute "method". Volgens mij zijn pmvnorm de juiste weights?
    PTc <- as.numeric(p - wt_bar[idx] * lq1)


    # complement
    goric.Hc <- -2 * (llc - PTc)
    c(loglik = llc, penalty = PTc, goric = goric.Hc)

  }
