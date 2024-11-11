GroupLambda <- function(X, Y, groups, lambda, penalty = "lasso", power = 1,
                             gamma = 1.5, solver = c("ecos","mosek","gurobi"),
                             model.size = NULL,
                             options = list(solver_opts = NULL,
                                            init = NULL,
                                            tol = 1e-7,
                                            iter = 100), 
                             display.progress=FALSE, ...) 
{ 
  register_solver(solver) #register lpSolve
  
  nlambda <- length(lambda)
  stopifnot(nlambda >= 1)
  return_val <- vector("list", nlambda)
  if(is.null(solver)) solver <- "ecos"
  
  if(is.null(options$init)) options$init <- rep(0, ncol(X))
  
  problem <- switch(as.character(power), "Inf" = lp_prob_winf(X = X, Y = Y, lambda = lambda[1], groups = groups),
                    "1" = lp_prob_w1(X = X, Y = Y, lambda = lambda[1], groups = groups))
  convert <- lp_prob_to_model(problem, solver, options$init, options$solver_opts)
  model <- convert$prob
  opts <- convert$opts
  if(penalty == "lasso") options$iter <- 1
  
  deriv_func <- switch(penalty ,"mcp"= rqPen_mcp_deriv,
                       "scad" = rqPen_scad_deriv,
                       "lasso" = function(x, lambda, a){lambda},
                       "none" = function(x, lambda, a){0})
  
  thresh_fun <- switch( penalty,
                        "lasso"=soft_threshold,
                        "mcp"= mcp_threshold,
                        "scad" = scad_threshold,
                        "none" = function(x, lambda, gamma){x})
  
  if(is.null(model.size) | length(model.size) == 0) {
    model.size <- ncol(X)
  }
  
  if(all(lambda == 0)) {
    return_val <- list( 
      lp_solve(model, problem$beta_idx, rep(0, length(problem$lambda_idx)), gamma, opts, solver, thresh_fun, problem$group_idx)
    )
  } else {
    if(display.progress) pb <- utils::txtProgressBar(min = 0, max = length(lambda), style = 3)
    
    for (pos in seq_along(lambda) ) {
      if(solver == "gurobi") {
        model$obj[problem$lambda_idx] <- lambda[[pos]]
      } else if (solver == "mosek") {
        model$c[problem$lambda_idx] <- lambda[[pos]]
      } else if (solver == "cone") {
        model$objective$L[problem$lambda_idx] <- lambda[[pos]]
      }
      # return_val[[pos]] <- rqPen::rq.group.fit(x = x, y = y, groups = groups, 
      #                                          tau = tau, lambda = lam, intercept = intercept, 
      #                                          penalty = penalty, alg = alg, penGroups = penGroups, 
      #                                          ...)
      return_val[[pos]] <- lp_norm(X = X, Y = Y, power = power,
                                     problem = problem, model = model, deriv_func = deriv_func, thresholder = thresh_fun,
                                     lambda = lambda[[pos]], groups=groups, solver= solver, 
                                     gamma = gamma, opts = opts, 
                                     init = options$init, iter = options$iter, tol = options$tol)
      if(sum(return_val[[pos]] != 0) > model.size) {
        if(pos != length(lambda)) return_val[(pos):length(return_val)] <- NULL
        break
      }
      options$init <- return_val[[pos]]
      if(display.progress) utils::setTxtProgressBar(pb, pos)
      
    }
  }
  return( do.call("cbind", return_val ))
}

lp_norm <- function(X, Y, power = 1, problem = NULL, model = NULL, deriv_func, thresholder, lambda, groups, solver, gamma = 1.5, opts = NULL, init = NULL, iter = 100, tol = 1e-7) {
  
  d <- ncol(X)
  if(is.null(init)) init <- rep(0,d)
  if(is.null(iter)) iter <- 100
  if(is.null(tol)) tol <- 1e-7
  
  if( is.null(problem)  | is.null(model)) {
    problem <- switch(as.character(power), "Inf" = lp_prob_winf(X = X, Y = Y, lambda = lambda, groups = groups),
                                 "1" = lp_prob_w1(X = X, Y = Y, lambda = lambda, groups = groups))
    convert <- lp_prob_to_model(problem, solver, init, opts)
    model <- convert$prob
    opts <- convert$opts
  }
  
  lambda_update <- list(rep(lambda, length(problem$lambda_idx)))
  
  
  beta <- beta_old <- list(rep(0.0,d))
  group_deriv <- list()
  
  for(i in 1:iter) {
    beta[[1L]] <- lp_solve(model, problem$beta_idx, lambda_update[[1L]], gamma, opts, solver, thresholder, problem$group_idx)
    
    if(!not.converged(beta[[1L]], beta_old[[1L]], tol)) {
      break
    }
    beta_old[[1L]] <- beta[[1L]]
    # group_deriv[[1]] <- rqPen::group_derivs(deriv_func = deriv_func, groups = groups,
    #                                         coefs = beta[[1]],
    #                                         lambda = lambda_update[[1]], a = gamma)
    group_deriv[[1L]] <- group_deriv(deriv_func = deriv_func, groups = problem$group_idx,
                                    coefs = beta[[1L]],
                                    lambda = lambda_update[[1L]], a = gamma)
    lambda_update[[1L]] <- group_deriv[[1L]]
    
    if(solver == "mosek") {
      model$c[problem$lambda_idx] <- lambda_update[[1L]]
    } else if (solver == "gurobi") {
      model$obj[problem$lambda_idx] <- lambda_update[[1L]]
    } else if (solver == "cone") {
      model$objective$L[problem$lambda_idx] <- lambda_update[[1L]]
      # browser()
    }
  }
  if(i == iter & iter > 1) warning("Maximum number of iterations hit!")
  return(beta[[1]])
}

lp_solve <- function(problem, beta.idx, lambda, gamma, opts, solver, thresholder, groups) {
  
  register_solver(solver) # registers ecos solver if needed
  # if (solver == "cone") {
  #   ROI::ROI_require_solver("ecos")
  # }
  
  res <- switch(solver,
                "cone" = ROI::ROI_solve(problem, "ecos", opts),
                "mosek" = Rmosek::mosek(problem, opts)#,
                # "gurobi" = gurobi::gurobi(problem,opts)
  )
  
  if(solver == "mosek") {
    param <- res$sol$itr$xx
  } else if (solver == "gurobi") {
    param <- res$x
  } else if (solver == "cone") {
    param <- res$solution
  }
  
  sol <- group_threshold(param[beta.idx], thresholder, lambda, gamma, groups)
  
  return(sol)
  
}

lp_prob_to_model <- function(problem, solver, init, opts) {
  num_param <- length(c(problem$C))
  init_full <- rep(0, num_param)
  init_full[problem$beta_idx] <- init[1:length(problem$beta_idx)]
  if(is.null(init)) init <- rep(0, num_param)
  if (solver == "mosek") {
    ngroups <- length(problem$group_idx)
    if(ngroups > 0) {
      cone <- matrix(list(), nrow = 2, ncol = ngroups)
      rownames(cone) <- c("type","sub")
      for(i in 1:ngroups) {
        cone[,i] <- list("QUAD", c(problem$lambda_idx[i], problem$beta_idx[problem$group_idx[[i]]]))
      }
    } else {
      cone <- NULL
    }
    
    prob <-  list(sense = problem$sense,
                  c = problem$C,
                  A = problem$Const,
                  bc = rbind(problem$Const_lower, problem$Const_upper),
                  bx = rbind(problem$LB, problem$UB),
                  sol = list(itr = list(xx = init_full)),
                  cones = cone
    )
    if(is.null(opts)) opts <- list(verbose = 0)
  } else if (solver == "gurobi") {
    prob <-  list()
    prob$Q <- problem$Q
    prob$modelsense <- 'min'
    prob$obj <- problem$C
    equiv.indices <- which(problem$Const_upper == problem$Const_lower)
    prob$A <- rbind(problem$Const, problem$Const[-equiv.indices,])
    prob$sense <- c(rep("<=", nrow(problem$Const)), rep(">=", nrow(problem$Const) - length(equiv.indices)))
    prob$sense[equiv.indices] <- "="
    # prob$sense <- rep(NA, length(qp$LC$dir))
    # prob$sense[qp$LC$dir=="E"] <- '='
    # prob$sense[qp$LC$dir=="L"] <- '<='
    # prob$sense[qp$LC$dir=="G"] <- '>='
    prob$rhs <- c(problem$Const_upper, problem$Const_lower[-equiv.indices])
    prob$vtype <- rep("C", num_param)
    prob$lb <- problem$LB
    prob$ub <- problem$UB
    total_length <- length(problem$C)
    Quad_con <- lapply(seq_along(problem$group_idx), 
                         function(g) {
                           grp <- problem$group_idx[[g]]
                           beta_grp <- problem$beta_idx[grp]
                           n_grp <- length(grp)
                           return(Matrix::sparseMatrix(i = c(beta_grp, problem$lambda_idx[g]),
                                                       j = c(beta_grp, problem$lambda_idx[g]),
                                                       x = c(rep(1,n_grp),-1),
                                                       dims = c(total_length, total_length)))
                         }
                       )
    prob$quadcon <- lapply(Quad_con, function(xx) list(Qc=xx,
                                                                 rhs = 0,
                                                                 sense = "<="))
    prob$start <- init_full
    if(is.null(opts)) {
      opts <- list(OutputFlag = 0)
    }
    # opts$NonConvex <- 2
  } else if (solver == "cone") {
    # browser()
    nvars <- length(problem$C)
    equiv.indices <- which(problem$Const_upper == problem$Const_lower)
    A <- rbind(problem$Const, 
               problem$Const[-equiv.indices,])
    dir <- c(rep("<=", nrow(problem$Const)), rep(">=", nrow(problem$Const) - length(equiv.indices)))
    dir[equiv.indices] <- "=="
    # prob$sense <- rep(NA, length(qp$LC$dir))
    # prob$sense[qp$LC$dir=="E"] <- '='
    # prob$sense[qp$LC$dir=="L"] <- '<='
    # prob$sense[qp$LC$dir=="G"] <- '>='
    rhs <- c(problem$Const_upper, problem$Const_lower[-equiv.indices])
    
    finite_rhs <- which(is.finite(rhs)) #ECOS can't handle infinite bounds
    
    lin_constr <- ROI::as.C_constraint(ROI::L_constraint(L = slam::as.simple_triplet_matrix(A[finite_rhs,]),
                                                        dir = dir[finite_rhs],
                                                        rhs = rhs[finite_rhs])
                                       )
    cone <- do.call(c, lapply(problem$group_idx, function(b) ROI::K_soc(length(b) + 1L)) )
    
    L     <- do.call("rbind",
                     lapply(1:length(problem$group_idx), function(i) {
      l_idx <- problem$lambda_idx[[i]]
      b_idx <- problem$beta_idx[problem$group_idx[[i]]]
      cmb_idx <- c(l_idx,b_idx)
      cur_nvar <- length(cmb_idx)
      v <- c(-1.0, rep(-1.0, length(b_idx)))
      temp <- slam::simple_triplet_matrix(i = 1:cur_nvar, j = cmb_idx, v = v, nrow = cur_nvar,  ncol = nvars)
      return(temp)
    }) )
    
    conic_constr <- ROI::C_constraint(
      L = L,
      cone = cone,
      rhs = rep(0,nrow(L))
    )
    
    obj_non_zero <- which(problem$C!=0)
    obj_length_non_zero <- length(obj_non_zero)
    objective <- slam::simple_triplet_matrix(i = rep(1,obj_length_non_zero), 
                                               j = obj_non_zero, 
                                               v = problem$C[obj_non_zero], ncol = nvars) %>% 
      ROI::L_objective()
     
     
    prob <- ROI::OP(
      objective = objective,
      constraints = rbind(lin_constr, conic_constr),
      types = NULL,
      bounds = ROI::V_bound(lb = problem$LB, ub = problem$UB),
      maximum = ifelse(problem$sense == "min", FALSE, TRUE)
    )
  }
  return(list(prob = prob, opts = opts))
}

lp_prob_winf <- function(X, Y, lambda, groups = NULL) {
  # group.lasso <- isTRUE(group.lasso)
  d_length <- length(Y)
  beta_length <- ncol(X)
  group_length <- length(groups)
  group_idx <- lapply(unique(groups), function(i) which(groups == i))
  ngroups <- length(group_idx)
  group_length <- sapply(group_idx, length)
  if(length(lambda) != ngroups) {
    lambda <- rep(lambda, ngroups)
  }
  stopifnot(length(lambda) == ngroups)
  
  # setup indices
  beta_idx <- (1+d_length + 1):(1 + d_length + beta_length) # where beta coef found in model
  total_length <- beta_length + d_length + ngroups + 1 # length of all variables in model
  # beta coef, residual coef, group penalty coef, variable to enforce max norm
  
  lambda_idx <- (total_length - ngroups + 1):(total_length ) # the conic/quadratic constraints
  
  # setup problem list
  # this problem will be min t + crossprod(s,1)
                   #                 st  |d|          <= t
                   #                     y-x %*% beta == d
                   # \sqrt{\sum_{k \in K_g} beta_k^2} <= s_g \forall g \in \{1,...,G\}
  problem <- list(C = c(1, #max bound
                        rep(0, d_length + beta_length), #parameters for: d residuals, beta_length beta coefficients
                        lambda), #l1 penalty of length ngroup
                  LB = c(0, rep(-Inf, d_length + beta_length), rep(0, ngroups)), # lower bounds of variables
                  UB = c(Inf, rep(Inf, d_length + beta_length), rep(Inf, ngroups)), # upper bounds of variables
                  Const = rbind(
                    #residual and upper bound
                    # residual <= t
                    Matrix::sparseMatrix(i = rep(1:d_length, 2), 
                                                     j = c(rep(1,d_length), #t
                                                           2:(d_length + 1)), #residual
                                                     x = c(rep(-1,d_length), # t
                                                           rep(1, d_length)), # residual
                                                     dims = c(d_length,total_length)), 
                                
                    #residual >= -t
                    Matrix::sparseMatrix(i = rep(1:d_length, 2),
                                         j = c(rep(1,d_length), 2:(d_length + 1)),
                                         x = 1,
                                         dims = c(d_length,total_length)), 
                    #residual and lower bound
                                
                    # this is XB + residual = Y
                    cbind(rep(0, d_length), # t variable doesn't contribute
                          Matrix::sparseMatrix(i = 1:d_length,
                                               j = 1:d_length,
                                               x = 1,
                                               dims = c(d_length, d_length)), # residual
                          X, # multiples X %*% beta
                          Matrix::sparseMatrix(i = 1, j = 1, x = 0, 
                                               dims = c(d_length, ngroups))) # lambda variable not contribute
                    
                                # Matrix::sparseMatrix(i = rep(1:beta_length,2),
                                #                      j = c(beta_idx, max(beta_idx) + beta_idx),
                                #                      x = 1,
                                #                      dims =c(beta_length, total_length)), #beta greater than t
                                # Matrix::sparseMatrix(i = rep(1:beta_length,2),
                                #                      j = c(beta_idx, max(beta_idx) + beta_idx),
                                #                      x = c(rep(1, beta_length), rep(-1, beta_length)),
                                #                      dims = c(beta_length, total_length)), #beta less than t
                                # Matrix::sparseMatrix(i = c(sapply(1:ngroups, function(i) rep(i, each = group_length[i])), 
                                #                            1:ngroups),
                                #                      j = c(beta_idx[unlist(group_idx)], 1:ngroups + max(beta_idx)),
                                #                      x = 1,
                                #                      dims = c(ngroups, total_length))
                  ), # sparse penalty sum
                  # Quad_Const = Matrix::crossprod(Matrix::sparseMatrix(i = 1:ngroups,
                  #                                    j = unlist(group_idx),
                  #                                    x = 1,
                  #                                   dims = c(ngroups, beta_length))),
                  Const_upper = c(rep(0, d_length), #upper bound d - z <= 0
                                  rep(Inf, d_length), # upper bound on 0 <= d + z
                                  c(Y) #residual upper bound
                                  # rep(Inf, beta_length), rep(0, beta_length)
                  ),
                  Const_lower = c(rep(-Inf, 
                                      d_length),  # lower bound on d - z <= 0
                                  rep(0, d_length),  # lower bound on 0 <= d + z
                                  c(Y) #residual lower bound
                                  # rep(0, beta_length), 
                                  # rep(-Inf, beta_length)
                  ), 
                  # Quad_const_U = rep(lambda^2, ngroups),
                  # Quad_const_L = rep(0, ngroups),
                  sense = "min",
                  beta_idx = beta_idx,
                  lambda_idx = lambda_idx,
                  group_idx = group_idx
                  
  )
  # if ( beta_length != ngroups ) { # not quite right but probably good enough
  #   problem$Quad_const_L <- rep(0, ngroups)
  #   problem$Quad_const_U <- rep(0, ngroups)
  #   problem$Quad_const <- lapply(seq_along(group_idx), function(g) {
  #     grp <- group_idx[[g]]
  #     beta_grp <- beta_idx[grp]
  #     n_grp <- length(grp)
  #     return(Matrix::sparseMatrix(i = c(beta_grp, lambda_idx[g]),
  #                                 j = c(beta_grp, lambda_idx[g]),
  #                                 x = c(rep(1,n_grp),-1),
  #                                 dims = c(total_length, total_length)))
  #   })
  # }
  
  return(problem)
}

lp_prob_w1 <- function(X, Y, lambda, groups = NULL) {
  # group.lasso <- isTRUE(group.lasso)
  d_length <- length(Y)
  beta_length <- ncol(X)
  group_length <- length(groups)
  group_idx <- lapply(unique(groups), function(i) which(groups == i))
  ngroups <- length(group_idx)
  group_length <- sapply(group_idx, length)
  if(length(lambda) != ngroups) {
    lambda <- rep(lambda, ngroups)
  }
  stopifnot(length(lambda) == ngroups)
  
  beta_idx <- (2*d_length + 1):(2*d_length + beta_length)
  total_length <- beta_length + 2*d_length + ngroups
  lambda_idx <- (total_length - ngroups + 1):(total_length )
  
  
  problem <- list(C = c(rep(1, d_length), #L1 bound
                        rep(0, d_length + beta_length), #parameters for: d residuals, beta_length beta
                        lambda), #l1 penalty of length ngroup
                  LB = c(rep(0, d_length), rep(-Inf, d_length + beta_length), rep(0, ngroups)),
                  UB = c(rep(Inf, 2*d_length + beta_length), rep(Inf, ngroups)),
                  Const = rbind(Matrix::sparseMatrix(i = rep(1:d_length, 2), 
                                                     j = c(1:d_length, (d_length + 1):(2*d_length)),
                                                     x = c(rep(-1,d_length), rep(1, d_length)),
                                                     dims = c(d_length,total_length)), #residual and upper bound
                                Matrix::sparseMatrix(i = rep(1:d_length, 2),
                                                     j = c(1:d_length, (d_length + 1):(2*d_length)),
                                                     x = 1,
                                                     dims = c(d_length,total_length)), #residual and lower bound
                                cbind(Matrix::Diagonal(d_length, 0), 
                                      Matrix::Diagonal(d_length, 1),
                                      X,
                                      Matrix::sparseMatrix(i = 1, j = 1, x = 0, 
                                                           dims = c(d_length, ngroups))) # residual to equal y
                  ), # sparse penalty sum
                  Const_upper = c(rep(0, d_length), #upper bound d - z <= 0
                                  rep(Inf, d_length), # upper bound on 0 <= d + z
                                  c(Y) #residual upper bound
                  ),
                  Const_lower = c(rep(-Inf, d_length),  # lower bound on d - z <= 0
                                  rep(0, d_length),  # lower bound on 0 <= d + z
                                  c(Y) #residual lower bound
                  ), 
                  # Quad_const_U = rep(lambda^2, ngroups),
                  # Quad_const_L = rep(0, ngroups),
                  sense = "min",
                  beta_idx = beta_idx,
                  lambda_idx = lambda_idx,
                  group_idx = group_idx
                  
  )
  # if ( beta_length != ngroups ) { # not quite right but probably good enough
  #   problem$Quad_const_L <- rep(0, ngroups)
  #   problem$Quad_const_U <- rep(0, ngroups)
  #   problem$Quad_const <- lapply(seq_along(group_idx), 
  #               function(g) {
  #                 grp <- group_idx[[g]]
  #                 beta_grp <- beta_idx[grp]
  #                 n_grp <- length(grp)
  #                 return(Matrix::sparseMatrix(i = c(beta_grp, lambda_idx[g]),
  #                                             j = c(beta_grp, lambda_idx[g]),
  #                                             x = c(rep(1,n_grp),-1),
  #                                             dims = c(total_length, total_length)))
  #               }
  #             )
  # }
  
  return(problem)
  
  d_length <- length(Y)
  beta_length <- ncol(X)
  n <- nrow(X)
  
  group_length <- length(groups)
  group_idx <- lapply(unique(groups), function(i) which(groups == i))
  ngroups <- length(group_idx)
  group_length <- sapply(group_idx, length)
  stopifnot(length(lambda) == ngroups)
  
  total_length <- 2*beta_length + 2*d_length + ngroups
  
  beta_idx <- (2*d_length + 1):(2*beta_length + 2*d_length)
  lambda_idx <- (1 + total_length - ngroups):(total_length)
  problem <- list(C = c(rep(1, d_length), #resid bound
                        rep(0, d_length), #resid
                        rep(0, 2*beta_length), #parameters for coef
                        lambda), #l1 penalty
                  LB = c(rep(-Inf, 2*d_length), # lower bound on residual
                         rep(-Inf, beta_length), # lower bound on coef
                         rep(0, beta_length), # bound on coef abs val
                         rep(0, ngroups)), #bounds on L1 penalty
                  UB = c(rep(Inf, 2*d_length), #upper bound on resid
                         rep(Inf, beta_length), # upper bound on coef
                         rep(Inf, beta_length), # bound on coef abs val
                         rep(Inf, ngroups)), #upper bound on L1 penalty
                  Const = rbind(Matrix::sparseMatrix(i = rep(1:d_length, 2), 
                                                     j = c(1:d_length, d_length+ 1:d_length),
                                                     x = c(rep(-1,d_length), rep(1, d_length)),
                                                     dims = c(d_length,total_length)), #residual and upper bound
                                Matrix::sparseMatrix(i = rep(1:d_length, 2), 
                                                     j = c(1:d_length, d_length + 1:d_length),
                                                     x = 1,
                                                     dims = c(d_length,total_length)), #residual and lower bound
                                cbind(Matrix::sparseMatrix(i = 1, 
                                                           j = 1,
                                                           x = 0,
                                                           dims = c(d_length,d_length)), #residual bound 0'd out
                                      Matrix::sparseMatrix(i = 1:d_length, 
                                                           j = 1:d_length,
                                                           x = 1,
                                                           dims = c(d_length, d_length)), #residual
                                      X, #  beta
                                      Matrix::sparseMatrix(i = 1, 
                                                           j = 1,
                                                           x = 0,
                                                           dims = c(d_length, beta_length + ngroups))), #0 out L1 penalty
                                # for the residual portion ^
                                Matrix::sparseMatrix(i = rep(1:beta_length,2),
                                                     j = c(beta_idx, max(beta_idx) + beta_idx),
                                                     x = 1,
                                                     dims =c(beta_length, total_length)), #beta greater than t
                                Matrix::sparseMatrix(i = rep(1:beta_length,2),
                                                     j = c(beta_idx, max(beta_idx) + beta_idx),
                                                     x = c(rep(1, beta_length), rep(-1, beta_length)),
                                                     dims = c(beta_length, total_length)), #beta less than t
                                Matrix::sparseMatrix(i = c(rep(1:ngroups, each = group_length), 1:ngroups),
                                                     j = c(max(beta_idx) + 
                                                             beta_idx[c(unlist(group_idx))], 
                                                           1:ngroups + total_length - ngroups),
                                                     x = c(rep(1,beta_length), rep(-1, ngroups)),
                                                     dims = c(ngroups, total_length))), #bound sum of abs(beta) less than t
                  Const_upper = c(rep(0, d_length), #upper bound
                                  rep(Inf, d_length), #lower bound
                                  c(Y), # outcome
                                  rep(Inf, beta_length), #beta 
                                  rep(Inf, beta_length), # abs(beta)
                                  rep(0, ngroups)), #penalty
                  Const_lower = c(rep(-Inf, d_length), # upper bound
                                  rep(0, d_length), # lower bound
                                  c(Y), #outcome
                                  rep(-Inf, beta_length), #beta 
                                  rep(0, beta_length),#abs(beta)
                                  rep(0, ngroups)),  #penalty (beta^+ + beta^- - t= 0)
                  # Quad_const_U = rep(lambda^2, ngroups),
                  # Quad_const_L = rep(0, ngroups),
                  sense = "min",
                  beta_idx = beta_idx,
                  lambda_idx = lambda_idx
                  
  )
  # problem$Const <- rbind(problem$Const,
  #                        )
  # if(length(group_idx) > 0 ){
  #   
  # }
  return(problem)
}

group_threshold <- function(x, threshold, lambda, gamma, groups) {
  n.groups <- length(groups)
  
  beta <- rep(NA, length(x))
  coefs_l2 <- g_index <- NULL
  for(g in 1:n.groups) {
    g_index <- groups[[g]]
    coefs_l2 <- sqrt(sum(x[g_index]^2))
    thresh <- threshold(coefs_l2, lambda[g], gamma)/coefs_l2
    beta[g_index] <- x[g_index] * thresh
  }
  return(beta)
}

soft_threshold <- function(x, lambda, gamma) {
  ifelse(abs(x) <= lambda, 0 , x)
}

mcp_threshold <- function(x, lambda, gamma) {
  firm_soft <- function(x,lambda,gamma) {
    res <- gamma/(gamma-1) * soft_threshold(x, lambda, gamma)
    if(is.infinite(res)) res <- 0
    if(is.nan(res)) res <- 0
    if(is.na(res)) res <- 0
    return(res)
  }
  ifelse(abs(x) <= gamma * lambda, firm_soft(x,lambda,gamma), x)
}

scad_threshold <- function(x, lambda, gamma) {
  ifelse(abs(x) < 2 * lambda, soft_threshold(x,lambda,gamma), 
         ifelse(abs(x) <= gamma * lambda, (gamma-1)/(gamma-2) * soft_threshold(x, gamma*lambda/(gamma-1), gamma), x ))
}

group_deriv <- function (deriv_func, groups, coefs, lambda, a = 3.7) 
{
  if (length(lambda) == 1) {
    lambda <- rep(lambda, length(groups))
  }
  derivs <- rep(NA, length(groups))
  for (g in 1:length(groups)) {
    g_index <- groups[[g]]
    current_lambda <- lambda[g]
    coefs_l2 <- sqrt(sum(coefs[g_index]^2))
    derivs[g] <- deriv_func(coefs_l2, current_lambda, a)
  }
  derivs
}


find_gurobi <- function() {
   return( rlang::is_installed("gurobi") )
}

find_mosek <- function() {
  return( rlang::is_installed("Rmosek") )
}

