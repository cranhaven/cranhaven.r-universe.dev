#' 2-Wasserstein distance selection by Integer Programming
#'
#' @param X Covariates
#' @param Y Predictions from arbitrary model
#' @param theta Parameters of original linear model. Required
#' @param transport.method Method for Wasserstein distance calculation. Should be one of the outputs of [transport_options()].
#' @param model.size Maximum number of coefficients in interpretable model
#' @param nvars The number of variables to explore. Should be an integer vector of model sizes. Default is NULL which will explore all models from 1 to `model.size`.
#' @param maxit Maximum number of solver iterations
#' @param infimum.maxit Maximum iterations to alternate binary program and Wasserstein distance calculation
#' @param tol Tolerance for convergence of coefficients
#' @param solver The solver to use. Must be one of "cone","lp", "cplex", "gurobi","mosek". 
#' @param display.progress Should progress be printed?
#' @param parallel foreach back end. See [foreach::foreach()] for more details.
#' @param ... Extra args to Wasserstein distance methods
#' 
#' @details
#' For argument `solution.method`, options "cone" and "lp" use the free solvers "ECOS" and "lpSolver", respectively. "cplex", "gurobi" and "mosek" require installing the corresponding commercial solvers.
#' 
#' @keywords internal
# @examples
# if(rlang::is_installed("stats")) {
# n <- 128
# p <- 10
# s <- 100
# 
# x <- matrix( stats::rnorm( p * n ), nrow = n, ncol = p )
# x_ <- t(x)
# beta <- (1:p)/p
# y <- x %*% beta + stats::rnorm(n)
# post_beta <- matrix(beta, nrow=p, ncol=s) + stats::rnorm(p*s, 0, 0.1)
# post_mu <- x %*% post_beta
# 
# test <- W2IP(X = x, Y = post_mu, theta = post_beta, transport.method = "exact",
#              infimum.maxit = 10,
#              tol = 1e-7, solution.method = "cone",
#              display.progress = FALSE,nvars = c(2,4,8))
#              }
W2IP <- function(X, Y=NULL, theta,
                 transport.method = transport_options(),
                 model.size = NULL,
                 nvars = NULL,
                 maxit = 100L,
                 infimum.maxit = 100L,
                 tol = 1e-7,
                 solver = c("cone","lp", "mosek", "cplex", "gurobi"),
                 display.progress=FALSE, parallel = NULL, ...) 
{
  this.call <- as.list(match.call()[-1])
  
  solution.method <- solver
  
  # `%doRNG%`` <- doRNG::`%dorng%`
  
  dots <- list(...)
  if(!is.matrix(X)) X <- as.matrix(X)
  if(!is.matrix(theta)) theta <- as.matrix(theta)
  dims <- dim(X)
  p <- dims[2]
  varnames <- colnames(X)
  if (is.null(varnames))
    varnames = paste("V", seq(p), sep = "")
  infm.maxit <- infimum.maxit
  if(is.null(infm.maxit)){
    infm.maxit <- 100
  }
  
  if (is.null(model.size)) {
    model.size <- p
  }
  
  if(is.null(nvars)) nvars <- 1:model.size
  
  p_star <- length(nvars)
  
  if(is.null(transport.method)){
    transport.method <- "exact"
  } else {
    transport.method <- match.arg(transport.method, transport_options())
  }
  
  if(is.null(solution.method)) {
    solution.method <- "cone"
  } else {
    solution.method <- match.arg(solution.method, choices = c("cone","lp", "mosek", "cplex", "gurobi"))
  }
  
  
  translate <- function(QP, solution.method) {
    switch(solution.method, 
           cone = ROI::ROI_reformulate(QP,to = "socp"),
           lp = ROI::ROI_reformulate(QP,"lp",method = "bqp_to_lp" ),
           cplex = QP,
           gurobi = QP,
           mosek = QP
    )
  }
  
  # using internal functions likely faster but not OK for being on CRAN
  # solver <- function(obj, control, solution.method, start) {
  #   switch(solution.method, 
  #          cone = ROI.plugin.ecos:::solve_OP(obj, control),
  #          lp =  ROI.plugin.lpsolve:::solve_OP(obj, control),
  #          cplex = ROI.plugin.cplex:::solve_OP(obj, control),
  #          gurobi = gurobi_solver(obj, control, start),
  #          mosek = mosek_solver(obj, control,start)
  #   )
  # }
  
  solver <- function(obj, control, solution.method, start) {
    switch(solution.method, 
           cone = ROI::ROI_solve(obj, solver = "ecos", control),
           lp =  ROI::ROI_solve(obj, solver = "lpsolve", control),
           cplex = ROI::ROI_solve(obj, solver = "cplex", control),
           # gurobi = gurobi_solver(obj, control, start),
           mosek = mosek_solver(obj, control,start)
    )
  }
  
  register_solver(solution.method) # registers ROI solver if needed
  
  # if ( solution.method %in% c("cone","lpsolve","cplex") ) ROI::ROI_require_solver(solution.method)
  
  if(ncol(theta) == ncol(X)){
    theta_ <- t(theta)
  } else {
    theta_ <- theta
  }
  if(nrow(theta) != p) stop("dimensions of theta must match X")
  theta_save <- theta_
  
  #transpose X
  X_ <- t(X)
  
  same <- FALSE
  if(is.null(Y)) {
    same <- TRUE
    Y_ <- crossprod(X_,theta_)
  } else{
    if(!any(dim(Y) %in% dim(X_))) stop("dimensions of Y must match X")
    if(!is.matrix(Y)) Y <- as.matrix(Y)
    if(nrow(Y) == ncol(X_)){ 
      # print("Transpose")
      Y_ <- Y
    } else{
      Y_ <- t(Y)
    }
    if(all(Y_==crossprod(X_, theta_))) same <- TRUE
  }
  if(ncol(Y_) != ncol(theta_)) stop("ncol of Y should be same as ncols of theta")
  if(nrow(Y_) != ncol(X_)) stop("The number of observations in Y and X don't line up. Make sure X is input with observations in rows.")
  rmv.idx <- NULL
  if(any(apply(theta_,1, function(x) all(x == 0)))) {
    rmv.idx <- which(apply(theta_,1, function(x) all(x == 0)))
    
    X_ <- X_[-rmv.idx, ]
    theta_ <- theta_[-rmv.idx,]
    penalty.factor <- penalty.factor[-rmv.idx]
    warning("Some dimensions of theta have no variation. These have been removed")
  }
  
  # get control functions
  control <- dots$control
  if(is.null(control)) {
    control <- list()
  } 
  
  epsilon <- dots$epsilon
  if(is.null(epsilon)) epsilon <- 0.05
  OTmaxit <- dots$OTmaxit
  if(is.null(OTmaxit)) OTmaxit <- 100
  # else if (solution.method == "lp") {
  #   if(!is.null(control$verbose)) control$verbose <- as.logical(control$verbose)
  #   if(!is.null(control$presolve)) control$presolve <- as.logical(control$presolve)
  #   if(!is.null(control$tm_limit)) control$tm_limit <- as.integer(control$tm_limit)
  #   if(!is.null(control$canonicalize_status)) control$canonicalize_status <- as.logical(control$canonicalize_status)
  # } else if (solution.method == "cone") {
  #   
  #   control <- ecos.control.better(control)
  # }
  
  #make R types align with c types
  infm.maxit <- as.integer(infm.maxit)
  display.progress <- as.logical(display.progress)
  transport.method <- as.character(transport.method)
  nvars <- as.integer(nvars)
  
  if (infm.maxit <=0) {
    stop("infimum.maxit should be greater than 0")
  }
  
  if(!is.null(parallel)){
    if(!inherits(parallel, "cluster") && !is.numeric(parallel)) {
      stop("parallel must be a registered cluster backend or the number of cores desired")
    }
    doParallel::registerDoParallel(parallel)
    display.progress <- FALSE
  } else{
    foreach::registerDoSEQ()
  }
  
  options <- list(infm_maxit = infm.maxit,
                  display_progress = display.progress, 
                  model_size = nvars)
  OToptions <- list(same = same,
                    method = "selection.variable",
                    transport.method = transport.method,
                    epsilon = epsilon,
                    niter = OTmaxit)
  
  ss <- sufficientStatistics(X, Y_, theta_, OToptions)
  xtx <- ss$XtX
  xty <- xty_init <- ss$XtY
  Ytemp <- Y_
  
  if(display.progress){
    pb <- utils::txtProgressBar(min = 0, max = p_star, style = 3)
    utils::setTxtProgressBar(pb, 0)
  }
  
  QP <- QP_orig <- qp_w2(ss$XtX,ss$XtY,1)
  # LP <- ROI::ROI_reformulate(QP,"lp",method = "bqp_to_lp" )
  alpha <- alpha_save <- rep(0,p)
  beta <- matrix(0, nrow = p, ncol = p_star)
  iter.seq <- rep(0, p_star)
  comb <- function(x, ...) {
    # from https://stackoverflow.com/questions/19791609/saving-multiple-outputs-of-foreach-dopar-loop
    lapply(seq_along(x),
           function(i) c(x[[i]], lapply(list(...), function(y) y[[i]]))
    )
  }
  
  idx <- NULL
  
  output <- foreach::foreach(idx=1:p_star, .combine='comb', .multicombine=TRUE,
                             .init=list(list(), list()),
                             .errorhandling = 'pass', 
                             .inorder = FALSE) %dorng% 
    {
       m <- options$model_size[idx]
       QP <- QP_orig
       QP$constraints$rhs[1L] <- m
       results <- list(NULL, NULL)
       for(inf in 1:options$infm_maxit) {
         TP <- translate(QP, solution.method)
         # sol.meth <- if ( solution.method == "cone" && !("cone" %in% names(TP)) ) {
         #   "lp"
         # } else {
         #   solution.method
         # }
         # browser()
         # sol <- ROI::ROI_solve(LP, "glpk")
         # can use ROI.plugin.glpk:::.onLoad("ROI.plugin.glpk","ROI.plugin.glpk") to use base solver ^
         sol <- solver(TP, control, solution.method=solution.method, start=alpha)
         # print(ROI::solution(sol))
         alpha <- switch(solution.method,
                         "gurobi" = sol,
                         "mosek" = sol,
                         ROI::solution(sol)[1:p])
         if(all(is.na(alpha))) {
           warning("Likely terminated early")
           break
         }
         if(not.converged(alpha, alpha_save, tol)){
           alpha_save <- alpha
           Ytemp <- selVarMeanGen(X_, theta_, as.double(alpha))
           xty <- xtyUpdate(X, Ytemp, theta_, result_ = alpha, 
                                             OToptions)
           QP <- qp_w2(xtx,xty,m)
         } else {
           break
         }
       }
       if(display.progress) utils::setTxtProgressBar(pb, idx)
       results[[2]] <- inf
       results[[1]] <- alpha
       return(results)
       # iter.seq[idx] <- inf
       # if ( same ) QP <- QP_orig
       # QP$constraints$rhs[1] <- m + 1
       # beta[,idx] <- alpha
    }
  if (display.progress) close(pb)
  names(output) <- c("beta","niter")
  output$beta <- do.call("cbind", output$beta)
  output$niter <- unlist(output$niter)
  
  # if (!is.null(parallel) ){
  #   parallel::stopCluster(parallel)
  # }
  output[c("xtx", "xty_init","xty_final")] <- list(xtx, xty_init, xty)
  
  output$nvars <- p
  output$varnames <- varnames
  output$call <- formals(W2L1)
  output$call[names(this.call)] <- this.call
  output$remove.idx <- rmv.idx
  output$nonzero_beta <- colSums(output$beta != 0)
  # output$nzero <- nz
  class(output) <- c("WpProj","IP")
  extract <- extractTheta(output, theta_)
  output$nzero <- extract$nzero
  output$eta <- lapply(extract$theta, function(tt) crossprod(X_, tt))
  output$theta <- extract$theta
  if(!is.null(rmv.idx)) {
    for(i in seq_along(output$theta)){
      output$theta[[i]] <- theta_save
      output$theta[[i]][-rmv.idx,] <- extract$theta[[i]]
    }
  }
  
  return(output)
  
}

qp_w2 <- function(xtx, xty, K) {
  d <- NCOL(xtx)
  Q0 <- 2 * xtx # *2 since the Q part is 1/2 a^\top (x^\top x) a in ROI!!!
  L0 <- c(a = c(-2*xty))
  op <- ROI::OP(objective = ROI::Q_objective(Q = Q0, L = L0, names = as.character(1:d)),
                maximum = FALSE)
  ## sum(alpha) = K 
  A1 <- rep(1,d) 
  LC1 <- ROI::L_constraint(A1, ROI::eq(1), K)
  ROI::constraints(op) <- LC1
  ROI::types(op) <- rep.int("B", d)
  return(op)
}


# gurobi_solver <- function(problem,opts = NULL, start) {
#   
#   prob <-  list()
#   prob$Q <- Matrix::sparseMatrix(i=problem$objective$Q$i,
#                                  j = problem$objective$Q$j,
#                                  x = problem$objective$Q$v/2)
#   prob$modelsense <- 'min'
#   prob$obj <- as.numeric(problem$objective$L$v)
#   num_param <- length(problem$objective$L$v)
#   
#   prob$A <- Matrix::sparseMatrix(i=problem$constraints$L$i,
#                                  j = problem$constraints$L$j,
#                                  x = problem$constraints$L$v)
#   
#   prob$sense <- ifelse(problem$constraints$dir == "==", "=", NA)
#   # prob$sense <- rep(NA, length(qp$LC$dir))
#   # prob$sense[qp$LC$dir=="E"] <- '='
#   # prob$sense[qp$LC$dir=="L"] <- '<='
#   # prob$sense[qp$LC$dir=="G"] <- '>='
#   prob$rhs <- problem$constraints$rhs
#   prob$vtype <- rep("B", num_param)
#   prob$start <- start
#   
#   if(is.null(opts) | length(opts) == 0) {
#     opts <- list(OutputFlag = 0)
#   }
#   
#   res <- gurobi::gurobi(prob, opts)
#   
#   sol <- as.integer(res$x)
#   
#   return(sol)
# }

mosek_solver <- function(problem, opts = NULL, start) {
  
  num_param <- length(problem$objective$L$v)
  # qobj <- Matrix::sparseMatrix(i = problem$objective$Q$i,
  #                              j = problem$objective$Q$j,
  #                              x = problem$objective$Q$v/2)
  lower.tri <- which(problem$objective$Q$j <= problem$objective$Q$i)
  # trimat <- Matrix::tril(qobj)
  prob <-  list(sense = "min",
                c = problem$objective$L$v,
                A = Matrix::sparseMatrix(i=problem$constraints$L$i,
                                         j = problem$constraints$L$j,
                                         x = problem$constraints$L$v),
                bc = rbind(problem$constraints$rhs, problem$constraints$rhs),
                bx = rbind(rep(0,num_param), rep(1, num_param)),
                qobj = list(i =  problem$objective$Q$i[lower.tri], 
                             j =  problem$objective$Q$j[lower.tri], 
                             v =  problem$objective$Q$v[lower.tri]/2),
                sol = list(int = list(xx = start)),
                intsub = 1:num_param)
  
  if(is.null(opts) | length(opts) == 0) opts <- list(verbose = 0)
  
  res <- Rmosek::mosek(prob, opts)
  
  sol <- as.integer(res$sol$int$xx)
  
  return(sol)
}

