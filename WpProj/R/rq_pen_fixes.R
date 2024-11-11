l1.group.fit <- function (x, y, groups, lambda, intercept = TRUE, 
                          penalty = "MCP", alg = "QICD", a = 3.7, penGroups = NULL, 
                          ...) 
{
  tau <- 0.5
  p <- ncol(x)
  n <- nrow(x)
  if(is.null(alg) | missing(alg)) alg <- "QICD"
  if(is.null(penalty) | missing(penalty)) penalty <- "MCP"
  
  if (!penalty %in% c("SCAD", "MCP")) {
    stop("Penalty must be SCAD or MCP")
  }
  if (is.null(dim(x))) {
    stop("x must be matrix with at least 1 column")
  }
  if (length(groups) != ncol(x)) {
    stop("length(groups) must be equal to ncol(x)")
  }
  if (lambda <= 0) {
    stop("lambda must be positive")
  }
  if (penalty == "LASSO") {
    pen_func <- rqPen_lasso
  }
  if (penalty == "SCAD") {
    pen_func <- rqPen_scad
  }
  if (penalty == "MCP") {
    pen_func <- rqPen_mcp
  }
  if (alg == "QICD") {
    if (length(lambda) != 1) 
      stop("QICD Algorithm only allows 1 lambda value")
    coefs <- rqPen_QICD.group(y = y, x = x, groups = groups, 
                               lambda = lambda, intercept = intercept, 
                               penalty = penalty, a = a, ...)
    coefnames <- paste("x", 1:p, sep = "")
    if (intercept) 
      coefnames <- c("(Intercept)", coefnames)
    names(coefs) <- coefnames
    if (intercept) {
      residuals <- c(y - x %*% (coefs[-1]) - coefs[1])
      pen_vars <- coefs[-1]
    }
    else {
      residuals <- c(y - x %*% coefs)
      pen_vars <- coefs
    }
    if (penalty == "LASSO") {
      pen_val <- sum(pen_func(tapply(abs(pen_vars), groups, 
                                     sum), lambda = lambda))
    }
    else {
      pen_val <- sum(pen_func(tapply(abs(pen_vars), groups, 
                                     sum), lambda = lambda, a = a))
    }
    rho <- sum(rqPen_check(residuals))
    PenRho <- rho + pen_val
    return_val <- list(coefficients = coefs, PenRho = PenRho, 
                       residuals = residuals, rho = rho, tau = tau, n = n, 
                       intercept = intercept, penalty = penalty, alg = "QICD")
    class(return_val) <- c("rq.group.pen", "rq.pen")
  }
  else {
    group_num <- length(unique(groups))
    if (length(lambda) == 1) {
      lambda <- rep(lambda, group_num)
    }
    if (length(lambda) != group_num) {
      stop("lambdas do not match with group number")
    }
    if (sum(groups == 0) > 0) {
      stop("0 cannot be used as a group")
    }
    if (dim(x)[2] != length(groups)) {
      stop("length of groups must be equal to number of columns in x")
    }
    if (penalty == "LASSO") {
      stop("doesn't handle lasso penalties")
    }
    else {
      return_val <- rqPen_rq.group.lin.prog(x, y, groups, tau, 
                                             lambda, intercept = intercept, penalty = penalty, 
                                             penGroups = penGroups, a = a, ...)
      return_val$alg <- "lin.prog"
      class(return_val) <- c("rq.group.pen", "rq.pen")
    }
  }
  return_val
}


rqGroupLambda <- function(x, y, groups, lambda, intercept = FALSE, tau = 0.5,
                          penalty = "MCP", alg = "QICD_warm", penGroups = NULL, a = 3.7,
                          model.size = NULL,
                          display.progress = FALSE,
                          ...) 
{
  return_val <- vector("list", length(lambda))
  pos <- 1
  tau <- 0.5
  intercept <- FALSE
  if(is.null(model.size) | length(model.size) == 0) model.size <- ncol(x)
  if(is.null(alg) | missing(alg)) alg <- "QICD_warm"
  alg <- match.arg(alg, choices = c("QICD_warm","lin.prog"))
  if(is.null(penalty) | missing(penalty)) penalty <- "MCP"
  
  
  if(penalty == "LASSO") {
    # dots <- list(...)
    # for(lam in lambda) {
    #   #X, Y, lambda, groups, solver, opts = NULL, init = NULL,...
    #   return_val[[pos]] <- l1_norm(X = x, Y = y, lambda = lam,
    #                                groups = groups, solver = dots$solver, 
    #                                opts = dots$opts, init = dots$init)
    #   pos <- pos + 1L
    # }
    # pen.lp <- switch(penalty,
    #                  "MCP" = "mcp",
    #                  "SCAD" = "scad",
    #                  "LASSO" = "lasso")
    solver <- list(...)$solver
    if(is.null(solver)) solver <- "rqPen"
    if(solver == "rqPen" ) {
      if(find_mosek()) {
        solver <- "mosek"
      } else if (find_gurobi()) {
        solver <- "gurobi"
      } else {
        solver <- "quadprog"
      }
    }
    temp_beta <- GroupLambda(X = x, Y = y, power = 1, groups = groups, lambda = lambda,
                             penalty = "lasso",
                             gamma = a, solver = solver,
                             model.size = model.size, 
                             options = list(...)$options, 
                             ...)
    return_val <- lapply(1:ncol(temp_beta), function(tt) list(coefficients = temp_beta[,tt]))
    return(return_val)
  }
  if (alg != "QICD_warm") {
    pos <- 1
    if(display.progress) pb <- utils::txtProgressBar(min = 0, max = length(lambda), style = 3)
    
    for (lam in lambda) {
      return_val[[pos]] <- l1.group.fit(x = x, y = y, groups = groups,
                                        tau = tau, lambda = lam, intercept = intercept, a = a,
                                        penalty = penalty, alg = alg, penGroups = penGroups, 
                                        ...)
      if(sum(return_val[[pos]]$coefficients != 0) > model.size) {
        if(pos != length(lambda)) return_val[(pos):length(return_val)] <- NULL
        break
      }
      if(display.progress) utils::setTxtProgressBar(pb, pos)
      pos <- pos + 1
    }
  } else {
    p <- dim(x)[2]
    pos <- 1
    alg = "QICD"
    if (intercept) {
      initial_beta <- list(c(stats::quantile(y, tau), rep(0, p)))
    }
    else {
      initial_beta <- list(rep(0, p))
    }
    if(display.progress) pb <- utils::txtProgressBar(min = 0, max = 2*length(lambda), style = 3)
    
    for (lam in lambda) {
      return_val[[pos]] <- rqPen_QICD.group(y = y, x = x, groups = groups, tau = tau, 
                                             lambda = lam, intercept = intercept, initial_beta = initial_beta[[1]],
                                             penalty = "LASSO", ...)
      # return_val[[pos]] <- rqPen_rq.group.fit(x = x, y = y, groups = groups,
      #                                   tau = tau, lambda = lam, intercept = intercept,
      #                                   penalty = "LASSO", alg = alg, initial_beta = initial_beta[[1]],
      #                                   penGroups = penGroups, ...)
      initial_beta[[1]] <- return_val[[pos]]
      if(display.progress) utils::setTxtProgressBar(pb, pos)
      
      pos <- pos + 1
    }
    # temp_beta <- GroupLambda(X = x, Y = y, power = 1, groups = groups, lambda = lambda,
    #               penalty = "lasso",
    #               gamma = a, solver = list(...)$solver,
    #               options = list(...)$options, ...)
    # return_val <- lapply(1:ncol(temp_beta), function(tt) temp_beta[,tt])
    # if (penalty != "LASSO") {
    pos <- 1
    for (lam in lambda) {
      initial_beta[[1]] <- return_val[[pos]]
      return_val[[pos]] <- l1.group.fit(x = x, y = y,
                                        groups = groups, tau = tau, lambda = lam, intercept = intercept,
                                        a = a,
                                        penalty = penalty, alg = alg, initial_beta = initial_beta[[1]], 
                                        penGroups = penGroups, ...)
      if(sum(return_val[[pos]]$coefficients != 0) > model.size) {
        if(pos != length(lambda)) return_val[(pos):length(return_val)] <- NULL
        break
      }
      if(display.progress) utils::setTxtProgressBar(pb, length(lambda) + pos)
      
      pos <- pos + 1
      
    }
    # }
  }
  return_val
}

# l1_norm <- function(X, Y, lambda, groups, solver, opts = NULL, init = NULL,...) {
#   
#   d <- ncol(X)
#   
#   group_length <- length(groups)
#   if(group_length > 0 ) {
#     group_idx <- lapply(unique(groups), function(i) which(groups == i))
#     ngroups <- length(group_idx)
#     group_length <- sapply(group_idx, length)
#   }
#   
#   if(is.null(init)) init <- rep(0, d)
#   
#   lambda_update <- list(rep(lambda, ngroups))
#   
#   problem <- lp_prob_w1(X = X, Y = Y, lambda = lambda_update[[1]], groups = groups)
#   
#   if (solver == "mosek") {
#     prob <-  list(sense = problem$sense,
#                   c = problem$C,
#                   A = problem$Const,
#                   bc = rbind(problem$Const_lower, problem$Const_upper),
#                   bx = rbind(problem$LB, problem$UB))
#     if(!is.null(init)) prob$sol <- list(bas = list(xx = init))
#     if(is.null(opts)) opts <- list(verbose = 0, )
#   } else if (solver == "gurobi") {
#     prob <-  list()
#   }
#   
#   beta <- lp_solve(prob, opts, solver)
#   return(beta)
# }

rqPen_lasso <- function (x, lambda = 1, a = 1) 
{
  lambda * a * abs(x)
}

rqPen_QICD.group <- function (y, x, groups, tau = 0.5, lambda, intercept = TRUE, 
                              penalty = "SCAD", initial_beta = NULL, maxin = 100, maxout = 20, 
                              eps = 1e-05, coef.cutoff = 1e-08, a = 3.7, scalex = TRUE, 
                              norm = 2, ...) 
{
  rqPen_cleanInputs(y, x, lambda, initial_beta, intercept, penalty, 
              a)
  if (scalex) {
    x <- scale(x)
    mu_x <- attributes(x)$`scaled:center`
    sigma_x <- attributes(x)$`scaled:scale`
  }
  if (penalty == "SCAD") {
    pentype <- as.integer(0)
  }
  else if (penalty == "MCP") {
    pentype <- as.integer(1)
  }
  else {
    pentype <- as.integer(2)
  }
  if (is.null(initial_beta)) {
    initial_beta <- rqPen_LASSO.fit(y, x, tau, lambda, intercept, 
                              coef.cutoff)
  }
  if (intercept) {
    beta <- initial_beta[-1]
    intval <- initial_beta[1]
  }
  else {
    beta <- initial_beta
    intval <- 0
  }
  n <- length(y)
  y <- as.double(y)
  xdoub <- as.double(x)
  p <- as.integer(ncol(x))
  tau <- as.double(tau)
  int <- as.integer(intercept)
  nint <- as.integer(length(y))
  a <- as.double(a)
  eps <- as.double(eps)
  maxin <- as.integer(maxin)
  if (length(lambda) == 1) {
    lambda <- rep(lambda, p)
  }
  else if (length(lambda) != p) {
    stop("lambda must be of length 1 or p")
  }
  lambda <- as.double(lambda)
  i = 0
  distance <- eps + 1
  groupNorm <- rep(0, p)
  beta0 <- beta
  intval0 <- intval
  residuals <- as.double(y - x %*% beta - intval)
  badValues <- FALSE
  while ((i < maxout) & (distance >= eps)) {
    for (grps in unique(groups)) {
      if (norm == 2) {
        groupNorm[groups == grps] <- sqrt(sum(beta[groups == 
                                                     grps]^2))
      }
      else {
        groupNorm[groups == grps] <- sum(abs(beta[groups == 
                                                    grps]))
      }
    }
    out <- list()
    # uncomment to use
    # out <- penderiv_cpp(as.double(groupNorm), p, a, lambda, 
    #           pentype)
    # penweight <- as.double(n * out)
    # out <- QCD_cpp(xdoub, as.double(beta), as.double(intval), 
    #           penweight, residuals, nint, p, int, tau, eps, maxin)
    
    beta <- out[[1L]]
    intval <- out[[2L]]
    residuals <- as.double(out[[3L]])
    i <- i + 1
    distance <- sqrt(sum((beta - beta0)^2) + (intval0 - intval)^2)
    beta0 <- beta
    intval0 <- intval
    if (max(abs(beta)) > 1e+10) {
      badValues <- TRUE
      break
    }
  }
  if (badValues) {
    warning("Some coefficients diverged to infinity (bad results)")
  }
  if (i == maxout & distance > eps) {
    warning(paste("did not converge after ", maxout, " iterations", 
                  sep = ""))
  }
  beta[abs(beta) < coef.cutoff] <- 0
  coefficients <- beta
  if (intercept) {
    coefficients <- c(intval, beta)
  }
  if (scalex) {
    coefficients <- rqPen_transform_coefs(coefficients, mu_x, sigma_x, 
                                    intercept)
  }
  return(coefficients)
}

rqPen_check <- function (x, tau = 0.5) 
{
  x * (tau - (x < 0))
}

rqPen_mcp <- function (x, lambda = 1, a = 3) 
{
  absx <- abs(x)
  ifelse(absx < a * lambda, lambda * (absx - absx^2/(2 * a * 
                                                       lambda)), a * lambda^2/2)
}

rqPen_mcp_deriv <- function (x, lambda = 1, a = 3) 
{
  ll <- length(lambda)
  if (ll != 1 && ll != length(x)) {
    stop("lambda must be of length 1 or same length as x")
  }
  u <- x
  u[] <- 0
  index <- abs(x) < a * lambda
  if (ll == 1) {
    u[index] <- ifelse(x[index] == 0, lambda, lambda * sign(x[index]) - 
                         x[index]/a)
  }
  else {
    u[index] <- ifelse(x[index] == 0, lambda[index], lambda[index] * 
                         sign(x[index]) - x[index]/a)
  }
  u
}

rqPen_rq.group.lin.prog <- function (x, y, groups, tau, lambda, intercept = TRUE, eps = 1e-05, 
                                     penalty = "SCAD", a = 3.7, coef.cutoff = 1e-08, initial_beta = NULL, 
                                     iterations = 1, converge_criteria = 1e-04, penGroups = NULL, 
                                     ...) 
{
  group_num <- length(unique(groups))
  if (length(lambda) == 1) {
    lambda <- rep(lambda, group_num)
  }
  if (length(lambda) != group_num) {
    stop("lambdas do not match with group number")
  }
  if (sum(groups == 0) > 0) {
    stop("0 cannot be used as a group")
  }
  if (dim(x)[2] != length(groups)) {
    stop("length of groups must be equal to number of columns in x")
  }
  if (penalty == "SCAD") {
    deriv_func <- rqPen_scad_deriv
  }
  if (penalty == "MCP") {
    deriv_func <- rqPen_mcp_deriv
  }
  new_lambda <- NULL
  group_count <- stats::xtabs(~groups)
  for (g in 1:group_num) {
    new_lambda <- c(new_lambda, rep(lambda[g], each = group_count[g]))
  }
  if (is.null(penGroups) == FALSE) {
    zero_pen_spots <- which(!groups %in% penGroups)
    new_lambda[zero_pen_spots] <- 0
  }
  if (is.null(initial_beta)) {
    initial_beta <- rqPen_rq.lasso.fit(x, y, tau, new_lambda, intercept = intercept, 
                                 coef.cutoff = coef.cutoff, ...)$coefficients
  }
  coef_by_group_deriv <- rqPen_group_derivs(deriv_func, groups, initial_beta, 
                                      lambda, a)
  lambda_update <- coef_by_group_deriv[groups]
  old_beta <- initial_beta
  iter_complete <- FALSE
  iter_num <- 0
  coef_range <- (1 + intercept):(dim(x)[2] + intercept)
  while (!iter_complete) {
    sub_fit <- rqPen_rq.lasso.fit(x = x, y = y, tau = tau, lambda = lambda_update, 
                            intercept = intercept, ...)
    coef_by_group_deriv <- rqPen_group_derivs(deriv_func, groups, 
                                        sub_fit$coefficients[coef_range], lambda, a)
    lambda_update <- coef_by_group_deriv[groups]
    if (is.null(penGroups) == FALSE) {
      lambda_update[zero_pen_spots] <- 0
    }
    iter_num <- 1
    new_beta <- sub_fit$coefficients
    beta_diff <- sum((old_beta - new_beta)^2)
    if (iter_num == iterations | beta_diff < converge_criteria) {
      iter_complete <- TRUE
      if (iter_num == iterations & beta_diff > converge_criteria) {
        warning(paste("did not converge after ", iterations, 
                      " iterations", sep = ""))
      }
    }
    else {
      old_beta <- new_beta
    }
  }
  sub_fit$penalty <- penalty
  class(sub_fit) <- c("rq.pen", "rqNC")
  sub_fit
}

rqPen_scad <- function (x, lambda = 1, a = 3.7) 
{
  absx <- abs(x)
  ifelse(absx < lambda, lambda * absx, ifelse(absx < a * lambda, 
                                              ((a^2 - 1) * lambda^2 - (absx - a * lambda)^2)/(2 * (a - 
                                                                                                     1)), (a + 1) * lambda^2/2))
}

rqPen_scad_deriv <- function (x, lambda = 1, a = 3.7) 
{
  ll <- length(lambda)
  if (ll != 1 && ll != length(x)) {
    stop("lambda must be of length 1 or same length as x")
  }
  absx <- u <- abs(x)
  u[] <- 0
  index <- absx < a * lambda & absx > 0
  if (ll == 1) {
    u[index] <- ifelse(absx[index] <= lambda, lambda, (a * 
                                                         lambda - absx[index])/(a - 1))
    u[index] <- u[index] * sign(x[index])
    u[x == 0] <- lambda
  }
  else {
    u[index] <- ifelse(absx[index] <= lambda[index], lambda[index], 
                       (a * lambda[index] - absx[index])/(a - 1))
    u[index] <- u[index] * sign(x[index])
    u[x == 0] <- lambda[x == 0]
  }
  u
}

rqPen_cleanInputs <- function (y, x, lambda, initial_beta = NULL, intercept = TRUE, 
                               penalty, a, ...) 
{
  if (methods::is(x, "matrix") == FALSE) {
    stop("x needs to be a matrix")
  }
  if (any(lambda <= 0)) {
    stop("lambda must be positive")
  }
  if (nrow(x) != length(y)) {
    stop("length of y and rows of x do not match")
  }
  if (!is.null(initial_beta) & (length(initial_beta) < (ncol(x) + 
                                                        intercept))) {
    stop("initial_beta must contain initial value for intercept (if TRUE) and each coefficient")
  }
  if (penalty == "SCAD") {
    if (a <= 2) 
      stop("a must be > 2 for SCAD penalty")
  }
  else if (penalty == "MCP") {
    if (a <= 1) 
      stop("a must be > 1 for MCP penalty")
  }
  else {
    if (penalty != "LASSO") 
      stop("wrong penalty function")
  }
  return(NULL)
}

rqPen_LASSO.fit <- function (y, x, tau, lambda, intercept, coef.cutoff, weights = NULL) 
{
  p <- ncol(x)
  n <- nrow(x)
  ynew <- c(y, rep(0, 2 * p))
  xnew <- rbind(x, diag(n * lambda, p), -diag(n * lambda, p))
  if (intercept) 
    xnew <- cbind(c(rep(1, n), rep(rep(0, 2 * p))), xnew)
  if (!is.null(weights)) {
    xnew[1:n, ] <- xnew[1:n, ] * weights
    ynew[1:n] <- ynew[1:n] * weights
  }
  out <- rqPen_shortrq.fit.br(xnew, ynew, tau)
  out[abs(out) < coef.cutoff] <- 0
  return(out)
}

rqPen_shortrq.fit.br <- function (x, y, tau = 0.5) 
{
  tol <- .Machine$double.eps^(2/3)
  eps <- tol
  big <- .Machine$double.xmax
  x <- as.matrix(x)
  p <- ncol(x)
  n <- nrow(x)
  ny <- NCOL(y)
  nsol <- 2
  ndsol <- 2
  lci1 <- FALSE
  qn <- rep(0, p)
  cutoff <- 0
  # from rq pen code
  # z <- .Fortran("rqbr", as.integer(n), as.integer(p), 
  #               as.integer(n + 5), as.integer(p + 3), as.integer(p + 4), as.double(x), 
  #               as.double(y), as.double(tau), as.double(tol), flag = as.integer(1), 
  #               coef = double(p), resid = double(n), integer(n), 
  #               double((n +5) * (p + 4)), double(n), as.integer(nsol), as.integer(ndsol), 
  #               sol = double((p + 3) * nsol), dsol = double(n * ndsol), 
  #               lsol = as.integer(0), h = integer(p * nsol), qn = as.double(qn), 
  #               cutoff = as.double(cutoff), ci = double(4 * p), tnmat = double(4 * p), 
  #               as.double(big), as.logical(lci1))
  # coef <- z$coef
  # rqbr(m,nn,m5,n3,n4,a,b,t,toler,ift,x,e,s,wa,wb,nsol,nds
  #      *ol,sol,dsol,lsol,h,qn,cutoff,ci,tnmat,big,lci1)
  
  coef <- NULL
  #uncomment to use
  # coef <- rqbr_cpp(
  #               m = as.integer(n), nn = as.integer(p), 
  #               m5 = as.integer(n + 5), n3 = as.integer(p + 3), 
  #               n4 = as.integer(p + 4), 
  #               a = as.double(x), b = as.double(y), 
  #               t = as.double(tau), toler = as.double(tol), 
  #               ift = as.integer(1), #flag
  #               x = double(p), #coef
  #               e = double(n), #resid
  #               s = integer(n), 
  #               wa = double((n +5) * (p + 4)), wb = double(n), 
  #               nsol = as.integer(nsol), ndsol = as.integer(ndsol), 
  #               sol = double((p + 3) * nsol), dsol = double(n * ndsol), 
  #               lsol = as.integer(0), h = integer(p * nsol), qn = as.double(qn), 
  #               cutoff = as.double(cutoff), ci = double(4 * p), tnmat = double(4 * p), 
  #               big = as.double(big), lci1 = as.logical(lci1)
  #         )
  
  coef
}

rqPen_transform_coefs <- function (coefs, mu_x, sigma_x, intercept = TRUE) 
{
  new_coefs <- coefs
  if (intercept) {
    intercept <- coefs[1]
    for (j in 2:length(coefs)) {
      new_coefs[j] <- coefs[j]/sigma_x[j - 1]
      intercept <- intercept - coefs[j] * mu_x[j - 1]/sigma_x[j - 
                                                                1]
    }
    new_coefs[1] <- intercept
  }
  else {
    for (j in 1:length(coefs)) {
      new_coefs[j] <- coefs[j]/sigma_x[j]
    }
  }
  new_coefs
}

rqPen_scad_deriv <- function (x, lambda = 1, a = 3.7) 
{
  ll <- length(lambda)
  if (ll != 1 && ll != length(x)) {
    stop("lambda must be of length 1 or same length as x")
  }
  absx <- u <- abs(x)
  u[] <- 0
  index <- absx < a * lambda & absx > 0
  if (ll == 1) {
    u[index] <- ifelse(absx[index] <= lambda, lambda, (a * 
                                                         lambda - absx[index])/(a - 1))
    u[index] <- u[index] * sign(x[index])
    u[x == 0] <- lambda
  }
  else {
    u[index] <- ifelse(absx[index] <= lambda[index], lambda[index], 
                       (a * lambda[index] - absx[index])/(a - 1))
    u[index] <- u[index] * sign(x[index])
    u[x == 0] <- lambda[x == 0]
  }
  u
}

rqPen_mcp_deriv <- function (x, lambda = 1, a = 3) 
{
  ll <- length(lambda)
  if (ll != 1 && ll != length(x)) {
    stop("lambda must be of length 1 or same length as x")
  }
  u <- x
  u[] <- 0
  index <- abs(x) < a * lambda
  if (ll == 1) {
    u[index] <- ifelse(x[index] == 0, lambda, lambda * sign(x[index]) - 
                         x[index]/a)
  }
  else {
    u[index] <- ifelse(x[index] == 0, lambda[index], lambda[index] * 
                         sign(x[index]) - x[index]/a)
  }
  u
}

rqPen_rq.lasso.fit <- function (x, y, tau = 0.5, lambda = NULL, weights = NULL, intercept = TRUE, 
                                coef.cutoff = 1e-08, method = "br", penVars = NULL, scalex = TRUE, 
                                lambda.discard = TRUE, ...) 
{
  if (is.null(dim(x))) {
    stop("x needs to be a matrix with more than 1 column")
  }
  p <- dim(x)[2]
  if (p == 1) {
    stop("x needs to be a matrix with more than 1 column")
  }
  n <- dim(x)[1]
  if (n != length(y)) {
    stop("length of y and rows of x do not match")
  }
  if (is.null(lambda) == TRUE | (length(lambda) != 1 & length(lambda) != 
                                 dim(x)[2])) {
    stop(paste("input of lambda must be of length 1 or", 
               dim(x)[2]))
  }
  if (sum(lambda < 0) > 0) {
    stop(paste("lambda must be positive and we have a lambda of ", 
               lambda, sep = ""))
  }
  if (scalex) {
    original_x <- x
    x <- scale(x)
    mu_x <- attributes(x)$`scaled:center`
    sigma_x <- attributes(x)$`scaled:scale`
  }
  if (is.null(penVars) != TRUE) {
    if (length(lambda) == 1) {
      mult_lambda <- rep(0, p)
      mult_lambda[penVars] <- lambda
      lambda <- mult_lambda
    }
    else {
      lambda[-penVars] <- 0
    }
  }
  lambda <- lambda * n
  if (length(lambda) == 1) {
    pen_x <- rbind(diag(rep(lambda, p)), diag(rep(-lambda, 
                                                  p)))
  }
  else {
    pen_x <- rbind(diag(lambda), diag(-lambda))
    pen_x <- pen_x[rowSums(pen_x == 0) != dim(pen_x)[2], 
    ]
  }
  aug_n <- dim(pen_x)[1]
  aug_x <- rbind(x, pen_x)
  if (intercept) {
    aug_x <- cbind(c(rep(1, n), rep(0, aug_n)), aug_x)
  }
  aug_y <- c(y, rep(0, aug_n))
  if (is.null(weights)) {
    model <- quantreg::rq(aug_y ~ aug_x + 0, tau = tau, method = method)
  }
  else {
    if (length(weights) != n) {
      stop("Length of weights does not match length of y")
    }
    orig_weights <- weights
    weights <- c(weights, rep(1, aug_n))
    model <- quantreg::rq(aug_y ~ aug_x + 0, tau = tau, weights = weights, 
                method = method)
  }
  p_star <- p + intercept
  coefs <- stats::coefficients(model)[1:p_star]
  return_val <- NULL
  return_val$coefficients <- coefs
  if (is.null(colnames(x))) {
    x_names <- paste("x", 1:p, sep = "")
  }
  else {
    x_names <- colnames(x)
  }
  if (intercept) {
    x_names <- c("intercept", x_names)
  }
  attributes(return_val$coefficients)$names <- x_names
  return_val$coefficients[abs(return_val$coefficients) < coef.cutoff] <- 0
  if (scalex) {
    return_val$coefficients <- rqPen_transform_coefs(return_val$coefficients, 
                                               mu_x, sigma_x, intercept)
    if (intercept) {
      fits <- cbind(1, original_x) %*% return_val$coefficients
    }
    else {
      fits <- original_x %*% return_val$coefficients
    }
    res <- y - fits
    return_val$PenRho <- sum(sapply(res, rqPen_check, tau)) + rqPen_get_coef_pen(return_val$coefficients, 
                                                                     lambda, intercept, penVars)
  }
  else {
    return_val$PenRho <- model$rho
    res <- model$residuals[1:n]
  }
  if (is.null(weights)) {
    return_val$rho <- sum(sapply(res, rqPen_check, tau))
  }
  else {
    return_val$rho <- sum(orig_weights * sapply(res, rqPen_check, 
                                                tau))
  }
  return_val$tau <- tau
  return_val$n <- n
  return_val$intercept <- intercept
  class(return_val) <- c("rq.pen", "rqLASSO")
  return_val
}

rqPen_get_coef_pen <- function (coefs, lambda, intercept, penVars, penalty = "LASSO", 
                                a = NULL) 
{
  if (intercept) {
    coefs <- coefs[-1]
  }
  if (is.null(penVars) == FALSE) {
    coefs <- coefs[penVars]
    lambda <- lambda[penVars]
  }
  if (penalty == "LASSO") {
    sum(abs(coefs) * lambda)
  }
  else if (penalty == "SCAD") {
    sum(rqPen_scad(coefs, lambda, a))
  }
  else {
    sum(rqPen_mcp(coefs, lambda, a))
  }
}

rqPen_group_derivs <- function (deriv_func, groups, coefs, lambda, a = 3.7, norm = 1) 
{
  if (length(lambda) == 1) {
    lambda <- rep(lambda, length(groups))
  }
  derivs <- NULL
  for (g in 1:length(unique(groups))) {
    g_index <- which(groups == g)
    current_lambda <- lambda[g]
    if (norm == 1) {
      gnorm <- sum(abs(coefs[g_index]))
    }
    else if (norm == 2) {
      gnorm <- sqrt(sum(coefs[g_index]^2))
    }
    else {
      stop("Invalid norm, use 1 or 2")
    }
    derivs <- c(derivs, deriv_func(gnorm, current_lambda, 
                                   a))
  }
  derivs
}
