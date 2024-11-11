#' 1-Wasserstein projection
#'
#' @param X Covariates
#' @param Y Predictions from arbitrary model
#' @param theta Parameters of original linear model. Optional.
#' @param penalty penalty term to use. One of "none", "lasso","scad","mcp"
#' @param model.size Maximum number of coefficients in interpretable model
#' @param lambda Lambdas to use
#' @param lambda.min.ratio Minimum lambda to select if choosing lambdas using default methods
#' @param nlambda number of lambdas to look through
#' @param gamma parameter for SCAD and MCP methods
#' @param display.progress Print intermediate output?
#' @param solver Solver to use. Must be one of "rqPen", "gurobi", "mosek", though "mosek" is preferred.
#' @param ... options to pass to solvers
#'
#' @return `WpProj` object
#' 
#' @keywords internal
W1L1 <- function(X, Y, theta = NULL, penalty = c("none", "lasso","scad","mcp"), 
                 model.size = NULL,
                 lambda = numeric(0), 
                 lambda.min.ratio = 1e-4, 
                 nlambda = 10, 
                 gamma = 1, 
                 display.progress = FALSE,
                 solver = c( "cone", "rqPen", "gurobi", "mosek"),
                 ...) {
  
  this.call <- as.list(match.call()[-1])
  
  # if(penalty == "lasso") stop("Lasso group penalty is currently incorrect in rqPen package!")
  if(any(penalty == "ols")) penalty <- "none"
  if(any(grepl("lasso", penalty))) penalty <- "lasso"
  if(any(penalty == "elastic.net")) penalty <- "lasso"
  if(any(grepl("mcp", penalty))) penalty <- "mcp"
  if(any(grepl("scad", penalty))) penalty <- "scad"
  penalty <- match.arg(penalty, choices = c("none","lasso","scad","mcp"))
  
  solver <- match.arg(solver)
  
  n <- nrow(X)
  d <- ncol(X)
  
  if(is.null(Y) | missing(Y)) {
    if(!(is.null(theta) | missing(theta))) {
      if(nrow(theta) != ncol(X)) theta <- t(theta)
      Y <- X %*% theta
    }
  }
  
  s <- ncol(Y)
  # cols <- lapply(1:s, function(ss) Matrix::sparseMatrix(i = n*(ss-1) + rep(1:n,d), 
  #                                                       j = rep(1:d,each = n), 
  #                                                       x = c(X),
  #                                                       dims = c(n*s, d)))
  # Xmat <- do.call(cbind, cols)
  Xmat <- Matrix::sparseMatrix(i = c(sapply(1:s, function(ss) n*(ss-1) + rep(1:n,d))), 
                               j = c(sapply(1:s, function(ss) rep(1:d + d * (ss-1),each = n))), 
                               x = c(X),
                               dims = c(n*s, d*s))
  # rm(cols)
  
  if(length(lambda) == 0) {
    if(penalty != "lasso" & solver == "rqPen") {
      lambda.max <- max(colSums(abs(X))/n)
    } else {
      lambda.max <- max(sqrt(colSums(X^2)))
    }
    lambda <-  exp(log(lambda.max) + seq(0, log(lambda.min.ratio), length.out = nlambda))
  } 
  if(length(lambda) == 1) if(lambda == 0) penalty <- "none"
  
  
  if(is.null(model.size) | length(model.size) == 0) {
    model.size <- ncol(Xmat)
  } else {
    model.size <- model.size * s
  }
  if(solver == "rqPen") {
    args <-  list(
      x = as.matrix(Xmat),
      y = c(Y),
      groups = rep(1:d, s),
      tau = 0.5,
      intercept = FALSE,
      a = gamma,
      model.size = model.size,
      penalty = switch(penalty, "lasso" = "LASSO", 
                       "mcp" = "MCP",
                       "scad" = "SCAD"),
      lambda = lambda,
      display.progress = display.progress,
      ...)
    if(is.null(args$method)) {
      args$method <- if(nrow(Xmat) > 1000 & ncol(Xmat) > 100) { #sfn gives error
        #   "sfn"
        #   args$x <- as(Xmat, "matrix.csr")
        # } else if(nrow(Xmat) > 1000 & ncol(Xmat) > 100) {
        "pfn"
      } else if(nrow(Xmat) < 1000 & ncol(Xmat) > 100) {
        "fn"
      } else if(nrow(Xmat)  < 1000 & ncol(Xmat) < 100) {
        "br"
      }
    }
    allowed.args <- if(length(lambda) == 1 & penalty != "none") {
      names(c(formals(l1.group.fit), 
              formals(quantreg::rq.fit.pfn), 
              formals(quantreg::rq.fit.br), 
              formals(quantreg::rq.fit.fnb)))
    } else if (length(lambda) > 1 & penalty != "none") {
      c(names(c(formals(l1.group.fit), 
                formals(quantreg::rq.fit.pfn), 
                formals(quantreg::rq.fit.br), 
                formals(quantreg::rq.fit.fnb))), 
        "model.size")
    } else if (penalty == "none") {
      # args <- list(
      #   x = as.matrix(Xmat),
      #   y = c(Y),
      #   tau = 0.5,
      #   ...
      # )
      # args <- list(formula = formula("Y ~ . + 0"),
      #   data = data.frame(Y = c(Y), X = as.matrix(Xmat)),
      #   tau = 0.5,
      #   ...
      # )
      switch(args$method,
                          "pfn" = names(c(formals(quantreg::rq.fit.pfn))), 
                          "br" = names(formals(quantreg::rq.fit.br)), 
                          "fn" = names(formals(quantreg::rq.fit.fnb)))
      
    }
    if(is.null(args$alg)) args$alg <- "QICD"
    args <- args[names(args) %in% allowed.args]
    
  } else {
    args <-  list(
      X = Xmat,
      Y = c(Y),
      groups = rep(1:d, s),
      lambda = lambda,
      penalty = penalty,
      power = 1,
      gamma = gamma,
      solver = solver,
      model.size = model.size,
      display.progress = display.progress,
      ...)
    args <- args[!duplicated(names(args))]
    
    args <- args[names(args) %in% names(c(formals(GroupLambda)))]
    
  }
  
  args <- args[!duplicated(names(args))]
  
  
  argn <- lapply(names(args), as.name) 
  names(argn) <- names(args)
  
  
  if(penalty != "none") {
    
    if(solver == "rqPen") {
      if(length(lambda) ==1 & penalty != "lasso" ) {
        f.call <- as.call(c(list(call("::", as.name("rqPen"), 
                                      as.name("rq.group.pen"))), argn))
      } else {
        # if(penalty == "lasso") {
        #   argn <- c(argn, "solver" = solver)
        #   args$solver <- solver
        # }
        f.call <- as.call(c(list(as.name("rqGroupLambda")), argn))
      }
    } else {
      f.call <- as.call(c(list(as.name("GroupLambda")), argn))
      
    }
    res <- eval(f.call, envir = args)
    beta <- switch(solver,
                   "rqPen" = sapply(res, function(r) r$coefficients),
                   res)
  } else {
    lambda <- 0
    nlambda <- 0
    if(solver == "rqPen") {
      # alg <- switch()
      f.call <- as.call(c(list(call("::", as.name("quantreg"), 
                                  as.name("rq.fit"))), argn))
    } else {
      args$lambda <- lambda
      # args <- list(
      #   X = as.matrix(Xmat),
      #   Y = c(Y),
      #   groups = 1:ncol(Xmat),
      #   lambda = 0,
      #   penalty = penalty,
      #   power = 1,
      #   gamma = gamma,
      #   model.size = ncol(x),
      #   solver = solver,
      #   ...)
      args <- args[names(args) %in% names(c(formals(GroupLambda)))]
      args <- args[!duplicated(names(args))]
      argn <- lapply(names(args), as.name) 
      names(argn) <- names(args)
      
      f.call <- as.call(c(list(as.name("GroupLambda")), argn))
    }
    res <- eval(f.call, envir = args)
    # res <- do.call(quantreg::rq, args)
    
    beta <- switch(solver,
                   "rqPen" = as.matrix(res$coefficients),
                   res)
  }
  output <- list()
  output$beta <- beta
  output$penalty <- penalty
  output$lambda <- lambda
  output$nvars <- d
  output$maxit <- NULL
  output$call <- formals(W1L1)
  output$call[names(this.call)] <- this.call
  output$nonzero_beta <- colSums(output$beta != 0)
  output$method <- "projection"
  output$power <- 1
  class(output) <- c("WpProj", "optimization")
  
  extract <- extractTheta(output, matrix(0, d,s))
  output$nzero <- extract$nzero
  output$eta <- lapply(extract$theta, function(tt) X %*%tt )
  output$theta <- extract$theta
  output$model <- res
  return(output)
} 
